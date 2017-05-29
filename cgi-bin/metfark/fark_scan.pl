#!/usr/bin/perl -w
#
use ExtUtils::testlib;
use fark;
use farkdata;
#
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use Data::Dumper;
use POSIX 'strftime';
use Capture::Tiny 'capture';
use File::Basename;
use File::Find;
use File::Path qw( make_path );
use File::Compare;
use File::Copy;
#
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
my $user=$ENV{USERNAME} // "www";
my $pub="/metfark/pub";
my $modelDir="$pub/model/";check_path($modelDir);
my $modelOldDir="$pub/old/model/"; check_path($modelOldDir);
my $modelCacheDir="$pub/cache/model/"; check_path($modelCacheDir);
my $modelRegDir="$pub/reg/model/"; check_path($modelRegDir);
my $obsDir="$pub/obs/";check_path($obsDir);
my $obsOldDir="$pub/old/obs/"; check_path($obsOldDir);
my $obsCacheDir="$pub/cache/obs/"; check_path($obsCacheDir);
my $obsRegDir="$pub/reg/obs/"; check_path($obsRegDir);
my $scanDir="$pub/scan/";check_path($scanDir);
my $useModelDir="$pub/use/model/";check_path($useModelDir);
my $useObsDir="$pub/use/obs/";check_path($useObsDir);
my $lockDir="$pub/lock/"; check_path($lockDir);
#
#
my $myname = basename($0);
#my $user = qx(/usr/bin/whoami); chomp($user);
#my $host = qx(/bin/uname -n); chomp($host);
#my $homeDir  = $ENV{HOME} // "/home/$user";
#
my $ref=CGI->new();
#
$XML::LibXML::skipXMLDeclaration = 1;
my $param= $ref->{param};
if (! defined $param->{root}) {&term("Undefined root file.".Dumper($param))};
my $root=basename($param->{root}[0]);
my $password=($param->{password}[0] // "");
my $save=0;
my $scanfile=$scanDir . $root;
my $file=basename(($param->{file}[0] // ""));
# scan config file...
my $parser = XML::LibXML->new();
if (-f $scanfile) {
    # read config file into memory
    my $doc = $parser->parse_file($scanfile);
    if ( my ($node)=$doc->findnodes("scan/scan_config")) {
	my $pass=($node->getAttribute("password")||"");
	$save=($pass eq $password);
	if (defined $param->{type}) {
	    if ($param->{type}->[0] eq "model") {
		if (!&scanModel($node, $file, $modelDir, $modelCacheDir, $modelRegDir)) {
		    &term("File not in scan list:$file");
		};
	    };
	    if ($param->{type}->[0] eq "obs") {
		if (! &scanObs($node, $file, $obsDir, $obsCacheDir, $obsRegDir)) {
		    &term("File not in scan list:$file");
		};
	    }
	} else {
	    &scanModel($node,"", $modelDir, $modelCacheDir, $modelRegDir);
	    &scanObs($node,"", $obsDir, $obsCacheDir, $obsRegDir);
	}
    }
    if ($save) {
	if (open(my $fh, '>', $scanfile)) {
	    print $fh $doc->toString;
	    close $fh;
	    chmod 0666, $scanfile;
	} else {
	    &term("Unable to open:".$scanfile);
	}
    };
    print $doc->toString . "\n";
} else {  #scan file does not exist, create temporary xml-structure
    if (defined $param->{type} && $file) {
	my $doc = $parser->parse_string("<scan><scan_config/></scan>");
	my ($node) = $doc->findnodes("scan/scan_config");
	if ($param->{type}->[0] eq "model") {
	    my $parent = XML::LibXML::Element->new( 'model' );
	    $parent->setAttribute("file",$file);
	    $node->addChild( $parent );
	    if (!&scanModel($node, $file, $modelDir, $modelCacheDir, $modelRegDir)) {
		&term("File not found:$file");
	    };
	};
	if ($param->{type}->[0] eq "obs") {
	    my $parent = XML::LibXML::Element->new( 'obs' );
	    $parent->setAttribute("file",$file);
	    $node->addChild( $parent );
	    if (!&scanObs($node, $file, $obsDir, $obsCacheDir, $obsRegDir)) {
		&term("File not found:$file");
	    };
	}
	print $doc->toString . "\n";
    } else {
	&term("File not found:".$scanfile);
    }
}
sub scanModel {
    my $node =shift;
    my $file = shift;
    my $modelDir = shift;
    my $modelCacheDir = shift;
    my $modelRegDir = shift;
    my $modelFile = $modelDir . $file;
    my @models=();
    if ($node) {
	my @modelnodes=$node->findnodes("model");
	foreach my $model (@modelnodes) {
	    my $nodefile=$model->getAttribute("file");
	    if ($file) {
		if ($nodefile eq $file) {
		    push(@models,[$nodefile,$model]);
		} 
	    } else {
		push(@models,[$nodefile,$model]);
	    }
	}
    } else {
	push(@models,$file);
    };
    foreach my $model (@models) {
	my $lastScan = "today";
	my $lastAccess = "never";
	#
	# check lockfile
	#
	my $lockfilename="$lockDir/model_$file.lock";
	if ( not open(MLOCKFILE, ">$lockfilename") ) {
	    #&term("$myname couldnt open Lockfile: $lockfilename");
	} else {
	    chmod 0666, $lockfilename;
	    &term("$myname is already running.") unless (flock (MLOCKFILE,2+4));
	    #
	    # read model config file and get file filter and index variable...
	    #
	    my $base=basename($model->[0]);
	    my $xmlfile=$modelDir . $base;
	    my $xmloldfile = $modelOldDir . $base;
	    my $cachefile=$modelCacheDir . $base;
	    my $registerfile=$modelRegDir . $base;
	    if (compare($xmlfile,$xmloldfile) != 0) { # file has changed
		unlink $cachefile;
		unlink $registerfile;
		copy ($xmlfile,$xmloldfile);
	    };
	    # scan config file...
	    if ( ! -e $xmlfile) { &term("$myname unable to find: $xmlfile");}
	    # read config file into memory
	    my $parser = XML::LibXML->new();
	    my $modeldoc = $parser->parse_file($xmlfile);
	    if ( my ($modelnode)=$modeldoc->findnodes("model/model_config")) {
		my $filterDir=$modelnode->getAttribute("fileFilterDir");
		my $filter=$modelnode->getAttribute("fileFilter");
		my $index=$modelnode->getAttribute("index");
		# get list of processed files
		eval {
		    my $log=capture {
			my $fark=fark->open();
			$fark->clearModelFileStack($index);
			if (-e $cachefile) {
			    $fark->loadModelCache($cachefile);
			};
			$fark->updateModelRegister($registerfile,$filterDir,$filter);
			chmod 0666, $registerfile;
			$fark->makeModelCache($cachefile);
			chmod 0666, $cachefile;
			$fark->close();
			my $now = time();
			$lastScan=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($now));
			my $useModelFile=$useModelDir . $base; 
			if (-f $useModelFile) {
			    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) 
				= stat($useModelFile);
			    $lastAccess=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($atime));
			};
		    };
		};
		my $ret=$@;if ($ret) {term($ret);}
		#
	    } else {
		&term("$myname corrupted file: $xmlfile");
	    }
	    close(MLOCKFILE);
	    if ($node) {
		$model->[1]->setAttribute("lastScan",        $lastScan);
		$model->[1]->setAttribute("lastUsed",        $lastAccess);
	    }
	}
    }
    return @models;
};

sub scanObs {
    my $node =shift;
    my $file = shift;
    my $obsDir = shift;
    my $obsCacheDir = shift;
    my $obsRegDir = shift;
    my $obsFile = $obsDir . $file;
    my @obses=();
    if ($node) {
	my @obsnodes=$node->findnodes("obs");
	foreach my $obs (@obsnodes) {
	    my $nodefile=$obs->getAttribute("file");
	    if ($file) {
		if ($nodefile eq $file) {
		    push(@obses,[$nodefile,$obs]);
		}
	    } else {
		push(@obses,[$nodefile,$obs]);
	    }
	}
    } else {
	push(@obses,[$file]);
    };
    foreach my $obs (@obses) {
	my $lastScan = "today";
	my $lastAccess = "never";
	#
	# check lockfile...
	#
	my $lockfilename="$lockDir/obs_$file.lock";
	if ( not open(MLOCKFILE, ">$lockfilename") ) {
	    #&term("$myname couldnt open Lockfile: $lockfilename");
	} else {
	    chmod 0666, $lockfilename;
	    &term("$myname is already running") unless (flock (MLOCKFILE,2+4));
	    #
	    # read obs config file and get file filter and index variable...
	    #
	    my $base=basename($obs->[0]);
	    my $xmlfile=$obsDir . $base;
	    my $xmloldfile = $obsOldDir . $base;
	    my $cachefile=$obsCacheDir . $base;
	    my $registerfile=$obsRegDir . $base;
	    if (compare($xmlfile,$xmloldfile) != 0) { # file has changed
		unlink $cachefile;
		unlink $registerfile;
		copy ($xmlfile,$xmloldfile);
	    };
	    # scan config file...
	    if ( ! -e $xmlfile) { &term("$myname unable to find: $xmlfile");}
	    # read config file into memory
	    my $parser = XML::LibXML->new();
	    my $obsdoc = $parser->parse_file($xmlfile);
	    if ( my ($node)=$obsdoc->findnodes("obs/obs_config")) {
		my $filterDir=$node->getAttribute("fileFilterDir");
		my $filter=$node->getAttribute("fileFilter");
		my $tablepath=$node->getAttribute("tablePath");
		my $indexTarget=$node->getAttribute("indexTarget");
		my $indexExp=$node->getAttribute("indexExp");
		my $bufrType=$node->getAttribute("bufrType");
		my $subType=$node->getAttribute("subType");
		# get list of processed files
		eval {
		    my $log=capture {
			my $fark=fark->open();
			$fark->setObservationTablePath($tablepath);
			$fark->setObservationIndex($indexTarget,$indexExp);
			$fark->setObservationType($bufrType,$subType);
			my @targets=$node->findnodes("target");
			foreach my $trg (@targets) {
			    $fark->pushObservationTarget($trg->getAttribute("name"),
							 $trg->getAttribute("pos"),
							 $trg->getAttribute("descr"),
							 $trg->getAttribute("info"),
							 $trg->getAttribute("min"),
							 $trg->getAttribute("max"));
			}
			$fark->clearObservationFileStack();
			if (-e $cachefile) {
			    $fark->loadObservationCache($cachefile);
			};
			$fark->updateObservationRegister($registerfile,$filterDir,$filter);
			chmod 0666, $registerfile;
			$fark->makeObservationCache($cachefile);
			chmod 0666, $cachefile;
			$fark->close();
			my $now = time();
			$lastScan=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($now));
			my $useObsFile=$useObsDir . $base; 
			if (-f $useObsFile) {
			    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) 
				= stat($useObsFile);
			    $lastAccess=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($atime));
			};
		    };
		};
		my $ret=$@;if ($ret) {term($ret);}
	    } else {
		&term("$myname corrupted file: $xmlfile");
	    }
	    close(MLOCKFILE);
	    $obs->[1]->setAttribute("lastScan",        $lastScan);
	    $obs->[1]->setAttribute("lastUsed",        $lastAccess);
	}
    }
    return @obses;
};

sub check_path{
    my $path=shift;
    eval {
	my $log=capture {
	    if(!-d $path) {
		make_path $path; 
		chmod 0777, $path;
	    }
	}
    };my $ret=$@;if ($ret) {term($ret);}
}

sub term {
    my $msg=shift;
    $msg=~s/[^a-zA-Z0-9 _\-\+\.\,\/\:\[\]\(\)]/ /g;
    $msg=~s/ +/ /g;
    print "<error message='".$msg."'/>\n";
    exit 1;
}
