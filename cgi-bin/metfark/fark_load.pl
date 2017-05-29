#!/usr/bin/perl -w
#
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use File::Basename;
use Capture::Tiny 'capture';
use File::Path qw( make_path );
use POSIX 'strftime';
use Data::Dumper;
#
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
my $user=$ENV{USERNAME} // "www";
$user="www";
my $pub="/metfark/pub";
my $modelDir="$pub/model/";check_path($modelDir);
my $obsDir="$pub/obs/";check_path($obsDir);
my $scanDir="$pub/scan/";check_path($scanDir);
my $colocDir="$pub/coloc/";check_path($colocDir);
my $useModelDir="$pub/use/model/";check_path($useModelDir);
my $useObsDir="$pub/use/obs/";check_path($useObsDir);
my $lockDir="$pub/lock"; check_path($lockDir);
#
#
my $ref=CGI->new();
#
$XML::LibXML::skipXMLDeclaration = 1;
my $param= $ref->{param};
if (! defined $param->{type}) {&term("Undefined type.".Dumper($param))};
if ($param->{type}->[0] eq "model") {
    &loadModel($param, $modelDir);
} elsif ($param->{type}->[0] eq "obs") {
    &loadObs($param, $obsDir);
} elsif ($param->{type}->[0] eq "scan") {
    &loadScan($param, $scanDir);
} elsif ($param->{type}->[0] eq "coloc") {
    &loadColoc($param, $colocDir);
}

sub loadModel {
    my $param =shift;
    my $dir = shift;
    # make a new document
    my $new = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $parent = XML::LibXML::Element->new( 'model' );
    $new->addChild( $parent );
    # load model data
    my $parser = XML::LibXML->new();
    #$parser->expand_entities( 0 ); # leave entities alone
    if (opendir (DIR, $dir)) {
	while (my $file = readdir(DIR)) {
	    my $path=$dir . $file;
	    if (-f $path) {
		my $doc = $parser->parse_file($path);
		my ($node)=$doc->findnodes("model/model_config");
		if ( $node ) {$node->removeAttribute("password");} # remove passwords
		$parent->addChild( $node );
	    }
	}
	closedir(DIR);
    }
    print $parent->toString . "\n";
};

sub loadObs {
    my $param =shift;
    my $dir = shift;
    # make a new document
    my $new = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $parent = XML::LibXML::Element->new( 'obs' );
    $new->addChild( $parent );
    # load obs data
    my $parser = XML::LibXML->new();
    #$parser->expand_entities( 0 ); # leave entities alone
    if (opendir (DIR, $dir)) {
	while (my $file = readdir(DIR)) {
	    my $path=$dir . $file;
	    if (-f $path) {
		my $doc = $parser->parse_file($path);
		my ($node)=$doc->findnodes("obs/obs_config");
		if ( $node ) {$node->removeAttribute("password");} # remove passwords
		$parent->addChild( $node );
	    }
	}
	closedir(DIR);
    }
    print $parent->toString . "\n";
};

sub loadScan {
    my $param =shift;
    my $dir = shift;
    my $file=($param->{root}->[0]||"scan.cfg");
    # make a new document
    my $new = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $parent = XML::LibXML::Element->new( 'scan' );
    $new->addChild( $parent );
    # load obs data
    my $parser = XML::LibXML->new();
    #$parser->expand_entities( 0 ); # leave entities alone
    my $path=$dir . $file;
    if (-f $path) {
	my $doc = $parser->parse_file($path);
	my ($node)=$doc->findnodes("scan/scan_config");
	if ( $node ) {
	    $node->removeAttribute("password"); # remove passwords
	    my @models=$node->findnodes("model");
	    foreach my $model (@models) {
		my $file=$model->getAttribute("file");
		my $lastAccess="never";
		my $useModelFile=$useModelDir . $file; 
		if (-f $useModelFile) {
		    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) 
			= stat($useModelFile);
		    $lastAccess=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($atime));
		};
		$model->setAttribute("lastUsed",        $lastAccess);
		my $lockfilename="$lockDir/model_$file.lock";
		if ( not open(MLOCKFILE, ">$lockfilename") ) {
		    $model->setAttribute("status","");
		} elsif (flock (MLOCKFILE,2+4)) {
		    $model->setAttribute("status","");
		} else {
		    $model->setAttribute("status","running");
		};	
		close(MLOCKFILE);
	    }
	    my @obss=$node->findnodes("obs");
	    foreach my $obs (@obss) {
		my $file=$obs->getAttribute("file");
		my $lastAccess="never";
		my $useObsFile=$useObsDir . $file; 
		if (-f $useObsFile) {
		    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) 
			= stat($useObsFile);
		    $lastAccess=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($atime));
		};
		my $lockfilename="$lockDir/obs_$file.lock";
		if ( not open(MLOCKFILE, ">$lockfilename") ) {
		    $obs->setAttribute("status","");
		} elsif (flock (MLOCKFILE,2+4)) {
		    $obs->setAttribute("status","");
		} else {
		    $obs->setAttribute("status","running");
		};	
		close(MLOCKFILE);
	    }
	}
	$parent->addChild( $node );
    } else {
	&term("Undefined file:".Dumper($param))
    };
    print $parent->toString . "\n";
};

sub loadColoc {
    my $param =shift;
    my $dir = shift;
    # make a new document
    my $new = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $parent = XML::LibXML::Element->new( 'coloc' );
    $new->addChild( $parent );
    my $parser = XML::LibXML->new();
    if (opendir (DIR, $dir)) {
	while (my $file = readdir(DIR)) {
	    my $path=$dir . $file;
	    if (-f $path) {
		my $doc = $parser->parse_file($path);
		my ($node)=$doc->findnodes("coloc/coloc_config");
		if ( $node ) {
		    $node->removeAttribute("password");  # remove passwords
		}
		$parent->addChild( $node );
	    }
	}
	closedir(DIR);
    }
    print $parent->toString . "\n";
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
