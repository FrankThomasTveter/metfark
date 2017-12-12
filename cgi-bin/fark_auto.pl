#!/usr/bin/perl -w
#
use ExtUtils::testlib;
use fark;
use farkdata;
use farkdir;
#
use strict;
use CGI;
#use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use Data::Dumper;
use POSIX 'strftime';
use Capture::Tiny 'capture';
use File::Basename;
use File::Compare;
use File::Copy;
#
my $user=$ENV{USERNAME} // "www";
my $pub="/metfark/pub";
#
my $debug=1;       # debug this script (0=omit output)
fark::debug(1);  # debug observations
fark::debug(2);  # debug models
fark::debug(3);  # debug colocation
fark::debug(4);  # debug plot
#fark::debug(5);  # debug parse
#
my $modelDir=     farkdir::getRootDir("model") || farkdir::term("Invalid root directory (model)");
my $modelOldDir=  farkdir::getRootDir("model_old") || farkdir::term("Invalid root directory (model_old)");
my $modelCacheDir=farkdir::getRootDir("model_cache") || farkdir::term("Invalid root directory (model_cache)");
my $modelRegDir=  farkdir::getRootDir("model_reg") || farkdir::term("Invalid root directory (model_reg)");
my $modelUseDir=  farkdir::getRootDir("model_use") || farkdir::term("Invalid root directory (model_use)");
my $modelLogDir=  farkdir::getRootDir("model_log") || farkdir::term("Invalid root directory (model_log)");
#
my $obsDir=       farkdir::getRootDir("obs") || farkdir::term("Invalid root directory (obs)");
my $obsOldDir=    farkdir::getRootDir("obs_old") || farkdir::term("Invalid root directory (obs_old)");
my $obsCacheDir=  farkdir::getRootDir("obs_cache") || farkdir::term("Invalid root directory (obs_cache)");
my $obsRegDir=    farkdir::getRootDir("obs_reg") || farkdir::term("Invalid root directory (obs_reg)");
my $obsUseDir=    farkdir::getRootDir("obs_use") || farkdir::term("Invalid root directory (obs_use)");
my $obsLogDir=    farkdir::getRootDir("obs_log") || farkdir::term("Invalid root directory (obs_log)");
#
my $colocDir=     farkdir::getRootDir("coloc") || farkdir::term("Invalid root directory (coloc)");
my $colocOldDir=  farkdir::getRootDir("coloc_old") || farkdir::term("Invalid root directory (coloc_old)");
my $colocUseDir=  farkdir::getRootDir("coloc_use") || farkdir::term("Invalid root directory (coloc_use)");
my $colocLogDir=  farkdir::getRootDir("coloc_log") || farkdir::term("Invalid root directory (coloc_log)");
#
my $plotDir=      farkdir::getRootDir("plot") || farkdir::term("Invalid root directory (plot)");
my $plotOldDir=   farkdir::getRootDir("plot_old") || farkdir::term("Invalid root directory (plot_old)");
my $plotUseDir=   farkdir::getRootDir("plot_use") || farkdir::term("Invalid root directory (plot_use)");
my $plotLogDir=   farkdir::getRootDir("plot_log") || farkdir::term("Invalid root directory (plot_log)");
#
my $autoDir=      farkdir::getRootDir("auto") || farkdir::term("Invalid root directory (auto)");
my $scriptDir=    farkdir::getRootDir("script") || farkdir::term("Invalid root directory (script)");
my $lockRoot=     farkdir::getRootDir("lock") || farkdir::term("Invalid root directory (lock)");
#
#
my $myname = basename($0);
#
my $ref=CGI->new();

print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
#
if($debug){print "Args:" . @ARGV . "\n";}
#
$XML::LibXML::skipXMLDeclaration = 1;
my $param= $ref->{param};
my $password=($param->{password}[0] // "");
if (! defined $param->{root}) {farkdir::term("Undefined root file.".Dumper($param))};
my $autoFile = $param->{root}[0] // "";
my $ipath = $param->{file}[0] // "";
my $cls = $param->{type}[0]//"undef";
my $save=0;
my $autopath=$autoDir . $autoFile;
my ($dir,$name)=farkdir::splitName($ipath);
my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
my $file = $loc . $name;
my $test = (defined $param->{test}[0]) ? 1 : 0;

# auto config file...
my $parser = XML::LibXML->new();
if (-f $autopath) {
    # read config file into memory
    my $doc = $parser->parse_file($autopath);
    if ( my ($node)=$doc->findnodes("auto/auto_config")) {
	my $pass=($node->getAttribute("password")//"");
	&updateTimes($node);
	$save=($pass eq $password);
	if (defined $param->{type}) {
	    if ($param->{type}->[0] eq "model") {
		if (!&autoModel($node, $file, $modelDir, $modelCacheDir, 
				$modelRegDir, $test, $modelLogDir)) {
		    farkdir::term("Error return from autoModel:$file");
		};
	    };
	    if ($param->{type}->[0] eq "obs") {
		if (! &autoObs($node, $file, $obsDir, $obsCacheDir, 
			       $obsRegDir, $test, $obsLogDir)) {
		    farkdir::term("Error return from autoObs:$file");
		};
	    }
	    if ($param->{type}->[0] eq "coloc") {
		if (! &autoColoc($node,$file, $colocDir, 
				$modelDir, $modelCacheDir, $obsDir, 
				$obsCacheDir, $test, $colocLogDir)) {
		    farkdir::term("Error return from autoColoc:$file");
		};
	    }
	    if ($param->{type}->[0] eq "plot") {
		if (! &autoPlot($node,$file, $plotDir, $colocDir, 
				$modelDir, $modelCacheDir, $obsDir, 
				$obsCacheDir, $test, $plotLogDir)) {
		    farkdir::term("Error return from autoPlot:$file");
		};
	    }
	} else {
	    &autoModel($node,"", $modelDir, $modelCacheDir, $modelRegDir, $test, $modelLogDir);
	    &autoObs($node,"", $obsDir, $obsCacheDir, $obsRegDir, $test, $obsLogDir);
	    &autoColoc($node,"", $colocDir, 
		       $modelDir, $modelCacheDir, $obsDir, $obsCacheDir, $test, $colocLogDir);
	    &autoPlot($node,"", $plotDir, $colocDir, 
		      $modelDir, $modelCacheDir, $obsDir, $obsCacheDir, $test, $plotLogDir);
	}
    }
    if ($save) {
	if (open(my $fh, '>', $autopath)) {
	    print $fh $doc->toString;
	    close $fh;
	    chmod 0666, $autopath;
	} else {
	    farkdir::term("Unable to open:".$autopath);
	}
    } else {
	farkdir::info("Processing complete.");
    };
    print $doc->toString . "\n";
} else {  #auto file does not exist, create temporary xml-structure
    if (defined $param->{type}) {
	my $doc = $parser->parse_string("<auto><auto_config/></auto>");
	my ($node) = $doc->findnodes("auto/auto_config");
	if ($param->{type}->[0] eq "model") {
	    my $parent = XML::LibXML::Element->new( 'model' );
	    $parent->setAttribute("file",$name);
	    $node->addChild( $parent );
	    if (!&autoModel($node, $file, $modelDir, $modelCacheDir, $modelRegDir, $test, $modelLogDir)) {
		farkdir::term("File not found:$file");
	    };
	};
	if ($param->{type}->[0] eq "obs") {
	    my $parent = XML::LibXML::Element->new( 'obs' );
	    $parent->setAttribute("file",$name);
	    $node->addChild( $parent );
	    if (!&autoObs($node, $file, $obsDir, $obsCacheDir, $obsRegDir, $test, $obsLogDir)) {
		farkdir::term("File not found:$file");
	    };
	}
	if ($param->{type}->[0] eq "coloc") {
	    my $parent = XML::LibXML::Element->new( 'coloc' );
	    $parent->setAttribute("file",$name);
	    $node->addChild( $parent );
	    if (!&autoColoc($node, $file, $colocDir,
			   $modelDir, $modelCacheDir, 
			   $obsDir, $obsCacheDir, $test, $colocLogDir)) {
		farkdir::term("File not found:$file");
	    };
	}
	if ($param->{type}->[0] eq "plot") {
	    my $parent = XML::LibXML::Element->new( 'plot' );
	    $parent->setAttribute("file",$name);
	    $node->addChild( $parent );
	    if (!&autoPlot($node, $file, $plotDir, $colocDir,
			   $modelDir, $modelCacheDir, 
			   $obsDir, $obsCacheDir, $test, $plotLogDir)) {
		farkdir::term("File not found:$file");
	    };
	}
	print $doc->toString . "\n";
    } else {
	farkdir::term("File not found:".$name);
    }
}
sub autoModel {
    my $node =shift;
    my $file = shift;
    my $modelDir = shift;
    my $modelCacheDir = shift;
    my $modelRegDir = shift;
    my $test = shift;
    my $modelLogDir = shift;
    my $modelFile = $modelDir . $file;
    my @models=();
    if ($node) {
	my @modelnodes=$node->findnodes("model");
	my $found=0;
	foreach my $model (@modelnodes) {
	    my $auto=(($model->getAttribute("auto")||"") eq "true");
	    my $nodefile=$model->getAttribute("file");
	    if ($file) {
		if ($nodefile eq $file) {
		    push(@models,[$nodefile,$model]);
		    $found++;
		} 
	    } elsif ($auto) {
		push(@models,[$nodefile,$model]);
	    }
	}
	if ($file && ! $found) {
	    my $model = XML::LibXML::Element->new( 'model' );
	    $model->setAttribute("file",$file);
	    $node->addChild( $model );
	    push(@models,[$file,$model]);
	}
    } elsif ($file) {
	push(@models,[$file]);
    };
    foreach my $modelRef (@models) {
	my $lastAuto = "";
	my $lastAccess = "";
	#
	# check lockfile
	#
	my $file=$modelRef->[0];
	my $lockfile = $lockRoot ."model/$file.lock";
	my ($dir,$name)=farkdir::splitName($lockfile);
	farkdir::makePath($dir); # make sure directory exists in case we create lockfile next
	if ( open(MLOCKFILE, ">$lockfile")  && flock (MLOCKFILE,2+4) ) {
	    if (!$test) {farkdir::touchFile($lockfile);}; # this defines processing start time
	    chmod 0666, $lockfile;
	    my $lastStart=time();
	    #
	    # read model config file and get file filter and index variable...
	    #
	    my $model=$modelRef->[0];
	    my $xmlfile=$modelDir . $model;
	    my $xmloldfile = $modelOldDir . $model;
	    my $cachefile=$modelCacheDir . $model;
	    my $registerfile=$modelRegDir . $model;
	    if (compare($xmlfile,$xmloldfile) != 0) { # file has changed
		if($debug){print "************ Unlinking '$registerfile' '$xmlfile' '$xmloldfile'\n";}
		unlink $cachefile;
		unlink $registerfile;
		my ($dir,$name)=farkdir::splitName($xmloldfile);
		farkdir::makePath($dir);
		copy ($xmlfile,$xmloldfile);
	    };
	    # auto config file...
	    if ( ! -e $xmlfile) { farkdir::term("$myname unable to find: $xmlfile");}
	    # read config file into memory
	    my $parser = XML::LibXML->new();
	    my $modeldoc = $parser->parse_file($xmlfile);
	    if ( my ($modelnode)=$modeldoc->findnodes("model/model_config")) {
		# get list of processed files
		if ($debug) {
		    print "Processing model $xmlfile\n";
		    processModel($xmlfile,
				 $modelnode,
				 $cachefile,
				 $registerfile,
				 $model,
				 $test,$modelLogDir);
		} else {
		    eval {
		        my $log=capture {
			    processModel($xmlfile,
					 $modelnode,
					 $cachefile,
					 $registerfile,
					 $model,
					 $test,$modelLogDir);
			};
		    };
		    my $ret=$@;if ($ret) {farkdir::term("$myname model file: $xmlfile: $ret");}
		}
	    } else {
		farkdir::term("$myname corrupted file: $xmlfile");
	    }
	    my $modelUseFile=$modelUseDir . $file; 
	    if (!$test) {farkdir::touchFile($modelUseFile);}; # this defines processing end time
	    my $lastStop=time();
	    $lastAuto=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($lastStart));
	    $lastAccess="**done** (".($lastStop-$lastStart) . "s)";
	    if (! $test && defined $modelRef->[1]) {
		$modelRef->[1]->setAttribute("last",        $lastAuto);
		$modelRef->[1]->setAttribute("info",        $lastAccess);
	    }
	}
	close(MLOCKFILE);
    }
    return @models;
};

sub processModel {
    my $xmlfile = shift;
    my $node = shift;
    my $cachefile = shift;
    my $registerfile = shift;
    my $file = shift;
    my $test = shift;
    my $logDir = shift;
    #
    my $logFile=$logDir . $file; 
    # make sure output file directories exist...
    ($dir,$name)=farkdir::splitName($logFile);
    farkdir::makePath($dir);
    #
    my $fark=fark->open();
    &setModelConfig($fark,$node,$cachefile,$test);
    #
    my $filterDir=$node->getAttribute("filterDir");
    my $filterFile=$node->getAttribute("filterFile");
    $fark->updateModelRegister($registerfile,$filterDir,$filterFile,$test);
    $fark->makeModelCache($cachefile,$test);
    #
    $fark->close();
    #
    if (! -e $cachefile) {
	farkdir::term("Unable to make cache file:'".$cachefile."'")
    };
    return;
}

sub autoObs {
    my $node =shift;
    my $file = shift;
    my $obsDir = shift;
    my $obsCacheDir = shift;
    my $obsRegDir = shift;
    my $test = shift;
    my $obsLogDir = shift;
    my $obsFile = $obsDir . $file;
    my @obses=();
    if ($node) {
	my @obsnodes=$node->findnodes("obs");
	my $found=0;
	foreach my $obs (@obsnodes) {
	    my $auto=($obs->getAttribute("auto")=="true");
	    my $nodefile=$obs->getAttribute("file");
	    if ($file) {
		if ($nodefile eq $file) {
		    push(@obses,[$nodefile,$obs]);
		    $found++;
		}
	    } elsif ($auto) {
		push(@obses,[$nodefile,$obs]);
	    }
	}
	if ($file && ! $found) {
	    my $obs = XML::LibXML::Element->new( 'obs' );
	    $obs->setAttribute("file",$file);
	    $node->addChild( $obs );
	    push(@obses,[$file,$obs]);
	}
    } elsif ($file) {
	push(@obses,[$file]);
    };
    foreach my $obsRef (@obses) {
	my $lastAuto = "";
	my $lastAccess = "";
	#
	# check lockfile...
	#
	my $lockfile=$lockRoot . "obs/$file.lock";
	my ($dir,$name)=farkdir::splitName($lockfile);
	farkdir::makePath($dir); # make sure directory exists in case we create lockfile next
	if ( open(MLOCKFILE, ">$lockfile") && flock (MLOCKFILE,2+4)) {
	    if (!$test) {farkdir::touchFile($lockfile);};
	    chmod 0666, $lockfile;
	    my $lastStart=time();
	    #
	    # read obs config file and get file filter and index variable...
	    #
	    my $obs=$obsRef->[0];
	    my $xmlfile=$obsDir . $obs;
	    my $xmloldfile = $obsOldDir . $obs;
	    my $cachefile=$obsCacheDir . $obs;
	    my $registerfile=$obsRegDir . $obs;
	    if (compare($xmlfile,$xmloldfile) != 0) { # file has changed
		if($debug){print "************ Unlinking '$registerfile' '$xmlfile' '$xmloldfile'\n";}
		unlink $cachefile;
		unlink $registerfile;
		my ($dir,$name)=farkdir::splitName($xmloldfile);
		farkdir::makePath($dir);
		copy ($xmlfile,$xmloldfile);
	    };
	    # auto config file...
	    if ( ! -e $xmlfile) { farkdir::term("$myname unable to find: $xmlfile");}
	    # read config file into memory
	    my $parser = XML::LibXML->new();
	    my $obsdoc = $parser->parse_file($xmlfile);
	    if ( my ($obsnode)=$obsdoc->findnodes("obs/obs_config")) {
		if ($debug) {
		    print "Processing obs $xmlfile\n";
		    processObs($xmlfile,
			       $obsnode,
			       $cachefile,
			       $registerfile,
			       $obs,
			       $test,$obsLogDir);
		} else {
		    eval {
		        my $log=capture {
			    processObs($xmlfile,
				       $obsnode,
				       $cachefile,
				       $registerfile,
				       $obs,
				       $test,$obsLogDir);
			};
		    };
		    my $ret=$@;if ($ret) {farkdir::term("$myname obs file: $xmlfile: $ret");}
		}
	    } else {
		farkdir::term("$myname corrupted file: $xmlfile");
	    }
	    my $obsUseFile=$obsUseDir . $file; 
	    if (!$test) {farkdir::touchFile($obsUseFile);}; # this defines processing end time
	    my $lastStop=time();
	    $lastAuto=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($lastStart));
	    $lastAccess="**done** (".($lastStop-$lastStart) . "s)";
	    if (! $test && defined $obsRef->[1]) {
		$obsRef->[1]->setAttribute("last",        $lastAuto);
		$obsRef->[1]->setAttribute("info",        $lastAccess);
	    };
	}
	close(MLOCKFILE);
    }
    return @obses;
};

sub processObs {
    my $xmlfile = shift;
    my $node = shift;
    my $cachefile = shift;
    my $registerfile = shift;
    my $file = shift;
    my $test = shift;
    my $logDir = shift;
    #
    my $logFile=$logDir . $file; 
    # make sure output file directories exist...
    ($dir,$name)=farkdir::splitName($logFile);
    farkdir::makePath($dir);
    #
    my $fark=fark->open();
    &setObsConfig($fark,$node,$cachefile,$test);
    #
    my $filterDir=$node->getAttribute("filterDir");
    my $filterFile=$node->getAttribute("filterFile");
    $fark->updateObservationRegister($registerfile,$filterDir,$filterFile,$test);
    $fark->makeObservationCache($cachefile,$test);
    #
    $fark->close();
    #
    if (! -e $cachefile) {
	farkdir::term("Unable to make cache file:'".$cachefile."'")
    };
    return;
}

sub autoColoc {
    my $node =shift;
    my $file = shift;
    my $colocDir = shift;
    my $modelDir = shift;
    my $modelCacheDir = shift;
    my $obsDir = shift;
    my $obsCacheDir = shift;
    my $test = shift;
    my $plotLogDir = shift;
    #
    my $colocFile = $colocDir . $file;
    my @coloces=();
    if ($node) {
	my @colocnodes=$node->findnodes("coloc");
	my $found=0;
	foreach my $coloc (@colocnodes) {
	    my $auto=(($coloc->getAttribute("auto")||"") eq "true");
	    my $nodefile=$coloc->getAttribute("file");
	    if ($file) {
		if ($nodefile eq $file) {
		    push(@coloces,[$nodefile,$coloc]);
		    $found++;
		}
	    } elsif ($auto) {
		push(@coloces,[$nodefile,$coloc]);
	    }
	}
	if ($file && ! $found) {
	    my $coloc = XML::LibXML::Element->new( 'coloc' );
	    $coloc->setAttribute("file",$file);
	    $node->addChild( $coloc );
	    push(@coloces,[$file,$coloc]);
	}
    } elsif ($file) {
	push(@coloces,[$file]);
    };
    foreach my $colocRef (@coloces) {
	my $lastAuto = "";
	my $lastAccess = "";
	#
	# check lockfile...
	#
	my $lockfile=$lockRoot . "coloc/$file.lock";
	my ($dir,$name)=farkdir::splitName($lockfile);
	farkdir::makePath($dir); # make sure directory exists in case we create lockfile next
	if ( open(MLOCKFILE, ">$lockfile") && flock (MLOCKFILE,2+4)) {
	    if (!$test) {farkdir::touchFile($lockfile);};
	    chmod 0666, $lockfile;
	    my $lastStart=time();
	    #
	    # read coloc config file and get file filter and index variable...
	    #
	    my $coloc=$colocRef->[0];
	    my $xmlfile=$colocDir . $coloc;
	    my $xmloldfile = $colocOldDir . $coloc;
	    if (compare($xmlfile,$xmloldfile) != 0) { # file has changed
		my ($dir,$name)=farkdir::splitName($xmloldfile);
		farkdir::makePath($dir);
		copy ($xmlfile,$xmloldfile);
	    };
	    # auto config file...
	    if ( ! -e $xmlfile) { farkdir::term("$myname unable to find: $xmlfile");}
	    # read config file into memory
	    my $parser = XML::LibXML->new();
	    if($debug){print "Reading $xmlfile\n";}
	    my $colocdoc = $parser->parse_file($xmlfile);
	    if ( my ($colocnode)=$colocdoc->findnodes("coloc/coloc_config")) {
		if ($debug) {
		    print "Processing coloc $xmlfile\n";
		    processColoc($xmlfile,
				 $colocnode,
				 $colocDir,
				 $modelDir,
				 $modelCacheDir,
				 $obsDir,
				 $obsCacheDir,
				 $coloc,
				 $test,$colocLogDir);
		} else {
		    eval {
		        my $log=capture {
			    processColoc($xmlfile,
					 $colocnode,
					 $colocDir,
					 $modelDir,
					 $modelCacheDir,
					 $obsDir,
					 $obsCacheDir,
					 $coloc,
					 $test,$colocLogDir);
			};
		    };
		    my $ret=$@;if ($ret) {farkdir::term("$myname coloc file: $xmlfile: $ret");}
		}
	    } else {
		farkdir::term("$myname corrupted file: $xmlfile");
	    }
	    my $colocUseFile=$colocUseDir . $file; 
	    if (!$test) {farkdir::touchFile($colocUseFile);}; # this defines processing end time
	    my $lastStop=time();
	    $lastAuto=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($lastStart));
	    $lastAccess="**done** (".($lastStop-$lastStart) . "s)";
	    if (! $test && defined $colocRef->[1]) {
		$colocRef->[1]->setAttribute("last",        $lastAuto);
		$colocRef->[1]->setAttribute("info",        $lastAccess);
	    };
	}
	close(MLOCKFILE);
    }
    return @coloces;
};

sub processColoc {
    my $xmlfile = shift;
    my $node = shift;
    my $colocDir = shift;
    my $modelDir = shift;
    my $modelCacheDir = shift;
    my $obsDir = shift;
    my $obsCacheDir = shift;
    my $file = shift;
    my $test = shift;
    my $logDir = shift;
    #
    my $logFile=$logDir . $file; 
    # make sure output file directories exist...
    ($dir,$name)=farkdir::splitName($logFile);
    farkdir::makePath($dir);
    #
    if($debug){print "processColoc Entering with '$xmlfile' '$file'\n";}
    my $fark=fark->open();
    &setColocConfig($fark,$node,$test);
    #
    # make the resulting XML
    print "Calling colocXML\n";
    my $xml       = $node->getAttribute("xml");
    my ($irc, $msg) = $fark->makeColocXML($xml,$test);
    #
    # close session
    $fark->close();
    if($debug){print "processColoc Exiting.\n";}
    return;
}

sub autoPlot {
    my $node =shift;
    my $file = shift;
    my $plotDir = shift;
    my $colocDir = shift;
    my $modelDir = shift;
    my $modelCacheDir = shift;
    my $obsDir = shift;
    my $obsCacheDir = shift;
    my $test = shift;
    my $plotLogDir = shift;
    #
    my $plotFile = $plotDir . $file;
    my @plotes=();
    if ($node) {
	my @plotnodes=$node->findnodes("plot");
	my $found=0;
	foreach my $plot (@plotnodes) {
	    my $auto=($plot->getAttribute("auto") eq "true");
	    my $nodefile=$plot->getAttribute("file");
	    if ($file) {
		if ($nodefile eq $file) {
		    push(@plotes,[$nodefile,$plot]);
		    $found++;
		}
	    } elsif ($auto) {
		push(@plotes,[$nodefile,$plot]);
	    }
	}
	if ($file && ! $found) {
	    my $plot = XML::LibXML::Element->new( 'plot' );
	    $plot->setAttribute("file",$file);
	    $node->addChild( $plot );
	    push(@plotes,[$file,$plot]);
	}
    } elsif ($file) {
	push(@plotes,[$file]);
    };
    foreach my $plotRef (@plotes) {
	my $lastAuto = "";
	my $lastAccess = "";
	#
	# check lockfile...
	#
	my $lockfile=$lockRoot . "plot/$file.lock";
	my ($dir,$name)=farkdir::splitName($lockfile);
	farkdir::makePath($dir); # make sure directory exists in case we create lockfile next
	if ( open(MLOCKFILE, ">$lockfile")  && flock (MLOCKFILE,2+4)) {
	    if (!$test) {farkdir::touchFile($lockfile);};
	    chmod 0666, $lockfile;
	    my $lastStart=time();
	    #
	    # read plot config file and get file filter and index variable...
	    #
	    my $plot=$plotRef->[0];
	    my $xmlfile=$plotDir . $plot;
	    my $xmloldfile = $plotOldDir . $plot;
	    if (compare($xmlfile,$xmloldfile) != 0) { # file has changed
		my ($dir,$name)=farkdir::splitName($xmloldfile);
		farkdir::makePath($dir);
		copy ($xmlfile,$xmloldfile);
	    };
	    # auto config file...
	    if ( ! -e $xmlfile) { farkdir::term("$myname unable to find: $xmlfile");}
	    # read config file into memory
	    my $parser = XML::LibXML->new();
	    if($debug){print "Reading $xmlfile\n";}
	    my $plotdoc = $parser->parse_file($xmlfile);
	    if ( my ($plotnode)=$plotdoc->findnodes("plot/plot_config")) {
		if ($debug) {
		    print "Processing plot $xmlfile\n";
		    processPlot($xmlfile,
				$plotnode,
				$colocDir,
				$modelDir,
				$modelCacheDir,
				$obsDir,
				$obsCacheDir,
				$plot,
				$test,$plotLogDir);
		} else {
		    eval {
		        my $log=capture {
			    processPlot($xmlfile,
					$plotnode,
					$colocDir,
					$modelDir,
					$modelCacheDir,
					$obsDir,
					$obsCacheDir,
					$plot,
					$test,$plotLogDir);
			};
		    };
		    my $ret=$@;if ($ret) {farkdir::term("$myname plot file: $xmlfile: $ret");}
		}
	    } else {
		farkdir::term("$myname corrupted file: $xmlfile");
	    }
	    my $plotUseFile=$plotUseDir . $file; 
	    if (!$test) {farkdir::touchFile($plotUseFile);}; # this defines processing end time
	    my $lastStop=time();
	    $lastAuto=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($lastStart));
	    $lastAccess="**done** (".($lastStop-$lastStart) . "s)";
	    if (! $test && defined $plotRef->[1]) {
		$plotRef->[1]->setAttribute("last",        $lastAuto);
		$plotRef->[1]->setAttribute("info",        $lastAccess);
	    };
	}
	close(MLOCKFILE);
    }
    return @plotes;
};

sub processPlot {
    my $xmlfile = shift;
    my $node = shift;
    my $colocDir = shift;
    my $modelDir = shift;
    my $modelCacheDir = shift;
    my $obsDir = shift;
    my $obsCacheDir = shift;
    my $file = shift;
    my $test = shift;
    my $logDir = shift;
    #
    my $logFile=$logDir . $file; 
    # make sure output file directories exist...
    ($dir,$name)=farkdir::splitName($logFile);
    farkdir::makePath($dir);
    #
    if($debug){print "processPlot Entering with '$xmlfile' '$file'\n";}
    my $table=$node->getAttribute("table");
    my $graphics=$node->getAttribute("graphics");
    my $cat=$node->getAttribute("cat");
    #print "Processing plot: $xmlfile\n";
    my $fark=fark->open();
    $fark->setPlotType($cat);
    $fark->clearPlotAttributeStack();
    my @attrs=$node->findnodes("attribute");
    foreach my $attr (@attrs) {
	$fark->pushPlotAttribute($attr->getAttribute("name"),
				 $attr->getAttribute("value"));
    }
    my @cols=$node->findnodes("column");
    if($debug){print "Set coloc model obs\n";}
    $fark->clearPlotSetStack();
    my @sets=$node->findnodes("set");
    foreach my $trg (@sets) {
	# load XML-data into coloc-, model- and obs-modules
	my $colocfile=$colocDir.$trg->getAttribute("coloc");
	if (!-e $colocfile) {farkdir::term("Undefined coloc file '$colocfile'");};
	my $parser = XML::LibXML->new();
	my $doc = $parser->parse_file($colocfile);
	if ( my ($node) = $doc->findnodes("coloc/coloc_config")) {
	    eval {
		&setColocConfig($fark,$node,$test); # set colocation config parameters
	    };
	    my $mret=$@;if ($mret) {
		my $modelFile = $node->getAttribute("modelFile");
		my $obsFile   = $node->getAttribute("obsFile");
		die("model:$modelFile obs:$obsFile $mret");
	    };
	    # push columns
	    if($debug){print "Setting columns.\n";};
	    $fark->clearPlotColumn();
	    my @colv=$trg->findnodes("column");
	    for ( my $i = 0; $i < @cols; $i++) {
		my $nam=$cols[$i]->getAttribute("name");
		my $val=$colv[$i]->getAttribute("value");
		if($debug){print "Setting column: $nam -> $val\n";};
		$fark->pushPlotColumn($nam,$val);
	    }
	    # push all the data onto the plot-stack...
	    if($debug){print "Push data to plot-stack obs\n";}
	    $fark->pushPlotSet($trg->getAttribute("name"),
			       $trg->getAttribute("legend"));
	}
    }
    #
    # colocate and generate table file...
    if($debug){print "****** Make table\n";}
    my ($tablefile, $plotfile) = $fark->makePlotTable($table,$graphics,$test); 
    #
    if($debug){print "****** Make graphics '$tablefile' '$plotfile'\n";}

    my ($root, $loc, $priv) = farkdir::splitDir( $scriptDir, "script" );
    my $fpath=$root . $loc . $cat;
    my $cmd="Rscript --vanilla $fpath $tablefile $plotfile $test";
    if ($debug) {print "Executing graphics command: '$cmd'\n";};
    my $log="";
    eval {
	$log=capture {
	    system $cmd;
	};
    };
    my $ret=$@;if ($ret) {
	open(my $fh, '>', $plotfile) or farkdir::term("$myname Unable to open: $plotfile ($ret)");
	print($fh,$log);
	close($fh);
	farkdir::term("$myname Command error: $cmd: $ret");
    }
    #
    $fark->close();
    if($debug){print "processPlot Exiting.\n";}
    return;
}

sub updateTimes {
    my $node=shift;
    updateTime($node,"model");
    updateTime($node,"obs");
    updateTime($node,"coloc");
    updateTime($node,"plot");
}
sub updateTime {
    my $node= shift;
    my $cls = shift;
    my $clsUseDir=    farkdir::getRootDir($cls."_use") || farkdir::term("Invalid root directory (use)");
    my @clss=$node->findnodes("cls");
    foreach my $cls (@clss) {
	my $file=$cls->getAttribute("file");
	my $lastAuto="";
	my $lastAccess="";
	my $lastStart=0;
	my $lastStop=0;
	my $clsUseFile=$clsUseDir . $file; 
	#print "Checking $clsUseFile\n";
	if (-f $clsUseFile) {
	    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
		$mtime,$ctime,$blksize,$blocks) = stat($clsUseFile);
	    $lastStop=$atime;
	};
	my $lockfilename=$lockRoot."$cls/$file.lock";
	if (-f $lockfilename) {
	    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
		$mtime,$ctime,$blksize,$blocks) = stat($lockfilename);
	    $lastAuto=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($atime));
	    $lastStart=$atime;
	    if ( not open(MLOCKFILE, ">$lockfilename") ) {
	    } elsif (flock (MLOCKFILE,2+4)) {
		my $duration = $lastStop-$lastStart;
		if ($duration < 0) {
		    $lastAccess="**abort**";
		} else {
		    $lastAccess="**done** (".$duration . "s)";
		}
	    } else {
		my $duration = time()-$lastStart;
		$lastAccess="**running** (".$duration . "s)";
	    };	
	    close(MLOCKFILE);
	};
	$cls->setAttribute("last",        $lastAuto);
	$cls->setAttribute("info",        $lastAccess);
    }
};

sub setModelConfig {
    my $fark = shift;
    my $node = shift;
    my $cachefile = shift;
    my $test = shift;
    my $indexTarget=$node->getAttribute("indexTarget");
    my $indexVariable=$node->getAttribute("indexVariable");
    print "Clear model file stack, '$indexVariable'\n";
    $fark->clearModelFileStack($indexVariable); # clear model file stack
    print "Setting model cache '$cachefile'\n";
    $fark->setModelCache($cachefile);
    print "Setting model index '$indexTarget' '$indexVariable'\n";
    $fark->setModelIndex($indexTarget,$indexVariable);
    if (-e $cachefile) {
	$fark->loadModelCache($cachefile);# load cached model file stack
    };
}

sub setObsConfig {
    my $fark = shift;
    my $node = shift;
    my $cachefile = shift;
    my $test = shift;
    #
    $fark->clearObservationFileStack(); # clear observation file stack
    my $tablepath=  ($node->getAttribute("tablePath")//"");
    my $indexTarget=($node->getAttribute("indexTarget")//"");
    my $indexExp=   ($node->getAttribute("indexExp")//"");
    my $bufrType=   ($node->getAttribute("bufrType")//"999");
    my $subType=    ($node->getAttribute("subType")//"999");
    print "Setting obs\n";
    $fark->setObservationTablePath($tablepath);
    if ( $indexTarget && $indexExp) {
	if($debug){print "Obs index: '$indexTarget' -> '$indexExp'\n";}
	$fark->setObservationIndex($indexTarget,$indexExp);
    };
    $fark->setObservationType($bufrType,$subType);
    $fark->clearObservationTargetStack();
    my @targets=$node->findnodes("target");
    foreach my $trg (@targets) {
	if($debug){print "Obs target '".$trg->getAttribute("name")."'\n";}
	$fark->pushObservationTarget($trg->getAttribute("name"),
				     $trg->getAttribute("pos"),
				     $trg->getAttribute("descr"),
				     $trg->getAttribute("info"),
				     $trg->getAttribute("min"),
				     $trg->getAttribute("max"));
    }
    $fark->clearObservationFileStack();
    $fark->setObservationCache($cachefile);
    if (-e $cachefile) {
	$fark->loadObservationCache($cachefile,$test);
    };
}

sub setColocConfig {
    my $fark = shift;
    my $node = shift;
    my $test = shift;
    #
    print "Setting coloc\n";
    my $modelFile = $node->getAttribute("modelFile");
    my $obsFile   = $node->getAttribute("obsFile");
    # get config from model file
    $fark->clearModelTargetStack();
    $fark->clearObservationTargetStack();
    if (length($modelFile//"")) {
	my $modelCache=$modelCacheDir.$modelFile;
	my $modelConfig = $modelDir.$modelFile;
	my $parser = XML::LibXML->new();
	if (-e $modelConfig) { # read config file parameters into memory
	    my $doc = $parser->parse_file($modelConfig);
	    if ((my $node)=$doc->findnodes("model/model_config")) {
		&setModelConfig($fark,$node,$modelCache,$test);
		if (! -e $modelCache) {
		    farkdir::term("Mising model cache:'".$modelCache."'")
		};
	    };
	}
    };
    # get config from obs file
    if (length($obsFile//"")) { # initialise any observation processing
	my $obs=0;
	#print "Calling clearObservationFileStack\n";
	my $obsCache=$obsCacheDir.$obsFile;
	#print "Calling loadObservationCache\n";
	my $obsConfig = $obsDir.$obsFile;
	my $parser = XML::LibXML->new();
	if (-e $obsConfig) {     # read config file parameters into memory
	    my $doc = $parser->parse_file($obsConfig);
	    if ((my $node)=$doc->findnodes("obs/obs_config")) {
		&setObsConfig($fark,$node,$obsCache,$test);
		if (! -e $obsCache) {
		    farkdir::term("Mising model cache:'".$obsCache."'")
		};
	    };
	};
    };
    # get config from coloc file
    my $filter = $node->getAttribute("filter")//"";
    $fark->setModelFilter($filter);
    if (length($modelFile//"")) {
	my @oldNodes=$node->findnodes("modelTarget");
	foreach my $oldnode (@oldNodes) {
	    print "Setting model target ".$oldnode->getAttribute("name")."\n";
	    $fark->pushModelTarget( ($oldnode->getAttribute("name")//""),
				    ($oldnode->getAttribute("variable")//""),
				    ($oldnode->getAttribute("min")//""),
				    ($oldnode->getAttribute("max")//""));
	};
    };
    if (length($modelFile//"") && ! length($obsFile//"")) {
	my @oldDefault=$node->findnodes("modelDefault");
	foreach my $default (@oldDefault) {
	    my @oldDefs=$default->findnodes("def");
	    if (@oldDefs) {
		foreach my $def (@oldDefs) {
		    print "Setting model default ".$def->getAttribute("name")."\n";
		    $fark->addModelDefault(($def->getAttribute("name")//""),
					   ($def->getAttribute("value")//""));
		}
		$fark->pushModelDefault(); # "target:value"
	    }
	};
    };
    if (length($obsFile//"")) {
	my @oldNodes=$node->findnodes("obsTarget");
	foreach my $oldnode (@oldNodes) {
	    print "Setting obs target ".$oldnode->getAttribute("name")."\n";
	    $fark->pushObservationTarget(($oldnode->getAttribute("name")//""),
					 ($oldnode->getAttribute("pos"//"")),
					 ($oldnode->getAttribute("descr")//""),
					 ($oldnode->getAttribute("info")//""),
					 ($oldnode->getAttribute("min")//""),
					 ($oldnode->getAttribute("max")//""));
	}
    };
    if (length($obsFile//"") && length($modelFile//"")) {
	my @oldNodes=$node->findnodes("matchRules");
	foreach my $oldnode (@oldNodes) {
	    print "Setting match rule '".
		$oldnode->getAttribute("name")."' -> '".
		$oldnode->getAttribute("expression")."'\n";
	    $fark->pushMatchRule(($oldnode->getAttribute("name")//""),
				 ($oldnode->getAttribute("expression")//""),
				 ($oldnode->getAttribute("min")//""),
				 ($oldnode->getAttribute("max")//""));
	};
    }
    my $modelStart= ($node->getAttribute("modelStart"//""));
    my $modelStop = ($node->getAttribute("modelStop")//"");
    my $obsStart  = ($node->getAttribute("obsStart")//"");
    my $obsStop   = ($node->getAttribute("obsStop")//"");
    my $obsFilter = ($node->getAttribute("obsFilter")//"");
    my $modelFilter = ($node->getAttribute("modelFilter")//"");
    if (length($modelStart//"") ||  length($modelStop//"")) {
	if($debug){print "Setting model index limits '$modelStart' '$modelStop'\n";}
	$fark->setModelIndexLimits($modelStart, $modelStop); # "target:value"
    };
    if (length($obsStart//"") || length($obsStop//"")) {
	if($debug){print "Setting obs index limits '$obsStart' '$obsStop'\n";}
	$fark->setObservationIndexLimits($obsStart, $obsStop); # "target:value"
    };
    if (length($obsFilter//"")) {
	if($debug){print "Setting obs filter '$obsFilter'\n";}
	$fark->setObservationFilter($obsFilter); # "target:value"
    };
    if (length($modelFilter//"")) {
	if($debug){print "Setting colocation filter '$modelFilter'\n";}
	$fark->setModelFilter($modelFilter); # "target:value"
    };
    if($debug){print "setColocConfig done.\n";}
}
