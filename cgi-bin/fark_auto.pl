#!/usr/bin/perl -w
#
use ExtUtils::testlib;
use fark;
use farkdata;
use farkdir;
#
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use Data::Dumper;
use POSIX 'strftime';
use Capture::Tiny 'capture';
use File::Basename;
use File::Compare;
use File::Copy;
#
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
my $debug=0;
my $user=$ENV{USERNAME} // "www";
my $pub="/metfark/pub";
#
my $modelDir=     farkdir::getRootDir("model");
my $modelOldDir=  farkdir::getRootDir("model_old");
my $modelCacheDir=farkdir::getRootDir("model_cache");
my $modelRegDir=  farkdir::getRootDir("model_reg");
my $useModelDir=  farkdir::getRootDir("model_use");
#
my $obsDir=       farkdir::getRootDir("obs");
my $obsOldDir=    farkdir::getRootDir("obs_old");
my $obsCacheDir=  farkdir::getRootDir("obs_cache");
my $obsRegDir=    farkdir::getRootDir("obs_reg");
my $useObsDir=    farkdir::getRootDir("obs_use");
#
my $colocDir=     farkdir::getRootDir("coloc");
#
my $plotDir=      farkdir::getRootDir("plot");
my $plotOldDir=   farkdir::getRootDir("plot_old");
my $usePlotDir=   farkdir::getRootDir("plot_use");
#
my $autoDir=      farkdir::getRootDir("auto");
my $lockRoot=     farkdir::getRootDir("lock");
#
#
my $myname = basename($0);
#
my $ref=CGI->new();

print "Args:" . @ARGV . "\n";

#
$XML::LibXML::skipXMLDeclaration = 1;
my $param= $ref->{param};
my $password=($param->{password}[0] // "");
if (! defined $param->{root}) {farkdir::term("Undefined root file.".Dumper($param))};
my $path = $param->{root}[0] // "";
my $ipath = $param->{file}[0] // "";
my ($rootDir, $rootName) = farkdir::splitName($path);
my $save=0;
my $cls = $param->{type}[0]//"undef";
my $autofile=$autoDir . $rootName;
my ($dir,$name)=farkdir::splitName($ipath);
my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
my $file = $loc . $name;
my $lockDir=$lockRoot . $cls . "/";
# auto config file...
my $parser = XML::LibXML->new();
if (-f $autofile) {
    # read config file into memory
    my $doc = $parser->parse_file($autofile);
    if ( my ($node)=$doc->findnodes("auto/auto_config")) {
	my $pass=($node->getAttribute("password")||"");
	$save=($pass eq $password);
	if (defined $param->{type}) {
	    if ($param->{type}->[0] eq "model") {
		if (!&autoModel($node, $file, $modelDir, $modelCacheDir, $modelRegDir)) {
		    farkdir::term("Error return from autoModel:$file");
		};
	    };
	    if ($param->{type}->[0] eq "obs") {
		if (! &autoObs($node, $file, $obsDir, $obsCacheDir, $obsRegDir)) {
		    farkdir::term("Error return from autoObs:$file");
		};
	    }
	    if ($param->{type}->[0] eq "plot") {
		if (! &autoPlot($node,$file, $plotDir, $colocDir, 
				$modelDir, $modelCacheDir, $obsDir, $obsCacheDir)) {
		    farkdir::term("Error return from autoPlot:$file");
		};
	    }
	} else {
	    &autoModel($node,"", $modelDir, $modelCacheDir, $modelRegDir);
	    &autoObs($node,"", $obsDir, $obsCacheDir, $obsRegDir);
	    &autoPlot($node,"", $plotDir, $colocDir, 
		      $modelDir, $modelCacheDir, $obsDir, $obsCacheDir);
	}
    }
    if ($save) {
	if (open(my $fh, '>', $autofile)) {
	    print $fh $doc->toString;
	    close $fh;
	    chmod 0666, $autofile;
	} else {
	    farkdir::term("Unable to open:".$autofile);
	}
    };
    print $doc->toString . "\n";
} else {  #auto file does not exist, create temporary xml-structure
    if (defined $param->{type} && $path) {
	my $doc = $parser->parse_string("<auto><auto_config/></auto>");
	my ($node) = $doc->findnodes("auto/auto_config");
	if ($param->{type}->[0] eq "model") {
	    my $parent = XML::LibXML::Element->new( 'model' );
	    $parent->setAttribute("file",$name);
	    $node->addChild( $parent );
	    if (!&autoModel($node, $file, $modelDir, $modelCacheDir, $modelRegDir)) {
		farkdir::term("File not found:$file");
	    };
	};
	if ($param->{type}->[0] eq "obs") {
	    my $parent = XML::LibXML::Element->new( 'obs' );
	    $parent->setAttribute("file",$name);
	    $node->addChild( $parent );
	    if (!&autoObs($node, $file, $obsDir, $obsCacheDir, $obsRegDir)) {
		farkdir::term("File not found:$file");
	    };
	}
	if ($param->{type}->[0] eq "plot") {
	    my $parent = XML::LibXML::Element->new( 'plot' );
	    $parent->setAttribute("file",$name);
	    $node->addChild( $parent );
	    if (!&autoPlot($node, $file, $plotDir, $colocDir,
			   $modelDir, $modelCacheDir, 
			   $obsDir, $obsCacheDir)) {
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
    foreach my $modelRef (@models) {
	my $lastAuto = "Today";
	my $lastAccess = "Never used";
	#
	# check lockfile
	#
	my $lockfile = $lockDir . "$file.lock";
	farkdir::touch($lockfile);
	if ( not open(MLOCKFILE, ">$lockfile") ) {
	    farkdir::term("$myname couldnt open Lockfile: $lockfile");
	} else {
	    chmod 0666, $lockfile;
	    farkdir::term("$myname is already running.") unless (flock (MLOCKFILE,2+4));
	    #
	    # read model config file and get file filter and index variable...
	    #
	    my $model=$modelRef->[0];
	    my $xmlfile=$modelDir . $model;
	    my $xmloldfile = $modelOldDir . $model;
	    my $cachefile=$modelCacheDir . $model;
	    my $registerfile=$modelRegDir . $model;
	    if (compare($xmlfile,$xmloldfile) != 0) { # file has changed
		unlink $cachefile;
		unlink $registerfile;
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
		    ($lastAuto,$lastAccess)=
			processModel($xmlfile,
				     $modelnode,
				     $cachefile,
				     $registerfile,
				     $useModelDir,
				     $model,
				     $lastAuto,
				     $lastAccess);
		} else {
		    eval {
		        my $log=capture {
			    ($lastAuto,$lastAccess)=
				processModel($xmlfile,
					     $modelnode,
					     $cachefile,
					     $registerfile,
					     $useModelDir,
					     $model,
					     $lastAuto,
					     $lastAccess);
			};
		    };
		    my $ret=$@;if ($ret) {farkdir::term("$myname model file: $xmlfile: $ret");}
		}
	    } else {
		farkdir::term("$myname corrupted file: $xmlfile");
	    }
	    close(MLOCKFILE);
	    if ($node) {
		$modelRef->[1]->setAttribute("last",        $lastAuto);
		$modelRef->[1]->setAttribute("info",        $lastAccess);
	    }
	}
    }
    return @models;
};

sub processModel {
    my $xmlfile = shift;
    my $node = shift;
    my $cachefile = shift;
    my $registerfile = shift;
    my $useModelDir = shift;
    my $file = shift;
    my $lastAuto = shift;
    my $lastAccess = shift;
    #
    my $filterDir=$node->getAttribute("filterDir");
    my $filterFile=$node->getAttribute("filterFile");
    my $index=$node->getAttribute("index");
    #print "Processing model: $xmlfile\n";
    my $fark=fark->open();
    $fark->clearModelFileStack($index);
    if (-e $cachefile) {
	$fark->loadModelCache($cachefile);
    };
    $fark->updateModelRegister($registerfile,$filterDir,$filterFile);
    chmod 0666, $registerfile;
    $fark->makeModelCache($cachefile);
    chmod 0666, $cachefile;
    $fark->close();
    my $now = time();
    $lastAuto=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($now));
    my $useModelFile=$useModelDir . $file; 
    if (-f $useModelFile) {
	my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
	    $atime,$mtime,$ctime,$blksize,$blocks) 
	    = stat($useModelFile);
	$lastAccess=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($atime));
    };
    return ($lastAuto,$lastAccess);
}

sub autoObs {
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
    foreach my $obsRef (@obses) {
	my $lastAuto = "Today";
	my $lastAccess = "Never used";
	#
	# check lockfile...
	#
	my $lockfile=$lockDir . "$file.lock";
	farkdir::touch($lockfile);
	if ( not open(MLOCKFILE, ">$lockfile") ) {
	    farkdir::term("$myname couldnt open Lockfile: $lockfile");
	} else {
	    chmod 0666, $lockfile;
	    farkdir::term("$myname is already running") unless (flock (MLOCKFILE,2+4));
	    #
	    # read obs config file and get file filter and index variable...
	    #
	    my $obs=$obsRef->[0];
	    my $xmlfile=$obsDir . $obs;
	    my $xmloldfile = $obsOldDir . $obs;
	    my $cachefile=$obsCacheDir . $obs;
	    my $registerfile=$obsRegDir . $obs;
	    if (compare($xmlfile,$xmloldfile) != 0) { # file has changed
		unlink $cachefile;
		unlink $registerfile;
		copy ($xmlfile,$xmloldfile);
	    };
	    # auto config file...
	    if ( ! -e $xmlfile) { farkdir::term("$myname unable to find: $xmlfile");}
	    # read config file into memory
	    my $parser = XML::LibXML->new();
	    my $obsdoc = $parser->parse_file($xmlfile);
	    if ( my ($obsnode)=$obsdoc->findnodes("obs/obs_config")) {
		if ($debug) {
		    ($lastAuto,$lastAccess)=
			processObs($xmlfile,
				   $obsnode,
				   $cachefile,
				   $registerfile,
				   $useObsDir,
				   $obs,
				   $lastAuto,
				   $lastAccess);
		} else {
		    eval {
		        my $log=capture {
			    ($lastAuto,$lastAccess)=
				processObs($xmlfile,
					   $obsnode,
					   $cachefile,
					   $registerfile,
					   $useObsDir,
					   $obs,
					   $lastAuto,
					   $lastAccess);
			};
		    };
		    my $ret=$@;if ($ret) {farkdir::term("$myname obs file: $xmlfile: $ret");}
		}
	    } else {
		farkdir::term("$myname corrupted file: $xmlfile");
	    }
	    close(MLOCKFILE);
	    $obs->[1]->setAttribute("last",        $lastAuto);
	    $obs->[1]->setAttribute("info",        $lastAccess);
	}
    }
    return @obses;
};

sub processObs {
    my $xmlfile = shift;
    my $node = shift;
    my $cachefile = shift;
    my $registerfile = shift;
    my $useObsDir = shift;
    my $file = shift;
    my $lastAuto = shift;
    my $lastAccess = shift;
    #
    my $filterDir=$node->getAttribute("filterDir");
    my $filterFile=$node->getAttribute("filterFile");
    my $tablepath=$node->getAttribute("tablePath");
    my $indexTarget=$node->getAttribute("indexTarget");
    my $indexExp=$node->getAttribute("indexExp");
    my $bufrType=$node->getAttribute("bufrType");
    my $subType=$node->getAttribute("subType");
    #print "Processing obs: $xmlfile\n";
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
    $fark->updateObservationRegister($registerfile,$filterDir,$filterFile);
    chmod 0666, $registerfile;
    $fark->makeObservationCache($cachefile);
    chmod 0666, $cachefile;
    $fark->close();
    my $now = time();
    $lastAuto=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($now));
    my $useObsFile=$useObsDir . $file; 
    if (-f $useObsFile) {
	my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
	    $atime,$mtime,$ctime,$blksize,$blocks) 
	    = stat($useObsFile);
	$lastAccess=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($atime));
    };
    return ($lastAuto,$lastAccess);
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
    #
    my $plotFile = $plotDir . $file;
    my @plotes=();
    if ($node) {
	my @plotnodes=$node->findnodes("plot");
	foreach my $plot (@plotnodes) {
	    my $nodefile=$plot->getAttribute("file");
	    if ($file) {
		if ($nodefile eq $file) {
		    push(@plotes,[$nodefile,$plot]);
		}
	    } else {
		push(@plotes,[$nodefile,$plot]);
	    }
	}
    } else {
	push(@plotes,[$file]);
    };
    foreach my $plotRef (@plotes) {
	my $lastAuto = "Today";
	my $lastAccess = "Never used";
	#
	# check lockfile...
	#
	my $lockfile=$lockDir . "$file.lock";
	farkdir::touch($lockfile);
	if ( not open(MLOCKFILE, ">$lockfile") ) {
	    farkdir::term("$myname couldnt open Lockfile: $lockfile");
	} else {
	    chmod 0666, $lockfile;
	    farkdir::term("$myname is already running") unless (flock (MLOCKFILE,2+4));
	    #
	    # read plot config file and get file filter and index variable...
	    #
	    my $plot=$plotRef->[0];
	    my $xmlfile=$plotDir . $plot;
	    my $xmloldfile = $plotOldDir . $plot;
	    if (compare($xmlfile,$xmloldfile) != 0) { # file has changed
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
		    ($lastAuto,$lastAccess)=
			processPlot($xmlfile,
				    $plotnode,
				    $colocDir,
				    $modelDir,
				    $modelCacheDir,
				    $obsDir,
				    $obsCacheDir,
				    $usePlotDir,
				    $plot,
				    $lastAuto,
				    $lastAccess);
		} else {
		    eval {
		        my $log=capture {
			    ($lastAuto,$lastAccess)=
				processPlot($xmlfile,
					    $plotnode,
					    $colocDir,
					    $modelDir,
					    $modelCacheDir,
					    $obsDir,
					    $obsCacheDir,
					    $usePlotDir,
					    $plot,
					    $lastAuto,
					    $lastAccess);
			};
		    };
		    my $ret=$@;if ($ret) {farkdir::term("$myname plot file: $xmlfile: $ret");}
		}
	    } else {
		farkdir::term("$myname corrupted file: $xmlfile");
	    }
	    close(MLOCKFILE);
	    $plotRef->[1]->setAttribute("last",        $lastAuto);
	    $plotRef->[1]->setAttribute("info",        $lastAccess);
	}
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
    my $usePlotDir = shift;
    my $file = shift;
    my $lastAuto = shift;
    my $lastAccess = shift;
    #
    if($debug){print "processPlot Entering with '$xmlfile' '$colocDir' '$modelDir' '$modelCacheDir'" .
	" '$obsDir' '$obsCacheDir' '$usePlotDir' '$file' '$lastAuto' '$lastAccess'\n";}
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
    if($debug){print "Set coloc model obs\n";}
    $fark->clearPlotSetStack();
    my @sets=$node->findnodes("set");
    foreach my $trg (@sets) {
	# load XML-data into coloc-, model- and obs-modules
	my $colocfile=$colocDir.$trg->getAttribute("coloc");
	if (!-e $colocfile) {farkdir::term("Undefined coloc file '$colocfile'");};
	&setColocModelObs($fark,$colocfile,$modelDir,$modelCacheDir,$obsDir,$obsCacheDir);
	# push all the data onto the plot-stack...
	if($debug){print "Push data to plot-stack obs\n";}
	$fark->pushPlotSet($trg->getAttribute("name"),
			   $trg->getAttribute("x"),
			   $trg->getAttribute("y"),
			   $trg->getAttribute("legend"));
    }
    if($debug){print "****** Make table\n";}
    my ($tablefile, $plotfile) = $fark->makePlotTable($table,$graphics); # colocate and generate table file...
    if($debug){print "****** Make graphics '$tablefile' '$plotfile'\n";}
    $fark->makePlotGraphics($tablefile,$plotfile); # generate graphics file...
    $fark->close();
    my $now = time();
    $lastAuto=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($now));
    my $usePlotFile=$usePlotDir . $file; 
    if (-f $usePlotFile) {
	my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
	    $atime,$mtime,$ctime,$blksize,$blocks) 
	    = stat($usePlotFile);
	$lastAccess=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($atime));
    };
    if($debug){print "processPlot Exiting with '$lastAuto' '$lastAccess'\n";}
    return ($lastAuto,$lastAccess);
}

# set colocation parameters... &setColoc($fark,$colocfile,...);
sub setColocModelObs {
    my $fark= shift;
    my $coloc = shift;
    my $modelDir = shift;
    my $modelCacheDir = shift;
    my $obsDir = shift;
    my $obsCacheDir = shift;
    my $ret=0;
    my $parser = XML::LibXML->new();
    eval {
	my $doc = $parser->parse_file($coloc);
	if ( my ($node) = $doc->findnodes("coloc/coloc_config")) {
	    my $filter = $node->getAttribute("filter")//"";
	    $fark->setColocFilter($filter);
	    $fark->clearModelTargetStack();
	    my @mtrgs=$node->findnodes("modelTarget");
	    foreach my $trg (@mtrgs) {
		my $name = $trg->getAttribute("name");
		my $var = $trg->getAttribute("variable");
		my $min = $trg->getAttribute("min");
		my $max = $trg->getAttribute("max");	
		$fark->pushModelTarget($name,$var,$min,$max);
	    }
	    $fark->clearObservationTargetStack();
	    my @otrgs=$node->findnodes("obsTarget");
	    foreach my $trg (@otrgs) {
		my $name = $trg->getAttribute("name");
		my $pos = $trg->getAttribute("pos");
		my $descr = $trg->getAttribute("descr");
		my $info = $trg->getAttribute("info");
		my $min = $trg->getAttribute("min");
		my $max = $trg->getAttribute("max");
		$fark->pushObservationTarget($name,$pos,$descr,$info,$min,$max);
	    }
	    $fark->clearMatchRuleStack();
	    my @rls=$node->findnodes("matchRules");
	    foreach my $rl (@rls) {
		my $name = $rl->getAttribute("name");
		my $expr = $rl->getAttribute("expression");
		$fark->addMatchRule($name,$expr,"","");   # no minimum or maximum
	    }
	    my $modelFile = $modelDir . $node->getAttribute("modelFile");
	    eval {
		my $mdoc = $parser->parse_file($modelFile);
		if ( my ($mnode) = $mdoc->findnodes("model/model_config")) {
		    my $file=$mnode->getAttribute("path");
		    my $cache=$modelCacheDir . $file;
		    my $index=$mnode->getAttribute("index");
		    my $start=$mnode->getAttribute("start");
		    my $stop=$mnode->getAttribute("stop");
		    $fark->setModelCache($cache);
		    $fark->setModelIndex($index);
		}
	    };
	    my $mret=$@;if ($mret) {die("model:$modelFile $mret");};
	    # push model data...
	    my $obsFile =$obsDir .  $node->getAttribute("obsFile");
	    eval {
		my $odoc = $parser->parse_file($obsFile);
		if ( my ($onode) = $odoc->findnodes("obs/obs_config")) {
		    my $file=$onode->getAttribute("path");
		    my $cache=$obsCacheDir . $file;
		    my $tablepath = $onode->getAttribute("tablePath");
		    my $bufrType = $onode->getAttribute("bufrType");
		    my $subType = $onode->getAttribute("subType");
		    my $index = $onode->getAttribute("indexTarget");
		    my $exp = $onode->getAttribute("indexExp");
		    $fark->setObservationCache($cache);
		    $fark->setObservationTablePath($tablepath);
		    $fark->setObservationType($bufrType,$subType);
		    $fark->setObservationIndex($index,$exp);
		    $fark->clearObservationTargetStack();
		    my @trgs=$node->findnodes("target");
		    foreach my $trg (@trgs) {
			my $name = $trg->getAttribute("name");
			my $pos = $trg->getAttribute("pos");
			my $descr = $trg->getAttribute("descr");
			my $info = $trg->getAttribute("info");
			my $min = $trg->getAttribute("min");
			my $max = $trg->getAttribute("max");
			$fark->pushObservationTarget($name,$pos,$descr,$info,$min,$max);
		    }
		};
	    };
	    my $oret=$@;if ($oret) {die("obs:$obsFile $oret");};
	};
	$ret=1;
    };    
    my $cret=$@;if ($cret) {die("coloc:$coloc $cret");};
    return $ret;
}
