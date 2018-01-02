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
use Capture::Tiny 'capture_merged';
use File::Basename;
use File::Compare;
use File::Copy;
#
my $user=$ENV{USERNAME} // "www";
my $pub="/metfark/pub";
#
my $debug=0;       # debug this script (0=omit output)
#fark::debug(1);  # debug observations
#fark::debug(2);  # debug models
#fark::debug(3);  # debug colocation
#fark::debug(4);  # debug plot
#fark::debug(5);  # debug parse
#
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
#
my $autoDir=      farkdir::getRootDir("auto") || farkdir::term("Invalid root directory (auto)");
my $scriptDir=    farkdir::getRootDir("script") || farkdir::term("Invalid root directory (script)");
my $lockDir=     farkdir::getRootDir("lock") || farkdir::term("Invalid root directory (lock)");
#
#
my $myname = basename($0);
#
my $ref=CGI->new();

if($debug){print "Argument='" . shift . "'\n";}
#
$XML::LibXML::skipXMLDeclaration = 1;
my $param    = $ref->{param};
my $password = $param->{password}[0] // "";
my $autoFile = $param->{root}[0] // "";
my $cron     = $param->{cron}[0] // "";
my $ipath    = $param->{file}[0] // "";
my $cls      = $param->{type}[0] // "undef";
if (! defined $param->{root}) {farkdir::term("Undefined root file.".Dumper($param))};
my $save=0;
my $autopath = $autoDir . $autoFile;
my ($dir,$name)=farkdir::splitName($ipath);
my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
my $file = $loc . $name;
my $test = (defined $param->{test}[0]) ? 1 : 0;

# auto config file...
my $parser = XML::LibXML->new();
if (-f $autopath) { # we have a config file...
    my $modelr;
    my $obsr  ;
    my $colocr;
    my $plotr ;
    # read config file into memory
    my $doc = $parser->parse_file($autopath);
    if ( my ($node)=$doc->findnodes("auto/auto_config")) {
	my $pass=($node->getAttribute("password")//"");
	$save=($pass eq $password);
	if (($param->{type}->[0]//"") eq "model") {
	    $modelr=&getCls("model",$node,$cron,$file);
	    &loopCls("model",$test, $modelr) || farkdir::term("Error return from loopModel:$file");
        } elsif (($param->{type}->[0]//"") eq "obs") {
	    $obsr=&getCls("obs",$node,$cron,$file);
	    &loopCls("obs",$test, $obsr) || farkdir::term("Error return from loopObs:$file");
        } elsif (($param->{type}->[0]//"") eq "coloc") {
	    $colocr=&getCls("coloc",$node,$cron,$file);
	    &loopCls("coloc",$test, $colocr) || farkdir::term("Error return from loopColoc:$file");
        } elsif (($param->{type}->[0]//"") eq "plot") {
	    $plotr=&getCls("plot",$node,$cron,$file);
	    &loopCls("plot",$test, $plotr) || farkdir::term("Error return from loopPlot:$file");
        } else {
	    if($debug){print "Processing all... '$cron'\n";}
	    $modelr=&getCls("model",$node,$cron,$file);
	    &loopCls("model",$test, $modelr) || farkdir::term("Error return from loopModel:$file");
	    $obsr=&getCls("obs",$node,$cron,$file);
	    &loopCls("obs",$test, $obsr) || farkdir::term("Error return from loopObs:$file");
	    $colocr=&getCls("coloc",$node,$cron,$file);
	    &loopCls("coloc",$test, $colocr) || farkdir::term("Error return from loopColoc:$file");
	    $plotr=&getCls("plot",$node,$cron,$file);
	    &loopCls("plot",$test, $plotr) || farkdir::term("Error return from loopPlot:$file");
	}
    }
    if ($save) {
	my $doc = $parser->parse_file($autopath);
	#print $doc->toString . "\n";
	if ( my ($node)=$doc->findnodes("auto/auto_config")) {
	    &updateTime($node,"model");
	    &updateTime($node,"obs");
	    &updateTime($node,"coloc");
	    &updateTime($node,"plot");
	}
	if (open(my $fh, '>', $autopath)) {
	    print $fh $doc->toString;
	    close $fh;
	    chmod 0666, $autopath;
	} else {
	    farkdir::term("Unable to open:".$autopath);
	}
	print $doc->toString . "\n";
    } else {
	farkdir::info("Processing complete.");
    };
} else {  # config file does not exist, create temporary xml-structure
    if (defined $param->{type}) {
	my $doc = $parser->parse_string("<auto><auto_config/></auto>");
	my ($node) = $doc->findnodes("auto/auto_config");
	if (($param->{type}->[0]//"") eq "model") {
	    my $parent = XML::LibXML::Element->new( 'model' );
	    $parent->setAttribute("file",$name);
	    $node->addChild( $parent );
	    my $modelr=&getCls("model",$node,$cron,$file);
	    &loopCls("model",$test, $modelr) || farkdir::term("Missing file '$file'");
	    &updateCls("model",$node,$modelr);
	} elsif (($param->{type}->[0]//"") eq "obs") {
	    my $parent = XML::LibXML::Element->new( 'obs' );
	    $parent->setAttribute("file",$name);
	    $node->addChild( $parent );
	    my $obsr=&getCls("obs",$node,$cron,$file);
	    &loopCls("obs",$test, $obsr) || farkdir::term("Missing file '$file'");
	    &updateCls("obs",$node,$obsr);
	} elsif (($param->{type}->[0]//"") eq "coloc") {
	    my $parent = XML::LibXML::Element->new( 'coloc' );
	    $parent->setAttribute("file",$name);
	    $node->addChild( $parent );
	    my $colocr=&getCls("coloc",$node,$cron,$file);
	    &loopCls("coloc",$test, $colocr) || farkdir::term("Missing file '$file'");
	    &updateCls("coloc",$node,$colocr);
	} elsif (($param->{type}->[0]//"") eq "plot") {
	    my $parent = XML::LibXML::Element->new( 'plot' );
	    $parent->setAttribute("file",$name);
	    $node->addChild( $parent );
	    my $plotr=&getCls("plot",$node,$cron,$file);
	    &loopCls("plot",$test, $plotr) || farkdir::term("Missing file '$file'");
	    &updateCls("plot",$node,$plotr);
	} else {
	    farkdir::term("Unknown type:",$param->{type}->[0]);
	}
	print $doc->toString . "\n";
    } else {
	farkdir::term("No type specified.");
    }
}

sub getCls {
    my $cls  = shift//"";
    my $node = shift;
    my $cron = shift//"";
    my $file = shift//"";
    my %clshash=();
    if ($node) {
	my @clsnodes=$node->findnodes($cls);
	my $found=0;
	foreach my $clsref (@clsnodes) {
	    my $auto=($clsref->getAttribute("auto") || "");
	    my $clsfile=$clsref->getAttribute("file");
	    if ($file) {
		if ($clsfile eq $file) {
		    $clshash{$clsfile}=[$clsref];
		    $found++;
		} 
	    } elsif (! $cron || ($cron && $auto eq $cron)) {
		if($debug){print "Should process '$clsfile'... '$auto'\n";}
		$clshash{$clsfile}=[$clsref];
	    } else {
		if($debug){print "Ignoring '$clsfile'... '$auto'\n";}
	    }
	}
	if ($file && ! $found) {
	    my $clsref = XML::LibXML::Element->new( 'cls' );
	    $clsref->setAttribute("file",$file);
	    $node->addChild($clsref);
	    $clshash{$file}=[$clsref];
	}
    } elsif ($file) {
	$clshash{$file}=[];
    };
    if ($debug){print "Found ".(keys %clshash) ." file(s).\n";};
    return \%clshash;
}

sub loopCls {
    my $cls           = shift // "";
    my $test          = shift // 0;
    my $clsr          = shift;
    my %clshash=%{$clsr};
    #
    my $clsDir=     farkdir::getRootDir("$cls") || 
	farkdir::term("Invalid root directory (".$cls.")");
    my $clsOldDir=  farkdir::getRootDir($cls."_old") || 
	farkdir::term("Invalid root directory (".$cls."_old)");
    my $clsUseDir=  farkdir::getRootDir($cls."_use") || 
	farkdir::term("Invalid root directory (".$cls."_use)");
    my $clsFillDir=  farkdir::getRootDir($cls."_fill") || 
	farkdir::term("Invalid root directory (".$cls."_fill)");
    my $clsLogDir=  farkdir::getRootDir($cls."_log") || 
	farkdir::term("Invalid root directory (".$cls."_log)");
    # type-specific directories
    my $modelDir=     farkdir::getRootDir("model") || 
	farkdir::term("Invalid root directory (model)");
    my $modelCacheDir=farkdir::getRootDir("model_cache") || 
	farkdir::term("Invalid root directory (model_cache)");
    my $modelRegDir=  farkdir::getRootDir("model_reg") || 
	farkdir::term("Invalid root directory (model_reg)");
    my $obsDir=       farkdir::getRootDir("obs") || 
	farkdir::term("Invalid root directory (obs)");
    my $obsCacheDir=farkdir::getRootDir("obs_cache") || 
	farkdir::term("Invalid root directory (obs_cache)");
    my $obsRegDir=  farkdir::getRootDir("obs_reg") || 
	farkdir::term("Invalid root directory (obs_reg)");
    my $colocDir=     farkdir::getRootDir("coloc") || 
	farkdir::term("Invalid root directory (coloc)");
    #
    foreach my $clsfile (keys %clshash) {
	my $lastAuto = "";
	my $lastAccess = "";
	#
	# check logfile
	my $logfile = $clsLogDir ."$clsfile.log";
	my ($logdir,$logname)=farkdir::splitName($logfile);
	farkdir::makePath($logdir) || farkdir::term("$myname unable to make: $logdir"); # make sure directory exists in case we create lockfile next
	if($debug){print "Logfile '$logfile'\n";}
	#
	# check lockfile
	my $lockfile = $lockDir ."$cls/$clsfile.lock";
	my ($lockdir,$lockname)=farkdir::splitName($lockfile);
	farkdir::makePath($lockdir) || farkdir::term("$myname unable to make: $lockdir"); # make sure directory exists in case we create lockfile next
	#if($debug){print "Lockfile '$lockfile'\n";}
	if ( open(MLOCKFILE, ">$lockfile")  && flock (MLOCKFILE,2+4) ) {
	    # this defines processing start time
	    farkdir::touchFile($lockfile) || farkdir::term("$myname unable to touch '$lockfile'");
	    #system "ls -lu $lockfile";
	    chmod 0666, $lockfile;
	    my $lastStart=time();
	    my $clsFillFile=$clsFillDir . $clsfile; 
	    my ($filldir,$fillname)=farkdir::splitName($clsFillFile);
	    farkdir::makePath($filldir) || farkdir::term("$myname unable to make: $filldir");
	    my $xmlfile=$clsDir . $clsfile;
	    my $xmloldfile = $clsOldDir . $clsfile;
	    my $clean=compare($xmlfile,$xmloldfile);
	    if ($clean) { # xml-file has changed
		my ($dir,$name)=farkdir::splitName($xmloldfile);
		farkdir::makePath($dir);
		copy ($xmlfile,$xmloldfile);
	    };
	    # auto config file...
	    if ( ! -e $xmlfile) { farkdir::term("$myname unable to find: $xmlfile");}
	    # read config file into memory
	    my $parser = XML::LibXML->new();
	    my $clsdoc = $parser->parse_file($xmlfile);
	    if ( my ($clsnode)=$clsdoc->findnodes($cls."/".$cls."_config")) {
		farkdir::termval {
		    print "Processing $cls $xmlfile\n";
		    if ($cls eq "model") {
			my $cachefile=$modelCacheDir . $clsfile;
			my $registerfile=$modelRegDir . $clsfile;
			if ($clean) {
			    if($debug){print "Unlinking '$cachefile' '$registerfile'\n";}
			    unlink $cachefile;
			    unlink $clsFillFile;
			    unlink $registerfile;
			}
			&processModel($xmlfile,
				      $clsnode,
				      $cachefile,
				      $registerfile,
				      $cls,
				      $test,$clsFillFile);
		    } elsif ($cls eq "obs") {
			my $cachefile=$obsCacheDir . $clsfile;
			my $registerfile=$obsRegDir . $clsfile;
			if ($clean) {
			    if($debug){print "Unlinking '$cachefile' '$registerfile'\n";}
			    unlink $cachefile;
			    unlink $registerfile;
			}
			&processObs($xmlfile,
				    $clsnode,
				    $cachefile,
				    $registerfile,
				    $cls,
				    $test,$clsFillFile);
		    } elsif ($cls eq "coloc") {
			&processColoc($xmlfile,
				      $clsnode,
				      $colocDir,
				      $modelDir,
				      $modelCacheDir,
				      $obsDir,
				      $obsCacheDir,
				      $cls,
				      $test,$clsFillFile);
		    } elsif ($cls eq "plot") {
			&processPlot($xmlfile,
				     $clsnode,
				     $colocDir,
				     $modelDir,
				     $modelCacheDir,
				     $obsDir,
				     $obsCacheDir,
				     $cls,
				     $test,$clsFillFile);
		    };
		    #### if (-e $clsFillFile) {chmod 0777, $clsFillFile;}
		} "$myname $cls file: $xmlfile, see $logfile",$logfile;
	    } else {
		farkdir::term("$myname corrupted file: '$xmlfile' ::$cls");
	    }
	    my $clsUseFile=$clsUseDir . $clsfile; 
	    # this defines processing end time
	    if($debug){print "Usefile '$clsUseFile'\n";}
	    farkdir::touchFile($clsUseFile) || farkdir::term("$myname unable to touch '$clsUseFile'");
	    #system "ls -l $clsUseFile; date";
	    my $lastStop=time();
	    $lastAuto=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($lastStart));
	    my $duration = $lastStop-$lastStart;
	    # last "fill" time, i.e. last time we had any data...
	    my $lastFill=0;
	    if (-f $clsFillFile) {
		my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
		    $mtime,$ctime,$blksize,$blocks) = stat($clsUseFile);
		$lastFill=$atime;
	    };
	    if ($lastFill >= $lastStart) {
		$lastAccess="> ok (".(farkdir::dtg($duration)) . ")";
	    } else {
		$lastAccess="# no data (".(farkdir::dtg($duration)) . ")";
	    }
	    if($debug){print "Time: '$lastAuto'\n";}
	    if($debug){print "Info: '$lastAccess'\n";}
	    #if ($debug){print "Done '$lastStop' '$lastStart' $lastAccess\n";};
	    if (defined $clshash{$clsfile}->[0]) {
		$clshash{$clsfile}->[1]=$lastAuto;
		$clshash{$clsfile}->[2]=$lastAccess;
		$clshash{$clsfile}->[3]=$logfile;
	    }
	}
	#print "Closing lock file $lockfile\n";
	close(MLOCKFILE);
	#farkdir::touchFile($lockfile) || farkdir::term("$myname unable to touch '$lockfile'");
	#system "ls -lu $lockfile";
    }
    return \%clshash;
};

sub updateCls { 
    my $cls    = shift // "";
    my $node   = shift;
    my $clsr = shift;
    my %clshash=%{$clsr};
    my @clsnodes=$node->findnodes($cls);
    my $found=0;
    foreach my $clsref (@clsnodes) {
	my $clsfile=$clsref->getAttribute("file");
	if (defined $clshash{$clsfile}->[0]) {
	    $clsref->setAttribute("last", $clshash{$clsfile}->[1]);
	    $clsref->setAttribute("info", $clshash{$clsfile}->[2]);
	    $clsref->setAttribute("log", $clshash{$clsfile}->[3]);
	}
    }
}

sub updateTime {
    my $node= shift;
    my $cls = shift;
    my $clsUseDir=    farkdir::getRootDir($cls."_use") || farkdir::term("Invalid root directory (use)");
    my $clsFillDir=   farkdir::getRootDir($cls."_fill") || farkdir::term("Invalid root directory (fill)");
    my @clss=$node->findnodes($cls);
    foreach my $clsr (@clss) {
	my $file=$clsr->getAttribute("file");
	my $lastAuto="";
	my $lastAccess="";
	my $lastStart=0;
	my $lastStop=0;
	my $clsUseFile=$clsUseDir . $file; 
	if (-f $clsUseFile) {
	    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
		$mtime,$ctime,$blksize,$blocks) = stat($clsUseFile);
	    $lastStop=$atime;
	};
	my $lockfilename=$lockDir."$cls/$file.lock";
	if (-f $lockfilename) {
	    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
		$mtime,$ctime,$blksize,$blocks) = stat($lockfilename);
	    $lastAuto=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($atime));
	    $lastStart=$atime;
	    if ( not open(MLOCKFILE, ">$lockfilename") ) {
	    } elsif (flock (MLOCKFILE,2+4)) {
		my $duration = $lastStop-$lastStart;
		if ($duration < 0) {
		    $lastAccess="# abort";
		} else {
		    my $clsFillFile=$clsFillDir . $file; 
		    my $lastFill=0;
		    if (-f $clsFillFile) {
			my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
			    $mtime,$ctime,$blksize,$blocks) = stat($clsUseFile);
			$lastFill=$atime;
		    };
		    if ($lastFill >= $lastStart) {
			$lastAccess="> ok (".(farkdir::dtg($duration)) . ")";
		    } else {
			$lastAccess="# no data (".(farkdir::dtg($duration)) . ")";
		    }
		}
	    } else {
		my $duration = time()-$lastStart;
		$lastAccess="# running (".farkdir::dtg($duration) . ")";
	    };	
	    close(MLOCKFILE);
	};
	$clsr->setAttribute("last",        $lastAuto);
	$clsr->setAttribute("info",        $lastAccess);
    }
};

sub processModel {
    my $xmlfile = shift;
    my $node = shift;
    my $cachefile = shift;
    my $registerfile = shift;
    my $file = shift;
    my $test = shift;
    my $fillfile = shift;
    #
    my $fark=fark->open();
    &setModelConfig($fark,$node,$cachefile);
    #
    my $filterDir=$node->getAttribute("filterDir");
    my $filterDirMin=$node->getAttribute("filterDirMin")||"";
    my $filterDirMax=$node->getAttribute("filterDirMax")||"";
    my $filterFile=$node->getAttribute("filterFile");
    # registerfile is updated if (! $test)
    $fark->updateModelRegister($registerfile,$filterDir,$filterFile,$filterDirMin,$filterDirMax,$test,$fillfile);
    if (! $test) {$fark->makeModelCache($cachefile);}
    #
    $fark->close();
    #
    if (! -e $cachefile) {
	farkdir::term("Unable to make cache file:'".$cachefile."'")
    };
    return;
}

sub processObs {
    my $xmlfile = shift;
    my $node = shift;
    my $cachefile = shift;
    my $registerfile = shift;
    my $file = shift;
    my $test = shift;
    my $fillfile = shift;
    #
    my $fark=fark->open();
    &setObsConfig($fark,$node,$cachefile);
    #
    my $filterDir=$node->getAttribute("filterDir");
    my $filterDirMin=$node->getAttribute("filterDirMin")||"";
    my $filterDirMax=$node->getAttribute("filterDirMax")||"";
    my $filterFile=$node->getAttribute("filterFile");
    # registerfile is updated if (! $test)
    $fark->updateObservationRegister($registerfile,$filterDir,$filterFile,$filterDirMin,$filterDirMax,$test,$fillfile);
    if (! $test) {$fark->makeObservationCache($cachefile);}
    #
    $fark->close();
    #
    if (! -e $cachefile) {
	farkdir::term("Unable to make cache file:'".$cachefile."'")
    };
    return;
}

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
    my $fillfile = shift;
    #
    if($debug){print "processColoc Entering with '$xmlfile' '$file'\n";}
    my $fark=fark->open();
    &setColocConfig($fark,$node,$modelDir,$modelCacheDir,$obsDir,$obsCacheDir);
    #
    # make the resulting XML
    #print "Calling colocXML\n";
    my $xml       = $node->getAttribute("xml");
    my ($irc, $msg) = $fark->makeColocXML($xml,$test,$fillfile);
    #
    # close session
    $fark->close();
    if($debug){print "processColoc Exiting.\n";}
    return;
}

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
    my $fillfile = shift;
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
	    # set colocation config parameters
	    eval {
		&setColocConfig($fark,$node,$modelDir,$modelCacheDir,
				$obsDir,$obsCacheDir);
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
    my ($root, $loc, $priv) = farkdir::splitDir( $scriptDir, "script" );
    my $fpath=$root . $loc . $cat;
    my ($tablefile, $plotfile) = $fark->makePlotTable($table,$graphics,$fpath,$test,$fillfile); 
    #
    if($debug){print "****** Make graphics '$tablefile' '$plotfile'\n";}
    my $cmd="Rscript --vanilla $fpath $tablefile $plotfile $test";
    print "Executing '$cmd'\n";
    system $cmd;
    #
    $fark->close();
    if($debug){print "processPlot Exiting.\n";}
    return;
}
sub setModelConfig {
    my $fark = shift;
    my $node = shift;
    my $cachefile = shift;
    my $indexTarget=$node->getAttribute("indexTarget");
    my $indexVariable=$node->getAttribute("indexVariable");
    #print "Clear model file stack, '$indexVariable'\n";
    $fark->clearModelFileStack($indexVariable); # clear model file stack
    #print "Setting model cache '$cachefile'\n";
    $fark->setModelCache($cachefile);
    #print "Setting model index '$indexTarget' '$indexVariable'\n";
    $fark->setModelIndex($indexTarget,$indexVariable);
    if (-e $cachefile) {
	$fark->loadModelCache($cachefile);# load cached model file stack
    };
}

sub setObsConfig {
    my $fark = shift;
    my $node = shift;
    my $cachefile = shift;
    #
    $fark->clearObservationFileStack(); # clear observation file stack
    my $tablepath=  ($node->getAttribute("tablePath")//"");
    my $indexTarget=($node->getAttribute("indexTarget")//"");
    my $indexExp=   ($node->getAttribute("indexExp")//"");
    my $bufrType=   ($node->getAttribute("bufrType")//"999");
    my $subType=    ($node->getAttribute("subType")//"999");
    #print "Setting obs\n";
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
	$fark->pushObservationTarget($trg->getAttribute("name")//"",
				     $trg->getAttribute("pos")//"",
				     $trg->getAttribute("descr")//"",
				     $trg->getAttribute("info")//"",
				     $trg->getAttribute("min")//"",
				     $trg->getAttribute("max")//"");
    }
    $fark->clearObservationFileStack();
    $fark->setObservationCache($cachefile);
    if (-e $cachefile) {
	$fark->loadObservationCache($cachefile);
    };
}

sub setColocConfig {
    my $fark = shift;
    my $node = shift;
    my $modelDir = shift // "";
    my $modelCacheDir = shift //"";
    my $obsDir = shift // "";
    my $obsCacheDir = shift // "";
    #
    #print "Setting coloc\n";
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
		&setModelConfig($fark,$node,$modelCache);
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
		&setObsConfig($fark,$node,$obsCache);
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
	    #print "Setting model target ".$oldnode->getAttribute("name")."\n";
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
		    #print "Setting model default ".$def->getAttribute("name")."\n";
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
	    #print "Setting obs target ".$oldnode->getAttribute("name")."\n";
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
	    #print "Setting match rule '".
		#$oldnode->getAttribute("name")."' -> '".
		#$oldnode->getAttribute("expression")."'\n";
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



