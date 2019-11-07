#!/usr/bin/perl -w
#
use strict;
use lib "/home/ubuntu/perl5/lib/perl5/x86_64-linux-gnu-thread-multi";
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
use ExtUtils::testlib;
use fark;
use farkdata;
use farkdir;
#
my $ref=CGI->new();
my $param    = $ref->{param};
#
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
#
eval {
    #fark::debug(1);  # debug observations
    #fark::debug(2);  # debug models
    #fark::debug(4);  # debug table
    #fark::debug(3);  # debug colocation
    #fark::debug(5);  # debug parse
    my $debug=0;
    if (defined $param->{debug}[0]) {
	$debug=1;       # debug this script (0=omit output)
	#fark::debug(1);  # debug observations
	#fark::debug(2);  # debug models
	#fark::debug(3);  # debug colocation
	#fark::debug(4);  # debug table
	#fark::debug(5);  # debug parse
    }
    #
    my $shapeDir = farkdir::getRootDir("shape") || farkdir::term("Invalid root directory (shape)");
    my $execDir  = farkdir::getRootDir("exec") || farkdir::term("Invalid root directory (exec)");
    my $rerunDir  = farkdir::getRootDir("rerun") || farkdir::term("Invalid root directory (rerun)");
    my $scriptDir= farkdir::getRootDir("script") || farkdir::term("Invalid root directory (script)");
    my $lockDir  = farkdir::getRootDir("lock") || farkdir::term("Invalid root directory (lock)");
    #
    my $myname = basename($0);
    #
    $XML::LibXML::skipXMLDeclaration = 1;
    my $password = $param->{password}[0] // "";
    my $execFile = $param->{root}[0] // "";
    my $cron     = $param->{cron}[0]//"";
    my $ipath    = $param->{file}[0] // "";
    my $cls      = $param->{type}[0] // "";
    if (! defined $param->{root}) {farkdir::term("Undefined root file.".Dumper($param))};
    my $save=0;
    my $execpath = $execDir . $execFile;
    my ($dir,$name)=farkdir::splitName($ipath);
    my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
    my $file = $loc . $name;
    my $test = (defined $param->{test}[0]) ? 1 : 0;
    my $abort = (defined $param->{abort}[0]) ? 1 : 0;
    #
    #####if (defined $cron) {$debug=1;};
    ####if ($abort) {print "Aborting...\n"};
    #
    if ($debug) {print "Argument='" . shift . "'\n";}
    if ($debug) {print "Processing $file\n";}
    #
    # exec config file...
    my $parser = XML::LibXML->new();
    if (-f $execpath) { # we have a config file...
	# read config file into memory
	my $doc = $parser->parse_file($execpath);
	if ( my ($node)=$doc->findnodes("exec/exec_config")) {
	    my $pass=($node->getAttribute("password")//"");
	    $save=($pass eq $password);
	    if ($cls eq "model" or 
		$cls eq "obs" or 
		$cls eq "coloc" or 
		$cls eq "table" or 
		$cls eq "join" or 
		$cls eq "plot" or 
		$cls eq "rerun" or 
		$cls eq "clean" ) {
		if (!$save) {farkdir::term("Permission denied (invalid password).");};
		if($debug){print "Processing ${cls}...\n";}
		&loopCls($cls,$test, $abort, &getCls($cls,$node,$cron,$file), $cron) || farkdir::term("Error return from loopModel:$file");
		if($debug){print "Processing ${cls}... done\n";}
	    } elsif (! "$cls" eq "") {
		farkdir::term("Invalid class ($cls).");
	    } else {
		if($debug){print "Processing all... '$cron'\n";}
		&loopCls("model",$test, $abort, &getCls("model",$node,$cron,$file), $cron) || farkdir::term("Error return from loopModel:$file");
		&loopCls("obs",$test, $abort,   &getCls("obs",$node,$cron,$file), $cron) || farkdir::term("Error return from loopModel:$file");
		&loopCls("coloc",$test, $abort, &getCls("coloc",$node,$cron,$file), $cron) || farkdir::term("Error return from loopModel:$file");
		&loopCls("table",$test, $abort, &getCls("table",$node,$cron,$file), $cron) || farkdir::term("Error return from loopModel:$file");
		&loopCls("join",$test, $abort,  &getCls("join",$node,$cron,$file), $cron) || farkdir::term("Error return from loopModel:$file");
		&loopCls("plot",$test, $abort,  &getCls("plot",$node,$cron,$file), $cron) || farkdir::term("Error return from loopModel:$file");
		&loopCls("rerun",$test, $abort,  &getCls("rerun",$node,$cron,$file), $cron) || farkdir::term("Error return from loopModel:$file");
		&loopCls("clean",$test, $abort,  &getCls("clean",$node,$cron,$file), $cron) || farkdir::term("Error return from loopModel:$file");
		if($debug){print "Processing all... done\n";}
	    }
	}
	if($debug){print "Cleaning up...\n";}
	#$doc = $parser->parse_file($execpath);
	#print $doc->toString . "\n";
	if ( my ($node)=$doc->findnodes("exec/exec_config")) {
	    &updateTime($node,"model");
	    &updateTime($node,"obs");
	    &updateTime($node,"coloc");
	    &updateTime($node,"table");
	    &updateTime($node,"join");
	    &updateTime($node,"plot");
	    &updateTime($node,"rerun");
	    &updateTime($node,"clean");
	}
	if ($save) {
	    farkdir::docsave($execpath, $doc);
	};
	if ( my ($node)=$doc->findnodes("exec/exec_config")) {
	    if (defined $node->getAttribute("password")) {$node->removeAttribute("password");}
	}
	print $doc->toString . "\n";
    } else {  # config file does not exist, create temporary xml-structure
	if (defined $param->{type}) {
	    my $doc = $parser->parse_string("<exec><exec_config/></exec>");
	    my ($node) = $doc->findnodes("exec/exec_config");
	    if ($cls eq "model" or 
		$cls eq "obs" or 
		$cls eq "coloc" or 
		$cls eq "table" or 
		$cls eq "join" or 
		$cls eq "plot" or 
		$cls eq "rerun" or 
		$cls eq "clean" ) {
		my $parent = XML::LibXML::Element->new( $cls );
		$parent->setAttribute("file",($name//""));
		$node->addChild( $parent );
		my $refs=&getCls($cls,$node,$cron,$file);
		&loopCls($cls,$test, $abort, $refs,$cron) || farkdir::term("Missing file '$file'");
		&updateCls($cls,$node,$refs);
	    } else {
		farkdir::term("Unknown type:",$param->{type}->[0]);
	    }
	    print $doc->toString . "\n";
	} else {
	    farkdir::term("No type specified.");
	}
    };
    if($debug){print "Normal end of program...\n";}
    1;

    sub getCls {
	my $cls  = shift//"";
	my $node = shift;
	my $cron = shift//0;
	my $file = shift//"";
	my %clshash=();
	if ($node) {
	    my @clsnodes=$node->findnodes($cls);
	    my $found=0;
	    foreach my $clsref (@clsnodes) {
		my $exec=($clsref->getAttribute("exec") || "");
		my $clsfile=$clsref->getAttribute("file");
		if ($file) {
		    if ($clsfile eq $file) {
			$clshash{$clsfile}=[$clsref];
			$found++;
		    } 
		} elsif (! $cron || ($cron && $exec eq $cron)) {
		    if($debug){print "Should process '$clsfile'... '$exec'\n";}
		    $clshash{$clsfile}=[$clsref];
		} else {
		    if($debug){print "Ignoring '$clsfile'... '$exec'\n";}
		}
	    }
	    if ($file && ! $found) {
		my $clsref = XML::LibXML::Element->new( 'cls' );
		$clsref->setAttribute("file",($file//""));
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
	my $abort         = shift // 0;
	my $clsr          = shift;
	my $cron          = shift // 0;
	my $variable      = shift;
	my $value         = shift;
	my $offset        = shift;
	my $clsDir=     farkdir::getRootDir("$cls") || 
	    farkdir::term("Invalid root directory (".$cls.")");
	my $clsLogDir=  farkdir::getRootDir($cls."_log") || 
	    farkdir::term("Invalid root directory (".$cls."_log)");
	my $abortDir=farkdir::getRootDir("abort") || 
	    farkdir::term("Invalid root directory (abort)");
	#
	foreach my $clsfile (sort keys %{$clsr}) {
	    my $xmlfile=$clsDir . $clsfile;
	    my $abortfile = $abortDir ."$cls/$clsfile";
	    # check lockfile
	    my $lockfile = $lockDir ."$cls/$clsfile.lock";
	    my ($lockdir,$lockname)=farkdir::splitName($lockfile);
	    # make sure directory exists in case we create lockfile next
	    if (farkdir::makePath($lockdir)) {
	    } elsif ($cron) { 
		print "$myname unable to make: $lockdir";
		return;
	    } else {
		farkdir::term("$myname unable to make: $lockdir")
	    };
	    if($debug){print "Lockfile '$lockfile'\n";};
	    ###print ">>>> Lockfile '$lockfile'\n";
	    my $lock;
	    if ( $abort) {
		my $logfile="";
		farkdir::sandbox {
		    my $cmd=&execCls($cls,$test, $abort,$clsr,$cron,$clsfile,$xmlfile,$logfile,$variable,$value,$offset);
		    if ($cmd) {system($cmd);};
		}{abort     => $abort,
		  abortfile => $abortfile,
		  stdout    => "always"
		};
	    } elsif(open($lock, ">$lockfile")  && flock ($lock,2+4) ) {
		close($lock);
		if (unlink($lockfile)) {
		    if($debug){print "*** $myname Able to rm '$lockfile'\n";}
		} elsif ($cron) {
		    print "*** $myname unable to rm '$lockfile'\n";
		    return;
		} else {
		    farkdir::term("$myname unable to rm '$lockfile'");
		};
		open($lock, ">$lockfile")  && flock ($lock,2+4) 
		    || farkdir::term("$myname unable to lock '$lockfile'");
		# this defines processing start time
		if (farkdir::touchFile($lockfile)) {
		} elsif ($cron) {
		    print "$myname unable to touch '$lockfile'";
		    return;
		} else {
		    farkdir::term("$myname unable to touch '$lockfile'");
		};
		if($debug){print "*** $myname Lockfile is open: '$lockfile'\n";}
		chmod 0666, $lockfile;
		#system "ls -lu $lockfile";
		# check logfile
		my $logfile = $clsLogDir ."$clsfile.log";
		#print "Abort file: $abortfile\n";
		my ($logdir,$logname)=farkdir::splitName($logfile);
		farkdir::makePath($logdir) || farkdir::term("$myname unable to make: $logdir"); # make sure directory exists in case we create lockfile next
		if (-e $logfile) {
		    unlink($logfile) || farkdir::term("Unable to rm $logfile");
		};
		if ($cron)  {
		    if($debug){print "Calling farkdir::sandbox (cron=$cron) '$clsfile'\n";};
		    #if ($cls eq "model") { fark::debug(2);};  # debug models
		    farkdir::sandbox {
			my $cmd=&execCls($cls,$test, $abort,$clsr,$cron,$clsfile,$xmlfile,$logfile,$variable,$value,$offset);
			if ($cmd) {system($cmd);};
		    }{logfile   => $logfile,
		      fork      => 1,
		      debug     => 1,
		      abort     => $abort,
		      abortfile => $abortfile,
		      stdout    => "always"
		    };
		    if($debug){print "After A farkdir::sandbox\n";};
		} elsif ($debug) {
		    if($debug){print "Calling farkdir::sandbox (debug) '$clsfile'\n";};
		    farkdir::sandbox {
			my $cmd=&execCls($cls,$test, $abort,$clsr,$cron,$clsfile,$xmlfile,$logfile,$variable,$value,$offset);
			if ($cmd) {system($cmd);};
		    }{message=>"$myname Failed to process $xmlfile ($cls)\n",
		      debug     => 1,
		      stdout    => "always"
		    };
#
		    #message=>"$myname Failed to process $xmlfile ($cls)\nSee '$logfile.err' for more information.\n",
		    #	      logfile   => $logfile,
		    if($debug){print "After B farkdir::sandbox\n";};
		} else {
		    if($debug){print "Calling farkdir::sandbox '$clsfile'\n";};
		    farkdir::sandbox {
			my $cmd=&execCls($cls,$test, $abort,$clsr,$cron,$clsfile,$xmlfile,$logfile,$variable,$value,$offset);
			if ($cmd) {system($cmd);};
		    }{message   => "$myname Failed to process $xmlfile ($cls)\nSee '$logfile.err' for more information.\n",
#		      debug     => 1,
#		      stdout    => "always"
		      logfile   => $logfile,
		      fork      => 1,
		      abort     => $abort,
		      abortfile => $abortfile,
		      stdout    => "never"
		    };
		    if($debug){print "After C farkdir::sandbox\n";};
		};
		if($debug) {print "*** Closing lock file $lockfile\n";}
		close($lock);
		#farkdir::touchFile($lockfile) || farkdir::term("$myname unable to touch '$lockfile'");
		#system "ls -lu $lockfile";
	    } elsif ($cron) {
		print "$myname Unable to lock '$lockfile'";
		return;
	    } else {
		farkdir::term("Process is running ($clsfile).");
	    };
	};
	if($debug) {print "Ending loopCls... $cls\n";}
	return $clsr;
    }
    #
    sub execCls {
	my $cls           = shift // "";
	my $test          = shift // 0;
	my $abort         = shift // 0;
	my $clsr          = shift;
	my $cron          = shift // 0;
	my $clsfile       = shift;
	my $xmlfile       = shift;
	my $logfile       = shift;
	my $variable      = shift;
	my $value         = shift;
	my $offset        = shift;
	my %clshash=%{$clsr};
	#
	my $cmd="";
	if ($debug){print "Starting loopCls... ($cls, $cron)\n";};
	if($debug){print "Logfile '$logfile'\n";}
	print ">>>> $myname Starting sandbox...($debug,$cron,$test)\n";
	my $clsOldDir=  farkdir::getRootDir($cls."_old") || 
	    farkdir::term("Invalid root directory (".$cls."_old)");
	my $clsUseDir=  farkdir::getRootDir($cls."_use") || 
	    farkdir::term("Invalid root directory (".$cls."_use)");
	my $clsFillDir=  farkdir::getRootDir($cls."_fill") || 
	    farkdir::term("Invalid root directory (".$cls."_fill)");
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
	
	my $timeExec = "";
	my $infoExec = "";
	my $lastStart=time();
	my $clsFillFile=$clsFillDir . $clsfile; 
	my ($filldir,$fillname)=farkdir::splitName($clsFillFile);
	farkdir::makePath($filldir) || farkdir::term("$myname unable to make: $filldir");
	# check if setup-file has changed since last run (clean up cache files if it has)
	my $xmloldfile = $clsOldDir . $clsfile;
	if ($debug) {print "Re-run check against: '$xmloldfile'\n";};
	my $clean=compare($xmlfile,$xmloldfile);
	if ($clean) { # xml-file has changed
	    my ($dir,$name)=farkdir::splitName($xmloldfile);
	    farkdir::makePath($dir);
	    copy ($xmlfile,$xmloldfile);
	};
	# exec config file...
	if ( ! -e $xmlfile) { farkdir::term("$myname unable to find: $xmlfile ($cls)");}
	# read config file into memory
	my $parser = XML::LibXML->new();
	my $clsdoc = $parser->parse_file($xmlfile);
	if($debug){print "Processing '$xmlfile'\n";}
	if ( my ($clsnode)=$clsdoc->findnodes($cls."/".$cls."_config")) {
	    if($debug){print "Processing XML $xmlfile ($debug,$cron)\n";}
	    $cmd=&processCls($xmlfile,
			     $clsnode,
			     $colocDir,
			     $modelDir,
			     $modelCacheDir,
			     $modelRegDir,
			     $obsDir,
			     $obsCacheDir,
			     $obsRegDir,
			     $cls,$clsfile,$clean,
			     $test, $abort,$clsFillFile,$logfile,
			     $variable,$value,$offset
		);
	} else {
	    farkdir::term("$myname corrupted file: '$xmlfile' ::$cls");
	}
	my $clsUseFile=$clsUseDir . $clsfile; 
	# this defines processing end time
	if($debug){
	    print "Usefile '$clsUseFile'\n";
	}
	unlink($clsUseFile);
	farkdir::touchFile($clsUseFile) || farkdir::term("$myname unable to touch '$clsUseFile'");
	#system "ls -l $clsUseFile; date";
	my $lastStop=time();
	$timeExec=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($lastStart));
	my $duration = $lastStop-$lastStart;
	# last "fill" time, i.e. last time we had any data...
	my $lastFill=0;
	if($debug){print "Fillfile '$clsFillFile'\n";}
	if (-f $clsFillFile) {
	    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
		$mtime,$ctime,$blksize,$blocks) = stat($clsFillFile);
	    $lastFill=$atime;
	};
	my $timeFill=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($lastFill));
	if ($lastFill >= $lastStart) {
	    $infoExec="> ok (".(farkdir::dtg($duration)) . ")";
	} elsif ($cls eq "model" or $cls eq "obs") {
	    $infoExec="# no new data (".(farkdir::dtg($duration)) . ")";
	} else {
	    $infoExec="# no data (".(farkdir::dtg($duration)) . ")";
	}
	if($debug){print "Last fill: '$timeFill'\n";}
	if($debug){print "Last time: '$timeExec'\n";}
	if($debug){print "Last info: '$infoExec'\n";}
	if (defined $clsr->{$clsfile}->[0]) {
	    $clsr->{$clsfile}->[1]=$timeExec;
	    $clsr->{$clsfile}->[2]=$infoExec;
	    $clsr->{$clsfile}->[3]=$logfile;
	}
	if($debug) {print "Ending loopCls... $cls\n";}
	print ">>>> $myname Sandbox done...($debug,$cron,$test)\n";
	return $cmd;
    };

    sub processCls{
	my ($xmlfile,
	    $clsnode,
	    $colocDir,
	    $modelDir,
	    $modelCacheDir,
	    $modelRegDir,
	    $obsDir,
	    $obsCacheDir,
	    $obsRegDir,
	    $cls,$clsfile,$clean,
	    $test, $abort,$clsFillFile,$logfile,
	    $variable,$value,$offset
	    ) =@_;
	my $cmd="";
	if($debug){print "Processing $cls $xmlfile\n";}
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
			  $test, $abort,$clsFillFile,
			  $variable,$value,$offset
		);
	} elsif ($cls eq "obs") {
	    my $cachefile=$obsCacheDir . $clsfile;
	    my $registerfile=$obsRegDir . $clsfile;
	    if ($clean) {
		if($debug){print "Unlinking '$cachefile' '$registerfile'\n";}
		unlink $cachefile;
		unlink $clsFillFile;
		unlink $registerfile;
	    }
	    &processObs($xmlfile,
			$clsnode,
			$cachefile,
			$registerfile,
			$cls,
			$test, $abort,$clsFillFile,
			$variable,$value,$offset
		);
	} elsif ($cls eq "coloc") {
	    &processColoc($xmlfile,
			  $clsnode,
			  $colocDir,
			  $modelDir,
			  $modelCacheDir,
			  $obsDir,
			  $obsCacheDir,
			  $cls,
			  $test, $abort,$clsFillFile,
			  $variable,$value,$offset
		);
	} elsif ($cls eq "table") {
	    &processTable($xmlfile,
			  $clsnode,
			  $colocDir,
			  $modelDir,
			  $modelCacheDir,
			  $obsDir,
			  $obsCacheDir,
			  $cls,
			  $test, $abort,$clsFillFile,$logfile,
			  $variable,$value,$offset
		);
	} elsif ($cls eq "join") {
	    &processJoin($xmlfile,
			 $clsnode,
			 $colocDir,
			 $modelDir,
			 $modelCacheDir,
			 $obsDir,
			 $obsCacheDir,
			 $cls,
			 $test, $abort,$clsFillFile,$logfile,
			 $variable,$value,$offset
		);
	} elsif ($cls eq "plot") {
	    $cmd=&processPlot($xmlfile,
			      $clsnode,
			      $colocDir,
			      $modelDir,
			      $modelCacheDir,
			      $obsDir,
			      $obsCacheDir,
			      $cls,
			      $test, $abort,$clsFillFile,$logfile,
			      $variable,$value,$offset
		);
	} elsif ($cls eq "rerun") {
	    &processRerun($xmlfile,
			  $clsnode,
			  $colocDir,
			  $modelDir,
			  $modelCacheDir,
			  $obsDir,
			  $obsCacheDir,
			  $cls,
			  $test, $abort,$clsFillFile,$logfile,
			  $variable,$value,$offset
		);
	} elsif ($cls eq "clean") {
	    &processClean($xmlfile,
			  $clsnode,
			  $colocDir,
			  $modelDir,
			  $modelCacheDir,
			  $obsDir,
			  $obsCacheDir,
			  $cls,
			  $test, $abort,$clsFillFile,$logfile,
			  $variable,$value,$offset
		);
	};
	#### if (-e $clsFillFile) {chmod 0777, $clsFillFile;}
	return $cmd;
    };

    sub updateCls { 
	my $cls    = shift // "";
	my $node   = shift;
	my $clsr   = shift;
	my %clshash=%{$clsr};
	my @clsnodes=$node->findnodes($cls);
	my $found=0;
	foreach my $clsref (@clsnodes) {
	    my $clsfile=$clsref->getAttribute("file");
	    if (defined $clshash{$clsfile}->[0]) {
		$clsref->setAttribute("last", ($clshash{$clsfile}->[1]//""));
		$clsref->setAttribute("info", ($clshash{$clsfile}->[2]//""));
		$clsref->setAttribute("log", ($clshash{$clsfile}->[3]//""));
	    }
	}
    }

    sub updateTime {
	my $node= shift;
	my $cls = shift;
	my $clsUseDir=    farkdir::getRootDir($cls."_use") || farkdir::term("Invalid root directory (".$cls."_use)");
	my $clsFillDir=   farkdir::getRootDir($cls."_fill") || farkdir::term("Invalid root directory (".$cls."_fill)");
	my @clss=$node->findnodes($cls);
	foreach my $clsr (@clss) {
	    my $file=$clsr->getAttribute("file");
	    my $timeExec="";
	    my $infoExec="";
	    my $lastStart=0;
	    my $lastStop=0;
	    my $clsUseFile=$clsUseDir . $file; 
	    if (-f $clsUseFile) {
		my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
		    $mtime,$ctime,$blksize,$blocks) = stat($clsUseFile);
		$lastStop=$atime;
	    };
	    my $lockfilename=$lockDir."$cls/$file.lock";
	    #if ($debug) {print "### Usefile: $clsUseFile Lockfile: $lockfilename\n"};
	    if (-f $lockfilename) {
		my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
		    $mtime,$ctime,$blksize,$blocks) = stat($lockfilename);
		$timeExec=strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($atime));
		$lastStart=$atime;
		my $lock;
		if ( not open($lock, ">$lockfilename") ) {
		} elsif (flock ($lock,2+4)) {
		    my $duration = $lastStop-$lastStart;
		    if ($duration < 0) {
			$infoExec="# abort";
		    } else {
			my $clsFillFile=$clsFillDir . $file; 
			my $lastFill=0;
			if (-f $clsFillFile) {
			    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
				$mtime,$ctime,$blksize,$blocks) = stat($clsFillFile);
			    $lastFill=$atime;
			};
			if ($lastFill >= $lastStart) {
			    $infoExec="> ok (".(farkdir::dtg($duration)) . ")";
			} elsif ($cls eq "model" or $cls eq "obs") {
			    $infoExec="# no new data (".(farkdir::dtg($duration)) . ")";
			} else {
			    $infoExec="# no data (".(farkdir::dtg($duration)) . ")";
			}
		    }
		} else {
		    my $duration = time()-$lastStart;
		    $infoExec="# running (".farkdir::dtg($duration) . ")";
		};	
		close($lock);
	    };
	    $clsr->setAttribute("last",        ($timeExec//""));
	    $clsr->setAttribute("info",        ($infoExec//""));
	}
    };

    sub processModel {
	my $xmlfile = shift;
	my $node = shift;
	my $cachefile = shift;
	my $registerfile = shift;
	my $file = shift;
	my $test = shift;
	my $abort         = shift // 0;
	my $clsFillFile = shift;
	my $variable      = shift;
	my $value         = shift;
	my $offset        = shift;
	#
	print ">>>> Loading $cls file: $xmlfile\n";
	my $tolerance = "10.0";            # shape tolerance in km
	fark->simplifyShapes($tolerance);  # simplify shapes
	my $shapeFile=$shapeDir . "ne_50m_admin_0_countries";
	fark->setShapeFile($shapeFile);    # read shape file
	my $fark=fark->open();
	&setRerunConfig($fark,$variable,$value,$offset);
	&setModelConfig($fark,$node,$cachefile);
	#
	my $filterDir=$node->getAttribute("filterDir");
	my $filterDirMin=$node->getAttribute("filterDirMin")//"";
	my $filterDirMax=$node->getAttribute("filterDirMax")//"";
	my $filterFile=$node->getAttribute("filterFile");
	#
	my $timeoffset=$fark->getRerunOffset();
	# min/max may be expressions depending on the rerun variable
	my $filterNumMin=("$filterDirMin" ne ""?$fark->expression($filterDirMin) - $timeoffset:$filterDirMin);
	my $filterNumMax=("$filterDirMax" ne ""?$fark->expression($filterDirMax) - $timeoffset:$filterDirMax);
	#
	print ">>>> Looking for files in '$filterDir' matching '$filterFile' ($filterNumMin,$filterNumMax) Offset=$timeoffset\n";
	# registerfile is updated if (! $test)
	$fark->updateModelRegister($registerfile,$filterDir,$filterFile,$filterNumMin,$filterNumMax,$test,$clsFillFile);
	$fark->makeModelCache($cachefile);
	#
	$fark->close();
	#
	if (! -e $cachefile) {
	    unlink $clsFillFile;
	    unlink $registerfile;
	    farkdir::term(">>>> Unable to make model index '$cachefile'");
	} else {
	    print ">>>> Normal end of process...\n";
	};
	return; # no system command will be run afterwards
    }

    sub processObs {
	my $xmlfile = shift;
	my $node = shift;
	my $cachefile = shift;
	my $registerfile = shift;
	my $file = shift;
	my $test = shift;
	my $abort         = shift // 0;
	my $clsFillFile = shift;
	my $variable      = shift;
	my $value         = shift;
	my $offset        = shift;
	#
	print ">>>> Loading $cls file: $xmlfile\n";
	my $tolerance = "10.0";            # shape tolerance in km
	fark->simplifyShapes($tolerance);  # simplify shapes
	my $shapeFile=$shapeDir . "ne_50m_admin_0_countries";
	fark->setShapeFile($shapeFile);
	my $fark=fark->open();
	&setRerunConfig($fark,$variable,$value,$offset);
	&setObsConfig($fark,$node,$cachefile);
	#
	my $filterDir=$node->getAttribute("filterDir");
	my $filterDirMin=$node->getAttribute("filterDirMin")//"";
	my $filterDirMax=$node->getAttribute("filterDirMax")//"";
	my $filterFile=$node->getAttribute("filterFile");
	#
	my $timeoffset=$fark->getRerunOffset();
	# min/max may be expressions depending on the rerun variable
	my $filterNumMin=("$filterDirMin" ne ""?$fark->expression($filterDirMin) - $timeoffset:$filterDirMin);
	my $filterNumMax=("$filterDirMax" ne ""?$fark->expression($filterDirMax) - $timeoffset:$filterDirMax);
	#
	print ">>>> Looking for files in '$filterDir' matching '$filterFile' ($filterNumMin,$filterNumMax) Offset=$timeoffset\n";
	# registerfile is updated if (! $test)
	$fark->updateObservationRegister($registerfile,$filterDir,$filterFile,$filterNumMin,$filterNumMax,$test,$clsFillFile);
	$fark->makeObservationCache($cachefile);
	#
	$fark->close();
	#
	if (! -e $cachefile) {
	    unlink $clsFillFile;
	    unlink $registerfile;
	    farkdir::term(">>>> Unable to make obs index '$cachefile'");
	} else {
	    print ">>>> Normal end of process...\n";
	};
	return; # no system command will be run afterwards
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
	my $abort         = shift // 0;
	my $clsFillFile = shift;
	my $variable      = shift;
	my $value         = shift;
	my $offset        = shift;
	#
	print ">>>> Loading $cls file: $xmlfile\n";
	if($debug){print "processColoc Entering with '$xmlfile' '$file'\n";}
	my $tolerance = "10.0";            # shape tolerance in km
	fark->simplifyShapes($tolerance);  # simplify shapes
	my $shapeFile=$shapeDir . "ne_50m_admin_0_countries";
	fark->setShapeFile($shapeFile);
	my $fark=fark->open();
	&setRerunConfig($fark,$variable,$value,$offset);
	&setColocConfig($fark,$node,$modelDir,$modelCacheDir,$obsDir,$obsCacheDir);
	#
	# make the resulting XML
	#print "Calling colocXML\n";
	my $xml       = $node->getAttribute("xml");
	my ($irc, $msg) = $fark->makeColocXML($xml,$test,$clsFillFile);
	#
	# close session
	$fark->close();
	print ">>>> Normal end of process...\n";
	if($debug){print "processColoc Exiting.\n";}
	return; # no system command will be run afterwards
    };
    sub processTable {
	my $xmlfile = shift;
	my $node = shift;
	my $colocDir = shift;
	my $modelDir = shift;
	my $modelCacheDir = shift;
	my $obsDir = shift;
	my $obsCacheDir = shift;
	my $cls = shift;
	my $test = shift;
	my $abort         = shift // 0;
	my $clsFillFile = shift;
	my $logfile = shift;
	my $variable      = shift;
	my $value         = shift;
	my $offset        = shift;
	#
	print ">>>> Loading $cls file: $xmlfile\n";
	if($debug){print "processTable Entering with '$xmlfile' '$cls'\n";}
	my $table=$node->getAttribute("table");
	my $graphics=$node->getAttribute("graphics");
	my $overwrite=$node->getAttribute("overwrite")//"true";
	my $cat=$node->getAttribute("cat");
	#print "Processing table: $xmlfile\n";
	my $tolerance = "10.0";            # shape tolerance in km
	fark->simplifyShapes($tolerance);  # simplify shapes
	my $shapeFile=$shapeDir . "ne_50m_admin_0_countries";
	fark->setShapeFile($shapeFile);
	my $fark=fark->open();
	&setRerunConfig($fark,$variable,$value,$offset);
	my $timeoffset=$fark->getRerunOffset();
	my $tablefile=$fark->strepTableFile($table);
	if($debug){print "****** Make table $tablefile ($timeoffset,$value)\n";}
	if ( ! -f $tablefile || "$overwrite" eq "true") {
	    $fark->setTableType($cat);
	    $fark->clearTableAttributeStack();
	    my @attrs=$node->findnodes("attribute");
	    foreach my $attr (@attrs) {
		$fark->pushTableAttribute($attr->getAttribute("name"),
					  $attr->getAttribute("value"));
	    }
	    my @cols=$node->findnodes("column");
	    if($debug){print "Set coloc model obs\n";}
	    $fark->clearTableSetStack();
	    my @sets=$node->findnodes("set");
	    foreach my $trg (@sets) {
		# load XML-data into coloc-, model- and obs-modules
		my $colocfile=$colocDir.$trg->getAttribute("coloc");
		if($debug){print "Coloc file '$colocfile'\n";}
		if (!-e $colocfile) {farkdir::term("Undefined coloc file '$colocfile'");};
		print ">>>> Loading coloc file: $colocfile (set:".
		    $trg->getAttribute("name").",". 
		    $trg->getAttribute("legend").")\n";
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
			if($debug){print "Failed setColocConfig '$mret'.\n";}
			die("model:$modelFile obs:$obsFile $mret");
		    };
		    # push columns
		    if($debug){print "Setting columns.\n";};
		    $fark->clearTableColumn();
		    my @colv=$trg->findnodes("col");
		    for ( my $i = 0; $i < @cols; $i++) {
			my $nam=$cols[$i]->getAttribute("name");
			my $val=$colv[$i]->getAttribute("value");
			if($debug){print "Setting column: $nam -> $val\n";};
			$fark->pushTableColumn($nam,$val);
		    }
		    # push all the data onto the table-stack...
		    if($debug){print "Push data to table-stack obs\n";}
		    $fark->pushTableSet($trg->getAttribute("name"),
					$trg->getAttribute("legend"));
		}
	    }
	    #
	    # colocate and generate table file...
	    my ($root, $loc, $priv) = farkdir::splitDir( $scriptDir, "script" );
	    my $fpath=$root . $loc . $cat;
	    my ($troot, $tloc, $tpriv) = farkdir::splitPattern( $tablefile, "output" );
	    if ($tpriv ne "rw") {
		farkdir::term("$myname Permission denied for output: $table $tpriv");
	    }
	    my ($dir,$name)=farkdir::splitName($tablefile);
	    if (farkdir::makePath($dir) && $debug){
		print "****** Made directory '$dir'\n";
	    } else {
		print "****** Directory exists '$dir'\n";
	    };
	    my ($tablefile,$graphicsdir) = $fark->makeTableFile($table,$graphics,$fpath,$test,$clsFillFile); 
	    #
	    if (-f $tablefile) {
		if ($debug){print("****** Touching:$tablefile ($timeoffset)\n");}
		farkdir::touchFile($tablefile,-($timeoffset));
		if ($debug){system("ls -l $tablefile");};
	    };
	    #
	    #   my $cmd="Rscript --vanilla $fpath $tablefile $graphicsdir $test";
	    #
	} else {
	    if($debug){print "****** Table file exists: '$tablefile'\n";}
	    farkdir::touchFile($clsFillFile) || farkdir::term("$myname unable to touch '$clsFillFile'");
	};
	#
	$fark->close();
	print ">>>> Normal end of process...\n";
	if($debug){print "processTable Exiting.\n";}
	return; # no system command will be run afterwards
    }
    sub processJoin {
	my $xmlfile = shift;
	my $node = shift;
	my $colocDir = shift;
	my $modelDir = shift;
	my $modelCacheDir = shift;
	my $obsDir = shift;
	my $obsCacheDir = shift;
	my $cls = shift;
	my $test = shift;
	my $abort         = shift // 0;
	my $clsFillFile = shift;
	my $logfile = shift;
	my $variable      = shift;
	my $value         = shift;
	my $offset        = shift;
	#
	print ">>>> Loading $cls file: $xmlfile\n";
	if($debug){print "processJoin Entering with '$xmlfile' '$cls'\n";}
	my $table=$node->getAttribute("table");
	my $graphics=$node->getAttribute("graphics");
	my $filter=$node->getAttribute("filter")||"";
	my $overwrite=$node->getAttribute("overwrite")//"true";
	my $cat=$node->getAttribute("cat");
	#print "Processing join: $xmlfile\n";
	#
	# get table files
	my $filterDir=$node->getAttribute("filterDir");
	my $filterDirMin=$node->getAttribute("filterDirMin")//"";
	my $filterDirMax=$node->getAttribute("filterDirMax")//"";
	my $filterFile=$node->getAttribute("filterFile");
	#
	# write attributes to output file
	my $fark=fark->open();
	&setRerunConfig($fark,$variable,$value,$offset);
	my $tablefile=$fark->strepTableFile($table);
	if ( ! -f $tablefile || "$overwrite" eq "true") {
	    $fark->setTableType($cat);
	    $fark->setTableFilter($filter);
	    if($debug){print "****** Make table C\n";}
	    my ($troot, $tloc, $tpriv) = farkdir::splitPattern( $table, "output" );
	    if ($tpriv ne "rw") {
		farkdir::term("$myname Permission denied for output: $table $tpriv");
	    }
	    $fark->clearTableAttributeStack();
	    my @attrs=$node->findnodes("attribute");
	    foreach my $attr (@attrs) {
		$fark->pushTableAttribute($attr->getAttribute("name"),
					  $attr->getAttribute("value"));
	    }
	    my @cols=$node->findnodes("column");
	    if($debug){print "Set coloc model obs\n";}
	    for ( my $i = 0; $i < @cols; $i++) {
		my $nam=$cols[$i]->getAttribute("name")//"";
		my $val=$cols[$i]->getAttribute("value")//"";
		my $min=$cols[$i]->getAttribute("min")//"";
		my $max=$cols[$i]->getAttribute("max")//"";
		$fark->pushTableColumn($nam,$val);
		if ("$min" ne "" && "$max" ne "") {
		    if($debug){print "Setting column limits for '$nam' -> '$min' '$max'\n";}
		    $fark->limitTableColumn($nam,$min,$max);
		} else {
		    if($debug){print "No column limits for '$nam'\n";}
		}
	    }
	    $fark->clearTableFileStack();
	    #
	    my $timeoffset=$fark->getRerunOffset();
	    # min/max may be expressions depending on the rerun variable
	    my $filterNumMin=("$filterDirMin" ne ""?$fark->expression($filterDirMin) - $timeoffset:$filterDirMin);
	    my $filterNumMax=("$filterDirMax" ne ""?$fark->expression($filterDirMax) - $timeoffset:$filterDirMax);
	    #
	    print ">>>> Looking for files in '$filterDir' matching '$filterFile' ($filterNumMin,$filterNumMax) Offset=$timeoffset\n";
	    $fark->addTableFiles($filterDir,$filterFile,$filterNumMin,$filterNumMax,$test,$clsFillFile);
	    #
	    my ($root, $loc, $priv) = farkdir::splitDir( $scriptDir, "script" );
	    my $fpath=$root . $loc . $cat;
	    my ($tablefile,$graphicsdir) = $fark->joinTableFile($table,$graphics,$fpath,$test,$clsFillFile); 
	    #
	    if (-f $tablefile) {
		if ($debug){print("****** Touching:$tablefile ($timeoffset)\n");}
		farkdir::touchFile($tablefile,-($timeoffset));
		if ($debug){system("ls -l $tablefile");};
	    };
	    #
	    # my $cmd="Rscript --vanilla $fpath $tablefile $test";
	    if($debug){print "****** Make table D file: '$tablefile'\n";}
	    #
	    farkdir::touchFile($clsFillFile) || farkdir::term("$myname unable to touch '$clsFillFile'");
	} else {
	    if($debug){print "****** Table file exists: '$tablefile'\n";}
	    farkdir::touchFile($clsFillFile) || farkdir::term("$myname unable to touch '$clsFillFile'");
	};
	$fark->close();
	print ">>>> Normal end of process...\n";
	if($debug){print "processJoin Exiting.\n";}
	return; # no system command will be run afterwards
    }
    sub processPlot {
	my $xmlfile = shift;
	my $node = shift;
	my $colocDir = shift;
	my $modelDir = shift;
	my $modelCacheDir = shift;
	my $obsDir = shift;
	my $obsCacheDir = shift;
	my $cls = shift;
	my $test = shift;
	my $abort         = shift // 0;
	my $clsFillFile = shift;
	my $logfile = shift;
	my $variable      = shift;
	my $value         = shift;
	my $offset        = shift;
	#
	print ">>>> Loading $cls file: $xmlfile\n";
	if($debug){print "processPlot Entering with '$xmlfile' '$cls'\n";}
	my $table=$node->getAttribute("table");
	my $graphics=$node->getAttribute("graphics");
	# get table and graphics files...
	my $fark=fark->open();
	my $tablefile=$fark->strepTableFile($table);
	my $graphicsfile=$fark->strepTableFile($graphics);
	$fark->close();
	# get name of script/command (first line)
	my $fpath = "/metfark/config/splus/synop"; # should be full path...
	# make command
	my $cmd="Rscript --vanilla $fpath $tablefile $graphicsfile $test";
	# run command
	if($debug){print "Executing '$cmd'\n";}
	print ">>>> Running script...\n   $cmd\n";
	my $ret=0;
	eval{
	    system $cmd; # The system command may exit, in which case it is caught by the higher level sandbox...
	    $ret=$?;
	};
	print "Return code:  $ret\n";
	if ($ret != 0) {
	    farkdir::term("$myname failed command '$cmd'");
	}
	print ">>>> Normal end of process...\n";
	if($debug){print "processPlot Exiting.\n";}
	return $cmd; # run system command afterwards
    };
    sub processRerun {
	my $xmlfile = shift;
	my $node = shift;
	my $colocDir = shift;
	my $modelDir = shift;
	my $modelCacheDir = shift;
	my $obsDir = shift;
	my $obsCacheDir = shift;
	my $cls = shift;
	my $test = shift;
	my $abort         = shift // 0;
	my $clsFillFile = shift;
	my $logfile = shift;
	my $variable      = shift;
	my $value         = shift;
	my $offset        = shift;
	my $file="";
	#
	print ">>>> Loading $cls file: $xmlfile\n";
	my $doc = $parser->parse_file($xmlfile);
	if ( my ($rerun)=$doc->findnodes("rerun/rerun_config")) {
	    my $variable=$rerun->getAttribute("variable")||"rid";
	    my $start=$rerun->getAttribute("start")//"0";
	    my $stop=$rerun->getAttribute("stop")//"0";
	    my $offset=$rerun->getAttribute("offset")//"0";
	    for (my $value=$start;$value<=$stop;$value++) {
		# loop over files
		print ">>> Rerun: $variable='$value'\n";
		&loopCls("model",$test, $abort, &getCls("model",$rerun,$cron,$file), 
			 $cron,$variable,$value,$offset) || farkdir::term("Error return from loopModel:$file");
		&loopCls("obs",$test, $abort,   &getCls("obs",$rerun,$cron,$file), 
			 $cron,$variable,$value,$offset) || farkdir::term("Error return from loopModel:$file");
		&loopCls("coloc",$test, $abort, &getCls("coloc",$rerun,$cron,$file), 
			 $cron,$variable,$value,$offset) || farkdir::term("Error return from loopModel:$file");
		&loopCls("table",$test, $abort, &getCls("table",$rerun,$cron,$file), 
			 $cron,$variable,$value,$offset) || farkdir::term("Error return from loopModel:$file");
		&loopCls("join",$test, $abort,  &getCls("join",$rerun,$cron,$file), 
			 $cron,$variable,$value,$offset) || farkdir::term("Error return from loopModel:$file");
		&loopCls("plot",$test, $abort,  &getCls("plot",$rerun,$cron,$file), 
			 $cron,$variable,$value,$offset) || farkdir::term("Error return from loopModel:$file");
	    }
	}
	#$doc = $parser->parse_file($execpath);
	#print $doc->toString . "\n";
	if ( my ($rerun)=$doc->findnodes("rerun/rerun_config")) {
	    &updateTime($rerun,"model");
	    &updateTime($rerun,"obs");
	    &updateTime($rerun,"coloc");
	    &updateTime($rerun,"table");
	    &updateTime($rerun,"join");
	    &updateTime($rerun,"plot");
	}
	farkdir::docsave($xmlfile, $doc);
	farkdir::touchFile($clsFillFile) || farkdir::term("$myname unable to touch '$clsFillFile'");
	return; # no system command will be run afterwards
    };
    sub processClean {
	use URI::Encode qw(uri_encode uri_decode);
	my $xmlfile = shift;
	my $node = shift;
	my $colocDir = shift;
	my $modelDir = shift;
	my $modelCacheDir = shift;
	my $obsDir = shift;
	my $obsCacheDir = shift;
	my $cls = shift;
	my $test = shift;
	my $abort         = shift // 0;
	my $clsFillFile = shift;
	my $logfile = shift;
	my $variable      = shift;
	my $value         = shift;
	my $offset        = shift;
	#
	print ">>>> Loading $cls file: $xmlfile\n";
	my $doc = $parser->parse_file($xmlfile);
	if ( my ($clean)=$doc->findnodes("clean/clean_config")) {
	    my @filters=$clean->findnodes("filter");
	    foreach my $filter (@filters) {
		my $filterDir=uri_decode($filter->getAttribute("dir")//"");
		my $filterFile=uri_decode($filter->getAttribute("file")//".*");
		my $filterDirMin=uri_decode($filter->getAttribute("age")//"");
		my $filterDirMax="0";
		my ($root, $loc, $priv) = farkdir::splitDir( $dir, "output" );
		my @files=farkdir::GetFiles($filterDir,$filterFile,$filterDirMin,$filterDirMax);
		if ($debug) {print "Filtering: '$filterDir' '$filterFile' '$filterDirMin' '$filterDirMax'\n";}
		print ">>>> Files found= ".($#files+1)."\n";
		foreach my $file (@files) {
		    print "Deleting file: '$file'\n";
		}
	    }
	    farkdir::touchFile($clsFillFile) || farkdir::term("$myname unable to touch '$clsFillFile'");
	}
	return; # no system command will be run afterwards
    };
    sub setRerunConfig { # setRerunConfig($fark,$variable,$value,$offset);
	my $fark= shift;
	my $variable=shift;
	my $value=shift;
	my $offset=shift;
	if (defined $value  && ! "$value" eq "") {
	    $fark->setRerunVariable($variable);
	    $fark->setRerunValue($value);
	    $fark->setRerunOffset($offset);
	} else {
	    if  ($debug) {print "No RERUN variable (rid) is defined...\n";};
	    $fark->setRerunVariable("rid");
	    $fark->setRerunValue("0");
	    $fark->setRerunOffset("0");
	}
    };	
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
	if($debug){print "Model config complete\n";}
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
	    $fark->parseMd5();
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
			farkdir::term("Missing model cache:'".$modelCache."'")
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
		    my $cnt=0;
		    foreach my $def (@oldDefs) {
			#print "Setting model default ".$def->getAttribute("name")."\n";
			my $nam=$def->getAttribute("name")//"";
			my $val=$def->getAttribute("value")//"";
			if ($val) {
			    $fark->addDefault($nam,$val);
			    $cnt++;
			}
		    }
		    if ($cnt) {
			#print "Pushing model default.\n";
			$fark->pushDefault(); # "target:value"
		    };
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
	if (length($obsFile//"") &&length($modelFile//"")) {
	    my @oldNodes=$node->findnodes("matchRules");
	    foreach my $oldnode (@oldNodes) {
		#print "Setting match rule '".
		#$oldnode->getAttribute("name")."' -> '".
		#$oldnode->getAttribute("expression")."'\n";
		if (length($oldnode->getAttribute("expression")//"")) {
		    $fark->pushMatchRule(($oldnode->getAttribute("name")//""),
					 ($oldnode->getAttribute("expression")//""),
					 ($oldnode->getAttribute("min")//""),
					 ($oldnode->getAttribute("max")//""));
		};
	    };
	}
	my $modelStart= ($node->getAttribute("modelStart")//"");
	my $modelStop = ($node->getAttribute("modelStop")//"");
	my $obsStart  = ($node->getAttribute("obsStart")//"");
	my $obsStop   = ($node->getAttribute("obsStop")//"");
	my $obsFilter = ($node->getAttribute("obsFilter")//"");
	my $modelFilter = ($node->getAttribute("modelFilter")//"");
	if (length($modelFile//"") && (length($modelStart//"") ||  length($modelStop//""))) {
	    if($debug){print "Setting model index limits '$modelStart' '$modelStop'\n";}
	    $fark->setModelIndexLimits($modelStart, $modelStop); # "target:value"
	};
	if (length($obsFile//"") && (length($obsStart//"") || length($obsStop//""))) {
	    if($debug){print "Setting obs index limits '$obsStart' '$obsStop'\n";}
	    $fark->setObservationIndexLimits($obsStart, $obsStop); # "target:value"
	};
	if (length($obsFile//"") && length($obsFilter//"")) {
	    if($debug){print "Setting obs filter '$obsFilter'\n";}
	    $fark->setObservationFilter($obsFilter); # "target:value"
	};
	if (length($modelFile//"") && length($modelFilter//"")) {
	    if($debug){print "Setting colocation filter '$modelFilter'\n";}
	    $fark->setModelFilter($modelFilter); # "target:value"
	};
	if($debug){print "setColocConfig done.\n";}
    };
} or do {
    my $msg = $@;
    farkdir::term($msg);
};
