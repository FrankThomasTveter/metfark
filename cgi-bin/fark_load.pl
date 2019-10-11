#!/usr/bin/perl -w
#
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use File::Basename;
use Capture::Tiny 'capture';
use POSIX 'strftime';
use Data::Dumper;
use File::Path qw( make_path );
use Cwd; use Cwd 'abs_path';
use farkdir;
#
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
#
# make sure config directories exist...
#
my $modelDir= farkdir::getRootDir("model") || farkdir::term("invalid root directory (model)");
my $obsDir=   farkdir::getRootDir("obs")   || farkdir::term("Invalid root directory (obs)");
my $colocDir= farkdir::getRootDir("coloc") || farkdir::term("Invalid root directory (coloc)");
my $plotDir=  farkdir::getRootDir("plot")  || farkdir::term("Invalid root directory (plot)");
my $autoDir=  farkdir::getRootDir("auto")  || farkdir::term("Invalid root directory (auto)");
my $rerunDir=  farkdir::getRootDir("rerun")  || farkdir::term("Invalid root directory (rerun)");
#
# system directories...
#
my $colocUseDir= farkdir::getRootDir("coloc_use") || farkdir::term("Invalid root directory (coloc_use)");
my $plotUseDir=  farkdir::getRootDir("plot_use")  || farkdir::term("Invalid root directory (plot_use)");
my $lockDir=    farkdir::getRootDir("lock")       || farkdir::term("Invalid root directory (lock)");
my $scriptDir=   farkdir::getRootDir("script")    || farkdir::term("Invalid root directory (script)");
#
#
my $ref=CGI->new();
#
$XML::LibXML::skipXMLDeclaration = 1;
my $param= $ref->{param};
my $types = $param->{"type"};
if (! defined $types) { $types = $param->{"type[]"}||[]};
my $args = $param->{"arg[]"};
#
my $len=@{$types};
if ($len==0 || $types->[0] eq "") {
    farkdir::term("Undefined 'type'.")
};
#
my $doc = XML::LibXML::Document->new( '1.0', 'UTF-8' );
my $top = XML::LibXML::Element->new( 'load' );
$doc->addChild( $top );
foreach my $cls (@{$types}) {
#
    if ($cls eq "auto") {
	&loadAuto($doc,$top,$param);
    } elsif ($cls eq "cat") {
	&loadCat($doc,$top,$param);
    } else {
	if (! defined $args) {farkdir::term("Undefined $args.".Dumper($param))};
	my $arg = $args->[0];
	&loadCls($doc,$top,$param, $arg, $cls);
    }
}
print $doc->toString . "\n";

sub loadCls {
    my $doc=shift;
    my $top=shift;
    my $param =shift;
    my $arg = shift;
    my $cls = shift;
    my $name="";
    my ($root, $loc, $priv) = farkdir::splitDir( $arg, $cls );
    my $fpath=$root . $loc; # full absolute path
    if (! -d  $fpath) {
	($fpath, $name) = farkdir::splitName($arg);
	($root, $loc, $priv) = farkdir::splitDir( $fpath, $cls );
	$fpath=$root . $loc; # full absolute path
    }
    #print ("$arg:$cls**$root:$loc:$priv\n");
    if (-d  $fpath && $priv eq "rw") {
	my $path=$fpath . $name;
	#print "Org: '$cls' '$arg' => '$root' '$loc' '$priv' '$name'\n";
	# make a new document
	my $parent = XML::LibXML::Element->new( $cls );
	$top->addChild( $parent );
	# load model data
	my $parser = XML::LibXML->new();
	#$parser->expand_entities( 0 ); # leave entities alone
	if (-d $path) {                   # argument is directory
	    $fpath=$path;
	    my ($root,$loc,$priv) = farkdir::splitDir($fpath,$cls);
	    #print "New: '$cls' '$fpath' => '$root' '$loc' '$priv' '$name'\n";
	    my $node = $doc->createElement("Root");
	    $node->setAttribute("class",     $cls);
	    $node->setAttribute("root",      $root);
	    $node->setAttribute("location",  $loc);
	    $node->setAttribute("status",    $priv);
	    $node->setAttribute("arg",       $arg);
	    $node->setAttribute("type",      "dir");
	    $parent->addChild( $node );
	} elsif (-f $path) {               # argument is file
	    my $node = $doc->createElement("Root");
	    $node->setAttribute("class",     $cls);
	    $node->setAttribute("root",      $root);
	    $node->setAttribute("location",  $loc);
	    $node->setAttribute("file",      $name);
	    $node->setAttribute("status",    $priv);
	    $node->setAttribute("arg",       $arg);
	    $node->setAttribute("type",      "file");
	    $parent->addChild( $node );
	} else {                            # unknown argument
	    my $node = $doc->createElement("Root");
	    $node->setAttribute("class",     $cls);
	    $node->setAttribute("root",      $root);
	    $node->setAttribute("location",  $loc);
	    $node->setAttribute("child",     $name);
	    $node->setAttribute("status",    $priv);
	    $node->setAttribute("arg",       $arg);
	    $node->setAttribute("type",      "unknown");
	    $parent->addChild( $node );
	};
	#print "dir: $path $priv\n";
	if ($priv eq "rw" && opendir (DIR, $fpath)) {
	    while (my $child = readdir(DIR)) {
		next if (substr($child,0,1) eq ".");
		my $cpath=$fpath . $child;
		farkdir::sandbox {
		    #print "Child: $cpath\n";
		    if (-f $cpath) {
			my $cpriv=$priv;
			if (! -z $cpath) {# used for debugging
			    my $mod = $parser->parse_file($cpath);
			    my ($node)=$mod->findnodes($cls."/".$cls."_config");
			    # remove passwords...
			    if ( $node ) {$node->removeAttribute("password");
					  $node->setAttribute("root",      $root);
					  $node->setAttribute("location",  $loc);
					  if(defined $node->getAttribute("filterDir")){
					      if (-d $node->getAttribute("filterDir")) {
						  $node->setAttribute("filterDirStat","");
					      }  else {
						  $node->setAttribute("filterDirStat",$node->getAttribute("filterDir"));
					      }
					  }
					  #print "**********Ref: " . ref($node) . "\n";
					  #print "*******Dumper: " . Dumper($node);
					  $parent->addChild( $node );
			    };
			} else {
			    $cpriv="empty";
			};
			{
			    #print "Added file: $root $loc $child $cpriv\n";
			    my $node = $doc->createElement("File");
			    $node->setAttribute("class",     $cls);
			    $node->setAttribute("root",      $root);
			    $node->setAttribute("location",  $loc);
			    $node->setAttribute("file",      $child);
			    $node->setAttribute("status",    $cpriv);
			    $parent->addChild( $node );
			};
		    } elsif (-d $cpath) {
			#print "Added subdir: $root $loc $child $priv\n";
			# add directory to list
			my $node = $doc->createElement("Dir");
			$node->setAttribute("class",     $cls);
			$node->setAttribute("root",      $root);
			$node->setAttribute("location",  $loc);
			$node->setAttribute("dir",      $child);
			$node->setAttribute("status",    $priv);
			$parent->addChild( $node );
		    } else {
			# ignored
		    }
		}{message => "Unable to process: $cpath",
		  stdout=>"never"};
	    }
	    closedir(DIR);
	}
    } else {
	farkdir::term("Unable to open file :$fpath: type=:$cls: ($priv).");
    };
}

sub loadAuto {
    my $doc=shift;
    my $top=shift;
    my $param =shift;
    my $cls = "auto";
    my $file=($param->{root}->[0]||"auto.cfg");
    my ($dir, $name) = farkdir::splitName($file);
    my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
    my $fpath=$root . $loc; # full absolute path
    if (-d  $fpath && $priv eq "rw") {
	my $path=$fpath . $name;
	my $parent = XML::LibXML::Element->new( 'auto' );
	$top->addChild( $parent );
	# load obs data
	my $parser = XML::LibXML->new();
	#$parser->expand_entities( 0 ); # leave entities alone
	farkdir::sandbox {
	    if (-f $path) {
		my $auto = $parser->parse_file($path) || next;
		my ($node)=$auto->findnodes("auto/auto_config");
		if ( $node ) {
		    $node->removeAttribute("password"); # remove passwords
		    &updateTime($node,"model");
		    &updateTime($node,"obs");
		    &updateTime($node,"coloc");
		    &updateTime($node,"plot");
		}
		$parent->addChild( $node );
	    } else {
		farkdir::term("Invalid file: $path");
	    };
	}{message=> "Unable to process: $path",
	  stdout=>"never"};
    } else {
	farkdir::term("Invalid directory ($fpath,$priv)");
    };
};

sub loadRerun {
    my $doc=shift;
    my $top=shift;
    my $param =shift;
    my $cls = "rerun";
    #farkdir::term("Debug:".Dumper($param).":".$param->{"arg[]"}->[0]);
    my $file=($param->{"arg[]"}->[0]||"rerun.cfg");
    my ($dir, $name) = farkdir::splitName($file);
    my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
    my $fpath=$root . $loc; # full absolute path
    if (-d  $fpath && $priv eq "rw") {
	my $path=$fpath . $name;
	my $parent = XML::LibXML::Element->new( 'rerun' );
	$top->addChild( $parent );
	# load obs data
	my $parser = XML::LibXML->new();
	#$parser->expand_entities( 0 ); # leave entities alone
	farkdir::sandbox {
	    if (-f $path) {
		my $rerun = $parser->parse_file($path) || next;
		my ($node)=$rerun->findnodes("rerun/rerun_config");
		if ( $node ) {
		    $node->removeAttribute("password"); # remove passwords
		    &updateTime($node,"model");
		    &updateTime($node,"obs");
		    &updateTime($node,"coloc");
		    &updateTime($node,"plot");
		}
		$parent->addChild( $node );
	    } else {
		farkdir::term("Invalid file: $path");
	    };
	}{message=> "Unable to process: $path",
	  stdout=>"never"};
    } else {
	farkdir::term("Invalid directory ($fpath,$priv)");
    };
};

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
	#print "Usefile: '$clsUseFile'\n";
	if (-f $clsUseFile) {
	    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
		$mtime,$ctime,$blksize,$blocks) = stat($clsUseFile);
	    $lastStop=$atime;
	};
	my $lockfilename=$lockDir."$cls/$file.lock";
	#print "Lockfile: '$lockfilename'\n";
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
			    $mtime,$ctime,$blksize,$blocks) = stat($clsFillFile);
			$lastFill=$atime;
		    };
		    if ($lastFill >= $lastStart) {
			$lastAccess="> ok (".(farkdir::dtg($duration)) . ")";
		    } elsif ($cls eq "model" or $cls eq "obs") {
			$lastAccess="# no new data (".(farkdir::dtg($duration)) . ")";
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

sub loadCat {
    my $doc=shift;
    my $top=shift;
    my $param =shift;
    #
    my $cat;
    my $attr;
    my $line;
    my $clmn;
    #
    # open script directory
    #
    my ($root, $loc, $priv) = farkdir::splitDir( $scriptDir, "script" );
    my $fpath=$root . $loc;
    if ($priv eq "rw" && opendir (DIR, $fpath)) {
	#
	# loop through script files
	#
	my $parent = XML::LibXML::Element->new( 'cat' );
	$top->addChild( $parent );
	while (my $child = readdir(DIR)) {
	    next if (substr($child,0,1) eq ".");
	    my $cpath=$fpath . $child;
	    if (-f $cpath && open(my $fh, $cpath)) {
		$cat  = XML::LibXML::Element->new( 'cat_config' );
		$cat->setAttribute("name",$child);
		$parent->addChild( $cat );
		#
		# extract columns and attributes
		#
		while (my $row = <$fh>) {
		    chomp $row;
		    if (! $row =~ m/^#/) {last;};
		    if ($row =~ m/^#\s*Column\s*=\s*(\S+)\s*$/) {
			$clmn  = XML::LibXML::Element->new( 'column' );
			$clmn->setAttribute("name",$1);
			$cat->addChild( $clmn );
		    } elsif ($row =~ m/^#\s*Attribute\s*=\s*(.+)\s*$/) {
			my $line=$1;
			if ($line =~ m/^(.+)\s*\|\s*(.*)$/) {
			    $attr = XML::LibXML::Element->new( 'attr' );
			    $attr->setAttribute("name",$1);
			    $attr->setAttribute("value",$2);
			    $cat->addChild( $attr );
			} else {
			    $attr = XML::LibXML::Element->new( 'attr' );
			    $attr->setAttribute("name",$line);
			    $cat->addChild( $attr );
			}
		    } elsif ($row =~ m/^#\s*Id\s*=\s*(.+)\s*$/) {
			my $line=$1;
			if ($line =~ m/^(.+)\s*\|\s*(.*)$/) {
			    $line = XML::LibXML::Element->new( 'line' );
			    $line->setAttribute("id",$1);
			    $line->setAttribute("name",$2);
			    $cat->addChild( $line );
			}
		    }
		}
	    }
	}
    }
};

