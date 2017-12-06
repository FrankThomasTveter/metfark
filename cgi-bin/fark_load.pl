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
my $modelUseDir= farkdir::getRootDir("model_use");
my $obsUseDir=   farkdir::getRootDir("obs_use");
my $colocUseDir= farkdir::getRootDir("coloc_use");
my $plotUseDir=  farkdir::getRootDir("plot_use");
my $lockRoot=    farkdir::getRootDir("lock");
#
#
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
#
my $ref=CGI->new();
#
$XML::LibXML::skipXMLDeclaration = 1;
my $param= $ref->{param};
my $types = $param->{"type"};
if (! defined $types) { $types = $param->{"type[]"}};
my $args = $param->{"arg[]"};
#
my $len=@{$types};
if ($len==0 || $types->[0] eq "") {
    farkdir::term("Undefined type.".Dumper($param))
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
	if (! defined $args) {farkdir::term("Undefined arg.".Dumper($param))};
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
					  if (! -d $node->getAttribute("filterDir")) {
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
	if (-f $path) {
	    my $auto = $parser->parse_file($path);
	    my ($node)=$auto->findnodes("auto/auto_config");
	    if ( $node ) {
		$node->removeAttribute("password"); # remove passwords
		my @models=$node->findnodes("model");
		foreach my $model (@models) {
		    my $file=$model->getAttribute("file");
		    my $lastAuto="";
		    my $lastAccess="";
		    my $lastStart=0;
		    my $lastStop=0;
		    my $modelUseFile=$modelUseDir . $file; 
		    #print "Checking $modelUseFile\n";
		    if (-f $modelUseFile) {
			my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
			    $mtime,$ctime,$blksize,$blocks) = stat($modelUseFile);
			$lastStop=$atime;
		    };
		    my $lockfilename=$lockRoot."model/$file.lock";
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
		    $model->setAttribute("last",        $lastAuto);
		    $model->setAttribute("info",        $lastAccess);
		}
		my @obss=$node->findnodes("obs");
		foreach my $obs (@obss) {
		    my $file=$obs->getAttribute("file");
		    my $lastAuto="";
		    my $lastAccess="";
		    my $lastStart=0;
		    my $lastStop=0;
		    my $obsUseFile=$obsUseDir . $file; 
		    if (-f $obsUseFile) {
			my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
			    $mtime,$ctime,$blksize,$blocks) = stat($obsUseFile);
			$lastStop=$atime;
		    };
		    my $lockfilename=$lockRoot."obs/$file.lock";
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
			    };
			} else {
			    my $duration = time()-$lastStart;
			    $lastAccess="**running** (".$duration . "s)";
			};
			close(MLOCKFILE);
		    };	
		    $obs->setAttribute("last",        $lastAuto);
		    $obs->setAttribute("info",        $lastAccess);
		}
		my @colocs=$node->findnodes("coloc");
		foreach my $coloc (@colocs) {
		    my $file=$coloc->getAttribute("file");
		    my $lastAuto="never";
		    my $lastAccess="";
		    my $lastStart=0;
		    my $lastStop=0;
		    my $colocUseFile=$colocUseDir . $file; 
		    if (-f $colocUseFile) {
			my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
			    $mtime,$ctime,$blksize,$blocks) = stat($colocUseFile);
			$lastStop=$atime;
		    };
		    my $lockfilename=$lockRoot."coloc/$file.lock";
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
			    };
			} else {
			    my $duration = time()-$lastStart;
			    $lastAccess="**running** (".$duration . "s)";
			};	
			close(MLOCKFILE);
		    };
		    $coloc->setAttribute("last",        $lastAuto);
		    $coloc->setAttribute("info",        $lastAccess);
		}
		my @plots=$node->findnodes("plot");
		foreach my $plot (@plots) {
		    my $file=$plot->getAttribute("file");
		    my $lastAuto="never";
		    my $lastAccess="";
		    my $lastStart=0;
		    my $lastStop=0;
		    my $plotUseFile=$plotUseDir . $file; 
		    if (-f $plotUseFile) {
			my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
			    $mtime,$ctime,$blksize,$blocks) = stat($plotUseFile);
			$lastStop=$atime;
		    };
		    my $lockfilename=$lockRoot."plot/$file.lock";
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
			    };
			} else {
			    my $duration = time()-$lastStart;
			    $lastAccess="**running** (".$duration . "s)";
			};	
			close(MLOCKFILE);
		    };
		    $plot->setAttribute("last",        $lastAuto);
		    $plot->setAttribute("info",        $lastAccess);
		}
	    }
	    $parent->addChild( $node );
	} else {
	    farkdir::term("Invalid file: $path");
	};
    } else {
	farkdir::term("Invalid directory ($fpath,$priv)");
    };
};


sub loadCat {
    my $doc=shift;
    my $top=shift;
    my $param =shift;
    my $parent = XML::LibXML::Element->new( 'cat' );
    $top->addChild( $parent );
    my $cat;
    my $attr;
    my $line;
    #
    $cat  = XML::LibXML::Element->new( 'cat_config' );
    $cat->setAttribute("name","Text");
    $parent->addChild( $cat );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","xlabel");
    $attr->setAttribute("value","X");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","ylabel");
    $attr->setAttribute("value","Y");
    $cat->addChild( $attr );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","1");
    $line->setAttribute("name","solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","2");
    $line->setAttribute("name","dotted");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","3");
    $line->setAttribute("name","dashed");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","4");
    $line->setAttribute("name","blue-solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","5");
    $line->setAttribute("name","red-solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","6");
    $line->setAttribute("name","gold-solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","7");
    $line->setAttribute("name","blue-dashed");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","8");
    $line->setAttribute("name","red-dashed");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","9");
    $line->setAttribute("name","gold-dashed");
    $cat->addChild( $line );
    #
    $cat  = XML::LibXML::Element->new( 'cat_config' );
    $cat->setAttribute("name","Rms+stdv");
    $parent->addChild( $cat );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","title");
    $attr->setAttribute("value","All stations");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","xlab");
    $attr->setAttribute("value","Lead time (hour)");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","ylab");
    $attr->setAttribute("value","");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","dx");
    $attr->setAttribute("value","float");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","dy");
    $attr->setAttribute("value","integer");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","IsLayoutPortrait");
    $attr->setAttribute("value","yes");
    $cat->addChild( $attr );
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","nx");
    $attr->setAttribute("value","3");
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","IsLayoutHorisontal");
    $attr->setAttribute("value","yes");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","referenceIsX");
    $attr->setAttribute("value","yes");
    $cat->addChild( $attr );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","1");
    $line->setAttribute("name","solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","2");
    $line->setAttribute("name","dotted");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","3");
    $line->setAttribute("name","dashed");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","4");
    $line->setAttribute("name","blue-solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","5");
    $line->setAttribute("name","red-solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","6");
    $line->setAttribute("name","gold-solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","7");
    $line->setAttribute("name","blue-dashed");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","8");
    $line->setAttribute("name","red-dashed");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","9");
    $line->setAttribute("name","gold-dashed");
    $cat->addChild( $line );
    #
    $cat  = XML::LibXML::Element->new( 'cat_config' );
    $cat->setAttribute("name","Scatter");
    $parent->addChild( $cat );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","title");
    $attr->setAttribute("value","All stations");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","xlab");
    $attr->setAttribute("value","X parameter (unit)");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","ylab");
    $attr->setAttribute("value","Y parameter (unit)");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","dx");
    $attr->setAttribute("value","float");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","dy");
    $attr->setAttribute("value","integer");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","IsLayoutPortrait");
    $attr->setAttribute("value","yes");
    $cat->addChild( $attr );
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","nx");
    $attr->setAttribute("value","3");
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","IsLayoutHorisontal");
    $attr->setAttribute("value","yes");
    $cat->addChild( $attr );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","1");
    $line->setAttribute("name","circle");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","2");
    $line->setAttribute("name","triangle");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","3");
    $line->setAttribute("name","star");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","4");
    $line->setAttribute("name","blue-circle");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","5");
    $line->setAttribute("name","red-circle");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","6");
    $line->setAttribute("name","gold-circle");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","7");
    $line->setAttribute("name","blue-triangle");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","8");
    $line->setAttribute("name","red-triangle");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","9");
    $line->setAttribute("name","gold-triangle");
    $cat->addChild( $line );
    #
    $cat  = XML::LibXML::Element->new( 'cat_config' );
    $cat->setAttribute("name","Histogram");
    $parent->addChild( $cat );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","title");
    $attr->setAttribute("value","All stations");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","xlab");
    $attr->setAttribute("value","X parameter (unit)");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","ylab");
    $attr->setAttribute("value","Y parameter (unit)");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","IsLayoutPortrait");
    $attr->setAttribute("value","yes");
    $cat->addChild( $attr );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","1");
    $line->setAttribute("name","solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","2");
    $line->setAttribute("name","dotted");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","3");
    $line->setAttribute("name","dashed");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","4");
    $line->setAttribute("name","blue-solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","5");
    $line->setAttribute("name","red-solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","6");
    $line->setAttribute("name","gold-solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","7");
    $line->setAttribute("name","blue-dashed");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","8");
    $line->setAttribute("name","red-dashed");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","9");
    $line->setAttribute("name","gold-dashed");
    $cat->addChild( $line );
    #
    $cat  = XML::LibXML::Element->new( 'cat_config' );
    $cat->setAttribute("name","Skill");
    $parent->addChild( $cat );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","title");
    $attr->setAttribute("value","All stations");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","units");
    $attr->setAttribute("value","m/s");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","referenceIsX");
    $attr->setAttribute("value","yes");
    $cat->addChild( $attr );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","1");
    $line->setAttribute("name","solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","2");
    $line->setAttribute("name","dotted");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","3");
    $line->setAttribute("name","dashed");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","4");
    $line->setAttribute("name","blue-solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","5");
    $line->setAttribute("name","red-solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","6");
    $line->setAttribute("name","gold-solid");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","7");
    $line->setAttribute("name","blue-dashed");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","8");
    $line->setAttribute("name","red-dashed");
    $cat->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line' );
    $line->setAttribute("id","9");
    $line->setAttribute("name","gold-dashed");
    $cat->addChild( $line );
};