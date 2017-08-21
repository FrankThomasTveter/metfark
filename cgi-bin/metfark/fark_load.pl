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
use Cwd; use Cwd 'abs_path';
# must end with slash...
my %dirs = ( "data" => {"/lustre/storeA/users/"     => "ro",
		      "/lustre/storeA/project/"   => "ro",
		      "/elysium/data/"            => "rw" }, 
	     "obs" => {"/elysium/metfark/obs/"      => "rw" },
	     "model" => {"/elysium/metfark/mod/"      => "rw" },
	     "coloc" => {"/elysium/metfark/coloc/"  => "rw" },
	     "plot" => {"/elysium/metfark/plot/"    => "rw" },
	     "auto" => {"/elysium/metfark/auto/"    => "rw" }
    );
#
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
my $user=$ENV{USERNAME} // "www";
$user="www";
my $pub="/metfark/pub";

my $useModelDir="$pub/use/model/";check_path($useModelDir);
my $useObsDir="$pub/use/obs/";check_path($useObsDir);
my $lockDir="$pub/lock"; check_path($lockDir);
#
#
my $ref=CGI->new();
#
$XML::LibXML::skipXMLDeclaration = 1;
my $param= $ref->{param};
my $types = $param->{"type"};
if (! defined $types) { $types = $param->{"type[]"}};
my $args = $param->{"arg[]"};
#
if (! defined $types) {&term("Undefined type.".Dumper($param))};
#
my $doc = XML::LibXML::Document->new( '1.0', 'UTF-8' );
my $top = XML::LibXML::Element->new( 'load' );
$doc->addChild( $top );
foreach my $cls (@{$types}) {
#
    if ($cls eq "auto") {
	if (! defined $args) {&term("Undefined arg.".Dumper($param))};
	my $arg = $args->[0];
	&loadAuto($doc,$top,$param, $arg);
    } elsif ($cls eq "cat") {
	&loadCat($doc,$top,$param);
    } elsif ($cls eq "line") {
	&loadLine($doc,$top,$param);
    } else {
	if (! defined $args) {&term("Undefined arg.".Dumper($param))};
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
    my $idir = (keys (%{$dirs{$cls}}))[0];
    my( $adir, $afile ) = splitArg($arg);
    #print "arg: $adir $afile\n";
    my $xdir = $idir;
    my $prefix = substr $adir,0,1;
    if ($prefix eq "\/") {
	$xdir = $adir; # absolute path
    } else {
	$xdir = $xdir . $adir; # relative path
    };
    # make a new document
    my $parent = XML::LibXML::Element->new( $cls );
    $top->addChild( $parent );
    # load model data
    my $parser = XML::LibXML->new();
    #$parser->expand_entities( 0 ); # leave entities alone
    if (-d $xdir.$afile) {
	$xdir = $xdir . $afile;
	my ($sdir,$root,$loc) = check_dir($xdir,$cls);
	my $node = $doc->createElement("Root");
	$node->setAttribute("class",     $cls);
	$node->setAttribute("root",      $root);
	$node->setAttribute("location",  $loc);
	$node->setAttribute("status",    $sdir);
	$node->setAttribute("arg",  $arg);
	$node->setAttribute("type",      "dir");
	$parent->addChild( $node );
    } elsif (-f $xdir.$afile) {
	my ($sdir,$root,$loc) = check_dir($xdir,$cls);
	my $node = $doc->createElement("Root");
	$node->setAttribute("class",     $cls);
	$node->setAttribute("root",      $root);
	$node->setAttribute("location",  $loc);
	$node->setAttribute("file",      $afile);
	$node->setAttribute("status",    $sdir);
	$node->setAttribute("arg",  $arg);
	$node->setAttribute("type",      "file");
	$parent->addChild( $node );
    } else {
	my ($sdir,$root,$loc) = check_dir($xdir,$cls);
	my $node = $doc->createElement("Root");
	$node->setAttribute("class",     $cls);
	$node->setAttribute("root",      $root);
	$node->setAttribute("location",  $loc);
	$node->setAttribute("child",     $afile);
	$node->setAttribute("status",    $sdir);
	$node->setAttribute("arg",  $arg);
	$node->setAttribute("type",      "unknown");
	$parent->addChild( $node );
    };
    my ($sdir,$root,$loc) = check_dir($xdir,$cls);
    #print "xdir: $xdir $sdir\n";
    if ($sdir eq "rw") {
	if (opendir (DIR, $xdir)) {
	    my $dir=abs_path($xdir);
	    while (my $child = readdir(DIR)) {
		#print "Child: $child\n";
		next if ($child eq "." || $child eq "..");
		my $path=$xdir . "/" . $child;
		if (-f $path) {
		    my $sfile=$sdir;
		    if (! -z $path) {# used for debugging
			my $mod = $parser->parse_file($path);
			my ($node)=$mod->findnodes($cls."/".$cls."_config");
			 # remove passwords...
			if ( $node ) {$node->removeAttribute("password");
				      $node->setAttribute("root",      $root);
				      $node->setAttribute("location",  $loc);
				      #print "**********Ref: " . ref($node) . "\n";
				      #print "*******Dumper: " . Dumper($node);
				      $parent->addChild( $node );
			};
		    } else {
			$sfile="empty";
		    };
		    {
			#print "Added file: $root $loc $child $sfile\n";
			my $node = $doc->createElement("File");
			$node->setAttribute("class",     $cls);
			$node->setAttribute("root",      $root);
			$node->setAttribute("location",  $loc);
			$node->setAttribute("file",      $child);
			$node->setAttribute("status",    $sfile);
			$parent->addChild( $node );
		    };
		} elsif (-d $path) {
		    #print "Added subdir: $root $loc $child $sdir\n";
		    # add directory to list
		    my $node = $doc->createElement("Dir");
		    $node->setAttribute("class",     $cls);
		    $node->setAttribute("root",      $root);
		    $node->setAttribute("location",  $loc);
		    $node->setAttribute("dir",      $child);
		    $node->setAttribute("status",    $sdir);
		    $parent->addChild( $node );
		} else {
		    #print "Ignored: $path\n";
		}
	    }
	    closedir(DIR);
	}
    }
};


sub loadAuto {
    my $doc=shift;
    my $top=shift;
    my $param =shift;
    my $arg = shift;
    my $idir = (keys (%{$dirs{"auto"}}))[0];
    my $file=($param->{root}->[0]||"auto.cfg");
    my $parent = XML::LibXML::Element->new( 'auto' );
    $top->addChild( $parent );
    # load obs data
    my $parser = XML::LibXML->new();
    #$parser->expand_entities( 0 ); # leave entities alone
    my $path=$idir . $file;
    if (-f $path) {
	my $auto = $parser->parse_file($path);
	my ($node)=$auto->findnodes("auto/auto_config");
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
};


sub loadCat {
    my $doc=shift;
    my $top=shift;
    my $param =shift;
    my $parent = XML::LibXML::Element->new( 'cat' );
    $top->addChild( $parent );
    my $cat;
    my $attr;
    #
    $cat  = XML::LibXML::Element->new( 'cat_config' );
    $cat->setAttribute("name","rms");
    $parent->addChild( $cat );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","title");
    $attr->setAttribute("value","default1");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","xlab");
    $attr->setAttribute("value","default2");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","ylab");
    $attr->setAttribute("value","default3");
    $cat->addChild( $attr );
    #
    $cat  = XML::LibXML::Element->new( 'cat_config' );
    $cat->setAttribute("name","stdv");
    $parent->addChild( $cat );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","title");
    $attr->setAttribute("value","default4");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","xlab");
    $attr->setAttribute("value","default5");
    $cat->addChild( $attr );
    $attr = XML::LibXML::Element->new( 'attr' );
    $attr->setAttribute("name","ylab");
    $attr->setAttribute("value","default6");
    $cat->addChild( $attr );
};

sub loadLine {
    my $doc=shift;
    my $top=shift;
    my $param =shift;
    my $parent = XML::LibXML::Element->new( 'line' );
    $top->addChild( $parent );
    my $line;
    #
    $line  = XML::LibXML::Element->new( 'line_config' );
    $line->setAttribute("name","solid");
    $line->setAttribute("id","1");
    $parent->addChild( $line );
    $line  = XML::LibXML::Element->new( 'line_config' );
    $line->setAttribute("name","dotted");
    $line->setAttribute("id","2");
    $parent->addChild( $line );
};

sub check_dir {
    my $idir = shift;
    my $cls = shift;
    if (-d $idir) {
	my $adir = abs_path( $idir );
	return getPrivileges( $adir, $cls );
    } else {
	return ("missing","",$idir); # missing directory
    }
}

sub check_path{
    my $path=shift;
    eval {
	my $log=capture {
	    if(!-d $path) {
		make_path $path; 
		chmod 0777, $path;
	    }
	}
    };my $ret=$@;
    if ($ret) {
	return($ret);
    }else {
	return(0);
    }
}

sub remove_path{
    my $path=shift;
    eval {
	my $log=capture {
	    if(-d $path) {
		remove_tree( $path ) ; 
	    }
	}
    };my $ret=$@;
    if ($ret) {
	return($ret);
    }else {
	return(0);
    }
}

sub check_path2{
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

sub getPrivileges {
    my $dir = shift;
    my $cls = shift;
    foreach my $k (keys %{$dirs{$cls}}) {
	my $kk = substr($k,0,-1); # remove last "/" from "%dirs" directory name
	#print "Checking $dir~$kk\n";
	if ( $dir =~ /$kk\/?(.*)/) {
	    return ($dirs{$cls}{$k},$k,$1);
	}
    }
    return ("denied","",$dir);
}

sub makeDirNode {
    my $doc = shift;
    my $cls  = shift;
    my $root = shift;
    my $loc  = shift;
    my $item = shift;
    my $stat = shift;
    my $child = $doc->createElement("dir");
    $child->setAttribute("class",     $cls);
    $child->setAttribute("root",      $root);
    $child->setAttribute("location",  $loc);
    $child->setAttribute("child",     $item);
    $child->setAttribute("status",    $stat);
    return $child;
}
sub makeFileNode {
    my $doc = shift;
    my $cls  = shift;
    my $root = shift;
    my $loc  = shift;
    my $item = shift;
    my $stat = shift;
    my $child = $doc->createElement("file");
    $child->setAttribute("class",     $cls);
    $child->setAttribute("root",      $root);
    $child->setAttribute("location",  $loc);
    $child->setAttribute("child",     $item);
    $child->setAttribute("status",    $stat);
    return $child;
}

sub splitArgs {
    my $arg = shift;
    my @ret=();
    if ($arg =~ /^\s*(\S+)\s+(\S.*)\s*$/) {
	push (@ret,[splitArg($1)]);
	push (@ret,splitArgs($2));
    } else {
	push (@ret,[splitArg($arg)]);
    }
    return @ret;
}
sub splitArg {
    my $arg = shift;
    if ($arg =~ /^(.*\/)([^\/]*)$/) {
	return ($1,$2)
    } else {
	return ("",$arg)
    }
}
