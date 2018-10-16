#!/usr/bin/perl -w
#
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use Capture::Tiny 'capture';
use POSIX 'strftime';
#use Data::Dumper;
use farkdir;
use URI::Encode;
#
# Syntax:
#  ./fark_dir.pl '&cmd=mv;cls=obs;path=/elysium/metfark/obs/dest;dest=/elysium/metfark/obs/dest2;'
#
# Valid parameters:
#  cmd= mk ls cp rm mv
#  cls= data obs mod coloc plot
#
# Only directories within valid domains are visible to user.
# Available domains (and permissions) are given below:
#
#
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
my $pub="/metfark/pub";
my $lockDir="$pub/lock"; farkdir::makePath($lockDir);
my $ref=CGI->new();
#
$XML::LibXML::skipXMLDeclaration = 1;
my $param= $ref->{param};
if (! defined $param->{cmd}) {farkdir::term("Undefined 'command'")};
if ($param->{cmd}->[0] eq "ls") {
    my $dir=$param->{path}->[0]||"";
    &cmdls($param,$dir);
} elsif ($param->{cmd}->[0] eq "mk") {
    my $dir=$param->{path}->[0]||"";
    &cmdmk($param, $dir);
} elsif ($param->{cmd}->[0] eq "rm") {
    my $dir=$param->{path}->[0]||"";
    &cmdrm($param, $dir);
} elsif ($param->{cmd}->[0] eq "rf") {
    my $dir=$param->{path}->[0]||"";
    &cmdrf($param, $dir);
} else {
    farkdir::term("Unknown 'command' ".$param->{cmd}->[0]);
}

sub cmdls {
    my $param =shift;
    my $idir = shift;
    my $cls=$param->{cls}->[0] || "";
    my $filter=$param->{filter}->[0] || "";
    # make a new document
    my $doc = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $parent = XML::LibXML::Element->new( 'ls' );
    my $roots = 0;
    if ($idir eq "") { # only list valid root directories
	$roots=1;
    } else {
	my ($root, $loc, $priv) = farkdir::splitDir( $idir, $cls );
	my $fpath=$root . $loc;
	#print "Path '$root' '$loc' '$priv' '$idir' '$fpath'\n";
	if ($priv eq "rw" || $priv eq "ro") {
	    $parent->setAttribute("path",$fpath);
	    $parent->setAttribute("root",$root);
	    $parent->setAttribute("location",$loc);
	    $parent->setAttribute("status",$priv);
	    $doc->addChild( $parent );
	    if (-d $fpath && opendir (DIR, $fpath)) {
		my @entries = sort { $a cmp $b } readdir(DIR);
		my @files;
		while (my $name = shift @entries) {
		    next if (substr($name,0,1) eq ".");
		    #print "Processing: $name '". substr($name,0,1) . "'\n";
		    my $path=$fpath . $name;
		    if (-d $path) {
			my $d = XML::LibXML::Element->new( 'dir' );
			$d->setAttribute("path",$path);
			$parent->addChild( $d );
		    } elsif (-f $path) {
			if (! $filter || $name =~ m/$filter/ ) {
			    my $size = size_in_mb(-s $path);
			    my $f = XML::LibXML::Element->new( 'file' );
			    $f->setAttribute("path",$path);
			    $f->setAttribute("size",$size);
			    $parent->addChild( $f );
			    push(@files,$name);
			}
		    }
		}
		closedir(DIR);
		# list patterns and regexp
		my %patterns = farkdir::findPattern(@files);
		foreach my $patt (sort keys %patterns) {
		    my $f = XML::LibXML::Element->new( 'pattern' );
		    $f->setAttribute("struct",$patt);
		    $f->setAttribute("regexp",$patterns{$patt}[0]);
		    $parent->addChild( $f );
		} 
	    } else {
		farkdir::term("Unable to open directory.");
		# my $d = XML::LibXML::Element->new( 'error' );
		# $d->setAttribute("message","Unable to open directory.");
		# $parent->addChild( $d );
	    }
	} else {
	    farkdir::term("permission=$priv $idir (permission denied)");
	    # my $d = XML::LibXML::Element->new( 'error' );
	    # $d->setAttribute("message","Permission denied.");
	    # $parent->addChild( $d );
	};
    };
    if ($roots) {  # only list valid root directories
	#print "Printing roots.\n";
	$parent->setAttribute("path","");
	$parent->setAttribute("root","");
	$parent->setAttribute("location","");
	$parent->setAttribute("status","");
	$doc->addChild( $parent );
	foreach my $k (keys %{$farkdirs{$cls}}) {
	    my $d = XML::LibXML::Element->new( 'dir' );
	    $d->setAttribute("path",$k);
	    $parent->addChild( $d );
	};
    }
    print $doc->toString . "\n";
};


# make directory
sub cmdmk {
    my $param =shift;
    my $idir = shift;
    my $cls=$param->{cls}->[0] || "";
    my $filter=$param->{filter}->[0] || "";
    # make a new document
    my $doc = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $parent = XML::LibXML::Element->new( 'mk' );
    my ($dir, $ndir) = farkdir::splitName($idir);
    if ($ndir ne "") {
	my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
	my $fpath=$root . $loc;
	if (-d  $fpath && $priv eq "rw") {
	    my $npath=$fpath. $ndir;
	    #print "\n\n$fpath => $npath\n\n";
	    $parent->setAttribute("path",$fpath);
	    $doc->addChild( $parent );
	    my $ret=farkdir::makePath($npath); # make directory
	    if (! $ret) {
		farkdir::term("Unable to make directory $npath ($ret)");
		# my $e = XML::LibXML::Element->new( 'error' );
		# $e->setAttribute("message","Unable to make directory.");
		# $parent->addChild( $e );
	    } else {
		$parent->setAttribute("new",$npath);
		if (opendir (DIR, $fpath)) {
		    my @entries = sort { $a cmp $b } readdir(DIR);
		    while (my $name = shift @entries) {
			next if ($name eq "." || $name eq "..");
			my $path=$fpath . $name;
			if (-d $path) {
			    my $d = XML::LibXML::Element->new( 'dir' );
			    $d->setAttribute("path",$npath);
			    $parent->addChild( $d );
			} elsif (-f $path) {
			    if (! $filter || $name =~ m/$filter/ ) {
				my $size = size_in_mb(-s $path);
				my $f = XML::LibXML::Element->new( 'file' );
				$f->setAttribute("path",$npath);
				$f->setAttribute("size",$size);
				$parent->addChild( $f );
			    }
			}
		    }
		    closedir(DIR);
		} else {
		    farkdir::term("Unable to open directory $fpath ($!)");
		    # my $d = XML::LibXML::Element->new( 'error' );
		    # $d->setAttribute("message","Unable to open directory.");
		    # $parent->addChild( $d );
		}
	    };
	} else {
	    farkdir::term("permission=$priv $fpath (denied)");
	    # my $e = XML::LibXML::Element->new( 'error' );
	    # $e->setAttribute("message","Permission denied.");
	    # $parent->addChild( $e );
	}
    } else {
	farkdir::term("Invalid mk directory ($idir)");
	# my $e = XML::LibXML::Element->new( 'error' );
	# $e->setAttribute("message","Invalid directory.");
	# $parent->addChild( $e );
    };
    print $doc->toString . "\n";
};

# remove directory (with matching password)
sub cmdrm {
    my $param =shift;
    my $idir = shift;
    my $cls=$param->{cls}->[0] || "";
    my $password=($param->{password}[0] // "");
    # make a new document
    my $doc = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $parent = XML::LibXML::Element->new( 'rm' );
    my ($root, $loc, $priv) = farkdir::splitDir( $idir, $cls );
    if ($root ne "" && $loc eq "") { $priv="ro";};
    my $fpath=$root . $loc;
    if (-d $fpath) {
	#print "\n\n$fpath => $root $loc\n\n";
	$parent->setAttribute("path",$fpath);
	$parent->setAttribute("root",$root);
	$parent->setAttribute("location",$loc);
	$parent->setAttribute("status",$priv);
	$doc->addChild( $parent );
	if ($priv eq "rw") {
	    #print "Priv: $priv, '$fpath' $cls '$password'\n";
	    my $ret=farkdir::removeDir($fpath,$password);
	    if ($ret) {
		farkdir::term("Unable to remove directory $fpath ($!)");
		# my $e = XML::LibXML::Element->new( 'error' );
		# $e->setAttribute("message","Unable to remove directory.");
		# $parent->addChild( $e );
	    } else {
		$parent->setAttribute("removed",$fpath);
	    };
	} else {
	    farkdir::term("Permission $priv $fpath (denied)");
	    # my $e = XML::LibXML::Element->new( 'error' );
	    # $e->setAttribute("message","Permission denied.");
	    # $parent->addChild( $e );
	}
    } else {
	farkdir::term("Invalid rm directory $fpath");
	# my $e = XML::LibXML::Element->new( 'error' );
	# $e->setAttribute("message","Invalid directory.");
	# $parent->addChild( $e );
    };
    print $doc->toString . "\n";
};

sub cmdrf {
    my $param =shift;
    my $ipath = shift; # full path
    my ($idir, $iname) = farkdir::splitName($ipath);
    my $cls=$param->{cls}->[0] || "";
    my $password=($param->{password}[0] // "");
    # make a new document
    my $doc = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $parent = XML::LibXML::Element->new( 'rm' );
    my ($root, $loc, $priv) = farkdir::splitDir( $idir, $cls );
    my $fpath=$root . $loc;
    if (-d $fpath) {
	#print "Path: $fpath $idir $ipath\n";
	$parent->setAttribute("path",$fpath);
	$parent->setAttribute("root",$root);
	$parent->setAttribute("location",$loc);
	$parent->setAttribute("status",$priv);
	$doc->addChild( $parent );
	my $path=$fpath . $iname;
	if ($priv eq "rw") {
	    if (farkdir::removeFile($path,$password)) {
		$parent->setAttribute("removed",$path);
	    } else {
		farkdir::term("Unable to remove file $path ($!)");
		# my $e = XML::LibXML::Element->new( 'error' );
		# $e->setAttribute("message","Unable to remove directory.");
		# $parent->addChild( $e );
	    };
	} else {
	    farkdir::term("Permission=$priv  $fpath (denied)");
	    # my $e = XML::LibXML::Element->new( 'error' );
	    # $e->setAttribute("message","Permission denied.");
	    # $parent->addChild( $e );
	}
    } else {
	farkdir::term("Invalid rf directory $fpath");
	# my $e = XML::LibXML::Element->new( 'error' );
	# $e->setAttribute("message","Invalid directory.");
	# $parent->addChild( $e );
    };
    print $doc->toString . "\n";
};

sub size_in_mb {
    my $size=shift;
    my $text= reverse (sprintf "%.2fMb", $size/(1024 * 1024));
    $text =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
    $text=reverse $text;
    return $text;
}
