#!/usr/bin/perl -w
#
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use Capture::Tiny 'capture';
use File::Basename;
use File::Path qw( make_path remove_tree );
use File::Copy::Recursive qw(dircopy dirmove);
use POSIX 'strftime';
use Data::Dumper;
use Cwd; use Cwd 'abs_path';
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
my %dirs = ( data => {"/lustre/storeA/users/"     => "ro",
		      "/lustre/storeA/project/"   => "ro",
		      "/opdata/"                  => "ro",
		      "/elysium/data/"            => "rw" }, 
	     obs => {"/elysium/metfark/obs/"      => "rw" },
	     model => {"/elysium/metfark/mod/"      => "rw" },
	     coloc => {"/elysium/metfark/coloc/"  => "rw" },
	     plot => {"/elysium/metfark/plot/"    => "rw" }
    );
#
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
my $pub="/metfark/pub";
my $lockDir="$pub/lock"; check_path($lockDir);
my $ref=CGI->new();
#
$XML::LibXML::skipXMLDeclaration = 1;
my $param= $ref->{param};
if (! defined $param->{cmd}) {&term("Undefined cmd.".Dumper($param))};
if ($param->{cmd}->[0] eq "ls") {
    my $dir=$param->{path}->[0]||"";
    &cmdls($param,$dir);
} elsif ($param->{cmd}->[0] eq "cp") {
    my $dir=$param->{path}->[0]||"";
    my $dest=$param->{dest}->[0]||"";
    &cmdcp($param, $dir, $dest);
} elsif ($param->{cmd}->[0] eq "mv") {
    my $dir=$param->{path}->[0]||"";
    my $dest=$param->{dest}->[0]||"";
    &cmdmv($param, $dir, $dest);
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
    &term("Unknown cmd.".Dumper($param));
}

sub cmdls {
    my $param =shift;
    my $idir = shift;
    my $cls=$param->{cls}->[0] || "";
    # make a new document
    my $new = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $root = XML::LibXML::Element->new( 'ls' );
    if ($idir eq "") {
	$root->setAttribute("path","");
	$root->setAttribute("root","");
	$root->setAttribute("location","");
	$root->setAttribute("status","");
	foreach my $k (keys %{$dirs{$cls}}) {
	    my $d = XML::LibXML::Element->new( 'dir' );
	    $d->setAttribute("path",$k);
	    $root->addChild( $d );
	};
	# @adir should be a list of valid root directories if $idir is empty...
    } else {
	my $adir=getAdir($cls,$idir);
	if (defined $adir && -d $adir) {
	    my $dir = abs_path( $adir ) . "/" ;
	    my ($sdir,$sroot,$loc) = check_dir($dir,$cls);
	    #print "\n\n$adir => $dir\n\n";
	    $root->setAttribute("path",$dir);
	    $root->setAttribute("root",$sroot);
	    $root->setAttribute("location",$loc);
	    $root->setAttribute("status",$sdir);
	    $new->addChild( $root );
	    my $priv=getPrivileges($cls,$dir);
	    if ($priv eq "rw" || $priv eq "ro") {
		# load model data
		if (opendir (DIR, $dir)) {
		    while (my $file = readdir(DIR)) {
			next if ($file eq "." || $file eq "..");
			my $path=$dir . $file;
			if (-d $path) {
			    my $d = XML::LibXML::Element->new( 'dir' );
			    $d->setAttribute("path",$path);
			    $root->addChild( $d );
			} elsif (-f $path) {
			    my $f = XML::LibXML::Element->new( 'file' );
			    $f->setAttribute("path",$path);
			    $root->addChild( $f );
			}
		    }
		    closedir(DIR);
		} else {
		    &term("Unable to open directory.".Dumper($param));
		    # my $d = XML::LibXML::Element->new( 'error' );
		    # $d->setAttribute("message","Unable to open directory.");
		    # $root->addChild( $d );
		}
	    } else {
		&term("Permission denied ($dir,$priv).".Dumper($param));
		# my $d = XML::LibXML::Element->new( 'error' );
		# $d->setAttribute("message","Permission denied.");
		# $root->addChild( $d );
	    }
	} else {
	    foreach my $k (keys %{$dirs{$cls}}) {
		my $d = XML::LibXML::Element->new( 'dir' );
		$d->setAttribute("path",$k);
		$root->addChild( $d );
	    };
	};
    };
    print $root->toString . "\n";
};

sub cmdcp {
    my $param =shift;
    my $idir = shift;
    my $idest = shift;
    my $cls=$param->{cls}->[0] || "";
    # make a new document
    my $new = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $root = XML::LibXML::Element->new( 'cp' );
    my $adir=getAdir($cls,$idir);
    if (defined $adir && -d $adir) {
	my $dir = abs_path( $adir ) . "/" ;
	my $dest = abs_path( $idest ) . "/" ;
	#print "\n\n$adir => $dir\n\n";
	$root->setAttribute("path",$dir);
	$new->addChild( $root );
	my $priv=getPrivileges($cls,$dir);
	my $privdest=getPrivileges($cls,$dest);
	if ($priv eq "rw" && $privdest eq "rw") {
	    my $path=$dir;
	    my $path_dest=$dest;
	    local $File::Copy::Recursive::SkipFlop = 1; # continue if possible
	    my $ret=dircopy($path, $path_dest); # number of directories+files copied
	    if (! $ret) {
		&term("Unable to copy directory.".Dumper($param));
		# my $e = XML::LibXML::Element->new( 'error' );
		# $e->setAttribute("message","Unable to copy directory.");
		# $root->addChild( $e );
	    } else {
		$root->setAttribute("new",$path_dest);
		if (opendir (DIR, $dest)) {
		    while (my $file = readdir(DIR)) {
			next if ($file eq "." || $file eq "..");
			my $path=$dest . $file;
			if (-d $path) {
			    my $d = XML::LibXML::Element->new( 'dir' );
			    $d->setAttribute("path",$path);
			    $root->addChild( $d );
			} elsif (-f $path) {
			    my $f = XML::LibXML::Element->new( 'file' );
			    $f->setAttribute("path",$path);
			    $root->addChild( $f );
			}
		    }
		    closedir(DIR);
		} else {
		    &term("Unable to open directory.".Dumper($param));
		    # my $d = XML::LibXML::Element->new( 'error' );
		    # $d->setAttribute("message","Unable to open directory.");
		    # $root->addChild( $d );
		}
	    };
	} else {
	    &term("Permission denied ($dir,$priv).".Dumper($param));
	    # my $e = XML::LibXML::Element->new( 'error' );
	    # $e->setAttribute("message","Permission denied.");
	    # $root->addChild( $e );
	}
    } else {
	&term("Invalid directory ($adir).".Dumper($param));
	# my $e = XML::LibXML::Element->new( 'error' );
	# $e->setAttribute("message","Invalid directory.");
	# $root->addChild( $e );
    };
    print $root->toString . "\n";
};

sub cmdmv {
    my $param =shift;
    my $idir = shift;
    my $idest = shift;
    my $cls=$param->{cls}->[0] || "";
    # make a new document
    my $new = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $root = XML::LibXML::Element->new( 'mv' );
    my $adir=getAdir($cls,$idir);
    if (defined $adir && -d $adir) {
	my $dir = abs_path( $adir ) . "/" ;
	my $dest = abs_path( $idest ) . "/" ;
	#print "\n\n$adir => $dir\n\n";
	$root->setAttribute("path",$dir);
	$new->addChild( $root );
	my $priv=getPrivileges($cls,$dir);
	my $privdest=getPrivileges($cls,$dest);
	if ($priv eq "rw" && $privdest eq "rw") {
	    my $path=$dir;
	    my $path_dest=$dest;
	    local $File::Copy::Recursive::SkipFlop = 1; # continue if possible
	    my $ret=dirmove($path, $path_dest); # number of directories+files copied
	    if (! $ret) {
		&term("Unable to move directory.".Dumper($param));
		# my $e = XML::LibXML::Element->new( 'error' );
		# $e->setAttribute("message","Unable to copy directory.");
		# $root->addChild( $e );
	    } else {
		$root->setAttribute("new",$path_dest);
		if (opendir (DIR, $dest)) {
		    while (my $file = readdir(DIR)) {
			next if ($file eq "." || $file eq "..");
			my $path=$dest . $file;
			if (-d $path) {
			    my $d = XML::LibXML::Element->new( 'dir' );
			    $d->setAttribute("path",$path);
			    $root->addChild( $d );
			} elsif (-f $path) {
			    my $f = XML::LibXML::Element->new( 'file' );
			    $f->setAttribute("path",$path);
			    $root->addChild( $f );
			}
		    }
		    closedir(DIR);
		} else {
		    &term("Unable to open directory.".Dumper($param));
		    # my $d = XML::LibXML::Element->new( 'error' );
		    # $d->setAttribute("message","Unable to open directory.");
		    # $root->addChild( $d );
		}
	    };
	} else {
	    &term("Permission denied ($dir,$priv).".Dumper($param));
	    # my $e = XML::LibXML::Element->new( 'error' );
	    # $e->setAttribute("message","Permission denied.");
	    # $root->addChild( $e );
	}
    } else {
	&term("Invalid directory ($adir).".Dumper($param));
	# my $e = XML::LibXML::Element->new( 'error' );
	# $e->setAttribute("message","Invalid directory.");
	# $root->addChild( $e );
    };
    print $root->toString . "\n";
};

sub cmdmk {
    my $param =shift;
    my $idir = shift;
    my $cls=$param->{cls}->[0] || "";
    # make a new document
    my $new = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $root = XML::LibXML::Element->new( 'mk' );
    my $adir=getAdir($cls,$idir);
    my $rdir=dirname($adir);
    my $ndir=basename($adir);
    if (defined $adir && -d $rdir) {
	my $dir = abs_path( $rdir ) . "/";
	if ($ndir) {  $dir=$dir. $ndir . "/"  ;};
	#print "\n\n$adir => $dir\n\n";
	$root->setAttribute("path",$dir);
	$new->addChild( $root );
	my $priv=getPrivileges($cls,$dir);
	#print "############# Priv=$priv:\n";
	if ($priv eq "rw") {
	    #print "Dest: $dest\n";
	    my $path=$dir;
	    my $ret=check_path($path);
	    if ($ret) {
		&term("Unable to make directory.".Dumper($param));
		# my $e = XML::LibXML::Element->new( 'error' );
		# $e->setAttribute("message","Unable to make directory.");
		# $root->addChild( $e );
	    } else {
		$root->setAttribute("new",$path);
		if (opendir (DIR, $dir)) {
		    while (my $file = readdir(DIR)) {
			next if ($file eq "." || $file eq "..");
			my $path=$dir . $file;
			if (-d $path) {
			    my $d = XML::LibXML::Element->new( 'dir' );
			    $d->setAttribute("path",$path);
			    $root->addChild( $d );
			} elsif (-f $path) {
			    my $f = XML::LibXML::Element->new( 'file' );
			    $f->setAttribute("path",$path);
			    $root->addChild( $f );
			}
		    }
		    closedir(DIR);
		} else {
		    &term("Unable to open directory.".Dumper($param));
		    # my $d = XML::LibXML::Element->new( 'error' );
		    # $d->setAttribute("message","Unable to open directory.");
		    # $root->addChild( $d );
		}
	    };
	} else {
	    &term("Permission denied ($cls,$adir,$dir,$priv).".Dumper($param));
	    # my $e = XML::LibXML::Element->new( 'error' );
	    # $e->setAttribute("message","Permission denied.");
	    # $root->addChild( $e );
	}
    } else {
	&term("Invalid directory ($adir).".Dumper($param));
	# my $e = XML::LibXML::Element->new( 'error' );
	# $e->setAttribute("message","Invalid directory.");
	# $root->addChild( $e );
    };
    print $root->toString . "\n";
};

sub cmdrm {
    my $param =shift;
    my $idir = shift;
    my $cls=$param->{cls}->[0] || "";
    # make a new document
    my $new = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $root = XML::LibXML::Element->new( 'rm' );
    my $adir=getAdir($cls,$idir);
    if (defined $adir && -d $adir) {
	my $dir = abs_path( $adir ) . "/" ;
	#print "\n\n$adir => $dir\n\n";
	$root->setAttribute("path",$dir);
	$new->addChild( $root );
	my $priv=getPrivileges($cls,$dir);
	if ($priv eq "rw") {
	    my $path=$dir;
	    my $ret=remove_path($path);
	    if ($ret) {
		&term("Unable to remove directory.".Dumper($param));
		# my $e = XML::LibXML::Element->new( 'error' );
		# $e->setAttribute("message","Unable to remove directory.");
		# $root->addChild( $e );
	    } else {
		$root->setAttribute("removed",$path);
	    };
	} else {
	    &term("Permission denied ($dir,$priv).".Dumper($param));
	    # my $e = XML::LibXML::Element->new( 'error' );
	    # $e->setAttribute("message","Permission denied.");
	    # $root->addChild( $e );
	}
    } else {
	&term("Invalid directory ($adir).".Dumper($param));
	# my $e = XML::LibXML::Element->new( 'error' );
	# $e->setAttribute("message","Invalid directory.");
	# $root->addChild( $e );
    };
    print $root->toString . "\n";
};

sub cmdrf {
    my $param =shift;
    my $ifile = shift;
    my $idir = dirname($ifile);
    my $cls=$param->{cls}->[0] || "";
    # make a new document
    my $new = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $root = XML::LibXML::Element->new( 'rm' );
    my $adir=getAdir($cls,$idir);
    if (defined $adir && -d $adir) {
	my $dir = abs_path( $adir ) . "/" ;
	#print "\n\n$adir => $dir\n\n";
	$root->setAttribute("path",$dir);
	$new->addChild( $root );
	my $priv=getPrivileges($cls,$dir);
	if ($priv eq "rw") {
	    my $path=$dir . $ifile;
	    my $ret=remove_file($path);
	    if ($ret) {
		&term("Unable to remove file.".Dumper($param));
		# my $e = XML::LibXML::Element->new( 'error' );
		# $e->setAttribute("message","Unable to remove directory.");
		# $root->addChild( $e );
	    } else {
		$root->setAttribute("removed",$path);
	    };
	} else {
	    &term("Permission denied ($dir,$priv).".Dumper($param));
	    # my $e = XML::LibXML::Element->new( 'error' );
	    # $e->setAttribute("message","Permission denied.");
	    # $root->addChild( $e );
	}
    } else {
	&term("Invalid directory ($adir).".Dumper($param));
	# my $e = XML::LibXML::Element->new( 'error' );
	# $e->setAttribute("message","Invalid directory.");
	# $root->addChild( $e );
    };
    print $root->toString . "\n";
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

sub remove_file{
    my $path=shift;
    eval {
	my $log=capture {
	    if(-f $path) {
		unlink ($path);
	    }
	}
    };my $ret=$@;
    if ($ret) {
	return($ret);
    }else {
	return(0);
    }
}

sub term {
    my $msg=shift;
    $msg=~s/[^a-zA-Z0-9 _\-\+\.\,\/\:\[\]\(\)]/ /g;
    $msg=~s/ +/ /g;
    print "<error message='".$msg."'/>\n";
    exit 1;
}

sub getPrivileges {
    my $cls = shift;
    my $dir = shift;
    foreach my $k (keys %{$dirs{$cls}}) {
	if ( $dir =~ /$k/) {
	    return $dirs{$cls}{$k};
	}
    }
    return "Denied";
}
sub getPrivileges3 {
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

sub getAdir {
    my $cls = shift;
    my $idir = shift;
    #print "Idir:'".$idir."'\n";
    if ($idir =~ /^[^\/].*/ || $idir eq "" || $idir eq ".") {
	my @k = keys (%{$dirs{$cls}});
	#print "Local dir:'".$k[0]."'\n";
	return $k[0] . $idir;
    } else {
	return $idir;
    }
}
sub check_dir {
    my $idir = shift;
    my $cls = shift;
    if (-d $idir) {
	my $adir = abs_path( $idir );
	return getPrivileges3( $adir, $cls );
    } else {
	return ("missing","",$idir); # missing directory
    }
}

