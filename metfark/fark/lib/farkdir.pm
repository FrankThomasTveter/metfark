package farkdir;

=head1 NAME

  farkdir.pm - a perl library for handling fark directories

=head1 SYNOPSIS

   use farkdir;

=head1 DESCRIPTION

The fark perl library is a tool for handling fark directories.

=head1 SESSION FUNCTIONS

=cut

use 5.014002;
use strict;
use warnings;

use Getopt::Long;
use Pod::Usage qw(pod2usage);
use File::Path qw( make_path );
use Cwd 'abs_path';
use Capture::Tiny 'capture';
use File::Touch;

require Exporter;

our @ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use fark ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw() ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(%farkdirs);

our $VERSION = '0.13';

#
# This is where we define the root directories to the different classes of functions.
#
#
# NB: Must end with "/"  ...
our %farkdirs = ( data => {"/lustre/storeA/"   => "ro",                  # input data
			   "/lustre/storeB/"   => "ro",
			   "/opdata/"          => "ro",
			   "/metfark/data/"    => "rw" }, 
		  tables => {"/lustre/storeB/users/"     => "ro",        # BUFR tables
			     "/lustre/storeB/project/"   => "ro",
			     "/opdata/"                  => "ro",
			     "/metfark/bufrtables/"      => "ro" }, 
		  output => {"/lustre/storeA/project/nwp/fark/data/"     => "rw",
			     "/metfark/data/"          => "rw" }, 
		  script      => {"/lustre/storeA/project/nwp/fark/splus/" => "rw" }, # splus scripts
		  model       => {"/metfark/config/mod/"     => "rw" }, # model config files
		  model_old   => {"/metfark/config/old/mod/" => "rw" }, # model old config files
		  model_use   => {"/metfark/config/use/mod/" => "rw" }, # model use files
		  model_cache => {"/metfark/config/index/mod/" => "rw" }, # model cache/index files
		  model_reg   => {"/metfark/config/reg/mod/" => "rw" }, # model register files
		  model_log   => {"/lustre/storeA/project/nwp/fark/log/mod/" => "rw" }, # model log files
		  obs       => {"/metfark/config/obs/"       => "rw" },
		  obs_use   => {"/metfark/config/use/obs/"   => "rw" },
		  obs_cache => {"/metfark/config/index/obs/"   => "rw" },
		  obs_reg   => {"/metfark/config/reg/obs/"   => "rw" },
		  obs_old   => {"/metfark/config/old/obs/"   => "rw" },
		  obs_log   => {"/lustre/storeA/project/nwp/fark/log/obs/"   => "rw" },
		  coloc       => {"/metfark/config/coloc/"   => "rw" },
		  coloc_use   => {"/metfark/config/use/coloc/"   => "rw" },
		  coloc_reg   => {"/metfark/config/reg/coloc/"   => "rw" },
		  coloc_old   => {"/metfark/config/old/coloc/"   => "rw" },
		  coloc_log   => {"/lustre/storeA/project/nwp/fark/log/coloc/"   => "rw" },
		  plot =>      {"/metfark/config/plot/"    => "rw" },
		  plot_old =>  {"/metfark/config/old/plot/"    => "rw" },
		  plot_use =>  {"/metfark/config/use/plot/"    => "rw" },
		  plot_log =>  {"/lustre/storeA/project/nwp/fark/log/plot/"    => "rw" },
		  auto  => {"/metfark/config/auto/"    => "rw" },       # auto config files
		  url  =>  {"/metfark/config/url/"     => "rw" },       # url config files (not used?)
		  lock =>  {"/metfark/config/lock/"    => "rw" }        # lock files (must be local disk)
    );

# returns dir and name components of path
# my ($dir,$name) = splitName($path);
sub splitName {
    my $ipath=shift; # input path
    if ($ipath =~ /\/+\.+$/) {
	$ipath=$ipath . "/";
    };
    my $dir="";   # parent directory
    my $name=$ipath;
    if ($ipath =~ m/^(.*\/)\/*([^\/]*)$/) {
	$dir = $1; # parent directory
	$name = $2;
    };
    #print "splitName '$ipath' -> '$dir' '$name'\n";
    return ($dir, $name);
}

# returns root and location components of path, and privileges
# my ($root, $loc, $priv) = splitDir( $ipath, $cls );

sub splitDir {
    my $ipath=shift; # input path
    my $cls=shift;   # class
    my $priv = "";   # ipath privileges
    my $root = "";   # root
    my $loc = "";    # location
    makeRoot($cls);
    my $ddir = (keys (%{$farkdirs{$cls}}))[0]; # get default path
    # ...get complete path
    my $pdir = $ipath;
    my $tdir = $ddir;
    if ($pdir ne "") {
	my $prefix = substr $pdir,0,1;
	if ($prefix eq "\/") {
	    $tdir = $pdir; # absolute path
	} elsif (defined $ddir) {
	    $tdir = $ddir . $pdir; # relative path
	} else {
	    $tdir = $pdir; # relative path
	};
    }
    #
    #print "Dir: '$ipath' => '$tdir'\n";
    # get abs path
    if (defined $tdir && -d $tdir) { # total path
	my $adir = abs_path( $tdir ); # absolute total path
	if (defined $adir) {
	    $priv = "denied";
	    foreach my $k (keys %{$farkdirs{$cls}}) {
		my $kk = substr($k,0,-1); # remove last "/" from "%farkdirs" directory name
		#print "Checking $adir~$kk $priv\n";
		if ( $tdir =~ /^$kk\/?(.*)$/) {
		    $root = $k;
		    $loc = $1;
		    $loc =~ s/\/+/\//g;
		    if ($loc ne "") {
			my $suffix = substr $loc,-1;
			if ($suffix ne "\/") {
			    $loc = $loc . "/"; # relative path
			};
		    }
		    $priv=$farkdirs{$cls}{$k};
		    #print "Match $adir~$kk $priv\n";
		}
	    }
	} else {
	    $root="";
	    $loc=$tdir;
	    $priv="invalid";
	}
    } else {
	$root="";
	$loc=$tdir // "";
	$priv="missing";
    }
    #print "splitDir '$ipath' '$cls' -> '$root' '$loc' '$priv'\n";
    return ( $root, $loc, $priv );
}

sub makeRoot {
    my $cls = shift;
    my @dirs=keys %{$farkdirs{$cls}};
    if (@dirs) {
	if (! -d $dirs[0] && $farkdirs{$cls}{$dirs[0]} eq "rw") { 
	    return makePath($dirs[0]);
	};
    }
    return 0; # fail
}

sub getRootDir {
    my $cls = shift;
    my @dirs=keys %{$farkdirs{$cls}};
    if (@dirs) {
	if (! -d $dirs[0]) { 
	    if (makePath($dirs[0])) {
		chmod 0777, $dirs[0];
		return $dirs[0]; # success
	    } else {
		return; # failure
	    }
	} else {
	    return $dirs[0];
	}
    } else {
	$_="Undefined dir-class $cls";
	return; # failure
    };
}

#
# Check that directory exists, create if necessary...
#   my $ret = check_dir($path);
#

sub makePath{
    my $path=shift;
#    print "Path:$path\n";
    eval {
	my $log=capture {
	    if(!-d $path) {
		make_path $path; 
		chmod 0777, $path;
	    }
	};
#	print $log;
    };
    my $ret=$@;
    if ($ret) {
	$_=$ret;
	return(0);
    }else {
	return(1); # success
    }
}

sub touchFile {
    my $path=shift;
    my ($dir,$name)=splitName($path);
    if (! -d $dir) {makePath($dir);}
    #print "Touching $path\n";
    if (touch($path)) {
	if (chmod 0777, $path) {
	    #print "Touched $path\n";
	    return 1;
	} else {
	    return 0;
	}
    } else {
	system "touch $path";
	if ($? == -1 || $? & 127) {
	    return 0;
	} else {
	    return 1;
	}
    }
}

#
# Remove password-protected file
#   my $ret=remove_file($path, $password);
#

sub removeFile{
    my $path=shift;
    my $password=shift;
    if(-f $path) {
	my $parser = XML::LibXML->new();
	my $doc = $parser->parse_file($path);
	my @nodes=();
	if (! @nodes) {@nodes=$doc->findnodes("model/model_config");};
	if (! @nodes) {@nodes=$doc->findnodes("obs/obs_config");};
	if (! @nodes) {@nodes=$doc->findnodes("coloc/coloc_config");};
	if (! @nodes) {@nodes=$doc->findnodes("plot/plot_config");};
	if (@nodes) {
	    my $size = @nodes;
	    #print "Found $size nodes in file $path\n";
	    foreach my $node (@nodes) { 
		my $fpassword = $node->getAttribute("password"); 
		#print "Found password: $fpassword\n";
		if ($fpassword ne "" && $fpassword ne $password) {
		    &term( "Password mismatch in $path");
		    #print "Keeping file  $path\n";
		} else {
		    #print "Removing file $path\n";
		    unlink($path) or &term("Unable to delete $path: $!");
		}
	    }
	} else {
	    #print "Removing file $path\n";
	    unlink($path) or &term("Unable to delete $path: $!");
	}
    } else {
	&term( "Invalid file $path");
    }
    return(1); # success
}

#
# Remove password-protected files in directory, and directory tree if empty
#    my $ret = removeDir($root,$password);
#

sub removeDir {
    my $root = shift;
    my $password = shift;
    my ($sdir) = &cwd; 
    my $ret=0;
    opendir(DIR, $root) or &term("Unable to open $root: $!");
    my $parser = XML::LibXML->new();
    my @dirs = ();
    #print "Here...\n";
    my @entries = sort { $a cmp $b } readdir(DIR);
    while ( my $name = shift @entries ){
	next if ($name eq ".");
	next if ($name eq "..");
	next if (-l $name ); # skip symlinks
	my $path=$root . $name;
	#print "Processing $path\n";
	if (-d $path){
	    push (@dirs, $name);
	} elsif (-f $path ) {
	    my $doc = $parser->parse_file($path);
	    my @nodes=();
	    if (! @nodes) {@nodes=$doc->findnodes("model/model_config");};
	    if (! @nodes) {@nodes=$doc->findnodes("obs/obs_config");};
	    if (! @nodes) {@nodes=$doc->findnodes("coloc/coloc_config");};
	    if (! @nodes) {@nodes=$doc->findnodes("plot/plot_config");};
	    my $size = @nodes;
	    #print "Found $size nodes in file $path\n";
	    foreach my $node (@nodes) { 
		my $fpassword = $node->getAttribute("password"); 
		if ($fpassword ne "" && $fpassword ne $password) {
		    $ret=1; # do not remove
		    &term("Password mismatch in $path");
		} else {
		    #print "Removing file $path\n";
		    unlink($path) or &term("Unable to delete $path: $!");
		}
	    }
	} else {
	    #print "How strange $path does not exist...\n";
	}
    };
    closedir(DIR);
    # loop over sub directories
    chdir($root) or &term("Unable to enter dir $root: $!");
    foreach my $name (@dirs) {
	#print "Processing $name\n";
	if (! &removeDir($name,$password)) {$ret=1;};
    }
    chdir($root) or &term("Unable to change to dir $root: $!");
    if ($ret) {
	&term("Keeping $root");
    } else {
	#print "Removing $root\n";
	rmdir($root) or &term("Unable to rmdir $root: $!");
    };
    chdir($sdir) or die "Unable to change to dir $sdir:$!\n";
    return($ret);
}

#
# my @files = &find (".*\.nc",$dir,"",time);
#

sub find{
    use Cwd;
    my $pattern = shift;
    my ($wdir) = shift;
    my $min = shift || "";
    my $max = shift || "";
    my $root = shift || "";
    my $t = shift || time;
    my $hits = shift || 0;
    my @ret = ();
    if (time - $t > 2.0) {return @ret;}; # use maximum 2 seconds
    #print "Time:" . (time-$t) . "\n";
    if ($root ne "") {
	$root=$root . $wdir;
    } else {
	$root=$wdir;
    }
    if (substr($root,-1) ne "/") { $root = $root . "/";};
    my ($sdir) = &cwd; 
    #print "Opening $wdir\n";
    opendir(DIR, $wdir) or die "Unable to open $wdir:$!\n";
    my @dirs = ();
    #print "Here...\n";
    my @entries = sort { $a cmp $b } readdir(DIR);
    while ( my $name = shift @entries ){
	#print "Checking $name\n";
	if (time - $t > 2.0 && $hits > 1) {last;}; # use max 2 seconds if we have hits
	if (time - $t > 10.0) {last;}; # use max 10 seconds
        next if ($name eq ".");
        next if ($name eq "..");
        next if (-l $name ); # skip symlinks
	my $path=$root . $name;
        if (-d $path){
	    push (@dirs, $name);
        } elsif (-f $path ) {
	    if ($name  =~ m/$pattern/) {
		my $ok=1;
		if ($min || $max) {
		    my $age=(-M $path);
		    #print "Age $age '$path' '$min' '$max'\n";
		    if ($min && $age < $min) {
			$ok=0;
		    };
		    if ($max && $age > $max) {
			$ok=0;
		    };
		};
		if ($ok) {
		    push (@ret, $path);
		    $hits++;
		}
	    } else {
		#print "No match: $pattern $name\n";
	    };
	} else {
	    #print "How strange $path does not exist...\n";
	}
    };
    closedir(DIR);
    # loop over sub directories
    chdir($wdir) or die "Unable to enter dir $wdir:$!\n";
    foreach my $name (@dirs) {
	#print "Processing $name\n";
	my @lret=&find($pattern,$name,$min,$max,$root,$t,$hits);
	push (@ret,@lret);
	$hits+=@lret;
    }
    chdir($sdir) or die "Unable to change to dir $sdir:$!\n";
    return sort @ret;
}

#
# my $ret = &term ("System error...");
#

sub term {
    my $msg=shift;
    $msg=~s/[^a-zA-Z0-9 _\-\+\.\,\/\:\[\]\(\)]/ /g;
    $msg=~s/ +/ /g;
    print "<error message='".$msg."'/>\n";
    exit 1;
}

sub info {
    my $msg=shift;
    $msg=~s/[^a-zA-Z0-9 _\-\+\.\,\/\:\[\]\(\)]/ /g;
    $msg=~s/ +/ /g;
    print "<info message='".$msg."'/>\n";
    exit 1;
}

sub termAll {
    my $msg=shift;
    $msg=~s/[^a-zA-Z0-9 _\-\+\.\,\/\:\[\]\(\)]/ /g;
    $msg=~s/\n/ /g;
    $msg=~s/ +/ /g;
    print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
    print "<error message='".$msg."'/>\n";
}

1;
