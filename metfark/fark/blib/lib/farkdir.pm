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

# Preloaded methods go here.

# NB: Must end with "/"  ...
our %farkdirs = ( data => {"/lustre/storeA/"   => "ro",
			   "/lustre/storeB/"   => "ro",
			   "/opdata/"          => "ro",
			   "/elysium/data/"    => "rw" }, 
		  tables => {"/lustre/storeA/users/"     => "ro",
			     "/lustre/storeA/project/"   => "ro",
			     "/opdata/"                  => "ro",
			     "/elysium/"                 => "ro" }, 
		  output => {"/lustre/storeA/"    => "rw",
			     "/lustre/storeB/"    => "rw",
			     "/elysium/"          => "rw" }, 
		  obs       => {"/elysium/metfark/obs/"       => "rw" },
		  obs_use   => {"/elysium/metfark/use/obs/"   => "rw" },
		  obs_cache => {"/elysium/metfark/cch/obs/"   => "rw" },
		  obs_reg   => {"/elysium/metfark/reg/obs/"   => "rw" },
		  obs_old   => {"/elysium/metfark/old/obs/"   => "rw" },
		  model       => {"/elysium/metfark/mod/"     => "rw" },
		  model_use   => {"/elysium/metfark/use/mod/" => "rw" },
		  model_cache => {"/elysium/metfark/cch/mod/" => "rw" },
		  model_reg   => {"/elysium/metfark/reg/mod/" => "rw" },
		  model_old   => {"/elysium/metfark/old/mod/" => "rw" },
		  coloc =>     {"/elysium/metfark/coloc/"   => "rw" },
		  plot =>      {"/elysium/metfark/plot/"    => "rw" },
		  plot_old =>  {"/elysium/metfark/old/plot/"    => "rw" },
		  plot_use =>  {"/elysium/metfark/use/plot/"    => "rw" },
		  auto  => {"/elysium/metfark/auto/"    => "rw" },
		  url  =>  {"/elysium/metfark/url/"     => "rw" },
		  lock =>  {"/elysium/metfark/lock/"    => "rw" }
    );

sub makeRoot {
    my $cls = shift;
    my @dirs=keys %{$farkdirs{$cls}};
    if (@dirs) {
	if (! -d $dirs[0] && $farkdirs{$cls}{$dirs[0]} eq "rw") { 
	    my $ret=makePath($dirs[0]);
	};
    }
}

sub getDir {
    my $cls = shift;
    my @dirs=keys %{$farkdirs{$cls}};
    if (@dirs) {
	if (! -d $dirs[0]) { makePath($dirs[0]) || die "Unable to makePath ".$dirs[0];};
	return $dirs[0];
    } else {
	die "Undefined dir-class $cls";
    };
}

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
	} else {
	    $tdir = $ddir . $pdir; # relative path
	};
    }
    #
    #print "Dir: '$ipath' => '$tdir'\n";
    # get abs path
    if (-d $tdir) { # total path
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
	$loc=$tdir;
	$priv="missing";
    }
    #print "splitDir '$ipath' '$cls' -> '$root' '$loc' '$priv'\n";
    return ( $root, $loc, $priv );
}

#
# Check that directory exists, create if necessary...
#   my $ret = check_dir($path);
#

sub makePath{
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
	if (! @nodes) {@nodes=$doc->findnodes("obs/obs_config");};
	if (! @nodes) {@nodes=$doc->findnodes("model/model_config");};
	if (! @nodes) {@nodes=$doc->findnodes("plot/plot_config");};
	if (! @nodes) {@nodes=$doc->findnodes("coloc/coloc_config");};
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
    return(0);
}

#
# Remove password-protected files in directory, and directory tree if empty
#    my $ret = removeDir($root,$password);
#

sub removeDir {
    my $root = shift;
    my $password = shift;
    my ($sdir) = &cwd;  # current working directory
    my $ret=0;
    opendir(DIR, $root) or &term("Unable to open $root: $!");
    my $parser = XML::LibXML->new();
    my @dirs = ();
    #print "Here...\n";
    while ( my $name = readdir(DIR) ){
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
	    if (! @nodes) {@nodes=$doc->findnodes("obs/obs_config");};
	    if (! @nodes) {@nodes=$doc->findnodes("model/model_config");};
	    if (! @nodes) {@nodes=$doc->findnodes("plot/plot_config");};
	    if (! @nodes) {@nodes=$doc->findnodes("coloc/coloc_config");};
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
    chdir($sdir) or &term("Unable to change to dir $sdir: $!");
    if ($ret) {
	&term("Keeping $sdir");
    } else {
	#print "Removing $sdir\n";
	rmdir($sdir) or &term("Unable to rmdir $sdir: $!");
    };
    return($ret);
}

#
# my @files = &find (".*\.nc",$dir,"",time);
#

sub find{
    use Cwd;
    my $pattern = shift;
    my ($wdir) = shift;
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
    while ( my $name = readdir(DIR) ){
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
		push (@ret, $path);
		$hits++;
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
	my @lret=&find($pattern,$name,$root,$t,$hits);
	push (@ret,@lret);
	$hits+=@lret;
    }
    chdir($sdir) or die "Unable to change to dir $sdir:$!\n";
    return @ret;
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

sub termAll {
    my $msg=shift;
    $msg=~s/[^a-zA-Z0-9 _\-\+\.\,\/\:\[\]\(\)]/ /g;
    $msg=~s/\n/ /g;
    $msg=~s/ +/ /g;
    print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
    print "<error message='".$msg."'/>\n";
}

1;
