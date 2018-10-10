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
use File::Touch;

no warnings 'once';


# capture term-exit signals in sanbox block
our $override_exit = 0;
BEGIN { 
    *CORE::GLOBAL::exit = sub (;$) {
        no warnings 'exiting';
        last EXIT_OVERRIDE if $override_exit;
        CORE::exit($_[0] // 0);
    };
 }

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
		  shape => {"/metfark/config/shape/" => "ro"},
		  tables => {"/lustre/storeB/users/"     => "ro",        # BUFR tables
			     "/lustre/storeB/project/"   => "ro",
			     "/opdata/"                  => "ro",
			     "/metfark/bufrtables/"      => "ro" }, 
		  output => {"/lustre/storeA/project/nwp/fark/"     => "rw",
			     "/metfark/output/"          => "rw" }, 
		  script      => {"/metfark/config/splus/" => "rw" }, # splus scripts
		  model       => {"/metfark/config/mod/"     => "rw" }, # model config files
		  model_old   => {"/metfark/config/old/mod/" => "rw" }, # model old config files
		  model_use   => {"/metfark/config/use/mod/" => "rw" }, # model use files
		  model_fill  => {"/metfark/config/fill/mod/" => "rw" }, # model use files
		  model_cache => {"/metfark/config/index/mod/" => "rw" }, # model cache/index files
		  model_reg   => {"/metfark/config/reg/mod/" => "rw" }, # model register files
		  model_log   => {"/metfark/config/log/mod/" => "rw" }, # model log files
		  obs       => {"/metfark/config/obs/"       => "rw" },
		  obs_use   => {"/metfark/config/use/obs/"   => "rw" },
		  obs_fill  => {"/metfark/config/fill/obs/"   => "rw" },
		  obs_cache => {"/metfark/config/index/obs/"   => "rw" },
		  obs_reg   => {"/metfark/config/reg/obs/"   => "rw" },
		  obs_old   => {"/metfark/config/old/obs/"   => "rw" },
		  obs_log   => {"/metfark/config/log/obs/"   => "rw" },
		  coloc       => {"/metfark/config/coloc/"   => "rw" },
		  coloc_use   => {"/metfark/config/use/coloc/"   => "rw" },
		  coloc_fill  => {"/metfark/config/fill/coloc/"   => "rw" },
		  coloc_reg   => {"/metfark/config/reg/coloc/"   => "rw" },
		  coloc_old   => {"/metfark/config/old/coloc/"   => "rw" },
		  coloc_log   => {"//metfark/config/log/coloc/"   => "rw" },
		  plot =>      {"/metfark/config/plot/"    => "rw" },
		  plot_old =>  {"/metfark/config/old/plot/"    => "rw" },
		  plot_use =>  {"/metfark/config/use/plot/"    => "rw" },
		  plot_fill => {"/metfark/config/fill/plot/"    => "rw" },
		  plot_log =>  {"/metfark/config/log/plot/"    => "rw" },
		  auto  => {"/metfark/config/auto/"    => "rw" },       # auto config files
		  url  =>  {"/metfark/config/url/"     => "rw" },       # url config files (not used?)
		  abort=>  {"/metfark/config/abort/"     => "rw" },       # url config files (not used?)
		  lock =>  {"/metfark/config/lock/"    => "rw" }        # lock files (must be local disk)
    );

our $debug=0;

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
	#print "splitDir abs: $tdir -> $adir\n";
	if (defined $adir) {
	    $priv = "denied";
	    foreach my $k (keys %{$farkdirs{$cls}}) {
		my $kk = substr($k,0,-1); # remove last "/" from "%farkdirs" directory name
		#print "Checking $adir~$kk $priv\n";
		if ( $adir =~ /^$kk\/?(.*)$/) {
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

sub splitPattern {
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
    if (defined $tdir) { # total path
	my $adir = $tdir; # total path
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
	    if (makePath($dirs[0])) {
		chmod 0777, $dirs[0];
		return $dirs[0];
	    } else {
		return;
	    }
	};
    }
    return 0; # fail
}

sub getRootDir {
    my $cls = shift;
    #print "getRootDir entering '$cls'\n"
    my @dirs=keys %{$farkdirs{$cls}};
    if (@dirs) {
	#print "getRootDir Dir '".$dirs[0]."'\n"
	if (! -d $dirs[0]) { 
	    #print "getRootDir Makepath '".$dirs[0]."'\n"
	    if (makePath($dirs[0])) {
		#print "getRootDir Chmod '".$dirs[0]."'\n"
		chmod 0777, $dirs[0];
		return $dirs[0]; # success
	    } else {
		return; # failure
	    }
	} else {
	    return $dirs[0];
	}
    } else {
	#print "getRootDir No dirs.\n"
	$_="Undefined dir-class $cls";
	return; # failure
    };
}

#
# Check that directory exists, create if necessary...
#   my $ret = check_dir($path);
#

sub makePath{
    use Capture::Tiny 'capture_merged';
    my $path=shift;
#    print "Path:$path\n";
    my ($merged,$irc)=capture_merged {
	eval {
	    if(!-d $path) {
		make_path $path; 
		chmod 0777, $path;
	    };1;
	};
	return ($@); # send error return code to $irc
    };
    if ($irc) {
	return(0); # failure
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
# check if string is present in any of the files with the root class directory (besides $file)...
#
sub checkClassForStrings {
    my ($cls, $file, @strings) = @_;
    my @dirs=keys %{$farkdirs{$cls}};
    my $root= (keys (%{$farkdirs{$cls}}))[0]; # get default path
    my @files=();
    find({wanted => sub {if (-f $File::Find::name) {push(@files, $File::Find::name);}}}, $root);
    #print "Looking for ".join(" ",@strings)." in $root\n";
    foreach my $f (@files) {
	#print "Found file '$f' <> '$root$file'...";
	if ("$root$file" eq "$f" ) {
	    #print "... skipping\n";
	}else {
	    #print "... searching\n";
	    open(my $fh, '<:encoding(UTF-8)', $f)
		or break;
	    while (my $row = <$fh>) {
		chomp $row;
		foreach my $s (@strings) {
		    if ($row =~ m/\Q$s/g) {
			my $fn=$f;
			if ($fn=~m/$root(.*)$/) {
			    $fn=$1;
			}
			#print "Found '$s' in '$fn'\n";
			return $fn; # we found a match
		    }
		}
	    }
	}
    }
    return; # no match
}
#
# farkdir::sandbox {
#     system "ls -l";
# } {
#   message=>"See 'log' for more detail,",
#   logfile   => "log",
#   stdout    => "success",# "always", must have logfile to use "success"...
#   fork      => 1,
#   debug     => 0,
#   terminate => \&farkdir::term};

sub docsave {
    my $path=shift;
    my $doc=shift;
    if (open(PATH, '>', $path) && flock (PATH,2+4)) {
	print PATH $doc->toString;
	close PATH;
	chmod 0666, $path;
    } else {
	farkdir::term("Unable to open: '".$path."'");
    }
}

sub sandbox (&@) {
    use 5.014;
    use File::Copy;
    my $code = shift; # reference to block to be executed
    my $opts = shift;
    my $msg    = $opts->{"message"}//"";      # termination-message in case of error
    my $log    = $opts->{"logfile"}//"";      # logfile
    my $print  = $opts->{"stdout"}//"always"; # print stdout: "always", "never", "success"
    my $term   = $opts->{"terminate"};        # terminate on error using function...
    my $fork   = $opts->{"fork"}//"0";        # fork process?
    my $abort= $opts->{"abort"}//"0";  # check abortfile
    my $abortfile= $opts->{"abortfile"}//"0";  # check abortfile
    my $debug   = $opts->{"debug"}//"0";      # print debug info
    my $append   = $opts->{"append"}//"0";      # print debug info
    if ($abort) { # this is an abort-request
	farkdir::touchFile($abortfile);
    } else { # start processes
	if ($msg && ! $term) {$term=\&term;}      # if we have message, use default term-function
	my ($pid,$core,$sig,$ext,$wait,$blk,$mrg)=(0,0,0,0,0,0,0);
	my $cmd="";
	my $merged="";
	if (!$log && $print eq "success") {
	    die ("farkdir::sandbox A logfile is required when using option stdout=>'success'.");
	}
	if (defined $log && $log) {
	    &redirect_streams($log,$append);
	} elsif ( $print eq "never") {
	    &redirect_streams("/dev/null");
	};
	($pid,$core,$sig,$ext,$wait,$blk,$cmd)=sandcorn($code,$fork,$abortfile,$debug);
	if ($debug) {
	    my $s="";
	    if ($core) {$s .=" core:$core";}
	    if ($sig)  {$s .=" signal:$sig";}
	    if ($ext)  {$s .=" exit:$ext";}
	    if ($wait) {$s .=" wait:$wait";}
	    if ($blk)  {$s .=" block: $blk";}
	    if ($cmd)  {$s .=" cmd:$cmd";}
	    #if ($mrg)  {$s .=" merge:$mrg";}
	    print $s . "\n";
	};
	my $ok=(! $core       # child core dump 
		&& ! $sig     # child abort signal
		&& ! $ext     # child exit/die
		&& ! $wait    # child pid vanished
		&& ! $cmd     # eval return string
		&& ! $blk );
	if (defined $log && $log) {
	    &restore_streams();
	    $merged=&message_file($log);
	    if ($print eq "always"  || ($print eq "success" && $ok)) {
		&print_file($log);
		if ($cmd) {print $cmd;}
	    }
	    if (! $ok) { # save to errfile
		copy($log, $log.".err");
	    };
	} elsif ( $print eq "never") {
	    &restore_streams();
	};
	# handle errors...
	if (! $ok) {
	    my $message=$msg;
	    if ($merged) {
		$message=$merged;
	    } elsif ($core){
		$message = $msg . " (Process $pid dumped core.)";
	    }elsif($sig){
		$message=$msg . " (Process $pid died suddenly.)";
	    }elsif ($ext) {
		$message=$msg . " (Process $pid returned $ext.)";
	    }elsif ($wait) {
		$message=$msg . " (Process $pid just vanished. How strange.)";
	    } elsif ($cmd) {
		$message=$cmd;
	    } elsif ($blk) {
		$message=$msg . " [$cmd]";
	    };
	    if ($debug) {print "Message: $message\n";};
	    if ($term) {
		$term->($message);
	    } elsif ($debug) {
		print $message;
	    };
	}
    }
}

sub sandcorn {
    #use POSIX;
    use POSIX ":sys_wait_h";
    my ($code,$fork,$abortfile,$debug)=@_;
    my $pid=0;      # child pid
    my $core=0;     # child core dump 
    my $sig=0;      # child abort signal
    my $ext=0;      # child exit/die
    my $wait=0;     # child pid vanished
    my $cmd="";     # eval return string
    my $blk = 0;    # block exit
    #
    my $tstart=time();
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();
    #
    if($debug){printf ("farkdir::sandbox started '$fork' at %02d:%02d:%02d\n",$hour,$min,$sec);}
    if ($fork) {
	my $ret=0;
	my $ps="";
	if (-f $abortfile) {
	    unlink($abortfile);
	};
	if($debug){print "farkdir::sandbox Forking process.\n";}
	eval {
	    $pid = fork();
	    if (defined $pid) {
		if ($pid){      # parent process
		    my $kid;
		    sleep(0.25);
		    do {
			my $lps=`ps --pid $pid -o pid,ppid,pmem,pcpu,cputime,args`;
			if ($lps =~ m/^.*\n.*\n/g) {$ps=$lps;};
			$kid= waitpid($pid, WNOHANG);
			$ret= $?;
			if ($kid==0) {sleep(1);}
			if (-f $abortfile) {
			    print "farkdir::sandbox User killed child process $pid\n";
			    unlink($abortfile);
			    kill 9,$pid;
			};
		    } while ($kid == 0); # $kid==0:running, $kid==$pid:exiting,$kid==-1:no such process
		    if ($ret == -1) {
			($ext, $sig, $core) = (-1,0,0);
		    }else {
			($ext, $sig, $core) = ($ret >> 8, $ret & 127, $ret & 128);
		    }
		} else { # child process
		    $|=1; # flush stdout buffer, otherwise it will not be captured by parent...
		    if($debug){print "farkdir::sandbox child start=$$.\n";}
		    eval {&{$code};1;};$cmd=$@;   # capture any "die" messages from the system...
		    if($debug){print "farkdir::sandbox child end=$$.\n";}
		    if ($cmd) {
			print $cmd;
			exit 1;
		    } else {
			exit 0;
		    }
		}
	    } else {
		$cmd="Unable to fork.";
	    };1;
	};
	$cmd=$@;   # capture any "die" messages from the system...
	if($debug && $ps){print "farkdir::sandbox Last child info:\n$ps";}
	if($debug){print "farkdir::sandbox Completed fork (pid=$$) code='$ret' (exit='$ext') (signal='$sig') (core='$core').\n";}
    } else { # no fork, must catch exit signals
	$blk = 1;
      EXIT_OVERRIDE: {
	  $|=1; # flush stdout buffer, otherwise it will not be captured by parent...
	  local $override_exit = 1;
	  eval {&{$code};1;};$cmd=$@;   # capture any "die" messages from the system...
	  $blk = 0;
	};
    }
    #
    my $tstop=time();
    my $dtime=$tstop-$tstart;
    ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();
    if($debug){printf("farkdir::sandbox completed at %02d:%02d:%02d (%s)\n",
		      $hour,$min,$sec,dtg($dtime));}
    return ($pid,$core,$sig,$ext,$wait,$blk,$cmd);
}

#

sub dtg {
    use POSIX qw (floor);
    my $tsec=shift;
    my $s="";
    if ($tsec > 86400) {
	my $dd=floor($tsec/86400);
	$tsec=$tsec-$dd*86400;
	$s=$s . $dd . "d";
    }
    if ($tsec > 3600) {
	my $hh=floor($tsec/3600);
	$tsec=$tsec-$hh*3600;
	$s=$s . $hh . "h";
    }
    if ($tsec > 60) {
	my $mm=floor($tsec/60);
	$tsec=$tsec-$mm*60;
	$s=$s . $mm . "m";
    }
    $s=$s . $tsec . "s";
    return $s;
}

#
# my $ret = &term ("System error...");
#
sub term {
    use URI::Encode qw(uri_encode);
    my $msg=uri_encode(shift);
    print '<error message="'.$msg.'"/>'."\n";
    exit 2;
}

sub info {
    my $msg=shift;
    $msg=~s/[^a-zA-Z0-9 _\-\+\.\,\/\:\[\]\(\)]/ /g;
    $msg=~s/ +/ /g;
    print "<info message='".$msg."'/>\n";
    exit 3;
}

sub termAll {
    my $msg=shift;
    $msg=~s/[^a-zA-Z0-9 _\-\+\.\,\/\:\[\]\(\)]/ /g;
    $msg=~s/\n/ /g;
    $msg=~s/ +/ /g;
    print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
    print "<error message='".$msg."'/>\n";
}

#
# my @files = &find (".*\.nc",$dir,"",time);
#

sub FindFiles{
    use File::stat;
    use Cwd;
    my $wdir = shift;
    my $pattern = shift;
    my $min = shift || "";
    my $max = shift || "";
    my $maxtime = shift || 0;
    my $root = shift || "";
    my $t = shift || time;
    my $hits = shift || 0;
    my @ret = ();
    if ($maxtime && time - $t > $maxtime) {return @ret;}; # use maximum 2 seconds
    #print "Time:" . (time-$t) . "\n";
    if ($root ne "") {
	$root=$root . $wdir;
    } else {
	$root=$wdir;
    }
    if (substr($root,-1) ne "/") { $root = $root . "/";};
    my ($sdir) = &cwd; 
    if ($debug) {print "Opening $wdir\n";}
    opendir(DIR, $wdir) or die "Unable to open $wdir:$!\n";
    my @dirs = ();
    #print "Here...\n";
    my @entries = sort { $a cmp $b } readdir(DIR);
    while ( my $name = shift @entries ){
	#print "Checking $name\n";
	if ($maxtime && time - $t > $maxtime && $hits > 1) {last;}; # use max 2 seconds if we have hits
	if ($maxtime && time - $t > $maxtime*3) {last;}; # use max 10 seconds
        next if ($name eq ".");
        next if ($name eq "..");
        next if (-l $name ); # skip symlinks
	my $path=$root . $name;
        if (-d $path){
	    my $ok=1;
	    if ($min || $max) {
		my $ref=stat($path);
		my $fmin=($ref->ctime);
		my $fmax=($ref->mtime);
		# get times from file name...
		my %patt=findPattern($path);
		foreach my $k (keys %patt) {
		    $fmin=$patt{$k}[1];
		    $fmax=$patt{$k}[2];
		    if ($debug){print "Resetting: '$path' '$k' '$fmin' '$fmax'\n";};
		};
		my $mge=($t-$fmax)/86400;
		my $cge=($t-$fmin)/86400; # inode change time...
#		if ($min && $cge < $min) {$ok=0;}; # if directory has moved , this will fail
		if ($max && $mge > $max) {$ok=0;};
		if ($debug){print "Directory: min=$cge, max=$mge  '$path' '$min' '$max' $t $fmin $fmax $ok\n";};
	    };
	    if ($ok) {
		push (@dirs, $name);
	    }
        } elsif (-f $path ) {
	    if ($name  =~ m/$pattern/) {
		my $ok=1;
		if ($min || $max) {
		    my $age=(-M $path);
		    #print "Age $age '$path' '$min' '$max'\n";
		    if ($min && $age < $min) {$ok=0;};
		    if ($max && $age > $max) {$ok=0;};
		    if ($debug) {print "File: '$path' '$age' '$ok'\n";};
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
	my @lret=&FindFiles($name,$pattern,$min,$max,$maxtime,$root,$t,$hits);
	push (@ret,@lret);
	$hits+=@lret;
    }
    chdir($sdir) or die "Unable to change to dir $sdir:$!\n";
    return sort @ret;
}

sub GetFiles {   # full scan of all files...
    use File::Find;
    my($filterDir,$filter,$min,$max) = @_;
    if (! defined($min)) {$min="";};
    if (! defined($max)) {$max="";};
    my @files=();
    find({wanted => sub {
	if (-f $File::Find::name && $File::Find::name =~ m/$filter/) {
	    my $file=$File::Find::name;
	    my $ok=1;
	    if ($min || $max) {
		my $age=(-M $file);
		if ($min && $age < $min) {
		    $ok=0;
		};
		if ($max && $age > $max) {
		    $ok=0;
		};
	    }
	    if ($ok) {
		my $sb=(stat($file))[9];
		my $s= $sb. " " . $file;
		push(@files, $s);
	    }
	}
	  }}, $filterDir);
    return @files;
}

# find YYYY MM DD pattern in file/directory names
# usage: my %patterns = farkdir::findPattern(@files);
# return: %patterns{"fileYYYYMMDD"}=["file\d\d\d\d\d\d\d\d",tmin,tmax]
#

sub findPattern {
    use strict;
    use URI::Encode;
    my @files= @_;
    my %patterns;
    # divide into groups with same character settings
    my %ptree;
    my $uri = URI::Encode->new( { encode_reserved => 0 } );
    foreach my $name (@files) {
	my $p = $name;
	$p =~ s/\d/\\d/g;
	if (! defined $ptree{$p}) {$ptree{$p}=();};
	push (@{$ptree{$p}},$name);
    }
    foreach my $p (keys %ptree) {
	my %lim=();
	my %pptree;
	my $len =@{$ptree{$p}};
	#print "Found '$p'\n";
	my $m=$p;
	$m =~ s/\\d/@/g;    # replace "digit" with "@"
	my $lenm=length($m);
	# loop over number-groups...
	GROUP: while ($m =~ m/(@+)/g) { # identify before-group-after
	    my $p1=substr($m,0,$-[0]);
	    my $p2=$1; # group
	    my $p3=substr($m,$+[0],$lenm-$+[0]);
	    my $offset=$-[0];
	    #print "Match '$p1' '$p2' '$p3'\n"; next GROUP;
	    $p1 =~ s/@/\\d/g;
	    $p2 =~ s/@/\\d/g;
	    $p3 =~ s/@/\\d/g;
	    my $pp="$p1($p2)$p3";
	    #print "   found sub-pattern: '$pp' '$p'\n";
	    if (! defined $pptree{$p})      {$pptree{$p}={};};
	    if (! defined $pptree{$p}{$pp}) {$pptree{$p}{$pp}=();};
	    # loop over all instances
	    foreach my $name (@{$ptree{$p}}) {
		#print "Found: '$name'\n";
		if ($name =~ m/$pp/) { # instances with number group
		    my $cc=$1; # number group
		    #print "Found item: '$pp' '$cc'\n";
		    # loop over characters in number group...
		    my $pos=0;
		    # check 4-digits
		    &findLimits($cc,4,$offset,\%lim);
		    &findLimits($cc,3,$offset,\%lim);
		    &findLimits($cc,2,$offset,\%lim);
		    &findLimits($cc,1,$offset,\%lim);
		}
	    }	
	}
	# best estimates
	my $byyyy;
	my $bmm;
	my $bdd;
	my $bhh;
	my $bmi;
	# find 4-digit year
	my $cyyyy = &getCandidates($lim{4},0,$lenm-3,1900,2100,"YYYY"); # year candidates
	my $cmm =   &getCandidates($lim{2},0,$lenm-1,1,12,"MM");        # month  candidates
	my $cdd =   &getCandidates($lim{2},0,$lenm-1,1,31,"DD");        # day candidates
	my $chh =   &getCandidates($lim{2},0,$lenm-1,0,24,"HH");        # hour candidates
	my $cmi =   &getCandidates($lim{2},0,$lenm-1,0,60,"MI");        # minutes candidates
	my $ccc =   &getCandidates($lim{2},0,$lenm-1,0,24,"CC");        # cycle candidates
	my $cll =   &getCandidates($lim{3},0,$lenm-1,0,300,"LL");       # lead time candidates
	my $cnn =   &getCandidates($lim{1},0,$lenm-1,0,9,"N");       # lead time candidates
	my $s=$m;
	$s  =~ s/@/ /g; # replace "digit" with " ", "string    "
	my $w=$s;
        $w  =~ s/\S/@/g; # only show number as "@", "      @@@@"
	#print "Making patterns for '$p2'\n";
	my ($patt,$order) = &showPattern($s,$w,$cnn,$cyyyy,$cmm,$cdd,$chh,$cmi,$ccc,$cll);
	$m =~ s/@/\\d/g;
	if ($m ne $patt) {
	    my $tmin=0;
	    my $tmax=0;
	    # loop over files, find max/min times
	    foreach my $name (@{$ptree{$p}}) {
		#print "Found: '$name'\n";
		my ($fmin,$fmax)=getPatternTime($name,$order);
		if (! $tmin || $tmin > $fmin) {
		    $tmin=$fmin;
		}
		if (! $tmax || $tmax > $fmax) {
		    $tmax=$fmax;
		}
	    };
	    $patterns{$uri->encode($patt)} = [$uri->encode($m),$tmin,$tmax];
	};
    }
    return %patterns;
}
#
# usage findlimits ( $string, $group_length, $global_start, $limit_output);
# returns %{$limit_output}{$group_length}($start_pos)(min,max,mod);
sub findLimits {
    use strict;
    my ($cc,$dlen,$offset,$lim)=@_;
    if (! defined $lim->{$dlen}){ $lim->{$dlen}=();};
    my $clen=length($cc);
    for (my $ii=0;$ii < $clen-$dlen+1;$ii++) {
	my $ss=substr $cc,$ii,$dlen;
	if (! defined $lim->{$dlen}[$ii+$offset]){
	    $lim->{$dlen}[$ii+$offset]=[$ss+0,$ss+0,$dlen,$ss+0];
	} else {
	    $lim->{$dlen}[$ii+$offset][0]=min($lim->{$dlen}[$ii+$offset][0],$ss+0);
	    $lim->{$dlen}[$ii+$offset][1]=max($lim->{$dlen}[$ii+$offset][1],$ss+0);
	    $lim->{$dlen}[$ii+$offset][3]=mod($lim->{$dlen}[$ii+$offset][3],$ss+0);
	};
    }
}    
#
# returns reference to array with possible candidates:
#  ( pos_start, pos_stop, label, score, steplen, min_value, max_value )
#
#
sub getCandidates {
    use strict;
    my ($lim,$istart,$istop,$rmin,$rmax,$label)= @_;
    #print "Looking for candidates $rmin, $rmax\n";
    my @ret=();
    foreach my $ii (keys @{$lim}) {
	my $min=$lim->[$ii][0];
	my $max=$lim->[$ii][1];
	my $dlen=$lim->[$ii][2];
	my $delta=$lim->[$ii][3];
	#if (defined $min) { print " Range $ii, $min, $max, $dlen\n";};
	if (defined $min && $min >= $rmin && $max <= $rmax) {
	    my $ratio=0;
	    if ($rmax>$rmin) {
		$ratio=($max-$min)/($rmax-$rmin);
	    };
	    #print "   found candidate at $ii\n";
	    my $score=(2 - 6*$ratio + 8*$ratio*$ratio)*$dlen;
	    my $step=(($delta-1)/max(1,$rmax))*$dlen*8;
	    #print "   found candidate $score, $step, $delta, $rmax\n";
	    my @res = ($ii,$ii+$dlen-1,$score,$step,$min,$max);
	    push (@ret,\@res);
	}
    }
    return [$label,\@ret];
}

sub showPattern {
    use strict;
    use POSIX qw (floor);
    my ($s,$w,$cnn,@candidateTypes)= @_;
    if ($debug) {print "showPattern Entering with '$s' '$w'\n";};
    my $bdone=0;
    my %order; # order of the different labels
    while (!$bdone) {   # loop while changed
	# find candidate with highest score...
	my $maxss=0;
	my $maxns;
	my $maxnb;
	my $maxistart;
	my $maxistop;
	my $maxlabel;
	#print "Bdone loop '$s' '$w'\n";
	foreach my $candidates (@candidateTypes) {  # loop over types
	    my $label=$candidates->[0];
	    #print "  Type loop '$s' '$w'\n";
	    foreach my $candidate (@{$candidates->[1]}) { # loop over candidates
		my $istart=$candidate->[0];
		if (defined $istart) {
		    my $istop=$candidate->[1];
		    my $score=$candidate->[2];
		    my $delta=$candidate->[3];
		    my $min=$candidate->[4];
		    my $max=$candidate->[5];
		    #print "    Candidate loop '$s' '$w' '$label' $score $delta\n";
		    if (my ($ns,$nb)=&addPattern($s,$w,$istart,$istop,$label)) { # check if pattern is possible
			my $ss=&getScore($ns,$nb,$score);
			#if ($label eq "CC") {
			#    $ss=$ss+$delta;
			#};
			if ($ss > $maxss) { # store candidate with highest score
			    $maxns=$ns;
			    $maxnb=$nb;
			    $maxss=$ss;
			    $maxistart=$istart;
			    $maxistop=$istop;
			    $maxlabel=$label;
			};
		    } else {
			$candidate=[];
		    };
		};
	    };
	}   # end candidate loop
	# use candidate with highest score (if any)...
	if ($maxss) {
	    $s=$maxns;
	    $w=$maxnb;
	    $maxss=floor($maxss*100)/100;
	    $order{$maxlabel}=[$maxistart,$maxistop-$maxistart+1];
	    if ($debug) {print "######## '$s' $maxss ### WINNER ###\n";}
	} else {
	    $bdone=1;
	}
    }; # end change loop
    # replace any remaining characters with their original substring...
    my $label=$cnn->[0];
    foreach my $candidate (@{$cnn->[1]}) { # loop over candidates
	my $istart=$candidate->[0];
	if (defined $istart) {
	    my $istop=$candidate->[1];
	    my $min=$candidate->[4];
	    my $max=$candidate->[5];
	    if ($min == $max) {$label="$min"};
	    #print "    Candidate loop '$s' '$w' '$label' $score $delta\n";
	    if (my ($ns,$nb)=&addPattern($s,$w,$istart,$istop,$label)) { # check if pattern is possible
		$s=$ns;
		$w=$nb;
	    };
	};
    };
    #print "showPattern Exiting\n";
    return ($s, \%order);
}
#
# Get max/min time based on name...
# usage: my ($tmin,$tmax)=getPatternTime($name,$order);
#
sub getPatternTime {
    use strict;
    use Time::Local;
    my ($name,$order) =@_;
    my $yy=&getPatternNumber("YYYY",$name,$order);
    my $mm=&getPatternNumber("MM",$name,$order);
    my $dd=&getPatternNumber("DD",$name,$order);
    my $hh=&getPatternNumber("HH",$name,$order);
    my $yymax;
    my $yymin;
    if (defined $yy) {
	$yymax=$yy;
	$yymin=$yy;
    } else {
	$yymin=1970;
	$yymax=2036;
    };
    my $mmmax;
    my $mmmin;
    if (defined $mm) {
	$mmmax=$mm;
	$mmmin=$mm;
    } else {
	$mmmin=1;
	$mmmax=12;
    };
    my $ddmax;
    my $ddmin;
    if (defined $dd) {
	$ddmax=$dd;
	$ddmin=$dd;
    } else {
	$ddmin=1;
	$ddmax=31 - (($mmmax == 2) ?
		     (3 - &IsLeapYear($yymax)) : (($mmmax - 1) % 7 % 2));
    };
    my $hhmax;
    my $hhmin;
    if (defined $hh) {
	$hhmax=$hh;
	$hhmin=$hh;
    } else {
	$hhmin=0;
	$hhmax=23;
    };
    my $tmin=timelocal(0,0,$hhmin,$ddmin,$mmmin-1,$yymin);
    my $tmax=timelocal(59,59,$hhmax,$ddmax,$mmmax-1,$yymax);
    return ($tmin,$tmax);
}
sub IsLeapYear
{
   my $year = shift;
   return 0 if $year % 4;
   return 1 if $year % 100;
   return 0 if $year % 400;
   return 1;
}
#
# get number at location used for $label...
#
sub getPatternNumber {
    my ($label,$name,$order) = @_;
    my $ret;
    if (defined $order->{$label}) {
	$ret=(substr($name,$order->{$label}[0],$order->{$label}[1]) +0);;
    }
    return $ret;
}

#
# replace string with label
#
sub addPattern {
    use strict;
    my ($s,$w,$istart,$istop,$label) = @_;
    #print "addPattern $s $istart $istop\n";
    my $ss=substr($s,$istart,$istop-$istart+1)//"";
    if (&blank($ss)) { # blank
	my $len=length($s);
	my $s1=substr($s,0,max(0,$istart))//"";
	my $s2=substr($s,$istop+1,$len-$istop)//"";
	my $w1=substr($w,0,max(0,$istart))//"";
	my $w2=substr($w,$istop+1,$len-$istop)//"";
	my $patt = $s1 . $label . $s2;
	my $watt = $w1 . $label . $w2;
	#print "****'$s'->'$s1'+'$label'+'$s2' $istart $istop $len ".length($patt)."\n";
	#print "addPattern '$s' -> '$patt'\n";
	return ($patt,$watt);
    } else {
	return;
    }
}

sub getScore {
    use POSIX qw (floor);
    my ($s,$w,$score)=@_;
    # adjacent
    if ($debug) {my $s2=floor($score*100)/100;
		 print "getScore '$s' $s2";}
    if ($w =~ m/^@*HH@*$/) {$score+=5;};
    while ($w =~ m/YYYY@*MM/g) {$score+=3;};
    while ($w =~ m/YYYY@*MM@*DD/g) {$score+=3;};
    while ($w =~ m/MM@*DD/g) {$score+=3;};
    while ($w =~ m/DD@*HH/g) {$score+=2;};
    while ($w =~ m/HH@*MI/g) {$score+=2;};
    #
    if ($debug) {my $s2=floor($score*100)/100;
		 print " -> $s2\n";}
    return $score;
}

sub blank {
    use strict;
    my $s = shift;
    if (defined $s) {
	if ($s =~ m/\S/) {
	    return 0;
	} else {
	    return 1;
	}
    } else {
	return 1;
    }
}
sub min {
    use strict;
    my ($min, @vars) = @_;
    for (@vars) {
	if (defined $min) {
	    $min = $_ if $_ < $min;
	} else {
	    $min = $_;
	}
    }
    return $min;
}
sub max {
    use strict;
    my ($max, @vars) = @_;
    for (@vars) {
        $max = $_ if $_ > $max;
    }
    return $max;
}
#
# Largest common space between values...  (i.e.   1000, 1010, 1020... -> 10)
#
sub mod {
    use strict;
    my ($mod, @vars) = @_;
    for (@vars) {
	my $nd=$mod;
	if ($mod > 0) {
	    while ($nd > 0 && $_%$nd!=0 && $mod%$nd !=0) { $nd--; }
	    $mod=$nd;
	} else {
	    $mod=$_+0;
	}
    }
    return $mod;
}

sub restore_streams
{
    use strict;
    close(STDOUT) || die "Can't close STDOUT: $!";
    close(STDERR) || die "Can't close STDERR: $!";
    open(STDERR, ">&OLDERR") || die "Can't restore stderr: $!";
    open(STDOUT, ">&OLDOUT") || die "Can't restore stdout: $!";
    if ($debug) {print "Streams restored.\n";};    
};

sub redirect_streams
{
    use strict;
    my ($log_file, $append) = @_;
    if ($debug) {print "Redirecting streams to '$log_file'\n";};    
    open OLDOUT,">&STDOUT" || die "Can't duplicate STDOUT: $!";
    open OLDERR,">&STDERR" || die "Can't duplicate STDERR: $!";
    if (defined $append && &append) {
	open(STDOUT,">> $log_file");
    } else {
	open(STDOUT,"> $log_file");
    }
    open(STDERR,">&STDOUT");
}

sub print_file
{
    my ($filename)=@_;
    if ($debug) {print "*** Start of '$filename' ***\n";};    
    open(my $fh, '<:encoding(UTF-8)', $filename)
	or die "Could not open file '$filename' $!";
    
    while (my $row = <$fh>) {
	chomp $row;
	print "$row\n";
    };
    if ($debug) {print "*** End of '$filename' ***\n";};    
}

sub message_file
{
    my ($filename)=@_;
    open(my $fh, '<:encoding(UTF-8)', $filename)
	or die "Could not open file '$filename' $!";
    
    while (my $row = <$fh>) {
	if ($row =~ m/<error message='(.*)'\/>/g) {
	    return $1;
	};
    };
}

1;
