package fark;

=head1 NAME

  fark.pm - a perl library for colocating model and observation data from BUFR, GRIB and NetCDF files. 

=head1 SYNOPSIS

   use fark;

=head1 DESCRIPTION

The fark perl library is a tool for colocating data from model files and 
observation files. The library accepts a list of model files and observation
files. Model data can be sorted using a index-variable specified by the user, and
the values of this variable is available when looping through the model files.
The observation files within a specified index period can be accessed in an
inner loop. For each observation file, the relevant observations can be retrieved.
Finally, the model fields can be sliced according to variable-match-rules.

By reducing the amount of data returned to the perl script, the user can dramatically
speed up the data processing. Processing data in perl typically takes 100-fold
more time than processing the data in Fortran/C-routines.

=head1 SESSION FUNCTIONS

=cut

use 5.014002;
use strict;
use warnings;

use Getopt::Long;
use Pod::Usage qw(pod2usage);
use File::Find;
use File::Basename;

use farkdir;

require Exporter;

our @ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use fark ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(
	
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '0.13';
require XSLoader;
XSLoader::load('fark', $VERSION);
use constant DEFAULT_TABLE_PATH => '/usr/local/lib/bufrtables';

# Preloaded methods go here.

=head2 remote

Run remote script.

=head4 EXAMPLE

fark::remote("dir.pl","?ls='/usr/var'","www-data");

=cut

sub remote { 
    use strict;
    use Capture::Tiny 'capture';
    my $scr = shift; 
    my $url = shift; 
    my $usr = shift; 
    my $log="";
    my ($stdout, $stderr, $irc)=capture {
	my $cmd="rsh -q -o NumberOfPasswordPrompts=0".
	    " franktt\@pc4804.pc.met.no /var/www/cgi-bin/metfark/$scr '$url'";
	$cmd =~ s/\=/\\\=/g;
	$cmd =~ s/\'/\\\'/g;
	$cmd =~ s/\;/\\\;/g;
	$cmd =~ s/\&/\\\&/g;
	$cmd =~ s/\@/\\\@/g;
	system $cmd;
    };
    chomp($stdout);chomp($stderr);
    if ($stdout) {
	$log=$stdout . "\n";
    } elsif ($irc) {
	if ($stderr) {
	    $log="Content-type: text/xml;\n\n<?xml version=\"1.0\" encoding=\"utf-8\"?>\n".
		"<error url=\"".&washXML($url)."\"".
		" message=\"$stderr\"".
		" user=\".$usr.\"/>\n";
	} else {
	    $log="Content-type: text/xml;\n\n<?xml version=\"1.0\" encoding=\"utf-8\"?>\n".
		"<error url=\"".&washXML($url)."\"".
		" message=\"rsh: Unable to execute command.\"".
		" user=\".$usr.\"/>\n";
	};
    } else {
	$log="Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n".
	    "<empty\>\n";
    }
    print $log;
}

sub washXML {
    my $cmd=shift;
    $cmd =~ s/</&lt;/g;
    $cmd =~ s/&/&amp;/g;
    $cmd =~ s/>/&gt/g;
    $cmd =~ s/"/&quot;/g;
    $cmd =~ s/'/&apos;/g;
    return $cmd;
}

=head2 open

open - creates a new fark session.

=head4 EXAMPLE

my $fark = fark->open();

=cut

sub open { 
    my $class = shift; 
    my $self={}; 
    if (my ($ret,$msg,$mid) = xs_openModelSession()){
    	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    	$self->{MID}=$mid;
    } else {
    	die "Unable to create class $class\n";
    }
    if (my ($ret,$msg,$bid) = xs_openObsSession()){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
	$self->{OID}=$bid;
    } else {
	die "Unable to create class $class\n";
    }
    if (my ($ret,$msg,$cid) = xs_openColocSession()){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
	$self->{CID}=$cid;
    } else {
	die "Unable to create class $class\n";
    }
    if (my ($ret,$msg,$pid) = xs_openTableSession()){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
	$self->{PID}=$pid;
    } else {
	die "Unable to create class $class\n";
    }
    $self->{ObsFileStack}=();
    bless $self => $class;
    return $self;
}


=head2 close

close - closes a fark session.

=head4 EXAMPLE

$fark->close();

=cut

sub close { 
    my $self = shift; 
    if (my ($ret,$msg) = xs_closeModelSession($self->{MID})){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    }
    if (my ($ret,$msg) = xs_closeObsSession($self->{OID})){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    }
    if (my ($ret,$msg) = xs_closeColocSession($self->{CID})){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    }
    if (my ($ret,$msg) = xs_closeTableSession($self->{PID})){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    }
}

################################# MODEL ################################
=head1 MODEL FUNCTIONS 

=cut

=head2 clearModelFileStack

clearModelFileStack - clears the model file stack and sets the sorting variable (optional).

Arguments:

=over 4

=item (string) variable used for sorting the files (optional).

=back

=head4 EXAMPLE

$fark->clearModelFileStack("time");

=cut


sub clearModelFileStack {
    my ($self,@args)=@_;
    # list bufr types that should be used...
    #print "clearModelFile: $self->{MID}, @args\n";
    my ($ret,$msg) = xs_clearModelFileStack($self->{MID},@args);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 pushModelFile

pushModelFile - adds model file to the model file stack.

Files pushed to the model file stack must be from the same model. An error is returned if the file type is unknown.

Arguments:

=over 4

=item (string) ... paths to model netCDF files.

=back

=head4 EXAMPLE

my @modelFiles=/opdata/arome25/AROME_MetCoOp*.nc>;
$fark->pushModelFile(@modelFiles);

=cut

sub pushModelFile {
    my ($self,@args)=@_;
    #print "pushModelFile Arguments: $self->{MID}, @args\n";
    my ($ret,$msg) = xs_pushModelFile($self->{MID},@args);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 peekModelFile 

peekModelFile - returns dimension and variable data on the last model file on the model file stack,
along with the values of the sorting variable specified by "clearModelFileStack".

Return:

=over 4

=item (hash) file structure

=back

=head4 EXAMPLE

my $peekdata = $fark->peekModelFile();

=cut

sub peekModelFile {
    my ($self)=@_;
    if (my ($ret,$msg,$nrep,@reps) = xs_peekModelFile($self->{MID})){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
	my $hash=getHash($nrep,0,@reps);
	bless $hash => "farkdata";
	return $hash;
    } else {
	return;
    }
}

=head2 popModelFile 

popModelFile - removes (the last) model file from the model file stack.

Arguments:

=over 4

=item (string) ... paths to model netCDF files (optional).

=back

Return:

=over 4

=item (hash) file structure hash

=back

=head4 EXAMPLE

$fark->popModelFile();

=cut

sub popModelFile {
    my ($self,@args)=@_;
    my ($ret,$msg) = xs_popModelFile($self->{MID},@args);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}



=head2 updateModelRegister

updateModelRegister - automatically pushes "new" files to the stack, and pops "deleted" files, using a register-file and file-mask.

Arguments:

=over 4

=item (string) path to register file (if missing, the register-file is created and no model-files are pushed to the stack).

=item (string) file mask.

=item (string) minimum file age (in days)

=item (string) maximum file age (in days)

=item (int) test flag (1 or 0), 1= only check input

=back

=head4 EXAMPLE

$fark->updateModelRegister($xmlfile,$modregfile,"/opdata/arome/", ".*\.nc","","",0);

=cut

sub updateModelRegister { 
    my $self = shift;
    my $register_file = shift;
    my $mask_dir = shift;
    my $mask = shift;
    my $min = farkdir::getOffset(shift // "",-1);
    my $max = farkdir::getOffset(shift // "",0);
    my $test = shift // 0;
    my $fill_file = shift // "";
    my $new_file_cnt = 0;
    my @new_file_list;
    my @push_files;
    my @pop_files;
    my $write_register_file=0;
    my @new_line_list = farkdir::FindFiles($mask_dir,$mask,$min,$max) or return @push_files;
    my %new_file_hash;
    foreach my $file (@new_line_list) {
	my $sb=(stat($file))[9];
	my $line = $sb . " " . $file;
	$new_file_hash{$file} = $line;
	push(@new_file_list, $file);
    }
    my %file_loaded;
    if ( ! -e $register_file ) {
	# make sure output file directories exist...
	my ($dir,$name)=farkdir::splitName($register_file);
	farkdir::makePath($dir);
	## No files will be retrieved first time...
	#%file_loaded = map {$_, 1} @new_file_list;
	print ">>>> No register file $register_file available.\n";
	$write_register_file=1;
    } else {
	# Load register file 
	if (CORE::open(REGISTER, "<$register_file")) { 
	    my $time=time() - <REGISTER>;
	    print ">>>> Reading register file: $register_file (age=".pretty($time).")\n";
	    while (<REGISTER>) {
		chomp;
		my $list=$_;
		(my $file) = ($list =~ m/^.*?\s+(\S+)$/);
		if (defined $new_file_hash{$file}) {
		    if ($list eq $new_file_hash{$file}) {
			$file_loaded{$file} = 1;
		    } else { # file has changed, need to remove and rescan
			$write_register_file=1;
			push (@pop_files,$file);
			push (@push_files,$file);
		    }
		} else { # old file, need to remove
		    $write_register_file=1;
		    push (@pop_files,$file);
		}
	    }
	    CORE::close REGISTER;
	} else {
	    print ">>>> Unable to open register file: $register_file\n";
	}
    }
    # find new files that need to be scanned
    if ($test) { print "Testing...";};
    foreach my $file (@new_file_list) {
	if (not defined $file_loaded{$file}) {
	    $write_register_file=1;
	    push (@push_files,$file);
	    if ($fill_file) { # touch fill-file
		farkdir::touchFile ($fill_file);
		if ($test) { # only process first file...
		    print ">>> Since this is a test, we only process: $file\n";
		    # do not update register or stack
		    $self->popModelFile(@pop_files); # remove old files

		    foreach my $f (@push_files) {
			print "File: '$f' '$file'\n";
		    }

		    print "pushing...\n";
		    $self->pushModelFile(@push_files); # add new files
		    return @push_files;
		};
		$fill_file="";
	    };
	}
    }
    $self->popModelFile(@pop_files); # remove old files
    if (@push_files) { # add new files
	print ">>>> Adding ".scalar(@push_files)." new files '$mask' ($mask_dir)";
	if ($min && $max) {print "<$min to $max>"; };
	print "\n";
	$self->pushModelFile(@push_files);
    } else {
	print ">>>> No new files '$mask' ($mask_dir)";
	if ($min && $max) {print "<$min to $max>"; };
	print "\n";
    }
    # Update local register file
    if ($write_register_file) {
	if (not CORE::open (REGISTER,">$register_file")) {
	    print ">>>> Unable to open register file: $register_file\n";
	} else {
	    print ">>>> Updating register file: $register_file\n";
	    print REGISTER time() . "\n";
	    foreach (@new_file_list) {
		print REGISTER "$new_file_hash{$_}\n";
	    }
	    CORE::close REGISTER;
	}
	chmod 0666, $register_file;

    }
    return @push_files;
}


=head2 makeModelCache

makeModelCache - makes a cache of the model-file-stack in the session.

Arguments:

=over 4

=item (string) path to model cache file.

=back

=head4 EXAMPLE

$fark->makeModelCache($modfile);

=cut

sub makeModelCache { 
    my $self = shift; 
    my $modfile = shift;
    my ($dir,$name)=farkdir::splitName($modfile);
    farkdir::makePath($dir);
    if (my ($ret,$msg) = xs_makeModelCache($self->{MID},$modfile)){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
	chmod 0666, $modfile;
    }
}

=head2 loadModelCache

loadModelCache - loads a cache of the model-file-stack created by "makeModelCache".

Arguments:

=over 4

=item (string) path to model cache file (optional in repeated calls).

=back

=head4 EXAMPLE

$fark->loadModelCache($modfile);

=cut

sub loadModelCache { 
    my $self = shift; 
    my $modfile = shift;
    if (my ($ret,$msg) = xs_loadModelCache($self->{MID},$modfile)){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;$msg =~ s/\\n/\n/g;die $msg;}
    }
}

=head2 setModelCache

setModelCache - sets name of cache file.

Arguments:

=over 4

=item (string) path to model cache file (optional in repeated calls).

=back

=head4 EXAMPLE

$fark->setModelCache($modfile);

=cut

sub setModelCache { 
    my $self = shift; 
    my $modfile = shift; 
    my ($dir,$name)=farkdir::splitName($modfile);
    farkdir::makePath($dir);
    if (my ($ret,$msg) = xs_setModelCache($self->{CID},$modfile)){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    }
}


=head2 setModelIndex

setModelIndex - sets a target for the model parameters

Arguments:

=over 4

=item (string) modelTarget

=item (string) modelVariable

=back

=head4 EXAMPLE

 $fark->setModelIndex("time_trg","time");

=cut

sub setModelIndex {
    my ($self,$trg,$var)=@_;
    my ($ret,$msg) = xs_setModelIndex($self->{MID},$trg,$var);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
}

=head2 setModelIndexLimits

setModelIndexLimits - sets model index limits

Arguments:

=over 4

=item (string) start index value 

=item (string) stop index value 

=back

=head4 EXAMPLE

 $fark->setModelSortLimits(0,1);

=cut

sub setModelIndexLimits {
    my ($self,$min,$max)=@_;
    my ($ret,$msg) = xs_setModelIndexLimits($self->{MID},$min,$max);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
}


=head2 clearModelTargetStack 

clearModelTargetStack -  clears the model target stack. If the model target is used, only models with valid targets are visible to the system.

=head4 EXAMPLE

$fark->clearModelTargetStack();

=cut

sub clearModelTargetStack {
    my ($self)=@_;
    my ($ret,$msg) = xs_clearModelTargetStack($self->{OID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 pushModelTarget 

pushModelTarget - defines an model target on the target stack

Arguments:

=over 4

=item (string) name

=item (string) variable

=item (real) min

=item (real) max

=back

=head4 EXAMPLE

$fark->pushModelTarget("modeltime","time","","");

=cut

sub pushModelTarget {
    my ($self,$nam,$var,$min,$max)=@_;
    my ($ret,$msg) = xs_pushModelTarget($self->{OID},$nam,$var,$min,$max);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}


=head2 clearDefaultStack 

clearDefaultStack -  clears the default stack. If the model default is used, only models with valid defaults are visible to the system.

=head4 EXAMPLE

$fark->clearDefaultStack();

=cut

sub clearDefaultStack {
    my ($self)=@_;
    my ($ret,$msg) = xs_clearDefaultStack($self->{CID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 addDefault 

addDefault - defines model default target value (in the absence of observations).

Arguments:

=over 4

=item (string) name

=item (int) value

=back

=head4 EXAMPLE

$fark->addDefault("modeltime","12220.0");

=cut

sub addDefault {
    my ($self,$nam,$val)=@_;
    my ($ret,$msg) = xs_addDefault($self->{CID},$nam,$val);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}


=head2 pushDefault 

pushDefault - pushes the "added" default values to the stack.

=head4 EXAMPLE

$fark->pushDefault();

=cut

sub pushDefault {
    my ($self)=@_;
    my ($ret,$msg) = xs_pushDefault($self->{CID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}




################################# OBSERVATIONS ##########################

=head1 OBSERVATION FUNCTIONS

=cut


=head2 setObservationTablePath

setObservationTablePath - defines the table path used to read the observation files.

Arguments:

=over 4

=item (string) path to the BUFR-table directory.

=back

=head4 EXAMPLE

$fark->setObservationTablePath("/usr/share/metno-bufrtables/");

=cut


sub setObservationTablePath {
    my ($self,$path)=@_;
    if ($path =~ m/.+[^\/]$/) {$path=$path . "/";};
    $ENV{"PRINT_TABLE_NAMES"}="FALSE";
    $ENV{"BUFR_TABLES"}=$path;
    my ($ret,$msg) = xs_setObsTablePath($self->{OID},$path);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 clearObservationFileStack

clearObservationFileStack - clears the observation file stack.

=head4 EXAMPLE

$fark->clearObservationFileStack();

=cut


sub clearObservationFileStack {
    my ($self)=@_;
    # list obs types that should be used...
    #print "clearObsFile: $self->{OID}\n";
    my ($ret,$msg) = xs_clearObsFileStack($self->{OID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 pushObservationFile

pushObservationFile - adds observation file to the observation file stack.

Arguments:

=over 4

=item (string) ... paths to observation files.

=back

=head4 EXAMPLE

   my @obsFiles=/opdata/arome25/AROME_MetCoOp*.nc>;
   $fark->pushObservationFile(@obsFiles);

=cut

sub pushObservationFile {
    my ($self,@args)=@_;
    my ($ret,$msg) = xs_pushObsFile($self->{OID},@args);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 peekObservationFile 

peekObservationFile - returns dimension and variable data on the last observation file on the observation file stack.
along with the values of the sorting variable specified by "clearObservationFileStack".

Return:

=over 4

=item (hash) file structure

=back

=head4 EXAMPLE

my $peekdata = $fark->peekObservationFile();

=cut

sub peekObservationFile {
    my ($self)=@_;
    if (my ($ret,$msg,$nrep,@reps) = xs_peekObsFile($self->{OID})){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
	my $hash=getHash($nrep,0,@reps);
	bless $hash => "farkdata";
	return $hash;
    } else {
	return;
    }
}

=head2 popObservationFile 

popObservationFile - removes (the last) observation file from the observation file stack.

Arguments:

=over 4

=item (string) ... paths to observation files (optional).

=back

Return:

=over 4

=item (hash) file structure hash

=back

=head4 EXAMPLE

$fark->popObservationFile();

=cut

sub popObservationFile {
    my ($self,@args)=@_;
    my ($ret,$msg) = xs_popObsFile($self->{OID},@args);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 updateObservationRegister

updateObservationRegister - automatically pushes "new" files to the stack, and pops "deleted" files, using a register-file and file-mask.

Arguments:

=over 4

=item (string) path to register file (if missing, the register-file is created and no observation-files are pushed to the stack).

=item (string) mask dir.

=item (string) file mask.

=item (string) minimum file age (in days)

=item (string) maximum file age (in days)

=back

=head4 EXAMPLE

$fark->updateObservationRegister($obsregfile,"/opdata/obs_dec/rdb/temp/temp_*06*.bufr","","",0);

=cut

sub updateObservationRegister { 
    my $self = shift;
    my $register_file = shift;
    my $mask_dir = shift;
    my $mask = shift;
    my $min = shift;
    my $max = shift;
    my $test = shift // 0; 
    my $fill_file = shift // "";
    my $new_file_cnt = 0;
    my @new_file_list;
    my @push_files;
    my @pop_files;
    my $write_register_file=0;
    my @new_line_list = farkdir::FindFiles($mask_dir,$mask,$min,$max) or return @push_files;
    my %new_file_hash;
    foreach my $file (@new_line_list) {
	my $sb=(stat($file))[9];
	my $line = $sb . " " . $file;
	$new_file_hash{$file} = $line;
	push(@new_file_list, $file);
    }
    my %file_loaded;
    if ( ! -e $register_file ) {
	#print "updateObservationRegister >>>> No register file '$register_file'\n";
	# make sure output file directories exist...
	my ($dir,$name)=farkdir::splitName($register_file);
	farkdir::makePath($dir);
	## No files will be retrieved first time...
	#%file_loaded = map {$_, 1} @new_file_list;
	print ">>>> No register file $register_file available.\n";
	$write_register_file=1;
    } else {
	# Load register file 
	if (CORE::open(REGISTER, "<$register_file")) { 
	    my $time=time() - <REGISTER>;
	    print ">>>> Reading register file: $register_file (age=".pretty($time).")\n";
	    while (<REGISTER>) {
		chomp;
		my $list=$_;
		(my $file) = ($list =~ m/^.*?\s+(\S+)$/);
		if (defined $new_file_hash{$file}) {
		    if ($list eq $new_file_hash{$file}) {
			#print "updateObservationRegister Un-changed: '$file' '$list'\n";
			$file_loaded{$file} = 1;
		    } else { # file has changed, need to remove and rescan
			$write_register_file=1;
			#print "updateObservationRegister Re-reading: '$file' '$list' '".$new_file_hash{$file}."'\n";
			push (@pop_files,$file);
			push (@push_files,$file);
		    }
		} else { # old file, need to remove
		    $write_register_file=1;
		    #print "updateObservationRegister Old: '$file' '$list'\n";
		    push (@pop_files,$file);
		}
	    }
	    CORE::close REGISTER;
	} else {
	    print ">>>> Unable to open register file: $register_file\n";
	}
    }
    # find new files that need to be scanned
    foreach my $file (@new_file_list) {
	if (not defined $file_loaded{$file}) {
	    $write_register_file=1;
	    push (@push_files,$file);
	    if ($fill_file) { # touch fill-file
		farkdir::touchFile ($fill_file);
		if ($test) {
		    print ">>> Since this is a test, we only process: $file\n";
		    # do not update register or stack
		    $self->popObservationFile(@pop_files); # remove old files
		    $self->pushObservationFile(@push_files); # add new files
		    return @push_files;
		};
		$fill_file="";
	    };
	}
    }
    $self->popObservationFile(@pop_files); # remove old files
    if (@push_files) { # add new files
	print ">>>> Adding ".scalar(@push_files)." new files '$mask' ($mask_dir)";
	$self->pushObservationFile(@push_files);
    } else {
	print ">>>> No new files...\n";
    }
    # Update local register file
    if ($write_register_file) {
	if (not CORE::open (REGISTER,">$register_file")) {
	    print ">>>> Unable to open register file: $register_file\n";
	} else {
	    print ">>>> Updating register file: $register_file\n";
	    print REGISTER time() . "\n";
	    foreach (@new_file_list) {
		print REGISTER "$new_file_hash{$_}\n";
	    }
	    CORE::close REGISTER;
	}
	chmod 0666, $register_file;
    }
    return @push_files;
}


=head2 makeObservationCache

makeObservationCache - makes a cache of the observation-file-stacks in the session.

Arguments:

=over 4

=item (string) path to obs cache file.

=item (int) test flag (1 or 0), 1= only check input

=back

=head4 EXAMPLE

$fark->makeObservationCache($obsfile);

=cut

sub makeObservationCache { 
    my $self = shift; 
    my $obsfile = shift;
    my ($dir,$name)=farkdir::splitName($obsfile);
    farkdir::makePath($dir);
    if (my ($ret,$msg) = xs_makeObsCache($self->{OID},$obsfile)){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
	chmod 0666, $obsfile;
    }
}

=head2 loadObservationCache

loadObservationCache - loads a cache of the observation-file-stack created by "makeObservationCache".

Arguments:

=over 4

=item (string) path to obs cache file (optional in repeated calls).

=back

=head4 EXAMPLE

$fark->loadObservationCache($obsfile);

=cut

sub loadObservationCache { 
    my $self = shift; 
    my $obsfile = shift;
    if (my ($ret,$msg) = xs_loadObsCache($self->{OID},$obsfile)){
	if ($ret != 0) {
	    $msg =~ s/\\n/\n/g;die $msg." (".$obsfile.")";
	};
    }
}

=head2 setObservationCache

setObservationCache - sets name of cache file.

Arguments:

=over 4

=item (string) path to obs cache file (optional in repeated calls).

=back

=head4 EXAMPLE

$fark->setObservationCache($obsfile);

=cut

sub setObservationCache { 
    my $self = shift; 
    my $obsfile = shift;
    my ($dir,$name)=farkdir::splitName($obsfile);
    farkdir::makePath($dir);
    if (my ($ret,$msg) = xs_setObsCache($self->{CID},$obsfile)){
	if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    }
}

=head2 setObservationType

setObservationType - sets the type of BUFR files that should be processed

Arguments:

=over 4

=item (integer) bufrType

=item (integer) subType

=back

=head4 EXAMPLE

 $fark->setObservationType(4,143);

=cut

sub setObservationType {
    my ($self,$bufrtype,$subtype)=@_;
    my ($ret,$msg) = xs_setObsBufrType($self->{OID},$bufrtype,$subtype);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
}



=head2 setObservationIndexLimits 

setObservationIndexLimits -  sets the first and last dates to consider in the processing of observation files and their observations. Call without arguments to clear the index limits.

Arguments:

=over 4

=item (string) start value of index expression

=item (string) stop value of index expression

=back

=head4 EXAMPLE

$fark->setObservationIndexLimits(10,100);
$fark->setObservationIndexLimits();

=cut

sub setObservationIndexLimits {
    my ($self,$start,$end)=@_;
    #print "fark.pm Calling xs_setObsIndexLimits with $start $end\n";
    my ($ret,$msg) = xs_setObsIndexLimits($self->{OID},$start,$end);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 clearObservationTargetStack 

clearObservationTargetStack -  clears the observation target stack. If the observation target is used, only observations with valid targets are visible to the system.

=head4 EXAMPLE

$fark->clearObservationTargetStack();

=cut

sub clearObservationTargetStack {
    my ($self)=@_;
    my ($ret,$msg) = xs_clearObsTargetStack($self->{OID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 pushObservationTarget 

pushObservationTarget - defines an observation target on the target stack

Arguments:

=over 4

=item (string) name

=item (int) pos

=item (int) descr

=item (string) info

=item (real) min

=item (real) max

=back

=head4 EXAMPLE

$fark->pushObservationTarget("yy","10","4001","year","","");

=cut

sub pushObservationTarget {
    my ($self,$name,$pos,$descr,$info,$min,$max)=@_;
    my ($ret,$msg) = xs_pushObsTarget($self->{OID},$name,$pos//"",$descr//"",$info//"",$min//"",$max//"");
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 setObservationIndex

setObservationIndex - set the observation index (used for sorting the stack).

Arguments:

=over 4

=item (string) target name

=item (string) expression

=back

=head4 EXAMPLE

$fark->setObservationIndex("time","dtg(yy,mm,dd,hh,mi)");
=cut

sub setObservationIndex {
    my ($self,$name,$expr)=@_;
    my ($ret,$msg,$nrep,@reps) = xs_setObsIndex($self->{OID},$name,$expr);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}


################################# COLOCATE ##########################



=head2 clearMatchRuleStack

clearMatchRuleStack - clear match-rule expressions.

=head4 EXAMPLE

 $fark->clearMatchRuleStack();

=cut

sub clearMatchRuleStack {
    my ($self)=@_;
    my ($ret,$msg) = xs_clearMatchRuleStack($self->{CID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
}

=head2 pushMatchRule

pushMatchRule - add a match-rule to the stack

Arguments:

=over 4

=item (string) model targetName

=item (string) obs expression

=item (string) min

=item (string) max

=back

=head4 EXAMPLE

 $fark->pushMatchRule("latitude_model","180.0*latitude_obs/3.14",0,90.0);

=cut

sub pushMatchRule {
    my ($self,$mod,$exp,$min,$max)=@_;
    my ($ret,$msg) = xs_pushMatchRule($self->{CID},$mod,$exp,$min,$max);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
}

=head2 makeMatchList

makeMatchList - make match list from match stack

=head4 EXAMPLE

 $fark->makeMatchList();

=cut

sub makeMatchList {
    my ($self)=@_;
    my ($ret,$msg) = xs_makeMatchList($self->{CID},$self->{MID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
}


=head2 setModelFilter

setModelFilter - add a model filter.

Arguments:

=over 4

=item (string) model filter (can contain observation targets).

=back

=head4 EXAMPLE

 $fark->setModelFilter("member(obs_id,1047,1049)");

=cut

sub setModelFilter {
    my ($self,$filter)=@_;
    my ($ret,$msg) = xs_setModelFilter($self->{MID},$filter);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
}


=head2 setObservationFilter

setObservationFilter - sets the type of BUFR files that should be processed

Arguments:

=over 4

=item (string) observation filter

=back

=head4 EXAMPLE

 $fark->setObservationFilter("member(obs_id,1,2,3)");

=cut

sub setObservationFilter {
    my ($self,$filter)=@_;
    my ($ret,$msg) = xs_setObsFilter($self->{OID},$filter);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
}


=head2 makeColocXML

makeColocXML - make XML file...

Arguments:

=over 4

=item (string) Path to XML file (with wildcards YYYY,MM,DD,HH,MI,SS for timestamp)

=item (int) test flag (1 or 0), 1= only check input

=back
=head4 EXAMPLE

my $xmlFile = $fark->makeColocXML($xmlPattern);

=cut

sub makeColocXML {
    my $self = shift;
    my $patt = shift;
    my $test = shift//0;
    my $fill_file = shift // "";
    #print "fark.pm Pattern $patt";
    my ($ret,$msg)= xs_setColocXMLFile($self->{PID},$patt);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    ($ret,$msg,my $xml)= xs_getColocXMLFile($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    my ($dir,$name)=farkdir::splitName($xml);
    farkdir::makePath($dir);
    #print " -> $xml\n";
    ($ret,$msg) = xs_makeColocXML($self->{CID},$self->{MID},$self->{OID},$xml,$test,$fill_file);
    if ($ret != 0) {
	if (-e $xml) {unlink $xml;};
	$msg =~ s/\\n/\n/g;die $msg;
    } else {
	if (-e $xml) {chmod 0666, $xml;};
    }
    return ($ret,$msg);
}

=head2 setShapeFile

setShapeFile - set default shape file.

Arguments:

=over 4

=item (string) shape file name

=back

=head4 EXAMPLE

$fark->setShapeFile("/metfark/config/shapefile/default");

=cut


sub setShapeFile {
    my ($self,$fn,$cn)=@_;
    if (! defined ($cn)) {$cn="SOVEREIGNT";};
    my ($ret,$msg) = xs_setShapeFile($fn,$cn);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}


=head2 simplifyShapes

simplifyShapes - simplify shapes using specified tolerance in km

Arguments:

=over 4

=item (string) tolerance in km

=back

=head4 EXAMPLE

$fark->simplifyShapes("10");

=cut


sub simplifyShapes {
    my ($self,$tol)=@_;
    my ($ret,$msg) = xs_simplifyShapes($tol);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}


=head2 clearShapeFile

clearShapeFile - clear shape file information.

=head4 EXAMPLE

$fark->clearShapeFile();

=cut


sub clearShapeFile {
    my ($self)=@_;
    my ($ret,$msg) = xs_clearShapeFile();
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}


################################# TABLE  ##########################

=head1 TABLE FUNCTIONS

=cut



=head2 setTableType

setTableType - defines the type of output files.

Arguments:

=over 4

=item (string) name of the type, e.g. "rms+stdv", "scatter", "skill", "text"

=back

=head4 EXAMPLE

$fark->setTableType("rms+stdv");

=cut


sub setTableType {
    my ($self,$type)=@_;
    my ($ret,$msg) = xs_setTableType($self->{PID},$type);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}


=head2 setTableFile

setTableFile - sets the tableFile.

Arguments:

=over 4

=item (string) name of table file.

=back

=head4 EXAMPLE

$fark->setTableFile("rms+stdv");

=cut


sub setTableFile {
    my ($self,$tableFile)=@_;
    my ($ret,$msg) = xs_setTableFile($self->{PID},$tableFile);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}


=head2 getTableFile

getTableFile - gets the tableFile.

Arguments:

=over 4

=item (string) name of table file.

=back

=head4 EXAMPLE

$fark->getTableFile("rms+stdv");

=cut


sub getTableFile {
    my ($self,$tableFile)=@_;
    my ($ret,$msg,$fn) = xs_getTableFile($self->{PID},$tableFile);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return $fn;
}


=head2 setGraphicsFile

setGraphicsFile - sets the graphicsFile.

Arguments:

=over 4

=item (string) name of graphics file.

=back

=head4 EXAMPLE

$fark->setGraphicsFile("rms+stdv");

=cut


sub setGraphicsFile {
    my ($self,$graphicsFile)=@_;
    my ($ret,$msg) = xs_setGraphicsFile($self->{PID},$graphicsFile);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}


=head2 getGraphicsFile

getGraphicsFile - gets the graphicsFile.

Arguments:

=over 4

=item (string) name of graphics file.

=back

=head4 EXAMPLE

$fark->getGraphicsFile("rms+stdv");

=cut


sub getGraphicsFile {
    my ($self,$graphicsFile)=@_;
    my ($ret,$msg,$fn) = xs_getGraphicsFile($self->{PID},$graphicsFile);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return $fn;
}


=head2 clearTableSetStack

clearTableSetStack - clears the datasets in the table set stack.

=head4 EXAMPLE

$fark->clearTableSetStack();

=cut


sub clearTableSetStack {
    my ($self)=@_;
    # list obs types that should be used...
    #print "clearTableSetStack: $self->{PID}\n";
    my ($ret,$msg) = xs_clearTableSetStack($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 clearTableColumn

clearTableColumn - clears all output columns from the set.


=head4 EXAMPLE

   $fark->clearTableColumn();

=cut

sub clearTableColumn {
    my ($self)=@_;
    my ($ret,$msg) = xs_clearTableColumn($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 pushTableColumn

pushTableColumn - adds a column expression to the set.

Arguments:

=over 4

=item (string) column name

=item (string) column value expression

=back

=head4 EXAMPLE

   $fark->pushTableColumn("X-value","temperature_2m");

=cut

sub pushTableColumn {
    my ($self,$name,$expr)=@_;
    my ($ret,$msg) = xs_pushTableColumn($self->{PID},$name,$expr);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 pushTableSet

pushTableSet - adds a data set to the table set stack.

Arguments:

=over 4

=item (string) table session id

=item (string) colocation session id

=item (string) model session id

=item (string) observation session id

=item (string) name of set

=item (string) legend

=back

=head4 EXAMPLE

   $fark->pushTableSet("1","time","temperature_2m","Arome (T2M)");

=cut

sub pushTableSet {
    my ($self,$name,$legend)=@_;
    my ($ret,$msg) = xs_pushTableSet($self->{PID},$self->{CID},$self->{MID},
				   $self->{OID},$name,$legend);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 clearTableAttributeStack

clearTableAttributeStack - clears the table attribute stack.

=head4 EXAMPLE

$fark->clearTableAttributeStack;

=cut


sub clearTableAttributeStack {
    my ($self)=@_;
    # list obs types that should be used...
    #print "clearTableSet: $self->{PID}\n";
    my ($ret,$msg) = xs_clearTableAttributeStack($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 pushTableAttribute

pushTableAttribute - adds an attribute.

Arguments:

=over 4

=item (string) name of attribute

=item (string) attribute value.

=back

=head4 EXAMPLE

   $fark->pushTableAttribute("title","Temperature - Tryvann");

=cut

sub pushTableAttribute {
    my ($self,$name,$value)=@_;
    my ($ret,$msg) = xs_pushTableAttribute($self->{PID},$name,$value);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 limitTableColumn

limitTableColumn - sets a min and max limit on a column.

Arguments:

=over 4

=item (string) name of column

=item (string) min value

=item (string) max value

=back

=head4 EXAMPLE

   $fark->limitTableColumn("t2m","270.0","290.0");

=cut

sub limitTableColumn {
    my ($self,$name,$min,$max)=@_;
    my ($ret,$msg) = xs_limitTableColumn($self->{PID},$name,$min,$max);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 makeTableFile

makeTableFile - make table file...

Arguments:

=over 4

=item (string) Path to table file (with wildcards YYYY,MM,DD,HH,MI,SS for timestamp)

=item (string) Path to graphics directory (with wildcards YYYY,MM,DD,HH,MI,SS for timestamp)

=item (string) Path to Rscript file

=item (int) test flag (1 or 0), 1= only check input

=back

=head4 EXAMPLE

my ($tablefile) = $fark->makeTableFile($tablepattern,$graphicspattern,$catfile,0);

=cut

sub makeTableFile {
    my $self = shift;
    my $tfile = shift;
    my $gdir = shift;
    my $cfile = shift;
    my $test = shift//0;
    my $fill_file = shift // "";
    my $ret;
    my $msg;
    my $dir;
    my $name;
    # set file names
    print ">>>> makeTableFile Entering....\n";
    ($ret,$msg)= xs_setTableFile($self->{PID},$tfile);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    # replace wildcards in file names
    print ">>>> makeTableFile Table strep....\n";
    ($ret,$msg)= xs_strepTableFile($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    ($ret,$msg,$tfile)= xs_getTableFile($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    #
    ($ret,$msg)= xs_setGraphicsDir($self->{PID},$gdir);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    print ">>>> makeTableFile Graphics strep....\n";
    ($ret,$msg)= xs_strepGraphicsDir($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    ($ret,$msg,$gdir)= xs_getGraphicsDir($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    $| = 1 ;
    # make sure output file directories exist...
    ($dir,$name)=farkdir::splitName($tfile);
    farkdir::makePath($dir);
    # make the output...
    print ">>>> makeTableFile calling xs_makeTableFile....\n";
    ($ret,$msg,my $tablefile,my $graphicsdir) = 
	xs_makeTableFile($self->{PID},$self->{CID},$self->{MID},$self->{OID},
			 $tfile,$gdir,$cfile,$test,$fill_file);
    if ($ret != 0) {
	if (-e $tablefile) {unlink $tablefile;};
	$msg =~ s/\\n/\n/g;die $msg;
    } else {
	if (-e $tablefile) {chmod 0666, $tablefile;};
	if (-d $graphicsdir) {chmod 0666, $graphicsdir;};
    }
    return ($tablefile,$graphicsdir);
}



=head2 updateTableFileList

updateTableFileList - automatically pushes "new" files to the stack, and pops "deleted" files, using a register-file and file-mask.

Arguments:

=over 4

=item (string) path to register file (if missing, the register-file is created and no model-files are pushed to the stack).

=item (string) file mask.

=item (string) minimum file age (in days)

=item (string) maximum file age (in days)

=item (int) test flag (1 or 0), 1= only check input

=back

=head4 EXAMPLE

$fark->updateTableFileList($xmlfile,$modregfile,"/opdata/arome/", ".*\.nc","","",0);

=cut

sub addTableFiles { 
    my $self = shift;
    my $mask_dir = shift;
    my $mask = shift;
    my $min = farkdir::getOffset(shift // "",-1);
    my $max = farkdir::getOffset(shift // "",0);
    my $test = shift // 0;
    my $fill_file = shift // "";
    my $new_file_cnt = 0;
    my $write_register_file=0;
    my @files = farkdir::FindFiles($mask_dir,$mask,$min,$max);
    if (@files) {
	print "**** pushTableFile $#files files found ($mask_dir,$mask,$min,$max): @files\n";
	$self->pushTableFile(@files);
    } else {
	print "**** pushTableFile No files found ($mask_dir,$mask,$min,$max)\n";
    };
    return @files;
}

=head2 clearTableFileStack

clearTableFileStack - clears the table file stack and sets the sorting variable (optional).

Arguments:

=over 4

=item (string) variable used for sorting the files (optional).

=back

=head4 EXAMPLE

$fark->clearTableFileStack();

=cut


sub clearTableFileStack {
    my ($self,@args)=@_;
    # list bufr types that should be used...
    #print "clearTableFile: $self->{MID}, @args\n";
    my ($ret,$msg) = xs_clearTableFileStack($self->{MID},@args);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}

=head2 pushTableFile

pushTableFile - adds table file to the table file stack.

Files pushed to the table file stack must be from the same table. An error is returned if the file type is unknown.

Arguments:

=over 4

=item (string) ... paths to table netCDF files.

=back

=head4 EXAMPLE

my @tableFiles=/opdata/arome25/AROME_MetCoOp*.nc>;
$fark->pushTableFile(@tableFiles);

=cut

sub pushTableFile {
    my ($self,@args)=@_;
    #print "pushTableFile Arguments: $self->{MID}, @args\n";
    my ($ret,$msg) = xs_pushTableFile($self->{MID},@args);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
}


=head2 joinTableFile

joinTableFile - make table file...

Arguments:

=over 4

=item (string) Path to table file (with wildcards YYYY,MM,DD,HH,MI,SS for timestamp)

=item (string) Path to graphics directory (with wildcards YYYY,MM,DD,HH,MI,SS for timestamp)

=item (string) Path to Rscript file

=item (int) test flag (1 or 0), 1= only check input

=back

=head4 EXAMPLE

my ($tablefile) = $fark->joinTableFile($tablepattern,$graphicspattern,$catfile,$dir,$mask,$min,$max,0);

=cut

sub joinTableFile {
    my $self = shift;
    my $tfile = shift;
    my $gdir = shift;
    my $cfile = shift;
    my $test = shift//0;
    my $fill_file = shift // "";
    my $ret;
    my $msg;
    my $dir;
    my $name;
    # set file names
    ($ret,$msg)= xs_setTableFile($self->{PID},$tfile);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    # replace wildcards in file names
    ($ret,$msg)= xs_strepTableFile($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    ($ret,$msg,my $tablefile)= xs_getTableFile($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    #
    ($ret,$msg)= xs_setGraphicsDir($self->{PID},$tfile);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    ($ret,$msg)= xs_strepGraphicsDir($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    ($ret,$msg,my $graphicsdir)= xs_getGraphicsDir($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    $| = 1 ;
    # make sure output file directories exist...
    ($dir,$name)=farkdir::splitName($tfile);
    farkdir::makePath($dir);
    # make the output...
    ($ret,$msg,$tablefile,$graphicsdir)= 
	xs_joinTableFile($self->{PID},$self->{CID},$self->{MID},$self->{OID},
			  $tfile,$gdir,$cfile,$test,$fill_file);
    if ($ret != 0) {
	if (-e $tablefile) {unlink $tablefile;};
	$msg =~ s/\\n/\n/g;die $msg;
    } else {
	if (-e $tablefile) {chmod 0666, $tablefile;};
	if (-d $graphicsdir) {chmod 0666, $graphicsdir;};
    }
    return ($tablefile,$graphicsdir);
}

sub catTableFiles {
    my ($tfile,$gdir,@files)=@_;
    die "catTableFiles not implemented.";
}

=head2 strepTableFile

strepTableFile - replace wildcards and make diretory for table file

Arguments:

=over 4

=item (string) Path to table file (with wildcards YYYY,MM,DD,HH,MI,SS for timestamp)

=item (int) test flag (1 or 0), 1= only check input

=back

=head4 EXAMPLE

my ($tablefile) = $fark->strepTableFile($tablepattern,$catfile,0);

=cut

sub strepTableFile {
    my $self = shift;
    my $tfile = shift;
    my $ret;
    my $msg;
    my $dir;
    my $name;
    # set file names
    ($ret,$msg)= xs_setTableFile($self->{PID},$tfile);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    # replace wildcards in file names
    ($ret,$msg)= xs_strepTableFile($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    # retrieve file names
    ($ret,$msg,$tfile)= xs_getTableFile($self->{PID});
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    #
    $| = 1 ;
    # make sure output file directories exist...
    ($dir,$name)=farkdir::splitName($tfile);
    farkdir::makePath($dir);
    return ($tfile);
}

################################# RERUN ##########################

sub setRerunVariable { # $fark->setRerunVariable($variable);
    my ($self,$variable)=@_;
    my ($ret,$msg) = xs_setVariable($variable);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
};

sub setRerunValue {    # $fark->setRerunValue($value);
    my ($self,$value)=@_;
    my ($ret,$msg) = xs_setValue($value);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
};

sub setRerunOffset {    # $fark->setTimeOffset($offset);
    my ($self,$offset)=@_;
    my ($ret,$msg) = xs_setOffset($offset);
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return;
};

sub getRerunOffset {    # $fark->getTimeOffset();
    my ($self)=@_;
    my ($ret,$msg,$offset) = xs_getOffset();
    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
    return $offset;
};


################################# GENERAL ##########################

sub getHash {
    my $hash={};
    my $cnt=0;
    my $nrep=shift;
    my $narr=shift;
    for (my $ii=0; $ii  < $nrep;  $ii++) {
#    foreach my $rep (@reps)  {
	my $rep=shift;
	if (defined ($rep)) {
	    if (++$cnt > $nrep) {last;}
	    $rep =~ s/\s+$//;
	    #print "getHash $rep\n";
	    my @items=split (/\|/,$rep,-1);
	    &makeHashBranch($hash,@items);
	}
    }
    if ($narr) {
	$hash->{"values"} = \@_;
    }
    return $hash;
}

sub makeHashBranch{
    my ($hash,@items)=@_;
    my $nitems=@items;
    if ($nitems==2) {
	$hash->{$items[0]}={$items[1]};
    } elsif ($nitems==3) {
	$hash->{$items[0]}->{$items[1]} = $items[2];
    } elsif ($nitems==4) {
	$hash->{$items[0]}->{$items[1]}->{$items[2]} = $items[3];
    } elsif ($nitems==5) {
	$hash->{$items[0]}->{$items[1]}->{$items[2]}->{$items[3]} = $items[4];
    } elsif ($nitems==6) {
	$hash->{$items[0]}->{$items[1]}->{$items[2]}->{$items[3]}->{$items[4]} = $items[5];
    } elsif ($nitems==7) {
	$hash->{$items[0]}->{$items[1]}->{$items[2]}->{$items[3]}->{$items[4]}->{$items[5]} = $items[6];
    } elsif ($nitems==8) {
	$hash->{$items[0]}->{$items[1]}->{$items[2]}->{$items[3]}->{$items[4]}->{$items[5]}->{$items[6]} = $items[7];
    } elsif ($nitems==9) {
	$hash->{$items[0]}->{$items[1]}->{$items[2]}->{$items[3]}->{$items[4]}->{$items[5]}->{$items[6]}->{$items[7]} = $items[8];
    } elsif ($nitems==10) {
	$hash->{$items[0]}->{$items[1]}->{$items[2]}->{$items[3]}->{$items[4]}->{$items[5]}->{$items[6]}->{$items[7]}->{$items[8]} = $items[9];
    }
}


=head2 expression

expression - evaluate expression

=head4 EXAMPLE

my $result = fark->expression('1+2');

=cut

sub expression { 
    my($self,$exp) = @_;
    my $res = "0";
    if ($exp) {
	my $ret;
	my $msg;
	if (($ret,$msg,$res) = xs_expression($exp)){
	    if ($ret != 0) {$msg =~ s/\\n/\n/g;die $msg;}
	} else {
	    die "Unable to evaluate expression $exp\n";
	}
    }
    return $res;
}

=head2 debug

debug - set debug flag.

Arguments:

=over 4

=item (int) 1=debug on, 0=debug off

=back

=head4 EXAMPLE

   fark::debug(1);
   ...
   fark::debug(0);

=cut

sub debug {
    my ($ideb)=@_;
    xs_setDebug($ideb);
    return;
}

sub pretty {
    my $ss=shift;
    my $dd=int($ss/86400);
    $ss=$ss-86400*$dd;
    my $hh=int($ss/3600);
    $ss=$ss-3600*$hh;
    my $mi=int($ss/60);
    $ss=$ss-60*$mi;
    my $s="";
    if ($dd) {
	if ($s){$s=$s.",";}
	$s=$s . sprintf("%s",$dd) . "d";
    }
    if ($hh) {
	if ($s){$s=$s.",";}
	$s=$s . sprintf("%s",$hh) . "h";
    }
    if ($mi) {
	if ($s){$s=$s.",";}
	$s=$s . sprintf("%s",$mi) . "m";
    }
    if ($ss) {
	if ($s){$s=$s.",";}
	$s=$s . sprintf("%s",$ss) . "s";
    }
    if (! $s) {$s="0s";}
    return $s;
}


1;
__END__

=head1 INSTALLATION

# fark-perl installation:   
 sudo dpkg --install /vol/fou/atmos2/franktt/fark/fark-perl_0.13-1_amd64.deb

# run test-script
 /vol/fou/atmos2/franktt/fark/farkmod.pl
 /vol/fou/atmos2/franktt/fark/farkobs.pl

# manual
 man fark
 man farkdata

# remove package
 sudo dpkg --remove fark-perl

=head1 AUTHOR

Frank Thomas Tveter, E<lt>f.t.tveter@met.noE<gt>

=head1 SEE ALSO

 NETCDF
 BUFR
 ncdump -h
 ncview

=cut

