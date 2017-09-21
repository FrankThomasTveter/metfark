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

=head2 open

open - creates a new fark session.

=head4 EXAMPLE

my $fark = fark->open();

=cut

sub open { 
    my $class = shift; 
    my $self={}; 
    if (my ($ret,$msg,$mid) = xs_openModelSession()){
    	if ($ret != 0) {die $msg;}
    	$self->{MID}=$mid;
    } else {
    	die "Unable to create class $class\n";
    }
    if (my ($ret,$msg,$bid) = xs_openObsSession()){
	if ($ret != 0) {die $msg;}
	$self->{OID}=$bid;
    } else {
	die "Unable to create class $class\n";
    }
    if (my ($ret,$msg,$cid) = xs_openColocSession()){
	if ($ret != 0) {die $msg;}
	$self->{CID}=$cid;
    } else {
	die "Unable to create class $class\n";
    }
    if (my ($ret,$msg,$pid) = xs_openPlotSession()){
	if ($ret != 0) {die $msg;}
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
	if ($ret != 0) {die $msg;}
    }
    if (my ($ret,$msg) = xs_closeObsSession($self->{OID})){
	if ($ret != 0) {die $msg;}
    }
    if (my ($ret,$msg) = xs_closeColocSession($self->{CID})){
	if ($ret != 0) {die $msg;}
    }
    if (my ($ret,$msg) = xs_closePlotSession($self->{PID})){
	if ($ret != 0) {die $msg;}
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
    if ($ret != 0) {die $msg;}
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
    if ($ret != 0) {die $msg;}
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
	if ($ret != 0) {die $msg;}
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
    if ($ret != 0) {die $msg;}
    return;
}



=head2 updateModelRegister

updateModelRegister - automatically pushes "new" files to the stack, and pops "deleted" files, using a register-file and file-mask.

Arguments:

=over 4

=item (string) path to register file (if missing, the register-file is created and no model-files are pushed to the stack).

=item (string) file mask.

=back

=head4 EXAMPLE

$fark->updateModelRegister($xmlfile,$modregfile,"/opdata/arome/", ".*\.nc");

=cut

sub updateModelRegister { 
    my ($self, $register_file, $mask_dir, $mask) = @_; 
    my $new_file_cnt = 0;
    my @new_file_list;
    my @push_files;
    my @pop_files;
    my $write_register_file=0;
    my @new_line_list = GetFiles($mask_dir,$mask) or return @push_files;
    my %new_file_hash;
    foreach (@new_line_list) {
	s/\s+/ /g; # replace any train of blanks with single blank
	my $line = $_;
	#next if (substr($line,0,1) ne '-'); # only process files
	my ($file) = ($line =~ m/\s+(\S+)$/); # last column is file name
	$new_file_hash{$file} = $line;
	push(@new_file_list, $file);
    }
    my %file_loaded;
    if ( ! -e $register_file ) {
	## No files will be retrieved first time...
	#%file_loaded = map {$_, 1} @new_file_list;
	#print ">>>> No register file $register_file available.\n";
	$write_register_file=1;
    } else {
	# Load register file 
	#print ">>>> Reading register file: $register_file\n";
	if (CORE::open(REGISTER, "<$register_file")) { 
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
	    print ">>>> Unable to open $register_file\n";
	}
    }
    # find new files that need to be scanned
    foreach my $file (@new_file_list) {
	if (not defined $file_loaded{$file}) {
	    $write_register_file=1;
	    push (@push_files,$file);
	}
    }
    $self->popModelFile(@pop_files); # remove old files
    $self->pushModelFile(@push_files); # add new files
    # Update local register file
    if ($write_register_file) {
	#print "Updating: $register_file\n";
	if (not CORE::open (REGISTER,">$register_file")) {
	    print ">>>> Unable to open $register_file\n";
	}
	foreach (@new_file_list) {
	    print REGISTER "$new_file_hash{$_}\n";
	}
	CORE::close REGISTER;
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
    if (my ($ret,$msg) = xs_makeModelCache($self->{MID},$modfile)){
	if ($ret != 0) {die $msg;}
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
	if ($ret != 0) {die $msg;}
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
    if (my ($ret,$msg) = xs_setModelCache($self->{CID},$modfile)){
	if ($ret != 0) {die $msg;}
    }
}


=head2 setModelIndex

setModelIndex - sets a target for the model parameters

Arguments:

=over 4

=item (string) modelVariable

=back

=head4 EXAMPLE

 $fark->setModelIndex("time");

=cut

sub setModelIndex {
    my ($self,$var)=@_;
    my ($ret,$msg) = xs_setModelIndex($self->{MID},$var);
    if ($ret != 0) {die $msg;}
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
    if ($ret != 0) {die $msg;}
}


=head2 clearModelTargetStack 

clearModelTargetStack -  clears the model target stack. If the model target is used, only models with valid targets are visible to the system.

=head4 EXAMPLE

$fark->clearModelTargetStack();

=cut

sub clearModelTargetStack {
    my ($self)=@_;
    my ($ret,$msg) = xs_clearModelTargetStack($self->{OID});
    if ($ret != 0) {die $msg;}
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
    if ($ret != 0) {die $msg;}
    return;
}


=head2 clearModelDefaultStack 

clearModelDefaultStack -  clears the model default stack. If the model default is used, only models with valid defaults are visible to the system.

=head4 EXAMPLE

$fark->clearModelDefaultStack();

=cut

sub clearModelDefaultStack {
    my ($self)=@_;
    my ($ret,$msg) = xs_clearModelDefaultStack($self->{OID});
    if ($ret != 0) {die $msg;}
    return;
}

=head2 addModelDefault 

addModelDefault - defines model default target value (in the absence of observations).

Arguments:

=over 4

=item (string) name

=item (int) value

=back

=head4 EXAMPLE

$fark->addModelDefault("modeltime","12220.0");

=cut

sub addModelDefault {
    my ($self,$nam,$val)=@_;
    my ($ret,$msg) = xs_addModelDefault($self->{OID},$nam,$val);
    if ($ret != 0) {die $msg;}
    return;
}


=head2 pushModelDefault 

pushModelDefault - pushes the "added" default values to the stack.

=head4 EXAMPLE

$fark->pushModelDefault();

=cut

sub pushModelDefault {
    my ($self)=@_;
    my ($ret,$msg) = xs_pushModelDefault($self->{OID});
    if ($ret != 0) {die $msg;}
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
    $ENV{"BUFR_TABLES"}=$path;
    my ($ret,$msg) = xs_setObsTablePath($self->{OID},$path);
    if ($ret != 0) {die $msg;}
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
    if ($ret != 0) {die $msg;}
    return;
}

=head2 pushObservationFile

pushObservationFile - adds observation file to the observation file stack.

Arguments:xs

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
    if ($ret != 0) {die $msg;}
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
	if ($ret != 0) {die $msg;}
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
    if ($ret != 0) {die $msg;}
    return;
}

=head2 updateObservationRegister

updateObservationRegister - automatically pushes "new" files to the stack, and pops "deleted" files, using a register-file and file-mask.

Arguments:

=over 4

=item (string) path to register file (if missing, the register-file is created and no observation-files are pushed to the stack).

=item (string) file mask.

=back

=head4 EXAMPLE

$fark->updateObservationRegister($obsregfile,"/opdata/obs_dec/rdb/temp/temp_*06*.bufr");

=cut

sub updateObservationRegister { 
    my ($self, $register_file, $mask_dir, $mask) = @_; 
    my $new_file_cnt = 0;
    my @new_file_list;
    my @push_files;
    my @pop_files;
    my $write_register_file=0;
    my @new_line_list = GetFiles($mask_dir,$mask) or return @push_files;
    my %new_file_hash;
    foreach (@new_line_list) {
	s/\s+/ /g; # replace any train of blanks with single blank
	my $line = $_;
	#next if (substr($line,0,1) ne '-'); # only process files
	my ($file) = ($line =~ m/\s+(\S+)$/); # last column is file name
	$new_file_hash{$file} = $line;
	push(@new_file_list, $file);
    }
    my %file_loaded;
    if ( ! -e $register_file ) {
	## No files will be retrieved first time...
	#%file_loaded = map {$_, 1} @new_file_list;
	#print ">>>> No register file $register_file available.\n";
	$write_register_file=1;
    } else {
	# Load register file 
	#print ">>>> Reading register file: $register_file\n";
	if (CORE::open(REGISTER, "<$register_file")) { 
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
	    print ">>>> Unable to open $register_file\n";
	}
    }
    # find new files that need to be scanned
    foreach my $file (@new_file_list) {
	if (not defined $file_loaded{$file}) {
	    $write_register_file=1;
	    push (@push_files,$file);
	}
    }
    $self->popObservationFile(@pop_files); # remove old files
    $self->pushObservationFile(@push_files); # add new files
    # Update local register file
    if ($write_register_file) {
	#print "Updating: $register_file\n";
	if (not CORE::open (REGISTER,">$register_file")) {
	    print ">>>> Unable to open $register_file\n";
	}
	foreach (@new_file_list) {
	    print REGISTER "$new_file_hash{$_}\n";
	}
	CORE::close REGISTER;
    }
    return @push_files;
}


=head2 makeObservationCache

makeObservationCache - makes a cache of the observation-file-stacks in the session.

Arguments:

=over 4

=item (string) path to obs cache file.

=back

=head4 EXAMPLE

$fark->makeObservationCache($obsfile);

=cut

sub makeObservationCache { 
    my $self = shift; 
    my $obsfile = shift;
    if (my ($ret,$msg) = xs_makeObsCache($self->{OID},$obsfile)){
	if ($ret != 0) {die $msg;}
    }
}

=head2 loadObservationCache

loadObservationCache - loads a cache of the observation-file-stack created by "makeObservationCache".

Arguments:

=over 4

=item (string) parh to obs cache file (optional in repeated calls).

=back

=head4 EXAMPLE

$fark->loadObservationCache($obsfile);

=cut

sub loadObservationCache { 
    my $self = shift; 
    my $obsfile = shift;
    if (my ($ret,$msg) = xs_loadObsCache($self->{OID},$obsfile)){
	if ($ret != 0) {die $msg;}
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
    if (my ($ret,$msg) = xs_setObsCache($self->{CID},$obsfile)){
	if ($ret != 0) {die $msg;}
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
    if ($ret != 0) {die $msg;}
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
    if ($ret != 0) {die $msg;}
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
    if ($ret != 0) {die $msg;}
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
    my ($ret,$msg) = xs_pushObsTarget($self->{OID},$name,$pos,$descr,$info,$min,$max);
    if ($ret != 0) {die $msg;}
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
    if ($ret != 0) {die $msg;}
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
    my ($ret,$msg) = xs_clearMatchRuleStack($self->{MID});
    if ($ret != 0) {die $msg;}
}

=head2 addMatchRule

addMatchRule - add a match-rule to the stack

Arguments:

=over 4

=item (string) model targetName

=item (string) obs expression

=item (string) min

=item (string) max

=back

=head4 EXAMPLE

 $fark->addMatchRule("latitude_model","180.0*latitude_obs/3.14",0,90.0);

=cut

sub addMatchRule {
    my ($self,$mod,$exp,$min,$max)=@_;
    my ($ret,$msg) = xs_addMatchRule($self->{MID},$mod,$exp,$min,$max);
    if ($ret != 0) {die $msg;}
}


=head2 setColocFilter

setColocFilter - add a colocation filter.

Arguments:

=over 4

=item (string) colocation filter.

=back

=head4 EXAMPLE

 $fark->setColocFilter("member(obs_id,1047,1049)");

=cut

sub setColocFilter {
    my ($self,$filter)=@_;
    my ($ret,$msg) = xs_setColocFilter($self->{MID},$filter);
    if ($ret != 0) {die $msg;}
}




=head2 colocXML

colocXML - slice model and observation files, dumping resulting XML to standard out...

=head4 EXAMPLE

 $fark->colocXML();

=cut

sub colocXML {
    my ($self)=@_;
    my ($ret,$msg) = xs_colocXML($self->{CID},$self->{MID},$self->{OID});
    return ($ret,$msg);
}

################################# PLOT  ##########################

=head1 PLOT FUNCTIONS

=cut



=head2 setPlotType

setPlotType - defines the type of output files.

Arguments:

=over 4

=item (string) name of the type, e.g. "rms+stdv", "scatter", "skill", "text"

=back

=head4 EXAMPLE

$fark->setPlotType("rms+stdv");

=cut


sub setPlotType {
    my ($self,$type)=@_;
    my ($ret,$msg) = xs_setPlotType($self->{PID},$type);
    if ($ret != 0) {die $msg;}
    return;
}


=head2 setPlotTableFile

setPlotTableFile - sets the tableFile.

Arguments:

=over 4

=item (string) name of table file.

=back

=head4 EXAMPLE

$fark->setPlotTableFile("rms+stdv");

=cut


sub setPlotTableFile {
    my ($self,$tableFile)=@_;
    my ($ret,$msg) = xs_setPlotTableFile($self->{PID},$tableFile);
    if ($ret != 0) {die $msg;}
    return;
}
sub setPlotType {
    my ($self,$type)=@_;
    my ($ret,$msg) = xs_setPlotType($self->{PID},$type);
    if ($ret != 0) {die $msg;}
    return;
}


=head2 getPlotTableFile

getPlotTableFile - gets the tableFile.

Arguments:

=over 4

=item (string) name of table file.

=back

=head4 EXAMPLE

$fark->getPlotTableFile("rms+stdv");

=cut


sub getPlotTableFile {
    my ($self,$tableFile)=@_;
    my ($ret,$msg,$fn) = xs_getPlotTableFile($self->{PID},$tableFile);
    if ($ret != 0) {die $msg;}
    return $fn;
}


=head2 setPlotGraphicsFile

setPlotGraphicsFile - sets the graphicsFile.

Arguments:

=over 4

=item (string) name of graphics file.

=back

=head4 EXAMPLE

$fark->setPlotGraphicsFile("rms+stdv");

=cut


sub setPlotGraphicsFile {
    my ($self,$graphicsFile)=@_;
    my ($ret,$msg) = xs_setPlotGraphicsFile($self->{PID},$graphicsFile);
    if ($ret != 0) {die $msg;}
    return;
}


=head2 getPlotGraphicsFile

getPlotGraphicsFile - gets the graphicsFile.

Arguments:

=over 4

=item (string) name of graphics file.

=back

=head4 EXAMPLE

$fark->getPlotGraphicsFile("rms+stdv");

=cut


sub getPlotGraphicsFile {
    my ($self,$graphicsFile)=@_;
    my ($ret,$msg,$fn) = xs_getPlotGraphicsFile($self->{PID},$graphicsFile);
    if ($ret != 0) {die $msg;}
    return $fn;
}


=head2 clearPlotSetStack

clearPlotSetStack - clears the datasets in the plot set stack.

=head4 EXAMPLE

$fark->clearPlotSetStack();

=cut


sub clearPlotSetStack {
    my ($self)=@_;
    # list obs types that should be used...
    #print "clearPlotSetStack: $self->{PID}\n";
    my ($ret,$msg) = xs_clearPlotSetStack($self->{PID});
    if ($ret != 0) {die $msg;}
    return;
}

=head2 pushPlotSet

pushPlotSet - adds a data set to the plot set stack.

Arguments:xs

=over 4

=item (string) plot session id

=item (string) colocation session id

=item (string) model session id

=item (string) observation session id

=item (string) name of set

=item (string) x-variable expression

=item (string) y-variable expression

=item (string) legend

=back

=head4 EXAMPLE

   $fark->pushPlotSet("1","time","temperature_2m","Arome (T2M)");

=cut

sub pushPlotSet {
    my ($self,$name,$x,$y,$legend)=@_;
    my ($ret,$msg) = xs_pushPlotSet($self->{PID},$self->{CID},$self->{MID},
				   $self->{OID},$name,$x,$y,$legend);
    if ($ret != 0) {die $msg;}
    return;
}

=head2 clearPlotAttributeStack

clearPlotAttributeStack - clears the plot attribute stack.

=head4 EXAMPLE

$fark->clearPlotAttributeStack;

=cut


sub clearPlotAttributeStack {
    my ($self)=@_;
    # list obs types that should be used...
    #print "clearPlotSet: $self->{PID}\n";
    my ($ret,$msg) = xs_clearPlotAttributeStack($self->{PID});
    if ($ret != 0) {die $msg;}
    return;
}

=head2 pushPlotAttribute

pushPlotAttribute - adds an attribute.

Arguments:xs

=over 4

=item (string) name of attribute

=item (string) attribute value.

=back

=head4 EXAMPLE

   $fark->pushPlotAttribute("title","Temperature - Tryvann");

=cut

sub pushPlotAttribute {
    my ($self,$name,$value)=@_;
    my ($ret,$msg) = xs_pushPlotAttribute($self->{PID},$name,$value);
    if ($ret != 0) {die $msg;}
    return;
}
=head2 makePlotTable

makePlotTable - make table file, return name of plot graphics file...

=head4 EXAMPLE

my ($tablefile,$plotfile) = $fark->makePlotTable($tablepattern,$plotpattern);

=cut

sub makePlotTable {
    my ($self,$tfile,$gfile)=@_;
    my $ret;
    my $msg;
    my $dir;
    my $name;
    # set file names
    ($ret,$msg)= xs_setTableFile($self->{PID},$tfile);
    if ($ret != 0) {die $msg;}
    ($ret,$msg)= xs_setGraphicsFile($self->{PID},$gfile);
    if ($ret != 0) {die $msg;}
    # replace wildcards in file names
    ($ret,$msg)= xs_strepPlotFiles($self->{PID});
    if ($ret != 0) {die $msg;}
    # retrieve file names
    ($ret,$msg,$tfile)= xs_getTableFile($self->{PID});
    if ($ret != 0) {die $msg;}
    ($ret,$msg,$gfile)= xs_getGraphicsFile($self->{PID});
    if ($ret != 0) {die $msg;}
    #
    # make sure output file directories exist...
    ($dir,$name)=farkdir::splitName($tfile);
    farkdir::makePath($dir);
    ($dir,$name)=farkdir::splitName($gfile);
    farkdir::makePath($dir);
    # make the output...
    ($ret,$msg,my $tablefile,my $graphicfile) = 
	xs_makePlotTable($self->{PID},$self->{CID},$self->{MID},$self->{OID},
			 $tfile,$gfile);
    if ($ret != 0) {die $msg;}
    return ($tablefile,$graphicfile);
}

=head2 makePlotGraphics

makePlotGraphics - generate plot graphics.

Arguments:

=over 4

=item (string) name of table input file.

=item (string) name of graphics output plot file.

=back

=head4 EXAMPLE

my $file = $fark->makePlotGraphics($tablefile,$graphicfile);

=cut

sub makePlotGraphics {
    my ($self,$tablefile,$graphicfile)=@_;
    my ($ret,$msg) = xs_makePlotGraphics($self->{PID},$tablefile,$graphicfile);
    if ($ret != 0) {die $msg;}
    return;
}

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

sub GetFiles {
    my($filterDir,$filter) = @_;
    my @files=();
    find({wanted => sub {
	if (-f $File::Find::name && $File::Find::name =~ m/$filter/) {
	    my $file=$File::Find::name;
	    my $sb=(stat($file))[9];
	    my $s= $sb. " " . $file;
	    push(@files, $s);
	}
	  }}, $filterDir);
    return @files;
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
	    if ($ret != 0) {die $msg;}
	} else {
	    die "Unable to evaluate expression $exp\n";
	}
    }
    return $res;
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

