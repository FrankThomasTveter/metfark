#!/usr/bin/perl
use ExtUtils::testlib;
####use lib '/home/franktt/fark/src/perlfark/blib/lib';
#
use fark;
use farkdata;
use strict;

##########################################################################
# open session, submit list of BUFR- and NetCDF-files
##########################################################################

print "===Opening session...\n";
my $fark = fark->open();

#
# TBD: load model and observation stack from stack-file (only loaded if file has changed)
# TBD: $fark->loadStackFile($file)
#

print "===Clearing file stack...\n";
$fark->clearModelFileStack("time");
$fark->clearObservationFileStack();

# $fark->pushObservationType( 2, 91); # radio-sonde

print "===Finding latest files...\n";
my @observationFiles = < /opdata/obs_dec/rdb/temp/temp_*06*.bufr >;
my @modelFiles       = < /opdata/simra/simbn12.nc >;

print "===Setup...\n";
$fark->observationTablePath("/usr/share/metno-bufrtables/");

print "===Adding files to file stack... @modelFiles\n";
$fark->pushModelFile(@modelFiles);
$fark->pushObservationFile(@observationFiles);

#
# TBD: save model and observation stack to stack-file
# TBD: $fark->saveStackFile($file)...
#

# ignore observations labels to speed up things...
#$fark->ignoreObservationLabels("missing","section","descriptor","description","unit");
#$fark->ignoreObservationLabels("missing","unit");

#
# OBSERVATION TYPE FILTER...
#
# $fark->pushObservationFilter( 2, 91); # radio-sonde filter
# $fark->pushObservationFilter( 2, 255); # temp temperature filter

#
# temp temperature, "<label>:<sequence>:<descriptor>:<min>:<max>"
   $fark->clearObservationFilterStack(); 
   $fark->pushObservationFilter( 2, 255,"lat:15:5001:59.0:61.0","lon:16:6001:9.0:11.0");
#

##########################################################################
# loop over model files, get start/end times
##########################################################################

print "===Looping modelfiles...\n";
while (my $modelfiledata=$fark->getPrevModelFile()) { # model files
    $fark->clearModelLocationStack("time","geopotential_height_ml"); #
    $modelfiledata->printTree("Modelfile:");
    my $tstart=getStartTime($modelfiledata);
    my $tstop=getStopTime($modelfiledata);
    my $cnt=0;
    my $coloc={};
    bless $coloc => "farkdata";
    print "===Looping obsfiles...\n";

    ##########################################################################
    # loop over observation files, get lat/lon/time
    ##########################################################################

    while (my $obsfiledata=$fark->getPrevObservationFile($tstart,$tstop)) { # obs files
    #while (my $obsfiledata=$fark->getPrevObservationFile()) { # obs files
	$obsfiledata->printTree("Obsfile:");
        print "===Looping observations...\n";
	while (my $obsdata=$fark->sliceCurrentObservationFile()) { # observations
#	while (my $obsdata=$fark->sliceCurrentObservationFileArray()) { # observations
	    $obsdata->printTree("Sub:");
	    #$obsdata->getBranch({"section"=>""})->printTree("Sub:");
	    print "FIRST value =" . $obsdata->{"values"}->[0] . "\n"; # only works with "sliceCurrentObservationFileArray"

           ##########################################################################
           # add model location
           ##########################################################################

	    my $lat=getLatitude($obsdata);
	    my $lon=getLongitude($obsdata);
	    my $hgt=getHeight($obsdata);
	    my $epoch=getEpoch($obsdata);

	    print "Location, lat=\"$lat\" lon=\"$lon\" epoch=\"$epoch\"\n";


	    die "Debug"; #######################################


	    if (defined($lat) & defined($lon) & defined($epoch)) {
		$cnt++;
		$fark->pushModelLocation($cnt,$lat,$lon,$epoch,$hgt);
		print "Count=$cnt\n" unless ($cnt % 1000);
		if ($cnt==1) {
		    $obsdata -> printTree("Sample:");
		}
		$coloc->{$cnt}->{"obs"} = $obsdata;
	    } else {
		print "   Warning: ignored observation...\n";
	    }
	}
    }

    ##########################################################################
    # loop over model file locations
    ##########################################################################

    print "===Looping $cnt model locations...\n";
    $cnt=0;
    while (my $modeldata=$fark->sliceCurrentModelFile("bilinear")) { 
	$modeldata->printTree("Modeldata:");
	my $postproc = ($modeldata
			->makeBilinear()
			->interpolate("bilinear")
			->selectKey("bilinear")
			->getBranch({"variable"=>""})
			#####->compress()
			->printTree("#Post processed: "));
	$cnt++;
	$coloc->{$cnt}->{"mod"} = $postproc;

    };

    ##########################################################################
    # process the colocated data
    ##########################################################################

    print "===Processing $cnt colocations\n";
    $coloc->printTree("Coloc:");
    
    #
    # process $coloc->{1..$cnt} here...
    #
}

##########################################################################
# wrap up and free memory gracefully
##########################################################################

print "===Clearing observation stack\n";
$fark->clearModelFileStack();
$fark->clearObservationFileStack();

print "===Closing session...\n";
$fark->close();

##########################################################################
# user-defined subroutines for extracting data from model file data...
##########################################################################

sub getStartTime {
    use farkdata;
    my $modelfiledata=shift;
    my $epoch=$modelfiledata->{"file"}{"variable"}{"time"}{"1"};
    return getEpochTime($epoch);
}
sub getStopTime {
    my $modelfiledata=shift;
    my $cnt=$modelfiledata->{"file"}{"variable"}{"time"}{"cnt"};
    my $epoch=$modelfiledata->{"file"}{"variable"}{"time"}{$cnt};
    return getEpochTime($epoch);
}

#
use DateTime;
sub getEpochTime {
    my $epoch=shift;
    print "Epoch:\"$epoch\"\n"; 
    my $dt=DateTime->from_epoch(epoch => $epoch);
    my $dtz = $dt->iso8601()."Z";
    print "Time:$dtz\n"; 
    return $dtz;
}

##########################################################################
# user-defined subroutines for extracting data from observation data...
##########################################################################

sub getLatitude {
    my $obsdata=shift;
    my $lat=$obsdata->{"values"}->[14];
    if (! defined($lat)) {
	$lat=$obsdata -> {"subset"}{"1"}{"sequence"}{"15"}{"descriptor"}{"5001"}{"value"};
    }
    return $lat;
}
sub getLongitude {
    my $obsdata=shift;
    my $lon=$obsdata->{"values"}->[15];
    if (! defined($lon)) {
	$lon=$obsdata -> {"subset"}{"1"}{"sequence"}{"16"}{"descriptor"}{"6001"}{"value"};
    }
    return $lon;
}
sub getEpoch {
    my $obsdata=shift;
    my $yy= $obsdata->{"values"}->[8];
    my $mm= $obsdata->{"values"}->[9];
    my $dd= $obsdata->{"values"}->[10];
    my $hh= $obsdata->{"values"}->[11];
    my $mi= $obsdata->{"values"}->[12];
    my $ss= $obsdata->{"values"}->[13];
    if (! defined($yy)) {
	$yy=$obsdata -> {"subset"}{"1"}{"sequence"}{"9"}{"descriptor"}{"4001"}{"value"};
	$mm=$obsdata -> {"subset"}{"1"}{"sequence"}{"10"}{"descriptor"}{"4002"}{"value"};
	$dd=$obsdata -> {"subset"}{"1"}{"sequence"}{"11"}{"descriptor"}{"4003"}{"value"};
	$hh=$obsdata -> {"subset"}{"1"}{"sequence"}{"12"}{"descriptor"}{"4004"}{"value"};
	$mi=$obsdata -> {"subset"}{"1"}{"sequence"}{"13"}{"descriptor"}{"4005"}{"value"};
	$ss=$obsdata -> {"subset"}{"1"}{"sequence"}{"14"}{"descriptor"}{"4006"}{"value"};
    }
    print "Found time: $yy-$mm-${dd}T$hh-$mi-${ss}Z\n";
    my $dt =  DateTime->new( year   => $yy,
			     month  => $mm,
			     day    => $dd,
			     hour   => $hh,
			     minute => $mi,
			     second => $ss);
    return $dt->epoch();
}
sub getHeight {
    my $obsdata=shift;
}
