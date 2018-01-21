#!/usr/bin/perl
use ExtUtils::testlib;
use lib '/home/franktt/fark/src/perlfark/blib/lib';
use fark;
use farkdata;
use strict;
#
# open new fark-session
my $fark = fark->open();
# clear BUFR file stack
$fark->clearBufrFileStack();
# add BUFR-files to stack
my @bufrFiles=</opdata/hirlam12/*.bufr>;
$fark->pushBufrFile(@bufrFiles);
# clear model stack. Set model config-file and file-sorting-variable
$fark->clearModelFileStack("time");
# define netcdf-files
my @modelFiles=</opdata/simra/simbn12.nc>;
# add netcdf-files to stack
$fark->pushModelFile(@modelFiles);
# loop over netcdf-files chronologically based on file-sorting-variable
while (my $farkdata=$fark->getPrevModelFile()) {
    # prepare observation data hash
    my $obs = {};
    my $cnt=0;
    # Prepare location stack. Define slice variables.
    $fark->clearModelLocationStack("(time)","geopotential_height_ml"); #
    my $tstart=$farkdata->getFirstEpoch();
    my $tstop=$farkdata->getLastEpoch();
    # loop over relevant bufr files to find relevant locations
    while (my $farkbufr=$fark->getPrevBufrFile($tstart,$stop)) {
        # loop over bufr-messages in files
	while (my $farkbufr=$fark->getNextObservation($tstart,$stop)) {
            my $time = $farkbufr->getEpoch();
            my $lat=$farkbufr->getLatitude();
            my $lon=$farkbufr->getLongitude();
            my $l=$farkbufr->getGeoPotentialHeight();
	    $cnt=$cnt+1;
            $fark->pushModelLocation($cnt,$lat,$lon,$time,$l);
	    $obs->{$cnt}=$farkbufr; # store the observation data for later
	}
    }
    # print location list
    $fark->printModelLocation();
    # colocate locations with model data
    while (my $farkdata=$fark->sliceCurrentModelFile("bilinear")) { 
	# print raw data to screen
	$farkdata->printTree("#Raw data: ");
        # print post processed data to screen
	my $postproc = ($farkdata
			->makeBilinear()
			->interpolate("bilinear")
			->selectKey("bilinear")
			->getBranch({"variable"=>""})
			#####->compress()
			->printTree("#Post processed: "));
    };
}
# clear memory
$fark->clearModelFileStack();
# close fark-session
$fark->close();

