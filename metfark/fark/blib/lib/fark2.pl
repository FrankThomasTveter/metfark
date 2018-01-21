#!/usr/bin/perl
use ExtUtils::testlib;
use lib '/home/franktt/fark/src/perlfark/blib/lib';
use fark;
use farkmodel;
use strict;
my $fark = fark->open();                    # open new fark-session
$fark->clearModelFileStack("time");               # clear model file stack
my @modelFiles=</opdata/simra/simbn12.nc>;    # define netcdf files
$fark->modelFileSetup("","netcdf");               # fimex config-file and type
$fark->pushModelFile(@modelFiles);                # add files to the fark-stack
$fark->peekModelFile()->printTree("#Peek: ");     # look at the last file on the stack
while (my $filedata=$fark->getPrevModelFile()) { # loop over the files on the stack
    $fark->clearModelLocationStack("(time)","geopotential_height_ml"); # define slicing variables
    my $time = 1.05;
    my $lat=65.431;
    my $lon=12.166;
    my $height = 970.0;
    $fark->pushModelLocation("synop",$lat,$lon,$time,$height); # define location
    $fark->printModelLocation();                  # print location stack
    while (my $farkmodel=$fark->sliceCurrentModelFile("bilinear")) { # loop over colocation data
	$farkmodel->printTree("#Raw data: "); # print raw data
	my $postproc = ($farkmodel
			->makeBilinear()      # add bilinear slicing coefficients
			->interpolate("bilinear") # add interpolated variable-data
			->selectKey("bilinear") # only keep "bilinear" data
			->getBranch({"variable"=>""}) # make a copy of the "variable" branch
			->compress()          # remove single-key hashes...
			->printTree("#Post processed: ")); # print post-processed data
	print ">>>>> Found surface_air_pressure=".$postproc->{"surface_air_pressure"}."\n";
    };
}
$fark->clearModelFileStack();                     # clear memory
$fark->close();                              # close fark-session
