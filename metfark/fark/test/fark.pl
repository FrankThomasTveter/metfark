#!/usr/bin/perl
use ExtUtils::testlib;
use fark;
use strict;
#
#
# clear Model stack. Set model config-file and file-sorting-variable
#
fark::clearFileStack("","time");
#
# define netcdf-files
#
my @modelFiles=</opdata/simra/simbn12.nc>;
####my @modelFiles=</opdata/arome25/AROME_MetCoOp*.nc>;

#
# add netcdf-files to stack
#
#my $farkStack = new fark();

#$farkStack->pushFile(@modelFiles);
fark::pushFile(@modelFiles);
#
# Peek/look at the last netcdf-file on the stack
#
if (my $hash=fark::peekFile()) { # uvanlig med "peek", perl-tie
    #print toString("#Peek: ",$hash) ;
}
#
# loop over netcdf-files chronologically based on file-sorting-variable
#
while (my $hash=fark::getPrevFile()) {
#
# print file-sorting-variable
#
#    print $hash->toString("#Analysis: ",$hash) ;
    #print toString("#Analysis: ",$hash) ;
#
# Prepare location stack. Define slice variables.
#
    fark::clearLocationStack("time","l");
#
# define locations
#
    my $time = $hash->{"file"}{"sort"}{"variable"}{"time"}{"2"};
    my $lat=65.431;
    my $lon=12.166;
    my $l=11.5;
    fark::pushLocation("synop",$lat,$lon,$time,$l);
#
    $lat=65.421;
    $lon=12.166;
    my $hgt=900.0;
#    fark::pushLocation("temp",$lat,$lon,$time,$hgt);
#
    $lat=65.431;
    $lon=12.146;
    $hgt=300.0;
#    fark::pushLocation("temp",$lat,$lon,$time,$hgt);
#
# print location list
#
    fark::printLocation();
#
# colocate locations with model data
#
    while (my $hash=fark::sliceCurrentFile("bilinear")) { 
#
# print colocated data to screen
#
	print toString("#Coloc: ",$hash) ;
    };
}
#
# clear memory
#
fark::clearFileStack();


#################################################################################

sub toString{
    my $pre=shift;
    return toString_($pre,$pre,@_);
}

sub toString_ {
    my $s="";
    my $pre=shift;
    my $pos=shift;
    my $cnt=0;
    foreach my $ref (@_){
	if ($cnt++>0) {$s=$s . "${pre}---\n"; $pre=$pos;}
	if (ref($ref) eq "ARRAY") {
	    $s=$s . toString_($pre."   ",@$ref);
	    $pre=$pos;
	} elsif (ref($ref) eq "HASH") {
	    foreach my $k (sort {$a <=> $b} keys %{$ref}) {
		my $v=$ref->{$k};
		my $lpos=$pos . " " x length($k) . "  ";
		$s=$s . "$pre$k=>" . toString_("",$lpos,$v);
		$pre=$pos;
	    }
	} else {
	    $s=$s . "$pre$ref\n";
	    $pre=$pos;
	}
    }
    return $s;
}

#################################################################################
