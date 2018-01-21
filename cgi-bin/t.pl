#!/usr/bin/perl -w
#
use farkdir;
#
my @files=farkdir::FindFiles("/lustre/storeB/immutable/archive/projects/metproduction/meps",
			     "meps_mbr0_extracted_2_5km_\\d\\d\\d\\d\\d\\d\\d\\dT\\d\\dZ.nc",
			     0,20,0);
#
foreach my $f(@files) {
    print "File: $f\n";
}
#
