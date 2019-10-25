#!/usr/bin/perl
# FUNCTION: 
#   "Perl script for remapping of netcdf files based on lat lon values" 
#    Script takes file names as arguments.
#
# example: 
#   ./nclatlon.pl   variables.nc   projection.nc   output.nc
#
# This script is basically a wrapper/config-file for "nclatlon.bin".
#
# AUTHOR: Frank Thomas Tveter, 10.01.2014
# MODIFIED: 

use strict;

# Usage: ./nclatlon.pl <variable file> <projection file> <output file>

# read file names
my $fileVar = shift;
my $filePro = shift;
my $fileOut = shift;

# run binary program with pipe to standard input
my $bindir   =  "/mir/netcdf/src/nclatlon";
my $cmd = "${bindir}/nclatlon.bin";
die "\n ** Unable to initiate $cmd **\n" unless (-x $cmd);
open(FH,"|$cmd");
print FH  <<EOF || die "\n ** Unable to run $cmd **\n";
# ?
# exit
#
 NCLATLON V1.0 [0]VFLR
 NCLATLON V1.0 [0]VFLR
#
 VARIABLE FILE (NETCDF) [1]VFLR
   $fileVar
#
 VARIABLE FILE LATITUDE LONGITUDE NAMES [1]VFLR
   latitude longitude
#
 PROJECTION FILE (NETCDF) [1]VFLR
   $filePro
#
 PROJECTION FILE LATITUDE LONGITUDE NAMES [1]VFLR
  lat lon
#
#
 OUTPUT FILE [1]VFLR
   $fileOut

#
EOF
close(FH) || die "\n ** Error while running $cmd **\n";
