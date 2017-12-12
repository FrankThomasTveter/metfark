#!/usr/bin/perl -w
#
use strict;
#use URI::Encode qw(uri_encode);
use fark;
#
my $scr = "fark_load.pl";
my $url = $ENV{QUERY_STRING} || join(' ',@ARGV); # uri_encode()
my $usr = getpwuid($<);
#
fark::remote($scr,$url,$usr);
#

