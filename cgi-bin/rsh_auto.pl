#!/usr/bin/perl -w
#
use strict;
#use URI::Encode qw(uri_encode);
use fark;
#
my $scr = "fark_auto.pl";
my $url = $ENV{QUERY_STRING} || join(' ',@ARGV);
my $usr = getpwuid($<);
#
fark::remote($scr,$url,$usr);
#

