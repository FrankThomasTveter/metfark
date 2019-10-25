#!/usr/bin/perl -w
#
use strict;
use URI::Encode qw(uri_encode);
use fark;
#
my $scr = "fark_exec.pl";
my $url = uri_encode($ENV{QUERY_STRING} || join(' ',@ARGV));
my $usr = getpwuid($<);
#
fark::remote($scr,$url,$usr);
#

