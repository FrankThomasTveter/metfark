#!/usr/bin/perl -w
#
use strict;
use lib "/home/ubuntu/perl5/lib/perl5/x86_64-linux-gnu-thread-multi";
use URI::Encode qw(uri_encode);
use fark;
#
my $scr = "fark_coloc.pl";
my $url = uri_encode($ENV{QUERY_STRING} || join(' ',@ARGV));
my $usr = getpwuid($<);
#
fark::remote($scr,$url,$usr);
#

