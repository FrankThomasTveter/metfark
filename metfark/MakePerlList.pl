#!/usr/bin/perl
if(opendir(D,".")){while (my $f=readdir(D)){if($f=~/perl.*/){print "$f ";}}}
