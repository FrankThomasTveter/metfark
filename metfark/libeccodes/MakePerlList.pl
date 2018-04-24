#!/usr/bin/perl
if(opendir(D,".")){while (my $f=readdir(D)){if(-d $f && $f=~/perl.*/){print "$f ";}}}
