#!/usr/bin/perl -w
use Capture::Tiny 'capture';
#
eval {
    my ($stdout,$stderr,$irc)=capture {
	return "test";
    };
    if ($irc) {
	die "Error return '$irc' '$stderr'\n";
    } else {
	die "No error.\n";
    }
};
if ($@) {die ($@);}
