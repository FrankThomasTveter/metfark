#!/usr/bin/perl -w
#
use farkdir;
our $override_exit = 0;
BEGIN { 
    *CORE::GLOBAL::exit = sub (;$) {
        no warnings 'exiting';
        last EXIT_OVERRIDE if $override_exit;
        CORE::exit($_[0] // 0);
    };
 }
#
print "Here...\n";
my $ext = 1;
 EXIT_OVERRIDE: {
    local $override_exit = 1;
    eval { ttt() };
    $ext = 0;
    die $@ if $@;
};
print "Exit was called\n" if $ext;
print "There...\n";

sub ttt {
    print "Where...\n";
    exit 1;
}
