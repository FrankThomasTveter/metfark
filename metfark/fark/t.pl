#!/usr/bin/perl

&foo(variable => "time", config_file => "test", ttt);


sub foo {
    while (my $arg=shift){
	print type($arg) . " $arg\n";
    }
}
