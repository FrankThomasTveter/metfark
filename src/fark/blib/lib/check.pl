#!/usr/bin/perl
use strict;

my $pmfile = "lib/fark.pm";
my $xsfile = "fark.xs";
my $fordir = "../";

print "Processing cgi-bin\n";
my %cgicalls=();
my @files = </var/www/cgi-bin/metfark/*.pl>;
foreach my $file (@files) {
    print "Processing $file\n";
    open(my $fh, "<", $file) || die "Unable to open $pmfile";
    while (my $row = <$fh>) {
	chomp $row;
	#print $row . "\n";
	if ($row =~ m/fark\-\>([^\(\)\s]+)/) {
	    print "   Found call to $1\n"; 
	    $cgicalls{$1} = $file;
	}
    }
    close ($fh);
}

print "Processing $pmfile\n";
open(my $ph, "<", $pmfile) || die "Unable to open $pmfile";
my %pmcalls=();
my %pmsubs=();
while (my $row = <$ph>) {
  chomp $row;
  #print $row . "\n";
  if ($row =~ m/(xs_[^\(\)\s]+)/) {
      print "   Found call to $1\n"; 
      $pmcalls{$1} = $pmfile;
  }
  if ($row =~ m/sub\s+([^\(\)\s]+)\s*\{/) {
      print "   Found declaration $1\n"; 
      $pmsubs{$1} = $pmfile;
  }
}
close ($ph);

print "Processing $xsfile\n";
open(my $xh, "<", $xsfile) || die "Unable to open $xsfile";
my %xssubs;
my %xscalls;
while (my $row = <$xh>) {
  chomp $row;
  if ($row =~ m/(xs_[^\(\)\s]+)/) {
      print "   Found declaration $1\n"; 
      $xssubs{$1} = $xsfile;
  }
  if ($row =~ m/([^\s]{3}_[^\(\)\s]+)_\(/) {
      print "   Found call to $1\n"; 
      $xscalls{$1} = $xsfile;
  }
}
close ($xh);

print "Looking for CGI-calls.\n";
foreach my $key (keys %cgicalls) {
    if (defined $pmsubs{$key}) {
	#print "    defined: $key\n";
    } else {
	print "NOT defined: $key \t\t(".$cgicalls{$key}.")\n";
    }
}
foreach my $key (keys %pmsubs) {
    if (defined $cgicalls{$key}) {
	#print "    defined: $key\n";
    } else {
	print "NOT used: $key \t\t(".$pmsubs{$key}.")\n";
    }
}

print "Looking for PM-calls.\n";
foreach my $key (keys %pmcalls) {
    if (defined $xssubs{$key}) {
	#print "    defined: $key\n";
    } else {
	print "NOT defined: $key \t\t(".$pmcalls{$key}.")\n";
    }
}
foreach my $key (keys %xssubs) {
    if (defined $pmcalls{$key}) {
	#print "    defined: $key\n";
    } else {
	print "NOT used: $key \t\t(".$xssubs{$key}.")\n";
    }
}

print "Looking for XS-calls.\n";
foreach my $key (keys %xscalls) {
    #print "Checking $key\n";
    my $ret=`grep -i $key ../*/*.F90`;
    if ($ret eq "") {
	print "Missing fortran definition: $key \t\t(".$xscalls{$key}.")\n"
    }
}
