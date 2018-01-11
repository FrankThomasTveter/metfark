#!/usr/bin/perl -w
#
use ExtUtils::testlib;
use fark;
use farkdata;
use farkdir;
#
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use File::Basename;
use Data::Dumper;
#dont know if you need this: sudo apt-get install libpath-tiny-perl
#but you need this: sudo apt-get install libcapture-tiny-perl
use Capture::Tiny 'capture';
#
my $ref=CGI->new();
my $param= $ref->{param};
my $parser = XML::LibXML->new();
#
my $debug=0;
if (defined $param->{debug}[0]) {
    $debug=1;
}
fark::debug(5);  # debug parse
#
$XML::LibXML::skipXMLDeclaration = 1;
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
#print "Content-type: text/xml;\n\n";
#
my $doc = $parser->parse_string("<expression><result></result></expression>");
my ($node) = $doc->findnodes("expression/result");
my $res="";
my $log="";
my ($stdout,$stderr,$irc)=("","",0);
if ($debug) {
    my $fark=fark->open();
    print "Processing '".($param->{exp}[0]//"")."'\n";
    $res=$fark->expression($param->{exp}[0]//"");
    $fark->close();
} else {
    farkdir::termval { 
	my $fark=fark->open();
	print "Processing '".($param->{exp}[0]//"")."'\n";
	$res=$fark->expression($param->{exp}[0]//"");
	$fark->close();
    } "Error while running.";
};
my $mret=$@;if ($mret || $irc) {farkdir::term("fark_exp.pl $stderr $mret");}
#print "Output: $stdout\n\n $stderr\n";
#
$node->setAttribute("value",$res);
#
if ($debug) {
    print "$stdout\n\n$stderr\n";
}
# report xml-structure
print $doc->toString . "\n";
