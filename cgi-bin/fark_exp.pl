#!/usr/bin/perl -w
#
use strict;
use lib "/home/ubuntu/perl5/lib/perl5/x86_64-linux-gnu-thread-multi";
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use File::Basename;
use Data::Dumper;
#dont know if you need this: sudo apt-get install libpath-tiny-perl
#but you need this: sudo apt-get install libcapture-tiny-perl
use Capture::Tiny 'capture';
#
use ExtUtils::testlib;
use fark;
use farkdata;
use farkdir;
#
my $ref=CGI->new();
my $param= $ref->{param};
my $parser = XML::LibXML->new();
#
my $debug=0;
if (defined $param->{debug}[0]) {
    $debug=1;
    fark::debug(5);  # debug parse
    fark::debug(6);  # debug shape
}
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
my $tolerance = "10.0";             # shape tolerance in km
fark->simplifyShapes($tolerance);  # simplify shapes
my $shapeDir=farkdir::getRootDir("shape") || farkdir::term("Invalid root directory (shape)");
my $shapeFile=$shapeDir . "ne_50m_admin_0_countries";
fark->setShapeFile($shapeFile);
if ($debug) {
    my $fark=fark->open();
    &setConfig($fark);
    #print "Processing '".($param->{exp}[0]//"")."'\n";
    $res=$fark->expression($param->{exp}[0]//"");
    $fark->close();
} else {
    farkdir::sandbox { 
	my $fark=fark->open();
	&setConfig($fark);
	#print "Processing '".($param->{exp}[0]//"")."'\n";
	$res=$fark->expression($param->{exp}[0]//"");
	$fark->close();
    } {message=>"Error while running.",
       logfile=>"/tmp/fark_exp.tmp",
       stdout=>"success"
    };
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
#
sub setConfig { # setConfig($fark,$variable,$value,$offset);
    my $fark= shift;
    my $variable=shift;
    my $value=shift;
    my $offset=shift;
    if (defined $value  && ! "$value" eq "") {
	$fark->setRerunVariable($variable);
	$fark->setRerunValue($value);
	$fark->setRerunOffset($offset);
    } else {
	if  ($debug) {print "No config\n";};
	$fark->setRerunVariable("rid");
	$fark->setRerunValue("0");
	$fark->setRerunOffset("0");
    }
};	
