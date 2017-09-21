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
$XML::LibXML::skipXMLDeclaration = 1;
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
#print "Content-type: text/xml;\n\n";
#
my $doc = $parser->parse_string("<expression><result></result></expression>");
my ($node) = $doc->findnodes("expression/result");
my $res="";
my $log="";
eval {
    $log=capture {
    my $fark=fark->open();
    $res=$fark->expression($param->{exp}[0]);
    $fark->close();
    };
};
my $ret=$@; if ($ret) {farkdir::term($ret . $log);}
#print "Res:$res\n";
$node->setAttribute("value",$res);
#
# report xml-structure
print $doc->toString . "\n";
