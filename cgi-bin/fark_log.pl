#!/usr/bin/perl -w
#
use ExtUtils::testlib;
use fark;
use farkdata;
use farkdir;
#
use strict;
use CGI;
#use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use Data::Dumper;
use POSIX 'strftime';
use Capture::Tiny 'capture_merged';
use File::Basename;
use File::Compare;
use File::Copy;
#
my $ref=CGI->new();
my $param    = $ref->{param};
#
print "Content-type: text/plain;\n\n";
#
my $debug=0;
if (defined $param->{debug}[0]) {
    $debug=1;       # debug this script (0=omit output)
}
#
my $myname = basename($0);
#
my $ipath    = $param->{file}[0] // "";
my $cls      = $param->{type}[0] // "undef";
my ($dir,$name)=farkdir::splitName($ipath);
my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
if ($priv ne "rw") {
    printStrong("$ipath   -- Permission denied --","!");
} else {
    my $file = $loc . $name;
    my $test = (defined $param->{test}[0]) ? 1 : 0;
    #
    if ($debug) {print "Argument='" . shift . "'\n";}
    if ($debug) {print "Processing $file\n";}
    #
    printType($cls,$file);
}
sub printType{
    use File::stat;
    my $ce = "#";
    my $co = "=";
    my $logstat;
    my $errstat;
    my $cls  = shift//"";
    my $file = shift//"";
    my $logcls=$cls."_log";
    my $clsLogDir=  farkdir::getRootDir($logcls) || 
	farkdir::term("Invalid root directory (".$cls."_log)");
    my $logfile = $clsLogDir ."$file.log";
    my $errfile = $clsLogDir ."$file.log.err";
    if (-e $logfile && -e $errfile) {
	$logstat = stat($logfile);
	$errstat = stat($errfile);
	if ($logstat->mtime > $errstat->mtime) { # log is youngest
	    printFile($logfile,$co);
	    printFile($errfile,$ce);
	} else {
	    printFile($errfile,$ce);
	    printFile($logfile,$co);
	}
    } elsif (-e $errfile) {
	printFile($errfile,$ce);
	printFile($logfile,$co);
    } else {
	printFile($logfile,$co);
	printFile($errfile,$ce);
    }
}

sub permissionDenied {
    my $logfile = shift;
    my $logcls = shift;
    my ($fpath, $name) = farkdir::splitName($logfile);
    my ($root, $loc, $priv) = farkdir::splitDir( $fpath, $logcls);
    if ($priv ne "rw") {
	print "Path: $fpath $logcls\n";
	print "splitDir Return: $fpath -> $root $loc $priv\n";
	printStrong("${logfile}   -- Permission denied --","!");
	return 1;
    } else {
	return;
    };
}

sub printFile {
    use POSIX;
    my $file = shift;
    my $str = shift;
    my $time=time();
    if (-e $file) {
	my $stat = stat($file);
	my $mtime = gmtime($stat->mtime);
	my $days;
	my $hours;
	my $minutes;
	my $age= $time  - $stat->mtime;
	my $s="";
	if ($age > 86400) {
	    $days=floor($age/86400);
	    $age=$age-$days*86400;
	    if ($s) {$s=$s . " "};
	    $s=$s . "${days}d";
	};
	if ($age > 3600) {
	    $hours=floor($age/3600);
	    $age=$age-$hours*3600;
	    if ($s) {$s=$s . " "};
	    $s=$s . "${hours}h";
	};
	if ($age > 60) {
	    $minutes=floor($age/60);
	    $age=$age-$minutes*60;
	    if ($s) {$s=$s . " "};
	    $s=$s . "${minutes}m";
	};
	if ($s) {$s="(".$s.")"};
	my $size = $stat->size;
	printStrong("${file}   ${size}  ${mtime} ${s}",$str);
	open (FH, "<", $file);
	while (<FH>) {
	    print;
	}
	close(FH);
    } else {
	printStrong("${file}   -- No such file --",$str);
    }
    print "\n";
}

sub printStrong {
    my $line="==========================================================================================";
    my $s = shift;
    my $c = shift;
    my $l = length($s);
    $line = ($c x $l);
    print "${line}\n${s}\n${line}\n";
}
