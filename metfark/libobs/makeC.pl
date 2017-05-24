#!/usr/bin/perl
#
#
#
 use strict;
#
my $filename="tableC.txt";
#
open(my $fh, '<:encoding(iso-8859-1)', $filename)
  or die "Could not open file '$filename' $!";
 
my $cnt=0;
while (my $row = <$fh>) {
  chomp $row;
  if ($row =~ /(\d{6})\s+(\d{4})\s+(\d{8})\s+(\d{2})\s(.*)$/) {
      my $code = $1;
      my $scnt = $2;
      my $subcode = $3;
      my $lcnt = $4;
      my $value=$5 . getRest($fh,$4-1);
      if (length($value)>80) {
	  $value=substr($value,0,75)."...";
      }
      $cnt=$cnt+1;
      print "
 ctable\%codes($cnt) = $code;
 allocate(ctable\%tables($cnt),stat=irc);
 ctable\%index($cnt) = $cnt;
 ctable\%tables($cnt)\%code=$code;
 ctable\%tables($cnt)\%maxnn=$scnt;
 ctable\%tables($cnt)\%nn=$scnt;
 allocate(ctable\%tables($cnt)\%subcodes($scnt),ctable\%tables($cnt)\%values($scnt),ctable\%tables($cnt)\%index($scnt),stat=irc);
 ctable\%tables($cnt)\%subcodes(1)=$subcode;
 ctable\%tables($cnt)\%values(1)=\"$value\";
 ctable\%tables($cnt)\%index(1)=1;";
      for (my $pos = 2; $pos <= $scnt; $pos++) {
	  my $row = <$fh>;
	  if ($row =~ /\s+(\d{8})\s+(\d{2})\s(.*)$/) {
	      $subcode=$1;
	      $value=$3 . getRest($fh, $2-1); 
	      if (length($value)>80) {
		  $value=substr($value,0,75)."...";
	      }
	      print "
 ctable\%tables($cnt)\%subcodes($pos)=$subcode;
 ctable\%tables($cnt)\%values($pos)=\"$value\";
 ctable\%tables($cnt)\%index($pos)=$pos;";
	  }
      }
  }
}
close ($fh);
print "
 ctable\%maxnn=$cnt;
 ctable\%nn=$cnt;
 allocate(ctable\%codes($cnt),ctable\%tables($cnt),ctable\%index($cnt),stat=irc);
";

sub getRest {
    my ($fh,$lcnt) = @_;
    my $ret="";
    while ($lcnt > 0) {
	my $row = <$fh>;
	chomp $row;
	if ($row =~ /^\s{22}(.*)\s*$/) {
	    $ret=$ret . $1;
	} else {
	    $ret=$ret . $row;
	}
	$lcnt = $lcnt-1;
    }
    return $ret;
}
