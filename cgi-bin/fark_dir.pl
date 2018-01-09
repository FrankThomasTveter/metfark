#!/usr/bin/perl -w
#
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use Capture::Tiny 'capture';
use POSIX 'strftime';
use Data::Dumper;
use farkdir;
use URI::Encode;
#
# Syntax:
#  ./fark_dir.pl '&cmd=mv;cls=obs;path=/elysium/metfark/obs/dest;dest=/elysium/metfark/obs/dest2;'
#
# Valid parameters:
#  cmd= mk ls cp rm mv
#  cls= data obs mod coloc plot
#
# Only directories within valid domains are visible to user.
# Available domains (and permissions) are given below:
#
#
my $debug=0;
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
my $pub="/metfark/pub";
my $lockDir="$pub/lock"; farkdir::makePath($lockDir);
my $ref=CGI->new();
#
$XML::LibXML::skipXMLDeclaration = 1;
my $param= $ref->{param};
if (! defined $param->{cmd}) {farkdir::term("Undefined cmd.".Dumper($param))};
if ($param->{cmd}->[0] eq "ls") {
    my $dir=$param->{path}->[0]||"";
    &cmdls($param,$dir);
} elsif ($param->{cmd}->[0] eq "mk") {
    my $dir=$param->{path}->[0]||"";
    &cmdmk($param, $dir);
} elsif ($param->{cmd}->[0] eq "rm") {
    my $dir=$param->{path}->[0]||"";
    &cmdrm($param, $dir);
} elsif ($param->{cmd}->[0] eq "rf") {
    my $dir=$param->{path}->[0]||"";
    &cmdrf($param, $dir);
} else {
    farkdir::term("Unknown cmd.".Dumper($param));
}

sub cmdls {
    my $param =shift;
    my $idir = shift;
    my $cls=$param->{cls}->[0] || "";
    my $filter=$param->{filter}->[0] || "";
    # make a new document
    my $doc = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $parent = XML::LibXML::Element->new( 'ls' );
    my $roots = 0;
    if ($idir eq "") { # only list valid root directories
	$roots=1;
    } else {
	my ($root, $loc, $priv) = farkdir::splitDir( $idir, $cls );
	my $fpath=$root . $loc;
	#print "Path '$root' '$loc' '$priv' '$idir' '$fpath'\n";
	if ($priv eq "rw" || $priv eq "ro") {
	    $parent->setAttribute("path",$fpath);
	    $parent->setAttribute("root",$root);
	    $parent->setAttribute("location",$loc);
	    $parent->setAttribute("status",$priv);
	    $doc->addChild( $parent );
	    if (-d $fpath && opendir (DIR, $fpath)) {
		my @entries = sort { $a cmp $b } readdir(DIR);
		my @files;
		while (my $name = shift @entries) {
		    next if (substr($name,0,1) eq ".");
		    #print "Processing: $name '". substr($name,0,1) . "'\n";
		    my $path=$fpath . $name;
		    if (-d $path) {
			my $d = XML::LibXML::Element->new( 'dir' );
			$d->setAttribute("path",$path);
			$parent->addChild( $d );
		    } elsif (-f $path) {
			if (! $filter || $name =~ m/$filter/ ) {
			    my $size = size_in_mb(-s $path);
			    my $f = XML::LibXML::Element->new( 'file' );
			    $f->setAttribute("path",$path);
			    $f->setAttribute("size",$size);
			    $parent->addChild( $f );
			    push(@files,$name);
			}
		    }
		}
		closedir(DIR);
		# list patterns and regexp
		my %patterns = &findPattern(@files);
		foreach my $patt (sort keys %patterns) {
		    my $f = XML::LibXML::Element->new( 'pattern' );
		    $f->setAttribute("struct",$patt);
		    $f->setAttribute("regexp",$patterns{$patt});
		    $parent->addChild( $f );
		} 
	    } else {
		farkdir::term("Unable to open directory.".Dumper($param));
		# my $d = XML::LibXML::Element->new( 'error' );
		# $d->setAttribute("message","Unable to open directory.");
		# $parent->addChild( $d );
	    }
	} else {
	    farkdir::term("Permission denied ($idir,$priv).".Dumper($param));
	    # my $d = XML::LibXML::Element->new( 'error' );
	    # $d->setAttribute("message","Permission denied.");
	    # $parent->addChild( $d );
	};
    };
    if ($roots) {  # only list valid root directories
	#print "Printing roots.\n";
	$parent->setAttribute("path","");
	$parent->setAttribute("root","");
	$parent->setAttribute("location","");
	$parent->setAttribute("status","");
	$doc->addChild( $parent );
	foreach my $k (keys %{$farkdirs{$cls}}) {
	    my $d = XML::LibXML::Element->new( 'dir' );
	    $d->setAttribute("path",$k);
	    $parent->addChild( $d );
	};
    }
    print $doc->toString . "\n";
};


# make directory
sub cmdmk {
    my $param =shift;
    my $idir = shift;
    my $cls=$param->{cls}->[0] || "";
    my $filter=$param->{filter}->[0] || "";
    # make a new document
    my $doc = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $parent = XML::LibXML::Element->new( 'mk' );
    my ($dir, $ndir) = farkdir::splitName($idir);
    if ($ndir ne "") {
	my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
	my $fpath=$root . $loc;
	if (-d  $fpath && $priv eq "rw") {
	    my $npath=$fpath. $ndir;
	    #print "\n\n$fpath => $npath\n\n";
	    $parent->setAttribute("path",$fpath);
	    $doc->addChild( $parent );
	    my $ret=farkdir::makePath($npath); # make directory
	    if (! $ret) {
		farkdir::term("Unable to make directory. $ret".Dumper($param));
		# my $e = XML::LibXML::Element->new( 'error' );
		# $e->setAttribute("message","Unable to make directory.");
		# $parent->addChild( $e );
	    } else {
		$parent->setAttribute("new",$npath);
		if (opendir (DIR, $fpath)) {
		    my @entries = sort { $a cmp $b } readdir(DIR);
		    while (my $name = shift @entries) {
			next if ($name eq "." || $name eq "..");
			my $path=$fpath . $name;
			if (-d $path) {
			    my $d = XML::LibXML::Element->new( 'dir' );
			    $d->setAttribute("path",$npath);
			    $parent->addChild( $d );
			} elsif (-f $path) {
			    if (! $filter || $name =~ m/$filter/ ) {
				my $size = size_in_mb(-s $path);
				my $f = XML::LibXML::Element->new( 'file' );
				$f->setAttribute("path",$npath);
				$f->setAttribute("size",$size);
				$parent->addChild( $f );
			    }
			}
		    }
		    closedir(DIR);
		} else {
		    farkdir::term("Unable to open directory $fpath.".Dumper($param));
		    # my $d = XML::LibXML::Element->new( 'error' );
		    # $d->setAttribute("message","Unable to open directory.");
		    # $parent->addChild( $d );
		}
	    };
	} else {
	    farkdir::term("Permission denied ($cls,$fpath,$priv).".Dumper($param));
	    # my $e = XML::LibXML::Element->new( 'error' );
	    # $e->setAttribute("message","Permission denied.");
	    # $parent->addChild( $e );
	}
    } else {
	farkdir::term("Invalid mk directory ($idir).".Dumper($param));
	# my $e = XML::LibXML::Element->new( 'error' );
	# $e->setAttribute("message","Invalid directory.");
	# $parent->addChild( $e );
    };
    print $doc->toString . "\n";
};

# remove directory (with matching password)
sub cmdrm {
    my $param =shift;
    my $idir = shift;
    my $cls=$param->{cls}->[0] || "";
    my $password=($param->{password}[0] // "");
    # make a new document
    my $doc = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $parent = XML::LibXML::Element->new( 'rm' );
    my ($root, $loc, $priv) = farkdir::splitDir( $idir, $cls );
    if ($root ne "" && $loc eq "") { $priv="ro";};
    my $fpath=$root . $loc;
    if (-d $fpath) {
	#print "\n\n$fpath => $root $loc\n\n";
	$parent->setAttribute("path",$fpath);
	$parent->setAttribute("root",$root);
	$parent->setAttribute("location",$loc);
	$parent->setAttribute("status",$priv);
	$doc->addChild( $parent );
	if ($priv eq "rw") {
	    #print "Priv: $priv, '$fpath' $cls '$password'\n";
	    my $ret=farkdir::removeDir($fpath,$password);
	    if ($ret) {
		farkdir::term("Unable to remove directory.".Dumper($param));
		# my $e = XML::LibXML::Element->new( 'error' );
		# $e->setAttribute("message","Unable to remove directory.");
		# $parent->addChild( $e );
	    } else {
		$parent->setAttribute("removed",$fpath);
	    };
	} else {
	    farkdir::term("Permission denied ($fpath,$priv).".Dumper($param));
	    # my $e = XML::LibXML::Element->new( 'error' );
	    # $e->setAttribute("message","Permission denied.");
	    # $parent->addChild( $e );
	}
    } else {
	farkdir::term("Invalid rm directory ($fpath).".Dumper($param));
	# my $e = XML::LibXML::Element->new( 'error' );
	# $e->setAttribute("message","Invalid directory.");
	# $parent->addChild( $e );
    };
    print $doc->toString . "\n";
};

sub cmdrf {
    my $param =shift;
    my $ipath = shift; # full path
    my ($idir, $iname) = farkdir::splitName($ipath);
    my $cls=$param->{cls}->[0] || "";
    my $password=($param->{password}[0] // "");
    # make a new document
    my $doc = XML::LibXML::Document->new( '1.0', 'UTF-8' );
    my $parent = XML::LibXML::Element->new( 'rm' );
    my ($root, $loc, $priv) = farkdir::splitDir( $idir, $cls );
    my $fpath=$root . $loc;
    if (-d $fpath) {
	#print "Path: $fpath $idir $ipath\n";
	$parent->setAttribute("path",$fpath);
	$parent->setAttribute("root",$root);
	$parent->setAttribute("location",$loc);
	$parent->setAttribute("status",$priv);
	$doc->addChild( $parent );
	my $path=$fpath . $iname;
	if ($priv eq "rw") {
	    if (farkdir::removeFile($path,$password)) {
		$parent->setAttribute("removed",$path);
	    } else {
		farkdir::term("Unable to remove file $path.".Dumper($param));
		# my $e = XML::LibXML::Element->new( 'error' );
		# $e->setAttribute("message","Unable to remove directory.");
		# $parent->addChild( $e );
	    };
	} else {
	    farkdir::term("Permission denied ($fpath,$priv).".Dumper($param));
	    # my $e = XML::LibXML::Element->new( 'error' );
	    # $e->setAttribute("message","Permission denied.");
	    # $parent->addChild( $e );
	}
    } else {
	farkdir::term("Invalid rf directory ($fpath).".Dumper($param));
	# my $e = XML::LibXML::Element->new( 'error' );
	# $e->setAttribute("message","Invalid directory.");
	# $parent->addChild( $e );
    };
    print $doc->toString . "\n";
};

sub findPattern {
    use strict;
    use Data::Dumper;
    my @files= @_;
    my %patterns;
    # divide into groups with same character settings
    my %ptree;
    my $uri = URI::Encode->new( { encode_reserved => 0 } );
    foreach my $name (@files) {
	my $p = $name;
	$p =~ s/\d/\\d/g;
	if (! defined $ptree{$p}) {$ptree{$p}=();};
	push (@{$ptree{$p}},$name);
    }
    foreach my $p (keys %ptree) {
	my %lim=();
	my %pptree;
	my $len =@{$ptree{$p}};
	#print "Found '$p'\n";
	my $m=$p;
	$m =~ s/\\d/@/g;
	my $lenm=length($m);
	# loop over number-groups...
	GROUP: while ($m =~ m/(@+)/g) { # identify before-group-after
	    my $p1=substr($m,0,$-[0]);
	    my $p2=$1; # group
	    my $p3=substr($m,$+[0],$lenm-$+[0]);
	    my $offset=$-[0];
	    #print "Match '$p1' '$p2' '$p3'\n"; next GROUP;
	    $p1 =~ s/@/\\d/g;
	    $p2 =~ s/@/\\d/g;
	    $p3 =~ s/@/\\d/g;
	    my $pp="$p1($p2)$p3";
	    #print "   found sub-pattern: '$pp' '$p'\n";
	    if (! defined $pptree{$p})      {$pptree{$p}={};};
	    if (! defined $pptree{$p}{$pp}) {$pptree{$p}{$pp}=();};
	    # loop over all instances
	    foreach my $name (@{$ptree{$p}}) {
		#print "Found: '$name'\n";
		if ($name =~ m/$pp/) { # instances with number group
		    my $cc=$1; # number group
		    #print "Found item: '$pp' '$cc'\n";
		    # loop over characters in number group...
		    my $pos=0;
		    # check 4-digits
		    &findLimits($cc,4,$offset,\%lim);
		    &findLimits($cc,3,$offset,\%lim);
		    &findLimits($cc,2,$offset,\%lim);
		    &findLimits($cc,1,$offset,\%lim);
		}
	    }	
	}
	# best estimates
	my $byyyy;
	my $bmm;
	my $bdd;
	my $bhh;
	my $bmi;
	# find 4-digit year
	my $cyyyy = &getCandidates($lim{4},0,$lenm-3,1900,2100,"YYYY"); # year candidates
	my $cmm =   &getCandidates($lim{2},0,$lenm-1,0,12,"MM");        # month  candidates
	my $cdd =   &getCandidates($lim{2},0,$lenm-1,1,31,"DD");        # day candidates
	my $chh =   &getCandidates($lim{2},0,$lenm-1,0,24,"HH");        # hour candidates
	my $cmi =   &getCandidates($lim{2},0,$lenm-1,0,60,"MI");        # minutes candidates
	my $ccc =   &getCandidates($lim{2},0,$lenm-1,0,24,"CC");        # cycle candidates
	my $cll =   &getCandidates($lim{3},0,$lenm-1,0,300,"LL");       # lead time candidates
	my $cnn =   &getCandidates($lim{1},0,$lenm-1,0,9,"N");       # lead time candidates
	my $s=$m;
	$s  =~ s/@/ /g; # make empty string
	my $w=$s;
        $w  =~ s/\S/@/g; # make empty string
	#print "Making patterns for '$p2'\n";
	my $patt = &showPattern($s,$w,$cnn,$cyyyy,$cmm,$cdd,$chh,$cmi,$ccc,$cll);
	$m =~ s/@/\\d/g;
	if ($m ne $patt) {$patterns{$uri->encode($patt)} = $uri->encode($m);};
    }
    return %patterns;
}

sub findLimits {
    use strict;
    my ($cc,$dlen,$offset,$lim)=@_;
    if (! defined $lim->{$dlen}){ $lim->{$dlen}=();};
    my $clen=length($cc);
    for (my $ii=0;$ii < $clen-$dlen+1;$ii++) {
	my $ss=substr $cc,$ii,$dlen;
	if (! defined $lim->{$dlen}[$ii+$offset]){
	    $lim->{$dlen}[$ii+$offset]=[$ss+0,$ss+0,$dlen,$ss+0];
	} else {
	    $lim->{$dlen}[$ii+$offset][0]=min($lim->{$dlen}[$ii+$offset][0],$ss+0);
	    $lim->{$dlen}[$ii+$offset][1]=max($lim->{$dlen}[$ii+$offset][1],$ss+0);
	    $lim->{$dlen}[$ii+$offset][3]=mod($lim->{$dlen}[$ii+$offset][3],$ss+0);
	};
    }
}    

sub getCandidates {
    use strict;
    my ($lim,$istart,$istop,$rmin,$rmax,$label)= @_;
    #print "Looking for candidates $rmin, $rmax\n";
    my @ret=();
    foreach my $ii (keys @{$lim}) {
	my $min=$lim->[$ii][0];
	my $max=$lim->[$ii][1];
	my $dlen=$lim->[$ii][2];
	my $delta=$lim->[$ii][3];
	#if (defined $min) { print " Range $ii, $min, $max, $dlen\n";};
	if (defined $min && $min >= $rmin && $max <= $rmax) {
	    my $ratio=0;
	    if ($rmax>$rmin) {
		$ratio=($max-$min)/($rmax-$rmin);
	    };
	    #print "   found candidate at $ii\n";
	    my $score=(2 - 6*$ratio + 8*$ratio*$ratio)*$dlen;
	    my $step=(($delta-1)/max(1,$rmax))*$dlen*8;
	    #print "   found candidate $score, $step, $delta, $rmax\n";
	    my @res = ($ii,$ii+$dlen-1,$label,$score,$step,$min,$max);
	    push (@ret,\@res);
	}
    }
    return \@ret;
}

sub showPattern {
    use strict;
    use Data::Dumper;
    use POSIX;
    my ($s,$w,$cnn,@candidateTypes)= @_;
    #print "showPattern Entering with '$s' '$w'\n";
    my $bdone=0;
    while (!$bdone) {   # loop while changed
	# find candidate with highest score...
	my $maxss=0;
	my $maxns;
	my $maxnb;
	#print "Bdone loop '$s' '$w'\n";
	foreach my $candidates (@candidateTypes) {  # loop over types
	    #print "  Type loop '$s' '$w'\n";
	    foreach my $candidate (@{$candidates}) { # loop over candidates
		my $istart=$candidate->[0];
		if (defined $istart) {
		    my $istop=$candidate->[1];
		    my $label=$candidate->[2];
		    my $score=$candidate->[3];
		    my $delta=$candidate->[4];
		    my $min=$candidate->[5];
		    my $max=$candidate->[6];
		    #print "    Candidate loop '$s' '$w' '$label' $score $delta\n";
		    if (my ($ns,$nb)=&addPattern($s,$w,$istart,$istop,$label)) { # check if pattern is possible
			my $ss=&getScore($ns,$nb,$score);
			#if ($label eq "CC") {
			#    $ss=$ss+$delta;
			#};
			if ($ss > $maxss) { # store candidate with highest score
			    $maxns=$ns;
			    $maxnb=$nb;
			    $maxss=$ss;
			};
		    } else {
			$candidate=[];
		    };
		};
	    };
	}   # end candidate loop
	# use candidate with highest score (if any)...
	if ($maxss) {
	    $s=$maxns;
	    $w=$maxnb;
	    $maxss=floor($maxss*100)/100;
	    if ($debug) {print "######## '$s' $maxss ### WINNER ###\n";}
	} else {
	    $bdone=1;
	}
    }; # end change loop
    # replace any remaining characters with their
    foreach my $candidate (@{$cnn}) { # loop over candidates
	my $istart=$candidate->[0];
	if (defined $istart) {
	    my $istop=$candidate->[1];
	    my $label=$candidate->[2];
	    my $min=$candidate->[5];
	    my $max=$candidate->[6];
	    if ($min == $max) {$label="$min"};
	    #print "    Candidate loop '$s' '$w' '$label' $score $delta\n";
	    if (my ($ns,$nb)=&addPattern($s,$w,$istart,$istop,$label)) { # check if pattern is possible
		$s=$ns;
		$w=$nb;
	    };
	};
    };
    #print "showPattern Exiting\n";
    return $s;
}

sub addPattern {
    use strict;
    my ($s,$w,$istart,$istop,$label) = @_;
    #print "addPattern $s $istart $istop\n";
    my $ss=substr($s,$istart,$istop-$istart+1)//"";
    if (&blank($ss)) { # blank
	my $len=length($s);
	my $s1=substr($s,0,max(0,$istart))//"";
	my $s2=substr($s,$istop+1,$len-$istop)//"";
	my $w1=substr($w,0,max(0,$istart))//"";
	my $w2=substr($w,$istop+1,$len-$istop)//"";
	my $patt = $s1 . $label . $s2;
	my $watt = $w1 . $label . $w2;
	#print "****'$s'->'$s1'+'$label'+'$s2' $istart $istop $len ".length($patt)."\n";
	#print "addPattern '$s' -> '$patt'\n";
	return ($patt,$watt);
    } else {
	return;
    }
}

sub getScore {
    my ($s,$w,$score)=@_;
    # adjacent
    if ($debug) {my $s2=floor($score*100)/100;
		 print "getScore '$s' $s2";}
    if ($w =~ m/^@*HH@*$/) {$score+=5;};
    while ($w =~ m/YYYY@*MM/g) {$score+=3;};
    while ($w =~ m/YYYY@*MM@*DD/g) {$score+=3;};
    while ($w =~ m/MM@*DD/g) {$score+=3;};
    while ($w =~ m/DD@*HH/g) {$score+=2;};
    while ($w =~ m/HH@*MI/g) {$score+=2;};
    #
    if ($debug) {my $s2=floor($score*100)/100;
		 print " -> $s2\n";}
    return $score;
}

sub blank {
    use strict;
    my $s = shift;
    if (defined $s) {
	if ($s =~ m/\S/) {
	    return 0;
	} else {
	    return 1;
	}
    } else {
	return 1;
    }
}
sub min {
    use strict;
    my ($min, @vars) = @_;
    for (@vars) {
	if (defined $min) {
	    $min = $_ if $_ < $min;
	} else {
	    $min = $_;
	}
    }
    return $min;
}
sub max {
    use strict;
    my ($max, @vars) = @_;
    for (@vars) {
        $max = $_ if $_ > $max;
    }
    return $max;
}
sub mod {
    use strict;
    my ($mod, @vars) = @_;
    for (@vars) {
	my $nd=$mod;
	if ($mod > 0) {
	    while ($nd > 0 && $_%$nd!=0 && $mod%$nd !=0) { $nd--; }
	    $mod=$nd;
	} else {
	    $mod=$_+0;
	}
    }
    return $mod;
}

sub size_in_mb {
    my $size=shift;
    my $text= reverse (sprintf "%.2fMb", $size/(1024 * 1024));
    $text =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
    $text=reverse $text;
    return $text;
}
