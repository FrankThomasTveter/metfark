package farkdata;
use strict;
use warnings "all";
use Data::Dumper;
use POSIX;
use Storable qw(dclone);

my $bdeb = 0; # debug flag

=head2 makeBilinear

  "makeBilinear" adds bilinear slice information to the Hash.

  Return:

=over 4

=item The hash object passed to it.

=back

=head4 EXAMPLE

    $farkdata->makeBilinear();

=cut

sub makeBilinear {
    my ($sref) = @_;
    my $path={}; # parent path
    my $slice = $sref -> getKeyArrayRef("slice") || die "Unable to find slice.";
    my $sliceDims = $sref -> getBranch({"slice"=>"dimension"})|| die "Unable to find slice=>dimension.";
    my @unsortedDimIndex = $sliceDims->getKeys({"slice" => "dimension"});
    # get reference to "slice" hash
    my @dimIndex = sort { $a <=> $b } @unsortedDimIndex;
    &makeBilinear_($sref,$sliceDims,$slice,\@dimIndex,$path);
    return $sref;
}

sub makeBilinear_ {
    my ($sref, $sliceDims, $slice, $dimIndexRef, $path)=@_;
    #print "makeBilinear Entering @$dimIndexRef\n";
    # make copy of dimension index
    if (@$dimIndexRef) {
	my @dimIndexCopy=@$dimIndexRef;
	my $slicecopy=$slice;
	my $dimIndex=shift @dimIndexCopy;
	#print "makeBilinear Processing dim-index: $dimIndex\n";
	my $indexBranch= getBranch($sliceDims, {"dimension" => $dimIndex});
	#print Dumper($indexBranch);
	my @dimensions= $indexBranch->getKeys({"dimension" => $dimIndex});
	if (scalar @dimensions != 1) {die "Strange dimensions for $dimIndex: @dimensions\n";}
	my $dimension = $dimensions[0];
	my @dimrefs=$indexBranch->getKeyArrayRef("dimension", $dimIndex, $dimension);
	if (scalar @dimrefs != 1) {die "Strange ref for $dimIndex\n";}
	my $dimref=$dimrefs[0];
	#print "makeBilinear Processing dimension: $dimension $dimIndex\n";
	my @variables=getKeys($indexBranch,{"dimension" => $dimIndex, $dimension => "variable"});
	if (scalar @variables > 1)  {die "Strange variable for $dimIndex => $dimension\n";}
	if (scalar @variables == 1) {
	    my $variable = $variables[0];
	    #print "makeBilinear   ---> variable: $variable\n";
	    # get target
	    my @targets=getKeys($indexBranch,{"dimension" => $dimIndex, $dimension => "variable",$variable=>"target"});	    
	    if (scalar @targets != 1) {die "Strange targets for  $dimIndex => $dimension => $variable\n";}
	    my $target=$targets[0];
	    $slice -> {"bilinear"} {$dimension} {"target"} = $target;
	    #print "makeBilinear Processing variable: $variable => $target\n";
	    # get ileft and iright values...
	    my $variableBranch = $sref -> getBranch({"variable" => $variable}); # copy branch
	    my $pathcopy = dclone($path);
	    #print Dumper($pathcopy);
	    my ($subVariableBranch,@jnk2)=$variableBranch -> getBranch_($pathcopy);
	    bless($subVariableBranch,"farkdata"); # we need to do this since getBranch_ is internal...
	    if($bdeb){$subVariableBranch->printTree("SubVarBranch: ");}
	    my @positions=$subVariableBranch->getKeys({"variable"=>$variable, $dimension => ""});
	    my %gridvalue =();
	    my %factor =();
	    if (scalar @positions ==0 ) {die "Unable to find any data for '$variable' (dimension='$dimension' -> ".scalar @positions.")";}
	    if (scalar @positions > 2 ) {die "Strange number of positions for $variable => $dimension (".scalar @positions.")";}
	    my $oldpos=0;
	    foreach my $pos (@positions) {
		#print "Processing $pos\n";
		my @varval=$subVariableBranch -> getValues({$dimension => $pos});
		if (scalar @varval != 1 and scalar @varval != 2 ) {
	     	    #print "makeBilinear Strange data structure for $variable => $dimension \n";
	     	} else {
		    $gridvalue{$pos} = $varval[0];
	     	    if ($oldpos) {
	     		$factor{$pos} = ($target-$gridvalue{$oldpos})/($gridvalue{$pos}-$gridvalue{$oldpos});
	     		$factor{$oldpos} = 1-$factor{$pos};
	     	    } else {
	     		$factor{$pos} = 1;
	     		$oldpos=$pos;
	     	    }
	     	}
	    }
	    foreach my $pos (keys %factor) {
		my $subslice={};
		$slice -> {"bilinear"} {$dimension} {$pos} = $subslice;
		$slice -> {"bilinear"} {$dimension} {$pos} {"factor"} = $factor{$pos};
		$slice -> {"bilinear"} {$dimension} {$pos} {"value"} = $gridvalue{$pos};
		$path -> {$dimension} = $pos;
		&makeBilinear_($sref,$sliceDims,$subslice,\@dimIndexCopy,$path); 
		delete ($path -> {$dimension});
	    }
	} else { # this is a dimension
	    #print "makeBilinear   ---> dimension.\n";
	    my @posReals=getKeys($indexBranch,{"dimension" => $dimIndex, $dimension => "target"});
	    if (scalar @posReals != 1) {die "Strange dimension target for $dimIndex => $dimension";}
	    my $posReal=$posReals[0];
	    $slice -> {"bilinear"} {$dimension} {"target"} = $posReal;
	    my %factor =();
	    if (floor($posReal) == ceil($posReal)) {
		my $arg=int($posReal+0.5);
		$factor{$arg} = 1;
	    } else {
		my $arg=floor($posReal);
		$factor{$arg} = ceil($posReal)-$posReal;
		$arg=ceil($posReal);
		$factor{$arg} = $posReal-floor($posReal);
	    }
	    foreach my $pos (keys %factor) {
		my $subslice={};
		$slice -> {"bilinear"} {$dimension} {$pos} = $subslice;
		$slice -> {"bilinear"} {$dimension} {$pos} {"factor"} = $factor{$pos};
		$slice -> {"bilinear"} {$dimension} {$pos} {"value"} = $pos;
		$path -> {$dimension} = $pos;
		&makeBilinear_($sref,$sliceDims,$subslice,\@dimIndexCopy,$path); 
		delete ($path -> {$dimension});
	    }
	}
    }
    #print "makeBilinear Done.\n";
    return $sref;
}

=head2 interpolate

  "interpolate" uses the specified method to interpolate all variables.

  Arguments:

=over 4

=item (string) names of interpolation method. The method slicing coefficients must
               be calculated and present in the "slice" branch.

=back


  Return:

=over 4

=item The hash object passed to it.

=back

=head4 EXAMPLE

    $farkdata->interpolate();

=cut


sub interpolate {
    my ($sref,$interpolatorType) = @_;
    my $interpolatorRef = $sref->getBranch({"slice" => $interpolatorType});
    my @variables=getKeys($sref,{"variable"=>""});
    foreach my $variable (@variables) {
	if($bdeb){print "interpolate found variable: $variable\n";}
	my $variableRef=getKeyRef($sref,{"variable"=>$variable});
	if($bdeb){print "interpolate >>> variable: '$variable', interpolator: '$interpolatorType'\n";}
	if($bdeb){print "interpolate >>> variableref: ". Dumper($variableRef);}
	# find matching top variable/interpolator dimensions ("time")
	if (my $value = &interpolateVariable_($variableRef, $interpolatorRef, $interpolatorType)) {
	    if($bdeb){print "interpolate Returned from interpolateVariable with $value\n";}
	    $variableRef->{$interpolatorType}=$value;
	}
    }
    if (ref($sref) eq "HASH") {bless($sref,"farkdata");}
    return  $sref;
}

sub interpolateVariable_{
    my ($variableRef, $interpolatorRef, $interpolatorType) = @_;
    # $variableRef must point to hash containing $dimension,
    if (ref($variableRef) eq "HASH" or ref($variableRef) eq "farkdata") {
	my @dimensions=keys %$variableRef; if (scalar @dimensions != 1) {die "Strange data structure.";}
	my $dimension= $dimensions[0];
	if (my $ret=getKeyRef($interpolatorRef,{$interpolatorType => $dimension})) {
	    if($bdeb){print "interpolateVariable >>> matched $dimension\n";}
	    # we have a match...
	    $interpolatorRef=$ret;
	    my $sumValue=0.0;
	    my $sumWeight=0.0;
	    my $cnt=0;
	    my @positions = keys %$interpolatorRef;
	    foreach my $pos (@positions) {
		if ($pos =~ /^\d+$/) {
		    if($bdeb){print "interpolateVariable Processing $dimension->$pos\n";}
		    my $weight= $interpolatorRef->{$pos};
		    my $value= $variableRef->{$dimension} {$pos};
		    if (not $value) {$value="";}
		    if (ref($weight) eq "HASH" or ref($weight) eq "farkdata") {
			if($bdeb){print "interpolateVariable Is parent: $dimension->$pos\n";}
			my $weight=$weight->{"factor"};
			if ($value=&interpolateVariable_($variableRef->{$dimension}{$pos}, $interpolatorRef->{$pos}, $interpolatorType)) {
			    if (ref($variableRef->{$dimension}{$pos}) eq "HASH" or ref($variableRef->{$dimension}{$pos}) eq "farkdata") {
				$variableRef->{$dimension}{$pos}{$interpolatorType}= $value;
			    }
			    $cnt = $cnt + 1;
			    $sumWeight = $sumWeight + $weight;
			    $sumValue = $sumValue + ($weight * $value);
			    if($bdeb){print "interpolateVariable Returned value: $value $weight\n";}
			} else {
			    if($bdeb){print "interpolateVariable Failed\n";}
			    return;
			}
		    } else {
			print "Dim:$dimension  Pos:$pos  Value:$value\n";
			$cnt = $cnt + 1;
			$sumWeight = $sumWeight + $weight;
			$sumValue = $sumValue + ($weight * $value);
			if($bdeb){print "interpolateVariable Got value: $value $weight\n";}
		    }
		}
	    }
	    if ($sumWeight > 0) {
		if($bdeb){print "interpolateVariable +++++++ Sum: $cnt $sumValue $sumWeight\n";}
		return $sumValue/$sumWeight;
	    }
	} else {
	    if($bdeb){print "interpolateVariable No interpolator for $interpolatorType => $dimension\n"; printTree($interpolatorRef,"#NoMatch: ");}
	    # no match, process all positions
	    my @positions=getKeys($variableRef,{$dimension=>""});
	    foreach my $pos (@positions) {
		if (my $value = &interpolateVariable_($variableRef->{$dimension}{$pos}, $interpolatorRef, $interpolatorType)) {
		    if (ref($variableRef->{$dimension}{$pos}) eq "HASH" or ref($variableRef->{$dimension}{$pos}) eq "farkdata") {
			$variableRef->{$dimension}{$pos}{$interpolatorType} = $value;
		    }
		    if($bdeb){print "interpolateVariable Assigning value: $value\n";}
		} else {
		    if($bdeb){print "interpolateVariable Failed\n";}
		    return;
		}
	    }
	}
    } else { # "$variableRef" contains the value...
	if($bdeb){print "interpolateVariable >>> value: $variableRef\n";}
	return $variableRef;
    }
    if($bdeb){print "interpolateVariable Failed again\n"; printTree($variableRef,"#Failed: ");}
    return;
}

=head2 compress

  "compress" removes all hashes with a single entry.

  Return:

=over 4

=item The compressed hash object.

=back

=head4 EXAMPLE

    $farkdata->compress();

=cut

 sub compress {
     my ($sref)=@_;
     if ( ref($sref)eq"HASH" or ref($sref)eq"farkdata") {
 	my @keys=keys %$sref;
 	if (scalar @keys == 1){
 	    my $key = $keys[0];
 	    $sref = compress($sref->{$key});
 	} else {
 	    foreach my $key (@keys) {
 		$sref->{$key} = compress($sref->{$key});
 	    }
 	}
     }
     if (ref($sref)eq"HASH") { bless ($sref,"farkdata");}
     return $sref;
 }

=head2 selectKey

  "selectKey" returns Hash where only the specified key-value is selected if more
   were available, in which case the key itself is also eliminated.

  Arguments:

=over 4

=item (string) names of key to search for.

=back

  Return:

=over 4

=item The Hash object where only the specified key-value is selected if more
   were available, in which case the key itself is also eliminated.

=back

=head4 EXAMPLE

    my $select = $farkdata->selectKey("bilinear");

=cut

# select branch with key, eliminating key...
 sub selectKey {
     my ($sref, $gkey)=@_;
     if ( ref($sref)eq"HASH" or ref($sref)eq"farkdata") {
 	if (exists $sref->{$gkey}){
 	    $sref = &selectKey($sref->{$gkey},$gkey);
 	} else {
	    my @keys=keys %$sref;
 	    foreach my $key (@keys) {
 		$sref->{$key} = &selectKey($sref->{$key},$gkey);
 	    }
 	}
     }
     if (ref($sref)eq"HASH") { bless ($sref,"farkdata");}
     return $sref;
 }


=head2 printTree

  "printTree" prints a string-representation of the given hash-reference

  Arguments:

=over 4

=item (String) prefix

=backs

=head4 EXAMPLE

    my $farkdata->printTree("#Fark: ");

=cut

sub printTree{
    my ($self,$pre)=@_;
    print printTree_($pre,$pre,$self);
    return $self;
}

sub printTree_ {
    my $s="";
    my $pre=shift;
    my $pos=shift;
    my $cnt=0;
    foreach my $ref (@_){
	my $rid=ref($ref);
	my $cnt=0;
        if ($rid eq "ARRAY") {
	    my $lpre=$pre;
            foreach my $el (@$ref) { 
               my $lpos=$pos . " " x length($cnt++) . "   ";
               $s=$s . printTree_("$lpre"."[".$cnt."]=",$lpos,$el);
	       $lpre=$pos;
            }
	    $pre=$pos;
        } elsif ($rid eq "HASH" or $rid eq "farkdata") {
	    my $lpre=$pre;
	    if (keys %{$ref}) {
		foreach my $k (sort my_complex_sort keys %{$ref}) { # sort {$a <=> $b} 
		    my $v=$ref->{$k};
		    my $lpos=$pos . " " x length($k) . "  ";
		    $s=$s . printTree_("$lpre$k=>",$lpos,$v);
		    $lpre=$pos;
		}
	    } else {
		$s=$s . "\n";
	    }
	    $pre=$pos;
        } else {
            $s=$s . "$pre$ref $rid\n";
            $pre=$pos;
        }
    }
    return $s;
}



=head2 getBranch

  "getBranch" returns a copy of the branch with the specified key=>value pairs,
   starting at the tree-root.

  Arguments:

=over 4

=item (Hashref) key => value pairs that should be present in the resulting branch.

=back


  Return:

=over 4

=item The Hash reference to the branch containing the specified key=>value pairs.

=back

=head4 EXAMPLE

    $hashref -> getBranch({"variable" => "geopotential_height_ml"});

=cut
sub getBranch {
    my ($sref, $href) = @_;
    my ($sres,$hres)=getBranch_($sref,$href);
    if (keys(%$href)) {return;} # no getBranch returned since not all keys were matched...
    bless $sres => "farkdata";
    return $sres;
}

sub getBranch_ {
    my ($sref, $href) = @_;
    my $bref = {};
    my $type=ref($sref);
    #print "getBranch Entering with $type\n";
    if ($type ne "HASH" and $type ne "farkdata") { 
	$bref = $sref;
    } else {
	my $found=0;
	my $skey;
	my $svalue;
	my @keys=();
	@keys =keys %$href;
	my $cnt=scalar @keys;
	foreach my $key ( @keys ) {
	    my $value=$href->{$key};
	    if ( exists $sref->{$key} ) {
		if($bdeb){print "getBranch >>>>> Found: $key\n";}
		my $svalue=$sref->{$key};
		if (ref($svalue) eq "HASH" or ref($svalue) eq "farkdata") {
		    if ($value eq "") { # found empty target: store all values
			my %hash_copy=%$href;
			delete($href->{$key});
			my ($b,$h)=getBranch_($svalue,$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			} else {
			    $bref->{$key} = $b;
			    $href = $h; 
			    $found=1; # do not include this sub-branch later
			}
		    } elsif ( exists $svalue->{$value}) { # found target: key=>value
			if($bdeb){print "getBranch >>>>> Also found '$value'\n";}
			my %hash_copy=%$href;
			delete($href->{$key});			
			my ($b,$h)=getBranch_($svalue->{$value},$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			    if($bdeb){print "In-Complete branch\n";}
			} else {
			    $bref->{$key}->{$value} = $b;
			    $href = $h; 
			    if($bdeb){print "Complete branch\n";}
			    $found=1; # do not include this sub-branch later
			}
		    } else {
			print "getBranch >>>>> Missing $value among ".keys(%$href) ."\n";
		    }
		} else {
		    if($bdeb){print "getBranch >>>>> Value is not a HASH: $svalue\n";}
		    if ($value eq "" or $svalue eq $value) {
			my %hash_copy=%$href;
			delete($href->{$key});
			my ($b,$h)=getBranch_($svalue,$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			    if($bdeb){print "   Reset: $key (" . keys(%$href) . ")\n";}
			} else {
			    $bref->{$key} = $b;
			    $href = $h; 
			    $found=1; # do not include this sub-branch later
			}
		    }
		}
	    }
	}
	if ($found==0) {
	    my @skeys=keys %$sref;
	    foreach my $skey (@skeys) {
		if ($cnt == 0 or keys(%$href)) {
		    my $svalue = $sref->{$skey};
		    my %hash_copy=%$href;
		    my ($b,$h)=getBranch_($svalue,$href);
		    if (keys(%$h)) {
			$href=\%hash_copy; # incomplete branch
		    } else {
			$bref->{$skey} = $b;
			$href = $h; 
		    }
		}
	    }
	}
    }
    return ($bref, $href);
}
# $hash -> getKeys( variable => "geopotential_height_ml") gets top keys below specified pattern


# Returns reference to last key in key->value pair found
# example $farkdata->getKeyArrayRef({"key1" => "key2", "key3"=>"keyn"});

sub getKeyRef {
    my ($sref, $href) = @_;
    (my $kref, $href)=getKeyRef_($sref,$href);
    if (keys(%$href)) {return;} # no getKeyRef returned since not all keys were matched...
    if (ref($kref) eq "HASH") {bless($kref,"farkdata");}
    return $kref;
}

sub getKeyRef_ {
    my ($sref, $href) = @_;
    my $kref;
    my $type=ref($sref);
    #print "getKeyRef Entering $type\n";
    if ($type ne "HASH" and $type ne "farkdata") { 
	return ($kref, $href);
    } else {
	my $found=0;
	my $skey;
	my $svalue;
	my @keys=keys %$href;
	my $cnt=scalar @keys;
	foreach my $key ( @keys ) {
	    my $value=$href->{$key};
	    if ( exists $sref->{$key} ) {
		if($bdeb){print "getKeyRef found '$key'\n";}
		my $svalue=$sref->{$key};
		if (ref($svalue) eq "HASH") {
		    if ($value eq "") { # found empty target: store all values
			my %hash_copy=%$href;
			delete($href->{$key});			
			my ($lref, $h)=getKeyRef_($svalue,$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			} else {
			    $kref=$lref;
			    $href=$h;
			    $found=1; # do not include this sub-branch later
			    if($bdeb){print "getKeyRef found branch '$key'\n";}
			}
		    } elsif (exists $svalue->{$value}) {# found target: key=>value
			if($bdeb){print "getKeyRef found '$key' => '$value'\n";}
			my %hash_copy=%$href;
			delete($href->{$key});
			my ($lref,$h)=getKeyRef_($svalue->{$value},$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			} else {
			    $kref=$lref;
			    $href=$h;
			    if($bdeb){print "getKeyRef found branch '$key'\n";}
			    $found=1; # do not include this sub-branch later
			}
		    }
		} else {
		    if ($value eq "" or $svalue eq $value) {
			my %hash_copy=%$href;
			delete($href->{$key});
			my ($lref, $h)=getKeyRef_($svalue,$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			} else {
			    $kref=$lref;
			    $href=$h;
			    if($bdeb){print "getKeyRef found branch '$key'\n";}
			    $found=1; # do not include this sub-branch later
			}
		    }
		}
	    }
	}
	if ($found==0) {
	    if ($cnt == 0) {
		$kref=$sref;
	    } else {
		my @skeys=keys %$sref;
		foreach my $skey (@skeys) {	
		    if ($cnt == 0 or keys(%$href)) {
			my $svalue = $sref->{$skey};
			my %hash_copy=%$href;
			my ($lref, $h) = getKeyRef_($svalue,$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			} else {
			    $kref=$lref;
			    $href=$h;
			}
		    }
		}
	    }
	}
    }
    return ($kref, $href);
}

# Returns reference to last key in key-array, maintaining the
# order in the key-array, but allowing intermediary, non-defined keys.
# example $farkdata->getKeyArrayRef("key1","key2","keyn");

sub getKeyArrayRef {
    my ($sref, @path) = @_;
    my $hashref=getKeyArrayRef_($sref,@path);
    if (ref($hashref) eq "HASH") {bless($hashref,"farkdata");}
    return $hashref;
}

sub getKeyArrayRef_ {
    my ($sref, @path) = @_;
    my $hashref;
    my $type=ref($sref);
    #print "Entering $type ".scalar @path ."\n";
    if ($type ne "HASH" and $type ne "farkdata") { 
	return;
    } else {
	my $found=0;
	my $skey;
	my $svalue;
	my @pathcopy=@path;
	my @keys=keys %$sref;
	my $key = shift @path;
	if ( exists $sref->{$key} ) {
	    #print ">>>>>>Found key $key\n";
	    my $svalue=$sref->{$key};
	    if (ref($svalue) eq "HASH" or ref($svalue) eq "farkdata") {
		if (@path) {
		    if ($hashref=getKeyArrayRef_($svalue,@path)) {
			return $hashref;
		    } else {
			@path=@pathcopy;
		    }
		} else {
		    $hashref=$svalue;
		    return $hashref;
		}
	    }
	} else { # check all sub branches
	    #print ">>>>>>Key $key not in @keys\n";
	    @path=@pathcopy;
	    my @keys=keys %$sref;
	    foreach my $key (@keys) {
		my $svalue=$sref->{$key};
		if (ref($svalue) eq "HASH" or ref($svalue) eq "farkdata") {
		    if ($hashref=getKeyArrayRef_($svalue,@path)) {
			return $hashref;
		    } else {
			@path=@pathcopy;
		    }
		} else {
		    #print "Found type:".ref($svalue)."\n";
		}
	    }
	}
    }
    return;
}





# $hash -> getValues( variable => "geopotential_height_ml") gets values below specified pattern

sub getValues {
    my ($sref, $href) = @_;
    my @res=();
    ($href,@res)=getValues_($sref,$href);
    if (keys(%$href)) {return;} # no getValues returned since not all keys were matched...
    return @res;
}

sub getValues_ {
    my ($sref, $href) = @_;
    my @res = ();
    my $type=ref($sref);
    if ($bdeb) {print "Entering $type\n";}
    my @keys=keys %$href;
    my $cnt=scalar @keys;
    if ($type ne "HASH" and $type ne "farkdata") { 
	push (@res,$sref);
    } else {
	my $found=0;
	my $skey;
	my $svalue;
	foreach my $key ( @keys ) {
	    my $value=$href->{$key};
	    if ($bdeb) {print "Found key: '$key'\n";}
	    if ( exists $sref->{$key} ) {
		my $svalue=$sref->{$key};
		if (ref($svalue) eq "HASH" or ref($svalue) eq "farkdata") {
		    if ($value eq "") { # found empty target: store all values
			my %hash_copy=%$href;
			delete($href->{$key});			
			if ($bdeb) {print "   Missing value: '$key' " . scalar keys (%$href) . "\n";}
			my ($h,@locres)=getValues_($svalue,$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			} else {
			    @res=(@res,@locres);
			    $href = $h; 
			    $found=1; # do not include this sub-branch later
			}
		    } elsif (exists $svalue->{$value}) {# found target: key=>value
			my %hash_copy=%$href;
			delete($href->{$key});
			my ($h,@locres)=getValues_($svalue->{$value},$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			} else {
			    @res=(@res,@locres);
			    $href = $h; 
			    $found=1; # do not include this sub-branch later
			}
		    }
		} else {
		    if ($value eq "" or $svalue eq $value) {
			my %hash_copy=%$href;
			delete($href->{$key});
			my ($h,@locres)=getValues_($svalue,$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			} else {
			    $href = $h; 
			    @res=(@res,@locres);
			    $found=1; # do not include this sub-branch later
			}
		    }
		}
	    }
	}
	if ($found==0) {
	    my @skeys=keys %$sref;
	    foreach my $skey (@skeys) {	
		if ($cnt == 0 or keys(%$href)) {
		    my $svalue = $sref->{$skey};
		    my %hash_copy=%$href;
		    my ($h, @locres) = getValues_($svalue,$href);
		    if (keys(%$h)) {
			$href=\%hash_copy; # incomplete branch
		    } else {
			$href = $h; 
			@res=(@res,@locres);
		    }
		}
	    }
	}
    }
    if ($bdeb) {print "Done, keys:$cnt\n";}
    return ($href,@res);
}


# $hash -> getKeys( variable => "geopotential_height_ml") gets top keys below specified pattern

sub getKeys {
    my ($sref, $href) = @_;
    my @res=();
    ($href,@res)=getKeys_($sref,$href);
    if (keys(%$href)) {return;} # no getKeys returned since not all keys were matched...
    return @res;
}

sub getKeys_ {
    my ($sref, $href) = @_;
    my @res = ();
    my $type=ref($sref);
    if ($type ne "HASH" and $type ne "farkdata") { 
	push (@res,$sref);
    } else {
	my $found=0;
	my $skey;
	my $svalue;
	my @keys=keys %$href;
	my $cnt=scalar @keys;
	foreach my $key ( @keys ) {
	    my $value=$href->{$key};
	    if ( exists $sref->{$key} ) {
		my $svalue=$sref->{$key};
		if (ref($svalue) eq "HASH" or ref($svalue) eq "farkdata") {
		    if ($value eq "") { # found empty target: store all values
			my %hash_copy=%$href;
			delete($href->{$key});			
			my ($h,@locres)=getKeys_($svalue,$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			} else {
			    @res=(@res,@locres);
			    $href = $h; 
			    $found=1; # do not include this sub-branch later
			}
		    } elsif (exists $svalue->{$value}) {# found target: key=>value
			#print "Found '$key' => '$value'\n";
			my %hash_copy=%$href;
			delete($href->{$key});
			my ($h,@locres)=getKeys_($svalue->{$value},$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			} else {
			    #print "Returned keys @locres\n";
			    @res=(@res,@locres);
			    $href = $h; 
			    $found=1; # do not include this sub-branch later
			}
		    }
		} else {
		    if ($value eq "" or $svalue eq $value) {
			my %hash_copy=%$href;
			delete($href->{$key});
			my ($h,@locres)=getKeys_($svalue,$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			} else {
			    $href = $h; 
			    @res=(@res,@locres);
			    $found=1; # do not include this sub-branch later
			}
		    }
		}
	    }
	}
	if ($found==0) {
	    my @skeys=keys %$sref;
	    if ($cnt == 0) {
		@res=@skeys;
	    } else {
		foreach my $skey (@skeys) {	
		    if ($cnt == 0 or keys(%$href)) {
			my $svalue = $sref->{$skey};
			my %hash_copy=%$href;
			my ($h, @locres) = getKeys_($svalue,$href);
			if (keys(%$h)) {
			    $href=\%hash_copy; # incomplete branch
			} else {
			    $href = $h; 
			    @res=(@res,@locres);
			}
		    }
		}
	    }
	}
    }
    return ($href,@res);
}


sub my_complex_sort {
  # code that compares $a and $b and returns -1, 0 or 1 as appropriate
  my ($number_a) = ($a =~ /([\d]+)/);
  my ($number_b) = ($b =~ /([\d]+)/);
  if (defined($number_a) & defined($number_b)) {
      return $number_a <=> $number_b;
  } else {
      return $a cmp $b;
  }
}

1;
