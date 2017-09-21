#!/usr/bin/perl -w
#
use ExtUtils::testlib;
use fark;
use farkdata;
#
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use Data::Dumper;
use Time::HiRes qw( time );
use farkdir;
#
#sudo apt-get update
#sudo apt-get install libfile-find-rule-perl-perl



#dont know if you need this: sudo apt-get install libpath-tiny-perl
#but you need this: sudo apt-get install libcapture-tiny-perl
use Capture::Tiny 'capture';
# must end with slash...
#
my $ref=CGI->new();

$XML::LibXML::skipXMLDeclaration = 1;
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
my $param= $ref->{param};
if (! defined $param->{type}) {farkdir::term("Undefined type.".Dumper($param))};
if ($param->{type}->[0] eq "model") {
    &findModel($param,"model");
} elsif ($param->{type}->[0] eq "modelfile") {
    &findModelFile($param,"model");
} elsif ($param->{type}->[0] eq "obs") {
    &findObs($param,"obs");
} elsif ($param->{type}->[0] eq "obsfile") {
    &findObsFile($param,"obs");
}

sub findModel {
    my $param =shift;
    my $cls=shift;
    my $ipath=$param->{file}[0];
    my $password=($param->{password}[0] // "");
    my $filterDir = ($param->{filterDir}->[0] || "/");
    my $filterFile = ($param->{filterFile}->[0] // "");
    my $index = ($param->{index}->[0] // "");
    # get config file paths...
    my ($idir,$ifile) = farkdir::splitName($ipath);
    my ($root, $loc, $priv) = farkdir::splitDir($idir,$cls);
    my $lpath = $loc.$ifile; # local path
    my $fpath = $root.$lpath; # full path
    # get filter paths...
    my ($filterroot, $filterloc, $filterpriv) = 
	farkdir::splitDir($filterDir,"data");
    # xml
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    # read old config file into memory
    my $passok=1;
    if (-e $fpath && $priv eq "rw") {
	$doc = $parser->parse_file($fpath);
	if ( ($node)=$doc->findnodes("model/model_config")) {
	    my $pass=($node->getAttribute("password")||"");
	    if ($pass ne $password) {
		$passok=0;
		$doc = $parser->parse_string("<model><model_config/></model>");
		($node) = $doc->findnodes("model/model_config");
	    }
	} else {
	    farkdir::term("Corrupt file: ".$fpath);
	}
    } else {
	$doc = $parser->parse_string("<model><model_config/></model>");
	($node) = $doc->findnodes("model/model_config");
    }
    my @oldNodes=$node->findnodes("stack");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    };
    $node->setAttribute("file",      $ifile);
    $node->setAttribute("class",     $cls);
    $node->setAttribute("root",      $root);
    $node->setAttribute("location",  $loc);
    $node->setAttribute("status",    $priv);
    if ($filterpriv eq "ro" || $filterpriv eq "rw") {
	my @files=farkdir::find($filterFile,$filterDir);
	if (@files) {
	    $node->setAttribute("password",        $password);
	    $node->setAttribute("filterDir",       $filterDir);
	    $node->setAttribute("filterFile",      $filterFile);
	    $node->setAttribute("index",           $index);
	    $node->setAttribute("hits",            scalar @files);
	    foreach my $sfile (@files) {
		my $parent = XML::LibXML::Element->new( 'stack' );
		$parent->setAttribute("name",$sfile);
		$node->addChild( $parent );
	    };
	    # put xml-structure into file
	    if ($passok) {
		if (open(my $fh, '>', $fpath)) {
		    print $fh $doc->toString;
		    close $fh;
		    chmod 0666, $fpath;
		} else {
		    farkdir::term("Unable to open:".$fpath);
		};
	    }
	    # report xml-structure
	    print $doc->toString . "\n";
	} else {
	    farkdir::term("No files found.");
	}
    } else {
	farkdir::term("Permission denied.");
    };
};

sub findModelFile {
    my $param =shift;
    my $cls=shift;
    my $ipath=$param->{file}[0]; # input path
    my $password=($param->{password}[0] // "");
    my $spath = $param->{target}[0] // "";
    # get config file paths...
    my ($idir,$ifile) = farkdir::splitName($ipath);
    my ($root, $loc, $priv) = farkdir::splitDir($idir,$cls);
    my $lpath = $loc.$ifile; # local path
    my $fpath = $root.$lpath; # full path
    # get stack paths...
    my ($sdir,$sfile) = farkdir::splitName($spath);
    my ($sroot, $sloc, $spriv) = farkdir::splitDir($sdir,"data");
    # xml
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    # read config file into memory
    my $passok=1;
    if (-e $fpath && $priv eq "rw") {
	$doc = $parser->parse_file($fpath);
	if ( ($node)=$doc->findnodes("model/model_config")) {
	    my $pass=($node->getAttribute("password")||"");
	    if ($pass ne $password) {
		$passok=0;
		$doc = $parser->parse_string("<model><model_config/></model>");
		($node) = $doc->findnodes("model/model_config");
	    }
	} else {
	    farkdir::term("Corrupt file: ".$fpath);
	}
    } else {
	$doc = $parser->parse_string("<model><model_config/></model>");
	($node) = $doc->findnodes("model/model_config");
    }
    # put inventory of first model-file into xml-structure
    my @oldNodes=$node->findnodes("variable");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    };
    $node->setAttribute("file",      $ifile);
    $node->setAttribute("class",     $cls);
    $node->setAttribute("root",      $root);
    $node->setAttribute("location",  $loc);
    $node->setAttribute("status",    $priv);
    if (-f $spath) {
	my $log="";
	eval {
	    $log=capture {
		my $index=$node->getAttribute("index");
		my $fark=fark->open();
		$fark->clearModelFileStack($index);
		$fark->pushModelFile($spath);
		my $data = $fark->peekModelFile();
		if ($data) {
		    #$data->printTree("Model:");
		    foreach my $index (sort keys %{$data->{file}->{variable}}) {
			my $parent = XML::LibXML::Element->new( 'variable' );
			if (defined $data->{file}->{variable}->{$index}->{dimension}) {
			    my $dims="";
			    foreach my $dim (sort keys %{$data->{file}->{variable}->{$index}->{dimension}}) {
				if ($dims) {
				    $dims=$dims . "," . $data->{file}->{variable}->{$index}->{dimension}->{$dim};
				} else {
				    $dims=$data->{file}->{variable}->{$index}->{dimension}->{$dim};
				}
			    }
			    $parent->setAttribute("dims",$dims);
			};
			$parent->setAttribute("name",$data->{file}->{variable}->{$index}->{name});
			$node->addChild( $parent );
		    }
		    if (defined $data->{file}->{index}->{start}) {
			$node->setAttribute("start",$data->{file}->{index}->{start});
		    };
		    if (defined $data->{file}->{index}->{stop}) {
			$node->setAttribute("stop",$data->{file}->{index}->{stop});
		    };
		};
		$fark->close();
	    };
	};
	my $ret=$@;if ($ret) {farkdir::term($ret);}
	if ($passok) {
	    # put xml-structure into file
	    if (open(my $fh, '>', $fpath)) {
		print $fh $doc->toString;
		close $fh;
		chmod 0666, $fpath;
	    } else {
		farkdir::term("Unable to open:".$fpath);
	    };
	}
	# report xml-structure
	print $doc->toString . "\n";
    } else {
	farkdir::term("Unable to open $spath.");
    };
}
sub findObs {
    my $param =shift;
    my $cls=shift;
    my $ipath=$param->{file}[0];
    my $password=($param->{password}[0] // "");
    my $filterDir = ($param->{filterDir}->[0]||"/tmp");
    my $filterFile = ($param->{filterFile}->[0] // ".*");
    my $bufrType=($param->{bufrType}[0] // "");
    my $subType=($param->{subType}[0] // "");
    my $typeInfo=($param->{typeInfo}[0] // "");
    my $indexTarget=($param->{indexTarget}[0] // "");
    my $indexExp=($param->{indexExp}[0] // "");
    my $table = ($param->{table}->[0] // "");
    # get config file paths...
    my ($idir,$ifile) = farkdir::splitName($ipath);
    my ($root, $loc, $priv) = farkdir::splitDir($idir,$cls);
    my $lpath = $loc.$ifile; # local path
    my $fpath = $root.$lpath; # full path
    # get filter paths...
    my ($filterroot, $filterloc, $filterpriv) = 
	farkdir::splitDir($filterDir,"data");
    # xml
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    # read config file into memory
    my $passok=1;
    if (-e $fpath && $priv eq "rw") {
	$doc = $parser->parse_file($fpath);
	if ( ($node)=$doc->findnodes("obs/obs_config")) {
	    my $pass=($node->getAttribute("password")||"");
	    if ($pass ne $password) {
		$passok=0;
		$doc = $parser->parse_string("<obs><obs_config/></obs>");
		($node) = $doc->findnodes("obs/obs_config");
	    }
	} else {
	    farkdir::term("Corrupt file: ".$fpath);
	}
    } else {
	$doc = $parser->parse_string("<obs><obs_config/></obs>");
	($node) = $doc->findnodes("obs/obs_config");
    }
    my @oldNodes=$node->findnodes("stack");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    };
    $node->setAttribute("file",      $ifile);
    $node->setAttribute("class",     $cls);
    $node->setAttribute("root",      $root);
    $node->setAttribute("location",  $loc);
    $node->setAttribute("status",    $priv);
    if ($filterpriv eq "ro" || $filterpriv eq "rw") {
	my @files=farkdir::find($filterFile,$filterDir);
	if (@files) {
	    $node->setAttribute("password",        $password);
	    $node->setAttribute("filterDir",       $filterDir);
	    $node->setAttribute("filterFile",      $filterFile);
	    $node->setAttribute("tablePath",       $table);
	    $node->setAttribute("bufrType",        $bufrType);
	    $node->setAttribute("subType",         $subType);
	    $node->setAttribute("typeInfo",        $typeInfo);
	    $node->setAttribute("indexTarget",     $indexTarget);
	    $node->setAttribute("indexExp",        $indexExp);
	    $node->setAttribute("hits",            scalar @files);
	    foreach my $spath (@files) {
		my $parent = XML::LibXML::Element->new( 'stack' );
		$parent->setAttribute("name",$spath);
		$node->addChild( $parent );
	    };
	    # put xml-structure into file
	    if ($passok) {
		if (open(my $fh, '>', $fpath)) {
		    print $fh $doc->toString;
		    close $fh;
		    chmod 0666, $fpath;
		} else {
		    farkdir::term("Unable to open:".$fpath);
		}
	    };
	    # report xml-structure
	    print $doc->toString . "\n";
	} else {
	    farkdir::term("No files found.");
	}
    } else {
	farkdir::term("Permission denied.");
    };
};

sub findObsFile {
    my $param =shift;
    my $cls=shift;
    my $ipath=$param->{file}[0];
    my $password=($param->{password}[0] // "");
    my $spath=$param->{target}[0] // "";
    my $obsTargets = ($param->{obsTargets}->[0] // "");
    my $indexTarget = ($param->{indexTarget}->[0]);
    my $indexExp = ($param->{indexExp}->[0]);
    my $bufrType=($param->{bufrType}->[0]);
    my $subType=($param->{subType}->[0]);
    my $typeInfo=($param->{typeInfo}[0]);
    my $table=($param->{table}[0]);
    # get config file paths...
    my ($idir,$ifile) = farkdir::splitName($ipath);
    my ($root, $loc, $priv) = farkdir::splitDir($idir,$cls);
    my $lpath = $loc.$ifile; # local path
    my $fpath = $root.$lpath; # full path
    # get stack paths...
    my ($sdir,$sfile) = farkdir::splitName($spath);
    my ($sroot, $sloc, $spriv) = farkdir::splitDir($sdir,"data");
    # xml
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    # read config file into memory
    my $passok=1;
    if (-e $fpath && $priv eq "rw") {
	$doc = $parser->parse_file($fpath);
	if ( ($node)=$doc->findnodes("obs/obs_config")) {
	    my $pass=($node->getAttribute("password")||"");
	    if ($pass ne $password) {
		$passok=0;
		$doc = $parser->parse_string("<obs><obs_config/></obs>");
		($node) = $doc->findnodes("obs/obs_config");
	    }
	} else {
	    farkdir::term("Corrupt file: ".$fpath);
	}
    } else {
	$doc = $parser->parse_string("<obs><obs_config/></obs>");
	($node) = $doc->findnodes("obs/obs_config");
    }
    my @oldNodes=$node->findnodes("bufr");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    };
    @oldNodes=$node->findnodes("time");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    };
    $node->setAttribute("file",      $ifile);
    $node->setAttribute("class",     $cls);
    $node->setAttribute("root",      $root);
    $node->setAttribute("location",  $loc);
    $node->setAttribute("status",    $priv);
    if (-f $spath) {
	my $log="";
	eval {
	    $log=capture {
	        if (! defined $table) {$table=$node->getAttribute("tablePath");}
		if (! defined $indexTarget) {$indexTarget=$node->getAttribute("indexTarget");}
		if (! defined $indexExp) {$indexExp=$node->getAttribute("indexExp");}
		if (! defined $bufrType) {$bufrType=$node->getAttribute("bufrType");}
		if (! defined $subType) {$subType=$node->getAttribute("subType");}
		if (! defined $typeInfo) {$typeInfo=$node->getAttribute("typeInfo");}
	        my $fark=fark->open();
		print "Starting with:$bufrType,$subType,$table\n";
		$fark->setObservationTablePath($table);
		if (defined $bufrType && defined $subType) {
		    #$fark->setObservationType($bufrType,$subType);
		    # should probably have tested for "defined $obsTargets" instead (and not used a default)
		    my @lines = split (/\|/, $obsTargets,-1);
		    if (! @lines) {
			my @targets=$node->findnodes("target");
			foreach my $trg (@targets) {
			    my $s=sprintf ("%s/%s/%s/%s/%s/%s",
					   $trg->getAttribute("name"),
					   $trg->getAttribute("pos"),
					   $trg->getAttribute("descr"),
					   $trg->getAttribute("info"),
					   $trg->getAttribute("min"),
					   $trg->getAttribute("max"));
			    push(@lines,$s);
			}
		    }
		    if (@lines) { 
			foreach my $line (@lines) {
			    if ($line ne "") {
				my @items=split (/\~/, $line,-1);
				my $len=$#items;
				if ($len == 5) {
				    #$fark->pushObservationTarget(@items); # name,pos,descr,info,min,max
				}
			    }
			}
		    }
		    if ($indexTarget && $indexExp) {
			print "fark_find.pl Setting observation index $indexTarget, $indexExp\n";
			#$fark->setObservationIndex($indexTarget,$indexExp);
#		    } else {
#			die "No observation index specified.";
		    }
#		} else {
#		    die "No bufr/subType specified.";
		}
		$fark->clearObservationFileStack();
		$fark->pushObservationFile($spath);
		my $data = $fark->peekObservationFile();
		if ($data) {
		    #$data->printTree("Observation:");
		    if (defined $data->{file}->{"index"}->{start}) {$node->setAttribute("start",$data->{file}->{"index"}->{start});};
		    if (defined $data->{file}->{"index"}->{stop} ) {$node->setAttribute("stop",$data->{file}->{"index"}->{stop});}
		    if (defined $data->{file}->{type}) {
			foreach my $type (sort keys %{$data->{file}->{type}}) {
			    if (defined $data->{file}->{type}->{$type}->{description}) {
				my $info=$data->{file}->{type}->{$type}->{description};
				my $cnt=$data->{file}->{type}->{$type}->{"message count"};
				my $parent = XML::LibXML::Element->new( 'bufr' );
				$parent->setAttribute("bufrType",$type);
				$parent->setAttribute("info",$info);
				$parent->setAttribute("cnt",$cnt);
				$node->addChild( $parent );
			    };
			    if (defined $data->{file}->{type}->{$type}->{subtype}) {
				foreach my $subtype (keys %{$data->{file}->{type}->{$type}->{subtype}}) {
				    my $parent = XML::LibXML::Element->new( 'bufr' );
				    $parent->setAttribute("bufrType",$type);
				    $parent->setAttribute("subType",$subtype);
				    $parent->setAttribute("cnt",$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{"message count"});
				    $parent->setAttribute("info",$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{"description"});
				    my $seqno=$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{seqno};
				    my $name=$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{name};
				    my $unit=$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{unit};
				    if (defined $seqno) {
					foreach my $pos (sort {$a<=>$b} keys %{$seqno}) {
					    if ($pos < 250) {
						my $posNode = XML::LibXML::Element->new( 'seq' );
						$posNode->setAttribute("pos",$pos);
						$posNode->setAttribute("descr",$seqno->{$pos}||"");
						$posNode->setAttribute("info",$name->{$pos}||"");
						$parent->addChild( $posNode );
					    }
					}
				    }
				    $node->addChild( $parent );
				}
			    };
			};
		    } else {
			die "How strange, no valid data was found.";
		    }
		};
		$fark->close();
	    };
	};
	#print $log;
	my $ret=$@;if ($ret) {farkdir::term($ret);}
	# put xml-structure into file
	if ($passok) {
	    if (open(my $fh, '>', $fpath)) {
		print $fh $doc->toString;
		close $fh;
		chmod 0666, $fpath;
	    } else {
		farkdir::term("Unable to open:".$fpath);
	    }
	}
	# report xml-structure
	print $doc->toString . "\n";
    } else {
	farkdir::term("Unable to open $spath.");
    }
};
sub tostring (&) {
  my $s;
  open local *STDOUT, '>', \$s;
  shift->();
  $s
}
