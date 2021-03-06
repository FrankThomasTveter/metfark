#!/usr/bin/perl -w
#
use strict;
use lib "/home/ubuntu/perl5/lib/perl5/x86_64-linux-gnu-thread-multi";
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use Data::Dumper;
use Time::HiRes qw( time );
#
use ExtUtils::testlib;
use fark;
use farkdata;
use farkdir;
#
#sudo apt-get update
#sudo apt-get install libfile-find-rule-perl-perl



#dont know if you need this: sudo apt-get install libpath-tiny-perl
#but you need this: sudo apt-get install libcapture-tiny-perl
use Capture::Tiny 'capture_merged';
# must end with slash...
#
#
my $ref=CGI->new();

$XML::LibXML::skipXMLDeclaration = 1;
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
my $param= $ref->{param};
#
# 1 = obs
# 2 = model
# 5 = parser
#
my $debug=0;
if (defined $param->{debug}[0]) {
    $debug=1;       # debug this script (0=omit output)
    fark::debug($debug);
}
#
if (! defined $param->{type}) {farkdir::term("Undefined type.".Dumper($param))};
if ($param->{type}->[0] eq "model") {
    &findModel($param,"model");
} elsif ($param->{type}->[0] eq "modelfile") {
    &findModelFile($param,"model");
} elsif ($param->{type}->[0] eq "obs") {
    &findObs($param,"obs");
} elsif ($param->{type}->[0] eq "obsfile") {
    &findObsFile($param,"obs");
} elsif ($param->{type}->[0] eq "join") {
    &findJoin($param,"join");
} elsif ($param->{type}->[0] eq "joinfile") {
    &findJoinFile($param,"join");
} elsif ($param->{type}->[0] eq "clean") {
    &findClean($param,"clean");
} else {
    farkdir::term("Invalid type: ".$param->{type}->[0]);
}

sub findModel {
    my $param =shift;
    my $cls=shift;
    my $ipath=$param->{file}[0];
    my $password=($param->{password}[0] // "");
    my $filterDir = ($param->{filterDir}->[0] // "/");
    my $filterDirMin = ($param->{filterDirMin}->[0] // "");
    my $filterDirMax = ($param->{filterDirMax}->[0] // "");
    my $filterFile = ($param->{filterFile}->[0] // "");
    my $indexTarget = ($param->{indexTarget}->[0] // "");
    my $indexVariable = ($param->{indexVariable}->[0] // "");
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
	    my $pass=($node->getAttribute("password")//"");
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
    my @oldNodes=$node->findnodes("stack");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    };
    $node->setAttribute("file",      $ifile//"");
    $node->setAttribute("class",     $cls//"");
    $node->setAttribute("root",      $root//"");
    $node->setAttribute("location",  $loc//"");
    $node->setAttribute("status",    $priv//"");
    if ($filterpriv eq "ro" || $filterpriv eq "rw") {
    	farkdir::sandbox {
	    $node->setAttribute("password",        $password//"");
	    $node->setAttribute("filterDir",       $filterDir//"");
	    $node->setAttribute("filterDirMin",    $filterDirMin // "");
	    $node->setAttribute("filterDirMax",    $filterDirMax // "");
	    $node->setAttribute("filterFile",      $filterFile//"");
	    $node->setAttribute("indexTarget",     $indexTarget//"");
	    $node->setAttribute("indexVariable",   $indexVariable//"");
	    my @files=farkdir::FindFiles($filterDir,$filterFile,
					 $filterDirMin,$filterDirMax,10);
	    if (@files) {
		$node->setAttribute("hits",            scalar @files);
		foreach my $sfile (@files) {
		    my $parent = XML::LibXML::Element->new( 'stack' );
		    $parent->setAttribute("name",$sfile//"");
		    if (-f $sfile) {
			$parent->setAttribute("age",(-M $sfile//""));
			$parent->setAttribute("size",size_in_mb(-s $sfile))
		    };
		    $node->addChild( $parent );
		};
		# put xml-structure into file
		if ($passok) {
		    farkdir::docsave($fpath,$doc);
		}
	    } else {
		my $parent = XML::LibXML::Element->new( 'stack' );
		$node->addChild( $parent );
		return "No files found.";
	    }
	}{message=>"Unable to findfiles: $filterDir,$filterFile,$filterDirMin,$filterDirMax",
	  stdout=>"never"}; #ignore output
	# report xml-structure
	print $doc->toString . "\n";
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
	    my $pass=($node->getAttribute("password")//"");
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
    @oldNodes=$node->findnodes("dimension");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    };
    $node->setAttribute("file",      $ifile//"");
    $node->setAttribute("class",     $cls//"");
    $node->setAttribute("root",      $root//"");
    $node->setAttribute("location",  $loc//"");
    $node->setAttribute("status",    $priv//"");
    my $indexTarget=$node->getAttribute("indexTarget")//"";
    my $indexVariable=$node->getAttribute("indexVariable")//"";
    if (-f $spath) {
	if ($debug) {
	    print "Calling processModelFile\n";
	    &processModelFile($node,$cls,$ipath,$password,$spath,
			      $indexTarget,$indexVariable);
	    print "Done calling processModelFile\n";
	} else {
	    my ($log,$irc)=capture_merged {
		eval {
		    &processModelFile($node,$cls,$ipath,$password,$spath,
				      $indexTarget,$indexVariable);
		};
		return $@;
	    };
	    if ($irc) { farkdir::term("Unable to process, $irc $log");}
	}
	if ($passok) {
	    # put xml-structure into file
	    farkdir::docsave($fpath,$doc);
	}
	# report xml-structure
	print $doc->toString . "\n";
    } else {
	farkdir::term("Unable to open '$spath'.");
    };
}

sub processModelFile {
    my $node       = shift;
    my $cls         = shift;
    my $ipath       = shift;
    my $password    = shift;
    my $spath       = shift;
    my $indexTarget = shift;
    my $indexVariable = shift;
    #
    my $fark=fark->open();
    $fark->clearModelFileStack($indexVariable);
    $fark->pushModelFile($spath);
    my $data = $fark->peekModelFile();
    if ($data) {
	if($debug){$data->printTree("Model:");}
	my %dimhash;
	foreach my $index (sort {$data->{file}->{dimension}->{$a} cmp 
				     $data->{file}->{dimension}->{$b}} 
			   keys %{$data->{file}->{dimension}}) {
	    foreach my $name (keys %{$data->{file}->{dimension}->{$index}}) {
		my $parent = XML::LibXML::Element->new( 'dimension' );
		$parent->setAttribute("name",$name//"");
		if (defined $data->{file}->{dimension}->{$index}->{$name}->{"size"}) {
		    my $size = $data->{file}->{dimension}->{$index}->{$name}->{"size"}+0;
		    $parent->setAttribute("size",$size//"");
		    $dimhash{$name}=$size//1;
		}
		$node->addChild( $parent );
	    }
	}
	foreach my $index (sort {$data->{file}->{variable}->{$a}->{name} cmp $data->{file}->{variable}->{$b}->{name}} keys %{$data->{file}->{variable}}) {
	    my $parent = XML::LibXML::Element->new( 'variable' );
	    my $name=$data->{file}->{variable}->{$index}->{name};
	    my $size=1;
	    if (defined $data->{file}->{variable}->{$index}->{dimension}) {
		my $dims="";
		foreach my $dim (sort keys %{$data->{file}->{variable}->{$index}->{dimension}}) {
		    my $dimname=$data->{file}->{variable}->{$index}->{dimension}->{$dim};
		    $size=$size*($dimhash{$dimname}||1)+0;
		    if ($dims) {
			$dims=$dims . "," . $data->{file}->{variable}->{$index}->{dimension}->{$dim};
		    } else {
			$dims=$data->{file}->{variable}->{$index}->{dimension}->{$dim};
		    }
		}
		$parent->setAttribute("dims",$dims//"");
	    };
	    print "Var: $name  -> $size\n";
	    $parent->setAttribute("name",$data->{file}->{variable}->{$index}->{name}//"");
	    $parent->setAttribute("size",$size//1);
	    $node->addChild( $parent );
	}
	if (defined $data->{file}->{index}->{start}) {
	    $node->setAttribute("start",$data->{file}->{index}->{start}//"");
	};
	if (defined $data->{file}->{index}->{stop}) {
	    $node->setAttribute("stop",$data->{file}->{index}->{stop}//"");
	};
    };
    $fark->close();
}

sub findObs {
    my $param =shift;
    my $cls=shift;
    my $ipath=$param->{file}[0];
    my $password=($param->{password}[0] // "");
    my $filterDir = ($param->{filterDir}->[0]//"/tmp");
    my $filterDirMin = ($param->{filterDirMin}->[0] // "");
    my $filterDirMax = ($param->{filterDirMax}->[0] // "");
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
	    my $pass=($node->getAttribute("password")//"");
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
    $node->setAttribute("file",      $ifile//"");
    $node->setAttribute("class",     $cls//"");
    $node->setAttribute("root",      $root//"");
    $node->setAttribute("location",  $loc//"");
    $node->setAttribute("status",    $priv//"");
    if ($filterpriv eq "ro" || $filterpriv eq "rw") {
	$node->setAttribute("password",        $password//"");
	$node->setAttribute("filterDir",       $filterDir//"");
	$node->setAttribute("filterDirMin",    $filterDirMin // "");
	$node->setAttribute("filterDirMax",    $filterDirMax // "");
	$node->setAttribute("filterFile",      $filterFile//"");
	$node->setAttribute("tablePath",       $table//"");
	$node->setAttribute("bufrType",        $bufrType//"");
	$node->setAttribute("subType",         $subType//"");
	$node->setAttribute("typeInfo",        $typeInfo//"");
	$node->setAttribute("indexTarget",     $indexTarget//"");
	$node->setAttribute("indexExp",        $indexExp//"");
	my @files=farkdir::FindFiles($filterDir,$filterFile,$filterDirMin,$filterDirMax,10);
	if (@files) {
	    $node->setAttribute("hits",            scalar @files);
	    foreach my $spath (@files) {
		my $parent = XML::LibXML::Element->new( 'stack' );
		$parent->setAttribute("name",$spath//"");
		if (-f $spath) {
		    $parent->setAttribute("age",(-M $spath//""));
		    $parent->setAttribute("size",size_in_mb(-s $spath))
		};
		$node->addChild( $parent );
	    };
	    # put xml-structure into file
	    if ($passok) {
		farkdir::docsave($fpath,$doc);
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
	    my $pass=($node->getAttribute("password")//"");
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
    $node->setAttribute("file",      $ifile//"");
    $node->setAttribute("class",     $cls//"");
    $node->setAttribute("root",      $root//"");
    $node->setAttribute("location",  $loc//"");
    $node->setAttribute("status",    $priv//"");
    if (-f $spath) {
	if ($debug) {
	    print "Calling processObsFile\n";
	    &processObsFile($node,$cls,$ipath,$password,$spath,$obsTargets,
			    $indexTarget,$indexExp,$bufrType,$subType,$typeInfo,$table);
	    print "Done calling processObsFile\n";
	} else {
	    my ($log,$irc)=capture_merged {
		eval {
		    &processObsFile($node,$cls,$ipath,$password,$spath,$obsTargets,
				    $indexTarget,$indexExp,$bufrType,$subType,$typeInfo,$table);
		};
		return $@;
	    };
	    if ($irc) { farkdir::term("Unable to process $irc $log");}
	}
	# put xml-structure into file
	if ($passok) {
	    farkdir::docsave($fpath,$doc);
	}
	# report xml-structure
	print $doc->toString . "\n";
    } else {
	farkdir::term("Unable to open '$spath'.");
    }
};

sub processObsFile {
    my $node       = shift;
    my $cls         = shift;
    my $ipath       = shift;
    my $password    = shift;
    my $spath       = shift;
    my $obsTargets  = shift;
    my $indexTarget = shift;
    my $indexExp    = shift;
    my $bufrType    = shift;
    my $subType     = shift;
    my $typeInfo    = shift;
    my $table       = shift;
    if (! defined $table) {$table=$node->getAttribute("tablePath");}
    if (! defined $indexTarget) {$indexTarget=$node->getAttribute("indexTarget");}
    if (! defined $indexExp) {$indexExp=$node->getAttribute("indexExp");}
    if (! defined $bufrType) {$bufrType=$node->getAttribute("bufrType");}
    if (! defined $subType) {$subType=$node->getAttribute("subType");}
    if (! defined $typeInfo) {$typeInfo=$node->getAttribute("typeInfo");}
    my $fark=fark->open();
    if ($debug) {print "Starting with:$bufrType,$subType,$table\n";}
    $fark->setObservationTablePath($table);
    if (defined $bufrType && defined $subType) {
	#$fark->setObservationType($bufrType,$subType);
	# should probably have tested for "defined $obsTargets" instead (and not used a default)
	my @lines = split (/\|/, $obsTargets,-1);
	if (! @lines) {
	    my @targets=$node->findnodes("target");
	    foreach my $trg (@targets) {
		my $s=sprintf ("%s/%s/%s/%s",
			       $trg->getAttribute("name"),
			       $trg->getAttribute("pos"),
			       $trg->getAttribute("descr"),
			       $trg->getAttribute("info"));
		push(@lines,$s);
	    }
	}
	if (@lines) { 
	    foreach my $line (@lines) {
		if ($line ne "") {
		    my @items=split (/\~/, $line,-1);
		    my $len=$#items;
		    if ($len == 3) {
			if ($debug) {print "fark_find.pl Setting observation target ".@items ."\n";};
			$fark->pushObservationTarget(@items); # name,pos,descr,info,min,max
		    }
		}
	    }
	}
	if ($indexTarget && $indexExp) {
	    if ($debug) {print "fark_find.pl Setting observation index $indexTarget, $indexExp\n";}
	    $fark->setObservationIndex($indexTarget,$indexExp);
	} elsif ($indexTarget || $indexExp) {
	    die "Missing index information.";
	}
    } else {
	die "No bufr/subType specified.";
    }
    $fark->clearObservationFileStack();
    $fark->pushObservationFile($spath);
    my $data = $fark->peekObservationFile();
    if ($data) {
	if ($debug) {$data->printTree("Observation:");}
	if (defined $data->{file}->{"index"}->{start}) {$node->setAttribute("start",$data->{file}->{"index"}->{start}//"");};
	if (defined $data->{file}->{"index"}->{stop} ) {$node->setAttribute("stop",$data->{file}->{"index"}->{stop}//"");}
	if (defined $data->{file}->{type}) {
	    foreach my $type (sort keys %{$data->{file}->{type}}) {
		if (defined $data->{file}->{type}->{$type}->{description}) {
		    my $info=$data->{file}->{type}->{$type}->{description};
		    my $cnt=$data->{file}->{type}->{$type}->{"message count"};
		    my $parent = XML::LibXML::Element->new( 'bufr' );
		    $parent->setAttribute("bufrType",$type//"");
		    $parent->setAttribute("info",$info//"");
		    $parent->setAttribute("cnt",$cnt//"");
		    $node->addChild( $parent );
		};
		if (defined $data->{file}->{type}->{$type}->{subtype}) {
		    foreach my $subtype (keys %{$data->{file}->{type}->{$type}->{subtype}}) {
			my $parent = XML::LibXML::Element->new( 'bufr' );
			if ($debug){print "Bufr: $type Sub: $subtype  \n";}
			$parent->setAttribute("bufrType",$type//"");
			$parent->setAttribute("subType",$subtype//"");
			$parent->setAttribute("cnt",$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{"message count"}//"");
			$parent->setAttribute("info",$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{"description"}//"");
			my $seqno=$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{seqno};
			my $name=$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{name};
			my $unit=$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{unit};
			my $val1=$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{val1};

			if (defined $seqno) {
			    foreach my $pos (sort {$a<=>$b} keys %{$seqno}) {
				if ($pos < 250) {
				    #print "### VALUE: '$pos'->'".($val1->{$pos}//"")."'\n";
			
				    my $posNode = XML::LibXML::Element->new( 'seq' );
				    $posNode->setAttribute("pos",$pos//"");
				    $posNode->setAttribute("descr",$seqno->{$pos}//"");
				    $posNode->setAttribute("info",$name->{$pos}//"");
				    $posNode->setAttribute("unit",$unit->{$pos}//"");
				    $posNode->setAttribute("val1",$val1->{$pos}//"");
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
}

sub findJoin {
    my $param =shift;
    my $cls=shift;
    my $ipath=$param->{file}[0];
    my $password=($param->{password}[0] // "");
    my $filterDir = ($param->{filterDir}->[0] // "/");
    my $filterFile = ($param->{filterFile}->[0] // "");
    my $filterDirMin = ($param->{filterDirMin}->[0] // "");
    my $filterDirMax = ($param->{filterDirMax}->[0] // "");
    # get config file paths...
    my ($idir,$ifile) = farkdir::splitName($ipath);
    my ($root, $loc, $priv) = farkdir::splitDir($idir,$cls);
    my $lpath = $loc.$ifile; # local path
    my $fpath = $root.$lpath; # full path
    # get filter paths...
    my ($filterroot, $filterloc, $filterpriv) = 
	farkdir::splitDir($filterDir,"output");
    # xml
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    # read old config file into memory
    my $passok=1;
    if (-e $fpath && $priv eq "rw") {
	$doc = $parser->parse_file($fpath);
	if ( ($node)=$doc->findnodes("join/join_config")) {
	    my $pass=($node->getAttribute("password")//"");
	    if ($pass ne $password) {
		$passok=0;
		$doc = $parser->parse_string("<join><join_config/></join>");
		($node) = $doc->findnodes("join/join_config");
	    }
	} else {
	    farkdir::term("Corrupt file: ".$fpath);
	}
    } else {
	$doc = $parser->parse_string("<join><join_config/></join>");
	($node) = $doc->findnodes("join/join_config");
    }
    # put inventory of first join-file into xml-structure
    my @oldNodes=$node->findnodes("stack");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    };
    $node->setAttribute("file",      $ifile//"");
    $node->setAttribute("class",     $cls//"");
    $node->setAttribute("root",      $root//"");
    $node->setAttribute("location",  $loc//"");
    $node->setAttribute("status",    $priv//"");
    if ($filterpriv eq "ro" || $filterpriv eq "rw") {
    	farkdir::sandbox {
	    $node->setAttribute("password",        $password//"");
	    $node->setAttribute("filterDir",       $filterDir//"");
	    $node->setAttribute("filterFile",      $filterFile//"");
	    $node->setAttribute("filterDirMin",    $filterDirMin // "");
	    $node->setAttribute("filterDirMax",    $filterDirMax // "");
	    my @files=farkdir::FindFiles($filterDir,$filterFile,
					 $filterDirMin,$filterDirMax,10);
	    if (@files) {
		$node->setAttribute("hits",            scalar @files);
		foreach my $sfile (@files) {
		    my $parent = XML::LibXML::Element->new( 'stack' );
		    $parent->setAttribute("name",$sfile//"");
		    if (-f $sfile) {
			$parent->setAttribute("age",(-M $sfile//""));
			$parent->setAttribute("size",size_in_mb(-s $sfile))
		    };
		    $node->addChild( $parent );
		};
		# put xml-structure into file
		if ($passok) {
		    farkdir::docsave($fpath,$doc);
		}
	    } else {
		my $parent = XML::LibXML::Element->new( 'stack' );
		$node->addChild( $parent );
		return "No files found.";
	    }
	}{message=>"Unable to findfiles: $filterDir,$filterFile,$filterDirMin,$filterDirMax",
	  stdout=>"never"}; #ignore output
	# report xml-structure
	print $doc->toString . "\n";
    } else {
	farkdir::term("Permission denied.");
    };
};

sub findJoinFile {
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
    if ($debug) { print "reading file: $fpath\n";}
    my ($sdir,$sfile) = farkdir::splitName($spath);
    my ($sroot, $sloc, $spriv) = farkdir::splitDir($sdir,"data");
    # xml
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    # read config file into memory
    my $passok=0;
    if (-e $fpath && $priv eq "rw") {
	$doc = $parser->parse_file($fpath);
	if ( ($node)=$doc->findnodes("join/join_config")) {
	    my $pass=($node->getAttribute("password")//"");
	    if ($pass ne $password) {
		if ($debug) { print "*** Invalid password: $fpath\n";}
		$passok=0;
		$node->setAttribute("password",        $password//"");
	    } else {
		$passok=1;
		if ($debug) { print "Password ok: $fpath\n";}
	    }
	} else {
	    farkdir::term("Corrupt file: ".$fpath);
	}
    } else {
	if ($debug) { print "strange file: $fpath '$priv'\n";}
	$doc = $parser->parse_string("<join><join_config/></join>");
	($node) = $doc->findnodes("join/join_config");
    }
    # put inventory of first join-file into xml-structure
    $node->setAttribute("file",      $ifile//"");
    $node->setAttribute("class",     $cls//"");
    $node->setAttribute("root",      $root//"");
    $node->setAttribute("location",  $loc//"");
    $node->setAttribute("status",    $priv//"");
    my @attributes=$node->findnodes("attribute");
    if (-f $spath) {
	my %names;
	foreach my $attribute (@attributes) {
	    my $name=$attribute->getAttribute("name");
	    $names{$name}=$attribute;
	    if ($debug) {print "Old attribute: $name\n";}
	}
	# scan files for internal attributes
	my $mode=0;
	my $lines=0;
	open(my $fh, '<:encoding(UTF-8)', $spath) or farkdir::term("Could not open '$spath'");
	while (my $row = <$fh>) {
	    chomp $row;
	    # check for the attributes block...
	    if ($mode==0) {
		if ($row =~ /^# TYPE:(\S+)/) {
		    if ($debug) {print "Type: $row\n";}
		    my $type=$1;
		    $node->setAttribute("cat", $type//"debug");
		} elsif ($row =~ /^# ATTRIBUTES:(\d+)/) {
		    if ($debug) {print "Attribute: $row\n";}
		    $lines=$1;
		    if ($lines>0){$mode=1;}
		} elsif ( $row =~ /^#/) { # this is a comment...
		    if ($debug) {print "Comment: $row\n";}
		} else {     # nothing more to do...
		    last;
		}
	    } elsif ($mode==1) {
		if ($row =~ /^# ([^\s]+):(.*\S)\s*$/) {
		    my $n=$1;
		    my $v=$2;
		    # overwrite internal variables
		    if ($debug) {print "Internal: $row '$n=>$v'\n";}
		    my $att=$names{$n};
		    if ($att ) {
			$att->setAttribute("value",$v);
		    } else {
			if ($debug) {print "*** No attribute: $n\n";}
		    };	
		} else {
		    if ($debug) {print "Unknown: $row\n";}
		}
		$lines=$lines-1;
		if ($lines==0) {
		    $mode=0;
		}
	    }
	}
	close($fh);
	#if ($passok) {
	#    # put xml-structure into file
	#    farkdir::docsave($fpath,$doc);
	#}
	# report xml-structure
	print $doc->toString . "\n";
    } else {
	farkdir::term("Unable to open '$spath'.");
    };
}


sub findClean {
    my $param =shift;
    my $cls=shift;
    my $ipath=$param->{file}[0];
    my $password=($param->{password}[0] // "");
    my $filterDir = ($param->{filterDir}->[0] // "/");
    my $filterFile = ($param->{filterFile}->[0] // "");
    my $filterAge = ($param->{filterAge}->[0] // "");
    # get config file paths...
    my ($idir,$ifile) = farkdir::splitName($ipath);
    my ($root, $loc, $priv) = farkdir::splitDir($idir,$cls);
    my $lpath = $loc.$ifile; # local path
    my $fpath = $root.$lpath; # full path
    # get filter paths...
    my ($filterroot, $filterloc, $filterpriv) = 
	farkdir::splitDir($filterDir,"output");
    # xml
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    # read old config file into memory
    my $passok=1;
    if (-e $fpath && $priv eq "rw") {
	$doc = $parser->parse_file($fpath);
	if ( ($node)=$doc->findnodes("clean/clean_config")) {
	    my $pass=($node->getAttribute("password")//"");
	    if ($pass ne $password) {
		$passok=0;
		$doc = $parser->parse_string("<clean><clean_config/></clean>");
		($node) = $doc->findnodes("clean/clean_config");
	    }
	} else {
	    farkdir::term("Corrupt file: ".$fpath);
	}
    } else {
	$doc = $parser->parse_string("<clean><clean_config/></clean>");
	($node) = $doc->findnodes("clean/clean_config");
    }
    # put inventory of first clean-file into xml-structure
    my @oldNodes=$node->findnodes("stack");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    };
    $node->setAttribute("file",      $ifile//"");
    $node->setAttribute("class",     $cls//"");
    $node->setAttribute("root",      $root//"");
    $node->setAttribute("location",  $loc//"");
    $node->setAttribute("status",    $priv//"");
    if ($filterpriv eq "ro" || $filterpriv eq "rw") {
    	farkdir::sandbox {
	    $node->setAttribute("password",        $password//"");
	    $node->setAttribute("filterDir",       $filterDir//"");
	    $node->setAttribute("filterFile",      $filterFile//"");
	    $node->setAttribute("filterAge",       $filterAge//0);
	    my @files=farkdir::FindFiles($filterDir,$filterFile,
					 $filterAge,9999,10);
	    if (@files) {
		$node->setAttribute("hits",            scalar @files);
		foreach my $sfile (@files) {
		    my $parent = XML::LibXML::Element->new( 'stack' );
		    $parent->setAttribute("name",$sfile//"");
		    if (-f $sfile) {
			$parent->setAttribute("age",(-M $sfile//""));
			$parent->setAttribute("size",size_in_mb(-s $sfile))
		    };
		    $node->addChild( $parent );
		};
		# put xml-structure into file
		if ($passok) {
		    farkdir::docsave($fpath,$doc);
		}
	    } else {
		my $parent = XML::LibXML::Element->new( 'stack' );
		$node->addChild( $parent );
		return "No files found.";
	    }
	}{message=>"Unable to findfiles: $filterDir,$filterFile,$filterAge",
	  stdout=>"never"}; #ignore output
	# report xml-structure
	print $doc->toString . "\n";
    } else {
	farkdir::term("Permission denied.");
    };
};

sub tostring (&) {
    my $s;
    open local *STDOUT, '>', \$s;
    shift->();
    $s
}

sub size_in_mb {
    my $size=shift;
    my $text= reverse (sprintf "%.2fMb", $size/(1024 * 1024));
    $text =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
    $text=reverse $text;
    return $text;
}
