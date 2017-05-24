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
use File::Basename;
use Data::Dumper;
use File::Find;
#dont know if you need this: sudo apt-get install libpath-tiny-perl
#but you need this: sudo apt-get install libcapture-tiny-perl
use Capture::Tiny 'capture';
#
my $user=$ENV{USERNAME} // "www";
my $modelDir="/home/".$user."/pub/model/";
my $obsDir="/home/".$user."/pub/obs/";
#
my $ref=CGI->new();

$XML::LibXML::skipXMLDeclaration = 1;
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
my $param= $ref->{param};
if (! defined $param->{type}) {&term("Undefined type.".Dumper($param))};
if ($param->{type}->[0] eq "model") {
    &findModel($param,$modelDir);
} elsif ($param->{type}->[0] eq "modelfile") {
    &findModelFile($param,$modelDir);
} elsif ($param->{type}->[0] eq "obs") {
    &findObs($param,$obsDir);
} elsif ($param->{type}->[0] eq "obsfile") {
    &findObsFile($param,$obsDir);
}

sub findModel {
    my $param =shift;
    my $dir =shift;
    my $base=basename($param->{file}[0]);
    my $file = $dir . $base;
    my $password=($param->{password}[0] // "");
    my $filterDir = ($param->{filterDir}->[0]||"/tmp");
    my $filter = ($param->{filter}->[0] // "");
    my $index = ($param->{index}->[0] // "");
    # read config file into memory
    my $parser = XML::LibXML->new();
    my $passok=1;
    my $doc;
    my $node;
    if (-e $file) {
	$doc = $parser->parse_file($file);
	if ( ($node)=$doc->findnodes("model/model_config")) {
	    my $pass=($node->getAttribute("password")||"");
	    if ($pass ne $password) {
		my $passok=0;
	    }
	} else {
	    &term("Corrupt file: ".$file);
	}
    } else {
	$doc = $parser->parse_string("<model><model_config/></model>");
	($node) = $doc->findnodes("model/model_config");
    }
    my @oldNodes=$node->findnodes("stack");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    };
    my @files=();
    find({wanted => sub {
	if (-f $File::Find::name && $File::Find::name =~ m/$filter/) {
	    push(@files, $File::Find::name);
	}
	  }}, $filterDir);
    if (@files) {
	$node->setAttribute("password",        $password);
	$node->setAttribute("file",            $base);
	$node->setAttribute("fileFilterDir",   $filterDir);
	$node->setAttribute("fileFilter",      $filter);
	$node->setAttribute("fileFilter",      $filter);
	$node->setAttribute("index",            $index);
	$node->setAttribute("hits",            scalar @files);
	foreach my $sfile (@files) {
	    my $parent = XML::LibXML::Element->new( 'stack' );
	    $parent->setAttribute("name",$sfile);
	    $node->addChild( $parent );
	};
	# put xml-structure into file
	if ($passok) {
	    if (open(my $fh, '>', $file)) {
		print $fh $doc->toString;
		close $fh;
		chmod 0666, $file;
	    } else {
		&term("Unable to open:".$file);
	    };
	}
	# report xml-structure
	print $doc->toString . "\n";
    } else {
	&term("No files found.");
    }
};

sub findModelFile {
    my $param =shift;
    my $dir =shift;
    my $base=basename($param->{file}[0]);
    my $file = $dir . $base;
    my $password=($param->{password}[0] // "");
    my $sfile = $param->{target}[0] // "";
    my $passok=1;
    # read config file into memory
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    if (-e $file) {
	$doc = $parser->parse_file($file);
	if ( ($node)=$doc->findnodes("model/model_config")) {
	    my $pass=($node->getAttribute("password")||"");
	    if ($pass ne $password) {
		$passok=0;
	    }
	} else {
	    &term("Corrupt file: ".$file);
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
    if ($sfile) {
	my $log="";
	eval {
	    $log=capture {
		my $index=$node->getAttribute("index");
		my $fark=fark->open();
		$fark->clearModelFileStack($index);
		$fark->pushModelFile($sfile);
		#$fark->getNextModelFile();
		my $data = $fark->peekModelFile();
		if ($data) {
		    #$data->printTree("Model:");
		    foreach my $index (sort keys $data->{file}->{variable}) {
			my $parent = XML::LibXML::Element->new( 'variable' );
			if (defined $data->{file}->{variable}->{$index}->{dimension}) {
			    my $dims="";
			    foreach my $dim (sort keys $data->{file}->{variable}->{$index}->{dimension}) {
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
	my $ret=$@;if ($ret) {term($ret);}
	if ($passok) {
	    # put xml-structure into file
	    if (open(my $fh, '>', $file)) {
		print $fh $doc->toString;
		close $fh;
		chmod 0666, $file;
	    } else {
		&term("Unable to open:".$file);
	    };
	}
    };
    # report xml-structure
    print $doc->toString . "\n";
};

sub findObs {
    my $param =shift;
    my $dir =shift;
    my $base=basename($param->{file}[0]);
    my $file = $dir . $base;
    my $password=($param->{password}[0] // "");
    my $filterDir = ($param->{filterDir}->[0]||"/tmp");
    my $filter = ($param->{filter}->[0] // ".*");
    my $bufrType=($param->{bufrType}[0] // "");
    my $subType=($param->{subType}[0] // "");
    my $typeInfo=($param->{typeInfo}[0] // "");
    my $indexTarget=($param->{indexTarget}[0] // "");
    my $indexExp=($param->{indexExp}[0] // "");
    my $table = ($param->{table}->[0] // "");
    # read config file into memory
    my $parser = XML::LibXML->new();
    my $passok=1;
    my $doc;
    my $node;
    if (-e $file) {
	$doc = $parser->parse_file($file);
	if ( ($node)=$doc->findnodes("obs/obs_config")) {
	    my $pass=($node->getAttribute("password")||"");
	    if ($pass ne $password) {
		$passok=0;
	    }
	} else {
	    &term("Corrupt file: ".$file);
	}
    } else {
	$doc = $parser->parse_string("<obs><obs_config/></obs>");
	($node) = $doc->findnodes("obs/obs_config");
    }
    my @oldNodes=$node->findnodes("stack");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    };
    my @files=();
    find({wanted => sub {
	if (-f $File::Find::name && $File::Find::name =~ m/$filter/) {
	    push(@files, $File::Find::name);
	}
	  }}, $filterDir);
    if (@files) {
	$node->setAttribute("password",        $password);
	$node->setAttribute("file",            $base);
	$node->setAttribute("fileFilterDir",   $filterDir);
	$node->setAttribute("fileFilter",      $filter);
	$node->setAttribute("tablePath",       $table);
	$node->setAttribute("bufrType",        $bufrType);
	$node->setAttribute("subType",         $subType);
	$node->setAttribute("typeInfo",        $typeInfo);
	$node->setAttribute("indexTarget",      $indexTarget);
	$node->setAttribute("indexExp",         $indexExp);
	$node->setAttribute("hits",            scalar @files);
	foreach my $sfile (@files) {
	    my $parent = XML::LibXML::Element->new( 'stack' );
	    $parent->setAttribute("name",$sfile);
	    $node->addChild( $parent );
	};
	# put xml-structure into file
	if ($passok) {
	    if (open(my $fh, '>', $file)) {
		print $fh $doc->toString;
		close $fh;
		chmod 0666, $file;
	    } else {
		&term("Unable to open:".$file);
	    }
	};
	# report xml-structure
	print $doc->toString . "\n";
    } else {
	&term("No files found.");
    }
};

sub findObsFile {
    my $param =shift;
    my $dir =shift;
    my $base=basename($param->{file}[0]);
    my $file = $dir . $base;
    my $password=($param->{password}[0] // "");
    my $sfile=$param->{target}[0] // "";
    my $obsTargets = ($param->{obsTargets}->[0] // "");
    my $indexTarget = ($param->{indexTarget}->[0]);
    my $indexExp = ($param->{indexExp}->[0]);
    my $bufrType=($param->{bufrType}->[0]);
    my $subType=($param->{subType}->[0]);
    my $typeInfo=($param->{typeInfo}[0]);
    my $passok=1;
    # read config file into memory
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    if (-e $file) {
	$doc = $parser->parse_file($file);
	if ( ($node)=$doc->findnodes("obs/obs_config")) {
	    my $pass=($node->getAttribute("password")||"");
	    if ($pass ne $password) {
		$passok=0;
	    }
	} else {
	    &term("Corrupt file: ".$file);
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
    if ($sfile) {
	my $log="";
	eval {
	    $log=capture {
		my $table=$node->getAttribute("tablePath");
		if (! defined $indexTarget) {$indexTarget=$node->getAttribute("indexTarget");}
		if (! defined $indexExp) {$indexExp=$node->getAttribute("indexExp");}
		if (! defined $bufrType) {$bufrType=$node->getAttribute("bufrType");}
		if (! defined $subType) {$subType=$node->getAttribute("subType");}
		if (! defined $typeInfo) {$typeInfo=$node->getAttribute("typeInfo");}
		#print "Starting with:$bufrType,$subType\n";
	        my $fark=fark->open();
		$fark->setObservationTablePath($table);
		if (defined $bufrType && defined $subType) {
		    $fark->setObservationType($bufrType,$subType);
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
				    $fark->pushObservationTarget(@items); # name,pos,descr,info,min,max
				}
			    }
			}
		    }
		    if ($indexTarget && $indexExp) {
			print "Setting observation index $indexTarget, $indexExp\n";
			$fark->setObservationIndex($indexTarget,$indexExp);
		    } else {
			die "No observation index specified.";
		    }
		} else {
		    die "No bufr/subType specified.";
		}
		$fark->clearObservationFileStack();
		$fark->pushObservationFile($sfile);
		#$fark->getNextObservationFile();
		my $data = $fark->peekObservationFile();
		if ($data) {
		    #$data->printTree("Observation:");
		    if (defined $data->{file}->{"index"}->{start}) {$node->setAttribute("start",$data->{file}->{"index"}->{start});};
		    if (defined $data->{file}->{"index"}->{stop} ) {$node->setAttribute("stop",$data->{file}->{"index"}->{stop});}
		    if (defined $data->{file}->{type}) {
			foreach my $type (sort keys $data->{file}->{type}) {
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
				foreach my $subtype (keys $data->{file}->{type}->{$type}->{subtype}) {
				    my $parent = XML::LibXML::Element->new( 'bufr' );
				    $parent->setAttribute("bufrType",$type);
				    $parent->setAttribute("subType",$subtype);
				    $parent->setAttribute("cnt",$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{"message count"});
				    $parent->setAttribute("info",$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{"description"});
				    my $seqno=$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{seqno};
				    my $name=$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{name};
				    my $unit=$data->{file}->{type}->{$type}->{subtype}->{$subtype}->{unit};
				    if (defined $seqno) {
					foreach my $pos (sort {$a<=>$b} keys $seqno) {
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
	my $ret=$@;if ($ret) {term($ret);}
	# put xml-structure into file
	if ($passok) {
	    if (open(my $fh, '>', $file)) {
		print $fh $doc->toString;
		close $fh;
		chmod 0666, $file;
	    } else {
		&term("Unable to open:".$file);
	    }
	}
	# report xml-structure
	print $doc->toString . "\n";
    } else {
	&term("No files found.");
    }
};

sub term {
    my $msg=shift;
    $msg=~s/[^a-zA-Z0-9 _\-\+\.\,\/\:\[\]\(\)]/ /g;
    $msg=~s/ +/ /g;
    print "<error message='".$msg."'/>\n";
    exit 1;
}

sub tostring (&) {
  my $s;
  open local *STDOUT, '>', \$s;
  shift->();
  $s
}
