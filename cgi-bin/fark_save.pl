#!/usr/bin/perl -w
#
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use Capture::Tiny 'capture';
use Data::Dumper;
use farkdir;
#
#  config directory
#
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
#
my $ref=CGI->new();
#
my $param= $ref->{param};
if (! defined $param->{type}) {farkdir::term("Undefined type.".Dumper($param))};
if ($param->{type}->[0] eq "model") {
    &saveModel($param);
} elsif ($param->{type}->[0] eq "obs") {
    &saveObs($param);
} elsif ($param->{type}->[0] eq "coloc") {
    &saveColoc($param);
} elsif ($param->{type}->[0] eq "plot") {
    &savePlot($param);
} elsif ($param->{type}->[0] eq "auto") {
    &saveAuto($param);
}


sub saveModel {
    my $param = shift;
    my $cls=$param->{type}->[0] || "";
    my $password=($param->{password}[0] // "");
    my $filterDir=($param->{filterDir}[0] // "");
    my $filterFile=($param->{filterFile}[0] // "");
    my $hits=($param->{hits}[0] // "");
    my $indexTarget=($param->{indexTarget}[0] // "");
    my $indexVariable=($param->{indexVariable}[0] // "");
    my $variables=($param->{variables}[0]// "");
    my $dimensions=($param->{dimensions}[0]// "");
    my $stack=($param->{stack}[0]// "");
    #
    if (! defined ($param->{file}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{file}->[0]||"";
    my ($dir, $file) = farkdir::splitName($ifile);
    my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
    my $fpath=$root . $loc;
    if (-d  $fpath && $priv eq "rw") {
	my $path=$fpath . $file;
	# check password
	my $parser = XML::LibXML->new();
	my $doc;
	my $node;
	if (-e $path) {
	    $doc = $parser->parse_file($path);
	    if ( ($node)=$doc->findnodes("model/model_config")) {
		my $pass=($node->getAttribute("password")||"");
		if ($pass ne $password) {
		    farkdir::term("Invalid password for file: ".$path);
		}
	    } else {
		my $parent=$doc->createElement("model");
		$node=$parent->createElement("model_config");
	    };
	} else {
	    $doc = $parser->parse_string("<model><model_config/></model>");
	    ($node) = $doc->findnodes("model/model_config");
	}
	$node->setAttribute("path",         $loc . $file);
	$node->setAttribute("root",         $root);
	$node->setAttribute("location",     $loc);
	$node->setAttribute("status",       $priv);
	$node->setAttribute("password",     $password);
	$node->setAttribute("file",         $file);
	$node->setAttribute("filterDir",    $filterDir);
	$node->setAttribute("filterFile",   $filterFile);
	$node->setAttribute("hits",         $hits);
	$node->setAttribute("indexTarget",  $indexTarget);
	$node->setAttribute("indexVariable",$indexVariable);
	my @oldNodes=$node->findnodes("stack");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	};
	my @sfiles = split (/\|/, $stack,-1);
	if (@sfiles) { 
	    foreach my $sfile (@sfiles) {
		my $parent = XML::LibXML::Element->new( 'stack' );
		$parent->setAttribute("name",$sfile);
		$node->addChild( $parent );
		$node->setAttribute("stack",           $sfile);
	    }
	}
	@oldNodes=$node->findnodes('variable');
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	};
	my @lines = split (/\|/, $variables,-1);
	if (@lines) { 
	    foreach my $line (@lines) {
		my @items = split (/\~/, $line,-1);
		my $name=$items[0];
		my $dims=$items[1] // "";
		if ($name) {
		    my $parent = XML::LibXML::Element->new( 'variable' );
		    $parent->setAttribute("name",$name);
		    $parent->setAttribute("dims",$dims);
		    $node->addChild( $parent );
		}
	    }
	}
	@oldNodes=$node->findnodes('dimension');
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	};
	@lines = split (/\|/, $dimensions,-1);
	if (@lines) { 
	    foreach my $line (@lines) {
		my @items = split (/\~/, $line,-1);
		my $name=$items[0];
		my $dimv=$items[1] // "";
		if ($name) {
		    my $parent = XML::LibXML::Element->new( 'dimension' );
		    $parent->setAttribute("name",$name);
		    $parent->setAttribute("value",$dimv);
		    $node->addChild( $parent );
		}
	    }
	}
	# save XML to file
	if (open(my $fh, '>', $path)) {
	    print $fh $doc->toString;
	    close $fh;
	    chmod 0666, $path;
	} else {
	    farkdir::term("Unable to open: '".$path."'");
	}
	print "<save message='success'/>\n";
    } else {
	farkdir::term("Permission denied: '".$fpath."'");
    }
}
sub saveObs {
    my $param = shift;
    my $cls=$param->{type}->[0] || "";
    my $password=($param->{password}[0] // "");
    my $filterDir=($param->{filterDir}[0] // "*");
    my $filterFile=($param->{filterFile}[0] // "*");
    my $stack=($param->{stack}[0] // "*");
    my $table=($param->{table}[0] // "");
    my $bufrType=($param->{bufrType}[0] // "");
    my $subType=($param->{subType}[0] // "");
    my $typeInfo=($param->{typeInfo}[0] // "");
    my $indexTarget=($param->{indexTarget}[0] // "");
    my $indexExp=($param->{indexExp}[0] // "");
    my $obsTargets=($param->{obsTargets}[0] // "");
    #
    if (! defined ($param->{file}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{file}->[0]||"";
    my ($dir, $file) = farkdir::splitName($ifile);
    my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
    my $fpath=$root . $loc;
    if (-d  $fpath && $priv eq "rw") {
	my $path=$fpath . $file;
	# check password
	my $parser = XML::LibXML->new();
	my $doc;
	my $node;
	if (-e $path) {
	    $doc = $parser->parse_file($path);
	    if ( ($node)=$doc->findnodes("obs/obs_config")) {
		my $pass=($node->getAttribute("password")||"");
		if ($pass ne $password) {
		    farkdir::term("Invalid password for file: ".$file);
		}
	    } else {
		my $parent=$doc->createElement("obs");
		$node=$parent->createElement("obs_config");
	    };
	} else {
	    $doc = $parser->parse_string("<obs><obs_config/></obs>");
	    ($node) = $doc->findnodes("obs/obs_config");
	}
	$node->setAttribute("path",        $loc . $file);
	$node->setAttribute("root",        $root);
	$node->setAttribute("location",    $loc);
	$node->setAttribute("status",      $priv);
	$node->setAttribute("password",    $password);
	$node->setAttribute("file",        $file);
	$node->setAttribute("filterDir",   $filterDir);
	$node->setAttribute("filterFile",  $filterFile);
	$node->setAttribute("tablePath",   $table);
	$node->setAttribute("bufrType",    $bufrType);
	$node->setAttribute("subType",     $subType);
	$node->setAttribute("typeInfo",    $typeInfo);
	$node->setAttribute("indexTarget", $indexTarget);
	$node->setAttribute("indexExp",    $indexExp);
	my @oldNodes=$node->findnodes("stack");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	};
	my @sfiles = split (/\|/, $stack,-1);
	if (@sfiles) { 
	    foreach my $sfile (@sfiles) {
		my $parent = XML::LibXML::Element->new( 'stack' );
		$parent->setAttribute("name",$sfile);
		$node->addChild( $parent );
		$node->setAttribute("stack",           $sfile);
	    }
	}
	# remove target nodes...
	@oldNodes=$node->findnodes("target");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	}
	if ($obsTargets) {
	    my $wrote = 0;
	    my @lines = split (/\|/, $obsTargets,-1);
	    if (@lines) { 
		foreach my $line (@lines) {
		    #print "Obstargets: $line\n";
		    my @items=split (/\~/, $line,-1);
		    my $len=$#items;
		    if ($len == 5) {
			$wrote++;
			my $parent = XML::LibXML::Element->new( 'target' );
			$parent->setAttribute("name",$items[0]);
			$parent->setAttribute("pos",$items[1]);
			$parent->setAttribute("descr",$items[2]);
			$parent->setAttribute("info",$items[3]);
			$parent->setAttribute("min",$items[4]);
			$parent->setAttribute("max",$items[5]);
			$node->addChild( $parent );
		    }
		}
	    } 
	    if ($wrote == 0) {
		my $parent = XML::LibXML::Element->new( 'target' );
		$node->addChild( $parent );
	    }
	};
	# save XML to file
	if (open(my $fh, '>', $path)) {
	    print $fh $doc->toString;
	    close $fh; 
	    chmod 0666, $path;
	} else {
	    farkdir::term("Unable to open:".$path);
	}
	print "<save message='success'/>\n";
    } else {
	farkdir::term("Permission denied: '".$fpath."'");
    }
}
sub saveColoc {
    my $param = shift;
    my $cls=$param->{type}->[0] || "";
    my $password     = ($param->{password}[0] // "");
    my $host         = ($param->{host}[0] // "localhost");
    my $xml          = ($param->{xml}[0] // "");
    my $filter       = ($param->{filter}[0] // "");
    my $modelFile    = ($param->{modelFile}[0] // "");
    my $modelStart   = ($param->{modelStart}[0] // "");
    my $modelStop    = ($param->{modelStop}[0] // "");
    my $indexExp     = ($param->{indexExp}[0] // "");
    my $modelTargets = ($param->{modelTargets}[0] // "");
    my $modelDefault = ($param->{modelDefault}[0] // "");
    my $obsFile      = ($param->{obsFile}[0] // "");
    my $obsFilter    = ($param->{obsFilter}[0] // "");
    my $obsStart     = ($param->{obsStart}[0] // "");
    my $obsStop      = ($param->{obsStop}[0] // "");
    my $obsTargets   = ($param->{obsTargets}[0] // "");
    my $matchRules   = ($param->{matchRules}[0] // "");
    #
    if (! defined ($param->{file}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{file}->[0]||"";
    my ($dir, $file) = farkdir::splitName($ifile);
    my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
    my $fpath=$root . $loc;
    if (-d  $fpath && $priv eq "rw") {
	my $path=$fpath . $file;
	# check password
	my $parser = XML::LibXML->new();
	my $doc;
	my $node;
	if (-e $path) {
	    $doc = $parser->parse_file($path);
	    if ( ($node)=$doc->findnodes("coloc/coloc_config")) {
		my $pass=($node->getAttribute("password")||"");
		if ($pass ne $password) {
		    farkdir::term("Invalid password for file: ".$path);
		}
	    } else {
		my $parent=$doc->createElement("coloc");
		$node=$parent->createElement("coloc_config");
	    };
	} else {
	    $doc = $parser->parse_string("<coloc><coloc_config/></coloc>");
	    ($node) = $doc->findnodes("coloc/coloc_config");
	}
	$node->setAttribute("path",        $loc . $file);
	$node->setAttribute("root",        $root);
	$node->setAttribute("location",    $loc);
	$node->setAttribute("status",      $priv);
	$node->setAttribute("password",    $password);
	$node->setAttribute("file",        $file);
	$node->setAttribute("host",        $host);
	$node->setAttribute("xml",         $xml);
	$node->setAttribute("filter",      $filter);
	$node->setAttribute("modelFile",   $modelFile);
	$node->setAttribute("modelStart",  $modelStart);
	$node->setAttribute("modelStop",   $modelStop);
	$node->setAttribute("indexExp",    $indexExp);
	$node->setAttribute("obsFile",     $obsFile);
	$node->setAttribute("obsFilter",   $obsFilter);
	$node->setAttribute("obsStart",    $obsStart);
	$node->setAttribute("obsStop",     $obsStop);
	# remove model nodes...
	my @oldNodes=$node->findnodes("modelTarget");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	}
	my @lines = split (/\|/, $modelTargets,-1);
	if (@lines) { 
	    foreach my $line (@lines) {
		if ($line ne "") {
		    my @items=split (/\~/, $line,-1);
		    my $len=$#items;
		    if ($len == 3) {
			my $parent = XML::LibXML::Element->new( 'modelTarget' );
			$parent->setAttribute("name",$items[0]);
			$parent->setAttribute("variable",$items[1]);
			$parent->setAttribute("min",$items[2]);
			$parent->setAttribute("max",$items[3]);
			$node->addChild( $parent );
		    }
		}
	    }
	}
	# remove model nodes...
	@oldNodes=$node->findnodes("modelDefault");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	};
	my @defs = split (/\[/, $modelDefault,-1);
	if (@defs) { 
	    foreach my $def (@defs) {
		@lines = split (/\|/, $def,-1);
		if (@lines) { 
		    my $default = XML::LibXML::Element->new( 'modelDefault' );
		    $node->addChild( $default );
		    foreach my $line (@lines) {
			if ($line ne "") {
			    my @items=split (/\~/, $line,-1);
			    my $len=$#items;
			    if ($len == 0) {
				$default->setAttribute("info",$items[0]);
			    } elsif ($len == 1) {
				my $parent = XML::LibXML::Element->new( 'def' );
				$parent->setAttribute("name",$items[0]);
				$parent->setAttribute("value",$items[1]);
				$default->addChild( $parent );
			    }
			}
		    }
		}
	    }
	}
	# remove obs nodes...
	@oldNodes=$node->findnodes("obsTarget");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	}
	@lines = split (/\|/, $obsTargets,-1);
	if (@lines) { 
	    foreach my $line (@lines) {
		if ($line ne "") {
		    my @items=split (/\~/, $line,-1);
		    my $len=$#items;
		    if ($len == 5) {
			my $parent = XML::LibXML::Element->new( 'obsTarget' );
			$parent->setAttribute("name",$items[0]);
			$parent->setAttribute("pos",$items[1]);
			$parent->setAttribute("descr",$items[2]);
			$parent->setAttribute("info",$items[3]);
			$parent->setAttribute("min",$items[4]);
			$parent->setAttribute("max",$items[5]);
			$node->addChild( $parent );
		    }
		}
	    }
	}
	@oldNodes=$node->findnodes("matchRules");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	}
	@lines = split (/\|/, $matchRules,-1);
	if (@lines) { 
	    foreach my $line (@lines) {
		if ($line ne "") {
		    my @items=split (/\~/, $line,-1);
		    my $len=$#items;
		    if ($len == 1) {
			my $parent = XML::LibXML::Element->new( 'matchRules' );
			$parent->setAttribute("name",$items[0]);
			$parent->setAttribute("expression",$items[1]);
			$node->addChild( $parent );
		    }
		}
	    }
	}
	# save XML to file
	if (open(my $fh, '>', $path)) {
	    print $fh $doc->toString;
	    close $fh;
	    chmod 0666, $path;
	} else {
	    farkdir::term("Unable to open:".$path);
	}
	print "<save message='success'/>\n";
    } else {
	farkdir::term("Permission denied: '".$fpath."'");
    }
}
sub savePlot {
    my $param = shift;
    my $cls=$param->{type}->[0] || "";
    my $password=($param->{password}[0] // "");
    my $table=($param->{table}[0] // "");
    my $graphics=($param->{graphics}[0] // "");
    my $cat=($param->{cat}[0] // "");
    my $plotSets=($param->{sets}[0] // "");
    my $plotAttrs=($param->{attributes}[0] // "");
    #
    if (! defined ($param->{file}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{file}->[0]||"";
    my ($dir, $file) = farkdir::splitName($ifile);
    my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
    my $fpath=$root . $loc;
    if (-d  $fpath && $priv eq "rw") {
	my $path=$fpath . $file;
	# check password
	my $parser = XML::LibXML->new();
	my $doc;
	my $node;
	if (-e $path) {
	    $doc = $parser->parse_file($path);
	    if ( ($node)=$doc->findnodes("plot/plot_config")) {
		my $pass=($node->getAttribute("password")||"");
		if ($pass ne $password) {
		    farkdir::term("Invalid password for file: ".$path);
		}
	    } else {
		my $parent=$doc->createElement("plot");
		$node=$parent->createElement("plot_config");
	    };
	} else {
	    $doc = $parser->parse_string("<plot><plot_config/></plot>");
	    ($node) = $doc->findnodes("plot/plot_config");
	}
	$node->setAttribute("path",        $loc . $file);
	$node->setAttribute("root",        $root);
	$node->setAttribute("location",    $loc);
	$node->setAttribute("status",      $priv);
	$node->setAttribute("password",    $password);
	$node->setAttribute("file",        $file);
	$node->setAttribute("table",       $table);
	$node->setAttribute("graphics",    $graphics);
	$node->setAttribute("cat",         $cat);
	# remove target nodes...
	my @oldNodes=$node->findnodes("set");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	}
	if ($plotSets) {
	    my @lines = split (/\|/, $plotSets,-1);
	    if (@lines) { 
		foreach my $line (@lines) {
		    #print "Plottargets: $line\n";
		    my @items=split (/\~/, $line,-1);
		    my $len=$#items;
		    if ($len == 4) {
			my $parent = XML::LibXML::Element->new( 'set' );
			$parent->setAttribute("name",$items[0]);
			$parent->setAttribute("coloc",$items[1]);
			$parent->setAttribute("x",$items[2]);
			$parent->setAttribute("y",$items[3]);
			$parent->setAttribute("legend",$items[4]);
			$node->addChild( $parent );
		    }
		}
	    }
	};
	@oldNodes=$node->findnodes("attribute");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	}
	if ($plotAttrs) {
	    my @lines = split (/\|/, $plotAttrs,-1);
	    if (@lines) { 
		foreach my $line (@lines) {
		    #print "Plotattrs: $line\n";
		    my @items=split (/\~/, $line,-1);
		    my $len=$#items;
		    if ($len == 1) {
			my $parent = XML::LibXML::Element->new( 'attribute' );
			$parent->setAttribute("name",$items[0]);
			$parent->setAttribute("value",$items[1]);
			$node->addChild( $parent );
		    }
		}
	    }
	};
	# save XML to file
	if (open(my $fh, '>', $path)) {
	    print $fh $doc->toString;
	    close $fh; 
	    chmod 0666, $path;
	} else {
	    farkdir::term("Unable to open:".$path);
	}
	print "<save message='success'/>\n";
    } else {
	farkdir::term("Permission denied: '".$fpath."'");
    }
}

sub saveAuto {
    my $param = shift;
    my $cls=$param->{type}->[0] || "";
    my $password=($param->{password}[0] // "");
    my $modelFiles=($param->{modelFiles}[0] // "");
    my $obsFiles=($param->{obsFiles}[0] // "");
    my $colocFiles=($param->{colocFiles}[0] // "");
    my $plotFiles=($param->{plotFiles}[0] // "");
    if (! defined ($param->{root}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{root}->[0]||"";
    my ($dir, $file) = farkdir::splitName($ifile);
    my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
    my $fpath=$root . $loc;
    if (-d  $fpath && $priv eq "rw") {
	my $path=$fpath . $file;
	# check password
	my $parser = XML::LibXML->new();
	my $doc;
	my $node;
	if (-e $path) {
	    $doc = $parser->parse_file($path);
	    if ( ($node)=$doc->findnodes("auto/auto_config")) {
		my $pass=($node->getAttribute("password")||"");
		if ($pass ne $password) {
		    farkdir::term("Invalid password for file: ".$path);
		}
	    } else {
		my $parent=$doc->createElement("auto");
		$node=$parent->createElement("auto_config");
	    };
	} else {
	    $doc = $parser->parse_string("<auto><auto_config/></auto>");
	    ($node) = $doc->findnodes("auto/auto_config");
	}
	$node->setAttribute("path",        $loc . $file);
	$node->setAttribute("root",        $root);
	$node->setAttribute("location",    $loc);
	$node->setAttribute("status",      $priv);
	$node->setAttribute("password",    $password);
	$node->setAttribute("file",        $file);
	# remove model nodes...
	my @oldNodes=$node->findnodes("model");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	}
	# make list of new nodes
	my @lines = split (/\|/, $modelFiles,-1);
	if (@lines) { 
	    foreach my $line (@lines) {
		if ($line ne "") {
		    my @items=split (/\~/, $line,-1);
		    my $len=$#items;
		    if ($len == 3) {
			my $parent = XML::LibXML::Element->new( 'model' );
			$parent->setAttribute("file",$items[0]);
			$parent->setAttribute("last",$items[1]);
			$parent->setAttribute("info",$items[2]);
			$parent->setAttribute("auto",$items[3]);
			$node->addChild( $parent );
		    }
		}
	    }
	}
	# remove obs nodes...
	@oldNodes=$node->findnodes("obs");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	}
	@lines = split (/\|/, $obsFiles,-1);
	if (@lines) { 
	    foreach my $line (@lines) {
		if ($line ne "") {
		    my @items=split (/\~/, $line,-1);
		    my $len=$#items;
		    if ($len == 3) {
			my $parent = XML::LibXML::Element->new( 'obs' );
			$parent->setAttribute("file",$items[0]);
			$parent->setAttribute("last",$items[1]);
			$parent->setAttribute("info",$items[2]);
			$parent->setAttribute("auto",$items[3]);
			$node->addChild( $parent );
		    }
		}
	    }
	}
	# remove plot nodes...
	@oldNodes=$node->findnodes("coloc");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	}
	@lines = split (/\|/, $colocFiles,-1);
	if (@lines) { 
	    foreach my $line (@lines) {
		if ($line ne "") {
		    my @items=split (/\~/, $line,-1);
		    my $len=$#items;
		    if ($len == 3) {
			my $parent = XML::LibXML::Element->new( 'coloc' );
			$parent->setAttribute("file",$items[0]);
			$parent->setAttribute("last",$items[1]);
			$parent->setAttribute("info",$items[2]);
			$parent->setAttribute("auto",$items[3]);
			$node->addChild( $parent );
		    }
		}
	    }
	}
	# remove plot nodes...
	@oldNodes=$node->findnodes("plot");
	foreach my $oldNode (@oldNodes) {
	    $node->removeChild($oldNode);
	}
	@lines = split (/\|/, $plotFiles,-1);
	if (@lines) { 
	    foreach my $line (@lines) {
		if ($line ne "") {
		    my @items=split (/\~/, $line,-1);
		    my $len=$#items;
		    if ($len == 3) {
			my $parent = XML::LibXML::Element->new( 'plot' );
			$parent->setAttribute("file",$items[0]);
			$parent->setAttribute("last",$items[1]);
			$parent->setAttribute("info",$items[2]);
			$parent->setAttribute("auto",$items[3]);
			$node->addChild( $parent );
		    }
		}
	    }
	}
	# save XML to file
	if (open(my $fh, '>', $path)) {
	    print $fh $doc->toString;
	    close $fh;
	    chmod 0666, $path;
	} else {
	    farkdir::term("Unable to open:".$path);
	}
	print "<save message='success'/>\n";
    } else {
	farkdir::term("Permission denied: '".$fpath."' $priv");
    }
}
