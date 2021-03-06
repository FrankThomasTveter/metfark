#!/usr/bin/perl -w
#
use strict;
use lib "/home/ubuntu/perl5/lib/perl5/x86_64-linux-gnu-thread-multi";
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
} elsif ($param->{type}->[0] eq "table") {
    &saveTable($param);
} elsif ($param->{type}->[0] eq "join") {
    &saveJoin($param);
} elsif ($param->{type}->[0] eq "plot") {
    &savePlot($param);
} elsif ($param->{type}->[0] eq "rerun") {
    &saveRerun($param);
} elsif ($param->{type}->[0] eq "clean") {
    &saveClean($param);
} elsif ($param->{type}->[0] eq "exec") {
    &saveExec($param);
}

sub saveModel {
    my $param = shift;
    my $cls=$param->{type}->[0] // "";
    my $password=($param->{password}[0] // "");
    my $filterDir=($param->{filterDir}[0] // "");
    my $filterDirMin=($param->{filterDirMin}[0] // "");
    my $filterDirMax=($param->{filterDirMax}[0] // "");
    my $filterFile=($param->{filterFile}[0] // "");
    my $hits=($param->{hits}[0] // "");
    my $indexTarget=($param->{indexTarget}[0] // "");
    my $indexVariable=($param->{indexVariable}[0] // "");
    my $variables=($param->{variables}[0]// "");
    my $dimensions=($param->{dimensions}[0]// "");
    my $stack=($param->{stack}[0]// "");
    #
    if (! defined ($param->{file}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{file}->[0]//"";
    my $logfile="/tmp/fark_save.tmp";
    farkdir::touchFile($logfile);
    farkdir::sandbox {
	my ($dir, $file) = farkdir::splitName($ifile);
	my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
	my $fpath=$root . $loc;
	if (-d  $fpath && $priv eq "rw") {
	    my $path=$fpath . $file;
	    # check password
	    my $parser = XML::LibXML->new();
	    my $doc;
	    my $node;
	    if (-d $path) {
		farkdir::term("Unable to save file: '$path'");
	    } elsif (-e $path) {
		$doc = $parser->parse_file($path);
		if ( ($node)=$doc->findnodes("model/model_config")) {
		    my $pass=($node->getAttribute("password")//"");
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
	    $node->setAttribute("filterDirMin", $filterDirMin);
	    $node->setAttribute("filterDirMax", $filterDirMax);
	    $node->setAttribute("filterFile",   $filterFile);
	    $node->setAttribute("hits",         $hits);
	    $node->setAttribute("indexTarget",  $indexTarget);
	    $node->setAttribute("indexVariable",$indexVariable);
	    if(defined $node->getAttribute("filterDir")){
		if (! -d $node->getAttribute("filterDir")) {
		    $node->setAttribute("filterDirStat",$node->getAttribute("filterDir"));
		}
	    }
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
			$parent->setAttribute("size",$dimv);
			$node->addChild( $parent );
		    }
		}
	    }
	    # save XML to file
	    farkdir::docsave($path,$doc);
	    print "<save message='success'/>\n";
	} else {
	    farkdir::term("Permission denied ($cls): '".$fpath."'");
	}
    }{message=> "Unable to save $ifile (see $logfile)\n",
      logfile=>$logfile,
      stdout=>"success"};
}

sub saveObs {
    my $param = shift;
    my $cls=$param->{type}->[0] // "";
    my $password=($param->{password}[0] // "");
    my $filterDir=($param->{filterDir}[0] // "*");
    my $filterDirMin=($param->{filterDirMin}[0] // "*");
    my $filterDirMax=($param->{filterDirMax}[0] // "*");
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
    my $ifile=$param->{file}->[0]//"";
    farkdir::sandbox {
	my ($dir, $file) = farkdir::splitName($ifile);
	my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
	my $fpath=$root . $loc;
	if (-d  $fpath && $priv eq "rw") {
	    my $path=$fpath . $file;
	    # check password
	    my $parser = XML::LibXML->new();
	    my $doc;
	    my $node;
	    if (-d $path) {
		farkdir::term("Unable to save file: '$path'");
	    } elsif (-e $path) {
		$doc = $parser->parse_file($path);
		if ( ($node)=$doc->findnodes("obs/obs_config")) {
		    my $pass=($node->getAttribute("password")//"");
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
	    $node->setAttribute("filterDirMin",$filterDirMin);
	    $node->setAttribute("filterDirMax",$filterDirMax);
	    $node->setAttribute("filterFile",  $filterFile);
	    $node->setAttribute("tablePath",   $table);
	    $node->setAttribute("bufrType",    $bufrType);
	    $node->setAttribute("subType",     $subType);
	    $node->setAttribute("typeInfo",    $typeInfo);
	    $node->setAttribute("indexTarget", $indexTarget);
	    $node->setAttribute("indexExp",    $indexExp);
	    if(defined $node->getAttribute("filterDir")){
		if (! -d $node->getAttribute("filterDir")) {
		    $node->setAttribute("filterDirStat",$node->getAttribute("filterDir"));
		}
	    }
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
			if ($len == 3) {
			    $wrote++;
			    my $parent = XML::LibXML::Element->new( 'target' );
			    $parent->setAttribute("name",$items[0]);
			    $parent->setAttribute("pos",$items[1]);
			    $parent->setAttribute("descr",$items[2]);
			    $parent->setAttribute("info",$items[3]);
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
	    farkdir::docsave($path,$doc);
	    print "<save message='success'/>\n";
	} else {
	    farkdir::term("Permission denied ($cls): '".$fpath."'");
	}
    }{message=> "Unable to save $ifile (see /tmp/fark_save.tmp)\n",
      logfile=>"/tmp/fark_save.tmp",
      stdout=>"success"
    };
}
sub saveColoc {
    my $param = shift;
    my $cls=$param->{type}->[0] // "";
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
    # emulate output strings
    my @ostrings = ("xml=\"$xml\"");
    #
    if (! defined ($param->{file}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{file}->[0]//"";
    farkdir::sandbox {
	my ($dir, $file) = farkdir::splitName($ifile);
	my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
	my $fpath=$root . $loc;
	if (-d  $fpath && $priv eq "rw") {
	    my $path=$fpath . $file;
	    # check password
	    my $parser = XML::LibXML->new();
	    my $doc;
	    my $node;
	    if (-d $path) {
		farkdir::term("Unable to save file: '$path'");
	    } elsif (-e $path) {
		$doc = $parser->parse_file($path);
		if ( ($node)=$doc->findnodes("coloc/coloc_config")) {
		    my $pass=($node->getAttribute("password")//"");
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
	    # check that output does not match output in other files...
	    my $ofile=farkdir::checkClassForStrings($cls,$ifile,@ostrings);
	    if ($ofile) {
		farkdir::term("Output overlaps with '$ofile'");
	    }
	    # process
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
	    farkdir::docsave($path,$doc);
	    print "<save message='success'/>\n";
	} else {
	    farkdir::term("Permission denied ($cls): '".$fpath."'");
	}
    }{message=> "Unable to save $ifile (see /tmp/fark_save.tmp)\n",
      logfile=>"/tmp/fark_save.tmp",
      stdout=>"success",
    };
}
sub saveTable {
    my $param = shift;
    my $cls=$param->{type}->[0] // "";
    my $password=($param->{password}[0] // "");
    my $table=($param->{table}[0] // "");
    my $graphics=($param->{graphics}[0] // "true");
    my $overwrite=($param->{overwrite}[0] // "");
    my $cat=($param->{cat}[0] // "");
    my $tableCols=($param->{columns}[0] // "");
    my $tableSets=($param->{sets}[0] // "");
    my $tableAttrs=($param->{attributes}[0] // "");
    # emulate output strings
    my @ostrings = ("table=\"$table\"","graphics=\"$graphics\"");
    #
    if (! defined ($param->{file}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{file}->[0]//"";
    farkdir::sandbox {
	my ($dir, $file) = farkdir::splitName($ifile);
	my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
	my $fpath=$root . $loc;
	#print "Started with '$root' '$loc' '$priv'\n";
	if (-d  $fpath && $priv eq "rw") {
	    my $path=$fpath . $file;
	    # check password
	    my $parser = XML::LibXML->new();
	    my $doc;
	    my $node;
	    #print "Parsing '$path'\n";
	    if (-d $path) {
		farkdir::term("Unable to save file: '$path'");
	    } elsif (-e $path) {
		$doc = $parser->parse_file($path);
		if ( ($node)=$doc->findnodes("table/table_config")) {
		    my $pass=($node->getAttribute("password")//"");
		    if ($pass ne $password) {
			farkdir::term("Invalid password for file: ".$path);
		    }
		} else {
		    my $parent=$doc->createElement("table");
		    $node=$parent->createElement("table_config");
		};
	    } else {
		$doc = $parser->parse_string("<table><table_config/></table>");
		($node) = $doc->findnodes("table/table_config");
	    }
	    # check that output does not match output in other files...
	    my $ofile=farkdir::checkClassForStrings($cls,$ifile,@ostrings);
	    if ($ofile && "$overwrite" eq "true") {
		farkdir::term("Output overlaps with '$ofile'");
	    }
	    $node->setAttribute("path",        $loc . $file);
	    $node->setAttribute("root",        $root);
	    $node->setAttribute("location",    $loc);
	    $node->setAttribute("status",      $priv);
	    $node->setAttribute("password",    $password);
	    $node->setAttribute("file",        $file);
	    $node->setAttribute("table",       $table);
	    $node->setAttribute("graphics",    $graphics);
	    $node->setAttribute("overwrite",   $overwrite);
	    $node->setAttribute("cat",         $cat);
	    #print "Processing\n";
	    # remove target nodes...
	    my @oldNodes=$node->findnodes("column");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    if ($tableCols) {
		#print "Tabletargets: $tableCols\n";
		my @cols=split (/\~/, $tableCols,-1);
		foreach my $col (@cols) {
		    #print "Col:$col   @cols\n";
		    my $child = XML::LibXML::Element->new( 'column' );
		    $child->setAttribute("name",$col);
		    $node->addChild( $child );
		}
	    }
	    # remove target nodes...
	    @oldNodes=$node->findnodes("set");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    if ($tableSets) {
		my @lines = split (/\|/, $tableSets,-1);
		if (@lines) { 
		    #print "Looping '$tableSets'\n";
		    foreach my $line (@lines) {
			#print "Tabletargets: $line\n";
			my @items=split (/\~/, $line,-1);
			my $len=$#items;
			#print "   Loop: '$line' $len\n";
			if ($len >2) {
			    my $parent = XML::LibXML::Element->new( 'set' );
			    $parent->setAttribute("name",$items[0]);
			    $parent->setAttribute("coloc",$items[1]);
			    $parent->setAttribute("legend",$items[2]);
			    my $ii=3;
			    while ($ii < $len) {
				my $child = XML::LibXML::Element->new( 'col' );
				$child->setAttribute("value",$items[$ii]);
				$parent->addChild( $child );
				$ii++;
			    }
			    $node->addChild( $parent );
			}
		    }
		}
	    };
	    @oldNodes=$node->findnodes("attribute");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    if ($tableAttrs) {
		my @lines = split (/\|/, $tableAttrs,-1);
		if (@lines) { 
		    #print "Looping '$tableAttrs'\n";
		    foreach my $line (@lines) {
			#print "Tableattrs: $line\n";
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
	    farkdir::docsave($path,$doc);
	    print "<save message='success'/>\n";
	} else {
	    farkdir::term("Permission denied ($cls): '".$fpath."'");
	}
    }{message =>  "Unable to save $ifile (see /tmp/fark_save.tmp)\n",
      logfile => "/tmp/fark_save.tmp",
      stdout  => "success"
    };
}

sub saveJoin {
    my $param = shift;
    my $cls=$param->{type}->[0] // "";
    my $password=($param->{password}[0] // "");
    my $table=($param->{table}[0] // "");
    my $graphics=($param->{graphics}[0] // "true");
    my $filter=($param->{filter}[0] // "1");
    my $overwrite=($param->{overwrite}[0] // "");
    my $filterDir=($param->{filterDir}[0] // "");
    my $filterDirMin=($param->{filterDirMin}[0] // "");
    my $filterDirMax=($param->{filterDirMax}[0] // "");
    my $filterFile=($param->{filterFile}[0] // "");
    my $hits=($param->{hits}[0] // "");
    my $cat=($param->{cat}[0] // "");
    my $joinCols=($param->{columns}[0] // "");
    my $joinColMin=($param->{columnMin}[0] // "");
    my $joinColMax=($param->{columnMax}[0] // "");
    my $joinAttrs=($param->{attributes}[0] // "");
    # emulate output strings
    my @ostrings = ("table=\"$table\"","graphics=\"$graphics\"");
    #
    if (! defined ($param->{file}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{file}->[0]//"";
    farkdir::sandbox {
	my ($dir, $file) = farkdir::splitName($ifile);
	my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
	my $fpath=$root . $loc;
	#print "Started with '$root' '$loc' '$priv'\n";
	if (-d  $fpath && $priv eq "rw") {
	    my $path=$fpath . $file;
	    # check password
	    my $parser = XML::LibXML->new();
	    my $doc;
	    my $node;
	    #print "Parsing '$path'\n";
	    if (-d $path) {
		farkdir::term("Unable to save file: '$path'");
	    } elsif (-e $path) {
		$doc = $parser->parse_file($path);
		if ( ($node)=$doc->findnodes("join/join_config")) {
		    my $pass=($node->getAttribute("password")//"");
		    if ($pass ne $password) {
			farkdir::term("Invalid password for file: ".$path);
		    }
		} else {
		    my $parent=$doc->createElement("join");
		    $node=$parent->createElement("join_config");
		};
	    } else {
		$doc = $parser->parse_string("<join><join_config/></join>");
		($node) = $doc->findnodes("join/join_config");
	    }
	    # check that output does not match output in other files...
	    my $ofile=farkdir::checkClassForStrings($cls,$ifile,@ostrings);
	    if ($ofile && "$overwrite" eq "true") {
		farkdir::term("Output overlaps with '$ofile'");
	    }
	    $node->setAttribute("path",        $loc . $file);
	    $node->setAttribute("root",        $root);
	    $node->setAttribute("location",    $loc);
	    $node->setAttribute("status",      $priv);
	    $node->setAttribute("password",    $password);
	    $node->setAttribute("file",        $file);
	    $node->setAttribute("table",       $table);
	    $node->setAttribute("graphics",    $graphics);
	    $node->setAttribute("filter",      $filter);
	    $node->setAttribute("overwrite",   $overwrite);
	    $node->setAttribute("filterDir",   $filterDir);
	    $node->setAttribute("filterDirMin",$filterDirMin);
	    $node->setAttribute("filterDirMax",$filterDirMax);
	    $node->setAttribute("filterFile",  $filterFile);
	    $node->setAttribute("hits",        $hits);
	    $node->setAttribute("cat",         $cat);
	    #print "Processing\n";
	    # remove target nodes...
	    my @oldNodes=$node->findnodes("column");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    if ($joinCols) {
		#print "JoinCols: $joinCols\n";
		#print "JoinMin:  $joinColMin\n";
		#print "JoinMax:  $joinColMax\n";
		my @cols=split (/\~/, $joinCols,-1);
		my @colMin=split (/\~/, $joinColMin,-1);
		my @colMax=split (/\~/, $joinColMax,-1);
		my $len=$#cols;
		my $ii=0;
		while ($ii <= $len) {
		    my $col=$cols[$ii];
		    my $min=$colMin[$ii]//"";
		    my $max=$colMax[$ii]//"";
		    #print "Col($ii) $len= '$col' '$min' '$max'\n";
		    my $child = XML::LibXML::Element->new( 'column' );
		    $child->setAttribute("name",$col);
		    if ("$min" ne "") {$child->setAttribute("min",$min);};
		    if ("$max" ne "") {$child->setAttribute("max",$max);};
		    $node->addChild( $child );
		    $ii++;
		}
	    }
	    @oldNodes=$node->findnodes("attribute");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    if ($joinAttrs) {
		my @lines = split (/\|/, $joinAttrs,-1);
		if (@lines) { 
		    #print "Looping '$joinAttrs'\n";
		    foreach my $line (@lines) {
			#print "Joinattrs: $line\n";
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
	    farkdir::docsave($path,$doc);
	    print "<save message='success'/>\n";
	} else {
	    farkdir::term("Permission denied ($cls): '".$fpath."'");
	}
    }{message =>  "Unable to save $ifile (see /tmp/fark_save.tmp)\n",
      logfile => "/tmp/fark_save.tmp",
      stdout  => "success"
    };
}
sub savePlot {
    my $param = shift;
    my $cls=$param->{type}->[0] // "";
    my $password=($param->{password}[0] // "");
    my $table=($param->{table}[0] // "");
    my $graphics=($param->{graphics}[0] // "");
    my $cat=($param->{cat}[0] // "");
    my $plotCols=($param->{columns}[0] // "");
    my $plotSets=($param->{sets}[0] // "");
    my $plotAttrs=($param->{attributes}[0] // "");
    # emulate output strings
    my @ostrings = ("table=\"$table\"","graphics=\"$graphics\"");
    #
    if (! defined ($param->{file}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{file}->[0]//"";
    farkdir::sandbox {
	my ($dir, $file) = farkdir::splitName($ifile);
	my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
	my $fpath=$root . $loc;
	#print "Started with '$root' '$loc' '$priv'\n";
	if (-d  $fpath && $priv eq "rw") {
	    my $path=$fpath . $file;
	    # check password
	    my $parser = XML::LibXML->new();
	    my $doc;
	    my $node;
	    #print "Parsing '$path'\n";
	    if (-d $path) {
		farkdir::term("Unable to save file: '$path'");
	    } elsif (-e $path) {
		$doc = $parser->parse_file($path);
		if ( ($node)=$doc->findnodes("plot/plot_config")) {
		    my $pass=($node->getAttribute("password")//"");
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
	    # check that output does not match output in other files...
	    my $ofile=farkdir::checkClassForStrings($cls,$ifile,@ostrings);
	    if ($ofile) {
		farkdir::term("Output overlaps with '$ofile'");
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
	    #print "Processing\n";
	    # remove target nodes...
	    my @oldNodes=$node->findnodes("column");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    if ($plotCols) {
		#print "Plottargets: $plotCols\n";
		my @cols=split (/\~/, $plotCols,-1);
		foreach my $col (@cols) {
		    #print "Col:$col   @cols\n";
		    my $child = XML::LibXML::Element->new( 'column' );
		    $child->setAttribute("name",$col);
		    $node->addChild( $child );
		}
	    }
	    # remove target nodes...
	    @oldNodes=$node->findnodes("set");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    if ($plotSets) {
		my @lines = split (/\|/, $plotSets,-1);
		if (@lines) { 
		    #print "Looping '$plotSets'\n";
		    foreach my $line (@lines) {
			#print "Plottargets: $line\n";
			my @items=split (/\~/, $line,-1);
			my $len=$#items;
			#print "   Loop: '$line' $len\n";
			if ($len >2) {
			    my $parent = XML::LibXML::Element->new( 'set' );
			    $parent->setAttribute("name",$items[0]);
			    $parent->setAttribute("coloc",$items[1]);
			    $parent->setAttribute("legend",$items[2]);
			    my $ii=3;
			    while ($ii < $len) {
				my $child = XML::LibXML::Element->new( 'col' );
				$child->setAttribute("value",$items[$ii]);
				$parent->addChild( $child );
				$ii++;
			    }
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
		    #print "Looping '$plotAttrs'\n";
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
	    farkdir::docsave($path,$doc);
	    print "<save message='success'/>\n";
	} else {
	    farkdir::term("Permission denied ($cls): '".$fpath."'");
	}
    }{message =>  "Unable to save $ifile (see /tmp/fark_save.tmp)\n",
      logfile => "/tmp/fark_save.tmp",
      stdout  => "success"
    };
}
sub saveRerun {
    my $param = shift;
    my $cls=$param->{type}->[0] // "";
    my $password=($param->{password}[0] // "");
    my $offset=($param->{offset}[0] // "");
    my $variable=($param->{variable}[0] // "");
    my $start=($param->{start}[0] // "");
    my $stop=($param->{stop}[0] // "");
    my $modelFiles=($param->{modelFiles}[0] // "");
    my $obsFiles=($param->{obsFiles}[0] // "");
    my $colocFiles=($param->{colocFiles}[0] // "");
    my $tableFiles=($param->{tableFiles}[0] // "");
    my $joinFiles=($param->{joinFiles}[0] // "");
    my $plotFiles=($param->{plotFiles}[0] // "");
    if (! defined ($param->{file}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{file}->[0]//"";
    farkdir::sandbox {
	my ($dir, $file) = farkdir::splitName($ifile);
	my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
	my $fpath=$root . $loc;
	if (-d  $fpath && $priv eq "rw") {
	    my $path=$fpath . $file;
	    # check password
	    my $parser = XML::LibXML->new();
	    my $doc;
	    my $node;
	    if (-d $path) {
		farkdir::term("Unable to save file: '$path'");
	    } elsif (-e $path) {
		$doc = $parser->parse_file($path);
		if ( ($node)=$doc->findnodes("rerun/rerun_config")) {
		    my $pass=($node->getAttribute("password")//"");
		    if ($pass ne $password) {
			farkdir::term("Invalid password for file: ".$path);
		    }
		} else {
		    my $parent=$doc->createElement("rerun");
		    $node=$parent->createElement("rerun_config");
		};
	    } else {
		$doc = $parser->parse_string("<rerun><rerun_config/></rerun>");
		($node) = $doc->findnodes("rerun/rerun_config");
	    }
	    $node->setAttribute("path",        $loc . $file);
	    $node->setAttribute("root",        $root);
	    $node->setAttribute("location",    $loc);
	    $node->setAttribute("status",      $priv);
	    $node->setAttribute("password",    $password);
	    $node->setAttribute("file",        $file);
	    $node->setAttribute("offset",      $offset);
	    $node->setAttribute("variable",    $variable);
	    $node->setAttribute("start",       $start);
	    $node->setAttribute("stop",        $stop);
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
			if ($len == 2) {
			    my $parent = XML::LibXML::Element->new( 'model' );
			    $parent->setAttribute("file",$items[0]);
			    $parent->setAttribute("last",$items[1]);
			    $parent->setAttribute("info",$items[2]);
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
			if ($len == 2) {
			    my $parent = XML::LibXML::Element->new( 'obs' );
			    $parent->setAttribute("file",$items[0]);
			    $parent->setAttribute("last",$items[1]);
			    $parent->setAttribute("info",$items[2]);
			    $node->addChild( $parent );
			}
		    }
		}
	    }
	    # remove coloc nodes...
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
			if ($len == 2) {
			    my $parent = XML::LibXML::Element->new( 'coloc' );
			    $parent->setAttribute("file",$items[0]);
			    $parent->setAttribute("last",$items[1]);
			    $parent->setAttribute("info",$items[2]);
			    $node->addChild( $parent );
			}
		    }
		}
	    }
	    # remove table nodes...
	    @oldNodes=$node->findnodes("table");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    @lines = split (/\|/, $tableFiles,-1);
	    if (@lines) { 
		foreach my $line (@lines) {
		    if ($line ne "") {
			my @items=split (/\~/, $line,-1);
			my $len=$#items;
			if ($len == 2) {
			    my $parent = XML::LibXML::Element->new( 'table' );
			    $parent->setAttribute("file",$items[0]);
			    $parent->setAttribute("last",$items[1]);
			    $parent->setAttribute("info",$items[2]);
			    $node->addChild( $parent );
			}
		    }
		}
	    }
	    # remove join nodes...
	    @oldNodes=$node->findnodes("join");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    @lines = split (/\|/, $joinFiles,-1);
	    if (@lines) { 
		foreach my $line (@lines) {
		    if ($line ne "") {
			my @items=split (/\~/, $line,-1);
			my $len=$#items;
			if ($len == 2) {
			    my $parent = XML::LibXML::Element->new( 'join' );
			    $parent->setAttribute("file",$items[0]);
			    $parent->setAttribute("last",$items[1]);
			    $parent->setAttribute("info",$items[2]);
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
			if ($len == 2) {
			    my $parent = XML::LibXML::Element->new( 'plot' );
			    $parent->setAttribute("file",$items[0]);
			    $parent->setAttribute("last",$items[1]);
			    $parent->setAttribute("info",$items[2]);
			    $node->addChild( $parent );
			}
		    }
		}
	    }
	    # save XML to file
	    farkdir::docsave($path,$doc);
	    print "<save message='success'/>\n";
	} else {
	    farkdir::term("Permission denied ($cls): '".$fpath."' $priv");
	}
    }{message=> "Unable to save $ifile (see /tmp/fark_save.tmp)\n",
      logfile=>"/tmp/fark_save.tmp",
      stdout=>"success"
    };
}

sub saveClean {
    my $param = shift;
    my $cls=$param->{type}->[0] // "";
    my $password=($param->{password}[0] // "");
    my $jobs=($param->{jobs}[0] // "");
    if (! defined ($param->{file}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{file}->[0]//"";
    farkdir::sandbox {
	my ($dir, $file) = farkdir::splitName($ifile);
	my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
	my $fpath=$root . $loc;
	if (-d  $fpath && $priv eq "rw") {
	    my $path=$fpath . $file;
	    # check password
	    my $parser = XML::LibXML->new();
	    my $doc;
	    my $node;
	    if (-d $path) {
		farkdir::term("Unable to save file: '$path'");
	    } elsif (-e $path) {
		$doc = $parser->parse_file($path);
		if ( ($node)=$doc->findnodes("clean/clean_config")) {
		    my $pass=($node->getAttribute("password")//"");
		    if ($pass ne $password) {
			farkdir::term("Invalid password for file: ".$path);
		    }
		} else {
		    my $parent=$doc->createElement("clean");
		    $node=$parent->createElement("clean_config");
		};
	    } else {
		$doc = $parser->parse_string("<clean><clean_config/></clean>");
		($node) = $doc->findnodes("clean/clean_config");
	    }
	    $node->setAttribute("path",        $loc . $file);
	    $node->setAttribute("root",        $root);
	    $node->setAttribute("location",    $loc);
	    $node->setAttribute("status",      $priv);
	    $node->setAttribute("password",    $password);
	    $node->setAttribute("file",        $file);
	    # remove model nodes...
	    my @oldNodes=$node->findnodes("filter");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    # make list of new nodes
	    my @lines = split (/\|/, $jobs,-1);
	    if (@lines) { 
		foreach my $line (@lines) {
		    if ($line ne "") {
			my @items=split (/\~/, $line,-1);
			my $len=$#items;
			if ($len == 2) {
			    my $parent = XML::LibXML::Element->new( 'filter' );
			    $parent->setAttribute("dir",$items[0]);
			    $parent->setAttribute("file",$items[1]);
			    $parent->setAttribute("age",$items[2]);
			    $node->addChild( $parent );
			}
		    }
		}
	    }
	    # save XML to file
	    farkdir::docsave($path,$doc);
	    print "<save message='success'/>\n";
	} else {
	    farkdir::term("Permission denied ($cls): '".$fpath."' $priv");
	}
    }{message=> "Unable to save $ifile (see /tmp/fark_save.tmp)\n",
      logfile=>"/tmp/fark_save.tmp",
      stdout=>"success"
    };
}

sub saveExec {
    my $param = shift;
    my $cls=$param->{type}->[0] // "";
    my $password=($param->{password}[0] // "");
    my $modelFiles=($param->{modelFiles}[0] // "");
    my $obsFiles=($param->{obsFiles}[0] // "");
    my $colocFiles=($param->{colocFiles}[0] // "");
    my $tableFiles=($param->{tableFiles}[0] // "");
    my $joinFiles=($param->{joinFiles}[0] // "");
    my $plotFiles=($param->{plotFiles}[0] // "");
    my $rerunFiles=($param->{rerunFiles}[0] // "");
    my $cleanFiles=($param->{cleanFiles}[0] // "");
    if (! defined ($param->{root}->[0])) {farkdir::term("Undefined file.");};
    my $ifile=$param->{root}->[0]//"";
    farkdir::sandbox {
	my ($dir, $file) = farkdir::splitName($ifile);
	my ($root, $loc, $priv) = farkdir::splitDir( $dir, $cls );
	my $fpath=$root . $loc;
	if (-d  $fpath && $priv eq "rw") {
	    my $path=$fpath . $file;
	    # check password
	    my $parser = XML::LibXML->new();
	    my $doc;
	    my $node;
	    if (-d $path) {
		farkdir::term("Unable to save file: '$path'");
	    } elsif (-e $path) {
		$doc = $parser->parse_file($path);
		if ( ($node)=$doc->findnodes("exec/exec_config")) {
		    my $pass=($node->getAttribute("password")//"");
		    if ($pass ne $password) {
			farkdir::term("Invalid password for file: ".$path);
		    }
		} else {
		    my $parent=$doc->createElement("exec");
		    $node=$parent->createElement("exec_config");
		};
	    } else {
		$doc = $parser->parse_string("<exec><exec_config/></exec>");
		($node) = $doc->findnodes("exec/exec_config");
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
			    $parent->setAttribute("exec",$items[3]);
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
			    $parent->setAttribute("exec",$items[3]);
			    $node->addChild( $parent );
			}
		    }
		}
	    }
	    # remove coloc nodes...
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
			    $parent->setAttribute("exec",$items[3]);
			    $node->addChild( $parent );
			}
		    }
		}
	    }
	    # remove plot nodes...
	    @oldNodes=$node->findnodes("table");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    @lines = split (/\|/, $tableFiles,-1);
	    if (@lines) { 
		foreach my $line (@lines) {
		    if ($line ne "") {
			my @items=split (/\~/, $line,-1);
			my $len=$#items;
			if ($len == 3) {
			    my $parent = XML::LibXML::Element->new( 'table' );
			    $parent->setAttribute("file",$items[0]);
			    $parent->setAttribute("last",$items[1]);
			    $parent->setAttribute("info",$items[2]);
			    $parent->setAttribute("exec",$items[3]);
			    $node->addChild( $parent );
			}
		    }
		}
	    }
	    # remove join nodes...
	    @oldNodes=$node->findnodes("join");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    @lines = split (/\|/, $joinFiles,-1);
	    if (@lines) { 
		foreach my $line (@lines) {
		    if ($line ne "") {
			my @items=split (/\~/, $line,-1);
			my $len=$#items;
			if ($len == 3) {
			    my $parent = XML::LibXML::Element->new( 'join' );
			    $parent->setAttribute("file",$items[0]);
			    $parent->setAttribute("last",$items[1]);
			    $parent->setAttribute("info",$items[2]);
			    $parent->setAttribute("exec",$items[3]);
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
			    $parent->setAttribute("exec",$items[3]);
			    $node->addChild( $parent );
			}
		    }
		}
	    }
	    # remove rerun nodes...
	    @oldNodes=$node->findnodes("rerun");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    @lines = split (/\|/, $rerunFiles,-1);
	    if (@lines) { 
		foreach my $line (@lines) {
		    if ($line ne "") {
			my @items=split (/\~/, $line,-1);
			my $len=$#items;
			if ($len == 3) {
			    my $parent = XML::LibXML::Element->new( 'rerun' );
			    $parent->setAttribute("file",$items[0]);
			    $parent->setAttribute("last",$items[1]);
			    $parent->setAttribute("info",$items[2]);
			    $parent->setAttribute("exec",$items[3]);
			    $node->addChild( $parent );
			}
		    }
		}
	    }
	    # remove clean nodes...
	    @oldNodes=$node->findnodes("clean");
	    foreach my $oldNode (@oldNodes) {
		$node->removeChild($oldNode);
	    }
	    @lines = split (/\|/, $cleanFiles,-1);
	    if (@lines) { 
		foreach my $line (@lines) {
		    if ($line ne "") {
			my @items=split (/\~/, $line,-1);
			my $len=$#items;
			if ($len == 3) {
			    my $parent = XML::LibXML::Element->new( 'clean' );
			    $parent->setAttribute("file",$items[0]);
			    $parent->setAttribute("last",$items[1]);
			    $parent->setAttribute("info",$items[2]);
			    $parent->setAttribute("exec",$items[3]);
			    $node->addChild( $parent );
			}
		    }
		}
	    }
	    # save XML to file
	    farkdir::docsave($path,$doc);
	    print "<save message='success'/>\n";
	} else {
	    farkdir::term("Permission denied ($cls): '".$fpath."' $priv");
	}
    }{message=> "Unable to save $ifile (see /tmp/fark_save.tmp)\n",
      logfile=>"/tmp/fark_save.tmp",
      stdout=>"success"
    };
}

