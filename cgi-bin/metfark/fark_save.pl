#!/usr/bin/perl -w
#
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use File::Basename;
use Data::Dumper;
use Capture::Tiny 'capture';
use File::Path qw( make_path );
#  config directory
#
print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
my $user=$ENV{USERNAME} // "www";
my $pub="/metfark/pub";
my $modelDir="$pub/model/"; check_path($modelDir);
my $obsDir="$pub/obs/"; check_path($obsDir);
my $scanDir="$pub/scan/"; check_path($scanDir);
my $colocDir="$pub/coloc/"; check_path($colocDir);
my $modelCacheDir="$pub/cache/model/"; check_path($modelCacheDir);
my $modelRegDir="$pub/reg/model/"; check_path($modelRegDir);
my $obsCacheDir="$pub/cache/obs/"; check_path($obsCacheDir);
my $obsRegDir="$pub/reg/obs/"; check_path($obsRegDir);
#
my $ref=CGI->new();
#
my $param= $ref->{param};
if (! defined $param->{type}) {&term("Undefined type.".Dumper($param))};
if ($param->{type}->[0] eq "model") {
    &saveModel($param, $modelDir);
} elsif ($param->{type}->[0] eq "obs") {
    &saveObs($param, $obsDir);
} elsif ($param->{type}->[0] eq "scan") {
    &saveScan($param, $scanDir);
} elsif ($param->{type}->[0] eq "coloc") {
    &saveColoc($param, $colocDir);
}


sub saveModel {
    my ($param, $dir) = @_;
    my $base=basename($param->{file}[0]);
    if (! defined ($param->{file}->[0])) {&term("Undefined file.");};
    my $file=$dir . $base;
    my $password=($param->{password}[0] // "");
    my $filterDir=($param->{filterDir}[0] // "");
    my $filter=($param->{filter}[0] // "");
    my $hits=($param->{hits}[0] // "");
    my $index=($param->{index}[0] // "");
    my $variables=($param->{variables}[0]// "");
    my $stack=($param->{stack}[0]// "");
    # check password
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    if (-e $file) {
	$doc = $parser->parse_file($file);
	if ( ($node)=$doc->findnodes("model/model_config")) {
	    my $pass=($node->getAttribute("password")||"");
	    if ($pass ne $password) {
		&term("Invalid password for file: ".$file);
	    }
	} else {
	    my $parent=$doc->createElement("model");
	    $node=$parent->createElement("model_config");
	};
    } else {
	$doc = $parser->parse_string("<model><model_config/></model>");
	($node) = $doc->findnodes("model/model_config");
    }
    $node->setAttribute("password",        $password);
    $node->setAttribute("file",            $base);
    $node->setAttribute("fileFilterDir",   $filterDir);
    $node->setAttribute("fileFilter",      $filter);
    $node->setAttribute("hits",            $hits);
    $node->setAttribute("index",            $index);
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
	}
    }
    @oldNodes=$node->findnodes("variable");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    };
    my @lines = split (/\|/, $variables,-1);
    if (@lines) { 
	foreach my $line (@lines) {
	    my @items = split (/\~/, $line,-1);
	    my $name=$items[0];
	    my $dims=$items[1];
	    if ($name) {
		my $parent = XML::LibXML::Element->new( 'variable' );
		$parent->setAttribute("name",$name);
		$parent->setAttribute("dims",$dims);
		$node->addChild( $parent );
	    }
	}
    }
    # save XML to file
    if (open(my $fh, '>', $file)) {
	print $fh $doc->toString;
	close $fh;
	chmod 0666, $file;
    } else {
	&term("Unable to open:".$file);
    }
    print "<save message='success'/>\n";
}
sub saveObs {
    my ($param, $dir) = @_;
    my $base=basename($param->{file}[0]);
    if (! defined ($param->{file}->[0])) {&term("Undefined file.");};
    my $file=$dir . $base;
    my $password=($param->{password}[0] // "");
    my $filterDir=($param->{filterDir}[0] // "*");
    my $filter=($param->{filter}[0] // "*");
    my $table=($param->{table}[0] // "");
    my $obsFilter=($param->{obsFilter}[0] // "");
    my $bufrType=($param->{bufrType}[0] // "");
    my $subType=($param->{subType}[0] // "");
    my $typeInfo=($param->{typeInfo}[0] // "");
    my $indexTarget=($param->{indexTarget}[0] // "");
    my $indexExp=($param->{indexExp}[0] // "");
    my $obsTargets=($param->{obsTargets}[0] // "");
    # check password
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    if (-e $file) {
	$doc = $parser->parse_file($file);
	if ( ($node)=$doc->findnodes("obs/obs_config")) {
	    my $pass=($node->getAttribute("password")||"");
	    if ($pass ne $password) {
		&term("Invalid password for file: ".$file);
	    }
	} else {
	    my $parent=$doc->createElement("obs");
	    $node=$parent->createElement("obs_config");
	};
    } else {
	$doc = $parser->parse_string("<obs><obs_config/></obs>");
	($node) = $doc->findnodes("obs/obs_config");
    }
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
    # remove target nodes...
    my @oldNodes=$node->findnodes("target");
    foreach my $oldNode (@oldNodes) {
	$node->removeChild($oldNode);
    }
    if ($obsTargets) {
	my @lines = split (/\|/, $obsTargets,-1);
	if (@lines) { 
	    foreach my $line (@lines) {
		#print "Obstargets: $line\n";
		my @items=split (/\~/, $line,-1);
		my $len=$#items;
		if ($len == 5) {
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
    };
    # save XML to file
    if (open(my $fh, '>', $file)) {
	print $fh $doc->toString;
	close $fh; 
	chmod 0666, $file;
    } else {
	&term("Unable to open:".$file);
    }
    print "<save message='success'/>\n";
}

sub saveScan {
    my ($param, $dir) = @_;
    if (! defined ($param->{root}[0])) {&term("Undefined file.");};
    my $file=basename($param->{root}[0]);
    my $path=$dir . $file;
    my $password=($param->{password}[0] // "");
    my $modelFiles=($param->{modelFiles}[0] // "");
    my $obsFiles=($param->{obsFiles}[0] // "");
    # check password
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    if (-e $path) {
	$doc = $parser->parse_file($path);
	if ( ($node)=$doc->findnodes("scan/scan_config")) {
	    my $pass=($node->getAttribute("password")||"");
	    if ($pass ne $password) {
		&term("Invalid password for file: ".$path);
	    }
	} else {
	    my $parent=$doc->createElement("scan");
	    $node=$parent->createElement("scan_config");
	};
    } else {
	$doc = $parser->parse_string("<scan><scan_config/></scan>");
	($node) = $doc->findnodes("scan/scan_config");
    }
    $node->setAttribute("password",        $password);
    $node->setAttribute("file",            $file);
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
		    $parent->setAttribute("lastScan",$items[1]);
		    $parent->setAttribute("lastUsed",$items[2]);
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
		    $parent->setAttribute("lastScan",$items[1]);
		    $parent->setAttribute("lastUsed",$items[2]);
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
	&term("Unable to open:".$path);
    }
    print "<save message='success'/>\n";
}
sub saveColoc {
    my ($param, $dir) = @_;
    my $base=basename($param->{file}[0]);
    if (! defined ($param->{file}->[0])) {&term("Undefined file.");};
    my $file=$dir . $base;
    my $host         = ($param->{host}[0] // "localhost");
    my $password     = ($param->{password}[0] // "");
    my $modelFile    = ($param->{modelFile}[0] // "");
    my $modelStart   = ($param->{modelStart}[0] // "");
    my $modelStop    = ($param->{modelStop}[0] // "");
    my $modelTargets = ($param->{modelTargets}[0] // "");
    my $modelDefault = ($param->{modelDefault}[0] // "");
    my $obsFile      = ($param->{obsFile}[0] // "");
    my $obsStart     = ($param->{obsStart}[0] // "");
    my $obsStop      = ($param->{obsStop}[0] // "");
    my $obsTargets   = ($param->{obsTargets}[0] // "");
    my $matchRules   = ($param->{matchRules}[0] // "");
    # check password
    my $parser = XML::LibXML->new();
    my $doc;
    my $node;
    if (-e $file) {
	$doc = $parser->parse_file($file);
	if ( ($node)=$doc->findnodes("coloc/coloc_config")) {
	    my $pass=($node->getAttribute("password")||"");
	    if ($pass ne $password) {
		&term("Invalid password for file: ".$file);
	    }
	} else {
	    my $parent=$doc->createElement("coloc");
	    $node=$parent->createElement("coloc_config");
	};
    } else {
	$doc = $parser->parse_string("<coloc><coloc_config/></coloc>");
	($node) = $doc->findnodes("coloc/coloc_config");
    }
    $node->setAttribute("host",            $host);
    $node->setAttribute("password",        $password);
    $node->setAttribute("file",            $base);
    $node->setAttribute("modelFile",       $modelFile);
    $node->setAttribute("modelStart",      $modelStart);
    $node->setAttribute("modelStop",       $modelStop);
    $node->setAttribute("obsFile",         $obsFile);
    $node->setAttribute("obsStart",        $obsStart);
    $node->setAttribute("obsStop",         $obsStop);
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
    if (open(my $fh, '>', $file)) {
	print $fh $doc->toString;
	close $fh;
	chmod 0666, $file;
    } else {
	&term("Unable to open:".$file);
    }
    print "<save message='success'/>\n";
}

sub check_path{
    my $path=shift;
    eval {
	my $log=capture {
	    if(!-d $path) {
		make_path $path; 
		chmod 0777, $path;
	    }
	}
    };my $ret=$@;if ($ret) {term($ret);}
}
sub term {
    my $msg=shift;
    $msg=~s/[^a-zA-Z0-9 _\-\+\.\,\/\:\[\]\(\)]/ /g;
    $msg=~s/ +/ /g;
    print "<error message='".$msg."'/>\n";
    exit 1;
}
