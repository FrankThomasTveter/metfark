#!/usr/bin/perl -w
#
############use ExtUtils::testlib;
use fark;
use farkdata;
#
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
use XML::LibXML;
use Data::Dumper;
use farkdir;
#
#dont know if you need this: sudo apt-get install libpath-tiny-perl
#but you need this: sudo apt-get install libcapture-tiny-perl
use Capture::Tiny 'capture';
#
#  config directory
#
my $user=$ENV{USERNAME} // "www";
my $ref=CGI->new();
#
$ref->{"modelDir"}     = farkdir::getRootDir("model") || farkdir::term("invalid root directory (model)");
$ref->{"modelCache"}   = farkdir::getRootDir("model_cache") || farkdir::term("invalid root directory (model_cache)");
$ref->{"obsDir"}       = farkdir::getRootDir("obs") || farkdir::term("invalid root directory (obs)");
$ref->{"obsCache"}     = farkdir::getRootDir("obs_cache") || farkdir::term("invalid root directory (obs_cache)");
$ref->{"urlDir"}       = farkdir::getRootDir("url") || farkdir::term("invalid root directory (url)");
#
my $param=$ref->{param};
#
##########################################################################
# open session
##########################################################################

my $fark;
my $log="";
my $ret;
eval {
#    $log=capture {
	$fark = fark->open(); # open fark session
#    };
};
$ret=$@;
if ($ret) {farkdir::termAll($ret);exit 1;};
    
##########################################################################
# preprocess model information
##########################################################################
my $model=0;
my $obs=0;

my $urlFile=$param->{urlFile}->[$model] // "";
my $modelFile=$param->{modelFile}->[$model] // "";
my $modelStart=$param->{modelStart}->[$model] // "";
my $modelStop=$param->{modelStop}->[$model] // "";
my $modelTargets=$param->{modelTargets}->[$model] // "";
my $modelDefault=$param->{modelDefault}->[$model] // "";
my $obsFile=$param->{obsFile}->[$obs] // "";
my $obsStart=$param->{obsStart}->[$obs] // "";
my $obsStop=$param->{obsStop}->[$obs] // "";
my $obsTargets=$param->{obsTargets}->[$obs] // "";
my $matchRules=$param->{matchRules}->[$obs] // "";

eval {
#    $log=capture {
	if ($urlFile) { # read data from xml-file
	    my $file=$ref->{"urlDir"} . $urlFile;
	    if (-e $file) {
		my $parser = XML::LibXML->new();
		my $doc = $parser->parse_file($file);
		if ( my ($node) = $doc->findnodes("url/url_config")) {
		    if ($modelFile eq "")  {$modelFile = $node->getAttribute("modelFile"); };
		    if ($modelStart eq "") {$modelStart= $node->getAttribute("modelStart");};
		    if ($modelStop eq "")  {$modelStop = $node->getAttribute("modelStop"); };
		    if ($obsFile eq "")    {$obsFile   = $node->getAttribute("obsFile");   };
		    if ($obsStart eq "")   {$obsStart  = $node->getAttribute("obsStart");  };
		    if ($obsStop eq "")    {$obsStop   = $node->getAttribute("obsStop");   };
		    if ($modelFile) {
			if ($modelTargets eq "") {
			    my @oldNodes=$node->findnodes("modelTarget");
			    foreach my $oldnode (@oldNodes) {
				$modelTargets=$modelTargets."|".
				    $oldnode->getAttribute("name")."~".
				    $oldnode->getAttribute("variable")."~".
				    $oldnode->getAttribute("min")."~".
				    $oldnode->getAttribute("max");
			    }
			};
		    };
		    if ($modelFile && ! $obsFile) {
			if ($modelDefault eq "") {
			    my @oldDefault=$node->findnodes("modelDefault");
			    foreach my $default (@oldDefault) {
				my @oldDefs=$default->findnodes("def");
				if (@oldDefs) {
				    $modelDefault=$modelDefault . "[";
				    foreach my $def (@oldDefs) {
					$modelDefault=$modelDefault.
					    $def->getAttribute("name")."~".
					    $def->getAttribute("value")."|";
				    }
				}
			    };
			};
		    };

		    if ($obsFile) {
			if ($obsTargets eq "") {
			    my @oldNodes=$node->findnodes("obsTarget");
			    foreach my $oldnode (@oldNodes) {
				$obsTargets=$obsTargets.
				    $oldnode->getAttribute("name")."~".
				    $oldnode->getAttribute("pos")."~".
				    $oldnode->getAttribute("descr")."~".
				    $oldnode->getAttribute("info")."~".
				    $oldnode->getAttribute("min")."~".
				    $oldnode->getAttribute("max")."|";
			    }
			};
		    };
		    if ($obsFile && $modelFile) {
			if ($matchRules eq "") {
			    my @oldNodes=$node->findnodes("matchRules");
			    foreach my $oldnode (@oldNodes) {
				$matchRules=$matchRules.
				    ($oldnode->getAttribute("name")||"")."~".
				    ($oldnode->getAttribute("expression")||"")."~".
				    ($oldnode->getAttribute("min")||"")."~".
				    ($oldnode->getAttribute("max")||"")."|";
			    }
			};
		    }
		}
	    }
	}

	if ($modelFile) { # initialise model processing
	    my $modelCache=$ref->{modelCache}."/".$modelFile;
	    my $modelConfig = $ref->{modelDir}."/".$modelFile;

	    #print "Model File=". $modelFile."\n";
	    #print Dumper($modelFile);

	    my $index="";
	    my $parser = XML::LibXML->new();
	    if (-e $modelConfig) { # read config file parameters into memory
		my $doc = $parser->parse_file($modelConfig);
		if ( (my $node)=$doc->findnodes("model/model_config")) {
		    $index=$node->getAttribute("index");
		};
	    }
	    $fark->clearModelFileStack($index); # clear model file stack
	    $fark->loadModelCache($modelCache);# load cached model file stack
	    if ( $modelTargets ) { # process model targets
		#print "Model targets: ". $modelTargets."\n";
		foreach my $target (split (/\|/, $modelTargets)) {
		    if ($target ne "") {
			print "Model target:'$target'\n";
			$fark->pushModelTarget( split (/\~/, $target,-1) );
		    }
		};
	    };
	    if ( $modelDefault ) { # process model defaults
		#print "Model default: ". $modelDefault."\n";
		foreach my $set ( split (/\[/, $modelDefault)) {
		    foreach my $target ( split (/\|/, $set)) {
			print "Default: $target\n";
			$fark->addModelDefault( split (/\~/, $target,-1)); # "target:value"
		    };
		    $fark->pushModelDefault(); # "target:value"
		};
	    };
	    if ( $matchRules ) { # process match rules
		$fark->clearMatchRuleStack();
		foreach my $rule ( split (/\|/,  $matchRules) ) {
		    print "Expression: $rule\n";
		    $fark->addMatchRule( split (/\~/, $rule,-1));
		};
	    };
	    #print "Calling setModelIndexLimits\n";
	    if ($modelStart &&  $modelStop) {
		$fark->setModelIndexLimits($modelStart, $modelStop); # "target:value"
	    }
	};

##########################################################################
# preprocess observation information
##########################################################################

	if ($obsFile) { # initialise any observation processing
	    my $obs=0;
	    #print "Calling clearObservationFileStack\n";
	    $fark->clearObservationFileStack(); # clear observation file stack
	    my $obsCache=$ref->{obsCache}."/".$obsFile;
	    #print "Calling loadObservationCache\n";
	    $fark->loadObservationCache($obsCache); # load cached observation file stack
	    my $obsConfig = $ref->{"obsDir"}."/".$obsFile;
	    my $parser = XML::LibXML->new();
	    if (-e $obsConfig) {     # read config file parameters into memory
		my $doc = $parser->parse_file($obsConfig);
		if ( (my $node)=$doc->findnodes("obs/obs_config")) {
		    my $bufrType=($node->getAttribute("bufrType")||"");
		    my $subType=($node->getAttribute("subType")||"");
		    $fark->setObservationType( $bufrType, $subType); # defined observation filter
		    my $tablePath = $node->getAttribute("tablePath");
		    $fark->setObservationTablePath($tablePath); # define BUFR table path
		    my $indexTarget=($node->getAttribute("indexTarget") // "");
		    my $indexExp=($node->getAttribute("indexExp") // "");
		    if ( $indexTarget && $indexExp) {
			print "Setting index: '".$indexTarget."' = '".$indexExp."'\n";
			$fark->setObservationIndex($indexTarget,$indexExp);
		    }
		    my @targets = $node->findnodes("target");
		    foreach my $target (@targets) {
			my $name=$target->getAttribute("name");
			my $pos=$target->getAttribute("pos");
			my $descr=$target->getAttribute("descr");
			my $info=$target->getAttribute("info");
			my $min=$target->getAttribute("min");
			my $max=$target->getAttribute("max");
			#print "calling pushObservationTarget\n";
			print "Obs target: $name~$pos~$descr~$info~$min~$max\n";
			$fark->pushObservationTarget($name,$pos,$descr,$info,$min,$max);
		    };
		};
	    };
	    if ( $obsTargets ) { # process obs targets
		foreach my $target ( split (/\|/,  $obsTargets) ) {
		    print "Obs target: $target\n";
		    $fark->pushObservationTarget( split (/\~/, $target,-1));
		};
	    };
	    if ($obsStart && $obsStop) {
		print "Obs Limits: $obsStart $obsStop\n";
		$fark->setObservationIndexLimits($obsStart, $obsStop); # "target:value"
	    };
	};

#    };
};
$ret=$@;
if ($ret) {
    farkdir::termAll($ret);
} else {
    ##########################################################################
    # make the resulting XML
    ##########################################################################
    
    #print "Calling colocXML\n";
    my ($irc, $msg) = $fark->colocXML();
    
    ##########################################################################
    # wrap up and free memory gracefully
    ##########################################################################
    
    $fark->clearModelFileStack();
    $fark->clearObservationFileStack();
    
}
##########################################################################
# close session
##########################################################################
$fark->close();
