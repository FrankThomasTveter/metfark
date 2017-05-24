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
use File::Basename;
use Data::Dumper;
use File::Touch;
#
#dont know if you need this: sudo apt-get install libpath-tiny-perl
#but you need this: sudo apt-get install libcapture-tiny-perl
use Capture::Tiny 'capture';
#
#  config directory
#
my $user=$ENV{USERNAME} // "www";
my $ref=CGI->new();
$ref->{"modelDir"} =  "/home/".$user."/pub/model/";check_path($ref->{"modelDir"});
$ref->{"modelCache"}   = "/home/".$user."/pub/cache/model";check_path($ref->{"modelCache"});
$ref->{"obsDir"}   = "/home/".$user."/pub/obs/";check_path($ref->{"obsDir"});
$ref->{"obsCache"}   = "/home/".$user."/pub/cache/obs";check_path($ref->{"obsCache"});
$ref->{"scanDir"}  = "/home/".$user."/pub/scan/";check_path($ref->{"scanDir"});
$ref->{"colocDir"}   = "/home/".$user."/pub/coloc/";check_path($ref->{"colocDir"});
$ref->{"useModelDir"}   = "/home/".$user."/pub/use/model/";check_path($ref->{"useModelDir"});
$ref->{"useObsDir"}   = "/home/".$user."/pub/use/obs/";check_path($ref->{"useObsDir"});
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
    #$log=capture {
	$fark = fark->open(); # open fark session
    #};
};
$ret=$@;
if ($ret) {term($ret);exit 1;};
    
##########################################################################
# preprocess model information
##########################################################################
my $model=0;
my $obs=0;

my $colocFile=$param->{colocFile}->[$model] // "";
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
    #$log=capture {
	if ($colocFile) { # read data from xml-file
	    my $file=$ref->{"colocDir"} . $colocFile;
	    if (-e $file) {
		my $parser = XML::LibXML->new();
		my $doc = $parser->parse_file($file);
		if ( my ($node) = $doc->findnodes("coloc/coloc_config")) {
		    if ($modelFile eq "")  {$modelFile = $node->getAttribute("modelFile"); };
		    if ($modelStart eq "") {$modelStart= $node->getAttribute("modelStart");};
		    if ($modelStop eq "")  {$modelStop = $node->getAttribute("modelStop"); };
		    if ($obsFile eq "")    {$obsFile   = $node->getAttribute("obsFile");   };
		    if ($obsStart eq "")   {$obsStart  = $node->getAttribute("obsStart");  };
		    if ($obsStop eq "")    {$obsStop   = $node->getAttribute("obsStop");   };
		    if ($modelFile) {
			touch($ref->{"useModelDir"} . $modelFile); # touch use file
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
			touch($ref->{"useObsDir"} . $obsFile); # touch use file
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
				    ($oldnode->getAttribute("name")//"")."~".
				    ($oldnode->getAttribute("expression")//"")."~".
				    ($oldnode->getAttribute("min")//"")."~".
				    ($oldnode->getAttribute("max")//"")."|";
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
			$fark->pushModelTarget( split (/\~/, $target,-1) );
		    }
		};
	    };
	    if ( $modelDefault ) { # process model defaults
		#print "Model default: ". $modelDefault."\n";
		foreach my $set ( split (/\[/, $modelDefault)) {
		    foreach my $target ( split (/\|/, $set)) {
			$fark->addModelDefault( split (/\~/, $target,-1)); # "target:value"
		    };
		    $fark->pushModelDefault(); # "target:value"
		};
	    };
	    if ( $matchRules ) { # process match expressions
		foreach my $rule ( split (/\|/,  $matchRules) ) {
		    $fark->addExpression( split (/\~/, $rule,-1));
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
		    my $bufrType=($node->getAttribute("bufrType")//"");
		    my $subType=($node->getAttribute("subType")//"");
		    if ($bufrType ne "" && $subType ne "") {
			$fark->setObservationType( $bufrType, $subType); # defined observation filter
		    } else {
			print "Invalid BUFR/sub type: '".$bufrType."' '". $subType."'\n";
		    }
		    my $tablePath = $node->getAttribute("tablePath");
		    $fark->setObservationTablePath($tablePath); # define BUFR table path
		    my $indexTarget=($node->getAttribute("indexTarget") // "");
		    my $indexExp=($node->getAttribute("indexExp") // "");
		    if ( $indexTarget && $indexExp) {
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
			$fark->pushObservationTarget($name,$pos,$descr,$info,$min,$max);
		    };
		};
	    };
	    if ( $obsTargets ) { # process obs targets
		foreach my $target ( split (/\|/,  $obsTargets) ) {
		    $fark->pushObservationTarget( split (/\~/, $target,-1));
		};
	    };
	    if ($obsStart && $obsStop) {
		$fark->setObservationIndexLimits($obsStart, $obsStop); # "target:value"

	    };
	};

    #};
}; $ret=$@;
if ($ret) {
    term($ret);
} else {
    ##########################################################################
    # make the resulting XML
    ##########################################################################
    
    $ENV{"PRINT_TABLE_NAMES"}="FALSE";
    #print "Calling colocXML\n";
    my ($irc, $msg) = $fark->colocXML();
    
    eval {
	$log=capture {
	    ##########################################################################
	    # wrap up and free memory gracefully
	    ##########################################################################
	    
	    $fark->clearModelFileStack();
	    $fark->clearObservationFileStack();
	    
	};
    };   #$ret=$@;if ($ret) {term($ret);};
}
##########################################################################
# close session
##########################################################################
$log=capture {
    $fark->close();
};
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
};
sub term {
    my $msg=shift;
    $msg=~s/[^a-zA-Z0-9 _\-\+\.\,\/\:\[\]\(\)]/ /g;
    $msg=~s/\n/ /g;
    $msg=~s/ +/ /g;
    print "Content-type: text/xml;\n\n<?xml version='1.0' encoding='utf-8'?>\n";
    print "<error message='".$msg."'/>\n";
}
