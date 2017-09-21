obs_file = "default.cfg";
obs_config = { "default.cfg" : { fileFilterDir : "/opdata/",
				 fileFilter : "def_.*\.bufr",
				 tablePath : "/var/tables/",
				 hits : "?",
				 bufr : { 99 : { 9 : { seq: [{pos: 1, descr:99999, info: "default info"}],
						       info : "more default info"
						     }
					       }
					},
				 filter : { 99 : {9 : "default info"}},
				 password: "test"
			       }
	     };
obs_configEd = 0;

function obs_setConfigFile(value) {
    if (obs_config[value] === undefined) {
	obs_config[value]=clone(obs_config[obs_file]);
	//console.log("cloned:",value,obs_config[value]);
    }
    obs_file=value;
}
function obs_getConfigFile() {
    return obs_file;
};
function obs_setArray(parameter,value) {
    var file=obs_getConfigFile();
    //console.log("File:",file,parameter,obs_config[file]);
    obs_config[file][parameter]=value;
};
function obs_show() {
    var file=obs_getConfigFile();
    setValue('obsConfigFile',file);
    setValue('obsFileFilterDir',obs_config[file]["fileFilterDir"]);
    setValue('obsFileFilter',obs_config[file]["fileFilter"]);
    setValue('obsTablePath',obs_config[file]["tablePath"]);
    obs_setFilterTable(file,obs_config[file]['filter']);
    setInnerHTML('obsPatternHits',obs_config[file]["hits"]);
};
// observation config methods
function obs_checkPassword() {
    var password=document.getElementById("obsConfigFilePsw").value;
    var file=obs_getConfigFile();
    if (obs_config[file] !== undefined) {
	if (obs_config[file]["password"] !== undefined) {
	    if (obs_config[file]["password"] !== password) {
		alert("Invalid password used when attempting to save file:\n"+file);
		return false;
	    };
	};
    };
    return true;
}
function obs_removeFilter(item,file,bufrType,subType) {
    //if (! obs_checkPassword()) {return;}
    //item.parentNode.removeChild(item);
    delete obs_config[file]["filter"][bufrType][subType];
    if (obs_isEmpty(obs_config[file]["filter"][bufrType])) {
	delete obs_config[file]["filter"][bufrType];
    }
    obs_setFilterTable(file,obs_config[file]["filter"]);
};

function obs_isEmpty(obj) {
    for(var prop in obj) {
        if(obj.hasOwnProperty(prop))
            return false;
    }
    return true;
}
function obs_newFilter(item) {
    //if (! obs_checkPassword()) {return;}
    var bufrType=item.parentNode.parentNode.children[1].children[0].value;
    var subType=item.parentNode.parentNode.children[3].children[0].value;
    var info=item.parentNode.parentNode.children[5].children[0].value;
    if (bufrType !== "" & subType !== "") {
	var file= obs_getConfigFile();
	if (obs_config[file] === undefined) {
	    obs_config[file]={};
	};
	if (obs_config[file]["filter"] === undefined) {
	    obs_config[file]["filter"]={};
	};
	if (obs_config[file]["filter"][bufrType] === undefined) {
	    obs_config[file]["filter"][bufrType]={};
	};
	obs_config[file]["filter"][bufrType][subType]=info;
	obs_setFilterTable(file,obs_config[file]["filter"]);
	item.parentNode.parentNode.children[1].children[0].value="";
	item.parentNode.parentNode.children[3].children[0].value="";
	item.parentNode.parentNode.children[5].children[0].value="";
    } else {
	alert("Invalid: BufrType ('"+bufrType+"'), SubType ('"+subType+"')");
    }
};
function obs_saveConfigFile() {
    var file=obs_getConfigFile();
    var password=document.getElementById("obsConfigFilePsw").value;
    var obsBufr="";
    var bufr=obs_config[file]["bufr"];
    for (var bufrType in bufr) {
	for (var subType in bufr[bufrType]) {
	    var info=bufr[bufrType][subType]["info"];
	    var seq=bufr[bufrType][subType]["seq"];
	    obsBufr=obsBufr+"|"+bufrType+"/"+subType+"/"+info;
	    var len=seq.length;
	    for (var jj=0;jj<len;jj++) {
		var pos=seq[jj]["pos"];
		var descr=seq[jj]["descr"];
		var pinfo=seq[jj]["info"];
		obsBufr=obsBufr+"/"+pos+"#"+descr+"#"+pinfo;
	    }
	}
    };
    var obsFilter="";
    var filter=obs_config[file]["filter"];
    for (var bufrType in filter) {
	for (var subType in filter[bufrType]) {
	    var info=filter[bufrType][subType];
	    obsFilter=obsFilter + "|" + bufrType + "/" + subType + "/" + info;
	}
    };
    obs_configEd++;
    documentLog.innerHTML="Sent obs-save request.";
    $.get("cgi-bin/save.pl",{type:"obs",file:file,password:password,
			     filterDir:obs_config[file]["fileFilterDir"],
			     filter:obs_config[file]["fileFilter"],
			     table:obs_config[file]["tablePath"],
			     obsBufr:obsBufr,
			     obsFilter:obsFilter
			    },
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to save file: "+file+"\n"+msg);
	      };
	      documentLog.innerHTML="";}
				}
	 );
};
function obs_setFileFilter(bufrType,subType,info) {
    var file=obs_getConfigFile();
    if (obs_config[file] !== undefined) {
	obs_config[file]["filter"][bufrType][subType]=info;
    };
}
function obs_getFilterBufrType() {
    var item=document.getElementById("obsFilterTable");
    var newline=getChild(item,"newlineFilter");
    return newline.children[1].children[0].value;
};
// make new obs-filter entry
function obs_setFilterTable(file,value) {
    var item=document.getElementById('obsFilterTable');
    var tail=removeTableChildFromTo(item,"labelsFilter","newlineFilter");
    for (var bufrType in value) {
	for (var subType in value[bufrType]) {
	    obs_insertFilterRow(tail,file,bufrType,subType,value[bufrType][subType]);
	}
    }
}
function obs_insertFilterRow(item,file,bufrType,subType,info) {
    var row = document.createElement("TR");
    var td,inp;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","obs_removeFilter(this.parentNode.parentNode,'"+file+"','"+bufrType+"','"+subType+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make BufrType column
    td=document.createElement("TD");
    td.innerHTML=bufrType;
    row.appendChild(td);
    // make select-BufrType column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make SubType column
    td=document.createElement("TD");
    td.innerHTML=subType;
    row.appendChild(td);
    // make select-SubType column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make info column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",info);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setFileFilter('"+bufrType+"','"+subType+"',this.value)");
    td.appendChild(inp);
    row.appendChild(td);
    // make add row to table
    item.parentNode.insertBefore(row,item);
    return row;
}
function obs_updataData() {
    documentLog.innerHTML="Sent obs-load request.";
    $.get("cgi-bin/load.pl",{type:"obs"},function(data, status){
	dataToArray(data,status,documentLog);
	obsLoaded=true;
	//console.log("Updating dropdown for ",target);
	obs_show();
	documentLog.innerHTML="";
    });
};
function obs_patternFind() {
    var file=obs_getConfigFile();
    var password=document.getElementById("obsConfigFilePsw").value;
    var filterDir = obs_config[file]["fileFilterDir"];
    var filter = obs_config[file]["fileFilter"];
    var table = obs_config[file]["tablePath"];
    documentLog.innerHTML="Sent obs-find request.";
    $.get("cgi-bin/find.pl",{type:"obs",
			     file:file,
			     password:password,
			     filterDir:obs_config[file]["fileFilterDir"],
			     filter:obs_config[file]["fileFilter"],
			     table:obs_config[file]["tablePath"]},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to peek at: "+filterDir+" "+filter+" (file:"+file+")\n"+msg);
	      } else {
		  dataToArray(data,status,documentLog);
		  obs_show();
	      };
	      documentLog.innerHTML="";}
				}
	 );
};
