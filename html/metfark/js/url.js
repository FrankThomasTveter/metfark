url_file="default.cfg";
url_config = { "default.cfg" : { modelConfigFile : { file: "default.cfg",
						     start: "", 
						     stop: "",
						     targets : { "def_model" : { variable : "def",
										  min: "def_min",
										  max : "def_max"} },
						     def : [ {targets: {"def_model": 101}, 
							      info:"default info"} ]
						   },
				 obsConfigFile : { file: "default.cfg",
						   start: "def_start",
						   stop : "def_stop",
						   targets : { "def_obs" : {bufrType:"", subType:"", 
									       pos:"", descr:"", min:"", max:""}
							     }
						 },
				 matchRules : { targets : {"def_target" : {expression:"",min:"",max:""}}},
				 password: ""
			       }
	     };
url_configEd = 0;
function url_getConfigFile() {
    return url_file;
};
function url_setConfigFile(value) {
    if (url_config[value] === undefined) {
	url_config[value]=clone(url_config[url_file]);
    }
    url_file=value;
    url_showURL();
}
function url_getModelConfigFile() {
    var file=url_getConfigFile();
    return url_config[file]["modelConfigFile"]["file"];
};
function url_getObsConfigFile() {
    var file=url_getConfigFile();
    return url_config[file]["obsConfigFile"]["file"];
};
function url_setConfig(type,parameter,val) {
    var file=url_getConfigFile();
    url_config[file][type][parameter]=val;
    // load if we are changing obs or model config files
    if (parameter === "file" && type === "model" && !modelLoaded) {
	documentLog.innerHTML="Sent "+type+"-load request.";
	$.get("cgi-bin/load.pl",{type:type},function(data, status){
	    dataToArray(data,status,documentLog);
	    modelLoaded=true;
	    documentLog.innerHTML="";
	});
    } else if (parameter === "file" && type === "obs" && !obsLoaded) {
	documentLog.innerHTML="Sent "+type+"-load request.";
	$.get("cgi-bin/load.pl",{type:type},function(data, status){
	    dataToArray(data,status,documentLog);
	    obsLoaded=true;
	    documentLog.innerHTML="";
	});
    }
    url_showURL();
}
function url_setConfigFilesTarget (type,target,parameter,val) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	url_config[file][type]['targets'][target][parameter]=val;
	url_showModelDefaultTable();
	url_showURL();
    }
}
function url_setConfigFilesDefault (ii,target,val) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	url_config[file]["modelConfigFile"]["def"][ii]["targets"][target]=val;
	url_showURL();
    }
}
function url_setConfigFilesDefaultInfo (ii,val) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	url_config[file]["modelConfigFile"]["def"][ii]["info"]=val;
	url_showURL();
    }
}

function url_newModelTarget(item) {
    var name=item.parentNode.parentNode.children[1].children[0].value;
    var variable=item.parentNode.parentNode.children[2].children[0].value;
    var minimum=item.parentNode.parentNode.children[4].children[0].value;
    var maximum=item.parentNode.parentNode.children[5].children[0].value;
    if (name !== "" & variable !== "") {
	var file= url_getConfigFile();
	if (url_config[file] === undefined) {
	    url_config[file]={modelConfigFile:{targets:{},def:{}},
			      obsConfigFile:{targets:{}},
			      matchRules:{ targets: {}},
			      password:""};
	};
	url_config[file]["modelConfigFile"]["targets"][name]={};
	url_config[file]["modelConfigFile"]["targets"][name]["variable"]=(variable || "");
	url_config[file]["modelConfigFile"]["targets"][name]["min"]=(minimum || "");
	url_config[file]["modelConfigFile"]["targets"][name]["max"]=(maximum || "");
	url_configEd++;
	//url_showModelTargetTable();
	//url_showModelDefaultTable();
	url_show();
	item.parentNode.parentNode.children[1].children[0].value="";	
	item.parentNode.parentNode.children[2].children[0].value="";	
	item.parentNode.parentNode.children[4].children[0].value="";	
	item.parentNode.parentNode.children[5].children[0].value="";	
    } else {
	alert("Invalid: name ('"+name+"'), variable ('"+variable+"')");
    }
};

function url_newModelDefault(item) {
    var file= url_getConfigFile();
    var line={targets:{},info:{}};
    var ok=false;
    var targets=url_config[file]["modelConfigFile"]["targets"];
    var pos=1;
    for (var target in targets) {
	var val=item.parentNode.parentNode.children[pos].children[0].value
	if (val !== undefined & val !== "") {
	    ok=true;
	}
	line["targets"][target]=(val|| "");
	item.parentNode.parentNode.children[pos].children[0].value="";
	pos=pos+1;
    }
    var info=item.parentNode.parentNode.children[pos].children[0].value;
    item.parentNode.parentNode.children[pos].children[0].value="";
    line["info"]=info;
    if (ok) {
	//console.log("url_config:",url_config[file]["modelConfigFile"]["def"]);
	url_config[file]["modelConfigFile"]["def"].push(line);
	//url_showModelDefaultTable();
	url_show();
    } else {
	alert("Invalid model default value.");
    }
};

function url_newObsTarget(item) {
    var name=item.parentNode.parentNode.children[1].children[0].value;
    var bufrType=item.parentNode.parentNode.children[2].children[0].value;
    var subType=item.parentNode.parentNode.children[4].children[0].value;
    var pos=item.parentNode.parentNode.children[6].children[0].value;
    var descr=item.parentNode.parentNode.children[8].children[0].value;
    var minimum=item.parentNode.parentNode.children[9].children[0].value;
    var maximum=item.parentNode.parentNode.children[10].children[0].value;
    console.log("Adding obsTarget:",name,bufrType,subType);
    if (name !== "" & bufrType !== ""& subType !== "") {
	var file= url_getConfigFile();
	if (url_config[file] === undefined) {
	    url_config[file]={modelConfigFile:{targets:{},def:{}},
				  obsConfigFile:{targets:{}},
				  matchRules:{ targets:{}},
				  password:""};
	};
	url_config[file]["obsConfigFile"]["targets"][name]={};
	url_config[file]["obsConfigFile"]["targets"][name]["bufrType"]=(bufrType || "");
	url_config[file]["obsConfigFile"]["targets"][name]["subType"]=(subType || "");
	url_config[file]["obsConfigFile"]["targets"][name]["pos"]=(pos || "");
	url_config[file]["obsConfigFile"]["targets"][name]["descr"]=(descr || "");
	url_config[file]["obsConfigFile"]["targets"][name]["min"]=(minimum || "");
	url_config[file]["obsConfigFile"]["targets"][name]["max"]=(maximum || "");
	url_configEd++;
	//url_showObsTargetTable();
	url_show();
	item.parentNode.parentNode.children[1].children[0].value="";
	item.parentNode.parentNode.children[2].children[0].value="";
	item.parentNode.parentNode.children[4].children[0].value="";
	item.parentNode.parentNode.children[6].children[0].value="";
	item.parentNode.parentNode.children[8].children[0].value="";
	item.parentNode.parentNode.children[9].children[0].value="";
	item.parentNode.parentNode.children[10].children[0].value="";
    } else {
	alert("Invalid: name ('"+name+"'), BUFR type ('"+bufrType+"'), subType ('"+subType+"')");
    }
};

function url_newTargetMatch(item) {
    var name=item.parentNode.parentNode.children[1].children[0].value;
    var expr=item.parentNode.parentNode.children[3].children[0].value;
    var minimum=item.parentNode.parentNode.children[5].children[0].value;
    var maximum=item.parentNode.parentNode.children[6].children[0].value;
    if (name !== "" & expr !== "" ) {
	var file= url_getConfigFile();
	if (url_config[file] === undefined) {
	    url_config[file]={modelConfigFile:{targets:{},def:{}},
				  obsConfigFile:{targets:{}},
				  matchRules:{targets:{}},
				  password:""};
	};
	url_config[file]["matchRules"]["targets"][name]={};
	url_config[file]["matchRules"]["targets"][name]["expression"]=(expr || "");
	url_config[file]["matchRules"]["targets"][name]["min"]=(minimum || "");
	url_config[file]["matchRules"]["targets"][name]["max"]=(maximum || "");
	url_configEd++;
	//url_showTargetMatchTable();
	url_show();
	item.parentNode.parentNode.children[1].children[0].value="";
	item.parentNode.parentNode.children[3].children[0].value="";
	item.parentNode.parentNode.children[5].children[0].value="";
	item.parentNode.parentNode.children[6].children[0].value="";
    } else {
	alert("Invalid: model target ('"+name+"'), observation target expression ('"+expr+"')");
    }
};

// create model target table
function url_showModelTargetTable() {
    var item=document.getElementById('modelTargetTable');
    var file=url_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsModelTarget","newlineModelTarget");
    var targets=url_config[file]["modelConfigFile"]["targets"];
    for (var target in targets) {
	url_insertModelTargetRow(tail,target,targets[target]["variable"],targets[target]["min"],targets[target]["max"]);
    }
};
// create scan table row
function url_insertModelTargetRow(item,target,variable,min,max) {
    var row = document.createElement("TR");
    var td, inp;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","url_removeModelTarget(this.parentNode.parentNode,'"+target+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make FILE NAME column
    td=document.createElement("TD");
    td.innerHTML=target;
    row.appendChild(td);
    // make variable column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",variable);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','variable',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-variable column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make minimum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','min',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make maximum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make add row to table
    item.parentNode.insertBefore(row,item);
    return row;
}
// create model target table
function url_showModelDefaultTable() {
    var item=document.getElementById('modelDefaultTable');
    var header=clearTableChild(item,"labelsModelDefault");
    var newline=clearTableChild(item,"newlineModelDefault");
    var file=url_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsModelDefault","newlineModelDefault");
    url_insertModelDefaultHeader(header,file);
    url_insertModelDefaultNewline(newline,file);
    url_insertModelDefaultRow(tail,file);
};
// create model default table header
function url_insertModelDefaultHeader(row,file) {
    var td,bf;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    var targets=url_config[file]["modelConfigFile"]["targets"];
    for (var target in targets) {
	// make variable names
	td=document.createElement("TD");
	bf=document.createElement("BF");
	bf.innerHTML=target;
	td.appendChild(bf);
	row.appendChild(td);
    }
    td=document.createElement("TD");
    bf=document.createElement("BF");
    bf.innerHTML="information";
    td.appendChild(bf);
    row.appendChild(td);
}

function url_insertModelDefaultRow(item,file) {
    var td;
    // make "-" column
    var defs=url_config[file]["modelConfigFile"]["def"];
    var len=defs.length;
    for (var ii=0;ii<len;ii++){
	if (url_config[file]["modelConfigFile"]["def"][ii]["targets"] !== undefined) {
	    var row = document.createElement("TR");
	    td=document.createElement("TD");
	    td.setAttribute("style","min-width:25px;width:25px");
	    var btn=document.createElement("BUTTON");
	    btn.setAttribute("onclick","url_removeModelDefault(this.parentNode.parentNode,'"+file+"',"+ii+")");
	    btn.setAttribute("style","width:100%");
	    var t=document.createTextNode("-");
	    btn.appendChild(t);
	    td.appendChild(btn);
	    row.appendChild(td);
	    var targets=url_config[file]["modelConfigFile"]["targets"];
	    for (var target in targets) {
		// make value column
		td=document.createElement("TD");
		td.setAttribute("class","fill");
		td.setAttribute("trg",target);
		inp=document.createElement("INPUT");
		inp.setAttribute("type","text");
		inp.setAttribute("value",(url_config[file]["modelConfigFile"]["def"][ii]["targets"][target]||""));
		inp.setAttribute("style","width:100%");
		inp.setAttribute("onblur","url_setConfigFilesDefault("+ii+",'"+target+"',this.value);");
		td.appendChild(inp);
		row.appendChild(td);
	    }
	    td=document.createElement("TD");
	    td.setAttribute("class","fill");
	    inp=document.createElement("INPUT");
	    inp.setAttribute("type","text");
	    inp.setAttribute("value",(url_config[file]["modelConfigFile"]["def"][ii]["info"]||""));
	    inp.setAttribute("style","width:100%");
	    inp.setAttribute("onblur","url_setConfigFilesDefaultInfo("+ii+",this.value);");
	    td.appendChild(inp);
	    row.appendChild(td);
	    // make add row to table
	    item.parentNode.insertBefore(row,item);
	};
    }
    return;
}

// create model default table newline
function url_insertModelDefaultNewline(row,file) {
    var td,btn,inp;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","url_newModelDefault(this)");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("+");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    var targets=url_config[file]["modelConfigFile"]["targets"];
    for (var target in targets) {
	// make variable names
	td=document.createElement("TD");
	td.setAttribute("class","fill");
	td.setAttribute("id","_"+target);
	inp=document.createElement("INPUT");
	inp.setAttribute("type","text");
	inp.setAttribute("value","");
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","this.value=this.value.replace(/[^\\d\\.]/g,'')");
	td.appendChild(inp);
	row.appendChild(td);
    }
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    td.setAttribute("id","information");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value","");
    inp.setAttribute("style","width:100%");
    td.appendChild(inp);
    row.appendChild(td);
}
function url_removeModelDefault(item,file,ii) {
    console.log("removing model default:",file,ii);
    url_config[file]["modelConfigFile"]["def"].splice(ii,1);
    //url_showModelDefaultTable();
    url_show();
};
function url_showObsTargetTable() {
    var item=document.getElementById('obsTargetTable');
    var file=url_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsObsTarget","newlineObsTarget");
    var targets=url_config[file]["obsConfigFile"]["targets"];
    for (var target in targets) {
	url_insertObsTargetRow(tail,target,targets[target]["bufrType"],targets[target]["subType"],targets[target]["pos"],
			   targets[target]["descr"],targets[target]["min"],targets[target]["max"]);
    }
};
// create scan table row
function url_insertObsTargetRow(item,target,bufrType,subType,pos,descr,min,max) {
    var row = document.createElement("TR");
    var td, inp;
    // make "-" column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","removeObsTarget(this.parentNode.parentNode,'"+target+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make NAME column  ***************************
    td=document.createElement("TD");
    td.innerHTML=target;
    row.appendChild(td);
    // make bufrType column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",bufrType);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('obsConfigFile','"+target+"','bufrType',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-variable column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make subType column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",subType);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('obsConfigFile','"+target+"','subType',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-subtype column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make descr column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",pos);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('obsConfigFile','"+target+"','pos',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-subtype column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",descr);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('obsConfigFile','"+target+"','descr',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make minimum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('obsConfigFile','"+target+"','min',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make maximum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('obsConfigFile','"+target+"','max',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
function url_showTargetMatchTable() {
    var item=document.getElementById('targetMatchTable');
    var file=url_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsTargetMatch","newlineTargetMatch");
    var targets=url_config[file]["matchRules"]["targets"];
    for (var target in targets) {
	url_insertTargetMatchRow(tail,target,targets[target]["expression"],targets[target]["min"],targets[target]["max"]);
    }
};
// create scan table row
function url_insertTargetMatchRow(item,target,expr,min,max) {
    var row = document.createElement("TR");
    var td, inp;
    // make "-" column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","url_removeTargetMatch(this.parentNode.parentNode,'"+target+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make model target column  ***************************
    td=document.createElement("TD");
    td.innerHTML=target;
    row.appendChild(td);
    // make select-model target column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make obs target expression column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",expr);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('matchRules','"+target+"','expr',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-obs target expression column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make minimum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('matchRules','"+target+"','min',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make maximum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('matchRules','"+target+"','max',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
function url_removeTargetMatch(item,target) {
    var file=url_getConfigFile();
    delete url_config[file]["matchRules"]["targets"][target];
    //url_showTargetMatchTable();
    url_show();
};
// URL methods
function url_show() {
    var item;
    var file=url_getConfigFile();
    var mod=(url_config[file]["modelConfigFile"]["file"] !== "");
    var obs=(url_config[file]["obsConfigFile"]["file"] !== "");
    if (mod) {
	(document.getElementById("displayModelSort")).setAttribute("style","");
	(document.getElementById("displayModelTargets")).setAttribute("style","");
    } else {
	(document.getElementById("displayModelSort")).setAttribute("style","display:none");
	(document.getElementById("displayModelTargets")).setAttribute("style","display:none");
	(document.getElementById("displayModelDefault")).setAttribute("style","display:none");
    };
    if (obs) {
	(document.getElementById("displayObsTime")).setAttribute("style","");
	(document.getElementById("displayObsTargets")).setAttribute("style","");
    } else {
	(document.getElementById("displayObsTime")).setAttribute("style","display:none");
	(document.getElementById("displayObsTargets")).setAttribute("style","display:none");
    }
    if (mod & obs) {
	(document.getElementById("displayMatchRules")).setAttribute("style","");
	(document.getElementById("displayModelDefault")).setAttribute("style","display:none");
    }else {
	(document.getElementById("displayMatchRules")).setAttribute("style","display:none");
	if (mod) {
	    (document.getElementById("displayModelDefault")).setAttribute("style","");
	} else {
	    (document.getElementById("displayModelDefault")).setAttribute("style","display:none");
	}
    }
    setValue('urlConfigFile',file);
    setValue('urlModelConfigFile',url_config[file]["modelConfigFile"]["file"]);
    setValue('urlObsConfigFile',url_config[file]["obsConfigFile"]["file"]);
    url_showModelTargetTable();
    url_showModelDefaultTable();
    url_showObsTargetTable();
    url_showTargetMatchTable();
    url_showURL();
}

function url_showURL() {
    var href="http://cgi-bin/request.pl";
    var file=url_getConfigFile();
    var mod=(url_config[file]["modelConfigFile"]["file"] !== "");
    var obs=(url_config[file]["obsConfigFile"]["file"] !== "");
    if (mod) {
	href=href+"?model="+url_config[file]["modelConfigFile"]["file"];
	var targets=url_config[file]["modelConfigFile"]["targets"];
	for (var target in targets) {
	    href=href+"?modelTarget="+target+"|"+targets[target]["variable"]+"|"+targets[target]["min"]+"|"+targets[target]["max"];
	}
	if (! obs) {
	    var defs=url_config[file]["modelConfigFile"]["def"];
	    var len=defs.length;
	    for (var ii=0;ii<len;ii++){
		var modelDef="";
		for (var target in defs[ii]["targets"]){
		    modelDef=modelDef+target+":"+defs[ii]["targets"][target]+"|";
		}
		if (modelDef) {
		    href=href+"?modelDef="+modelDef;
		};
	    };
	}
    };
    if (obs) {
	href=href+"?obsModel="+url_config[file]["obsConfigFile"]["file"];
	if (url_config[file]["obsConfigFile"]["start"]) {
	    href=href+"?obsStart="+ url_config[file]["obsConfigFile"]["start"];
	};
	if (url_config[file]["obsConfigFile"]["stop"]) {
	    href=href+"?obsStop="+ url_config[file]["obsConfigFile"]["stop"];
	};
	var targets=url_config[file]["obsConfigFile"]["targets"];
	for (var target in targets) {
	    href=href+"?obsTarget="+target+"|"+targets[target]["subType"]+"|"+
		targets[target]["pos"]+"|"+
		targets[target]["descr"]+"|"+
		targets[target]["min"]+"|"+
		targets[target]["max"];
	}
	if (mod) {
	    var targets=url_config[file]["matchRules"]["targets"];
	    for (var target in targets) {
		href=href+"?match="+target+"|"+targets[target]["expression"]+"|"+
		    targets[target]["min"]+"|"+
		    targets[target]["max"];
	    }
	}
    };
    document.getElementById("urlLink").innerHTML=href;
    document.getElementById("urlLink").href=href;
}

function url_removeModelTarget(item,target) {
    var file=url_getConfigFile();
    delete url_config[file]["modelConfigFile"]["targets"][target];
    if (obs_isEmpty(url_config[file]["modelConfigFile"]["targets"])) {
	delete url_config[file]["modelConfigFile"]["def"];
	url_config[file]["modelConfigFile"]["def"]=[];
    }
    //url_showModelTargetTable();
    //url_showModelDefaultTable();
    url_show();
};
function removeObsTarget(item,target) {
    var file=url_getConfigFile();
    delete url_config[file]["obsConfigFile"]["targets"][target];
    if (obs_isEmpty(url_config[file]["obsConfigFile"]["targets"])) {
	delete url_config[file]["obsConfigFile"]["def"];
	url_config[file]["obsConfigFile"]["def"]=[];
    }
    //url_showObsTargetTable();
    url_show();
};


function url_saveConfigFile(target) {
    var file=url_getConfigFile();
    var password=document.getElementById("urlConfigFilePsw").value;
    var modelFile = url_config[file]["modelConfigFile"]["file"];
    var modelStart = url_config[file]["modelConfigFile"]["start"];
    var modelStop = url_config[file]["modelConfigFile"]["stop"];
    var modelTargets = "";
    var modelTrg=url_config[file]["modelConfigFile"]["targets"];
    for (var target in modelTrg) {
	modelTargets=modelTargets + "|" + target + "/" + 
	    modelTrg[target]["variable"] + "/" + 
	    modelTrg[target]["min"] + "/" + 
	    modelTrg[target]["max"];
    };
    var modelDefault = "";
    var modelDef=url_config[file]["modelConfigFile"]["def"];
    var len=modelDef.length;
    for (var ii=len-1; ii>=0;ii--) {
	var info=modelDef[ii]["info"];
	var defTrg=modelDef[ii]["targets"];
	var first=true;
	for (var target in defTrg) {
	    if (first) {
		modelDefault=modelDefault + "[" + info;
		first=false;
	    };
	    modelDefault=modelDefault + "|" + 
		target + "/" + 
		defTrg[target];
	};
    };
    var obsFile = url_config[file]["obsConfigFile"]["file"];
    var obsStart = url_config[file]["modelConfigFile"]["start"];
    var obsStop = url_config[file]["modelConfigFile"]["stop"];
    var obsTargets = "";
    var obsTrg=url_config[file]["obsConfigFile"]["targets"];
    for (var target in obsTrg) {
	obsTargets=obsTargets + "|" + target + "/" + 
	    obsTrg[target]["bufrType"] + "/" + 
	    obsTrg[target]["subType"] + "/" + 
	    obsTrg[target]["pos"] + "/" + 
	    obsTrg[target]["descr"] + "/" + 
	    obsTrg[target]["min"] + "/" + 
	    obsTrg[target]["max"];
    };
    var matchRules = "";
    var matchTrg=url_config[file]["matchRules"]["targets"];
    for (var target in matchTrg) {
	matchRules=matchRules + "|" + target + "/" + 
	    matchTrg[target]["expression"] + "/" + 
	    matchTrg[target]["min"] + "/" + 
	    matchTrg[target]["max"];
    };
    documentLog.innerHTML="Sent url-save request.";
    $.get("cgi-bin/save.pl",{type:"url",file:file,password:password,
			     modelFile:modelFile,
			     modelStart:modelStart,
			     modelStop:modelStop,
			     modelTargets:modelTargets,
			     modelDefault:modelDefault,
			     obsFile:obsFile,
			     obsStart:obsStart,
			     obsTargets:obsTargets,
			     matchRules:matchRules},
	  function(data, status){
	      if (status == "success") {
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
function url_showConfig() {
    var file=url_getConfigFile();
    if (url_config[file] === undefined) { // create new entry locally...
	url_config[file]={modelConfigFile:{targets:{},def:{}},
			  obsConfigFile:{targets:{}},
			  matchRules:{targets:{}},
			  password:""};
	url_config[file]["modelConfigFile"]["file"]=document.getElementById("urlModelConfigFile").value;
	url_config[file]["modelConfigFile"]["start"]=document.getElementById("urlModelSortStart").value;
	url_config[file]["modelConfigFile"]["stop"]=document.getElementById("urlModelSortStop").value;
	url_config[file]["obsConfigFile"]["file"]=document.getElementById("urlObsConfigFile").value;
	url_config[file]["obsConfigFile"]["start"]=document.getElementById("urlObsTimeStart").value;
	url_config[file]["obsConfigFile"]["stop"]=document.getElementById("urlObsTimeStop").value;
	// model targets
	var item=document.getElementById("modelTargetTable");
	var tbody=item.children[0];
	var children=tbody.children;
	var len=children.length;
	url_config[file]["modelConfigFile"]["targets"]={};
	for (var ii=len-1;ii>=0;ii--){
	    //console.log("RemoveTableChildFromTo ",ii,children[ii],len,children.length);
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsModelTarget" & att !== "newlineModelTarget") {
			var name=children[ii].children[1].innerHTML;
			var variable=children[ii].children[2].children[0].value;
			var min=children[ii].children[4].children[0].value;
			var max=children[ii].children[5].children[0].value;
			url_config[file]["modelConfigFile"]["targets"][name]=
			    {variable:variable,min:min,max:max};;
		    }
		}
	    }
	};
	// model default
	item=document.getElementById("modelDefaultTable");
	tbody=item.children[0];
	children=tbody.children;
	len=children.length;
	url_config[file]["modelConfigFile"]["def"]=[];
	for (var ii=len-1;ii>=0;ii--){
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsModelDefault" & att !== "newlineModelDefault") {
			var clen=children[ii].children.length;
			var info=children[ii].children[clen-1].children[0].value;
			var targets={};
			for (var jj=0;jj<clen-1;jj++) {
			    var trg=children[ii].children[jj].getAttribute("trg");
			    if (trg !== undefined) {
				var value=children[ii].children[jj].children[0].value;
				targets[trg]=value;
			    } else {
				console.log("Warning unknown target detected.");
			    }
			}
			url_config[file]["modelConfigFile"]["def"].push({targets:targets,info:info});
		    }
		}
	    }
	};
	// obs targets
	item=document.getElementById("obsTargetTable");
	tbody=item.children[0];
	children=tbody.children;
	len=children.length;
	url_config[file]["obsConfigFile"]["targets"]={};
	for (var ii=len-1;ii>=0;ii--){
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsObsTarget" & att !== "newlineObsTarget") {
			var name=children[ii].children[1].innerHTML;
			var bufrType=children[ii].children[2].children[0].value;
			var subType=children[ii].children[4].children[0].value;
			var pos=children[ii].children[6].children[0].value;
			var descr=children[ii].children[8].children[0].value;
			var min=children[ii].children[9].children[0].value;
			var max=children[ii].children[10].children[0].value;
			url_config[file]["obsConfigFile"]["targets"][name]=
			    {bufrType:bufrType,subType:subType,pos:pos,descr:descr,min:min,max:max};
		    }
		}
	    }
	};
	// match rules
	item=document.getElementById("targetMatchTable");
	tbody=item.children[0];
	children=tbody.children;
	len=children.length;
	url_config[file]["matchRules"]["targets"]={};
	for (var ii=len-1;ii>=0;ii--){
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsTargetMatch" & att !== "newlineTargetMatch") {
			var name=children[ii].children[1].innerHTML;
			var expression=children[ii].children[3].children[0].value;
			var min=children[ii].children[5].children[0].value;
			var max=children[ii].children[6].children[0].value;
			url_config[file]["matchRules"]["targets"][name]=
			    {expression:expression,min:min,max:max};
		    }
		}
	    }
	};
	url_configEd++;
    } else { // load local values to screen
	setValue('urlModelConfigFile',url_config[file]["modelConfigFile"]["file"]);
	setValue('urlModelSortStart',url_config[file]["modelConfigFile"]["start"]);
	setValue('urlModelSortStop',url_config[file]["modelConfigFile"]["stop"]);
	setValue('urlObsConfigFile',url_config[file]["obsConfigFile"]["file"]);
	setValue('urlObsTimeStart',url_config[file]["obsConfigFile"]["start"]);
	setValue('urlObsTimeStop',url_config[file]["obsConfigFile"]["stop"]);
	url_show();
    }
};
function url_updateData() {
	documentLog.innerHTML="Sent url-load request.";
	$.get("cgi-bin/load.pl",{type:"url"},function(data, status){
	    dataToArray(data,status,documentLog);
	    if (! modelLoaded) {
		documentLog.innerHTML="Sent model-load request.";
		$.get("cgi-bin/load.pl",{type:"model"},function(data, status){
		    dataToArray(data,status,documentLog);
		    modelLoaded=true;
		    if (! obsLoaded) {
			documentLog.innerHTML="Sent obs-load request.";
			$.get("cgi-bin/load.pl",{type:"obs"},function(data, status){
			    dataToArray(data,status,documentLog);
			    obsLoaded=true;
			    url_show();
			    documentLog.innerHTML="";
			});
		    } else {
			url_show();
			documentLog.innerHTML="";
		    }
		});
	    } else if (!obsLoaded) {
		documentLog.innerHTML="Sent obs-load request.";
		$.get("cgi-bin/load.pl",{type:"obs"},function(data, status){
		    dataToArray(data,status,documentLog);
		    obsLoaded=true;
		    url_show();
		    documentLog.innerHTML="";
		});
	    } else {
		url_show();
		documentLog.innerHTML="";
	    }
	});
};
function url_getModelSortStart(target) {
};
function url_getModelSortStop(target) {
};
function url_addModelTarget(target) {
};
function url_getObsTargetBufrType() {
    var item=document.getElementById("obsTargetTable");
    var newline=getChild(item,"newlineObsTarget");
    return newline.children[2].children[0].value;
};
function url_getObsTargetSubType() {
    var item=document.getElementById("obsTargetTable");
    var newline=getChild(item,"newlineObsTarget");
    return newline.children[4].children[0].value;
};
