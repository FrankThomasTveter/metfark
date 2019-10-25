url_file="default.cfg";
url_config = { "default.cfg" : { modelConfigFile : { file: "default.cfg",
						     targets : { "def_model" : { variable : "def",
										  min: "def_min",
										  max : "def_max"} },
						     def : [ {targets: {"def_model": 101}, 
							      info:"default info"} ]
						   },
				 obsConfigFile : { file: "default.cfg",
						   start: "def_start",
						   stop : "def_stop",
						   targets : { "def_obs" : {pos:"", descr:"", info:"",  min:"", max:""}
							     }
						 },
				 matchRules : { targets : {"def_target" : {exp:""}}},
				 host:"localhost",
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
	$.get("cgi-bin/fark_load.pl",{type:type},function(data, status){
	    dataToArray(data,status,documentLog);
	    modelLoaded=true;
	    documentLog.innerHTML="";
	});
    } else if (parameter === "file" && type === "obs" && !obsLoaded) {
	documentLog.innerHTML="Sent "+type+"-load request.";
	$.get("cgi-bin/fark_load.pl",{type:type},function(data, status){
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
	if (url_config[file][type] === undefined || 
	    url_config[file][type]['targets'] ===undefined ||
	    url_config[file][type]['targets'][target] ===undefined ||
	    url_config[file][type]['targets'][target][parameter] ===undefined) {
	    console.log("Undefined:",type,target,parameter,val);
	};
	url_config[file][type]['targets'][target][parameter]=val;
	url_showModelDefaultTable();
	url_showURL();
    }
}
function url_setArray (type,parameter,val) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	url_config[file][type][parameter]=val;
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
			      host:"localhost",
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
    var ofile=url_getObsConfigFile();
    if ( obs_config[ofile] !== undefined) {
	var name=item.parentNode.parentNode.children[1].children[0].value;
	if (obs_config[ofile]["targets"][name] === undefined) {
	    var pos=item.parentNode.parentNode.children[2].children[0].value;
	    var descr=item.parentNode.parentNode.children[4].children[0].value;
	    var info=item.parentNode.parentNode.children[5].children[0].value;
	    var minimum=item.parentNode.parentNode.children[6].children[0].value;
	    var maximum=item.parentNode.parentNode.children[7].children[0].value;
	    var bufrType = obs_config[ofile]["bufrType"];
	    var subType = obs_config[ofile]["subType"];
	    if (name !== "" & bufrType !== ""& subType !== "") {
		var file= url_getConfigFile();
		if (url_config[file] === undefined) {
		    url_config[file]={modelConfigFile:{targets:{},def:{}},
				      obsConfigFile:{targets:{}},
				      matchRules:{ targets:{}},
				      host:"localhost",
				      password:""};
		};
		url_config[file]["obsConfigFile"]["targets"][name]={};
		url_config[file]["obsConfigFile"]["targets"][name]["pos"]=(pos || "");
		url_config[file]["obsConfigFile"]["targets"][name]["descr"]=(descr || "");
		url_config[file]["obsConfigFile"]["targets"][name]["info"]=(info || "");
		url_config[file]["obsConfigFile"]["targets"][name]["min"]=(minimum || "");
		url_config[file]["obsConfigFile"]["targets"][name]["max"]=(maximum || "");
		url_configEd++;
		//url_showObsTargetTable();
		url_show();
		item.parentNode.parentNode.children[1].children[0].value="";
		item.parentNode.parentNode.children[2].children[0].value="";
		item.parentNode.parentNode.children[4].children[0].value="";
		item.parentNode.parentNode.children[5].children[0].value="";
		item.parentNode.parentNode.children[6].children[0].value="";
		item.parentNode.parentNode.children[7].children[0].value="";
	    } else {
		alert("Invalid: name ('"+name+"'), BUFR type ('"+bufrType+"'), subType ('"+subType+"')");
	    }
	} else {
	    alert("'"+name+"' already used in obs config file: '"+ofile+"'");
	}
    } else {
	alert("Obs config file not loaded ('"+ofile+"')");
    }
};

function url_newTargetMatch(item) {
    var name=item.parentNode.parentNode.children[1].children[0].value;
    var expr=item.parentNode.parentNode.children[3].children[0].value;
    if (name !== "" & expr !== "" ) {
	var file= url_getConfigFile();
	if (url_config[file] === undefined) {
	    url_config[file]={modelConfigFile:{targets:{},def:{}},
			      obsConfigFile:{targets:{}},
			      matchRules:{targets:{}},
			      host:"localhost",
			      password:""};
	};
	url_config[file]["matchRules"]["targets"][name]={};
	url_config[file]["matchRules"]["targets"][name]["exp"]=(expr || "");
	//url_showTargetMatchTable();
	item.parentNode.parentNode.children[1].children[0].value="";
	item.parentNode.parentNode.children[3].children[0].value="";
	url_show();
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
// create auto table row
function url_insertModelTargetRow(item,target,variable,min,max) {
    var row = document.createElement("TR");
    var file = url_getModelConfigFile();
    var mark=(variable === model_config[file]["index"]);
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
    if (mark) {
	td.setAttribute("style","color:blue");
    } else {
	td.setAttribute("style","");
    }
    td.innerHTML=target;
    row.appendChild(td);
    // make variable column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",variable);
    if (mark) {
	inp.setAttribute("style","width:100%;color:blue");
    } else {
	inp.setAttribute("style","width:100%");
    }
    inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','variable',this.value);url_showModelTargetTable()");
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
    if (mark) {
	inp.setAttribute("id","urlModelIndexStart");
	inp.setAttribute("style","width:125px");
	inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','min',this.value);url_setArray('modelConfigFile','start',this.value);url_showModelTargetTable();");
    } else {
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','min',this.value);");
    }
    td.appendChild(inp);
    if (mark) {
	var btn=document.createElement("BUTTON");
	btn.setAttribute("onclick","url_getModelIndexStart('urlModelIndexStart','"+target+"')");
	btn.setAttribute("style","width:25px");
	var t=document.createTextNode("←"); // "→"
	btn.appendChild(t);
	td.appendChild(btn);
    }
    row.appendChild(td);
    // make maximum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    if (mark) {
	inp.setAttribute("id","urlModelIndexStop");
	inp.setAttribute("style","width:125px");
	inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);");
	inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);url_setArray('modelConfigFile','stop',this.value);url_showModeltargetTable();");
    } else {
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);");
    }
    td.appendChild(inp);
    if (mark) {
	var btn=document.createElement("BUTTON");
	btn.setAttribute("onclick","url_getModelIndexStop('urlModelIndexStop','"+target+"')");
	btn.setAttribute("style","width:25px");
	var t=document.createTextNode("→"); // "←"
	btn.appendChild(t);
	td.appendChild(btn);
    }
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
    var ofile=url_getObsConfigFile();
    var file=url_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsObsTarget","newlineObsTarget");
    // insert obs targets from obs-config file
    var otargets=obs_config[ofile]["targets"];
    for (var target in otargets) {
	url_insertOTargetRow(tail,target,otargets[target]["pos"],otargets[target]["descr"],
			   otargets[target]["info"],otargets[target]["min"],otargets[target]["max"]);
    }
    // insert obs target index expression from obs-config file
    // make "-" column  ***************************
    var row = document.createElement("TR");
    var td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make NAME column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","color:green");
    var target=obs_config[ofile]["indexTarget"];
    td.innerHTML=target;
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("colspan","4");
    td.innerHTML=obs_config[ofile]["indexExp"];
    row.appendChild(td);
    // make minimum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",url_config[file]["obsConfigFile"]["start"]);
    inp.setAttribute("id","urlObsIndexStart");
    inp.setAttribute("style","width:125px");
    inp.setAttribute("onblur","url_setArray('obsConfigFile','start',this.value);");
    td.appendChild(inp);
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","url_getObsIndexStart('urlObsIndexStart','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("←"); // "→"
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make maximum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",url_config[file]["obsConfigFile"]["stop"]);
    inp.setAttribute("id","urlObsIndexStop");
    inp.setAttribute("style","width:125px");
    inp.setAttribute("onblur","url_setArray('obsConfigFile','stop',this.value);");
    td.appendChild(inp);
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","url_getObsIndexStop('urlObsIndexStop','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("→"); // "←"
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    tail.parentNode.insertBefore(row,tail);
    // insert obs targets from url-config file
    var targets=url_config[file]["obsConfigFile"]["targets"];
    for (var target in targets) {
	url_insertObsTargetRow(tail,target,targets[target]["pos"],targets[target]["descr"],
			   targets[target]["info"],targets[target]["min"],targets[target]["max"]);
    }
};
// create auto table row
function url_insertOTargetRow(item,target,pos,descr,info,min,max) {
    var row = document.createElement("TR");
    var td, inp;
    // make "-" column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make NAME column  ***************************
    td=document.createElement("TD");
    td.innerHTML=target;
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.innerHTML=pos;
    row.appendChild(td);
    // make select-pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make descr column  ***************************
    td=document.createElement("TD");
    td.innerHTML=descr;
    row.appendChild(td);
    // make info column  ***************************
    td=document.createElement("TD");
    td.innerHTML=info;
    row.appendChild(td);
    // make min column  ***************************
    td=document.createElement("TD");
    td.innerHTML=min;
    row.appendChild(td);
    // make max column  ***************************
    td=document.createElement("TD");
    td.innerHTML=max;
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
// create auto table row
function url_insertObsTargetRow(item,target,pos,descr,info,min,max) {
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
    // make pos column  ***************************
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
    // make pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",info);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('obsConfigFile','"+target+"','info',this.value);");
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
	url_insertTargetMatchRow(tail,target,targets[target]["exp"]);
    };
    url_check('matchExpression','urlDebugExpression');

};
// create auto table row
function url_insertTargetMatchRow(item,target,expr) {
    var row = document.createElement("TR");
    var file = url_getConfigFile();
    if (url_config[file]!== undefined){
	var mfile = url_getModelConfigFile();
	var ofile = url_getObsConfigFile();
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
	if (model_config[mfile] !== undefined) {
	    if (url_config[file]["modelConfigFile"]["targets"][target] !== undefined) {
		var variable=url_config[file]["modelConfigFile"]["targets"][target]["variable"];
		var mark=(variable === model_config[mfile]["index"]);
		if (mark) {
		    td.setAttribute("style","color:blue");
		};
	    } else {
		td.setAttribute("style","color:red");
	    }
	};
	td.innerHTML=target;
	row.appendChild(td);
	// make select-model target column  ***************************
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px");
	row.appendChild(td);
	// make obs target expression column  ***************************
	td=document.createElement("TD");
	td.setAttribute("class","fill");
	if (obs_config[ofile] !== undefined) {
	    var variable=obs_config[ofile]["indexTarget"];
	    var mark=(expr.indexOf(variable) > -1);
	    if (mark) {
		td.setAttribute("style","color:green");
	    };
	};
	inp=document.createElement("INPUT");
	inp.setAttribute("type","text");
	inp.setAttribute("value",expr);
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","url_setConfigFilesTarget('matchRules','"+target+"','exp',this.value);url_showTargetMatchTable();");
	td.appendChild(inp);
	row.appendChild(td);
	// make select-obs target expression column  ***************************
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px");
	row.appendChild(td);
	// make add row to table  ***************************
	item.parentNode.insertBefore(row,item);
	return row;
    }
};
function url_removeTargetMatch(item,target) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	var item=document.getElementById("newlineTargetMatch");
	item.children[1].children[0].value=target;
	item.children[3].children[0].value=url_config[file]["matchRules"]["targets"][target]["exp"];
	delete url_config[file]["matchRules"]["targets"][target];
	//url_showTargetMatchTable();
	url_show();
    };
};
// URL methods
function url_show() {
    var item;
    var file=url_getConfigFile();
    var mod=(url_config[file]["modelConfigFile"]["file"] !== "");
    var obs=(url_config[file]["obsConfigFile"]["file"] !== "");
    if (mod) {
	(document.getElementById("displayModelTargets")).setAttribute("style","");
	url_showModelTargetTable();
	url_showModelDefaultTable();
    } else {
	(document.getElementById("displayModelTargets")).setAttribute("style","display:none");
	(document.getElementById("displayModelDefault")).setAttribute("style","display:none");
    };
    if (obs) {
	url_showObsTargetTable();
	(document.getElementById("displayObsTargets")).setAttribute("style","");
    } else {
	(document.getElementById("displayObsTargets")).setAttribute("style","display:none");
    }
    if (mod & obs) {
	(document.getElementById("displayMatchRules")).setAttribute("style","");
	(document.getElementById("displayModelDefault")).setAttribute("style","display:none");
	url_showTargetMatchTable();
	url_check('matchExpression','urlDebugExpression');
    }else {
	(document.getElementById("displayMatchRules")).setAttribute("style","display:none");
	(document.getElementById("urlDebugExpression")).setAttribute("style","display:none");
	if (mod) {
	    (document.getElementById("displayModelDefault")).setAttribute("style","");
	} else {
	    (document.getElementById("displayModelDefault")).setAttribute("style","display:none");
	}
	url_check('matchExpression','urlDebugExpression');
    }
    showValue('urlConfigFile',file);
    showValue('urlModelConfigFile',url_config[file]["modelConfigFile"]["file"]);
    showValue('urlObsConfigFile',url_config[file]["obsConfigFile"]["file"]);
    url_showURL();
}

function url_showURL() {
    var file=url_getConfigFile();
    var host=url_config[file]["host"];
    var href="http://"+host+"/cgi-bin/fark_url.pl?urlFile="+file;
    // if (mod) {
    // 	href=href+"?modelFile="+url_config[file]["modelConfigFile"]["file"];
    // 	if (url_config[file]["modelConfigFile"]["start"]) {
    // 	    href=href+"?modelStart="+ url_config[file]["modelConfigFile"]["start"];
    // 	};
    // 	if (url_config[file]["modelConfigFile"]["stop"]) {
    // 	    href=href+"?modelStop="+ url_config[file]["modelConfigFile"]["stop"];
    // 	};
    // 	var targets=url_config[file]["modelConfigFile"]["targets"];
    // 	var modelTargets="";
    // 	for (var target in targets) {
    // 	    modelTargets=modelTargets+target+"~"+targets[target]["variable"]+"~"+targets[target]["min"]+"~"+targets[target]["max"]+"|";
    // 	}
    // 	href=href+"?modelTargets="+modelTargets;
    // 	if (! obs) {
    // 	    var modelDefs="";
    // 	    var defs=url_config[file]["modelConfigFile"]["def"];
    // 	    var len=defs.length;
    // 	    for (var ii=0;ii<len;ii++){
    // 		var first=true;
    // 		for (var target in defs[ii]["targets"]){
    // 		    if (first) {modelDefs=modelDefs+"[";first=false;}
    // 		    modelDefs=modelDefs+target+"~"+defs[ii]["targets"][target]+"|";
    // 		}
    // 	    };
    // 	    href=href+"?modelDefault="+modelDefs;
    // 	}
    // };
    // if (obs) {
    // 	href=href+"?obsFile="+url_config[file]["obsConfigFile"]["file"];
    // 	if (url_config[file]["obsConfigFile"]["start"]) {
    // 	    href=href+"?obsStart="+ url_config[file]["obsConfigFile"]["start"];
    // 	};
    // 	if (url_config[file]["obsConfigFile"]["stop"]) {
    // 	    href=href+"?obsStop="+ url_config[file]["obsConfigFile"]["stop"];
    // 	};
    // 	var targets=url_config[file]["obsConfigFile"]["targets"];
    // 	var obsTargets="";
    // 	for (var target in targets) {
    // 	    obsTargets=obsTargets+target+"~"+
    // 		targets[target]["pos"]+"~"+
    // 		targets[target]["descr"]+"~"+
    // 		targets[target]["min"]+"~"+
    // 		targets[target]["max"]+"|";
    // 	}
    // 	href=href+"?obsTargets="+obsTargets;
    // 	if (mod) {
    // 	    var targets=url_config[file]["matchRules"]["targets"];
    // 	    var matchRules="";
    // 	    for (var target in targets) {
    // 		matchRules=matchRules+target+"~"+targets[target]["exp"]+"|";
    // 	    }
    // 	    href=href+"?matchRules="+matchRules;
    // 	}
    // };
    document.getElementById("urlLink").innerHTML=href;
    document.getElementById("urlLink").href=href;
    document.getElementById("urlLink").target="_blank";
}

function url_removeModelTarget(item,target) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	var item=document.getElementById("newlineModelTarget");
	item.children[1].children[0].value=target;
	item.children[2].children[0].value=url_config[file]["modelConfigFile"]["targets"][target]["variable"];
	item.children[4].children[0].value=url_config[file]["modelConfigFile"]["targets"][target]["min"];
	item.children[5].children[0].value=url_config[file]["modelConfigFile"]["targets"][target]["max"];
	delete url_config[file]["modelConfigFile"]["targets"][target];
	if (obs_isEmpty(url_config[file]["modelConfigFile"]["targets"])) {
	    delete url_config[file]["modelConfigFile"]["def"];
	    url_config[file]["modelConfigFile"]["def"]=[];
	}
	//url_showModelTargetTable();
	//url_showModelDefaultTable();
	url_show();
    }
};
function removeObsTarget(item,target) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	var item=document.getElementById("newlineObsTarget");
	item.children[1].children[0].value=target;
	item.children[2].children[0].value=url_config[file]["obsConfigFile"]["targets"][target]["pos"];
	item.children[4].children[0].value=url_config[file]["obsConfigFile"]["targets"][target]["descr"];
	item.children[5].children[0].value=url_config[file]["obsConfigFile"]["targets"][target]["info"];
	item.children[6].children[0].value=url_config[file]["obsConfigFile"]["targets"][target]["min"];
	item.children[7].children[0].value=url_config[file]["obsConfigFile"]["targets"][target]["max"];
	delete url_config[file]["obsConfigFile"]["targets"][target];
	if (obs_isEmpty(url_config[file]["obsConfigFile"]["targets"])) {
	    delete url_config[file]["obsConfigFile"]["def"];
	    url_config[file]["obsConfigFile"]["def"]=[];
	}
	//url_showObsTargetTable();
	url_show();
    }
};


function url_saveConfigFile(target) {
    var file=url_getConfigFile();
    var password=document.getElementById("urlConfigFilePsw").value;
    var host = url_config[file]["host"];
    var modelFile = url_config[file]["modelConfigFile"]["file"];
    var modelStart = url_config[file]["modelConfigFile"]["start"];
    var modelStop = url_config[file]["modelConfigFile"]["stop"];
    var modelTargets = "";
    var modelTrg=url_config[file]["modelConfigFile"]["targets"];
    for (var target in modelTrg) {
	modelTargets=modelTargets + "|" + target + "~" + 
	    modelTrg[target]["variable"] + "~" + 
	    modelTrg[target]["min"] + "~" + 
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
		target + "~" + 
		defTrg[target];
	};
    };
    var obsFile = url_config[file]["obsConfigFile"]["file"];
    var obsStart = url_config[file]["obsConfigFile"]["start"];
    var obsStop = url_config[file]["obsConfigFile"]["stop"];
    var obsTargets = "";
    var obsTrg=url_config[file]["obsConfigFile"]["targets"];
    for (var target in obsTrg) {
	obsTargets=obsTargets + "|" + target + "~" + 
	    obsTrg[target]["pos"] + "~" + 
	    obsTrg[target]["descr"] + "~" + 
	    obsTrg[target]["info"] + "~" + 
	    obsTrg[target]["min"] + "~" + 
	    obsTrg[target]["max"];
    };
    var matchRules = "";
    var matchTrg=url_config[file]["matchRules"]["targets"];
    for (var target in matchTrg) {
	matchRules=matchRules + "|" + target + "~" + 
	    matchTrg[target]["exp"];
    };
    documentLog.innerHTML="Sent url-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"url",
				  file:file,
				  host:host,
				  password:password,
				  modelFile:modelFile,
				  modelStart:modelStart,
				  modelStop:modelStop,
				  obsFile:obsFile,
				  obsStart:obsStart,
				  obsStop:obsStop,
				  obsTargets:obsTargets,
				  modelTargets:modelTargets,
				  modelDefault:modelDefault,
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
			  host:"localhost",
			  password:""};
	url_config[file]["modelConfigFile"]["file"]=document.getElementById("urlModelConfigFile").value;
	url_config[file]["obsConfigFile"]["file"]=document.getElementById("urlObsConfigFile").value;
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
			var expr=children[ii].children[3].children[0].value;
			url_config[file]["matchRules"]["targets"][name]=
			    {exp:expr};
		    }
		}
	    }
	};
	url_configEd++;
    } else { // load local values to screen
	showValue('urlModelConfigFile',url_config[file]["modelConfigFile"]["file"]);
	showValue('urlObsConfigFile',url_config[file]["obsConfigFile"]["file"]);
	url_show();
    }
};
function url_updateData() {
	documentLog.innerHTML="Sent url-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"url"},function(data, status){
	    dataToArray(data,status,documentLog);
	    if (! modelLoaded) {
		documentLog.innerHTML="Sent model-load request.";
		$.get("cgi-bin/fark_load.pl",{type:"model"},function(data, status){
		    dataToArray(data,status,documentLog);
		    modelLoaded=true;
		    if (! obsLoaded) {
			documentLog.innerHTML="Sent obs-load request.";
			$.get("cgi-bin/fark_load.pl",{type:"obs"},function(data, status){
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
		$.get("cgi-bin/fark_load.pl",{type:"obs"},function(data, status){
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
function url_getModelIndexStart(inp,target) {
    var file=url_getModelConfigFile();
    var item=document.getElementById(inp);
    item.value=Number(model_config[file]["start"]).toString();
    url_setConfigFilesTarget('modelConfigFile',target,'min',model_config[file]["start"]);
};
function url_getModelIndexStop(inp,target) {
    var file=url_getModelConfigFile();
    var item=document.getElementById(inp);
    item.value=Number(model_config[file]["stop"]).toString();
    url_setConfigFilesTarget('modelConfigFile',target,'max',model_config[file]["stop"]);
};
function url_getObsIndexStart(inp,target) {
    var file=url_getObsConfigFile();
    var item=document.getElementById(inp);
    console.log("fark.js start:",file,obs_config[file]["start"])
    item.value=obs_config[file]["start"];
    url_setArray('obsConfigFile','start',obs_config[file]["start"]);
};
function url_getObsIndexStop(inp,target) {
    var file=url_getObsConfigFile();
    var item=document.getElementById(inp);
    item.value=obs_config[file]["stop"];
    url_setArray('obsConfigFile','stop',obs_config[file]["stop"]);
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
function url_copyExp(f,t) {
    var fitem=document.getElementById(f);
    var titem=document.getElementById(t);
    titem.value=fitem.value;
};
function url_check(f,t) {
    var fitem=document.getElementById(f);
    var titem=document.getElementById(t);
    var exp=fitem.value||"";
    if (exp === "") {
	titem.setAttribute("style","display:none");
    } else {
	titem.setAttribute("style","");
    }
};
function url_debugExp(f,t) {
    var fitem=document.getElementById(f);
    var titem=document.getElementById(t);
    var expin=fitem.value;
    documentLog.innerHTML="Sent url-exp request:"+expin;
    $.get("cgi-bin/fark_exp.pl",{exp:expin},
	  function(data, status){
	      if (status === "success" && data !== null) {
		  var errors=data.getElementsByTagName("error");
		  if (errors.length > 0 ) {
		      console.log("Error:",data);
		      var msg=(errors[0].getAttribute("message")||"");
		      alert("Unable to evaluate expression:"+expin+"\n"+msg);
		  } else {
		      var results=data.getElementsByTagName("result");
		      if (results.length > 0 ) {
			  var val=(results[0].getAttribute("value")||"");
			  //titem.innerHTML=val;
			  titem.innerHTML=Number(val).toString();
		      };
		  };
		  documentLog.innerHTML="";
	      };
	  }
	 );
};
