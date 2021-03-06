coloc_file="default.cfg";
coloc_config = { "default.cfg" : { modelConfigFile : { file: "default.cfg",
						       targets : { "def_model" : { variable : "def",
										   min: "def_min",
										   max : "def_max",
									           exp : "" 
										 } },
						       def : [ {targets: {"def_model": 101}, 
								info:"default info"} ]
						     },
				   obsConfigFile : { file: "default.cfg",
						     start: "def_start",
						     stop : "def_stop",
						     targets : { "def_obs" : {pos:"", 
									      descr:"", 
									      info:"",  
									      min:"", 
									      max:""}
							       }
						   },
				   host:"fark.met.no",
				   filter:"",
				   password: ""
				 }
	       };
coloc_configEd = 0;
modelLoaded=false;
obsLoaded=false;
function coloc_modelIsNotLoaded(mfile) {
    return (model_config[mfile] === undefined);
};
function coloc_obsIsNotLoaded(ofile) {
    return (obs_config[ofile]===undefined);
};
function coloc_allocate(file) {
    if (coloc_config[file] === undefined) {
	coloc_config[file]=clone(coloc_config[coloc_file]);
	console.log("cloned:",file,coloc_file,coloc_config[file]);
    }
}
function coloc_setConfigFile(file) {
    setValue('colocConfigFile',file);
    setValue('colocConfigFileSave',file);
    if (file != "") {
	coloc_allocate(file);
	coloc_file=file;
	var mfile=coloc_getModelConfigFile();
	if (coloc_modelIsNotLoaded(mfile)) {coloc_updateModelData(mfile);}
	var ofile=coloc_getObsConfigFile();
	if (coloc_obsIsNotLoaded(ofile)) {coloc_updateObsData(ofile);}
	coloc_showCOLOC();
    };
};
function coloc_getConfigFile() {
    return coloc_file;
};
function coloc_getModelConfigFile( file = "") {
    if (file === "") {file=coloc_getConfigFile();}
    return coloc_config[file]["modelConfigFile"]["file"];
};
function coloc_getObsConfigFile(file = "") {
    if (file === "") {file=coloc_getConfigFile();}
    return coloc_config[file]["obsConfigFile"]["file"];
};
function coloc_setConfig(type,parameter,val) {
    var file=coloc_getConfigFile();
    coloc_config[file][type][parameter]=val;
    // load if we are changing obs or model config files
    if (parameter === "file" && type === "model") {
	documentLog.innerHTML="Sent "+type+"-load request.";
	$.get("cgi-bin/fark_load.pl",{type:type},function(data, status){
	    dataToArray(data,status,documentLog);
	    modelLoaded=true;
	    documentLog.innerHTML="";
	});
    } else if (parameter === "file" && type === "obs") {
	documentLog.innerHTML="Sent "+type+"-load request.";
	$.get("cgi-bin/fark_load.pl",{type:type},function(data, status){
	    dataToArray(data,status,documentLog);
	    obsLoaded=true;
	    documentLog.innerHTML="";
	});
    }
    coloc_showCOLOC();
}
function coloc_setConfigFilesTarget (type,target,parameter,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	if (coloc_config[file][type] === undefined || 
	    coloc_config[file][type]['targets'] ===undefined ||
	    coloc_config[file][type]['targets'][target] ===undefined ||
	    coloc_config[file][type]['targets'][target][parameter] ===undefined) {
	    console.log("Undefined:",type,target,parameter,val);
	};
	coloc_config[file][type]['targets'][target][parameter]=val;
	coloc_showModelDefaultTable();
	coloc_showCOLOC();
    }
}
function coloc_addConfigFilesTarget (type,target,parameter,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	if (coloc_config[file][type] === undefined || 
	    coloc_config[file][type]['targets'] ===undefined ||
	    coloc_config[file][type]['targets'][target] ===undefined ||
	    coloc_config[file][type]['targets'][target][parameter] ===undefined) {
	    console.log("Undefined:",type,target,parameter,val);
	};
	coloc_config[file][type]['targets'][target][parameter]=coloc_config[file][type]['targets'][target][parameter]+val;
	coloc_showModelDefaultTable();
	coloc_showCOLOC();
    }
}
function coloc_setArray (type,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	coloc_config[file][type]=val;
    }
}
function coloc_setArrayPar (type,parameter,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	coloc_config[file][type][parameter]=val;
    }
}
function coloc_setConfigFilesDefault (ii,target,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	coloc_config[file]["modelConfigFile"]["def"][ii]["targets"][target]=val;
	coloc_showCOLOC();
    }
}
function coloc_setConfigFilesDefaultInfo (ii,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	coloc_config[file]["modelConfigFile"]["def"][ii]["info"]=val;
	coloc_showCOLOC();
    }
}

function coloc_newModelTarget(item) {
    var name=item.parentNode.parentNode.children[1].children[0].value;
    var variable=item.parentNode.parentNode.children[2].children[0].value;
    var minimum=item.parentNode.parentNode.children[4].children[0].value;
    var maximum=item.parentNode.parentNode.children[5].children[0].value;
    if (name !== "" && variable !== "") {
	var file= coloc_getConfigFile();
	if (coloc_config[file] === undefined) {
	    coloc_config[file]={modelConfigFile:{targets:{},def:{}},
			      obsConfigFile:{targets:{}},
			      host:"fark.met.no",
			      password:""};
	};
	coloc_config[file]["modelConfigFile"]["targets"][name]={};
	coloc_config[file]["modelConfigFile"]["targets"][name]["variable"]=(variable || "");
	coloc_config[file]["modelConfigFile"]["targets"][name]["min"]=(minimum || "");
	coloc_config[file]["modelConfigFile"]["targets"][name]["max"]=(maximum || "");
	coloc_config[file]["modelConfigFile"]["targets"][name]["exp"]="";
	coloc_configEd++;
	//coloc_showModelTargetTable();
	//coloc_showModelDefaultTable();
	coloc_show();
	item.parentNode.parentNode.children[1].children[0].value="";	
	item.parentNode.parentNode.children[2].children[0].value="";	
	item.parentNode.parentNode.children[4].children[0].value="";	
	item.parentNode.parentNode.children[5].children[0].value="";	
    } else {
	alert("Invalid: name ('"+name+"'), variable ('"+variable+"')");
    }
};

function coloc_newModelDefault(item) {
    var file= coloc_getConfigFile();
    var line={targets:{},info:{}};
    var ok=false;
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    var pos=1;
    for (var target in targets) {
	var val=item.parentNode.parentNode.children[pos].children[0].value
	if (val !== undefined && val !== "") {
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
	//console.log("coloc_config:",coloc_config[file]["modelConfigFile"]["def"]);
	coloc_config[file]["modelConfigFile"]["def"].push(line);
	//coloc_showModelDefaultTable();
	coloc_show();
    } else {
	alert("Invalid model default value.");
    }
};

function coloc_newObsTarget(item) {
    var ofile=coloc_getObsConfigFile();
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
	    if (name !== "" && ((pos !== "" && bufrType !== "" && subType !== "") || (minimum != "" && maximum != ""))) {
		var file= coloc_getConfigFile();
		if (coloc_config[file] === undefined) {
		    coloc_config[file]={modelConfigFile:{targets:{},def:{}},
				      obsConfigFile:{targets:{}},
				      host:"fark.met.no",
				      password:""};
		};
		coloc_config[file]["obsConfigFile"]["targets"][name]={};
		coloc_config[file]["obsConfigFile"]["targets"][name]["pos"]=(pos || "");
		coloc_config[file]["obsConfigFile"]["targets"][name]["descr"]=(descr || "");
		coloc_config[file]["obsConfigFile"]["targets"][name]["info"]=(info || "");
		coloc_config[file]["obsConfigFile"]["targets"][name]["min"]=(minimum || "");
		coloc_config[file]["obsConfigFile"]["targets"][name]["max"]=(maximum || "");
		coloc_configEd++;
		//coloc_showObsTargetTable();
		coloc_show();
		item.parentNode.parentNode.children[1].children[0].value="";
		item.parentNode.parentNode.children[2].children[0].value="";
		item.parentNode.parentNode.children[4].children[0].value="";
		item.parentNode.parentNode.children[5].children[0].value="";
		item.parentNode.parentNode.children[6].children[0].value="";
		item.parentNode.parentNode.children[7].children[0].value="";
	    } else {
		alert("Invalid: name ('"+name+"'), position ('"+pos+"'), BUFR type ('"+bufrType+"') or subType ('"+subType+"') detected.");
	    }
	} else {
	    alert("'"+name+"' already used in obs config file: '"+ofile+"'");
	}
    } else {
	alert("Obs config file not loaded ('"+ofile+"')");
    }
};

// create model target table
function coloc_showModelTargetTable() {
    var item=document.getElementById('modelTargetTable');
    var file=coloc_getConfigFile();
    console.log("coloc: Showing ",file, coloc_config[file]["modelConfigFile"]["file"],coloc_config);
    var tail=removeTableChildFromTo(item,"labelsModelTarget","newlineModelTarget");
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    for (var target in targets) {
	coloc_insertModelTargetRow(tail,target,targets[target]["variable"],targets[target]["min"],targets[target]["max"]);
    }
};
// create auto table row
function coloc_insertModelTargetRow(item,target,variable,min,max) {
    var row = document.createElement("TR");
    var file = coloc_getModelConfigFile();
    console.log("coloc: Adding model target row for :",file,target,variable);
    if (model_config[file] === undefined) {
	var mark=false;
    } else {
	var mark=(variable === model_config[file]["index"]);
    }
    var td, inp;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","coloc_removeModelTarget(this.parentNode.parentNode,'"+target+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make target name column
    td=document.createElement("TD");
    if (mark) {
	td.setAttribute("style","color:blue");
    } else {
	td.setAttribute("style","");
    }
    td.innerHTML=target;
    row.appendChild(td);
    // make model variable column
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
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','variable',this.value);coloc_showModelTargetTable()");
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
	inp.setAttribute("id","colocModelIndexStart");
	inp.setAttribute("style","width:75px");
	inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','min',this.value);coloc_setArrayPar('modelConfigFile','start',this.value);coloc_showModelTargetTable();");
    } else {
	inp.setAttribute("style","width:100px");
	inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','min',this.value);");
    }
    td.appendChild(inp);
    if (mark) {
	var btn=document.createElement("BUTTON");
	btn.setAttribute("onclick","coloc_getModelIndexStart('colocModelIndexStart','"+target+"')");
	btn.setAttribute("style","width:25px");
	var t=document.createTextNode("←"); // "→"
	btn.appendChild(t);
	td.appendChild(btn);
    }
    row.appendChild(td);
    // make maximum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT"); // 
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    if (mark) {
	inp.setAttribute("id","colocModelIndexStop");
	inp.setAttribute("style","width:75px");
	inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);");
	inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);coloc_setArrayPar('modelConfigFile','stop',this.value);coloc_showModeltargetTable();");
    } else {
	inp.setAttribute("style","width:100px");
	inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);");
    }
    td.appendChild(inp);
    if (mark) {
	var btn=document.createElement("BUTTON");
	btn.setAttribute("onclick","coloc_getModelIndexStop('colocModelIndexStop','"+target+"')");
	btn.setAttribute("style","width:25px");
	var t=document.createTextNode("→"); // "←"
	btn.appendChild(t);
	td.appendChild(btn);
    }
    row.appendChild(td);
    // make add row to table
    if (item !== undefined) {
	item.parentNode.insertBefore(row,item);
    } else {
	console.log("coloc Undefined item.",target,variable,min,max);
    };
    return row;
}
// create model target table
function coloc_showModelDefaultTable() {
    var item=document.getElementById('modelDefaultTable');
    var header=clearTableChild(item,"labelsModelDefault");
    var newline=clearTableChild(item,"newlineModelDefault");
    var file=coloc_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsModelDefault","newlineModelDefault");
    coloc_insertModelDefaultHeader(header,file);
    coloc_insertModelDefaultNewline(newline,file);
    coloc_insertModelDefaultRow(tail,file);
};
// create model default table header
function coloc_insertModelDefaultHeader(row,file) {
    var td,bf;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
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

function coloc_insertModelDefaultRow(item,file) {
    var td;
    // make "-" column
    var defs=coloc_config[file]["modelConfigFile"]["def"];
    var len=defs.length;
    for (var ii=0;ii<len;ii++){
	if (coloc_config[file]["modelConfigFile"]["def"][ii]["targets"] !== undefined) {
	    var row = document.createElement("TR");
	    td=document.createElement("TD");
	    td.setAttribute("style","min-width:25px;width:25px");
	    var btn=document.createElement("BUTTON");
	    btn.setAttribute("onclick","coloc_removeModelDefault(this.parentNode.parentNode,'"+file+"',"+ii+")");
	    btn.setAttribute("style","width:100%");
	    var t=document.createTextNode("-");
	    btn.appendChild(t);
	    td.appendChild(btn);
	    row.appendChild(td);
	    var targets=coloc_config[file]["modelConfigFile"]["targets"];
	    for (var target in targets) {
		// make value column
		td=document.createElement("TD");
		td.setAttribute("class","fill");
		td.setAttribute("trg",target);
		inp=document.createElement("INPUT");
		inp.setAttribute("type","text");
		inp.setAttribute("value",(coloc_config[file]["modelConfigFile"]["def"][ii]["targets"][target]||""));
		inp.setAttribute("style","width:100%");
		inp.setAttribute("onblur","coloc_setConfigFilesDefault("+ii+",'"+target+"',this.value);");
		td.appendChild(inp);
		row.appendChild(td);
	    }
	    td=document.createElement("TD");
	    td.setAttribute("class","fill");
	    inp=document.createElement("INPUT");
	    inp.setAttribute("type","text");
	    inp.setAttribute("value",(coloc_config[file]["modelConfigFile"]["def"][ii]["info"]||""));
	    inp.setAttribute("style","width:100%");
	    inp.setAttribute("onblur","coloc_setConfigFilesDefaultInfo("+ii+",this.value);");
	    td.appendChild(inp);
	    row.appendChild(td);
	    // make add row to table
	    item.parentNode.insertBefore(row,item);
	};
    }
    return;
}

// create model default table newline
function coloc_insertModelDefaultNewline(row,file) {
    var td,btn,inp;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","coloc_newModelDefault(this)");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("+");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
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
function coloc_removeModelDefault(item,file,ii) {
    //console.log("removing model default:",file,ii);
    coloc_config[file]["modelConfigFile"]["def"].splice(ii,1);
    //coloc_showModelDefaultTable();
    coloc_show();
};
function coloc_showObsTargetTable() {
    var item=document.getElementById('obsTargetTable');
    var ofile=coloc_getObsConfigFile();
    var file=coloc_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsObsTarget","newlineObsTarget");
    // insert obs targets from obs-config file
    if (obs_config[ofile] !== undefined) {
	var otargets=obs_config[ofile]["targets"];
	for (var target in otargets) {
	    coloc_insertOTargetRow(tail,target,otargets[target]["pos"],otargets[target]["descr"],
				   otargets[target]["info"],otargets[target]["min"],otargets[target]["max"]);
	}
    };
    // insert obs target index expression from obs-config file
    // make "-" column  ***************************
    var row = document.createElement("TR");
    var td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make NAME column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","color:green");
    if (obs_config[ofile] !== undefined) {
	var target=obs_config[ofile]["indexTarget"];
    } else {
	var target="";
    }
    td.innerHTML=target;
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("colspan","4");
    if (obs_config[ofile] !== undefined) {
	td.innerHTML=obs_config[ofile]["indexExp"];
    } else {
	td.innerHTML="";
    };
    row.appendChild(td);
    // make minimum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",coloc_config[file]["obsConfigFile"]["start"]);
    inp.setAttribute("id","colocObsIndexStart");
    inp.setAttribute("style","width:75px");
    inp.setAttribute("onblur","coloc_setArrayPar('obsConfigFile','start',this.value);");
    td.appendChild(inp);
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","coloc_getObsIndexStart('colocObsIndexStart','"+target+"')");
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
    inp.setAttribute("value",coloc_config[file]["obsConfigFile"]["stop"]);
    inp.setAttribute("id","colocObsIndexStop");
    inp.setAttribute("style","width:75px");
    inp.setAttribute("onblur","coloc_setArrayPar('obsConfigFile','stop',this.value);");
    td.appendChild(inp);
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","coloc_getObsIndexStop('colocObsIndexStop','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("→"); // "←"
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    tail.parentNode.insertBefore(row,tail);
    // insert obs targets from coloc-config file
    var targets=coloc_config[file]["obsConfigFile"]["targets"];
    for (var target in targets) {
	coloc_insertObsTargetRow(tail,target,targets[target]["pos"],targets[target]["descr"],
			   targets[target]["info"],targets[target]["min"],targets[target]["max"]);
    }
};
// create auto table row
function coloc_insertOTargetRow(item,target,pos,descr,info,min,max) {
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
function coloc_insertObsTargetRow(item,target,pos,descr,info,min,max) {
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
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('obsConfigFile','"+target+"','pos',this.value);");
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
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('obsConfigFile','"+target+"','descr',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",info);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('obsConfigFile','"+target+"','info',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make minimum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('obsConfigFile','"+target+"','min',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make maximum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('obsConfigFile','"+target+"','max',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
function coloc_showTargetMatchTable() {
    var item=document.getElementById('targetMatchTable');
    var file=coloc_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsTargetMatch","newlineTargetMatch");
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    var cnt=0;
    for (var target in targets) {
	cnt=cnt+1;
	coloc_insertTargetMatchRow(tail,cnt,target,targets[target]["exp"]);
    };

};
// create auto table row
function coloc_insertTargetMatchRow(item,cnt,target,expr) {
    var row = document.createElement("TR");
    var file = coloc_getConfigFile();
    if (coloc_config[file]!== undefined){
        var mfile = coloc_getModelConfigFile();
        var ofile = coloc_getObsConfigFile();
        var td, inp,div,itemId;
        // make model target column  ***************************
        td=document.createElement("TD");
        if (model_config[mfile] !== undefined) {
            if (coloc_config[file]["modelConfigFile"]["targets"][target] !== undefined) {
                var variable=coloc_config[file]["modelConfigFile"]["targets"][target]["variable"];
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
        // make obs target expression column  ***************************
	itemId="matchExpression"+cnt;
        td=document.createElement("TD");
        td.setAttribute("class","fill");
	expr=(coloc_config[file]["modelConfigFile"]["targets"][target]["exp"]||"");
        inp=document.createElement("INPUT");
	inp.setAttribute("id",itemId);
        inp.setAttribute("type","text");
        inp.setAttribute("value",expr);
        inp.setAttribute("style","width:100%");
        inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','exp',this.value);coloc_showTargetMatchTable();");
        if (obs_config[ofile] !== undefined) {
            var variable=obs_config[ofile]["indexTarget"];
            var mark=(expr.indexOf(variable) > -1);
            if (mark) {
                inp.setAttribute("style","color:green");
            };
        };
	div=document.createElement("DIV");
	div.setAttribute("id",itemId+"Dropdown");
	div.setAttribute("class","dropdown-content");
        td.appendChild(inp);
        td.appendChild(div);
        row.appendChild(td);
	// make select column ***************************************
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px");
	var btn=document.createElement("BUTTON");
	btn.setAttribute("onclick","showDropdown('"+itemId+"',this.parentNode.parentNode.children[1].children[0].value);");
	btn.setAttribute("class","dropbtn");
	btn.setAttribute("style","width:100%");
	var t=document.createTextNode("\u2630");
	btn.appendChild(t);
	td.appendChild(btn);
	row.appendChild(td);
        // make add row to table  ***************************
        item.parentNode.insertBefore(row,item);
        return row;
    }
}
// COLOC methods
function coloc_show() {
    var item;
    var file=coloc_getConfigFile();
    var mod=(coloc_config[file]["modelConfigFile"]["file"] !== "");
    var obs=(coloc_config[file]["obsConfigFile"]["file"] !== "");
    if (mod) {
	(document.getElementById("displayModelTargets")).setAttribute("style","");
	coloc_showModelTargetTable();
	coloc_showModelDefaultTable();
    } else {
	(document.getElementById("displayModelTargets")).setAttribute("style","display:none");
	(document.getElementById("displayModelDefault")).setAttribute("style","display:none");
    };
    if (obs) {
	coloc_showObsTargetTable();
	(document.getElementById("displayObsTargets")).setAttribute("style","");
    } else {
	(document.getElementById("displayObsTargets")).setAttribute("style","display:none");
    }
    if (mod && obs) {
	(document.getElementById("displayMatchRules")).setAttribute("style","");
	(document.getElementById("displayModelDefault")).setAttribute("style","display:none");
	coloc_showTargetMatchTable();
    }else {
	(document.getElementById("displayMatchRules")).setAttribute("style","display:none");
	if (mod) {
	    (document.getElementById("displayModelDefault")).setAttribute("style","");
	} else {
	    (document.getElementById("displayModelDefault")).setAttribute("style","display:none");
	}
    }
    setValue('colocConfigFile',file);
    setValue('colocConfigFileSave',file);
    setValue('colocModelConfigFile',coloc_config[file]["modelConfigFile"]["file"]);
    setValue('colocObsConfigFile',coloc_config[file]["obsConfigFile"]["file"]);
    setValue('colocFilter',coloc_config[file]["filter"]);
    coloc_showCOLOC();
}

function coloc_showCOLOC() {
    var file=coloc_getConfigFile();
    var host=coloc_config[file]["host"];
    var href="http://"+host+"/cgi-bin/fark_coloc.pl?colocFile="+file;
    // if (mod) {
    // 	href=href+"?modelFile="+coloc_config[file]["modelConfigFile"]["file"];
    // 	if (coloc_config[file]["modelConfigFile"]["start"]) {
    // 	    href=href+"?modelStart="+ coloc_config[file]["modelConfigFile"]["start"];
    // 	};
    // 	if (coloc_config[file]["modelConfigFile"]["stop"]) {
    // 	    href=href+"?modelStop="+ coloc_config[file]["modelConfigFile"]["stop"];
    // 	};
    // 	var targets=coloc_config[file]["modelConfigFile"]["targets"];
    // 	var modelTargets="";
    // 	for (var target in targets) {
    // 	    modelTargets=modelTargets+target+"~"+targets[target]["variable"]+"~"+targets[target]["min"]+"~"+targets[target]["max"]+"|";
    // 	}
    // 	href=href+"?modelTargets="+modelTargets;
    // 	if (! obs) {
    // 	    var modelDefs="";
    // 	    var defs=coloc_config[file]["modelConfigFile"]["def"];
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
    // 	href=href+"?obsFile="+coloc_config[file]["obsConfigFile"]["file"];
    // 	if (coloc_config[file]["obsConfigFile"]["start"]) {
    // 	    href=href+"?obsStart="+ coloc_config[file]["obsConfigFile"]["start"];
    // 	};
    // 	if (coloc_config[file]["obsConfigFile"]["stop"]) {
    // 	    href=href+"?obsStop="+ coloc_config[file]["obsConfigFile"]["stop"];
    // 	};
    // 	var targets=coloc_config[file]["obsConfigFile"]["targets"];
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
    // 	    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    // 	    var matchRules="";
    // 	    for (var target in targets) {
    // 		matchRules=matchRules+target+"~"+targets[target]["exp"]+"|";
    // 	    }
    // 	    href=href+"?matchRules="+matchRules;
    // 	}
    // };
    //document.getElementById("colocLink").innerHTML=href;
    //document.getElementById("colocLink").href=href;
    //document.getElementById("colocLink").target="_blank";
}

function coloc_removeModelTarget(item,target) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	var item=document.getElementById("newlineModelTarget");
	item.children[1].children[0].value=target;
	item.children[2].children[0].value=coloc_config[file]["modelConfigFile"]["targets"][target]["variable"];
	item.children[4].children[0].value=coloc_config[file]["modelConfigFile"]["targets"][target]["min"];
	item.children[5].children[0].value=coloc_config[file]["modelConfigFile"]["targets"][target]["max"];
	delete coloc_config[file]["modelConfigFile"]["targets"][target];
	if (obs_isEmpty(coloc_config[file]["modelConfigFile"]["targets"])) {
	    delete coloc_config[file]["modelConfigFile"]["def"];
	    coloc_config[file]["modelConfigFile"]["def"]=[];
	}
	//coloc_showModelTargetTable();
	//coloc_showModelDefaultTable();
	coloc_show();
    }
};
function removeObsTarget(item,target) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	var item=document.getElementById("newlineObsTarget");
	item.children[1].children[0].value=target;
	item.children[2].children[0].value=coloc_config[file]["obsConfigFile"]["targets"][target]["pos"];
	item.children[4].children[0].value=coloc_config[file]["obsConfigFile"]["targets"][target]["descr"];
	item.children[5].children[0].value=coloc_config[file]["obsConfigFile"]["targets"][target]["info"];
	item.children[6].children[0].value=coloc_config[file]["obsConfigFile"]["targets"][target]["min"];
	item.children[7].children[0].value=coloc_config[file]["obsConfigFile"]["targets"][target]["max"];
	delete coloc_config[file]["obsConfigFile"]["targets"][target];
	if (obs_isEmpty(coloc_config[file]["obsConfigFile"]["targets"])) {
	    delete coloc_config[file]["obsConfigFile"]["def"];
	    coloc_config[file]["obsConfigFile"]["def"]=[];
	}
	//coloc_showObsTargetTable();
	coloc_show();
    }
};


function coloc_saveConfigFile(target) {
    var file=coloc_getConfigFile();
    var password=document.getElementById("colocConfigFilePsw").value;
    var host = coloc_config[file]["host"];
    var filter = coloc_config[file]["filter"];
    var modelFile = coloc_config[file]["modelConfigFile"]["file"];
    var modelStart = coloc_config[file]["modelConfigFile"]["start"];
    var modelStop = coloc_config[file]["modelConfigFile"]["stop"];
    var modelTargets = "";
    var modelTrg=coloc_config[file]["modelConfigFile"]["targets"];
    for (var target in modelTrg) {
	modelTargets=modelTargets + "|" + target + "~" + 
	    modelTrg[target]["variable"] + "~" + 
	    modelTrg[target]["min"] + "~" + 
	    modelTrg[target]["max"];
    };
    var modelDefault = "";
    var modelDef=coloc_config[file]["modelConfigFile"]["def"];
    var len=modelDef.length;
    for (var ii=0; ii<len;ii++) {
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
    var obsFile = coloc_config[file]["obsConfigFile"]["file"];
    var obsStart = coloc_config[file]["obsConfigFile"]["start"];
    var obsStop = coloc_config[file]["obsConfigFile"]["stop"];
    var obsTargets = "";
    var obsTrg=coloc_config[file]["obsConfigFile"]["targets"];
    for (var target in obsTrg) {
	obsTargets=obsTargets + "|" + target + "~" + 
	    obsTrg[target]["pos"] + "~" + 
	    obsTrg[target]["descr"] + "~" + 
	    obsTrg[target]["info"] + "~" + 
	    obsTrg[target]["min"] + "~" + 
	    obsTrg[target]["max"];
    };
    var matchRules = "";
    var matchTrg=coloc_config[file]["modelConfigFile"]["targets"];
    for (var target in matchTrg) {
	matchRules=matchRules + "|" + target + "~" + 
	    matchTrg[target]["exp"];
    };
    documentLog.innerHTML="Sent coloc-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"coloc",
				  file:file,
				  host:host,
				  filter:filter,
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
    makeUrl("coloc",file);
};
function coloc_showConfig() {
    var file=coloc_getConfigFile();
    if (coloc_config[file] === undefined) { // create new entry locally...
	coloc_config[file]={modelConfigFile:{targets:{},def:{}},
			  obsConfigFile:{targets:{}},
			  host:"fark.met.no",
			  password:""};
	coloc_config[file]["modelConfigFile"]["file"]=document.getElementById("colocModelConfigFile").value;
	coloc_config[file]["obsConfigFile"]["file"]=document.getElementById("colocObsConfigFile").value;
	// model targets
	var item=document.getElementById("modelTargetTable");
	var tbody=item.children[0];
	var children=tbody.children;
	var len=children.length;
	coloc_config[file]["modelConfigFile"]["targets"]={};
	for (var ii=len-1;ii>=0;ii--){
	    //console.log("RemoveTableChildFromTo ",ii,children[ii],len,children.length);
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsModelTarget" && att !== "newlineModelTarget") {
			var name=children[ii].children[1].innerHTML;
			var variable=children[ii].children[2].children[0].value;
			var min=children[ii].children[4].children[0].value;
			var max=children[ii].children[5].children[0].value;
			coloc_config[file]["modelConfigFile"]["targets"][name]=
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
	coloc_config[file]["modelConfigFile"]["def"]=[];
	for (var ii=len-1;ii>=0;ii--){
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsModelDefault" && att !== "newlineModelDefault") {
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
			coloc_config[file]["modelConfigFile"]["def"].push({targets:targets,info:info});
		    }
		}
	    }
	};
	// obs targets
	item=document.getElementById("obsTargetTable");
	tbody=item.children[0];
	children=tbody.children;
	len=children.length;
	coloc_config[file]["obsConfigFile"]["targets"]={};
	for (var ii=len-1;ii>=0;ii--){
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsObsTarget" && att !== "newlineObsTarget") {
			var name=children[ii].children[1].innerHTML;
			var bufrType=children[ii].children[2].children[0].value;
			var subType=children[ii].children[4].children[0].value;
			var pos=children[ii].children[6].children[0].value;
			var descr=children[ii].children[8].children[0].value;
			var min=children[ii].children[9].children[0].value;
			var max=children[ii].children[10].children[0].value;
			coloc_config[file]["obsConfigFile"]["targets"][name]=
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
	for (var ii=len-1;ii>=0;ii--){
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsTargetMatch" && att !== "newlineTargetMatch") {
			var name=children[ii].children[1].innerHTML;
			var expr=children[ii].children[3].children[0].value;
			coloc_config[file]["modelConfigFile"]["targets"][name]=
			    {exp:expr};
		    }
		}
	    }
	};
	coloc_configEd++;
    } else { // load local values to screen
	setValue('colocModelConfigFile',coloc_config[file]["modelConfigFile"]["file"]);
	setValue('colocObsConfigFile',coloc_config[file]["obsConfigFile"]["file"]);
	coloc_show();
    }
};
function coloc_updateModelData(arg = "") {
	var args=getArgs(arg);
	documentLog.innerHTML="Sent model-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"model",arg:args},function(data, status){
	    dataToArray(data,status,documentLog);
	    modelLoaded=true;
	    //console.log("Updating dropdown for ",target);
	    coloc_show();
	    documentLog.innerHTML="";
	});
};
function coloc_updateObsData(arg = "") {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent obs-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"obs",arg:args},function(data, status){
	dataToArray(data,status,documentLog);
	obsLoaded=true;
	//console.log("Updating dropdown for ",target);
	coloc_show();
	documentLog.innerHTML="";
    });
};
function coloc_updateData() {
	var args=getArgs(coloc_getConfigFile());
	documentLog.innerHTML="Sent coloc-load request.";
        console.log("coloc: *****loading  ",args);
	$.get("cgi-bin/fark_load.pl",{type:"coloc",arg:args},function(data, status){
	    dataToArray(data,status,documentLog);
	    documentLog.innerHTML="Sent model-load request.";
	    args=getArgs(coloc_getModelConfigFile());
	    console.log("coloc: *****loading model ",args);
	    $.get("cgi-bin/fark_load.pl",{type:"model",arg:args},function(data, status){
		dataToArray(data,status,documentLog);
		modelLoaded=true;
		args=getArgs(coloc_getObsConfigFile());
		console.log("coloc: *****loading obs ",args);
		documentLog.innerHTML="Sent obs-load request.";
		$.get("cgi-bin/fark_load.pl",{type:"obs",arg:args},function(data, status){
		    dataToArray(data,status,documentLog);
		    obsLoaded=true;
		    coloc_show();
		    documentLog.innerHTML="";
		});
	    });
	});
};
function coloc_getModelIndexStart(inp,target) {
    var file=coloc_getModelConfigFile();
    var item=document.getElementById(inp);
    item.value=Number(model_config[file]["start"]).toString();
    coloc_setConfigFilesTarget('modelConfigFile',target,'min',model_config[file]["start"]);
};
function coloc_getModelIndexStop(inp,target) {
    var file=coloc_getModelConfigFile();
    var item=document.getElementById(inp);
    item.value=Number(model_config[file]["stop"]).toString();
    coloc_setConfigFilesTarget('modelConfigFile',target,'max',model_config[file]["stop"]);
};
function coloc_getObsIndexStart(inp,target) {
    var file=coloc_getObsConfigFile();
    var item=document.getElementById(inp);
    //console.log("fark.js start:",file,obs_config[file]["start"])
    item.value=obs_config[file]["start"];
    coloc_setArrayPar('obsConfigFile','start',obs_config[file]["start"]);
};
function coloc_getObsIndexStop(inp,target) {
    var file=coloc_getObsConfigFile();
    var item=document.getElementById(inp);
    item.value=obs_config[file]["stop"];
    coloc_setArrayPar('obsConfigFile','stop',obs_config[file]["stop"]);
};
function coloc_getObsTargetBufrType() {
    var item=document.getElementById("obsTargetTable");
    var newline=getChild(item,"newlineObsTarget");
    return newline.children[2].children[0].value;
};
function coloc_getObsTargetSubType() {
    var item=document.getElementById("obsTargetTable");
    var newline=getChild(item,"newlineObsTarget");
    return newline.children[4].children[0].value;
};
function coloc_debugExp(f,t) {
    var fitem=document.getElementById(f);
    var titem=document.getElementById(t);
    var expin=fitem.value;
    documentLog.innerHTML="Sent coloc-exp request:"+expin;
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

function coloc_mkdir(path) {
    var password=document.getElementById("colocConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"coloc",
				 path:path,
				 password,password},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to mkdir: "+path+"\n"+msg);
	      };
	      documentLog.innerHTML="";}
				}
	 );
    
};

function coloc_rmdir(path) {
    var password=document.getElementById("colocConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"coloc",
				 path:path,
				 password,password},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to rmdir: "+path+"\n"+msg);
	      };
	      documentLog.innerHTML="";}
				}
	 );
    
};

function coloc_rmfile(path) {
    var password=document.getElementById("colocConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"coloc",
				 path:path,
				 password,password},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to rmfile: "+path+"\n"+msg);
	      };
	      documentLog.innerHTML="";}
				}
	 );
    
};

function coloc_mkfile(file) {
    console.log("Calling saveConfigFile: '"+file+"'");
    coloc_setConfigFile(file);
    coloc_saveConfigFile(file);
};

