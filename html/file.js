#__file: 'coloc.js' 0100664    **DO NOT DELETE**
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
						   targets : { "def_obs" : {pos:"", descr:"", info:"",  min:"", max:""}
							     }
						 },
				 host:"fark.met.no",
				 password: ""
			       }
	     };
coloc_configEd = 0;
function coloc_getConfigFile() {
    return coloc_file;
};
function coloc_setConfigFile(value) {
    if (coloc_config[value] === undefined) {
	coloc_config[value]=clone(coloc_config[coloc_file]);
    }
    coloc_file=value;
    coloc_showCOLOC();
}
function coloc_getModelConfigFile() {
    var file=coloc_getConfigFile();
    return coloc_config[file]["modelConfigFile"]["file"];
};
function coloc_getObsConfigFile() {
    var file=coloc_getConfigFile();
    return coloc_config[file]["obsConfigFile"]["file"];
};
function coloc_setConfig(type,parameter,val) {
    var file=coloc_getConfigFile();
    coloc_config[file][type][parameter]=val;
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
function coloc_setArray (type,parameter,val) {
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
	    if (name !== "" && pos !== "" && bufrType !== "" && subType !== "") {
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
    var mark=(variable === model_config[file]["index"]);
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
	inp.setAttribute("style","width:125px");
	inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','min',this.value);coloc_setArray('modelConfigFile','start',this.value);coloc_showModelTargetTable();");
    } else {
	inp.setAttribute("style","width:100%");
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
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    if (mark) {
	inp.setAttribute("id","colocModelIndexStop");
	inp.setAttribute("style","width:125px");
	inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);");
	inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);coloc_setArray('modelConfigFile','stop',this.value);coloc_showModeltargetTable();");
    } else {
	inp.setAttribute("style","width:100%");
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
    item.parentNode.insertBefore(row,item);
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
    var otargets=obs_config[ofile]["targets"];
    for (var target in otargets) {
	coloc_insertOTargetRow(tail,target,otargets[target]["pos"],otargets[target]["descr"],
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
    inp.setAttribute("value",coloc_config[file]["obsConfigFile"]["start"]);
    inp.setAttribute("id","colocObsIndexStart");
    inp.setAttribute("style","width:125px");
    inp.setAttribute("onblur","coloc_setArray('obsConfigFile','start',this.value);");
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
    inp.setAttribute("style","width:125px");
    inp.setAttribute("onblur","coloc_setArray('obsConfigFile','stop',this.value);");
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
	btn.setAttribute("onclick","showDropdown('"+itemId+"');");
	btn.setAttribute("class","dropbtn");
	btn.setAttribute("style","width:100%");
	var t=document.createTextNode("⚲");
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
    setValue('colocModelConfigFile',coloc_config[file]["modelConfigFile"]["file"]);
    setValue('colocObsConfigFile',coloc_config[file]["obsConfigFile"]["file"]);
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
    document.getElementById("colocLink").innerHTML=href;
    document.getElementById("colocLink").href=href;
    document.getElementById("colocLink").target="_blank";
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
function coloc_updateData() {
	documentLog.innerHTML="Sent coloc-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"coloc"},function(data, status){
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
			    coloc_show();
			    documentLog.innerHTML="";
			});
		    } else {
			coloc_show();
			documentLog.innerHTML="";
		    }
		});
	    } else if (!obsLoaded) {
		documentLog.innerHTML="Sent obs-load request.";
		$.get("cgi-bin/fark_load.pl",{type:"obs"},function(data, status){
		    dataToArray(data,status,documentLog);
		    obsLoaded=true;
		    coloc_show();
		    documentLog.innerHTML="";
		});
	    } else {
		coloc_show();
		documentLog.innerHTML="";
	    }
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
    coloc_setArray('obsConfigFile','start',obs_config[file]["start"]);
};
function coloc_getObsIndexStop(inp,target) {
    var file=coloc_getObsConfigFile();
    var item=document.getElementById(inp);
    item.value=obs_config[file]["stop"];
    coloc_setArray('obsConfigFile','stop',obs_config[file]["stop"]);
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
#__file: 'metfark.js' 0100664    **DO NOT DELETE**
documentLog = document.getElementById("log");
dropdownEd = {};	
modelLoaded=false;
obsLoaded=false;

// initialisation function
function load(){
    var types=["model","obs","coloc","plot","auto"];
    var url=getUrlVars();
    var type=decodeURIComponent(url["type"] || "model");
    var file=decodeURIComponent(url["file"] || "default.cfg");
    if (! types.includes(type)) { 
	type="model"; 
	file="default.cfg";
    };
    load_setActive(type);
    load_setConfigFile(type,file);
    load_updateData(type);
};

function makeUrl(type,file) {
    var url="?type="+type+"&file="+file;
    window.history.replaceState("", "js", url);
}

function getUrlVars() {
    var vars = {};
    var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi,    
    function(m,key,value) {
      //console.log("URL item:",key," ",value)
      vars[key] = value;
    });
    return vars;
};


function load_setActive(type) {
    var types=["model","obs","coloc","plot","auto"];
    var len=types.length;
    for (var ii=0;ii<len;ii++){
	var item_tab=document.getElementById(types[ii]+"_tab");
	var item=document.getElementById(types[ii]);
	if (types[ii] === type) {
	    item_tab.setAttribute("class","active");
	    item.setAttribute("class","tab-pane fade in active");
	} else if (item_tab != null) {
	    item_tab.setAttribute("class","");
	    item.setAttribute("class","tab-pane fade in");
	} else {
	    console.log("Invalid item:",ii,types[ii]+"_tab");
	};
    };
}

function load_setConfigFile(type,file) {
    if (type === "model") {
	model_setConfigFile(file);
    } else if (type === "obs") {
	obs_setConfigFile(file);
    } else if (type === "coloc") {
	coloc_setConfigFile(file);
    } else if (type === "plot") {
	//auto_setConfigFile(file);
    } else if (type === "auto") {
	//auto_setConfigFile(file);
    };
};
function load_updateData(type){
    if (type === "model") {
	model_updateData();
    } else if (type === "obs") {
	obs_updateData();
    } else if (type === "coloc") {
	coloc_updateData();
    } else if (type === "plot") {
	plot_updateData();
    } else if (type === "auto") {
	auto_updateData();
    };
};


// dropdown methods
window.onclick = function(event) {
    if (!event.target.matches('.dropbtn')) {
	var dropdowns = document.getElementsByClassName("dropdown-content");
	var i;
	for (i = 0; i < dropdowns.length; i++) {
	    var openDropdown = dropdowns[i];
	    if (openDropdown.classList.contains('show')) {
		openDropdown.classList.remove('show');
	    }
	}
    }
}

function removeChildren(item) {
    var children=item.childNodes;
    var len=children.length;
    for (var ii=len-1;ii>=0;ii--){
	if (children[ii] !== undefined) {
	    //console.log("Removing :",ii,len,children[ii]);
	    item.removeChild(children[ii]);
	}
    }
}
function clearTableChild(item,id) {
    //console.log("removeTableChildFromTo Entering",item);
    var tbody=item.children[0];
    var children=tbody.children;
    var len=children.length;
    var active=false;
    var tail;
    var active=false;
    for (var ii=len-1;ii>=0;ii--){
	//console.log("RemoveTableChild ",ii,children[ii],len,children.length);
	if (children[ii] !== undefined) {
	    if (children[ii].getAttribute !== undefined) {
		var att=children[ii].getAttribute("id");
		if (att === id) {
		    var grandChildren=children[ii].children;
		    var leng=grandChildren.length;
		    for (var jj=leng-1;jj>=0;jj--){
			children[ii].removeChild(grandChildren[jj]);
		    };
		    return children[ii];
		} 
	    }
	}
    };
    return;
}
function removeTableChildFromTo(item,from,to) {
    //console.log("removeTableChildFromTo Entering",item);
    var tbody=item.children[0];
    var children=tbody.children;
    var len=children.length;
    var active=false;
    var tail;
    var active=false;
    for (var ii=len-1;ii>=0;ii--){
	//console.log("RemoveTableChildFromTo ",ii,children[ii],len,children.length);
	if (children[ii] !== undefined) {
	    if (children[ii].getAttribute !== undefined) {
		var att=children[ii].getAttribute("id");
		//console.log("Child with Attribute:",ii,att);
		if (! active) {
		    if (att !== undefined & att === to) {
			tail=children[ii];
			//console.log("Tail child:",ii,att);
			active=true;
		    }
		} else {
		    if (att !== undefined & att === from) {
			//console.log("Head child:",ii,att);
			active=false;
			ii=-1;
		    } else {
			//console.log("Removing child:",ii,att);
			tbody.removeChild(children[ii]);
		    }
		}
	    } else {
		//console.log("Child:",ii,children[ii]);
		tbody.removeChild(children[ii]);
	    }
	    //children[ii].parentNode.removeChild(children[ii]);
	}
    };
    //console.log("removeTableChildFromTo Done",tail);
    return tail;
}
function getChild(item,target) {
    var tbody=item.children[0];
    var children=tbody.children;
    var len=children.length;
    for (var ii=len-1;ii>=0;ii--){
	if (children[ii] !== undefined) {
	    if (children[ii].getAttribute !== undefined) {
		var att=children[ii].getAttribute("id");
		if (att === target) {
		    return children[ii];
		};
	    }
	}
    };
    //console.log("removeTableChildFromTo Done",tail);
    return;
}
function addChildButton(item,text,onclick) {
    var btn=document.createElement("BUTTON");
    var t=document.createTextNode(text);
    var br=document.createElement("BR");
    btn.appendChild(t);
    btn.setAttribute("onclick",onclick);
    btn.setAttribute("style","width:100%");
    item.appendChild(btn);
    item.appendChild(br);
}

function showDropdown(target, arg = "") {
    var dropdown=target + 'Dropdown';
    var item=document.getElementById(dropdown);
    console.log("Dropdown argument:",arg);
    if (target === 'modelConfigFile') {
	documentLog.innerHTML="Sent model-load request.";
	var dir = arg.match(/(.*)[\/\\]/)[1]||'';
	var file =arg.match(/([^\/\\]*)$/)[1]||'';
	console.log("Dropdown dir:",dir," file:",file);
	$.get("cgi-bin/fark_load.pl",{type:"model"},function(data, status){
	    dataToArray(data,status,documentLog);
	    //console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    for (var model in model_config) {
		addChildButton(item,model,"model_setConfigFile('"+model+"');model_show();");
	    }
	    documentLog.innerHTML="";
	});
    } else if (target === 'modelIndex') {
	var file=model_getConfigFile();
	removeChildren(item);
 	if (model_config[file] !== undefined) {
	    var variables=model_config[file]["variables"];
	    if (variables !== undefined) {
		var len=variables.length;
		for (var ii=0;ii<len;ii++) {
		    var fullname=variables[ii][0];
		    if (variables[ii][1]) {fullname=fullname+"("+variables[ii][1]+")";};
		    addChildButton(item,fullname,"model_setArray('index','"+variables[ii][0]+"');model_show();");
		}
	    }
	}
    } else if (target === 'obsConfigFile') {
	documentLog.innerHTML="Sent obs-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"obs"},function(data, status){
	    dataToArray(data,status,documentLog);
	    //console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    for (var obs in obs_config) {
		addChildButton(item,obs,"obs_setConfigFile('"+obs+"');obs_show();");
	    };
	    documentLog.innerHTML="";
	});
    } else if (target === 'obsBufrType') { // always re-calculate
	var file=obs_getConfigFile();
	removeChildren(item);
	if (obs_config[file] !== undefined) {
	    var bufr=obs_config[file]["bufr"];
	    if (bufr !== undefined) {
		for (var bufrType in bufr) {
		    var info=obs_config[file]["bufr"][bufrType]["info"] || "";
		    var cnt=obs_config[file]["bufr"][bufrType]["cnt"] || "";
		    var ccnt="";
		    if (cnt !== "") {
			ccnt=" ("+cnt+")";
		    }
		    addChildButton(item,bufrType+" "+info+ccnt,"obs_setArray('bufrType','"+bufrType+"');setValue('obsBufrType','"+bufrType+"');");
		}
	    }
	}
    } else if (target === 'obsSubType') { // always re-calculate
	var file=obs_getConfigFile();
	var bufrType=obs_config[file]["bufrType"];
	removeChildren(item);
	if (obs_config[file] !== undefined) {
	    var bufr=obs_config[file]["bufr"];
	    if (bufr !== undefined) {
		for (var subType in bufr[bufrType]) {
		    if (subType !== "info" && subType !== "cnt")  {
			var info=bufr[bufrType][subType]["info"];
			var cnt=bufr[bufrType][subType]["cnt"] || "";
			var ccnt="";
			if (cnt !== "") {
			    ccnt=" ("+cnt+")";
			}
			addChildButton(item,subType+" : "+info+ccnt,"obs_setArray('subType','"+subType+"');obs_setArray('typeInfo','"+info+"');setValue('obsSubType','"+subType+"');setValue('obsTypeInfo','"+info+"');");
		    }
		}
	    }
	}
    } else if (target === 'obsIndexPOS') {
	var file=obs_getConfigFile();
	var bufrType=obs_config[file]["bufrType"];
	var subType=obs_config[file]["subType"];
	removeChildren(item);
	if (bufrType !== undefined && bufrType !== "" &&
	    subType !== undefined && subType !== "" && subType !== "info" &&subType !== "cnt" &&
	    obs_config[file] !== undefined && 
	    obs_config[file]["bufr"] !== undefined && 
	    obs_config[file]["bufr"][bufrType] !== undefined && 
	    obs_config[file]["bufr"][bufrType][subType] !== undefined && 
	    obs_config[file]["bufr"][bufrType][subType]["seq"] !== undefined ) {
	    var bufr=obs_config[file]["bufr"][bufrType][subType]["seq"];
	    var len=bufr.length;
	    for (var ii=0;ii<len;ii++) {
		var pos=bufr[ii]["pos"];
		var descr=bufr[ii]["descr"];
		var info=bufr[ii]["info"];
		addChildButton(item,pos+" : "+descr+" "+info,"setValue('obsIndexPOS','"+pos+"');setValue('obsIndexDESCR','"+descr+"');setValue('obsIndexInfo','"+info+"');");
	    }
	}
    } else if (target === 'obsIndexExp') {
	var file=obs_getConfigFile();
	var bufrType=obs_config[file]["bufrType"];
	var subType=obs_config[file]["subType"];
	removeChildren(item);
	if ( obs_config[file] !== undefined &&
	     obs_config[file]["targets"] !== undefined 
	   ) {
	    for (var target in obs_config[file]["targets"]) {
		addChildButton(item,target,"addValue('obsIndexExp','"+target+"');");
	    }
	    addChildButton(item,"sec1970(,,,,,)","addValue('obsIndexExp','sec1970(,,,,,)');");
	    addChildButton(item,"julian(,,,,,)","addValue('obsIndexExp','julian(,,,,,)');");
	    addChildButton(item,"abs()","addValue('obsIndexExp','abs()');");
	    addChildButton(item,"exp()","addValue('obsIndexExp','exp()');");
	    addChildButton(item,"log10()","addValue('obsIndexExp','log10()');");
	    addChildButton(item,"log()","addValue('obsIndexExp','log()');");
	    addChildButton(item,"sqrt()","addValue('obsIndexExp','sqrt()');");
	    addChildButton(item,"sin()","addValue('obsIndexExp','sin()');");
	    addChildButton(item,"cos()","addValue('obsIndexExp','cos()');");
	    addChildButton(item,"tan()","addValue('obsIndexExp','tan()');");
	    addChildButton(item,"asin()","addValue('obsIndexExp','asin()');");
	    addChildButton(item,"acos()","addValue('obsIndexExp','acos()');");
	    addChildButton(item,"atan2(,)","addValue('obsIndexExp','atan2(,)');");
	}
    } else if (target === 'autoModelConfigFile') {
	documentLog.innerHTML="Sent obs-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"model"},function(data, status){
	    dataToArray(data,status,documentLog);
	    if (dropdownEd[target] === undefined || 
		dropdownEd[target] < auto_configEd) {
		removeChildren(item);
		for (var model in model_config) {
		    if (auto_config["model"][model] !== undefined) {
			addChildButton(item,model,
				       "setValue('autoModelConfigFile','"      +model+"');"+
				       "setInnerHTML('autoModelLastAuto','"+auto_config["model"][model]["lastAuto"]+"');"+
				       "setInnerHTML('autoModelLastUsed','"+auto_config["model"][model]["lastUsed"]+"');");
		    } else {
			addChildButton(item,model,
				       "setValue('autoModelConfigFile','"      +model+"');");
		    }
		}
		dropdownEd[target]=auto_configEd;
	    }
	    documentLog.innerHTML="";
	});
    } else if (target === 'autoObsConfigFile') {
	documentLog.innerHTML="Sent obs-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"obs"},function(data, status){
	    dataToArray(data,status,documentLog);
	    removeChildren(item);
	    for (var obs in obs_config) {
		if (auto_config["obs"][obs] !== undefined) {
		    addChildButton(item,obs,
				   "setValue('autoObsConfigFile','"      +obs+"');"+
				   "setInnerHTML('autoObsLastAuto','"+auto_config["obs"][obs]["lastAuto"]+"');"+
				   "setInnerHTML('autoObsLastUsed','"+auto_config["obs"][obs]["lastUsed"]+"');");
		} else {
		    addChildButton(item,obs,
				   "setValue('autoObsConfigFile','"      +obs+"');");
		}
	    }
	    documentLog.innerHTML="";
	});
    } else if (target === 'colocConfigFile') {
	documentLog.innerHTML="Sent coloc-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"coloc"},function(data, status){
	    dataToArray(data,status,documentLog);
	    //console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    for (var coloc in coloc_config) {
		addChildButton(item,coloc,"coloc_setConfigFile('"+coloc+"');coloc_show();");
	    }
	    documentLog.innerHTML="";
	});
    } else if (target === 'colocModelConfigFile') {
	documentLog.innerHTML="Sent model-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"model"},function(data, status){
	    dataToArray(data,status,documentLog);
	    //console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    for (var model in model_config) {
		addChildButton(item,model,"coloc_setConfig('modelConfigFile','file','"+model+"');coloc_show();");
	    }
	    addChildButton(item,"<none>","coloc_setConfig('modelConfigFile','file','');coloc_show();");
	    documentLog.innerHTML="";
	});
	removeChildren(item);
	for (var model in model_config) {
	    addChildButton(item,model,"coloc_setConfig('modelConfigFile','file','"+model+"');coloc_show();");
	}
	addChildButton(item,"<none>","coloc_setConfig('modelConfigFile','file','');coloc_show();");
    } else if (target === 'colocModelTargetVariable') {
	var file=coloc_getModelConfigFile();
	removeChildren(item);
 	if (model_config[file] !== undefined) {
	    var variables=model_config[file]["variables"];
	    if (variables !== undefined) {
		var len=variables.length;
		for (var ii=0;ii<len;ii++) {
		    addChildButton(item,variables[ii][0],"setValue('colocModelTargetVariable','"+variables[ii][0]+"');");
		}
	    }
	}
    } else if (target === 'colocObsConfigFile') {
	documentLog.innerHTML="Sent obs-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"obs"},function(data, status){
	    dataToArray(data,status,documentLog);
	    //console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    for (var obs in obs_config) {
		addChildButton(item,obs,"coloc_setConfig('obsConfigFile','file','"+obs+"');coloc_show();");
	    }
	    addChildButton(item,"<none>","coloc_setConfig('obsConfigFile','file','');coloc_show();");
	    documentLog.innerHTML="";
	});
    } else if (target === 'colocObsPOS') {
	var file=coloc_getObsConfigFile();
	if ( obs_config[file] !== undefined) {
	    var bufrType = obs_config[file]["bufrType"];
	    var subType = obs_config[file]["subType"];
	    removeChildren(item);
	    if (bufrType !== undefined && bufrType !== "" &&
		subType !== undefined && subType !== "" && subType !== "info" &&subType !== "cnt" &&
		obs_config[file] !== undefined && 
		obs_config[file]["bufr"] !== undefined && 
		obs_config[file]["bufr"][bufrType] !== undefined && 
		obs_config[file]["bufr"][bufrType][subType] !== undefined && 
		obs_config[file]["bufr"][bufrType][subType]["seq"] !== undefined ) {
		var bufr=obs_config[file]["bufr"][bufrType][subType]["seq"];
		var len=bufr.length;
		for (var ii=0;ii<len;ii++) {
		    var pos=bufr[ii]["pos"];
		    var descr=bufr[ii]["descr"];
		    var info=bufr[ii]["info"];
		    addChildButton(item,pos+" : "+descr+" "+info,"setValue('colocObsPOS','"+pos+"');setValue('colocObsDESCR','"+descr+"');setValue('colocObsInfo','"+info+"');");
		}
	    }
	}
    } else if (target === 'matchModelTargetName') {
	var file=coloc_getConfigFile();
	removeChildren(item);
	if ( coloc_config[file] !== undefined &&
	     coloc_config[file]["modelConfigFile"]["targets"] !== undefined &&
	     coloc_config[file]["modelConfigFile"]["targets"] !== undefined 
	   ) {
	    for (var t in coloc_config[file]["modelConfigFile"]["targets"]) {
		addChildButton(item,t,"setValue('matchModelTargetName','"+t+"');");
	    }
	}
    } else if (target.substring(0,15) === 'matchExpression') {
	var cnt=target.substring(15);
	var file=coloc_getConfigFile();
	var mfile = coloc_getModelConfigFile();
	var ofile = coloc_getObsConfigFile();
	removeChildren(item);
	if ( coloc_config[file] !== undefined &&
	     coloc_config[file]["obsConfigFile"] !== undefined &&
	     coloc_config[file]["obsConfigFile"]["targets"] !== undefined 
	   ) {
	    var trg="";
	    var ii=0;
	    var targets=coloc_config[file]["modelConfigFile"]["targets"];
	    for (var t in targets) {
		ii=ii+1;
		if (cnt == ii) {
		    trg=t;
		}
	    };
	    if (coloc_config[file]["modelConfigFile"]["targets"][trg] !== undefined) {
		var variable=coloc_config[file]["modelConfigFile"]["targets"][trg]["variable"];
		var index=model_config[mfile]["index"];
		var indexTrg=obs_config[ofile]["indexTarget"];
		var mark=(index === variable);
		//console.log("matchExpression ",variable,trg,dropdown);
		if (mark) {
		    addChildButton(item,indexTrg,"addValue('"+target+"','"+indexTrg+"');coloc_addConfigFilesTarget('modelConfigFile','"+trg+"','exp','"+indexTrg+"');");
		} else {
		    for (var t in coloc_config[file]["obsConfigFile"]["targets"]) {
			addChildButton(item,t,"addValue('"+target+"','"+t+"');coloc_addConfigFilesTarget('modelConfigFile','"+trg+"','exp','"+t+"');");
		    }
		}
	    } else {
		for (var t in coloc_config[file]["obsConfigFile"]["targets"]) {
		    addChildButton(item,t,"addValue('"+target+"','"+t+"');coloc_addConfigFilesTarget('modelConfigFile','"+trg+"','exp','"+t+"');");
		}
	    }
	    addChildButton(item,"sec1970(,,,,,)","addValue('"+target+"','sec1970(,,,,,)');");
	    addChildButton(item,"julian(,,,,,)","addValue('"+target+"','sec1970(,,,,,)');");
	    addChildButton(item,"abs()","addValue('"+target+"','abs()');");
	    addChildButton(item,"exp()","addValue('"+target+"','exp()');");
	    addChildButton(item,"log10()","addValue('"+target+"','log10()');");
	    addChildButton(item,"log()","addValue('"+target+"','log()');");
	    addChildButton(item,"sqrt()","addValue('"+target+"','sqrt()');");
	    addChildButton(item,"sin()","addValue('"+target+"','sin()');");
	    addChildButton(item,"cos()","addValue('"+target+"','cos()');");
	    addChildButton(item,"tan()","addValue('"+target+"','tan()');");
	    addChildButton(item,"asin()","addValue('"+target+"','asin()');");
	    addChildButton(item,"acos()","addValue('"+target+"','acos()');");
	    addChildButton(item,"atan2(,)","addValue('"+target+"','atan2(,)');");
	}
    } else if (target.substring(0,13) === 'colocDebugExp') {
	removeChildren(item);
	addChildButton(item,"sec1970(,,,,,)","addValue('"+target+"','sec1970(,,,,,)');");
	addChildButton(item,"julian(,,,,,)","addValue('"+target+"','sec1970(,,,,,)');");
	addChildButton(item,"abs()","addValue('"+target+"','abs()');");
	addChildButton(item,"exp()","addValue('"+target+"','exp()');");
	addChildButton(item,"log10()","addValue('"+target+"','log10()');");
	addChildButton(item,"log()","addValue('"+target+"','log()');");
	addChildButton(item,"sqrt()","addValue('"+target+"','sqrt()');");
	addChildButton(item,"sin()","addValue('"+target+"','sin()');");
	addChildButton(item,"cos()","addValue('"+target+"','cos()');");
	addChildButton(item,"tan()","addValue('"+target+"','tan()');");
	addChildButton(item,"asin()","addValue('"+target+"','asin()');");
	addChildButton(item,"acos()","addValue('"+target+"','acos()');");
	addChildButton(item,"atan2(,)","addValue('"+target+"','atan2(,)');");
    } else {
	console.log("Unknown dropdown target:", target);
    }
    document.getElementById(dropdown).classList.toggle("show");
}
function setValue(target,value) {
    if (document.getElementById(target) == null) {
	console.log("Undefined target:",target," Value:",value);
    } else {
	document.getElementById(target).value=value;
    }
}
function addValue(target,value) {
    //console.log("Item:",value);
    document.getElementById(target).value=document.getElementById(target).value + " " + value;
}
function setInnerHTML(target,value) {
    //console.log("Item:",value);
    document.getElementById(target).innerHTML=value;
}

function dataToArray(data,status,documentLog) {
    if (status == "success") {
	dataToModel(data);
	dataToObs(data);
	dataToAuto(data);
	dataToColoc(data);
    }
    documentLog.innerHTML="";
}

function dataToModel(data) {
    // <model_config name="test" fileFilter="*"> <variable name="var1"> </model_config>
    var models=data.getElementsByTagName("model_config");
    for (var ii = 0; ii < models.length; ii++) {
	var name=models[ii].getAttribute("file");
	if (model_config[name] === undefined) {
	    model_config[name]={variables : [ ]}
	}
	model_config[name]["fileFilterDir"]=
	    set(model_config[name]["fileFilterDir"],models[ii].getAttribute("fileFilterDir"));
	model_config[name]["fileFilter"]=
	    set(model_config[name]["fileFilter"],models[ii].getAttribute("fileFilter"));
	model_config[name]["hits"]=
	    set(model_config[name]["hits"],models[ii].getAttribute("hits"));
	model_config[name]["index"]=
	    set(model_config[name]["index"],models[ii].getAttribute("index"));
	model_config[name]["start"]=
	    set(model_config[name]["start"],models[ii].getAttribute("start"));
	model_config[name]["stop"]=
	    set(model_config[name]["stop"],models[ii].getAttribute("stop"));
	var variables=models[ii].getElementsByTagName("variable");
	if (variables) {
	    model_config[name]["variables"]=[];
	    for (var jj = 0; jj < variables.length; jj++) {
		model_config[name]["variables"].push([variables[jj].getAttribute("name"),variables[jj].getAttribute("dims")]);
	    }
	} else if (model_config[name]["variables"] === undefined) {
	    model_config[name]["variables"]=[];
	}
	var files=models[ii].getElementsByTagName("stack");
	if (files) {
	    model_config[name]["files"]=[];
	    for (var jj = 0; jj < files.length; jj++) {
		model_config[name]["files"].push(files[jj].getAttribute("name"));
	    }
	} else if (model_config[name]["files"] === undefined) {
	    model_config[name]["files"]=[];
	}
    };
}
function dataToObs(data) {
    // <obs_config name="test" fileFilter="*"> <bufr id="var1"> <sub id="2"> ...</obs_config>
    var obs=data.getElementsByTagName("obs_config");
    for (var ii = 0; ii < obs.length; ii++) {
	var name=obs[ii].getAttribute("file");
	if (obs_config[name] === undefined) {
	    obs_config[name]={bufr :{}, filter:{}}
	}
	obs_config[name]["fileFilterDir"]=
	    set(obs_config[name]["fileFilterDir"],obs[ii].getAttribute("fileFilterDir"));
	obs_config[name]["fileFilter"]=
	    set(obs_config[name]["fileFilter"],obs[ii].getAttribute("fileFilter"));
	obs_config[name]["hits"]=
	    set(obs_config[name]["hits"],obs[ii].getAttribute("hits"));
	obs_config[name]["start"]=
	    set(obs_config[name]["start"],obs[ii].getAttribute("start"));
	//console.log("fark.js start:",name,obs_config[name]["start"])
	obs_config[name]["stop"]=
	    set(obs_config[name]["stop"],obs[ii].getAttribute("stop"));
	obs_config[name]["tablePath"]=
	    set(obs_config[name]["tablePath"],obs[ii].getAttribute("tablePath"));
	obs_config[name]["bufrType"]=
	    set(obs_config[name]["bufrType"],obs[ii].getAttribute("bufrType"));
	obs_config[name]["subType"]=
	    set(obs_config[name]["subType"],obs[ii].getAttribute("subType"));
	obs_config[name]["typeInfo"]=
	    set(obs_config[name]["typeInfo"],obs[ii].getAttribute("typeInfo"));
	obs_config[name]["indexTarget"]=
	    set(obs_config[name]["indexTarget"],obs[ii].getAttribute("indexTarget"));
	obs_config[name]["indexExp"]=
	    set(obs_config[name]["indexExp"],obs[ii].getAttribute("indexExp"));
	if (obs_config[name]["bufr"] === undefined) { 
	    obs_config[name]["bufr"]={};
	};
	var bufr=obs[ii].getElementsByTagName("bufr");
	if (bufr.length > 0) {
	    obs_config[name]["bufr"]={};
	    for (var jj = 0; jj < bufr.length; jj++) {
		var bufrType=bufr[jj].getAttribute("bufrType");
		var subType=bufr[jj].getAttribute("subType") || "";
		var info=bufr[jj].getAttribute("info");
		var cnt=bufr[jj].getAttribute("cnt");
		if (obs_config[name]["bufr"][bufrType] === undefined) { 
		    obs_config[name]["bufr"][bufrType]={};
		};
		if (subType !== undefined && subType !== "") {
		    if (obs_config[name]["bufr"][bufrType][subType] === undefined) { 
			obs_config[name]["bufr"][bufrType][subType]={seq:[],info:info,cnt:cnt};
		    }
		    var seq=bufr[jj].getElementsByTagName("seq");
		    for (var pp = 0; pp < seq.length; pp++) {
			var pos=seq[pp].getAttribute("pos");
			var descr=seq[pp].getAttribute("descr");
			var pinfo=seq[pp].getAttribute("info");
			obs_config[name]["bufr"][bufrType][subType]["seq"].push({pos:pos,descr:descr,info:pinfo});
		    }
		} else {
		    obs_config[name]["bufr"][bufrType]["info"]=info;
		    obs_config[name]["bufr"][bufrType]["cnt"]=cnt;
		}
	    }
	};
	// read targets
	obs_config[name]["targets"]={};
	var targets=obs[ii].getElementsByTagName("target");
	for (var jj = 0; jj < targets.length; jj++) {
	    var target=targets[jj].getAttribute("name");
	    obs_config[name]["targets"][target]={
		pos:targets[jj].getAttribute("pos"),
		descr:targets[jj].getAttribute("descr"),
		info:targets[jj].getAttribute("info"),
		min:targets[jj].getAttribute("min"),
		max:targets[jj].getAttribute("max")};
	}
	var files=obs[ii].getElementsByTagName("stack");
	if (files) {
	    obs_config[name]["files"]=[];
	    for (var jj = 0; jj < files.length; jj++) {
		obs_config[name]["files"].push(files[jj].getAttribute("name"));
	    }
	} else if (obs_config[name]["files"] === undefined) {
	    obs_config[name]["files"]=[];
	}
    }
}

function dataToAuto(data) {
    // <auto_config name="test" fileFilter="*"> <variable name="var1"> </auto_config>
    var autos=data.getElementsByTagName("auto_config");
    for (var ii = 0; ii < autos.length; ii++) {
	auto_config["model"]={};
	var models=autos[ii].getElementsByTagName("model");
	for (var jj = 0; jj < models.length; jj++) {
	    var model=models[jj].getAttribute("file");
	    var lastAuto=models[jj].getAttribute("lastAuto");
	    var lastUsed=models[jj].getAttribute("lastUsed");
	    var status=models[jj].getAttribute("status")||"";
	    auto_config["model"][model]={lastAuto:lastAuto, lastUsed:lastUsed, status:status};
	};
	auto_config["obs"]={};
	var obses=autos[ii].getElementsByTagName("obs");
	for (var jj = 0; jj < obses.length; jj++) {
	    var obs=obses[jj].getAttribute("file");
	    var lastAuto=obses[jj].getAttribute("lastAuto");
	    var lastUsed=obses[jj].getAttribute("lastUsed");
	    var status=obses[jj].getAttribute("status")||"";
	    auto_config["obs"][obs]={lastAuto:lastAuto, lastUsed:lastUsed, status:status};
	};
    }
}

function dataToColoc(data) {
    // <coloc_config name="test" fileFilter="*"> <variable name="var1"> </coloc_config>
    var colocs=data.getElementsByTagName("coloc_config");
    for (var ii = 0; ii < colocs.length; ii++) {
	var name=colocs[ii].getAttribute("file");
	coloc_config[name]={modelConfigFile:{targets:{},def:[]},
			  obsConfigFile:{targets:{}},
			  host:"localhost"
			 };
	coloc_config[name]["host"]=
	    set(coloc_config[name]["host"],colocs[ii].getAttribute("host"));
	//console.log("Host:",ii,name,coloc_config[name]["host"],colocs[ii].getAttribute("host"));
	coloc_config[name]["modelConfigFile"]["file"]=
	    set(coloc_config[name]["modelConfigFile"]["file"],colocs[ii].getAttribute("modelFile"));
	coloc_config[name]["modelConfigFile"]["start"]=
	    set(coloc_config[name]["modelConfigFile"]["start"],colocs[ii].getAttribute("modelStart"));
	coloc_config[name]["modelConfigFile"]["stop"]=
	    set(coloc_config[name]["modelConfigFile"]["stop"],colocs[ii].getAttribute("modelStop"));
	var modelTargets=colocs[ii].getElementsByTagName("modelTarget");
	for (var jj = 0; jj < modelTargets.length; jj++) {
	    var target=modelTargets[jj].getAttribute("name");
	    var variable=modelTargets[jj].getAttribute("variable");
	    var min=modelTargets[jj].getAttribute("min");
	    var max=modelTargets[jj].getAttribute("max");
	    coloc_config[name]["modelConfigFile"]["targets"][target]={variable:variable,min:min,max:max};
	};
	var defs=colocs[ii].getElementsByTagName("modelDefault");
	coloc_config[name]["modelConfigFile"]["def"]=[];
	for (var jj = 0; jj < defs.length; jj++) {
	    var info=defs[jj].getAttribute("info");
	    var targets={targets:{}, info:info};
	    var defTargets=defs[jj].getElementsByTagName("def");
	    for (var kk = 0; kk < defTargets.length; kk++) {
		var target=defTargets[kk].getAttribute("name");
		var value=defTargets[kk].getAttribute("value");
		targets["targets"][target]=value;
	    };
	    coloc_config[name]["modelConfigFile"]["def"].push( targets );
	};
	coloc_config[name]["obsConfigFile"]["file"]=
	    set(coloc_config[name]["obsConfigFile"]["file"],colocs[ii].getAttribute("obsFile"));
	coloc_config[name]["obsConfigFile"]["start"]=
	    set(coloc_config[name]["obsConfigFile"]["start"],colocs[ii].getAttribute("obsStart"));
	coloc_config[name]["obsConfigFile"]["stop"]=
	    set(coloc_config[name]["obsConfigFile"]["stop"],colocs[ii].getAttribute("obsStop"));
	var obsTargets=colocs[ii].getElementsByTagName("obsTarget");
	for (var jj = 0; jj < obsTargets.length; jj++) {
	    var target=obsTargets[jj].getAttribute("name");
	    var bufrType=obsTargets[jj].getAttribute("bufrType");
	    var subType=obsTargets[jj].getAttribute("subType");
	    var pos=obsTargets[jj].getAttribute("pos");
	    var descr=obsTargets[jj].getAttribute("descr");
	    var info=obsTargets[jj].getAttribute("info");
	    var min=obsTargets[jj].getAttribute("min");
	    var max=obsTargets[jj].getAttribute("max");
	    coloc_config[name]["obsConfigFile"]["targets"][target]={bufrType:bufrType,
								subType:subType,
								pos:pos,
								descr:descr,
								info:info,
								min:min,
								max:max};
	};
	var matchRules=colocs[ii].getElementsByTagName("matchRules");
	for (var jj = 0; jj < matchRules.length; jj++) {
	    var target=matchRules[jj].getAttribute("name");
	    var expression=matchRules[jj].getAttribute("expression");
	    coloc_config[name]["modelConfigFile"]["targets"][target]["exp"]=expression;
	}
    }
}
function clone(obj) {
    if (null == obj || "object" != typeof obj) return obj;
    var copy = obj.constructor();
    for (var attr in obj) {
        if (obj.hasOwnProperty(attr)) copy[attr] = obj[attr];
    }
    return copy;
}

function set(target, value) {
    var ret="";
    if (value !== undefined && value !== null) {
	ret=value;
    } else if (target !== undefined)  {
	ret=target;
    };
    return ret;
}
#__file: 'model.js' 0100664    **DO NOT DELETE**
model_file="default.cfg";
model_config = { "default.cfg" : {fileFilter: ".*\.nc",
				  fileFilterDir: "/opdata",
				  hits : "?",
				  index : "",
				  variables : [["def",""]],
				  files : ["file1","file2"],
				  password: ""
				 }
	       };
model_configEd = 0;

// make new obs-filter entry
function model_setConfigFile(value) {
    if (model_config[value] === undefined) {
	model_config[value]=clone(model_config[model_file]);
	//console.log("cloned:",value,model_config[value]);
    }
    model_file=value;
}
function model_getConfigFile() {
    return model_file;
};
function model_setArray(parameter,value) {
    var file=model_getConfigFile();
    //console.log("File:",file,parameter,model_config[file]);
    model_config[file][parameter]=value;
};
function model_show() {
    var file=model_getConfigFile();
    setValue('modelConfigFile',file);
    setValue('modelFileFilterDir',model_config[file]["fileFilterDir"]);
    setValue('modelFileFilter',model_config[file]["fileFilter"]);
    setValue('modelIndexVariable',model_config[file]["index"]);
    setInnerHTML('modelPatternHits',model_config[file]["hits"]);
};
// model config methods
function model_checkPassword() {
    var password=document.getElementById("modelConfigFilePsw").value;
    var file=model_getConfigFile();
    if (model_config[file] !== undefined) {
	if (model_config[file]["password"] !== undefined) {
	    if (model_config[file]["password"] !== password) {
		alert("Invalid password used when attempting to save file:\n"+file);
		return false;
	    }
	}
    };
    return true;
}
function model_saveConfigFile() {
    var file=model_getConfigFile();
    var password=document.getElementById("modelConfigFilePsw").value;
    // send to server...
    var stack="";
    var len=model_config[file]["files"].length;
    for (var ii=0; ii<len;ii++) {
	var sfile=model_config[file]["files"][ii];
	stack=stack+"|"+sfile;
    };
    var variables="";
    var len=model_config[file]["variables"].length;
    for (var ii=0; ii<len;ii++) {
	var variable=model_config[file]["variables"][ii][0];
	var dims=model_config[file]["variables"][ii][1];
	variables=variables+"|"+variable+"~"+dims;
    };
    documentLog.innerHTML="Sent model-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"model",
			     file:file,
			     password:password,
			     filterDir:model_config[file]["fileFilterDir"],
			     filter:model_config[file]["fileFilter"],
			     hits:model_config[file]["hits"],
			     index:model_config[file]["index"],
                             stack:stack,
			     variables:variables
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
    makeUrl("model",file);
};
function model_updateData() {
	documentLog.innerHTML="Sent model-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"model"},function(data, status){
	    dataToArray(data,status,documentLog);
	    modelLoaded=true;
	    //console.log("Updating dropdown for ",target);
	    model_show();
	    documentLog.innerHTML="";
	});
};
function model_patternFind(target) {
    var dropdown=target + 'Dropdown';
    var item=document.getElementById(dropdown);
    var file=model_getConfigFile();
    var password=document.getElementById("modelConfigFilePsw").value;
    var filterDir = model_config[file]["fileFilterDir"];
    var filter = model_config[file]["fileFilter"];
    documentLog.innerHTML="Sent model-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"model",
			     file:file,
			     password:password,
			     filterDir:model_config[file]["fileFilterDir"],
			     filter:model_config[file]["fileFilter"],
			     index:model_config[file]["index"]},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to peek at: "+filterDir+" "+filter+" (file:"+file+")\n"+msg);
	      } else {
		  dataToArray(data,status,documentLog);
		  removeChildren(item);
		  var len=model_config[file]["files"].length;
		  for (var ii=0; ii<len;ii++) {
		      var sfile=model_config[file]["files"][ii];
		      addChildButton(item,sfile,"model_fileFind('"+sfile+"');");
		  }
		  model_show();
	      };
	      documentLog.innerHTML="";
	      document.getElementById(dropdown).classList.toggle("show");
	  }
				}
	 );
};
function model_fileFind(sfile) {
    var file=model_getConfigFile();
    var password=document.getElementById("modelConfigFilePsw").value;
    documentLog.innerHTML="Sent model-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"modelfile",
			     file:file,
			     password:password,
			     target:sfile},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to auto file: "+sfile+" (file:"+file+")\n"+msg);
	      } else {
		  dataToArray(data,status,documentLog);
		  model_show();
	      };
	      documentLog.innerHTML="";}
				}
	 );
};
#__file: 'obs.js' 0100664    **DO NOT DELETE**
obs_file = "default.cfg";
obs_config = { "default.cfg" : { fileFilterDir : "/home/www/bufr/",
				 fileFilter : ".*\.bufr",
				 tablePath : "/home/www/fark-perl_0.15/bufrtables/",
				 hits : "?",
				 bufr : { 99 : { info : "default", 9 : { seq: [{pos: 1, descr:99999, info: "default info"}],
									 info : "more default info"
						     }
					       }
					},
				 bufrType : "99",
				 subType : "9",
				 typeInfo : "default info.",
				 targets : { "yy" : {pos:"", descr:"", info:"", min:"", max:""}},
				 indexTarget : "time",
				 indexExp : "sec1970(yy,mm,dd,hh,mi)",
				 files : ["file1","file2"],
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
function obs_setIndexTarget(target,parameter,value) {
    var file=obs_getConfigFile();
    obs_config[file]["targets"][target][parameter]=value;
};
function obs_show() {
    var file=obs_getConfigFile();
    setValue('obsConfigFile',file);
    setValue('obsFileFilterDir',obs_config[file]["fileFilterDir"]);
    setValue('obsFileFilter',obs_config[file]["fileFilter"]);
    setValue('obsTablePath',obs_config[file]["tablePath"]);
    setValue('obsBufrType',obs_config[file]["bufrType"]);
    setValue('obsSubType',obs_config[file]["subType"]);
    setValue('obsTypeInfo',obs_config[file]["typeInfo"]);
    obs_setIndexTargetTable(file,obs_config[file]['targets']);
    setValue('obsIndexTarget',obs_config[file]["indexTarget"]);
    setValue('obsIndexExp',obs_config[file]["indexExp"]);
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
function obs_removeTarget(target) {
    var file=obs_getConfigFile();
    var item=document.getElementById("newlineObsIndexTarget");
    item.children[1].children[0].value=target;
    item.children[2].children[0].value=obs_config[file]["targets"][target]["pos"];
    item.children[4].children[0].value=obs_config[file]["targets"][target]["descr"];
    item.children[5].children[0].value=obs_config[file]["targets"][target]["info"];
    item.children[6].children[0].value=obs_config[file]["targets"][target]["min"];
    item.children[7].children[0].value=obs_config[file]["targets"][target]["max"];
    delete obs_config[file]["targets"][target];
    obs_setIndexTargetTable(file,obs_config[file]["targets"]);
};

function obs_isEmpty(obj) {
    for(var prop in obj) {
        if(obj.hasOwnProperty(prop))
            return false;
    }
    return true;
};
function obs_newObsIndexTarget(item) {
    //if (! obs_checkPassword()) {return;}
    var target=item.parentNode.parentNode.children[1].children[0].value;
    var pos=item.parentNode.parentNode.children[2].children[0].value;
    var descr=item.parentNode.parentNode.children[4].children[0].value;
    var info=item.parentNode.parentNode.children[5].children[0].value;
    var min=item.parentNode.parentNode.children[6].children[0].value;
    var max=item.parentNode.parentNode.children[7].children[0].value;
//    console.log("New: trg:",target," pos:",pos," des:",descr," info:",info," min:",min," max:",max);
    if (target !== "") {
	var file= obs_getConfigFile();
	if (obs_config[file] === undefined) {
	    obs_config[file]={};
	};
	if (obs_config[file]["targets"] === undefined) {
	    obs_config[file]["targets"]={};
	};
	if (obs_config[file]["targets"][target] === undefined) {
	    obs_config[file]["targets"][target]={};
	};
	obs_config[file]["targets"][target]["pos"]=pos;
	obs_config[file]["targets"][target]["descr"]=descr;
	obs_config[file]["targets"][target]["info"]=info;
	obs_config[file]["targets"][target]["min"]=min;
	obs_config[file]["targets"][target]["max"]=max;
	obs_setIndexTargetTable(file,obs_config[file]["targets"]);
	item.parentNode.parentNode.children[1].children[0].value="";
	item.parentNode.parentNode.children[2].children[0].value="";
	item.parentNode.parentNode.children[4].children[0].value="";
	item.parentNode.parentNode.children[5].children[0].value="";
	item.parentNode.parentNode.children[6].children[0].value="";
	item.parentNode.parentNode.children[7].children[0].value="";
    } else {
	alert("Invalid observation target name: ('"+target+"')");
    }
};
function obs_saveConfigFile() {
    var file=obs_getConfigFile();
    var password=document.getElementById("obsConfigFilePsw").value;
    var bufrType=obs_config[file]["bufrType"];
    var subType=obs_config[file]["subType"];
    var typeInfo=obs_config[file]["typeInfo"];
    var indexTarget=obs_config[file]["indexTarget"];
    var indexExp=obs_config[file]["indexExp"];
    var obsTargets="";
    var targets=obs_config[file]["targets"];
    for (var target in targets) {
	var pos=targets[target]["pos"];
	var descr=targets[target]["descr"];
	var info=targets[target]["info"];
	var min=targets[target]["min"];
	var max=targets[target]["max"];
	obsTargets=obsTargets + "|" + target + "~" + pos + "~" + descr + "~" + info + "~" + min + "~" + max;
    };
    obs_configEd++;
    documentLog.innerHTML="Sent obs-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"obs",file:file,password:password,
			     filterDir:obs_config[file]["fileFilterDir"],
			     filter:obs_config[file]["fileFilter"],
			     table:obs_config[file]["tablePath"],
                             bufrType:bufrType,
                             subType:subType,
                             typeInfo:typeInfo,
                             indexTarget:indexTarget,
                             indexExp:indexExp,
			     obsTargets:obsTargets
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
    makeUrl("obs",file);
};
// make new obs-index entry
function obs_setIndexTargetTable(file,value) {
    var file=obs_getConfigFile();
    var bufrType = obs_config[file]["bufrType"];
    var subType = obs_config[file]["subType"];
    var item=document.getElementById('obsIndexTargetTable');
    var tail=removeTableChildFromTo(item,"labelsObsIndexTarget","newlineObsIndexTarget");
    for (var target in value) {
	obs_insertIndexTargetRow(tail,target,value[target]["pos"],value[target]["descr"],
				 value[target]["info"],value[target]["min"],value[target]["max"]);
    }
}
function obs_insertIndexTargetRow(item,target,pos,descr,info,min,max) {
    var row = document.createElement("TR");
    var td,inp;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","obs_removeTarget('"+target+"')");
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
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','pos',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make descr column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",descr);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','descr',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make info column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",info);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','info',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make minimum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','min',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make maximum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','max',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
function obs_updateData() {
    documentLog.innerHTML="Sent obs-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"obs"},function(data, status){
	dataToArray(data,status,documentLog);
	obsLoaded=true;
	//console.log("Updating dropdown for ",target);
	obs_show();
	documentLog.innerHTML="";
    });
};
function obs_patternFind(target) {
    var dropdown=target + 'Dropdown';
    var item=document.getElementById(dropdown);
    var file=obs_getConfigFile();
    var password=document.getElementById("obsConfigFilePsw").value;
    var filterDir = obs_config[file]["fileFilterDir"];
    var filter = obs_config[file]["fileFilter"];
    var table = obs_config[file]["tablePath"];
    var obsTargets = "";
    var obsTrg=obs_config[file]["targets"];
    for (var target in obsTrg) {
	obsTargets=obsTargets + "|" + target + "~" + 
	    obsTrg[target]["pos"] + "~" + 
	    obsTrg[target]["descr"] + "~" + 
	    obsTrg[target]["info"] + "~" + 
	    obsTrg[target]["min"] + "~" + 
	    obsTrg[target]["max"];
    };
    var indexTarget = obs_config[file]["indexTarget"];
    var indexExp = obs_config[file]["indexExp"];
    var bufrType = obs_config[file]["bufrType"];
    var subType = obs_config[file]["subType"];
    var typeInfo=obs_config[file]["typeInfo"];
    documentLog.innerHTML="Sent obs-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"obs",
				  file:file,
				  password:password,
				  filterDir:obs_config[file]["fileFilterDir"],
				  filter:obs_config[file]["fileFilter"],
				  table:obs_config[file]["tablePath"],
				  obsTargets:obsTargets,
				  indexTarget:indexTarget,
				  indexExp:indexExp,
				  bufrType:bufrType,
				  subType:subType,
				  typeInfo:typeInfo},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to peek at: "+filterDir+" "+filter+" (file:"+file+")\n"+msg);
	      } else {
		  dataToArray(data,status,documentLog);
		  removeChildren(item);
		  var len=obs_config[file]["files"].length;
		  for (var ii=0; ii<len;ii++) {
		      var sfile=obs_config[file]["files"][ii];
		      addChildButton(item,sfile,"obs_fileFind('"+sfile+"');");
		  }
		  obs_show();
	      };
	      documentLog.innerHTML="";
	      document.getElementById(dropdown).classList.toggle("show");
	  }
				}
	 );
};
function obs_fileFind(sfile) {
    var file=obs_getConfigFile();
    var password=document.getElementById("obsConfigFilePsw").value;
    var filterDir = obs_config[file]["fileFilterDir"];
    var filter = obs_config[file]["fileFilter"];
    var table = obs_config[file]["tablePath"];
    var obsTargets = "";
    var obsTrg=obs_config[file]["targets"];
    for (var target in obsTrg) {
	obsTargets=obsTargets + "|" + target + "~" + 
	    obsTrg[target]["pos"] + "~" + 
	    obsTrg[target]["descr"] + "~" + 
	    obsTrg[target]["info"] + "~" + 
	    obsTrg[target]["min"] + "~" + 
	    obsTrg[target]["max"];
    };
    var indexTarget = obs_config[file]["indexTarget"];
    var indexExp = obs_config[file]["indexExp"];
    var bufrType = obs_config[file]["bufrType"];
    var subType = obs_config[file]["subType"];
    var typeInfo=obs_config[file]["typeInfo"];
    documentLog.innerHTML="Sent obs-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"obsfile",
				  file:file,
				  target:sfile,
				  password:password,
				  filterDir:obs_config[file]["fileFilterDir"],
				  filter:obs_config[file]["fileFilter"],
				  table:obs_config[file]["tablePath"],
				  obsTargets:obsTargets,
				  indexTarget:indexTarget,
				  indexExp:indexExp,
				  bufrType:bufrType,
				  subType:subType,
				  typeInfo:typeInfo},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to auto file: "+sfile+" (file:"+file+")\n"+msg);
	      } else {
		  dataToArray(data,status,documentLog);
		  obs_show();
	      };
	      documentLog.innerHTML="";}
				}
	 );
};
#__file: 'auto.js' 0100664    **DO NOT DELETE**
// data structure
auto_config = { model : {"model_config1.cfg" : {lastAuto:"",lastUsed:"",status:""},
			 "model_config2.cfg": {lastAuto:"",lastUsed:"",status:""}},
		obs :  {"obs_config1.cfg": {lastAuto:"",lastUsed:"",status:""}},
		password: "test"
	      };
auto_configEd=0;


// auto methods
function auto_checkPassword() {
    var password=document.getElementById("autoConfigPsw").value;
    if (auto_config["password"] !== undefined) {
	if (auto_config["password"] !== password) {
	    alert("Invalid password used when attempting to save Auto configuration\n");
	    return false;
	}
    };
    return true;
}
function auto_updateData() {
    documentLog.innerHTML="Sent auto-load request.";
    var root="auto.cfg";
    $.get("cgi-bin/fark_load.pl",{type:"auto",root:root},function(data, status){
	dataToArray(data,status,documentLog);
	auto_setTable();
    });
};
function auto_newModelConfigFile(item) {
    var file=item.parentNode.parentNode.children[1].children[0].value;
    //item.parentNode.parentNode.children[1].children[0].value="";
    setValue('autoModelConfigFile',"");
    setInnerHTML('autoModelLastAuto',"");
    setInnerHTML('autoModelLastUsed',"");
    if (file !== "" ) {
	if (auto_config["model"][file] === undefined) {
	    auto_config["model"][file]={lastAuto:"",lastUsed:""};
	};
	auto_setTable();
    } else {
	alert("Invalid: Model config file ('"+file+"')");
    }
};
function auto_autoNow(target,type,file) {
    var root="";
    if (target === "") {root="auto.cfg";};
    if (file !== "") {
	documentLog.innerHTML="Sent auto-now request ("+file+").";
	$.get("cgi-bin/fark_auto.pl",{root:root,type:type,file:file},
	      function(data, status){
		  if (status == "success") {
		      var errors=data.getElementsByTagName("error");
		      if (errors.length > 0 ) {
			  console.log("Error:",data);
			  var msg=(errors[0].getAttribute("message")||"");
			  alert("Unable to auto, "+type+" config file: "+file+"\n"+msg);
		      };
		      if (target === "") {
			  dataToArray(data,status,documentLog);
			  auto_setTable();
		      } else {
			  target.children[3].innerHTML="manual";
		      }
		      documentLog.innerHTML="";}
	      }
	 );
    };
};
function auto_newObsConfigFile(item) {
    var file=item.parentNode.parentNode.children[1].children[0].value;
    //item.parentNode.parentNode.children[1].children[0].value="";
    setValue('autoObsConfigFile',"");
    setInnerHTML('autoObsLastAuto',"");
    setInnerHTML('autoObsLastUsed',"");
    if (file !== "" ) {
	if (auto_config["obs"][file] === undefined) {
	    auto_config["obs"][file]={lastAuto:"",lastUsed:""};
	};
	auto_setTable();
    } else {
	alert("Invalid: Obs config file ('"+file+"')");
    }
};
function auto_saveConfig(target) {
    var root="auto.cfg";
    var password=document.getElementById("autoConfigFilePsw").value;
    var modelFiles="";
    var obsFiles="";
    auto_setTable();
    var len=auto_config["model"];
    for (var model in auto_config["model"]) {
	modelFiles=modelFiles + "|" + model + "~" + 
	    auto_config["model"][model]["lastAuto"] + "~";
	    auto_config["model"][model]["lastUsed"];
    };
    for (var obs in auto_config["obs"]) {
	obsFiles=obsFiles + "|" + obs + "~" + 
	    auto_config["obs"][obs]["lastAuto"] + "~";
	    auto_config["obs"][obs]["lastUsed"];
    }
    documentLog.innerHTML="Sent obs-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"auto",root:root,password:password,modelFiles:modelFiles,obsFiles:obsFiles},
	  function(data, status){
	      if (status == "success") {
		  var errors=data.getElementsByTagName("error");
		  if (errors.length > 0 ) {
		      console.log("Error:",data);
		      var msg=(errors[0].getAttribute("message")||"");
		      alert("Unable to auto, config file: "+root+"\n"+msg);
		  };
		  documentLog.innerHTML="";}
	  }
	 );
    makeUrl("auto",root);
};
function auto_removeFile(item,type,file) {
    if (type == "model") {
	var item=document.getElementById("newlineAutoModel");
	item.children[1].children[0].value=file;
    } else if (type =="obs") {
	var item=document.getElementById("newlineAutoObs");
	item.children[1].children[0].value=file;
    };
    //if (! checkAutoPassword()) {return;}
    //item.parentNode.removeChild(item);
    delete auto_config[type][file];
    auto_setTable();
};

// create auto table
function auto_setTable() {
    var item=document.getElementById('autoTable');
    var tail=removeTableChildFromTo(item,"labelsAutoModel","newlineAutoModel");
    for (var model in auto_config["model"]) {
	auto_insertRow(tail,"model",model,auto_config["model"][model]["lastAuto"],auto_config["model"][model]["lastUsed"],auto_config["model"][model]["status"]);
    }
    var tail=removeTableChildFromTo(item,"labelsAutoObs","newlineAutoObs");
    for (var obs in auto_config["obs"]) {
	auto_insertRow(tail,"obs",obs,auto_config["obs"][obs]["lastAuto"],auto_config["obs"][obs]["lastUsed"],auto_config["obs"][obs]["status"]);
    }
};
// create auto table row
function auto_insertRow(item,type,file,lastAuto,lastUsed,status) {
    var row = document.createElement("TR");
    var td;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","auto_removeFile(this.parentNode.parentNode,'"+type+"','"+file+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make FILE NAME column
    td=document.createElement("TD");
    td.innerHTML=file;
    row.appendChild(td);
    // make select-FILE NAME column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make LAST AUTO column
    td=document.createElement("TD");
    if (status !== "") {
	td.setAttribute("style","color:blue");
    }
    td.innerHTML=lastAuto;
    row.appendChild(td);
    // make LAST USED column
    td=document.createElement("TD");
    td.innerHTML=lastUsed;
    row.appendChild(td);
    // make AUTO NOW column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","auto_autoNow('','"+type+"','"+file+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("Auto now");
    btn.appendChild(t);
    td.appendChild(btn);

    row.appendChild(td);
    // make add row to table
    item.parentNode.insertBefore(row,item);
    return row;
}
#__file: 'url.js' 0100664    **DO NOT DELETE**
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
    setValue('urlConfigFile',file);
    setValue('urlModelConfigFile',url_config[file]["modelConfigFile"]["file"]);
    setValue('urlObsConfigFile',url_config[file]["obsConfigFile"]["file"]);
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
	setValue('urlModelConfigFile',url_config[file]["modelConfigFile"]["file"]);
	setValue('urlObsConfigFile',url_config[file]["obsConfigFile"]["file"]);
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
