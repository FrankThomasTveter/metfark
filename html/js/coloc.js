coloc_file="default.cfg"; // always valid file
coloc_config = { "default.cfg" : { modelConfigFile : { file: "default.cfg",
						       min : "def_min",
						       max : "def_max",
						       exp : "",
						       targets : { "def_model" : { variable : "def",
										   min: "def_min",
										   max : "def_max",
									           exp : "" 
										 } },
						       targeto : ["def_model"],
						       def : [ {targets: {"def_model": 101}, 
								info:"default info"} ]
						     },
				   obsConfigFile : { file: "default.cfg",
						     min: "def_min",
						     max : "def_max",
						     targets : { "def_obs" : {pos:"", 
									      descr:"", 
									      info:"",  
									      min:"", 
									      max:""}
							       },
						     targeto : ["def_obs"]
						   },
				   host:"fark.met.no",
				   filter:"",
				   xml:"",
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
function coloc_setConfigFile2(file) {
    showValue('colocConfigFile',file);
    showValue('colocConfigFileSave',file);
    console.log("Setting file= '"+file+"'");
}
function coloc_setConfigFile(file) {
    console.log("Setting file= '"+file+"'");
    showValue('colocConfigFile',file);
    showValue('colocConfigFileSave',file);
    //if (file != "") {
	coloc_allocate(file);
	coloc_file=file;
	var mfile=coloc_getModelConfigFile();
        fark_last['model']=coloc_getModelConfigFile();
	if (coloc_modelIsNotLoaded(mfile)) {coloc_updateModelData(mfile);}
	var ofile=coloc_getObsConfigFile();
        fark_last['obs']=coloc_getObsConfigFile();
	if (coloc_obsIsNotLoaded(ofile)) {coloc_updateObsData(ofile);}
    //};
    coloc_showCOLOC();
};
function coloc_getConfigFile() {
    return coloc_file;
};
function coloc_getModelConfigFile( file = "") {
    if (file === "") {file=coloc_getConfigFile();}
    if (coloc_config[file] !== undefined) {
	return coloc_config[file]["modelConfigFile"]["file"];
    } else {
	return "";
    }
};
function coloc_getObsConfigFile(file = "") {
    if (file === "") {file=coloc_getConfigFile();}
    if (coloc_config[file] !== undefined) {
	return coloc_config[file]["obsConfigFile"]["file"];
    } else {
	return "";
    }
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
function coloc_addConfig (type,parameter,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	coloc_config[file][type][parameter]=coloc_config[file][type][parameter]+val;
	coloc_showCOLOC();
    }
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
	coloc_config[file][type]=decodeURI(val);
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
	    coloc_config[file]={modelConfigFile:{targets:{},targeto:[],def:{}},
				obsConfigFile:{targets:{},targeto:[]},
				host:"fark.met.no",
				password:""};
	};
	if (coloc_config[file]["modelConfigFile"]["targets"][name] === undefined) {
	    coloc_config[file]["modelConfigFile"]["targeto"].push(name);
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
    var mfile=coloc_getModelConfigFile();
    if (model_config[mfile] !== undefined) {
	var indexTarget=model_config[mfile]["indexTarget"];
    } else {
	var indexTarget="";
    }
    var line={targets:{},info:{}};
    var ok=false;
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    var pos=1;
    var val=item.parentNode.parentNode.children[pos].children[0].value
    if (val !== undefined && val !== "") {
	ok=true;
    }
    line["targets"][indexTarget]=(val|| "");
    item.parentNode.parentNode.children[pos].children[0].value="";
    pos=pos+1;
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
		    coloc_config[file]={modelConfigFile:{targets:{},targeto:[],def:{}},
					obsConfigFile:{targets:{},targeto:[]},
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
		coloc_config[file]["obsConfigFile"]["targeto"].push(name);
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
    var targeto=coloc_config[file]["modelConfigFile"]["targeto"];
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    var mfile=coloc_getModelConfigFile();
    //
    if (model_config[mfile] !== undefined) {
	var indexTarget=model_config[mfile]["indexTarget"];
	var indexVariable=model_config[mfile]["indexVariable"];
	var indexMin=coloc_config[file]["modelConfigFile"]["min"];
	var indexMax=coloc_config[file]["modelConfigFile"]["max"];
	var color="green";
	var variables=model_config[mfile]["variables"];
	if (variables !== undefined) {
	    if (variables[indexVariable] === undefined) {
		color="red";
	    };
	} else {
	    color="black";
	};
	coloc_insertModelTargetIndexRow(tail,indexTarget,indexVariable,color,indexMin,indexMax);
	//for (var target in targets) {
	if (model_config[mfile] !== undefined) {
	    var dimensions=model_config[mfile]["dimensions"];
	} else {
	    var dimensions;
	}
	for (var ii =0; ii< targeto.length;ii++) {
	    var target=targeto[ii];
	    var variable = targets[target]["variable"];
	    color="green";
	    var len=variable.length;
	    //console.log("*** Check:",variable);
	    if (variable.substring(0,1) == "(" && 
		variable.substring(len-1,len) == ")") { // dimension
		    var dim =variable.substring(1,len-1);
		//console.log("*** Dimension:",dim);
		if (dimensions !== undefined) {
		    if (dimensions[dim] === undefined) {
			color="red";
		    };
		} else {
		    color="black";
		};
	    } else {                                    // variable
		if (variables !== undefined) {
		    if (variables[variable] === undefined) {
			color="red";
		    };
		} else {
		    color="black";
		};
	    }
	    console.log("*** Target:",target,targets[target]["variable"],color,variables[target]);
	    coloc_insertModelTargetRow(tail,target,targets[target]["variable"],color,
				       targets[target]["min"],targets[target]["max"]);
	}
    };
};
// create auto table row
function coloc_insertModelTargetIndexRow(item,target,variable,color,min,max) {
    var row = document.createElement("TR");
    var td, inp;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    //
    // make target name column
    td=document.createElement("TD");
    td.setAttribute("style","color:darkorange");
    td.innerHTML=target;
    row.appendChild(td);
    //
    // make model variable column
    td=document.createElement("TD");
    td.setAttribute("colspan","2");
    var tb=document.createElement("TABLE");
    tb.setAttribute("style","width:100%");
    var trow=document.createElement("TR");
    var ttd=document.createElement("TD");
    ttd.setAttribute("style","color:"+color);
    ttd.innerHTML=variable;
    trow.appendChild(ttd);
    // button <-
    var ttd=document.createElement("TD");
    ttd.setAttribute("style","align:right;min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","coloc_getModelIndexStart('colocModelIndexStart','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("←"); // "→"
    btn.appendChild(t);
    ttd.appendChild(btn);
    trow.appendChild(ttd);
    // button ->
    var ttd=document.createElement("TD");
    ttd.setAttribute("style","align:right;min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","coloc_getModelIndexStop('colocModelIndexStop','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("→"); // "←"
    btn.appendChild(t);
    ttd.appendChild(btn);
    trow.appendChild(ttd);
    tb.appendChild(trow);
    td.appendChild(tb);
    row.appendChild(td);
    // make minimum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    inp.setAttribute("id","colocModelIndexStart");
    inp.setAttribute("style","width:150px");
    inp.setAttribute("onblur","coloc_setConfig('modelConfigFile','min',this.value);coloc_showModelTargetTable();");
    td.appendChild(inp);

    row.appendChild(td);
    // make maximum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT"); // 
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("id","colocModelIndexStop");
    inp.setAttribute("style","width:150px");
    inp.setAttribute("onblur","coloc_setConfig('modelConfigFile','max',this.value);coloc_showModeltargetTable();");
    td.appendChild(inp);
    row.appendChild(td);
    // make add row to table
    if (item !== undefined) {
	item.parentNode.insertBefore(row,item);
    } else {
	console.log("coloc Undefined item.",target,variable,min,max);
    };
    return row;
}
// create auto table row
function coloc_insertModelTargetRow(item,target,variable,color,min,max) {
    var row = document.createElement("TR");
    var file = coloc_getModelConfigFile();
    console.log("coloc: Adding model target row for :",file,target,variable);
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
    td.setAttribute("style","");
    td.innerHTML=target;
    row.appendChild(td);
    // make model variable column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",variable);
    inp.setAttribute("style","width:100%;color:"+color);
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
    inp.setAttribute("style","width:150px");
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','min',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make maximum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT"); // 
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("style","width:150px");
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);");
    td.appendChild(inp);
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
    var mfile=coloc_getModelConfigFile();
    if (model_config[mfile] !== undefined) {
	var indexTarget=model_config[mfile]["indexTarget"];
    } else {
	var indexTarget="";
    }
    td=document.createElement("TD");
    bf=document.createElement("BF");
    bf.innerHTML=indexTarget;
    td.appendChild(bf);
    row.appendChild(td);
    var targeto=coloc_config[file]["modelConfigFile"]["targeto"];
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
//    for (var target in targets) {
    for (var ii =0; ii< targeto.length;ii++) {
	var target=targeto[ii];
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
    var mfile=coloc_getModelConfigFile();
    if (model_config[mfile] !== undefined) {
	var indexTarget=model_config[mfile]["indexTarget"];
    } else {
	var indexTarget="";
    }
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
	    // insert index column
	    td=document.createElement("TD");
	    td.setAttribute("class","fill");
	    td.setAttribute("trg",indexTarget);
	    inp=document.createElement("INPUT");
	    inp.setAttribute("type","text");
	    inp.setAttribute("value",(coloc_config[file]["modelConfigFile"]["def"][ii]["targets"][indexTarget]||""));
	    inp.setAttribute("style","width:100%");
	    inp.setAttribute("onblur","coloc_setConfigFilesDefault("+ii+",'"+indexTarget+"',this.value);");
	    td.appendChild(inp);
	    row.appendChild(td);
	    // insert targets
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
    var mfile=coloc_getModelConfigFile();
    if (model_config[mfile] !== undefined) {
	var indexTarget=model_config[mfile]["indexTarget"];
    } else {
	var indexTarget="";
    }
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","coloc_newModelDefault(this)");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("+");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // insert index
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    td.setAttribute("id","_"+indexTarget);
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value","");
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","this.value=this.value.replace(/[^\\d\\.]/g,'')");
    td.appendChild(inp);
    row.appendChild(td);
    // insert targets
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
	var bufr=obs_config[ofile]["bufr"];
	var bufrType=obs_config[ofile]["bufrType"];
	var subType =obs_config[ofile]["subType"];
	var otargeto=obs_config[ofile]["targeto"];
	var otargets=obs_config[ofile]["targets"];
	// for (var target in otargets) {
	for (var ii =0; ii< otargeto.length;ii++) {
	    var target=otargeto[ii];
	    var pos = otargets[target]["pos"];
	    var color="green";
	    if (bufr !== undefined && 
		bufr[bufrType] !== undefined && 
		bufr[bufrType][subType] !== undefined &&
		bufr[bufrType][subType][pos] !== undefined) {
		var descr=bufr[bufrType][subType][pos]["descr"];
		if (descr!=otargets[target]["descr"]) {
			    color="red";
		};
	    } else {
		color="black";
	    };
	    coloc_insertOTargetRow(tail,target,otargets[target]["pos"],
				   otargets[target]["descr"],color,otargets[target]["info"]);
	}
    };
    // make index row
    if (obs_config[ofile] !== undefined) {
	var target=obs_config[ofile]["indexTarget"];
	var exp = obs_config[ofile]["indexExp"];
    } else {
	var target="";
	var exp="";
    }
    var min = coloc_config[file]["obsConfigFile"]["min"];
    var max = coloc_config[file]["obsConfigFile"]["max"];
    coloc_insertOTargetIndexRow(tail,target,exp,min,max);
    // insert obs targets from coloc-config file
    var targeto=coloc_config[file]["obsConfigFile"]["targeto"];
    var targets=coloc_config[file]["obsConfigFile"]["targets"];
    //for (var target in targets) {
    for (var ii =0; ii< targeto.length;ii++) {
	var target=targeto[ii];
	var pos = targets[target]["pos"];
	var color="green";
	if (obs_config[ofile] !== undefined) {
	    var bufr=obs_config[ofile]["bufr"];
	    var bufrType=obs_config[ofile]["bufrType"];
	    var subType =obs_config[ofile]["subType"];
	    if (bufr !== undefined &&
		bufr[bufrType] !== undefined &&
		bufr[bufrType][subType] !== undefined &&
		bufr[bufrType][subType][pos] !== undefined) {
		var descr=bufr[bufrType][subType][pos]["descr"];
		//console.log("*** Table ",pos,descr,targets[target]["descr"],color)
		if (descr!=targets[target]["descr"]) {
		    color="red";
		};
	    } else {
		color="black";
	    };
	} else {
	    color="black";
	}
	coloc_insertObsTargetRow(tail,target,targets[target]["pos"],targets[target]["descr"],color,
				 targets[target]["info"],targets[target]["min"],targets[target]["max"]);
    }
};
// create obs index target table row
function coloc_insertOTargetIndexRow(item,target,exp,min,max) {
    // insert obs target index expression from obs-config file
    // make "-" column  ***************************
    var row = document.createElement("TR");
    var td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    //
    // make target NAME column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","color:blue");
    td.innerHTML=target;
    row.appendChild(td);
    //
    // make expression column  ***************************
    td=document.createElement("TD");
    td.setAttribute("colspan","4");
    var tb=document.createElement("TABLE");
    tb.setAttribute("style","width:100%");
    var trow=document.createElement("TR");
    var ttd=document.createElement("TD");
    ttd.innerHTML=exp
    trow.appendChild(ttd);
    // button <-
    var ttd=document.createElement("TD");
    ttd.setAttribute("style","align:right;min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","coloc_getObsIndexStart('colocObsIndexStart','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("←"); // "→"
    btn.appendChild(t);
    ttd.appendChild(btn);
    trow.appendChild(ttd);
    // button ->
    var ttd=document.createElement("TD");
    ttd.setAttribute("style","align:right;min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","coloc_getObsIndexStop('colocObsIndexStop','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("→"); // "←"
    btn.appendChild(t);
    ttd.appendChild(btn);
    trow.appendChild(ttd);
    tb.appendChild(trow);
    td.appendChild(tb);
    row.appendChild(td);
    //
    // make minimum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    inp.setAttribute("id","colocObsIndexStart");
    inp.setAttribute("style","width:150px");
    inp.setAttribute("onblur","coloc_setArrayPar('obsConfigFile','min',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make maximum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("id","colocObsIndexStop");
    inp.setAttribute("style","width:150px");
    inp.setAttribute("onblur","coloc_setArrayPar('obsConfigFile','max',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    item.parentNode.insertBefore(row,item);
}
// create auto table row
function coloc_insertOTargetRow(item,target,pos,descr,color,info) {
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
    if (color !== "") {
	td.setAttribute("style","color:"+color);
    }
    row.appendChild(td);
    // make info column  ***************************
    td=document.createElement("TD");
    td.setAttribute("colspan","3");
    td.innerHTML=info;
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
// create auto table row
function coloc_insertObsTargetRow(item,target,pos,descr,color,info,min,max) {
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
    if (color !== "") {
	inp.setAttribute("style","width:100%;color:"+color);
    } else {
	inp.setAttribute("style","width:100%");
    }
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
    var mfile=coloc_getModelConfigFile();
    var tail=removeTableChildFromTo(item,"labelsTargetMatch","newlineTargetMatch");
    var targeto=coloc_config[file]["modelConfigFile"]["targeto"];
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    // insert index match
    var cnt=0;
    if (model_config[mfile]!== undefined) {
	var indexTarget=model_config[mfile]["indexTarget"];
    } else {
	var indexTarget="";
    }
    var indexExp=coloc_config[file]["modelConfigFile"]["exp"];;
    coloc_insertTargetMatchRow(tail,cnt,indexTarget,indexExp);
    cnt=cnt+1;
    //for (var target in targets) {
    for (var ii =0; ii< targeto.length;ii++) {
	var target=targeto[ii];
	console.log("TargetS:",target);
	cnt=cnt+1;
	var exp=(targets[target]["exp"]||"");
	coloc_insertTargetMatchRow(tail,cnt,target,exp);
    };
    for (var target of targeto) {
	console.log("TargetO:",target);
    }
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
	if (model_config[mfile]!== undefined) {
	    var indexTarget=model_config[mfile]["indexTarget"];
	} else {
	    var indexTarget="";
	}
	if (target == indexTarget) {
            td.setAttribute("style","color:darkorange");
	}
        td.innerHTML=target;
        row.appendChild(td);
        // make obs target expression column  ***************************
	itemId="matchExpression"+cnt;
        td=document.createElement("TD");
        td.setAttribute("class","fill");
        inp=document.createElement("INPUT");
	inp.setAttribute("id",itemId);
        inp.setAttribute("type","text");
        inp.setAttribute("value",expr);
        inp.setAttribute("style","width:100%");
	if (target == indexTarget) {
            inp.setAttribute("onblur","coloc_setConfig('modelConfigFile','exp',this.value);coloc_showTargetMatchTable();");
	} else {
            inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','exp',this.value);coloc_showTargetMatchTable();");
	}
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
	(document.getElementById("displayColocModelFilter")).setAttribute("style","");
    } else {
	(document.getElementById("displayModelTargets")).setAttribute("style","display:none");
	(document.getElementById("displayModelDefault")).setAttribute("style","display:none");
	(document.getElementById("displayColocModelFilter")).setAttribute("style","display:none");
    };
    if (obs) {
	coloc_showObsTargetTable();
	(document.getElementById("displayObsTargets")).setAttribute("style","");
	(document.getElementById("displayColocObsFilter")).setAttribute("style","");
    } else {
	(document.getElementById("displayObsTargets")).setAttribute("style","display:none");
	(document.getElementById("displayColocObsFilter")).setAttribute("style","display:none");
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
    showValue('colocConfigFile',file);
    showValue('colocConfigFileSave',file);
    showValue('colocModelConfigFile',coloc_config[file]["modelConfigFile"]["file"]);
    showValue('colocObsConfigFile',coloc_config[file]["obsConfigFile"]["file"]);
    showValue('colocXML',coloc_config[file]["xml"]);
    showValue('colocModelFilter',coloc_config[file]["filter"]);
    showValue('colocObsFilter',coloc_config[file]["obsConfigFile"]["filter"]);
    coloc_showCOLOC();
}

function coloc_showCOLOC() {
    var file=coloc_getConfigFile();
    var host=coloc_config[file]["host"];
    var href="http://"+host+"/cgi-bin/fark_coloc.pl?colocFile="+file;
    // if (mod) {
    // 	href=href+"?modelFile="+coloc_config[file]["modelConfigFile"]["file"];
    // 	if (coloc_config[file]["modelConfigFile"]["min"]) {
    // 	    href=href+"?modelStart="+ coloc_config[file]["modelConfigFile"]["min"];
    // 	};
    // 	if (coloc_config[file]["modelConfigFile"]["max"]) {
    // 	    href=href+"?modelStop="+ coloc_config[file]["modelConfigFile"]["max"];
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
    // 	if (coloc_config[file]["obsConfigFile"]["min"]) {
    // 	    href=href+"?obsStart="+ coloc_config[file]["obsConfigFile"]["min"];
    // 	};
    // 	if (coloc_config[file]["obsConfigFile"]["max"]) {
    // 	    href=href+"?obsStop="+ coloc_config[file]["obsConfigFile"]["max"];
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
	//arr=coloc_removeByValue(arr,item1,item2...)
	coloc_config[file]["modelConfigFile"]["targeto"]=
	    coloc_removeByValue(coloc_config[file]["modelConfigFile"]["targeto"],target);
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
	coloc_config[file]["obsConfigFile"]["targeto"]=
	    coloc_removeByValue(coloc_config[file]["obsConfigFile"]["targeto"],target);
	//coloc_showObsTargetTable();
	coloc_show();
    }
};


function coloc_saveConfigFile(target) {
    var file=coloc_getConfigFile();
    var password=document.getElementById("colocConfigFilePsw").value;
    var host = coloc_config[file]["host"];
    var xml = coloc_config[file]["xml"];
    var modelFilter = coloc_config[file]["filter"];
    var modelFile = coloc_config[file]["modelConfigFile"]["file"];
    var modelStart = coloc_config[file]["modelConfigFile"]["min"];
    var modelStop = coloc_config[file]["modelConfigFile"]["max"];
    var exp = coloc_config[file]["modelConfigFile"]["exp"];
    var modelTargets = "";
    var modelTrgo=coloc_config[file]["modelConfigFile"]["targeto"];
    var modelTrg=coloc_config[file]["modelConfigFile"]["targets"];
    //for (var target in modelTrg) {
    for (var ii =0; ii< modelTrgo.length;ii++) {
	var target=modelTrgo[ii];
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
    var obsFilter = coloc_config[file]["obsConfigFile"]["filter"];
    var obsStart = coloc_config[file]["obsConfigFile"]["min"];
    var obsStop = coloc_config[file]["obsConfigFile"]["max"];
    var obsTargets = "";
    var obsTrgo=coloc_config[file]["obsConfigFile"]["targeto"];
    var obsTrg=coloc_config[file]["obsConfigFile"]["targets"];
    //for (var target in obsTrg) {
    for (var ii =0; ii< obsTrgo.length;ii++) {
	var target=obsTrgo[ii];
	obsTargets=obsTargets + "|" + target + "~" + 
	    obsTrg[target]["pos"] + "~" + 
	    obsTrg[target]["descr"] + "~" + 
	    obsTrg[target]["info"] + "~" + 
	    obsTrg[target]["min"] + "~" + 
	    obsTrg[target]["max"];
    };
    var indexTarget=model_config[modelFile]["indexTarget"];
    var indexExp=coloc_config[file]["modelConfigFile"]["exp"];;
    var matchRules = "|" + indexTarget + "~" + indexExp;
    var matchTrg=coloc_config[file]["modelConfigFile"]["targets"];
    for (var target in matchTrg) {
	matchRules=matchRules + "|" + target + "~" + 
	    matchTrg[target]["exp"];
    };
    documentLog.innerHTML="Sent coloc-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"coloc",
				  file:file,
				  host:host,
				  xml:xml,
				  filter:modelFilter,
				  password:password,
				  modelFile:modelFile,
				  modelStart:modelStart,
				  modelStop:modelStop,
				  indexExp:exp,
				  obsFile:obsFile,
				  obsFilter:obsFilter,
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
	coloc_config[file]={modelConfigFile:{targets:{},targeto:[],def:{}},
			    obsConfigFile:{targets:{},targeto:[]},
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
		if (children[ii].getAttribute !== undefined && expr !== undefined) {
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
	showValue('colocModelConfigFile',coloc_config[file]["modelConfigFile"]["file"]);
	showValue('colocObsConfigFile',coloc_config[file]["obsConfigFile"]["file"]);
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
	    fark_last['model']=coloc_getModelConfigFile();
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
	fark_last['obs']=coloc_getObsConfigFile();
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
	    fark_last['model']=coloc_getModelConfigFile();
	    args=getArgs(coloc_getModelConfigFile());
	    console.log("coloc: *****loading model ",args);
	    $.get("cgi-bin/fark_load.pl",{type:"model",arg:args},function(data, status){
		dataToArray(data,status,documentLog);
		modelLoaded=true;
		fark_last['obs']=coloc_getObsConfigFile();
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
    coloc_setConfig('modelConfigFile','min',item.value);
};
function coloc_getModelIndexStop(inp,target) {
    var file=coloc_getModelConfigFile();
    var item=document.getElementById(inp);
    item.value=Number(model_config[file]["stop"]).toString();
    coloc_setConfig('modelConfigFile','max',item.value);
};
function coloc_getObsIndexStart(inp,target) {
    var file=coloc_getObsConfigFile();
    var item=document.getElementById(inp);
    console.log("fark.js start:'"+file+"' '"+obs_config[file]["start"]+"'")
    item.value=Number(obs_config[file]["start"]).toString();
    coloc_setArrayPar('obsConfigFile','min',item.value);
};
function coloc_getObsIndexStop(inp,target) {
    var file=coloc_getObsConfigFile();
    var item=document.getElementById(inp);
    item.value=Number(obs_config[file]["stop"]).toString();
    coloc_setArrayPar('obsConfigFile','max',item.value);
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
    titem.innerHTML="";
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


// arr=obs_removeByValue(arr,item1,item2...)
function coloc_removeByValue(arr) {
    var what, a = arguments, ll = a.length, ax;
    while (ll > 1 && arr.length) {
        what = a[--ll];
        while ((ax= arr.indexOf(what)) !== -1) {
            arr.splice(ax, 1);
        }
    }
    return arr;
};
