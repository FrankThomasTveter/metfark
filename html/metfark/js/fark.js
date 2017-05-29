documentLog = document.getElementById("log");
dropdownEd = {};	
modelLoaded=false;
obsLoaded=false;

// initialisation function
function onload(){
    var types=["model","obs","scan","coloc"];
    var url=getUrlVars();
    var type=decodeURIComponent(url["type"] || "model");
    var file=decodeURIComponent(url["file"] || "default.cfg");
    if (! types.includes(type)) { 
	type="model"; 
	file="default.cfg";
    };
    onload_setActive(type);
    onload_setConfigFile(type,file);
    onload_updateData(type);
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


function onload_setActive(type) {
    var types=["model","obs","scan","coloc"];
    var len=types.length;
    for (var ii=0;ii<len;ii++){
	var item_tab=document.getElementById(types[ii]+"_tab");
	var item=document.getElementById(types[ii]);
	if (types[ii] === type) {
	    item_tab.setAttribute("class","active");
	    item.setAttribute("class","tab-pane fade in active");
	} else {
	    item_tab.setAttribute("class","");
	    item.setAttribute("class","tab-pane fade in");
	};
    };
}

function onload_setConfigFile(type,file) {
    if (type === "model") {
	model_setConfigFile(file);
    } else if (type === "obs") {
	obs_setConfigFile(file);
    } else if (type === "scan") {
	//scan_setConfigFile(file);
    } else if (type === "coloc") {
	coloc_setConfigFile(file);
    };
};
function onload_updateData(type){
    if (type === "model") {
	model_updateData();
    } else if (type === "obs") {
	obs_updateData();
    } else if (type === "scan") {
	scan_updateData();
    } else if (type === "coloc") {
	coloc_updateData();
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

function showDropdown(target) {
    var dropdown=target + 'Dropdown';
    var item=document.getElementById(dropdown);
    if (target === 'modelConfigFile') {
	documentLog.innerHTML="Sent model-load request.";
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
    } else if (target === 'scanModelConfigFile') {
	documentLog.innerHTML="Sent obs-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"model"},function(data, status){
	    dataToArray(data,status,documentLog);
	    if (dropdownEd[target] === undefined || 
		dropdownEd[target] < scan_configEd) {
		removeChildren(item);
		for (var model in model_config) {
		    if (scan_config["model"][model] !== undefined) {
			addChildButton(item,model,
				       "setValue('scanModelConfigFile','"      +model+"');"+
				       "setInnerHTML('scanModelLastScan','"+scan_config["model"][model]["lastScan"]+"');"+
				       "setInnerHTML('scanModelLastUsed','"+scan_config["model"][model]["lastUsed"]+"');");
		    } else {
			addChildButton(item,model,
				       "setValue('scanModelConfigFile','"      +model+"');");
		    }
		}
		dropdownEd[target]=scan_configEd;
	    }
	    documentLog.innerHTML="";
	});
    } else if (target === 'scanObsConfigFile') {
	documentLog.innerHTML="Sent obs-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"obs"},function(data, status){
	    dataToArray(data,status,documentLog);
	    removeChildren(item);
	    for (var obs in obs_config) {
		if (scan_config["obs"][obs] !== undefined) {
		    addChildButton(item,obs,
				   "setValue('scanObsConfigFile','"      +obs+"');"+
				   "setInnerHTML('scanObsLastScan','"+scan_config["obs"][obs]["lastScan"]+"');"+
				   "setInnerHTML('scanObsLastUsed','"+scan_config["obs"][obs]["lastUsed"]+"');");
		} else {
		    addChildButton(item,obs,
				   "setValue('scanObsConfigFile','"      +obs+"');");
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
	dataToScan(data);
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

function dataToScan(data) {
    // <scan_config name="test" fileFilter="*"> <variable name="var1"> </scan_config>
    var scans=data.getElementsByTagName("scan_config");
    for (var ii = 0; ii < scans.length; ii++) {
	scan_config["model"]={};
	var models=scans[ii].getElementsByTagName("model");
	for (var jj = 0; jj < models.length; jj++) {
	    var model=models[jj].getAttribute("file");
	    var lastScan=models[jj].getAttribute("lastScan");
	    var lastUsed=models[jj].getAttribute("lastUsed");
	    var status=models[jj].getAttribute("status")||"";
	    scan_config["model"][model]={lastScan:lastScan, lastUsed:lastUsed, status:status};
	};
	scan_config["obs"]={};
	var obses=scans[ii].getElementsByTagName("obs");
	for (var jj = 0; jj < obses.length; jj++) {
	    var obs=obses[jj].getAttribute("file");
	    var lastScan=obses[jj].getAttribute("lastScan");
	    var lastUsed=obses[jj].getAttribute("lastUsed");
	    var status=obses[jj].getAttribute("status")||"";
	    scan_config["obs"][obs]={lastScan:lastScan, lastUsed:lastUsed, status:status};
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
