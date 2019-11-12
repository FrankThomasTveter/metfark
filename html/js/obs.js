obs_file = "default.cfg";
obs_config = { "default.cfg" : { filterDir : "/home/www/bufr/",
				 filterDirStat : "",
				 filterDirMin: "",
				 filterDirMax: "",
				 filterFile : ".*\.bufr",
				 tablePath : "/home/www/fark-perl_0.15/bufrtables/",
				 hits : "?",
				 bufr : { 99 : { info : "default", 9 : { 1 :{descr:99999, info: "default info"},
									 info : "more default info"
								       }
					       }
					},
				 bufrType : "99",
				 subType : "9",
				 typeInfo : "default info.",
				 targets : {"yy" : {pos:"", descr:"", info:""}},
				 targeto : ["yy"],
				 indexTarget : "time",
				 indexExp : "sec1970(yy,mm,dd,hh,mi)",
				 files : [],
				 stack : "",
				 password: "test"
			       }
	     };
obs_configEd = 0;

function obs_allocate(file) {
    if (obs_config[file] === undefined) {
	obs_config[file]=clone(obs_config[obs_file]);
	//console.log("cloned:",file,obs_file,obs_config[file]);
    }
}
function obs_setConfigFile(file) {
    showValue('obsConfigFile',file);
    showValue('obsConfigFileSave',file);
    //if (file != "") {
    obs_allocate(file);
    obs_file=file;
    //};
}
function obs_getConfigFile() {
    return obs_file;
};
function obs_setArray(parameter,value) {
    var file=obs_getConfigFile();
    //console.log("File:",file,parameter,obs_config[file]);
    obs_config[file][parameter]=decodeURI(value);
};
function obs_setFilterDir(value) {
    var file=obs_getConfigFile();
    //console.log("File:",file,"filterDir",obs_config[file]);
    var val=decodeURI(value);
    obs_config[file]["filterDir"]=val;
    $.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:"data",path:val})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length == 0 ) {
		    document.getElementById('obsFilterDir').style.color='black'
		    obs_config[file]["filterDirStat"]="";
		    //console.log("Dir ok:",val);
		} else {
		    obs_config[file]["filterDirStat"]=val;
		    document.getElementById('obsFilterDir').style.color='red'
		    console.log("Dir NOT ok:",val);
		}
		obs_show();
	    })
	.error(
	    function (error) { alert("Model filter dir request failed (system error)");}
	);
};
function obs_setIndexTarget(target,parameter,value) {
    var file=obs_getConfigFile();
    obs_config[file]["targets"][target][parameter]=value;
};
function obs_show() {
    var file=obs_getConfigFile();
    if (file != "") {
	obs_allocate(file);
	showValue('obsConfigFile',file);
	showValue('obsConfigFileSave',file);
	showValue('obsFilterDir',obs_config[file]["filterDir"]);
	showValue('obsFilterDirMin',obs_config[file]["filterDirMin"]);
	showValue('obsFilterDirMax',obs_config[file]["filterDirMax"]);
	showValue('obsFilterFile',obs_config[file]["filterFile"]);
	showValue('obsTablePath',obs_config[file]["tablePath"]);
	showValue('obsBufrType',obs_config[file]["bufrType"]);
	showValue('obsSubType',obs_config[file]["subType"]);
	showValue('obsTypeInfo',obs_config[file]["typeInfo"]);
	obs_setIndexTargetTable(file);
	showValue('obsIndexTarget',obs_config[file]["indexTarget"]);
	showValue('obsIndexExp',obs_config[file]["indexExp"]);
	setInnerHTML('obsPatternHits',obs_config[file]["hits"]);
	// this may seem strange, Stat stores name of dir only if it does not exist...
	if (obs_config[file]["filterDirStat"]==obs_config[file]["filterDir"]) {
	    document.getElementById('obsFilterDir').style.color='red'
	    console.log("Directory does not exist:",obs_config[file]["filterDir"]);
	} else {
	    document.getElementById('obsFilterDir').style.color='black'
	}
    };
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
    item.children[0].children[0].value=target;
    item.children[1].children[0].value=obs_config[file]["targets"][target]["pos"];
    item.children[3].children[0].value=obs_config[file]["targets"][target]["descr"];
    item.children[4].children[0].value=obs_config[file]["targets"][target]["info"];
    obs_config[file]["targeto"]=obs_removeByValue(obs_config[file]["targeto"],target)
    delete obs_config[file]["targets"][target];
    obs_setIndexTargetTable(file);
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
    var target=item.parentNode.parentNode.children[0].children[0].value;
    var pos=item.parentNode.parentNode.children[1].children[0].value;
    var descr=item.parentNode.parentNode.children[3].children[0].value;
    var info=item.parentNode.parentNode.children[4].children[0].value;
    //    console.log("New: trg:",target," pos:",pos," des:",descr," info:",info);
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
	    obs_config[file]["targeto"].push(target);
	};
	obs_config[file]["targets"][target]["pos"]=pos;
	obs_config[file]["targets"][target]["descr"]=descr;
	obs_config[file]["targets"][target]["info"]=info;
	obs_setIndexTargetTable(file);
	item.parentNode.parentNode.children[0].children[0].value="";
	item.parentNode.parentNode.children[1].children[0].value="";
	item.parentNode.parentNode.children[3].children[0].value="";
	item.parentNode.parentNode.children[4].children[0].value="";
    } else {
	alert("Invalid observation target name: ('"+target+"')");
    }
};
function obs_saveConfigFile() {
    var file=obs_getConfigFile();
    var password=document.getElementById("obsConfigFilePsw").value;
    var filterDir="";
    var filterDirMin="";
    var filterDirMax="";
    var filterFile="";
    var table="";
    var bufrType="";
    var subType="";
    var typeInfo="";
    var indexTarget="";
    var indexExp="";
    var stack="";
    var obsTargets="";
    if (obs_config[file]!= undefined) {
	filterDir=obs_config[file]["filterDir"]||"";
	filterDirMin=obs_config[file]["filterDirMin"]||"";
	filterDirMax=obs_config[file]["filterDirMax"]||"";
	filterFile=obs_config[file]["filterFile"]||"";
	var sfile=obs_config[file]["stack"];
	if (sfile !== "") {
	    stack=stack+"|"+sfile;
	};
	bufrType=obs_config[file]["bufrType"]||"";
	subType=obs_config[file]["subType"]||"";
	typeInfo=obs_config[file]["typeInfo"]||"";
	indexTarget=obs_config[file]["indexTarget"]||"";
	indexExp=obs_config[file]["indexExp"]||"";
	var targeto=obs_config[file]["targeto"];
	var targets=obs_config[file]["targets"];
	//for (var target in targets) {
	for (var ii =0; ii< targeto.length;ii++) {
	    var target=targeto[ii];
	    var pos=targets[target]["pos"];
	    var descr=targets[target]["descr"];
	    var info=targets[target]["info"];
	    obsTargets=obsTargets + "|" + target + "~" + pos + "~" + descr + "~" + info;
	};
	table=obs_config[file]["tablePath"]||"";
    }
    if (obsTargets == "") {obsTargets="none";}
    obs_configEd++;
    documentLog.innerHTML="Sent obs-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"obs",file:file,password:password,
				  filterDir:filterDir,
				  filterDirMin:filterDirMin,
				  filterDirMax:filterDirMax,
				  filterFile:filterFile,
				  stack:stack,
				  table:table,
				  bufrType:bufrType,
				  subType:subType,
				  typeInfo:typeInfo,
				  indexTarget:indexTarget,
				  indexExp:indexExp,
				  obsTargets:obsTargets
				 })
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to save file: "+file+"\n"+msg);
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Obs save request failed (system error)");}
	);
    makeUrl("obs",file);
};
// make new obs-index entry
function obs_setIndexTargetTable(file) {
    var file=obs_getConfigFile();
    var bufrType = obs_config[file]["bufrType"];
    var subType = obs_config[file]["subType"];
    var item=document.getElementById('obsIndexTargetTable');
    var tail=removeTableChildFromTo(item,"labelsObsIndexTarget","newlineObsIndexTarget");
    var targeto=obs_config[file]["targeto"];
    var targets=obs_config[file]["targets"];
    //
    var bufr=obs_config[file]["bufr"];
    //for (var target in value) {
    for (var ii =0; ii< targeto.length;ii++) {
	var target=targeto[ii];
	var color="green";
	var pos = targets[target]["pos"];
	if (pos !== undefined &&
	    bufr !== undefined && 
	    bufr[bufrType] !== undefined && 
	    bufr[bufrType][subType] !== undefined && 
	    bufr[bufrType][subType][pos] !== undefined) {
	    var descr=bufr[bufrType][subType][pos]["descr"];
	    if (descr!=targets[target]["descr"]) {
		color="red";
	    };
	};
	obs_insertIndexTargetRow(tail,target,ii,
				 targets[target]["pos"],
				 targets[target]["descr"],color,
				 targets[target]["info"]);
    }
}
function obs_insertIndexTargetRow(item,target,ii,pos,descr,color,info) {
    var row = document.createElement("TR");
    var td,inp;
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
    //inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','pos',this.value);");
    inp.setAttribute("title","Position in BUFR sequence");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    td.setAttribute("align","center");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("title","Move target up");
    btn.setAttribute("onclick","obs_targetUp('"+ii+"');obs_show();");
    //btn.setAttribute("style","width:100%");
    //var t=document.createTextNode("--");
    //btn.appendChild(t);
    btn.innerHTML="&uarr;";
    //btn.setAttribute("align","center");
    td.appendChild(btn);
    row.appendChild(td);
    // make descr column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",descr);
    inp.setAttribute("title","BUFR descriptor");
    if (color !== "") {
	inp.setAttribute("style","color:"+color);
    }
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','descr',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make info column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",info);
    inp.setAttribute("title","Information");
    //inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','info',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    td.setAttribute("align","center");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","obs_removeTarget('"+target+"')");
    btn.setAttribute("style","width:100%");
    btn.setAttribute("title","Remove observation target");
    //var t=document.createTextNode("--");
    //btn.appendChild(t);
    btn.innerHTML="&#45";
    //btn.setAttribute("align","center");
    td.appendChild(btn);
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
function obs_updateData(arg=obs_getConfigFile()) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent obs-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"obs",arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		obsLoaded=true;
		//console.log("Updating dropdown for ",target);
		obs_show();
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Obs request failed (system error)");}
	);
};
function obs_fileFind(sfile) {
    var file=obs_getConfigFile();
    obs_config[file]["stack"]=sfile;
    var password=document.getElementById("obsConfigFilePsw").value;
    var filterDir = obs_config[file]["filterDir"];
    var filterFile = obs_config[file]["filterFile"];
    var table = obs_config[file]["tablePath"];
    var obsTargets = "";
    var obsTrg=obs_config[file]["targets"];
    for (var target in obsTrg) {
	obsTargets=obsTargets + "|" + target + "~" + 
	    obsTrg[target]["pos"] + "~" + 
	    obsTrg[target]["descr"] + "~" + 
	    obsTrg[target]["info"];
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
				  filterDir:obs_config[file]["filterDir"],
				  filterFile:obs_config[file]["filterFile"],
				  table:obs_config[file]["tablePath"],
				  obsTargets:obsTargets,
				  indexTarget:indexTarget,
				  indexExp:indexExp,
				  bufrType:bufrType,
				  subType:subType,
				  typeInfo:typeInfo})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to scan file: "+sfile+" (file:"+file+")\n"+msg);
		    } else {
			dataToArray(data,status,documentLog);
			obs_show();
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Obs find request failed (system error)");}
	);
};

function obs_mkdir(path) {
    var password=document.getElementById("obsConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"obs",
				 path:path,
				 password,password})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to mkdir: "+path+"\n"+msg);
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Obs mkdir request failed (system error)");}
	);
    
};

function obs_rmfile(path) {
    var password=document.getElementById("obsConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"obs",
				 path:path,
				 password,password})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to rmfile: "+path+"\n"+msg);
		    } else {
			//delete obs_config[path];
			if (obs_file == path) {obs_file="default.cfg";}
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Obs rmfile request failed (system error)");}
	);
    
};

function obs_rmdir(path) {
    var password=document.getElementById("obsConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"obs",
				 path:path,
				 password,password})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to rmdir: "+path+"\n"+msg);
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Obs rmdir request failed (system error)");}
	);
    
};

function obs_mkfile(file) {
    //console.log("Calling saveConfigFile: '"+file+"'");
    obs_setConfigFile(file);
    obs_saveConfigFile(file);
};

function obs_fgfile(path) { // clear file from internal memory
    if (obs_config[path] != undefined) {
	delete obs_config[path];
    }
};

// arr=obs_removeByValue(arr,item1,item2...)
function obs_removeByValue(arr) {
    var what, a = arguments, ll = a.length, ax;
    while (ll > 1 && arr.length) {
        what = a[--ll];
        while ((ax= arr.indexOf(what)) !== -1) {
            arr.splice(ax, 1);
        }
    }
    return arr;
};

function obs_targetUp(ii) {
    var file=obs_getConfigFile();
    if (obs_config[file] !== undefined && 
	obs_config[file]["targeto"] !== undefined) {
	var targeto = obs_config[file]["targeto"];
	arrayUp(targeto,ii);
    }
}

function obs_showConfigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent obs-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"obs",arg:args})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+arg+"'\n"+msg);
		} else {
		    var ret=dataToArray(data,status,documentLog);
		    var root=ret[0]||{};
		    //console.log("Updating dropdown for ",target);
		    removeChildren(item);
		    var added=false;
		    if (args.length >0 && looksLikeFile(args[0])) {
			var file=getFile(args[0]);
		    } else {
			var file="";
		    };
		    // add directories...
		    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
		    //console.log("Found entries: ",dirs.length-1,root);
		    var parent=dirs[0];
		    if (parent != null) {
			var dd=parent;
			addChildButton(item,"<up>","obs_setConfigFile('"+dd+"');","Change to parent <directory>");
			added=true;
		    }
		    if (args.length == 1) {
			//console.log("Arg ret:",ret);
			if (root["type"] == "dir" && root["loc"] != "") {
			    addChildButton(item,"<rmdir>","obs_rmdir('"+args[0]+"');","Remove <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<rmfile>","obs_rmfile('"+args[0]+"');","Remove <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			    if (looksLikeFile(args[0])) {
				addChildButton(item,"<mkfile>","obs_mkfile('"+args[0]+"');obs_show();","Make <file>");
				if (obs_config[args[0]] != undefined) {
				    addChildButton(item,"<fgfile>","obs_fgfile('"+args[0]+"');","Forget <file>");
				}
				added=true;
			    } else {
				addChildButton(item,"<mkdir>","obs_mkdir('"+args[0]+"');","Make <directory>");
				added=true;
			    }
			}
		    } else if (args.length == 2) {
			if (root["type"] == "dir") {
			    addChildButton(item,"<cpdir>","obs_cpdir('"+args[0]+"','"+args[1]+"');","Copy <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<cpfile>","obs_cpfile('"+args[0]+"','"+args[1]+"');obs_setConfigFile('"+args[2]+"');obs_show();","Copy <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			}
		    };
		    //for (var obs in obs_config) {
		    //console.log("Adding config button: ",obs);
		    //addChildButton(item,obs,"obs_setConfigFile('"+obs+"');obs_show();");
		    //added=true;
		    //}
		    var configfile=obs_getConfigFile();
		    for (var ii=1;ii<dirs.length;ii++) {
			var dir=dirs[ii];
			if (root["loc"] == "" || root["loc"] == ".") {
			    var dd = dir;
			} else {
			    var dd = root["loc"]+dir;
			};
			//if (dd.substr(dd.length-1) == "/" || dd == "") {
			//dd=dd + file;
			//}
			//console.log("Adding dir button: ",dd);
			if (dd === configfile) {
			    addChildButtonShaded(item,dd,"obs_setConfigFile('"+dd+"');obs_show();","Use <file>");
			    added=true;
			} else if (looksLikeFile(dd)) {
			    addChildButton(item,dd,"obs_setConfigFile('"+dd+"');obs_show();","Use <file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"obs_setConfigFile('"+dd+"');obs_show();","Change <directory>");
			    added=true;
			}
		    }
		    if (! added) {addChildText(item,"No data available...");}
		}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Obs config request failed (system error)");}
	);
    // documentLog.innerHTML="Sent obs-load request.";
    // $.get("cgi-bin/fark_load.pl",{type:"obs",arg:args},function(data, status){
    //     dataToArray(data,status,documentLog);
    //     //console.log("Updating dropdown for ",target);
    //     removeChildren(item);
    //     var added=false;
    //     for (var obs in obs_config) {
    // 	addChildButton(item,obs,"obs_setConfigFile('"+obs+"');obs_show();");
    //      added=true;
    //     };
    //     documentLog.innerHTML="";
    // });
}

function obs_showFilterDir(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent obs-load request.";
    var file=obs_getConfigFile();
    var path=args[0] || "";
    var cls = "data";
    var filter=obs_config[file]["filterFile"];
    $.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:cls,path:path,filter:filter})
	.success(
	    function(data, status){
		removeChildren(item);
		var added=false;
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+path+"'\n"+msg);
		} else {
		    var ls=data.getElementsByTagName("ls");
		    if (ls.length > 0) {
			var root=ls[0].getAttribute("root");
			var loc=ls[0].getAttribute("location");
			var pdirs=getSubDirs(cls,root,loc,"");
			var parent=pdirs[0];
			//console.log("Found parent: ",root,loc,parent);
			if (parent != null) {
			    var dd=root+parent;
			    addChildButton(item,"<up>",
					   "obs_setArray('filterDir','"+dd+"');obs_show();","Change to parent <directory>");
			    added=true;
			};
			var dirs=ls[0].getElementsByTagName("dir");
			//console.log("Found dir entries: ",dirs.length);
			for (var ii=0; ii< dirs.length; ii++) {
			    var dd = dirs[ii].getAttribute("path");
			    //console.log("Adding dir button: ",dd);
			    addChildButton(item,dd,"obs_setArray('filterDir','"+dd+"');obs_show();","Change <directory>");
			    added=true;
			};
			var patts=ls[0].getElementsByTagName("pattern");
			//console.log("Found file entries: ",patts.length);
			for (var ii=0; ii< patts.length; ii++) {
			    var rr = getFile(patts[ii].getAttribute("regexp"));
			    var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
			    if (dd !== '') {
				//console.log("Adding file button: ",dd,rr);
				addChildButtonShaded(item,dd,"obs_setArray('filterFile','"+rr+"');obs_show();","Copy <pattern> to filter");
				added=true;
			    };
			};
			var fils=ls[0].getElementsByTagName("file");
			//console.log("Found file entries: ",fils.length);
			for (var ii=0; ii< fils.length; ii++) {
			    var dd = getFile(fils[ii].getAttribute("path"));
			    var size = fils[ii].getAttribute("size")
			    if (dd !== '') {
				//console.log("Adding file button: ",dd);
				addChildButton(item,size+" "+dd,"obs_setArray('filterFile','"+dd+"');obs_show();","Copy <file name> to filter");
				added=true;
			    };
			};
		    };
		};
		if (! added) {addChildText(item,"No data available...");}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Obs filter request failed (system error)");}
	);
};

function obs_showFilterFile(item,target,arg) {
    var file=obs_getConfigFile();
    var password=document.getElementById("obsConfigFilePsw").value;
    var filterDir = obs_config[file]["filterDir"];
    var filterDirMin = obs_config[file]["filterDirMin"];
    var filterDirMax = obs_config[file]["filterDirMax"];
    var filterFile = obs_config[file]["filterFile"];
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
				  filterDir:filterDir,
				  filterDirMin:filterDirMin,
				  filterDirMax:filterDirMax,
				  filterFile:filterFile,
				  table:obs_config[file]["tablePath"],
				  obsTargets:obsTargets,
				  indexTarget:indexTarget,
				  indexExp:indexExp,
				  bufrType:bufrType,
				  subType:subType,
				  typeInfo:typeInfo})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			item.classList.toggle("show");
			var msg=getErrorMessage(errors);
			alert("Unable to find files at "+filterDir+" (filter:'"+filterFile+"', Setup file:"+file+")\n"+msg);
		    } else {
			dataToArray(data,status,documentLog);
			setInnerHTML('obsPatternHits',obs_config[file]["hits"]);
			removeChildren(item);
			var added=false;
			var len=obs_config[file]["files"].length;
			for (var ii=0; ii<len;ii++) {
			    var sfile=obs_config[file]["files"][ii][0];
			    var sage=parseFloat(obs_config[file]["files"][ii][1]).toFixed(2);
			    var ssize=obs_config[file]["files"][ii][2];
			    addChildButton(item,ssize+" "+sfile+" ("+sage+"d)","obs_fileFind('"+sfile+"');","Scan <observation file>");
			    added=true;
			}
			if (! added) {addChildText(item,"No data available...");}
		    };
		    documentLog.innerHTML="";
		}
	    })
	.error(
	    function (error) { alert("Obs filter file request failed (system error)");}
	);
};

function obs_showTablePath(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent obs-load request.";
    var path=args[0] || "";
    var cls = "tables";
    $.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:cls,path:path})
	.success(
	    function(data, status){
		removeChildren(item);
		var added=false;
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+path+"'\n"+msg);
		} else {
		    var ls=data.getElementsByTagName("ls");
		    if (ls.length > 0) {
			var root=ls[0].getAttribute("root");
			var loc=ls[0].getAttribute("location");
			var pdirs=getSubDirs(cls,root,loc,"");
			var parent=pdirs[0];
			//console.log("Found parent: ",root,loc,parent);
			if (parent != null) {
			    var dd=root+parent;
			    addChildButton(item,"<up>",
					   "obs_setArray('tablePath','"+dd+"');obs_show();","Change to parent <directory>");
			    added=true;
			};
			var dirs=ls[0].getElementsByTagName("dir");
			//console.log("Found dir entries: ",dirs.length);
			for (var ii=0; ii< dirs.length; ii++) {
			    var dd = dirs[ii].getAttribute("path");
			    //console.log("Adding dir button: ",dd);
			    addChildButton(item,dd,"obs_setArray('tablePath','"+dd+"');obs_show();","Change <directory>");
			    added=true;
			};
		    };
		};
		if (! added) {addChildText(item,"No data available...");}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Table request failed (system error)");}
	);
};

function obs_showBufrType(item,target,arg) {
    var file=obs_getConfigFile();
    removeChildren(item);
    var added=false;
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
		addChildButton(item,bufrType+" "+info+ccnt,"obs_setArray('bufrType','"+bufrType+"');showValue('obsBufrType','"+bufrType+"');","Use <BUFR type>");
		added=true;
	    }
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};

function obs_showSubType(item,target,arg) {
    var file=obs_getConfigFile();
    var bufrType=obs_config[file]["bufrType"];
    removeChildren(item);
    var added=false;
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
		    addChildButton(item,subType+" : "+info+ccnt,"obs_setArray('subType','"+subType+"');obs_setArray('typeInfo','"+info+"');showValue('obsSubType','"+subType+"');showValue('obsTypeInfo','"+info+"');","Use <BUFR sub type>");
		    added=true;
		}
	    }
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};

function obs_showIndexPos(item,target,arg) {
    var file=obs_getConfigFile();
    var bufrType=obs_config[file]["bufrType"];
    var subType=obs_config[file]["subType"];
    removeChildren(item);
    var added=false;
    if (bufrType !== undefined && bufrType !== "" &&
	subType !== undefined && subType !== "" && subType !== "info" &&subType !== "cnt" &&
	obs_config[file] !== undefined && 
	obs_config[file]["bufr"] !== undefined && 
	obs_config[file]["bufr"][bufrType] !== undefined && 
	obs_config[file]["bufr"][bufrType][subType] !== undefined) {
	var bufr=obs_config[file]["bufr"][bufrType][subType];
	for (var pos in bufr) {
	    if (pos !== "info" && pos !== "cnt")  {
		var descr=bufr[pos]["descr"];
		var info=" "+bufr[pos]["info"];
		if (bufr[pos]["val1"] !== undefined && bufr[pos]["val1"] != "NA") {
		    var value="    ~ "+bufr[pos]["val1"];
		} else {
		    var value="";
		};
		if (descr == "31001") {
		    addChildButtonShaded(item,pos+" : "+descr + info + value,"showValue('obsIndexPOS','"+pos+"');showValue('obsIndexDESCR','"+descr+"');showValue('obsIndexInfo','"+info+"');","use <BUFR delayed replicator>");
		    added=true;
		} else {
		    addChildButton(item,pos+" : "+descr + info + value,"showValue('obsIndexPOS','"+pos+"');showValue('obsIndexDESCR','"+descr+"');showValue('obsIndexInfo','"+info+"');","Use <BUFR sequence element>");
		    added=true;
		}
	    }
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};

function obs_showIndexExp(item,target,arg) {
    console.log("showIndexExp target='" + target + "'  arg='"+arg+"'");
    var file=obs_getConfigFile();
    var bufrType=obs_config[file]["bufrType"];
    var subType=obs_config[file]["subType"];
    removeChildren(item);
    var added=false;
    if ( obs_config[file] !== undefined &&
	 obs_config[file]["targets"] !== undefined ) {
	for (var trg in obs_config[file]["targets"]) {
	    addChildButton(item,trg,"addValue('obsIndexExp','"+trg+"');","observation target");
	    added=true;
	}
	addFunctionButtons(item,target);
    }
    if (! added) {addChildText(item,"No data available...");}
};
