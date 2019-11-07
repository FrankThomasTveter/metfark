
// data structure
rerun_file="default.cfg";
rerun_config = { "default.cfg" : {model :  {"default.cfg" : {last:"",info:"",rerun:"",status:""}},
				  obs   :  {"default.cfg" : {last:"",info:"",rerun:"",status:""}},
				  coloc :  {"default.cfg" : {last:"",info:"",rerun:"",status:""}},
				  table :  {"default.cfg" : {last:"",info:"",rerun:"",status:""}},
				  join  :  {"default.cfg" : {last:"",info:"",rerun:"",status:""}},
				  plot  :  {"default.cfg" : {last:"",info:"",rerun:"",status:""}},
				  variable: {name: 'rid',start: '0',stop: '0'},
				  offset:'-cnt'
				 }
	       };
rerun_configEd=0;

// rerun methods
function rerun_run() {
    var file=rerun_getConfigFile();
    var password=document.getElementById("rerunConfigFilePsw").value;
    documentLog.innerHTML="Sent rerun-now request ("+file+").";
    $.ajaxSetup({timeout:0}); // never timeout a request (and re-send it)...
    $.get("cgi-bin/fark_rerun.pl",{file:file,password:password})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			if (isRunning(msg)) {
			    alert(msg+"\nMonitoring has timed out.");
			} else {
			    alert("Unable to process config file: "+file+"\n"+msg);
			}
		    } else {
			dataToArray(data,status,documentLog);
			rerun_setTable();
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Request failed (system error)");}
	);
}

function rerun_allocate(file) {
    if (rerun_config[file] === undefined) {
	rerun_config[file]=clone(rerun_config[rerun_file]);
	//console.log("cloned:",file,rerun_file,rerun_config[file]);
    }
}
function rerun_getConfigFile() {
    return rerun_file;
};
function rerun_setOffset(value) {
    var file=rerun_getConfigFile();
    if (value === undefined) {
	value=getValue('rerunTimeOffset');
    };
    rerun_config[file]["offset"]=decodeURI(value);
};
function rerun_setArray(parameter,value) {
    var file=rerun_getConfigFile();
    rerun_config[file]["variable"][parameter]=decodeURI(value);
};
function rerun_updateData(arg=rerun_getConfigFile()) {
    console.log("Loading rerun-data:",JSON.stringify(arg));
    var args=getArgs(arg);
    documentLog.innerHTML="Sent rerun-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"rerun",arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		rerun_setConfigFile(args[0]);
		rerun_show();
	    })
	.error(
	    function (error) { alert("Rerun request failed (system error)");}
	);
};
function rerun_newSetupFile(item) {
    var file=rerun_getConfigFile();
    var type=item.parentNode.parentNode.children[0].children[0].value;
    var setup=item.parentNode.parentNode.children[2].children[0].value;
    showValue('rerunType',"");
    showValue('rerunSetupFile',"");
    if (setup !== "" ) {
	fark_last[type]=setup;
	if (type === "model") {
	    if (rerun_config[file]["model"][setup] === undefined) {
		rerun_config[file]["model"][setup]={last:"",info:""};
	    };
	} else if (type === "obs") {
	    if (rerun_config[file]["obs"][setup] === undefined) {
		rerun_config[file]["obs"][setup]={last:"",info:""};
	    };
	} else if (type === "coloc") {
	    if (rerun_config[file]["coloc"][setup] === undefined) {
		rerun_config[file]["coloc"][setup]={last:"",info:""};
	    };
	} else if (type === "table") {
	    if (rerun_config[file]["table"][setup] === undefined) {
		rerun_config[file]["table"][setup]={last:"",info:""};
	    };
	} else if (type === "join") {
	    if (rerun_config[file]["join"][setup] === undefined) {
		rerun_config[file]["join"][setup]={last:"",info:""};
	    };
	} else if (type === "plot") {
	    if (rerun_config[file]["plot"][setup] === undefined) {
		rerun_config[file]["plot"][setup]={last:"",info:""};
	    };
	}
	rerun_setTable();
	//console.log("Saving setup file.");
	rerun_saveConfigFile();
    } else {
	alert("Invalid: Model config file ('"+setup+"')");
    }
    //console.log("Adding ",type,setup,rerun);
};
function rerun_saveConfigFile() {
    var file=rerun_getConfigFile();
    var offset=rerun_config[file]["offset"];
    var variable=rerun_config[file]["variable"]["name"];
    var start=rerun_config[file]["variable"]["start"];
    var stop=rerun_config[file]["variable"]["stop"];
    var password=document.getElementById("rerunConfigFilePsw").value;
    var modelFiles="";
    var obsFiles="";
    var colocFiles="";
    var tableFiles="";
    var joinFiles="";
    var plotFiles="";
    rerun_setTable();
    for (var model in rerun_config[file]["model"]) {
	modelFiles=modelFiles + "|" + model + "~" + 
	    rerun_config[file]["model"][model]["last"] + "~" +
 	    rerun_config[file]["model"][model]["info"] ;
    };
    for (var obs in rerun_config[file]["obs"]) {
	obsFiles=obsFiles + "|" + obs + "~" + 
	    rerun_config[file]["obs"][obs]["last"] + "~" +
	    rerun_config[file]["obs"][obs]["info"] ;
    }
    for (var coloc in rerun_config[file]["coloc"]) {
	colocFiles=colocFiles + "|" + coloc + "~" + 
	    rerun_config[file]["coloc"][coloc]["last"] + "~" +
	    rerun_config[file]["coloc"][coloc]["info"];
	//#+
	//	    #rerun_config[file]["coloc"][coloc]["rerun"];
    }
    for (var table in rerun_config[file]["table"]) {
	tableFiles=tableFiles + "|" + table + "~" + 
	    rerun_config[file]["table"][table]["last"] + "~" +
	    rerun_config[file]["table"][table]["info"] ;
    }
    for (var join in rerun_config[file]["join"]) {
	joinFiles=joinFiles + "|" + join + "~" + 
	    rerun_config[file]["join"][join]["last"] + "~" +
	    rerun_config[file]["join"][join]["info"] ;
    }
    for (var plot in rerun_config[file]["plot"]) {
	plotFiles=plotFiles + "|" + plot + "~" + 
	    rerun_config[file]["plot"][plot]["last"] + "~" +
	    rerun_config[file]["plot"][plot]["info"] ;
    }
    documentLog.innerHTML="Sent rerun-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"rerun",file:file,password:password,
				  variable:variable,start:start,stop:stop,offset:offset,
				  modelFiles:modelFiles,
				  obsFiles:obsFiles,
				  colocFiles:colocFiles,
				  tableFiles:tableFiles,
				  joinFiles:joinFiles,
				  plotFiles:plotFiles})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to save rerun config file: "+file+"\n"+msg);
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Rerun save request failed (system error)");}
	);
    makeUrl("rerun",file);
};
function rerun_removeFile(item,type,setup) {
    var file=rerun_getConfigFile();
    var newitem=document.getElementById("newlineRerun");
    newitem.children[0].children[0].value=type;
    newitem.children[2].children[0].value=setup;
    //if (! checkRerunPassword()) {return;}
    //item.parentNode.removeChild(item);
    delete rerun_config[file][type][setup];
    rerun_setTable();
};

//#F78181
function rerun_show() {
    var file=rerun_getConfigFile();
    if (file != "") {
	rerun_allocate(file);
	rerun_setConfigFile(file);
	showValue('rerunTimeOffset',rerun_config[file]["offset"]);
	showValue('rerunVariableStart',rerun_config[file]["variable"]["start"]);
	showValue('rerunVariableStop',rerun_config[file]["variable"]["stop"]);
	rerun_setTable();
    };
};
// make new obs-filter entry
function rerun_setConfigFile(file) {
    showValue('rerunConfigFileSave',file);
    showValue('rerunConfigFile',file);
    //if (file != "") {
    rerun_allocate(file);
    rerun_file=file;
    //};
}
// create rerun table
function rerun_setTable() {
    var file=rerun_getConfigFile();
    //console.log("Rerun set table:",file,JSON.stringify(rerun_config));
    var item=document.getElementById('rerunTable');
    var tail=removeTableChildFromTo(item,"labelsRerun","newlineRerun");
    var modell=[];
    var obsl=[];
    var colocl=[];
    var tablel=[];
    var joinl=[];
    var plotl=[];
    for (var model in rerun_config[file]["model"]) {
	//console.log("*** Found: ",cron, model);
	modell.push(model);
    }
    for (var obs in rerun_config[file]["obs"]) {
	//console.log("*** Found: ",cron,obs);
	obsl.push(obs);
    }
    for (var coloc in rerun_config[file]["coloc"]) {
	//console.log("*** Found: ",cron,coloc);
	colocl.push(coloc);
    }
    for (var table in rerun_config[file]["table"]) {
	//console.log("*** Found: ",cron,table);
	tablel.push(table);
    }
    for (var join in rerun_config[file]["join"]) {
	//console.log("*** Found: ",cron,join);
	joinl.push(join);
    }
    for (var plot in rerun_config[file]["plot"]) {
	//console.log("*** Found: ",cron,plot);
	plotl.push(plot);
    }
    // sort...
    modell.sort();
    obsl.sort();
    colocl.sort();
    tablel.sort();
    joinl.sort();
    plotl.sort();
    for (var ii = 0; ii < modell.length; ++ii) {
	var model=modell[ii];
	//console.log("Insert row: ",model);
	rerun_insertRow(tail,"model",model,
			rerun_config[file]["model"][model]["last"],
			rerun_config[file]["model"][model]["info"],
			rerun_config[file]["model"][model]["status"],"#0A0"); 
    }
    for (var ii = 0; ii < obsl.length; ++ii) {
	var obs=obsl[ii];
	//console.log("Insert row: ",obs);
	rerun_insertRow(tail,"obs",obs,
			rerun_config[file]["obs"][obs]["last"],
			rerun_config[file]["obs"][obs]["info"],
			rerun_config[file]["obs"][obs]["status"],"#AFA");
    }
    for (var ii = 0; ii < colocl.length; ++ii) {
	var coloc=colocl[ii];
	//console.log("Insert row: ",coloc);
	rerun_insertRow(tail,"coloc",coloc,
			rerun_config[file]["coloc"][coloc]["last"],
			rerun_config[file]["coloc"][coloc]["info"],
			rerun_config[file]["coloc"][coloc]["status"],"#FAA");
    }
    for (var ii = 0; ii < tablel.length; ++ii) {
	var table=tablel[ii];
	//console.log("Insert row: ",table);
	rerun_insertRow(tail,"table",table,
			rerun_config[file]["table"][table]["last"],
			rerun_config[file]["table"][table]["info"],
			rerun_config[file]["table"][table]["status"],"#FA0");
    }
    for (var ii = 0; ii < joinl.length; ++ii) {
	var join=joinl[ii];
	//console.log("Insert row: ",join);
	rerun_insertRow(tail,"join",join,
			rerun_config[file]["join"][join]["last"],
			rerun_config[file]["join"][join]["info"],
			rerun_config[file]["join"][join]["status"],"#AF0");
    }
    for (var ii = 0; ii < plotl.length; ++ii) {
	var plot=plotl[ii];
	//console.log("Insert row: ",plot);
	rerun_insertRow(tail,"plot",plot,
			rerun_config[file]["plot"][plot]["last"],
			rerun_config[file]["plot"][plot]["info"],
			rerun_config[file]["plot"][plot]["status"],"#0AF");
    }
};

// create rerun table row
function rerun_insertRow(item,type,setup,last,info,status,color) {
    var row = document.createElement("TR");
    row.setAttribute("bgcolor",color);
    var td;
    // make TYPE column
    td=document.createElement("TD");
    td.innerHTML=type;
    td.setAttribute("title","Schedule type.");
    row.appendChild(td);
    // make select-TYPE column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make FILE NAME column
    td=document.createElement("TD");
    td.innerHTML=setup;
    if (type == "model") {
	td.setAttribute("title","Maintain <model file index>.");
    } else if (type == "obs") {
	td.setAttribute("title","Maintain <observation file index>.");
    } else if (type == "coloc") {
	td.setAttribute("title","Create <colocation xml> for debugging.");
    } else if (type == "table") {
	td.setAttribute("title","Create <table file>.");
    } else if (type == "join") {
	td.setAttribute("title","Join <table file> into new <table file>.");
    } else if (type == "plot") {
	td.setAttribute("title","Run plotting script using data in <table file>.");
    } else {
    };
    row.appendChild(td);
    //console.log("Row file name=",setup);
    // make select-FILE NAME column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make LAST column
    td=document.createElement("TD");
    if (status !== "") {
	td.setAttribute("style","color:blue");
    }
    var a = document.createElement('a');
    var linkText = document.createTextNode(last);
    a.appendChild(linkText);
    a.title = "View last log-file and error-file";
    a.href = "cgi-bin/fark_log.pl?type="+type+"&file="+setup;
    a.target ="_blank";
    td.appendChild(a);
    row.appendChild(td);
    // make INFO column
    td=document.createElement("TD");
    if (type =="model") {
	td.title = "Show status of the model-index-file.";
    } else if (type =="obs") {
	td.title = "Show status of the observation-index-file.";
    } else if (type =="table") {
	td.title = "Show status of the table-file.";
    } else if (type =="join") {
	td.title = "Show status of the joined table-file.";
    } else if (type =="plot") {
	td.title = "Show status of the table-file.";
    } else {
	td.title = "Show status.";
    };
    td.innerHTML=info;
    row.appendChild(td);
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px;");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","rerun_removeFile(this.parentNode.parentNode,'"+type+"','"+setup+"')");
    btn.setAttribute("style","width:100%");
    btn.setAttribute("title","Remove scheduled job");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make add row to table
    item.parentNode.insertBefore(row,item);
    return row;
}

function isRunning(msg) {
    return (msg.substring(0,18) === "Process is running");
}

function rerun_checkPassword() {
    var file=rerun_getConfigFile();
    var password=document.getElementById("rerunConfigFilePsw").value;
    if (rerun_config[file]["password"] !== undefined) {
	if (rerun_config[file]["password"] !== password) {
	    alert("Invalid password used when attempting to save Rerun configuration\n");
	    return false;
	}
    };
    return true;
}
function rerun_mkdir(path) {
    var password=document.getElementById("rerunConfigFilePsw").value;
    documentLog.innerHTML="Sent mkdir request.";
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"rerun",
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
	    function (error) { alert("Rerun mkdir request failed (system error)");}
	);
    
};

function rerun_rmdir(path) {
    var password=document.getElementById("rerunConfigFilePsw").value;
    documentLog.innerHTML="Sent rmdir request.";
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"rerun",
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
	    function (error) { alert("Rerun rmdir request failed (system error)");}
	);
    
};

function rerun_rmfile(path) {
    var password=document.getElementById("rerunConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"rerun",
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
			//delete rerun_config[path];
			if (rerun_file == path) {rerun_file="default.cfg";}
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Rerun rmfile request failed (system error)");}
	);
    
};

function rerun_mkfile(file) {
    //console.log("Calling saveConfigFile: '"+file+"'");
    rerun_setConfigFile(file);
    rerun_saveConfigFile(file);
};

function rerun_fgfile(path) { // clear file from internal memory
    if (rerun_config[path] != undefined) {
	delete rerun_config[path];
    }
};

function rerun_showConfigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent rerun-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"rerun",arg:args})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+arg+"'\n"+msg);
		} else {
		    //console.log("Updating dropdown for ",target,JSON.stringify(data));
		    var ret=dataToArray(data,status,documentLog);
		    var root=ret[0]||{};
		    //console.log("Got data ",target,JSON.stringify(root));
		    removeChildren(item);
		    var added=false;
		    if (args.length >0 && looksLikeFile(args[0])) {
			var file=getFile(args[0]);
		    } else {
			var file="";
		    };
		    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
		    //console.log("Found entries: ",dirs.length-1,root);
		    var parent=dirs[0];
		    if (parent != null) {
			var dd=parent;
			addChildButton(item,"<up>","rerun_setConfigFile('"+dd+"');","Change to parent <directory>");
			added=true;
		    }
		    if (args.length == 1) {
			//console.log("Arg ret:",ret);
			if (root["type"] == "dir" && root["loc"] != "") {
			    addChildButton(item,"<rmdir>","rerun_rmdir('"+args[0]+"');","Remove <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<rmfile>","rerun_rmfile('"+args[0]+"');","Remove <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			    if (looksLikeFile(args[0])) {
				addChildButton(item,"<mkfile>","rerun_mkfile('"+args[0]+"');rerun_show();","Make <file>");
				if (rerun_config[args[0]] != undefined) {
				    addChildButton(item,"<fgfile>","rerun_fgfile('"+args[0]+"');","Forget <file>");
				}
				added=true;
			    } else {
				addChildButton(item,"<mkdir>","rerun_mkdir('"+args[0]+"');","Make <directory>");
				added=true;
			    }
			}
		    } else if (args.length == 2) {
			if (root["type"] == "dir") {
			    addChildButton(item,"<cpdir>","rerun_cpdir('"+args[0]+"','"+args[1]+"');","Copy <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<cpfile>","rerun_cpfile('"+args[0]+"','"+args[1]+"');rerun_setConfigFile('"+args[2]+"');rerun_show();","Copy <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			}
		    };
		    //for (var rerun in rerun_config) {
		    //console.log("Adding config button: ",rerun);
		    //addChildButton(item,rerun,"rerun_setConfigFile('"+rerun+"');rerun_show();");
		    // added=true;
		    //}
		    // add directories...
		    for (var ii=1;ii<dirs.length;ii++) {
			var dir=dirs[ii];
			if (root["loc"] == "" || root["loc"] == ".") {
			    var dd = dir;
			} else {
			    var dd = root["loc"]+dir;
			};
			//if (dd.substr(dd.length-1) == "/" || dd == "") {
			//  dd=dd + file;
			//}
			//console.log("Adding dir button: ",dd,ii,dirs[ii]);
			if (looksLikeFile(dd)) {
			    addChildButton(item,dd,"rerun_setConfigFile('"+dd+"');rerun_show();","Use <file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"rerun_setConfigFile('"+dd+"');rerun_show();","Change <directory>");
			    added=true;
			}
		    };
		    if (! added) {addChildText(item,"No data available...");}
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Dropdown rerun request failed (system error)");}
	);
};

function rerun_showTimeOffset(item,target,arg) {
    var file=rerun_getConfigFile();
    var variable=rerun_config[file]["variable"]["name"];
    removeChildren(item);
    addChildButton(item,'rid',"addValue('"+target+"','-rid');rerun_setOffset();","Rerun variable.");
    addFunctionButtons(item,target);
};

function rerun_showType(item,target,arg) {
    removeChildren(item);
    addChildButton(item,"model","showValue('"+target+"','model');","Maintain model index");
    addChildButton(item,"observation","showValue('"+target+"','obs');","Maintain observation index");
    addChildButton(item,"colocation","showValue('"+target+"','coloc');","Make colocation XML (debugging only)");
    addChildButton(item,"table","showValue('"+target+"','table');","Make table file");
    addChildButton(item,"join","showValue('"+target+"','join');","Join table files");
    addChildButton(item,"plot","showValue('"+target+"','plot');","Make plot table and graphics");
};

function rerun_showSetupFile(item,target,arg) {
    var type=document.getElementById("rerunType").value // "obs";
    var args=getArgs(arg);
    documentLog.innerHTML="Sent rerun-load request.";
    $.get("cgi-bin/fark_load.pl",{type:type,arg:args})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+arg+"'\n"+msg);
		} else {
		    var ret=dataToArray(data,status,documentLog);
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			item.classList.toggle("show");
			var msg=getErrorMessage(errors);
			alert("Unable to list '"+arg+"', type '"+type+"' \n"+msg);
		    } else if (ret[0] !== undefined) {
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
			    //console.log("Adding up: ",dd);
			    addChildButton(item,"<up>","showValue('rerunSetupFile','"+dd+"');","Change to parent <directory>");
			    added=true;
			} else {
			    //console.log("Adding clear: ",dd);
			    addChildButton(item,"<up>","showValue('rerunSetupFile','');","Change to root <directory>");
			    added=true;
			}
			if (dirs.length > 0) {
			    for (var ii=1;ii<dirs.length;ii++) {
				var dir=dirs[ii];
				if (root["loc"] == "" || root["loc"] == ".") {
				    var dd = dir;
				} else {
				    var dd = root["loc"]+dir;
				};
				if (dd !== null && dd !== undefined) {
				    //if (dd.substr(dd.length-1) == "/" || dd == "") {
				    //dd=dd + file;
				    //}
				    //console.log("Adding dir button: ",dd,ii);
				    if (looksLikeFile(dd)) {
					addChildButton(item,dd,"showValue('rerunSetupFile','"+dd+"');","Use <file>");
					added=true;
				    } else {
					addChildButton(item,dd,"showValue('rerunSetupFile','"+dd+"');","Change <directory>");
					added=true;
				    };
				}
			    }
			}
			if (! added) {addChildText(item,"No data available...");}
		    } else {
			console.log("Undefined root.");
		    }
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Rerun request failed (system error)");}
	);
};
