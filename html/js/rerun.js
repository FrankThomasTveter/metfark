
// data structure
rerun_file="default.cfg";
rerun_config = { "default.cfg" : {model : {"default1.cfg" : {last:"",info:"",rerun:"",status:""}},
				  obs :   {"default2.cfg" : {last:"",info:"",rerun:"",status:""}},
				  coloc : {"default3.cfg" : {last:"",info:"",rerun:"",status:""}},
				  plot :  {"default4.cfg" : {last:"",info:"",rerun:"",status:""}},
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
		rerun_setTable();
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
    for (var plot in rerun_config[file]["plot"]) {
	plotFiles=plotFiles + "|" + plot + "~" + 
	    rerun_config[file]["plot"][plot]["last"] + "~" +
	    rerun_config[file]["plot"][plot]["info"] ;
    }
    documentLog.innerHTML="Sent rerun-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"rerun",file:file,password:password,variable:variable,start:start,stop:stop,offset:offset,
				  modelFiles:modelFiles,obsFiles:obsFiles,colocFiles:colocFiles,plotFiles:plotFiles})
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
    console.log("Rerun set table:",file,JSON.stringify(rerun_config));
    showValue('rerunTimeOffset',rerun_config[file]["offset"]);
    showValue('rerunVariableStart',rerun_config[file]["variable"]["start"]);
    showValue('rerunVariableStop',rerun_config[file]["variable"]["stop"]);
    var item=document.getElementById('rerunTable');
    var tail=removeTableChildFromTo(item,"labelsRerun","newlineRerun");
    var modell=[];
    var obsl=[];
    var colocl=[];
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
    for (var plot in rerun_config[file]["plot"]) {
	//console.log("*** Found: ",cron,plot);
	plotl.push(plot);
    }
    // sort...
    modell.sort();
    obsl.sort();
    colocl.sort();
    plotl.sort();
    for (var ii = 0; ii < modell.length; ++ii) {
	var model=modell[ii];
	//console.log("Insert row: ",model);
	rerun_insertRow(tail,"model",model,
			rerun_config[file]["model"][model]["last"],
			rerun_config[file]["model"][model]["info"],
			rerun_config[file]["model"][model]["status"],"#01DFD7"); 
    }
    for (var ii = 0; ii < obsl.length; ++ii) {
	var obs=obsl[ii];
	//console.log("Insert row: ",obs);
	rerun_insertRow(tail,"obs",obs,
			rerun_config[file]["obs"][obs]["last"],
			rerun_config[file]["obs"][obs]["info"],
			rerun_config[file]["obs"][obs]["status"],"#F3E2A9");
    }
    for (var ii = 0; ii < colocl.length; ++ii) {
	var coloc=colocl[ii];
	//console.log("Insert row: ",coloc);
	rerun_insertRow(tail,"coloc",coloc,
			rerun_config[file]["coloc"][coloc]["last"],
			rerun_config[file]["coloc"][coloc]["info"],
			rerun_config[file]["coloc"][coloc]["status"],"#66F");
    }
    for (var ii = 0; ii < plotl.length; ++ii) {
	var plot=plotl[ii];
	//console.log("Insert row: ",plot);
	rerun_insertRow(tail,"plot",plot,
			rerun_config[file]["plot"][plot]["last"],
			rerun_config[file]["plot"][plot]["info"],
			rerun_config[file]["plot"][plot]["status"],"#BDBDBD");
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
    } else if (type == "plot") {
	td.setAttribute("title","Create <table file> and run plotting script.");
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
function rerun_fileFind(sfile) {
    var file=rerun_getConfigFile();
    rerun_config[file]["stack"]=sfile;
    var password=document.getElementById("rerunConfigFilePsw").value;
    documentLog.innerHTML="Sent rerun-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"rerunfile",
				  file:file,
				  password:password,
				  target:sfile})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to scan file: "+sfile+" (file:"+file+")\n"+msg);
		    } else {
			dataToArray(data,status,documentLog);
			rerun_show();
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Rerun find request failed (system error)");}
	);
};

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
