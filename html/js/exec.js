
// data structure
exec_config = { model :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		obs   :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		coloc :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		table :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		join  :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		plot  :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		clean :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		rerun :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		password: "franktt"
	      };
exec_configEd=0;
exec_cron=["","daily","weekly","monthly","quarterly"];

// exec methods
function exec_checkPassword() {
    var password=document.getElementById("execConfigPsw").value;
    if (exec_config["password"] !== undefined) {
	if (exec_config["password"] !== password) {
	    alert("Invalid password used when attempting to save Exec configuration\n");
	    return false;
	}
    };
    return true;
}
function exec_updateData() {
    documentLog.innerHTML="Sent exec-load request.";
    var root="exec.cfg";
    $.get("cgi-bin/fark_load.pl",{type:"exec",root:root})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		exec_setTable();
	    })
	.error(
	    function (error) { alert("Exec request failed (system error)");}
	);
};
function exec_newConfigFile(item) {
    var type=item.parentNode.parentNode.children[0].children[0].value;
    var file=item.parentNode.parentNode.children[2].children[0].value;
    var exec=item.parentNode.parentNode.children[4].children[0].value;
    showValue('execType',"");
    showValue('execConfigFile',"");
    document.getElementById("execCron").value=exec_cron[0];
    if (file !== "" ) {
	fark_last[type]=file;
	if (type === "model") {
	    if (exec_config["model"][file] === undefined) {
		exec_config["model"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "obs") {
	    if (exec_config["obs"][file] === undefined) {
		exec_config["obs"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "coloc") {
	    if (exec_config["coloc"][file] === undefined) {
		exec_config["coloc"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "table") {
	    if (exec_config["table"][file] === undefined) {
		exec_config["table"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "join") {
	    if (exec_config["join"][file] === undefined) {
		exec_config["join"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "plot") {
	    if (exec_config["plot"][file] === undefined) {
		exec_config["plot"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "rerun") {
	    if (exec_config["rerun"][file] === undefined) {
		exec_config["rerun"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "clean") {
	    if (exec_config["clean"][file] === undefined) {
		exec_config["clean"][file]={last:"",info:"",exec:exec};
	    };
	}
	exec_setTable();
	//console.log("Saving setup file.");
	exec_saveConfigFile();
    } else {
	alert("Invalid: Model config file ('"+file+"')");
    }
    //console.log("Adding ",type,file,exec);
};
function exec_testNow(target,type,file,row) {
    var root="";
    var password=document.getElementById("execConfigFilePsw").value;
    if (target === "") {root="exec.cfg";};
    if (file !== "") {
	fark_last[type]=file;
	row.children[9].innerHTML=""; // last
	row.children[10].innerHTML="# running"; // info
	documentLog.innerHTML="Sent exec-now request ("+file+").";
	$.ajaxSetup({timeout:0}); // never timeout a request (and re-send it)...
	$.get("cgi-bin/fark_exec.pl",{root:root,password:password,type:type,file:file,test:1})
	    .success(
		function(data, status){
		    if (status == "success") {
			var errors=data.getElementsByTagName("error");
			if (errors.length > 0 ) {
			    var msg=getErrorMessage(errors);
			    if (isRunning(msg)) {
				alert(msg+"\nMonitoring has timed out.");
			    } else {
				alert("Unable to process, "+type+" config file: "+file+"\n"+msg);
			    }
			};
			if (target === "") {
			    dataToArray(data,status,documentLog);
			    exec_setTable();
			} else {
			    target.children[4].innerHTML="manual test";
			}
			documentLog.innerHTML="";}
		})
	    .error(
		function (error) { alert("Test request failed (system error)");}
	    );
    };
};
function exec_runNow(target,type,file,row) {
    var root="";
    var password=document.getElementById("execConfigFilePsw").value;
    if (target === "") {root="exec.cfg";};
    if (file !== "") {
	fark_last[type]=file;
	row.children[9].innerHTML=""; // last
	row.children[10].innerHTML="# running"; // info
	documentLog.innerHTML="Sent exec-now request ("+file+").";
	console.log("Sending sever request....",type,file);
	$.ajaxSetup({timeout:0}); // never timeout a request (and re-send it)...
	$.get("cgi-bin/fark_exec.pl",{root:root,password:password,type:type,file:file})
	    .success(
		function(data, status){
		    console.log("Success...");
		    //console.log("Here...");
		    if (status == "success") {
			var errors=data.getElementsByTagName("error");
			if (errors.length > 0 ) {
			    var msg=getErrorMessage(errors);
			    if (isRunning(msg)) {
				alert(msg+"\nMonitoring has timed out.");
			    } else {
				alert("Unable to process, "+type+" config file: "+file+"\n"+msg);
			    }
			};
			if (target === "") {
			    dataToArray(data,status,documentLog);
			    exec_setTable();
			} else {
			    target.children[4].innerHTML="manual run";
			}
			documentLog.innerHTML="";}
		})
	    .error(
		function (error) { documentLog.innerHTML="Run request failed (system error)";
				   alert("Run request failed (system error)");
				 }
	    );
    };
};
function exec_stopNow(target,type,file,row) {
    var root="";
    var password=document.getElementById("execConfigFilePsw").value;
    if (target === "") {root="exec.cfg";};
    if (file !== "") {
	fark_last[type]=file;
	row.children[9].innerHTML=""; // last
	row.children[10].innerHTML="# running"; // info
	documentLog.innerHTML="Sent exec-stop request ("+file+").";
	$.get("cgi-bin/fark_exec.pl",{root:root,password:password,type:type,file:file,abort:1})
	    .success(
		function(data, status){
		    if (status == "success") {
			var errors=data.getElementsByTagName("error");
			if (errors.length > 0 ) {
			    var msg=getErrorMessage(errors);
			    alert("Unable to process, "+type+" config file: "+file+"\n"+msg);
			};
			if (target === "") {
			    dataToArray(data,status,documentLog);
			    exec_setTable();
			} else {
			    target.children[4].innerHTML="manual stop";
			}
			documentLog.innerHTML="";}
		})
	    .error(
		function (error) { alert("Stop request failed (system error)");}
	    );
    };
};
function exec_saveConfigFile() {
    var root="exec.cfg";
    var password=document.getElementById("execConfigFilePsw").value;
    var modelFiles="";
    var obsFiles="";
    var colocFiles="";
    var tableFiles="";
    var joinFiles="";
    var plotFiles="";
    var rerunFiles="";
    var cleanFiles="";
    exec_setTable();
    for (var model in exec_config["model"]) {
	modelFiles=modelFiles + "|" + model + "~" + 
	    exec_config["model"][model]["last"] + "~" +
 	    exec_config["model"][model]["info"] + "~" +
 	    exec_config["model"][model]["exec"];
    };
    for (var obs in exec_config["obs"]) {
	obsFiles=obsFiles + "|" + obs + "~" + 
	    exec_config["obs"][obs]["last"] + "~" +
	    exec_config["obs"][obs]["info"] + "~" +
	    exec_config["obs"][obs]["exec"];
    }
    for (var coloc in exec_config["coloc"]) {
	colocFiles=colocFiles + "|" + coloc + "~" + 
	    exec_config["coloc"][coloc]["last"] + "~" +
	    exec_config["coloc"][coloc]["info"] + "~";
	//#+
	//	    #exec_config["coloc"][coloc]["exec"];
    }
    for (var table in exec_config["table"]) {
	tableFiles=tableFiles + "|" + table + "~" + 
	    exec_config["table"][table]["last"] + "~" +
	    exec_config["table"][table]["info"] + "~" +
	    exec_config["table"][table]["exec"];
    }
    for (var join in exec_config["join"]) {
	joinFiles=joinFiles + "|" + join + "~" + 
	    exec_config["join"][join]["last"] + "~" +
	    exec_config["join"][join]["info"] + "~" +
	    exec_config["join"][join]["exec"];
    }
    for (var plot in exec_config["plot"]) {
	plotFiles=plotFiles + "|" + plot + "~" + 
	    exec_config["plot"][plot]["last"] + "~" +
	    exec_config["plot"][plot]["info"] + "~" +
	    exec_config["plot"][plot]["exec"];
    }
    for (var rerun in exec_config["rerun"]) {
	rerunFiles=rerunFiles + "|" + rerun + "~" + 
	    exec_config["rerun"][rerun]["last"] + "~" +
	    exec_config["rerun"][rerun]["info"] + "~" +
	    exec_config["rerun"][rerun]["exec"];
    }
    for (var clean in exec_config["clean"]) {
	cleanFiles=cleanFiles + "|" + clean + "~" + 
	    exec_config["clean"][clean]["last"] + "~" +
	    exec_config["clean"][clean]["info"] + "~" +
	    exec_config["clean"][clean]["exec"];
    }
    documentLog.innerHTML="Sent exec-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"exec",root:root,password:password,
				  modelFiles:modelFiles,obsFiles:obsFiles,colocFiles:colocFiles,
				  tableFiles:tableFiles,joinFiles:joinFiles,plotFiles:plotFiles,
				  rerunFiles:rerunFiles,cleanFiles:cleanFiles})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to save exec config file: "+root+"\n"+msg);
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Exec save request failed (system error)");}
	);
    makeUrl("exec",root);
};
function exec_removeFile(item,type,file) {
    var newitem=document.getElementById("newlineExec");
    newitem.children[0].children[0].value=type;
    newitem.children[2].children[0].value=file;
    newitem.children[4].children[0].value=exec_cron[0];
    //if (! checkExecPassword()) {return;}
    //item.parentNode.removeChild(item);
    delete exec_config[type][file];
    exec_setTable();
};

//#F78181

// create exec table
function exec_setTable() {
    var item=document.getElementById('execTable');
    var tail=removeTableChildFromTo(item,"labelsExec","newlineExec");
    var models=[];
    var obss=[];
    var colocs=[];
    var tables=[];
    var joins=[];
    var plots=[];
    var reruns=[];
    var cleans=[];
    for (var ii = 0; ii < exec_cron.length; ++ii) {
	var cron=exec_cron[ii];
	var modell=[];
	var obsl=[];
	var colocl=[];
	var tablel=[];
	var joinl=[];
	var plotl=[];
	var rerunl=[];
	var cleanl=[];
	for (var model in exec_config["model"]) {
	    if (exec_config["model"][model]["exec"] === cron) {
		//console.log("*** Found: ",cron, model);
		modell.push(model);
	    }
	}
	for (var obs in exec_config["obs"]) {
	    if (exec_config["obs"][obs]["exec"] == cron) {
		//console.log("*** Found: ",cron,obs);
		obsl.push(obs);
	    }
	}
	for (var coloc in exec_config["coloc"]) {
	    if (exec_config["coloc"][coloc]["exec"] == cron) {
		//console.log("*** Found: ",cron,coloc);
		colocl.push(coloc);
	    }
	}
	for (var table in exec_config["table"]) {
	    if (exec_config["table"][table]["exec"] == cron) {
		//console.log("*** Found: ",cron,table);
		tablel.push(table);
	    }
	}
	for (var join in exec_config["join"]) {
	    if (exec_config["join"][join]["exec"] == cron) {
		//console.log("*** Found: ",cron,join);
		joinl.push(join);
	    }
	}
	for (var plot in exec_config["plot"]) {
	    if (exec_config["plot"][plot]["exec"] == cron) {
		//console.log("*** Found: ",cron,plot);
		plotl.push(plot);
	    }
	}
	for (var rerun in exec_config["rerun"]) {
	    if (exec_config["rerun"][rerun]["exec"] == cron) {
		//console.log("*** Found: ",cron,rerun);
		rerunl.push(rerun);
	    }
	}
	for (var clean in exec_config["clean"]) {
	    if (exec_config["clean"][clean]["exec"] == cron) {
		//console.log("*** Found: ",cron,clean);
		cleanl.push(clean);
	    }
	}
	// sort...
	modell.sort();
	obsl.sort();
	colocl.sort();
	tablel.sort();
	joinl.sort();
	plotl.sort();
	rerunl.sort();
	cleanl.sort();
	// add to global array...
	models.extend(modell);
	obss.extend(obsl);
	colocs.extend(colocl);
	tables.extend(tablel);
	joins.extend(joinl);
	plots.extend(plotl);
	reruns.extend(rerunl);
	cleans.extend(cleanl);
    }
    for (var ii = 0; ii < models.length; ++ii) {
	var model=models[ii];
	//console.log("Insert row: ",model);
	exec_insertRow(tail,"model",model,
		       exec_config["model"][model]["last"],
		       exec_config["model"][model]["info"],
		       exec_config["model"][model]["exec"],
		       exec_config["model"][model]["status"],"#0A0"); 
    }
    for (var ii = 0; ii < obss.length; ++ii) {
	var obs=obss[ii];
	//console.log("Insert row: ",obs);
	exec_insertRow(tail,"obs",obs,
		       exec_config["obs"][obs]["last"],
		       exec_config["obs"][obs]["info"],
		       exec_config["obs"][obs]["exec"],
		       exec_config["obs"][obs]["status"],"#AFA");
    }
    for (var ii = 0; ii < colocs.length; ++ii) {
	var coloc=colocs[ii];
	//console.log("Insert row: ",coloc);
	exec_insertRow(tail,"coloc",coloc,
		       exec_config["coloc"][coloc]["last"],
		       exec_config["coloc"][coloc]["info"],
		       exec_config["coloc"][coloc]["exec"],
		       exec_config["coloc"][coloc]["status"],"#FAA");
    }
    for (var ii = 0; ii < tables.length; ++ii) {
	var table=tables[ii];
	//console.log("Insert row: ",table);
	exec_insertRow(tail,"table",table,
		       exec_config["table"][table]["last"],
		       exec_config["table"][table]["info"],
		       exec_config["table"][table]["exec"],
		       exec_config["table"][table]["status"],"#FA0");
    }
    for (var ii = 0; ii < joins.length; ++ii) {
	var join=joins[ii];
	//console.log("Insert row: ",join);
	exec_insertRow(tail,"join",join,
		       exec_config["join"][join]["last"],
		       exec_config["join"][join]["info"],
		       exec_config["join"][join]["exec"],
		       exec_config["join"][join]["status"],"#AF0");
    }
    for (var ii = 0; ii < plots.length; ++ii) {
	var plot=plots[ii];
	//console.log("Insert row: ",plot);
	exec_insertRow(tail,"plot",plot,
		       exec_config["plot"][plot]["last"],
		       exec_config["plot"][plot]["info"],
		       exec_config["plot"][plot]["exec"],
		       exec_config["plot"][plot]["status"],"#0AF");
    }
    for (var ii = 0; ii < reruns.length; ++ii) {
	var rerun=reruns[ii];
	//console.log("Insert row: ",rerun);
	exec_insertRow(tail,"rerun",rerun,
		       exec_config["rerun"][rerun]["last"],
		       exec_config["rerun"][rerun]["info"],
		       exec_config["rerun"][rerun]["exec"],
		       exec_config["rerun"][rerun]["status"],"#F6F");
    }
    for (var ii = 0; ii < cleans.length; ++ii) {
	var clean=cleans[ii];
	//console.log("Insert row: ",clean);
	exec_insertRow(tail,"clean",clean,
		       exec_config["clean"][clean]["last"],
		       exec_config["clean"][clean]["info"],
		       exec_config["clean"][clean]["exec"],
		       exec_config["clean"][clean]["status"],"#F60");
    }
};
// create exec table row
function exec_insertRow(item,type,file,last,info,exec,status,color) {
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
    td.innerHTML=file;
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
    } else if (type == "rerun") {
	td.setAttribute("title","Rerun a sub-set of jobs.");
    } else if (type == "clean") {
	td.setAttribute("title","Clean a sub-set of jobs.");
    } else {
    };
    row.appendChild(td);
    //console.log("Row file name=",file);
    // make select-FILE NAME column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make EXEC checkbox column
    td=document.createElement("TD");
    td.setAttribute("title","Repetition interval.");
    td.innerHTML=exec;
    row.appendChild(td);
    // make select-TYPE column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make manual column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:75px;width:75px");
    td.setAttribute("align","center");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","exec_testNow('','"+type+"','"+file+"',this.parentNode.parentNode)");
    btn.setAttribute("class","test");
    btn.setAttribute("title","Test now, stop processing as soon as some output has been produced.");
    btn.setAttribute("width","100%");
    //var t=document.createTextNode();
    //btn.appendChild(t);
    if (type == "coloc") {
	btn.innerHTML="&#9661"
	td.appendChild(btn);
    } else if (type == "clean") {
	//btn.innerHTML="&#9661"
	//td.appendChild(btn);
    } else {
	btn.innerHTML="&#9655"
	td.appendChild(btn);
    }
    td.setAttribute("style","min-width:25px;width:25px");
    td.setAttribute("align","center");
    row.appendChild(td);
    // make RUN NOW column
    td=document.createElement("TD");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","exec_runNow('','"+type+"','"+file+"',this.parentNode.parentNode)");
    btn.setAttribute("class","run");
    btn.setAttribute("title","Run now, process all data completely.");
    btn.setAttribute("width","100%");
    //var t=document.createTextNode();
    //btn.appendChild(t);
    if (type == "coloc") {
	btn.innerHTML="&#9661"
	td.appendChild(btn);
    } else if (type == "clean") {
	btn.innerHTML="&#9661"
	td.appendChild(btn);
    } else {
	btn.innerHTML="&#9654"
	td.appendChild(btn);
    }
    td.setAttribute("style","min-width:25px;width:25px");
    td.setAttribute("align","center");
    row.appendChild(td);
    // make STOP column
    td=document.createElement("TD");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","exec_stopNow('','"+type+"','"+file+"',this.parentNode.parentNode)");
    btn.setAttribute("class","stop");
    btn.setAttribute("title","Stop process.");
    btn.setAttribute("width","100%");
    //var t=document.createTextNode();
    //btn.appendChild(t);
    if (type == "coloc") {
	btn.innerHTML="&#9661"
	td.appendChild(btn);
    } else if (type == "clean") {
	//btn.innerHTML="&#9661"
	//td.appendChild(btn);
    } else {
	btn.innerHTML="&#9632"
	td.appendChild(btn);
    }
    td.setAttribute("style","min-width:25px;width:25px");
    td.setAttribute("align","center");
    row.appendChild(td);
    // make LAST column
    row.appendChild(td);
    td=document.createElement("TD");
    if (status !== "") {
	td.setAttribute("style","color:blue");
    }
    var a = document.createElement('a');
    var linkText = document.createTextNode(last);
    a.appendChild(linkText);
    a.title = "View last log-file and error-file";
    a.href = "cgi-bin/fark_log.pl?type="+type+"&file="+file;
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
	td.title = "Show status of the plot files.";
    } else if (type =="rerun") {
	td.title = "Show status of the rerun files.";
    } else if (type =="clean") {
	td.title = "Show status of the clean files.";
    } else {
	td.title = "Show status.";
    };
    td.innerHTML=info;
    row.appendChild(td);
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px;");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","exec_removeFile(this.parentNode.parentNode,'"+type+"','"+file+"')");
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

function exec_showType(item,target,arg) {
    removeChildren(item);
    addChildButton(item,"model","showValue('"+target+"','model');","Maintain model index");
    addChildButton(item,"observation","showValue('"+target+"','obs');","Maintain observation index");
    addChildButton(item,"colocation","showValue('"+target+"','coloc');","Make colocation XML (debugging only)");
    addChildButton(item,"table","showValue('"+target+"','table');","Make table file");
    addChildButton(item,"join","showValue('"+target+"','join');","Join table files");
    addChildButton(item,"plot","showValue('"+target+"','plot');","Make plots");
    addChildButton(item,"rerun","showValue('"+target+"','rerun');","Rerun jobs");
    addChildButton(item,"clean","showValue('"+target+"','clean');","Clean jobs");
};

function exec_showConfigFile(item,target,arg) {
    var type=document.getElementById("execType").value // "obs";
    var args=getArgs(arg);
    documentLog.innerHTML="Sent exec-load request.";
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
			    addChildButton(item,"<up>","showValue('execConfigFile','"+dd+"');","Change to parent <directory>");
			    added=true;
			} else {
			    //console.log("Adding clear: ",dd);
			    addChildButton(item,"<up>","showValue('execConfigFile','');","Change to root <directory>");
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
					addChildButton(item,dd,"showValue('execConfigFile','"+dd+"');","Use <file>");
					added=true;
				    } else {
					addChildButton(item,dd,"showValue('execConfigFile','"+dd+"');","Change <directory>");
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
	    function (error) { alert("Exec request failed (system error)");}
	);
};

function exec_showCron(item,target,arg) {
    var args=getArgs(arg);
    removeChildren(item);
    var added=false;
    for (var ii=0;ii<exec_cron.length;ii++) {
	var cron=exec_cron[ii];
	if (cron === "") {
	    addChildButton(item,"<never>","showValue('execCron','"+cron+"');","Never run job");
	    added=true;
	} else {
	    addChildButton(item,cron,"showValue('execCron','"+cron+"');","Run job <interval>");
	    added=true;
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};
