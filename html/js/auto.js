
// data structure
auto_config = { model : {"default1.cfg" : {last:"",info:"",auto:"",status:""}},
		obs :   {"default2.cfg" : {last:"",info:"",auto:"",status:""}},
		coloc : {"default3.cfg" : {last:"",info:"",auto:"",status:""}},
		plot :  {"default4.cfg" : {last:"",info:"",auto:"",status:""}},
		password: "franktt"
	      };
auto_configEd=0;
auto_cron=["","daily","weekly","monthly","quarterly"];

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
    $.get("cgi-bin/fark_load.pl",{type:"auto",root:root})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		auto_setTable();
	    })
	.error(
	    function (error) { alert("Auto request failed (system error)");}
	);
};
function auto_newConfigFile(item) {
    var type=item.parentNode.parentNode.children[0].children[0].value;
    var file=item.parentNode.parentNode.children[2].children[0].value;
    var auto=item.parentNode.parentNode.children[4].children[0].value;
    showValue('autoType',"");
    showValue('autoConfigFile',"");
    document.getElementById("autoCron").value=auto_cron[0];
    if (file !== "" ) {
	fark_last[type]=file;
	if (type === "model") {
	    if (auto_config["model"][file] === undefined) {
		auto_config["model"][file]={last:"",info:"",auto:auto};
	    };
	} else if (type === "obs") {
	    if (auto_config["obs"][file] === undefined) {
		auto_config["obs"][file]={last:"",info:"",auto:auto};
	    };
	} else if (type === "coloc") {
	    if (auto_config["coloc"][file] === undefined) {
		auto_config["coloc"][file]={last:"",info:"",auto:auto};
	    };
	} else if (type === "plot") {
	    if (auto_config["plot"][file] === undefined) {
		auto_config["plot"][file]={last:"",info:"",auto:auto};
	    };
	}
	auto_setTable();
	//console.log("Saving setup file.");
	auto_saveConfigFile();
    } else {
	alert("Invalid: Model config file ('"+file+"')");
    }
   //console.log("Adding ",type,file,auto);
};
function auto_testNow(target,type,file,row) {
    var root="";
    var password=document.getElementById("autoConfigFilePsw").value;
    if (target === "") {root="auto.cfg";};
    if (file !== "") {
	fark_last[type]=file;
	row.children[7].innerHTML=""; // last
	row.children[8].innerHTML="# running"; // info
	documentLog.innerHTML="Sent auto-now request ("+file+").";
	$.get("cgi-bin/fark_auto.pl",{root:root,password:password,type:type,file:file,test:1})
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
			    auto_setTable();
			} else {
			    target.children[5].innerHTML="manual test";
			}
			documentLog.innerHTML="";}
		})
	    .error(
		function (error) { alert("Test request failed (system error)");}
	    );
    };
};
function auto_runNow(target,type,file,row) {
    var root="";
    var password=document.getElementById("autoConfigFilePsw").value;
    if (target === "") {root="auto.cfg";};
    if (file !== "") {
	fark_last[type]=file;
	row.children[7].innerHTML=""; // last
	row.children[8].innerHTML="# running"; // info
	documentLog.innerHTML="Sent auto-now request ("+file+").";
	$.get("cgi-bin/fark_auto.pl",{root:root,password:password,type:type,file:file})
	    .success(
		function(data, status){
		   //console.log("Here...");
		    if (status == "success") {
			var errors=data.getElementsByTagName("error");
			if (errors.length > 0 ) {
			    var msg=getErrorMessage(errors);
			    alert("Unable to process, "+type+" config file: "+file+"\n"+msg);
			};
			if (target === "") {
			    dataToArray(data,status,documentLog);
			    auto_setTable();
			} else {
			    target.children[5].innerHTML="manual run";
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
function auto_stopNow(target,type,file,row) {
    var root="";
    var password=document.getElementById("autoConfigFilePsw").value;
    if (target === "") {root="auto.cfg";};
    if (file !== "") {
	fark_last[type]=file;
	row.children[7].innerHTML=""; // last
	row.children[8].innerHTML="# running"; // info
	documentLog.innerHTML="Sent auto-stop request ("+file+").";
	$.get("cgi-bin/fark_auto.pl",{root:root,password:password,type:type,file:file,abort:1})
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
			    auto_setTable();
			} else {
			    target.children[5].innerHTML="manual stop";
			}
			documentLog.innerHTML="";}
		})
	    .error(
		function (error) { alert("Stop request failed (system error)");}
	    );
    };
};
function auto_saveConfigFile() {
    var root="auto.cfg";
    var password=document.getElementById("autoConfigFilePsw").value;
    var modelFiles="";
    var obsFiles="";
    var colocFiles="";
    var plotFiles="";
    auto_setTable();
    for (var model in auto_config["model"]) {
	modelFiles=modelFiles + "|" + model + "~" + 
	    auto_config["model"][model]["last"] + "~" +
 	    auto_config["model"][model]["info"] + "~" +
 	    auto_config["model"][model]["auto"];
    };
    for (var obs in auto_config["obs"]) {
	obsFiles=obsFiles + "|" + obs + "~" + 
	    auto_config["obs"][obs]["last"] + "~" +
	    auto_config["obs"][obs]["info"] + "~" +
	    auto_config["obs"][obs]["auto"];
    }
    for (var coloc in auto_config["coloc"]) {
	colocFiles=colocFiles + "|" + coloc + "~" + 
	    auto_config["coloc"][coloc]["last"] + "~" +
	    auto_config["coloc"][coloc]["info"] + "~";
	//#+
	//	    #auto_config["coloc"][coloc]["auto"];
    }
    for (var plot in auto_config["plot"]) {
	plotFiles=plotFiles + "|" + plot + "~" + 
	    auto_config["plot"][plot]["last"] + "~" +
	    auto_config["plot"][plot]["info"] + "~" +
	    auto_config["plot"][plot]["auto"];
    }
    documentLog.innerHTML="Sent auto-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"auto",root:root,password:password,modelFiles:modelFiles,obsFiles:obsFiles,colocFiles:colocFiles,plotFiles:plotFiles})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to save auto config file: "+root+"\n"+msg);
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Auto save request failed (system error)");}
	);
    makeUrl("auto",root);
};
function auto_removeFile(item,type,file) {
    var newitem=document.getElementById("newlineAuto");
    newitem.children[0].children[0].value=type;
    newitem.children[2].children[0].value=file;
    newitem.children[4].children[0].value=auto_cron[0];
    //if (! checkAutoPassword()) {return;}
    //item.parentNode.removeChild(item);
    delete auto_config[type][file];
    auto_setTable();
};

//#F78181

// create auto table
function auto_setTable() {
    var item=document.getElementById('autoTable');
    var tail=removeTableChildFromTo(item,"labelsAuto","newlineAuto");
    var models=[];
    var obss=[];
    var colocs=[];
    var plots=[];
    for (var ii = 0; ii < auto_cron.length; ++ii) {
	var cron=auto_cron[ii];
	var modell=[];
	var obsl=[];
	var colocl=[];
	var plotl=[];
	for (var model in auto_config["model"]) {
	    if (auto_config["model"][model]["auto"] === cron) {
		//console.log("*** Found: ",cron, model);
		modell.push(model);
	    }
	}
	for (var obs in auto_config["obs"]) {
	    if (auto_config["obs"][obs]["auto"] == cron) {
		//console.log("*** Found: ",cron,obs);
		obsl.push(obs);
	    }
	}
	for (var coloc in auto_config["coloc"]) {
	    if (auto_config["coloc"][coloc]["auto"] == cron) {
		//console.log("*** Found: ",cron,coloc);
		colocl.push(coloc);
	    }
	}
	for (var plot in auto_config["plot"]) {
	    if (auto_config["plot"][plot]["auto"] == cron) {
		//console.log("*** Found: ",cron,plot);
		plotl.push(plot);
	    }
	}
	// sort...
	modell.sort();
	obsl.sort();
	colocl.sort();
	plotl.sort();
	// add to global array...
	models.extend(modell);
	obss.extend(obsl);
	colocs.extend(colocl);
	plots.extend(plotl);
    }
    for (var ii = 0; ii < models.length; ++ii) {
	var model=models[ii];
	//console.log("Insert row: ",model);
	auto_insertRow(tail,"model",model,auto_config["model"][model]["last"],auto_config["model"][model]["info"],auto_config["model"][model]["auto"],auto_config["model"][model]["status"],"#01DFD7"); 
    }
    for (var ii = 0; ii < obss.length; ++ii) {
	var obs=obss[ii];
	//console.log("Insert row: ",obs);
	auto_insertRow(tail,"obs",obs,auto_config["obs"][obs]["last"],auto_config["obs"][obs]["info"],auto_config["obs"][obs]["auto"],auto_config["obs"][obs]["status"],"#F3E2A9");
    }
    for (var ii = 0; ii < colocs.length; ++ii) {
	var coloc=colocs[ii];
	//console.log("Insert row: ",coloc);
	auto_insertRow(tail,"coloc",coloc,auto_config["coloc"][coloc]["last"],auto_config["coloc"][coloc]["info"],auto_config["coloc"][coloc]["auto"],auto_config["coloc"][coloc]["status"],"#66F");
    }
    for (var ii = 0; ii < plots.length; ++ii) {
	var plot=plots[ii];
	//console.log("Insert row: ",plot);
	auto_insertRow(tail,"plot",plot,auto_config["plot"][plot]["last"],auto_config["plot"][plot]["info"],auto_config["plot"][plot]["auto"],auto_config["plot"][plot]["status"],"#BDBDBD");
    }
};



function auto_setCheckbox(item,type,file) {
    var checked = item.checked;
   //console.log("Checked:",checked);
    if (auto_config[type] !== undefined && auto_config[type][file] !== undefined) {
	auto_config[type][file]["auto"] = checked;
    }
}
// create auto table row
function auto_insertRow(item,type,file,last,info,auto,status,color) {
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
    } else if (type == "plot") {
	td.setAttribute("title","Create <table file> and run plotting script.");
    } else {
    };
    row.appendChild(td);
   //console.log("Row file name=",file);
    // make select-FILE NAME column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make AUTO checkbox column
    td=document.createElement("TD");
    td.setAttribute("title","Repetition interval.");
    td.innerHTML=auto;
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
    btn.setAttribute("onclick","auto_testNow('','"+type+"','"+file+"',this.parentNode.parentNode)");
    btn.setAttribute("class","test");
    if (type == "coloc") {
	btn.innerHTML="&#9762"
    } else {
	btn.innerHTML="&#9655"
    }
    btn.setAttribute("title","Test now, stop processing as soon as some output has been produced.");
    //var t=document.createTextNode();
    //btn.appendChild(t);
    td.appendChild(btn);
    // make RUN NOW column
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","auto_runNow('','"+type+"','"+file+"',this.parentNode.parentNode)");
    btn.setAttribute("class","run");
    if (type == "coloc") {
	btn.innerHTML="&#9762"
    } else {
	btn.innerHTML="&#9654"
    }
    btn.setAttribute("title","Run now, process all data completely.");
    //var t=document.createTextNode();
    //btn.appendChild(t);
    td.appendChild(btn);
    // make STOP column
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","auto_stopNow('','"+type+"','"+file+"',this.parentNode.parentNode)");
    btn.setAttribute("class","stop");
    if (type == "coloc") {
	btn.innerHTML="&#9762"
    } else {
	btn.innerHTML="&#9632"
    }
    btn.setAttribute("title","Stop process.");
    //var t=document.createTextNode();
    //btn.appendChild(t);
    td.appendChild(btn);
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
    btn.setAttribute("onclick","auto_removeFile(this.parentNode.parentNode,'"+type+"','"+file+"')");
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

