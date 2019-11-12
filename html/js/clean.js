clean_file="default.cfg";
clean_config = { "default.cfg": {jobs : [[encodeURI("/tmp"),encodeURI(".*.table"),encodeURI(10)]],
				 filterDir:"",
				 filterFile:"",
				 filterAge:"",
				 password: "franktt"
				}
	       };
clean_configEd=0;


function clean_allocate(file) {
    if (clean_config[file] === undefined) {
	clean_config[file]=clone(clean_config[clean_file]);
	//console.log("cloned:",file,clean_file,clean_config[file]);
    }
}

// make new obs-filter entry
function clean_setConfigFile(file) {
    showValue('cleanConfigFileSave',file);
    showValue('cleanConfigFile',file);
    //if (file != "") {
    clean_allocate(file);
    clean_file=file;
    //};
}
function clean_getConfigFile() {
    return clean_file;
};

// clean methods
function clean_checkPassword() {
    var password=document.getElementById("cleanConfigPsw").value;
    if (clean_config[file]["password"] !== undefined) {
	if (clean_config[file]["password"] !== password) {
	    alert("Invalid password used when attempting to save Clean configuration\n");
	    return false;
	}
    };
    return true;
}
function clean_updateData(arg=clean_getConfigFile()) {
    var args=getArgs(arg);
    var type="clean";
    documentLog.innerHTML="Sent clean-load request.";
    $.get("cgi-bin/fark_load.pl",{type:type,arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		clean_show();
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Clean request failed (system error)");}
	);
};
function clean_newJob(item) {
    var file=clean_getConfigFile();
    console.log("Starting add:",file,JSON.stringify(clean_config[file]));
    var filterDir=encodeURI(item.parentNode.parentNode.children[0].children[0].value);
    var filterFile=encodeURI(item.parentNode.parentNode.children[2].children[0].value);
    var filterAge=encodeURI(item.parentNode.parentNode.children[4].children[0].value);
    console.log("newJob:",filterDir,filterFile);
    var target=[filterDir,filterFile,filterAge];
    var item=clean_present(target);
    if (filterDir === "" || filterFile === "" || filterAge === "") {
	alert("Missing information.");
    } else if (item === undefined) {
	clean_config[file]["jobs"].push(target);
	clean_config[file]["filterDir"]="";
	clean_config[file]["filterFile"]="";
	clean_config[file]["filterAge"]="";
	showValue('cleanFilterDir',decodeURI(clean_config[file]["filterDir"]));
	showValue('cleanFilterFile',decodeURI(clean_config[file]["filterFile"]));
	showValue('cleanFilterAge',decodeURI(clean_config[file]["filterAge"]));
    } else {
	alert("Item already exists.");
	console.log("Item already exists:",JSON.stringify(item));
    };
    clean_showTable();
    //console.log("Saving setup file.");
    clean_saveConfigFile();
    console.log("Completed add:",file,JSON.stringify(clean_config[file]));
    //console.log("Adding ",filterDir);
};
function clean_present(target) {
    var file=clean_getConfigFile();
    if (clean_config[file]["jobs"] === undefined) {clean_config[file]["jobs"]=[];};
    var jobs=clean_config[file]["jobs"];
    for (var ii=0;ii< jobs.length;ii++) {
	job=jobs[ii];
	if (job[0]===target[0] && job[1]===target[1]) {
	    return job;
	}
    };
    return;
};
function clean_saveConfigFile() {
    var file=clean_getConfigFile();
    var root="clean.cfg";
    var password=document.getElementById("cleanConfigFilePsw").value;
    var jobstring="";
    clean_showTable();
    var jobs=clean_config[file]["jobs"];
    for (var ii =0; ii< jobs.length;ii++) {
	var job=jobs[ii];
	jobstring=jobstring + "|" + (job[0]||"") + "~" + (job[1]||"") + "~" + (job[2]||"");
    };
    documentLog.innerHTML="Sent clean-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"clean",file:file,password:password,jobs:jobstring})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to save clean config file: "+root+"\n"+msg);
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Clean save request failed (system error)");}
	);
    makeUrl("clean",root);
};
function clean_removeFile(item,filterDir,filterFile,filterAge) {
    var newitem=document.getElementById("newlineClean");
    if (clean_delete(filterDir,filterFile)) {
	newitem.children[0].children[0].value=decodeURI(filterDir);
	newitem.children[2].children[0].value=decodeURI(filterFile);
	newitem.children[4].children[0].value=decodeURI(filterAge);
    }
    clean_showTable();
};

function clean_delete(filterDir,filterFile) {
    var file=clean_getConfigFile();
    console.log("Starting delete:",file,JSON.stringify(clean_config[file]));
    var jobs=clean_config[file]["jobs"];
    var changed=false;
    var newjobs=[];
    for (var ii=0;ii < jobs.length;ii++) {
	var job=jobs[ii];
	if (job[0]!==filterDir || job[1]!==filterFile) {
	    console.log("Keeping:",JSON.stringify(job),filterDir,filterFile);
	    newjobs.push(job);
	} else {
	    console.log("Removing:",JSON.stringify(job),filterDir,filterFile);
	    clean_config[file]["filterDir"]=decodeURI(job[0]);
	    clean_config[file]["filterFile"]=decodeURI(job[1]);
	    clean_config[file]["filterAge"]=decodeURI(job[2]);
	    changed=true;
	}
    }
    if (changed) {
	clean_config[file]["jobs"]=newjobs;
    }
    console.log("Completed delete:",file,JSON.stringify(clean_config[file]));
    return changed;
}
//#F78181

// create clean table
function clean_showTable() {
    var file=clean_getConfigFile();
    var item=document.getElementById('cleanTable');
    var tail=removeTableChildFromTo(item,"labelsClean","newlineClean");
    var jobs=clean_config[file]["jobs"];
    for (var ii = 0; ii < jobs.length; ++ii) {
	var job=jobs[ii];
	clean_insertRow(tail,job,"#0A0"); 
    }
};
// create clean table row
function clean_insertRow(item,job,color) {
    var row = document.createElement("TR");
    row.setAttribute("bgcolor",color);
    var td;
    // make filterDir column
    td=document.createElement("TD");
    td.innerHTML=decodeURI(job[0]);
    td.setAttribute("title","Top directory for search.");
    row.appendChild(td);
    // make select-filterDir column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make filterFile column
    td=document.createElement("TD");
    td.innerHTML=decodeURI(job[1]);
    td.setAttribute("title","Filter file (regexp).");
    row.appendChild(td);
    // make select-filterFile column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make age column
    td=document.createElement("TD");
    td.innerHTML=decodeURI(job[2]);
    td.setAttribute("title","Minimum age of files.");
    row.appendChild(td);
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px;");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","clean_removeFile(this.parentNode.parentNode,'"+job[0]+"','"+job[1]+"','"+job[2]+"')");
    btn.setAttribute("style","width:100%");
    btn.setAttribute("title","Remove filter");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make add row to table
    item.parentNode.insertBefore(row,item);
    return row;
}

function clean_mkdir(path) {
    var password=document.getElementById("cleanConfigFilePsw").value;
    documentLog.innerHTML="Sent mkdir request.";
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"clean",
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
	    function (error) { alert("Clean mkdir request failed (system error)");}
	);
    
};

function clean_rmdir(path) {
    var password=document.getElementById("cleanConfigFilePsw").value;
    documentLog.innerHTML="Sent rmdir request.";
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"clean",
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
	    function (error) { alert("Clean rmdir request failed (system error)");}
	);
    
};

function clean_rmfile(path) {
    var password=document.getElementById("cleanConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"clean",
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
			//delete clean_config[path];
			if (clean_file == path) {clean_file="default.cfg";}
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Clean rmfile request failed (system error)");}
	);
    
};

function clean_mkfile(file) {
   //console.log("Calling saveConfigFile: '"+file+"'");
    clean_setConfigFile(file);
    clean_saveConfigFile(file);
};

function clean_fgfile(path) { // clear file from internal memory
    if (clean_config[path] != undefined) {
	delete clean_config[path];
    }
};

function clean_showConfigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent clean-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"clean",arg:args})
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
			addChildButton(item,"<up>","clean_setConfigFile('"+dd+"');","Change to parent <directory>");
			added=true;
		    };
		    //console.log("Args:",JSON.stringify(args),arg);
		    if (args.length == 1) {
			//console.log("Arg ret:",JSON.stringify(ret),JSON.stringify(root));
			if (root["type"] == "dir" && root["loc"] != "") {
			    addChildButton(item,"<rmdir>","clean_rmdir('"+args[0]+"');","Remove <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<rmfile>","clean_rmfile('"+args[0]+"');","Remove <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			    if (looksLikeFile(args[0])) {
				addChildButton(item,"<mkfile>","clean_mkfile('"+args[0]+"');clean_show();","Make <file>");
				if (clean_config[args[0]] != undefined) {
				    addChildButton(item,"<fgfile>","clean_fgfile('"+args[0]+"');","Forget <file>");
				}
				added=true;
			    } else {
				addChildButton(item,"<mkdir>","clean_mkdir('"+args[0]+"');","Make <directory>");
				added=true;
			    }
			}
		    } else if (args.length == 2) {
			if (root["type"] == "dir") {
			    addChildButton(item,"<cpdir>","clean_cpdir('"+args[0]+"','"+args[1]+"');","Copy <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<cpfile>","clean_cpfile('"+args[0]+"','"+args[1]+"');clean_setConfigFile('"+args[2]+"');clean_show();","Copy <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			}
		    };
		    //for (var clean in clean_config) {
		    //console.log("Adding config button: ",clean);
		    //addChildButton(item,clean,"clean_setConfigFile('"+clean+"');clean_show();");
		    // added=true;
		    //}
		    // add directories...
		    var configfile=clean_getConfigFile();
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
			if (dd===configfile) {
			    addChildButtonShaded(item,dd,"clean_setConfigFile('"+dd+"');clean_show();","Use <file>");
			    added=true;
			} else if (looksLikeFile(dd)) {
			    addChildButton(item,dd,"clean_setConfigFile('"+dd+"');clean_show();","Use <file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"clean_setConfigFile('"+dd+"');clean_show();","Change <directory>");
			    added=true;
			}
		    };
		    if (! added) {addChildText(item,"No data available...");}
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Dropdown clean request failed (system error)");}
	);
};


function clean_setArray(parameter,value) {
    var file=clean_getConfigFile();
    //console.log("File:",file,parameter,value,JSON.stringify(clean_config[file]));
    clean_config[file][parameter]=decodeURI(value);
};
function clean_showFilterDir(item,target,arg) {
    var file=clean_getConfigFile();
    var args=getArgs(arg);
    documentLog.innerHTML="Sent clean-load request.";
    var path=args[0] || "";
    var cls = "output";
    var filter=clean_config[file]["filterFile"];
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
					   "clean_setArray('filterDir','"+dd+"');clean_show();","Change to parent <directory>");
			    added=true;
			};
			var dirs=ls[0].getElementsByTagName("dir");
			//console.log("Found dir entries: ",dirs.length);
			for (var ii=0; ii< dirs.length; ii++) {
			    var dd = dirs[ii].getAttribute("path");
			    //console.log("Adding dir button: ",dd);
			    if (looksLikeFile(dd)) {
				addChildButton(item,dd,"clean_setArray('filterDir','"+dd+"');clean_show();","Use <file>");
				added=true;
			    } else {
				addChildButton(item,dd,"clean_setArray('filterDir','"+dd+"');clean_show();","Change <directory>");
				added=true;
			    }
			};
			var patts=ls[0].getElementsByTagName("pattern");
			//console.log("Found file entries: ",patts.length);
			for (var ii=0; ii< patts.length; ii++) {
			    var rr = getFile(patts[ii].getAttribute("regexp"));
			    var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
			    if (dd !== '') {
				//console.log("Adding file button: ",dd,rr);
				addChildButtonShaded(item,dd,"clean_setArray('filterFile','"+rr+"');clean_show();","Copy <pattern> to filter");
				added=true;
			    };
			};
			var fils=ls[0].getElementsByTagName("file");
			//console.log("Found file entries: ",fils.length);
			for (var ii=0; ii< fils.length; ii++) {
			    var dd = getFile(fils[ii].getAttribute("path"));
			    var size = fils[ii].getAttribute("size")
			    if (dd !== '') {
				//console.log("Adding file button: ",dd,":",size,":");
				addChildButton(item,size+" "+dd,"clean_setArray('filterFile','"+dd+"');clean_show();","Copy <file name> to filter");
				added=true;
			    };
			};
		    };
		};
		if (! added) {addChildText(item,"No data available...");}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Clean dir filter request failed (system error)");}
	);
};

function clean_show() {
    var file=clean_getConfigFile();
    showValue('cleanConfigFileSave',file);
    showValue('cleanConfigFile',file);
    showValue('cleanFilterDir',clean_config[file]["filterDir"]);
    showValue('cleanFilterFile',clean_config[file]["filterFile"]);
    showValue('cleanFilterAge',clean_config[file]["filterAge"]);
    clean_showTable();
};
function clean_showFilterFile(item,target,arg) {
    var file=clean_getConfigFile();
    var type="clean";
    var password=document.getElementById("cleanConfigFilePsw").value;
    var filterDir = clean_config[file]["filterDir"];
    var filterFile = clean_config[file]["filterFile"];
    var filterAge = clean_config[file]["filterAge"];
    var file=clean_getConfigFile();
    documentLog.innerHTML="Sent clean-find request.";
    $.get("cgi-bin/fark_find.pl",{type:type,
				  file:file,
				  password:password,
				  filterDir:filterDir,
				  filterFile:filterFile,
				  filterAge:filterAge
				 })
	.success(	
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			item.classList.toggle("show");
			var msg=getErrorMessage(errors);
			alert("Unable to find files at "+filterDir+" (filter:'"+filterFile+"', Setup file:"+file+")\n"+msg);
		    } else {
			//console.log("*****************",JSON.stringify(data),JSON.stringify(data.getElementsByTagName("clean_config")));
			dataToMetfark(data,status,documentLog);
			removeChildren(item);
			var added=false;
			if (clean_config[file]["files"] !== undefined) {
			    var len=clean_config[file]["files"].length;
			    for (var ii=0; ii<len;ii++) {
				var sfile=clean_basename(clean_config[file]["files"][ii][0]);
				var sage=parseFloat(clean_config[file]["files"][ii][1]).toFixed(2);
				var mage=parseFloat(Math.floor(clean_config[file]["files"][ii][1])).toFixed(0);
				var ssize=clean_config[file]["files"][ii][2];
				addChildButton(item,ssize+" "+sfile+" ("+sage+"d)","clean_setArray('filterFile','"+sfile+"');clean_setArray('filterAge','"+mage+"');clean_show();","Copy file to filter");
				added=true;
			    }
			};
			if (! added) {addChildText(item,"No data available...");}
		    };
		    documentLog.innerHTML="";
		}
	    })
	.error(
	    function (error) { alert("Clean file filter request failed (system error)");}
	);
};
function clean_basename(string) {
    var myRe = /^.*\/([^\/]*)$/g;
    var myArray = myRe.exec(string);
    console.log("Basename:",string,'->',myArray[1]);
    return myArray[1];
};
