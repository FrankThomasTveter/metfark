model_file="default.cfg";
model_config = { "default.cfg" : {filterDir: "/opdata",
				  filterDirStat: "",
				  filterDirMin: "",
				  filterDirMax: "",
				  filterFile: ".*\.nc",
				  hits : "?",
				  indexTarget : "",
				  indexVariable : "",
				  min:"",
				  max:"",
				  variables : {def:""},
				  dims : {},
				  sizes : {},
				  dimensions : {def:1},
				  files : [],
				  stack : "",
				  password: ""
				 }
	       };
model_configEd = 0;


function model_allocate(file) {
    if (model_config[file] === undefined) {
	model_config[file]=clone(model_config[model_file]);
	//console.log("cloned:",file,model_file,model_config[file]);
    }
}

// make new obs-filter entry
function model_setConfigFile(file) {
    showValue('modelConfigFileSave',file);
    showValue('modelConfigFile',file);
    //if (file != "") {
    model_allocate(file);
    model_file=file;
    //};
}
function model_getConfigFile() {
    return model_file;
};
function model_setArray(parameter,value) {
    var file=model_getConfigFile();
   //console.log("File:",file,parameter,model_config[file],value);
    model_config[file][parameter]=decodeURI(value);
};
//check if directory exists...
function model_setFilterDir(value) {
    var file=model_getConfigFile();
   //console.log("File:",file,"filterDir",model_config[file],value);
    var val=decodeURI(value);
    model_config[file]["filterDir"]=val;
    $.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:"data",path:val})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length == 0 ) {
		    document.getElementById('modelFilterDir').style.color='black'
		    model_config[file]["filterDirStat"]="";
		   //console.log("Dir ok:",val);
		} else {
		    model_config[file]["filterDirStat"]=val;
		    document.getElementById('modelFilterDir').style.color='red'
		    console.log("Dir NOT ok:",val);
		}
		model_show();
	    })
	.error(
	    function (error) { alert("Model filter dir request failed (system error)");}
	);
};
function model_show() {
    var file=model_getConfigFile();
    if (file != "" && model_config[file] != undefined) {
	model_allocate(file);
	showValue('modelConfigFile',file);
	showValue('modelConfigFileSave',file);
	showValue('modelFilterDir',model_config[file]["filterDir"]);
	showValue('modelFilterDirMin',model_config[file]["filterDirMin"]);
	showValue('modelFilterDirMax',model_config[file]["filterDirMax"]);
	showValue('modelFilterFile',model_config[file]["filterFile"]);
	showValue('modelIndexTarget',model_config[file]["indexTarget"]);
	showValue('modelIndexVariable',model_config[file]["indexVariable"]);
	model_checkVariable(document.getElementById("modelIndexVariable"));
	setInnerHTML('modelPatternHits',model_config[file]["hits"]);
	// this may seem strange, Stat stores name of dir only if it does not exist...
	if (model_config[file]["filterDirStat"]==model_config[file]["filterDir"]) {
	    document.getElementById('modelFilterDir').style.color='red'
	    console.log("Directory does not exist:",model_config[file]["filterDir"]);
	} else {
	    document.getElementById('modelFilterDir').style.color='black'
	}
    }
};
// model check variable
function model_checkVariable(item) {
    var file=model_getConfigFile();
    var variable=item.value;
    var color="green";
    var variables=model_config[file]["variables"];
    if (variables !== undefined) {
	if (variables[variable] === undefined) {
	    color="red";
	};
    };
    item.setAttribute("style","color:"+color);
}
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
    var filterDir="";
    var filterDirMin="";
    var filterDirMax="";
    var filterFile="";
    var hits="";
    var indexTarget="";
    var indexVariable="";
    var stack="";
    var variables="";
    var dims="";
    if (model_config[file]!= undefined) {
	filterDir=model_config[file]["filterDir"]//"";
	filterDirMin=model_config[file]["filterDirMin"]//"";
	filterDirMax=model_config[file]["filterDirMax"]//"";
	filterFile=model_config[file]["filterFile"]//"";
	hits=model_config[file]["hits"]//"";
	indexTarget=model_config[file]["indexTarget"]//"";
	indexVariable=model_config[file]["indexVariable"]//"";
	var sfile=model_config[file]["stack"]//"";
	if (sfile !== "") {
	    stack=stack+"|"+sfile;
	};
	if (model_config[file]["variables"] != undefined) {
	    for (var variable in model_config[file]["variables"]) {
		var dims=model_config[file]["variables"][variable]//"";
		variables=variables+"|"+variable+"~"+dims;
	    };
	};
	if (model_config[file]["dimensions"] != undefined) {
	    for (var dim in model_config[file]["dimensions"]) {
		var dimv=model_config[file]["dimensions"][dim]//"";
		dims=dims+"|"+dim+"~"+dimv;
	    };
	};
    } else {
	console.log("Warning: setup is not stored properly.");
    }
    //console.log("Variables:",variables," dimensions:",dims);
    documentLog.innerHTML="Sent model-save request.";
    $.get("cgi-bin/fark_save.pl",
	  {type:"model",
	   file:file,
	   password:password,
	   filterDir:filterDir,
	   filterDirMin:filterDirMin,
	   filterDirMax:filterDirMax,
	   filterFile:filterFile,
	   hits:hits,
	   indexTarget:indexTarget,
	   indexVariable:indexVariable,
	   stack:stack,
	   variables:variables,
	   dimensions:dims
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
	    function (error) { alert("Model save request failed (system error)");}
	);
    makeUrl("model",file);
};
function model_updateData(arg=model_getConfigFile()) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent model-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"model",arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		modelLoaded=true;
		//console.log("Updating dropdown for ",target);
		model_show();
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Model request failed (system error)");}
	);
};
function model_fileFind(sfile) {
    var file=model_getConfigFile();
    model_config[file]["stack"]=sfile;
    var password=document.getElementById("modelConfigFilePsw").value;
    documentLog.innerHTML="Sent model-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"modelfile",
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
			model_show();
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Model find request failed (system error)");}
	);
};

function model_mkdir(path) {
    var password=document.getElementById("modelConfigFilePsw").value;
    documentLog.innerHTML="Sent mkdir request.";
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"model",
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
	    function (error) { alert("Model mkdir request failed (system error)");}
	);
    
};

function model_rmdir(path) {
    var password=document.getElementById("modelConfigFilePsw").value;
    documentLog.innerHTML="Sent rmdir request.";
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"model",
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
	    function (error) { alert("Model rmdir request failed (system error)");}
	);
    
};

function model_rmfile(path) {
    var password=document.getElementById("modelConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"model",
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
			//delete model_config[path];
			if (model_file == path) {model_file="default.cfg";}
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Model rmfile request failed (system error)");}
	);
    
};

function model_mkfile(file) {
   //console.log("Calling saveConfigFile: '"+file+"'");
    model_setConfigFile(file);
    model_saveConfigFile(file);
};

function model_fgfile(path) { // clear file from internal memory
    if (model_config[path] != undefined) {
	delete model_config[path];
    }
};

function model_showConfigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent model-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"model",arg:args})
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
			addChildButton(item,"<up>","model_setConfigFile('"+dd+"');","Change to parent <directory>");
			added=true;
		    }
		    if (args.length == 1) {
			//console.log("Arg ret:",ret);
			if (root["type"] == "dir" && root["loc"] != "") {
			    addChildButton(item,"<rmdir>","model_rmdir('"+args[0]+"');","Remove <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<rmfile>","model_rmfile('"+args[0]+"');","Remove <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			    if (looksLikeFile(args[0])) {
				addChildButton(item,"<mkfile>","model_mkfile('"+args[0]+"');model_show();","Make <file>");
				if (model_config[args[0]] != undefined) {
				    addChildButton(item,"<fgfile>","model_fgfile('"+args[0]+"');","Forget <file>");
				}
				added=true;
			    } else {
				addChildButton(item,"<mkdir>","model_mkdir('"+args[0]+"');","Make <directory>");
				added=true;
			    }
			}
		    } else if (args.length == 2) {
			if (root["type"] == "dir") {
			    addChildButton(item,"<cpdir>","model_cpdir('"+args[0]+"','"+args[1]+"');","Copy <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<cpfile>","model_cpfile('"+args[0]+"','"+args[1]+"');model_setConfigFile('"+args[2]+"');model_show();","Copy <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			}
		    };
		    //for (var model in model_config) {
		    //console.log("Adding config button: ",model);
		    //addChildButton(item,model,"model_setConfigFile('"+model+"');model_show();");
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
			    addChildButton(item,dd,"model_setConfigFile('"+dd+"');model_show();","Use <file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"model_setConfigFile('"+dd+"');model_show();","Change <directory>");
			    added=true;
			}
		    };
		    if (! added) {addChildText(item,"No data available...");}
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Dropdown model request failed (system error)");}
	);
};

function model_showFilterDir(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent model-load request.";
    var file=model_getConfigFile();
    var path=args[0] || "";
    var cls = "data";
    var filter=model_config[file]["filterFile"];
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
					   "model_setArray('filterDir','"+dd+"');model_show();","Change to parent <directory>");
			    added=true;
			};
			var dirs=ls[0].getElementsByTagName("dir");
			//console.log("Found dir entries: ",dirs.length);
			for (var ii=0; ii< dirs.length; ii++) {
			    var dd = dirs[ii].getAttribute("path");
			    //console.log("Adding dir button: ",dd);
			    if (looksLikeFile(dd)) {
				addChildButton(item,dd,"model_setArray('filterDir','"+dd+"');model_show();","Use <file>");
				added=true;
			    } else {
				addChildButton(item,dd,"model_setArray('filterDir','"+dd+"');model_show();","Change <directory>");
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
				addChildButtonShaded(item,dd,"model_setArray('filterFile','"+rr+"');model_show();","Copy <pattern> to filter");
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
				addChildButton(item,size+" "+dd,"model_setArray('filterFile','"+dd+"');model_show();","Copy <file name> to filter");
				added=true;
			    };
			};
		    };
		};
		if (! added) {addChildText(item,"No data available...");}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Model dir filter request failed (system error)");}
	);
};

function model_showFilterFile(item,target,arg) {
    var file=model_getConfigFile();
    var password=document.getElementById("modelConfigFilePsw").value;
    var filterDir = model_config[file]["filterDir"];
    var filterDirMin = model_config[file]["filterDirMin"];
    var filterDirMax = model_config[file]["filterDirMax"];
    var filterFile = model_config[file]["filterFile"];
    var indexTarget = model_config[file]["indexTarget"];
    var indexVariable = model_config[file]["indexVariable"];
    documentLog.innerHTML="Sent model-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"model",
				  file:file,
				  password:password,
				  filterDir:filterDir,
				  filterDirMin:filterDirMin,
				  filterDirMax:filterDirMax,
				  filterFile:filterFile,
				  indexTarget:indexTarget,
				  indexVariable:indexVariable
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
			dataToArray(data,status,documentLog);
			setInnerHTML('modelPatternHits',model_config[file]["hits"]);
			removeChildren(item);
			var added=false;
			var len=model_config[file]["files"].length;
			for (var ii=0; ii<len;ii++) {
			    var sfile=model_config[file]["files"][ii][0];
			    var sage=parseFloat(model_config[file]["files"][ii][1]).toFixed(2);
			    var ssize=model_config[file]["files"][ii][2];
			    addChildButton(item,ssize+" "+sfile+" ("+sage+"d)","model_fileFind('"+sfile+"');","Scan <model file>");
			    added=true;
			}
			if (! added) {addChildText(item,"No data available...");}
		    };
		    documentLog.innerHTML="";
		}
	    })
	.error(
	    function (error) { alert("Model file filter request failed (system error)");}
	);
};

function model_showIndex(item,target,arg) {
    var file=model_getConfigFile();
    removeChildren(item);
    var added=false;
    if (model_config[file] !== undefined) {
	var variables=model_config[file]["variables"];
	if (variables !== undefined) {
	    for (var variable in variables) {
		var fullname=variable;
		var dims=model_config[file]["variables"][variable];
		var size=model_config[file]["sizes"][variable]||1;
		if (dims != null) {fullname=fullname+"("+dims+")";};
		console.log("Index:",fullname,size);
		if (size < 1000) {
		    addChildButton(item,fullname,"model_setArray('indexVariable','"+variable+"');model_show();","Select <model variable>");
		    added=true;
		}
	    }
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};
