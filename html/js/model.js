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
