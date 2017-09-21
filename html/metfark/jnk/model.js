model_file="default.cfg";
model_config = { "default.cfg" : {fileFilter: "/opdata",
				  fileFilterDir: "def_.*\.nc",
				  hits : "?",
				  sortVariable : "",
				  fimexConfigFile: "",
				  variables : ["def"],
				  password: "" 
				 }
	       };
model_configEd = 0;

// make new obs-filter entry
function model_setConfigFile(value) {
    if (model_config[value] === undefined) {
	model_config[value]=clone(model_config[model_file]);
	//console.log("cloned:",value,model_config[value]);
    }
    model_file=value;
}
function model_getConfigFile() {
    return model_file;
};
function model_setArray(parameter,value) {
    var file=model_getConfigFile();
    //console.log("File:",file,parameter,model_config[file]);
    model_config[file][parameter]=value;
};
function model_show() {
    var file=model_getConfigFile();
    setValue('modelConfigFile',file);
    setValue('modelFileFilterDir',model_config[file]["fileFilterDir"]);
    setValue('modelFileFilter',model_config[file]["fileFilter"]);
    setValue('modelSortVariable',model_config[file]["sortVariable"]);
    setValue('modelFimexConfigFile',model_config[file]["fimexConfigFile"]);
    setInnerHTML('modelPatternHits',model_config[file]["hits"]);
};
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
    var variables="";
    var len=model_config[file]["variables"].length;
    for (var ii=0; ii<len;ii++) {
	var variable=model_config[file]["variables"][ii];
	variables=variables+"|"+variable;
    };
    documentLog.innerHTML="Sent model-save request.";
    $.get("cgi-bin/save.pl",{type:"model",
			     file:file,
			     password:password,
			     filterDir:model_config[file]["fileFilterDir"],
			     filter:model_config[file]["fileFilter"],
			     hits:model_config[file]["hits"],
			     sort:model_config[file]["sortVariable"],
			     fimex:model_config[file]["fimexConfigFile"],
			     variables:variables
			    },
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to save file: "+file+"\n"+msg);
	      };
	      documentLog.innerHTML="";}
				}
	 );
};
function model_updateData() {
	documentLog.innerHTML="Sent model-load request.";
	$.get("cgi-bin/load.pl",{type:"model"},function(data, status){
	    dataToArray(data,status,documentLog);
	    modelLoaded=true;
	    //console.log("Updating dropdown for ",target);
	    model_show();
	    documentLog.innerHTML="";
	});
};
function model_patternFind() {
    var file=model_getConfigFile();
    var password=document.getElementById("modelConfigFilePsw").value;
    var filterDir = model_config[file]["fileFilterDir"];
    var filter = model_config[file]["fileFilter"];
    documentLog.innerHTML="Sent model-find request.";
    $.get("cgi-bin/find.pl",{type:"model",
			     file:file,
			     password:password,
			     filterDir:model_config[file]["fileFilterDir"],
			     filter:model_config[file]["fileFilter"],
			     fimex:model_config[file]["fimexConfigFile"]},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to peek at: "+filterDir+" "+filter+" (file:"+file+")\n"+msg);
	      } else {
		  dataToArray(data,status,documentLog);
		  model_show();
	      };
	      documentLog.innerHTML="";}
				}
	 );
};
