model_file="default.cfg";
model_config = { "default.cfg" : {fileFilter: ".*\.nc",
				  fileFilterDir: "/opdata",
				  hits : "?",
				  index : "",
				  variables : [["def",""]],
				  files : ["file1","file2"],
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
    setValue('modelIndexVariable',model_config[file]["index"]);
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
    var stack="";
    var len=model_config[file]["files"].length;
    for (var ii=0; ii<len;ii++) {
	var sfile=model_config[file]["files"][ii];
	stack=stack+"|"+sfile;
    };
    var variables="";
    var len=model_config[file]["variables"].length;
    for (var ii=0; ii<len;ii++) {
	var variable=model_config[file]["variables"][ii][0];
	var dims=model_config[file]["variables"][ii][1];
	variables=variables+"|"+variable+"~"+dims;
    };
    documentLog.innerHTML="Sent model-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"model",
			     file:file,
			     password:password,
			     filterDir:model_config[file]["fileFilterDir"],
			     filter:model_config[file]["fileFilter"],
			     hits:model_config[file]["hits"],
			     index:model_config[file]["index"],
                             stack:stack,
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
    makeUrl("model",file);
};
function model_updateData() {
	documentLog.innerHTML="Sent model-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"model"},function(data, status){
	    dataToArray(data,status,documentLog);
	    modelLoaded=true;
	    //console.log("Updating dropdown for ",target);
	    model_show();
	    documentLog.innerHTML="";
	});
};
function model_patternFind(target) {
    var dropdown=target + 'Dropdown';
    var item=document.getElementById(dropdown);
    var file=model_getConfigFile();
    var password=document.getElementById("modelConfigFilePsw").value;
    var filterDir = model_config[file]["fileFilterDir"];
    var filter = model_config[file]["fileFilter"];
    documentLog.innerHTML="Sent model-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"model",
			     file:file,
			     password:password,
			     filterDir:model_config[file]["fileFilterDir"],
			     filter:model_config[file]["fileFilter"],
			     index:model_config[file]["index"]},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to peek at: "+filterDir+" "+filter+" (file:"+file+")\n"+msg);
	      } else {
		  dataToArray(data,status,documentLog);
		  removeChildren(item);
		  var len=model_config[file]["files"].length;
		  for (var ii=0; ii<len;ii++) {
		      var sfile=model_config[file]["files"][ii];
		      addChildButton(item,sfile,"model_fileFind('"+sfile+"');");
		  }
		  model_show();
	      };
	      documentLog.innerHTML="";
	      document.getElementById(dropdown).classList.toggle("show");
	  }
				}
	 );
};
function model_fileFind(sfile) {
    var file=model_getConfigFile();
    var password=document.getElementById("modelConfigFilePsw").value;
    documentLog.innerHTML="Sent model-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"modelfile",
			     file:file,
			     password:password,
			     target:sfile},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to scan file: "+sfile+" (file:"+file+")\n"+msg);
	      } else {
		  dataToArray(data,status,documentLog);
		  model_show();
	      };
	      documentLog.innerHTML="";}
				}
	 );
};
