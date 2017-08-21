// data structure
auto_config = { model : {"model_config1.cfg" : {lastAuto:"",lastUsed:"",status:""},
			 "model_config2.cfg": {lastAuto:"",lastUsed:"",status:""}},
		obs :  {"obs_config1.cfg": {lastAuto:"",lastUsed:"",status:""}},
		password: "test"
	      };
auto_configEd=0;


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
    $.get("cgi-bin/fark_load.pl",{type:"auto",root:root},function(data, status){
	dataToArray(data,status,documentLog);
	auto_setTable();
    });
};
function auto_newModelConfigFile(item) {
    var file=item.parentNode.parentNode.children[1].children[0].value;
    //item.parentNode.parentNode.children[1].children[0].value="";
    setValue('autoModelConfigFile',"");
    setInnerHTML('autoModelLastAuto',"");
    setInnerHTML('autoModelLastUsed',"");
    if (file !== "" ) {
	if (auto_config["model"][file] === undefined) {
	    auto_config["model"][file]={lastAuto:"",lastUsed:""};
	};
	auto_setTable();
    } else {
	alert("Invalid: Model config file ('"+file+"')");
    }
};
function auto_autoNow(target,type,file) {
    var root="";
    if (target === "") {root="auto.cfg";};
    if (file !== "") {
	documentLog.innerHTML="Sent auto-now request ("+file+").";
	$.get("cgi-bin/fark_auto.pl",{root:root,type:type,file:file},
	      function(data, status){
		  if (status == "success") {
		      var errors=data.getElementsByTagName("error");
		      if (errors.length > 0 ) {
			  console.log("Error:",data);
			  var msg=(errors[0].getAttribute("message")||"");
			  alert("Unable to process, "+type+" config file: "+file+"\n"+msg);
		      };
		      if (target === "") {
			  dataToArray(data,status,documentLog);
			  auto_setTable();
		      } else {
			  target.children[3].innerHTML="manual";
		      }
		      documentLog.innerHTML="";}
	      }
	 );
    };
};
function auto_newObsConfigFile(item) {
    var file=item.parentNode.parentNode.children[1].children[0].value;
    //item.parentNode.parentNode.children[1].children[0].value="";
    setValue('autoObsConfigFile',"");
    setInnerHTML('autoObsLastAuto',"");
    setInnerHTML('autoObsLastUsed',"");
    if (file !== "" ) {
	if (auto_config["obs"][file] === undefined) {
	    auto_config["obs"][file]={lastAuto:"",lastUsed:""};
	};
	auto_setTable();
    } else {
	alert("Invalid: Obs config file ('"+file+"')");
    }
};
function auto_saveConfig(target) {
    var root="auto.cfg";
    var password=document.getElementById("autoConfigFilePsw").value;
    var modelFiles="";
    var obsFiles="";
    auto_setTable();
    var len=auto_config["model"];
    for (var model in auto_config["model"]) {
	modelFiles=modelFiles + "|" + model + "~" + 
	    auto_config["model"][model]["lastAuto"] + "~";
	    auto_config["model"][model]["lastUsed"];
    };
    for (var obs in auto_config["obs"]) {
	obsFiles=obsFiles + "|" + obs + "~" + 
	    auto_config["obs"][obs]["lastAuto"] + "~";
	    auto_config["obs"][obs]["lastUsed"];
    }
    documentLog.innerHTML="Sent obs-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"auto",root:root,password:password,modelFiles:modelFiles,obsFiles:obsFiles},
	  function(data, status){
	      if (status == "success") {
		  var errors=data.getElementsByTagName("error");
		  if (errors.length > 0 ) {
		      console.log("Error:",data);
		      var msg=(errors[0].getAttribute("message")||"");
		      alert("Unable to auto, config file: "+root+"\n"+msg);
		  };
		  documentLog.innerHTML="";}
	  }
	 );
    makeUrl("auto",root);
};
function auto_removeFile(item,type,file) {
    if (type == "model") {
	var item=document.getElementById("newlineAutoModel");
	item.children[1].children[0].value=file;
    } else if (type =="obs") {
	var item=document.getElementById("newlineAutoObs");
	item.children[1].children[0].value=file;
    };
    //if (! checkAutoPassword()) {return;}
    //item.parentNode.removeChild(item);
    delete auto_config[type][file];
    auto_setTable();
};

// create auto table
function auto_setTable() {
    var item=document.getElementById('autoTable');
    var tail=removeTableChildFromTo(item,"labelsAutoModel","newlineAutoModel");
    for (var model in auto_config["model"]) {
	auto_insertRow(tail,"model",model,auto_config["model"][model]["lastAuto"],auto_config["model"][model]["lastUsed"],auto_config["model"][model]["status"]);
    }
    var tail=removeTableChildFromTo(item,"labelsAutoObs","newlineAutoObs");
    for (var obs in auto_config["obs"]) {
	auto_insertRow(tail,"obs",obs,auto_config["obs"][obs]["lastAuto"],auto_config["obs"][obs]["lastUsed"],auto_config["obs"][obs]["status"]);
    }
};
// create auto table row
function auto_insertRow(item,type,file,lastAuto,lastUsed,status) {
    var row = document.createElement("TR");
    var td;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","auto_removeFile(this.parentNode.parentNode,'"+type+"','"+file+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make FILE NAME column
    td=document.createElement("TD");
    td.innerHTML=file;
    row.appendChild(td);
    // make select-FILE NAME column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make LAST AUTO column
    td=document.createElement("TD");
    if (status !== "") {
	td.setAttribute("style","color:blue");
    }
    td.innerHTML=lastAuto;
    row.appendChild(td);
    // make LAST USED column
    td=document.createElement("TD");
    td.innerHTML=lastUsed;
    row.appendChild(td);
    // make AUTO NOW column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","auto_autoNow('','"+type+"','"+file+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("Auto now");
    btn.appendChild(t);
    td.appendChild(btn);

    row.appendChild(td);
    // make add row to table
    item.parentNode.insertBefore(row,item);
    return row;
}
