// data structure
scan_config = { model : {"model_config1.cfg" : {lastScan:"",lastUsed:""},
			 "model_config2.cfg": {lastScan:"",lastUsed:""}},
		obs :  {"obs_config1.cfg": {lastScan:"",lastUsed:""}},
		password: "test"
	      };
scan_configEd=0;


// scan methods
function scan_checkPassword() {
    var password=document.getElementById("scanConfigPsw").value;
    if (scan_config["password"] !== undefined) {
	if (scan_config["password"] !== password) {
	    alert("Invalid password used when attempting to save Scan configuration\n");
	    return false;
	}
    };
    return true;
}
function scan_updateData() {
    documentLog.innerHTML="Sent scan-load request.";
    var root="scan.cfg";
    $.get("cgi-bin/load.pl",{type:"scan",root:root},function(data, status){
	dataToArray(data,status,documentLog);
	scan_setTable();
    });
};
function scan_newModelConfigFile(item) {
    var file=item.parentNode.parentNode.children[1].children[0].value;
    //item.parentNode.parentNode.children[1].children[0].value="";
    setValue('scanModelConfigFile',"");
    setInnerHTML('scanModelLastScan',"");
    setInnerHTML('scanModelLastUsed',"");
    if (file !== "" ) {
	if (scan_config["model"][file] === undefined) {
	    scan_config["model"][file]={lastScan:"",lastUsed:""};
	};
	scan_setTable();
    } else {
	alert("Invalid: Model config file ('"+file+"')");
    }
};
function scan_scanNow(target,type,file) {
    var root="scan.cfg";
    if (file !== "") {
	documentLog.innerHTML="Sent scan-now request ("+file+").";
	$.get("cgi-bin/scan.pl",{root:root,type:type,file:file},
	      function(data, status){
		  if (status == "success") {
		      var errors=data.getElementsByTagName("error");
		      if (errors.length > 0 ) {
			  console.log("Error:",data);
			  var msg=(errors[0].getAttribute("message")||"");
			  alert("Unable to scan, "+type+" config file: "+file+"\n"+msg);
		      };
		      dataToArray(data,status,documentLog);
		      scan_setTable();
		      documentLog.innerHTML="";}
	      }
	 );
    };
};
function scan_newObsConfigFile(item) {
    var file=item.parentNode.parentNode.children[1].children[0].value;
    //item.parentNode.parentNode.children[1].children[0].value="";
    setValue('scanObsConfigFile',"");
    setInnerHTML('scanObsLastScan',"");
    setInnerHTML('scanObsLastUsed',"");
    if (file !== "" ) {
	if (scan_config["obs"][file] === undefined) {
	    scan_config["obs"][file]={lastScan:"",lastUsed:""};
	};
	scan_setTable();
    } else {
	alert("Invalid: Obs config file ('"+file+"')");
    }
};
function scan_saveConfig(target) {
    var root="scan.cfg";
    var password=document.getElementById("scanConfigFilePsw").value;
    var modelFiles="";
    var obsFiles="";
    scan_setTable();
    var len=scan_config["model"];
    for (var model in scan_config["model"]) {
	modelFiles=modelFiles + "|" + model + "/" + 
	    scan_config["model"][model]["lastScan"] + "/";
	    scan_config["model"][model]["lastUsed"];
    };
    for (var obs in scan_config["obs"]) {
	obsFiles=obsFiles + "|" + obs + "/" + 
	    scan_config["obs"][obs]["lastScan"] + "/";
	    scan_config["obs"][obs]["lastUsed"];
    }
    documentLog.innerHTML="Sent obs-save request.";
    $.get("cgi-bin/save.pl",{type:"scan",root:root,password:password,modelFiles:modelFiles,obsFiles:obsFiles},
	  function(data, status){
	      if (status == "success") {
		  var errors=data.getElementsByTagName("error");
		  if (errors.length > 0 ) {
		      console.log("Error:",data);
		      var msg=(errors[0].getAttribute("message")||"");
		      alert("Unable to scan, config file: "+root+"\n"+msg);
		  };
		  documentLog.innerHTML="";}
	  }
	 );
};
function scan_removeFile(item,type,file) {
    //if (! checkScanPassword()) {return;}
    //item.parentNode.removeChild(item);
    delete scan_config[type][file];
    scan_setTable();
};

// create scan table
function scan_setTable() {
    var item=document.getElementById('scanTable');
    var tail=removeTableChildFromTo(item,"labelsScanModel","newlineScanModel");
    for (var model in scan_config["model"]) {
	scan_insertRow(tail,"model",model,scan_config["model"][model]["lastScan"],scan_config["model"][model]["lastUsed"]);
    }
    var tail=removeTableChildFromTo(item,"labelsScanObs","newlineScanObs");
    for (var obs in scan_config["obs"]) {
	scan_insertRow(tail,"obs",obs,scan_config["obs"][obs]["lastScan"],scan_config["obs"][obs]["lastUsed"]);
    }
};
// create scan table row
function scan_insertRow(item,type,file,lastScan,lastUsed) {
    var row = document.createElement("TR");
    var td;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","scan_removeFile(this.parentNode.parentNode,'"+type+"','"+file+"')");
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
    // make LAST SCAN column
    td=document.createElement("TD");
    td.innerHTML=lastScan;
    row.appendChild(td);
    // make LAST USED column
    td=document.createElement("TD");
    td.innerHTML=lastUsed;
    row.appendChild(td);
    // make SCAN NOW column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","scan_scanNow(this.parentNode.parentNode,'"+type+"','"+file+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("Scan now");
    btn.appendChild(t);
    td.appendChild(btn);

    row.appendChild(td);
    // make add row to table
    item.parentNode.insertBefore(row,item);
    return row;
}
