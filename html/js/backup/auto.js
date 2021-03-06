// data structure
auto_config = { model : {"default1.cfg" : {last:"",info:"",auto:"",status:""}},
		obs :   {"default2.cfg" : {last:"",info:"",auto:"",status:""}},
		plot :  {"default3.cfg" : {last:"",info:"",auto:"",status:""}},
		password: "franktt"
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
function auto_newConfigFile(item) {
    var type=item.parentNode.parentNode.children[1].children[0].value;
    var file=item.parentNode.parentNode.children[3].children[0].value;
    var auto=item.parentNode.parentNode.children[7].children[0].checked;
    setValue('autoType',"");
    setValue('autoConfigFile',"");
    document.getElementById("autoModCheck").checked=false;
    if (file !== "" ) {
	if (type === "model") {
	    if (auto_config["model"][file] === undefined) {
		auto_config["model"][file]={last:"",info:"",auto:""};
	    };
	} else if (type === "obs") {
	    if (auto_config["obs"][file] === undefined) {
		auto_config["obs"][file]={last:"",info:"",auto:""};
	    };
	} else if (type === "plot") {
	    if (auto_config["plot"][file] === undefined) {
		auto_config["plot"][file]={last:"",info:"",auto:""};
	    };
	}
	auto_setTable();
    } else {
	alert("Invalid: Model config file ('"+file+"')");
    }
    console.log("Adding ",type,file,auto);
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
function auto_saveConfig(target) {
    var root="auto.cfg";
    var password=document.getElementById("autoConfigFilePsw").value;
    var modelFiles="";
    var obsFiles="";
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
    for (var plot in auto_config["plot"]) {
	plotFiles=plotFiles + "|" + plot + "~" + 
	    auto_config["plot"][plot]["last"] + "~" +
	    auto_config["plot"][plot]["info"] + "~" +
	    auto_config["plot"][plot]["auto"];
    }
    documentLog.innerHTML="Sent auto-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"auto",root:root,password:password,modelFiles:modelFiles,obsFiles:obsFiles,plotFiles:plotFiles},
	  function(data, status){
	      if (status == "success") {
		  var errors=data.getElementsByTagName("error");
		  if (errors.length > 0 ) {
		      console.log("Error:",data);
		      var msg=(errors[0].getAttribute("message")||"");
		      alert("Unable to save auto config file: "+root+"\n"+msg);
		  };
		  documentLog.innerHTML="";}
	  }
	 );
    makeUrl("auto",root);
};
function auto_removeFile(item,type,file) {
    var newitem=document.getElementById("newlineAuto");
    newitem.children[1].children[0].value=type;
    newitem.children[3].children[0].value=file;
    newitem.children[7].children[0].checked=false;
    //if (! checkAutoPassword()) {return;}
    //item.parentNode.removeChild(item);
    delete auto_config[type][file];
    auto_setTable();
};

// create auto table
function auto_setTable() {
    var item=document.getElementById('autoTable');
    var tail=removeTableChildFromTo(item,"labelsAuto","newlineAuto");
    for (var model in auto_config["model"]) {
	auto_insertRow(tail,"model",model,auto_config["model"][model]["last"],auto_config["model"][model]["info"],auto_config["model"][model]["auto"],auto_config["model"][model]["status"]);
    }
    for (var obs in auto_config["obs"]) {
	auto_insertRow(tail,"obs",obs,auto_config["obs"][obs]["last"],auto_config["obs"][obs]["info"],auto_config["obs"][obs]["auto"],auto_config["obs"][obs]["status"]);
    }
    for (var plot in auto_config["plot"]) {
	auto_insertRow(tail,"plot",plot,auto_config["plot"][plot]["last"],auto_config["plot"][plot]["info"],auto_config["plot"][plot]["auto"],auto_config["plot"][plot]["status"]);
    }
};
function auto_setCheckbox(item,type,file) {
    var checked = item.checked;
    console.log("Checked:",checked);
    if (auto_config[type] !== undefined && auto_config[type][file] !== undefined) {
	auto_config[type][file]["auto"] = checked;
    }
}
// create auto table row
function auto_insertRow(item,type,file,last,info,auto,status) {
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
    // make TYPE column
    td=document.createElement("TD");
    td.innerHTML=type;
    row.appendChild(td);
    // make select-TYPE column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make FILE NAME column
    td=document.createElement("TD");
    td.innerHTML=file;
    row.appendChild(td);
    console.log("Row file name=",file);
    // make select-FILE NAME column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make LAST column
    td=document.createElement("TD");
    if (status !== "") {
	td.setAttribute("style","color:blue");
    }
    td.innerHTML=last;
    row.appendChild(td);
    // make INFO column
    td=document.createElement("TD");
    td.innerHTML=info;
    row.appendChild(td);
    // make AUTO checkbox column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:30px;width:30px");
    td.setAttribute("align","center");
    var chk=document.createElement("INPUT");
    chk.setAttribute("type","checkbox");
    console.log("Auto: "+auto);
    if (auto) {
	console.log("Auto checked: "+auto);
	chk.checked=true;
    } else {
	console.log("Auto un-checked: "+auto);
	chk.checked=false;
    };
    chk.setAttribute("onchange","auto_setCheckbox(this,this.parentNode.parentNode.children[1].innerHTML,this.parentNode.parentNode.children[3].innerHTML);");
    td.appendChild(chk);
    row.appendChild(td);
    // make AUTO NOW column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    td.setAttribute("align","center");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","auto_autoNow('','"+type+"','"+file+"')");
    btn.setAttribute("style","background-color:yellow");
    btn.innerHTML="&#9762"
    //var t=document.createTextNode();
    //btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make add row to table
    item.parentNode.insertBefore(row,item);
    return row;
}

