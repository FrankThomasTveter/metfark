obs_file = "default.cfg";
obs_config = { "default.cfg" : { filterDir : "/home/www/bufr/",
				 filterFile : ".*\.bufr",
				 tablePath : "/home/www/fark-perl_0.15/bufrtables/",
				 hits : "?",
				 bufr : { 99 : { info : "default", 9 : { seq: [{pos: 1, descr:99999, info: "default info"}],
									 info : "more default info"
						     }
					       }
					},
				 bufrType : "99",
				 subType : "9",
				 typeInfo : "default info.",
				 targets : {"yy" : {pos:"", descr:"", info:"", min:"", max:""}},
				 targeto : ["yy"],
				 indexTarget : "time",
				 indexExp : "sec1970(yy,mm,dd,hh,mi)",
				 files : [],
				 stack : "",
				 password: "test"
					   }
	     };
obs_configEd = 0;

function obs_allocate(file) {
    if (obs_config[file] === undefined) {
	obs_config[file]=clone(obs_config[obs_file]);
	console.log("cloned:",file,obs_file,obs_config[file]);
    }
}
function obs_setConfigFile(file) {
    setValue('obsConfigFile',file);
    setValue('obsConfigFileSave',file);
    if (file != "") {
	obs_allocate(file);
	obs_file=file;
    };
}
function obs_getConfigFile() {
    return obs_file;
};
function obs_setArray(parameter,value) {
    var file=obs_getConfigFile();
    //console.log("File:",file,parameter,obs_config[file]);
    obs_config[file][parameter]=value;
};
function obs_setIndexTarget(target,parameter,value) {
    var file=obs_getConfigFile();
    obs_config[file]["targets"][target][parameter]=value;
};
function obs_show() {
    var file=obs_getConfigFile();
    if (file != "") {
	obs_allocate(file);
	setValue('obsConfigFile',file);
	setValue('obsConfigFileSave',file);
	setValue('obsFilterDir',obs_config[file]["filterDir"]);
	setValue('obsFilterFile',obs_config[file]["filterFile"]);
	setValue('obsTablePath',obs_config[file]["tablePath"]);
	setValue('obsBufrType',obs_config[file]["bufrType"]);
	setValue('obsSubType',obs_config[file]["subType"]);
	setValue('obsTypeInfo',obs_config[file]["typeInfo"]);
	obs_setIndexTargetTable(file);
	setValue('obsIndexTarget',obs_config[file]["indexTarget"]);
	setValue('obsIndexExp',obs_config[file]["indexExp"]);
	setInnerHTML('obsPatternHits',obs_config[file]["hits"]);
    };
};
// observation config methods
function obs_checkPassword() {
    var password=document.getElementById("obsConfigFilePsw").value;
    var file=obs_getConfigFile();
    if (obs_config[file] !== undefined) {
	if (obs_config[file]["password"] !== undefined) {
	    if (obs_config[file]["password"] !== password) {
		alert("Invalid password used when attempting to save file:\n"+file);
		return false;
	    };
	};
    };
    return true;
}
function obs_removeTarget(target) {
    var file=obs_getConfigFile();
    var item=document.getElementById("newlineObsIndexTarget");
    item.children[1].children[0].value=target;
    item.children[2].children[0].value=obs_config[file]["targets"][target]["pos"];
    item.children[4].children[0].value=obs_config[file]["targets"][target]["descr"];
    item.children[5].children[0].value=obs_config[file]["targets"][target]["info"];
    item.children[6].children[0].value=obs_config[file]["targets"][target]["min"];
    item.children[7].children[0].value=obs_config[file]["targets"][target]["max"];
    obs_config[file]["targeto"]=obs_removeByValue(obs_config[file]["targeto"],target)
    delete obs_config[file]["targets"][target];
    obs_setIndexTargetTable(file);
};

function obs_isEmpty(obj) {
    for(var prop in obj) {
        if(obj.hasOwnProperty(prop))
            return false;
    }
    return true;
};
function obs_newObsIndexTarget(item) {
    //if (! obs_checkPassword()) {return;}
    var target=item.parentNode.parentNode.children[1].children[0].value;
    var pos=item.parentNode.parentNode.children[2].children[0].value;
    var descr=item.parentNode.parentNode.children[4].children[0].value;
    var info=item.parentNode.parentNode.children[5].children[0].value;
    var min=item.parentNode.parentNode.children[6].children[0].value;
    var max=item.parentNode.parentNode.children[7].children[0].value;
//    console.log("New: trg:",target," pos:",pos," des:",descr," info:",info," min:",min," max:",max);
    if (target !== "") {
	var file= obs_getConfigFile();
	if (obs_config[file] === undefined) {
	    obs_config[file]={};
	};
	if (obs_config[file]["targets"] === undefined) {
	    obs_config[file]["targets"]={};
	};
	if (obs_config[file]["targets"][target] === undefined) {
	    obs_config[file]["targets"][target]={};
	};
	obs_config[file]["targets"][target]["pos"]=pos;
	obs_config[file]["targets"][target]["descr"]=descr;
	obs_config[file]["targets"][target]["info"]=info;
	obs_config[file]["targets"][target]["min"]=min;
	obs_config[file]["targets"][target]["max"]=max;
	obs_config[file]["targeto"].push(target);
	obs_setIndexTargetTable(file);
	item.parentNode.parentNode.children[1].children[0].value="";
	item.parentNode.parentNode.children[2].children[0].value="";
	item.parentNode.parentNode.children[4].children[0].value="";
	item.parentNode.parentNode.children[5].children[0].value="";
	item.parentNode.parentNode.children[6].children[0].value="";
	item.parentNode.parentNode.children[7].children[0].value="";
    } else {
	alert("Invalid observation target name: ('"+target+"')");
    }
};
function obs_saveConfigFile() {
    var file=obs_getConfigFile();
    var password=document.getElementById("obsConfigFilePsw").value;
    var bufrType=obs_config[file]["bufrType"];
    var subType=obs_config[file]["subType"];
    var typeInfo=obs_config[file]["typeInfo"];
    var indexTarget=obs_config[file]["indexTarget"];
    var indexExp=obs_config[file]["indexExp"];
    var stack="";
    var sfile=obs_config[file]["stack"];
    if (sfile !== "") {
	stack=stack+"|"+sfile;
    };
    var obsTargets="";
    var targeto=obs_config[file]["targeto"];
    var targets=obs_config[file]["targets"];
    //for (var target in targets) {
    for (var ii =0; ii< targeto.length;ii++) {
	var target=targeto[ii];
	var pos=targets[target]["pos"];
	var descr=targets[target]["descr"];
	var info=targets[target]["info"];
	var min=targets[target]["min"];
	var max=targets[target]["max"];
	obsTargets=obsTargets + "|" + target + "~" + pos + "~" + descr + "~" + info + "~" + min + "~" + max;
    };
    if (obsTargets == "") {obsTargets="none";}
    obs_configEd++;
    documentLog.innerHTML="Sent obs-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"obs",file:file,password:password,
			     filterDir:obs_config[file]["filterDir"],
			     filterFile:obs_config[file]["filterFile"],
		             stack:stack,
			     table:obs_config[file]["tablePath"],
                             bufrType:bufrType,
                             subType:subType,
                             typeInfo:typeInfo,
                             indexTarget:indexTarget,
                             indexExp:indexExp,
			     obsTargets:obsTargets
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
    makeUrl("obs",file);
};
// make new obs-index entry
function obs_setIndexTargetTable(file) {
    var file=obs_getConfigFile();
    var bufrType = obs_config[file]["bufrType"];
    var subType = obs_config[file]["subType"];
    var item=document.getElementById('obsIndexTargetTable');
    var tail=removeTableChildFromTo(item,"labelsObsIndexTarget","newlineObsIndexTarget");
    var targeto=obs_config[file]["targeto"];
    var targets=obs_config[file]["targets"];
    //for (var target in value) {
    for (var ii =0; ii< targeto.length;ii++) {
	var target=targeto[ii];
	obs_insertIndexTargetRow(tail,target,targets[target]["pos"],targets[target]["descr"],
				 targets[target]["info"],targets[target]["min"],targets[target]["max"]);
    }
}
function obs_insertIndexTargetRow(item,target,pos,descr,info,min,max) {
    var row = document.createElement("TR");
    var td,inp;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:30px;width:30px");
    td.setAttribute("align","center");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","obs_removeTarget('"+target+"')");
    btn.setAttribute("style","width:100%");
    //var t=document.createTextNode("--");
    //btn.appendChild(t);
    btn.innerHTML="&#45";
    //btn.setAttribute("align","center");
    td.appendChild(btn);
    row.appendChild(td);
    // make NAME column  ***************************
    td=document.createElement("TD");
    td.innerHTML=target;
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",pos);
    //inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','pos',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make descr column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",descr);
    //inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','descr',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make info column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",info);
    //inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','info',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make minimum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    //inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','min',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make maximum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    //inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','max',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
function obs_updateData() {
    var args=getArgs(obs_getConfigFile());
    documentLog.innerHTML="Sent obs-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"obs",arg:args},function(data, status){
	dataToArray(data,status,documentLog);
	obsLoaded=true;
	//console.log("Updating dropdown for ",target);
	obs_show();
	documentLog.innerHTML="";
    });
};
function obs_fileFind(sfile) {
    var file=obs_getConfigFile();
    obs_config[file]["stack"]=sfile;
    var password=document.getElementById("obsConfigFilePsw").value;
    var filterDir = obs_config[file]["filterDir"];
    var filterFile = obs_config[file]["filterFile"];
    var table = obs_config[file]["tablePath"];
    var obsTargets = "";
    var obsTrg=obs_config[file]["targets"];
    for (var target in obsTrg) {
	obsTargets=obsTargets + "|" + target + "~" + 
	    obsTrg[target]["pos"] + "~" + 
	    obsTrg[target]["descr"] + "~" + 
	    obsTrg[target]["info"] + "~" + 
	    obsTrg[target]["min"] + "~" + 
	    obsTrg[target]["max"];
    };
    var indexTarget = obs_config[file]["indexTarget"];
    var indexExp = obs_config[file]["indexExp"];
    var bufrType = obs_config[file]["bufrType"];
    var subType = obs_config[file]["subType"];
    var typeInfo=obs_config[file]["typeInfo"];
    documentLog.innerHTML="Sent obs-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"obsfile",
				  file:file,
				  target:sfile,
				  password:password,
				  filterDir:obs_config[file]["filterDir"],
				  filterFile:obs_config[file]["filterFile"],
				  table:obs_config[file]["tablePath"],
				  obsTargets:obsTargets,
				  indexTarget:indexTarget,
				  indexExp:indexExp,
				  bufrType:bufrType,
				  subType:subType,
				  typeInfo:typeInfo},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to scan file: "+sfile+" (file:"+file+")\n"+msg);
	      } else {
		  dataToArray(data,status,documentLog);
		  obs_show();
	      };
	      documentLog.innerHTML="";}
				}
	 );
};

function obs_mkdir(path) {
    var password=document.getElementById("obsConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"obs",
				 path:path,
				 password,password},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to mkdir: "+path+"\n"+msg);
	      };
	      documentLog.innerHTML="";}
				}
	 );
    
};

function obs_rmfile(path) {
    var password=document.getElementById("obsConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"obs",
				 path:path,
				 password,password},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to rmfile: "+path+"\n"+msg);
	      };
	      documentLog.innerHTML="";}
				}
	 );
    
};

function obs_rmdir(path) {
    var password=document.getElementById("obsConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"obs",
				 path:path,
				 password,password},
	  function(data, status){if (status == "success") {
	      var errors=data.getElementsByTagName("error");
	      if (errors.length > 0 ) {
		  console.log("Error:",data);
		  var msg=(errors[0].getAttribute("message")||"");
		  alert("Unable to rmdir: "+path+"\n"+msg);
	      };
	      documentLog.innerHTML="";}
				}
	 );
    
};

function obs_mkfile(file) {
    console.log("Calling saveConfigFile: '"+file+"'");
    obs_setConfigFile(file);
    obs_saveConfigFile(file);
};

// arr=obs_removeByValue(arr,item1,item2...)
function obs_removeByValue(arr) {
    var what, a = arguments, ll = a.length, ax;
    while (ll > 1 && arr.length) {
        what = a[--ll];
        while ((ax= arr.indexOf(what)) !== -1) {
            arr.splice(ax, 1);
        }
    }
    return arr;
};
