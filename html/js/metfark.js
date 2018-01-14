documentLog = document.getElementById("log");
fark_last = {model:"default.cfg",obs:"default.cfg",coloc:"default.cfg",plot:"default.cfg"};
dropdownEd = {};

// directory structure

metfark_config = { "model" : {},
		   "obs"   : {},
		   "coloc" : {},
		   "plot"  : {}
		 };

var clss = ["model","obs","coloc","plot"];


// initialisation function
function load(){
    var types=["model","obs","coloc","plot","auto"];
    var type="model"; 
    var file="default.cfg";
    var url=getUrlVars();
    if ("type" in url && "file" in url) {
	var t=decodeURIComponent(url["type"] || "model");
	var f=decodeURIComponent(url["file"] || "default.cfg");
	if (types.includes(t)) { 
	    type=t;
	    file=f;
	};
    };
    load_setActive(type);
    load_setConfigFile(type,file);
    load_updateData(type);
};

function load_setActive(type) {
    var types=["model","obs","coloc","plot","auto"];
    var len=types.length;
    for (var ii=0;ii<len;ii++){
	var item_tab=document.getElementById(types[ii]+"_tab");
	var item=document.getElementById(types[ii]);
	if (types[ii] === type) {
	    item_tab.setAttribute("class","active");
	    item.setAttribute("class","tab-pane fade in active");
	} else if (item_tab != null) {
	    item_tab.setAttribute("class","");
	    item.setAttribute("class","tab-pane fade in");
	} else {
	    console.log("Invalid item:",ii,types[ii]+"_tab");
	};
    };
}

function load_setConfigFile(type,file) {
    if (type === "model") {
	model_setConfigFile(file);
    } else if (type === "obs") {
	obs_setConfigFile(file);
    } else if (type === "coloc") {
	coloc_setConfigFile(file);
    } else if (type === "plot") {
	plot_setConfigFile(file);
    } else if (type === "auto") {
	auto_updateData();
    };
};
function load_updateData(type){
    if (type === "model") {
	model_updateData();
	obs_updateData();
	coloc_updateData();
	plot_updateData();
	auto_updateData();
    } else if (type === "obs") {
	model_updateData();
	obs_updateData();
	coloc_updateData();
	plot_updateData();
	auto_updateData();
    } else if (type === "coloc") {
	coloc_updateData();
	model_setConfigFile(coloc_getModelConfigFile());
	model_updateData();
	obs_setConfigFile(coloc_getObsConfigFile());
	obs_updateData();
	plot_updateData();
	auto_updateData();
    } else if (type === "plot") {
	plot_updateData();
	coloc_updateData();
	model_updateData();
	obs_updateData();
	auto_updateData();
    } else if (type === "auto") {
	auto_updateData();
	model_updateData();
	obs_updateData();
	coloc_updateData();
	plot_updateData();
    };
};


function makeUrl(type,file) {
    var url="?type="+type+"&file="+file;
    window.history.replaceState("", "js", url);
}

function getUrlVars() {
    var vars = {};
    var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi,    
    function(m,key,value) {
      //console.log("URL item:",key," ",value)
      vars[key] = value;
    });
    return vars;
};


// dropdown methods
window.onclick = function(event) {
    if (!event.target.matches('.dropbtn')) {
	var dropdowns = document.getElementsByClassName("dropdown-content");
	var i;
	for (i = 0; i < dropdowns.length; i++) {
	    var openDropdown = dropdowns[i];
	    if (openDropdown.classList.contains('show')) {
		openDropdown.classList.remove('show');
	    }
	}
    }
}

function removeChildren(item) {
    var children=item.childNodes;
    var len=children.length;
    for (var ii=len-1;ii>=0;ii--){
	if (children[ii] !== undefined) {
	    //console.log("Removing :",ii,len,children[ii]);
	    item.removeChild(children[ii]);
	}
    }
}
function clearTableChild(item,id) {
    //console.log("removeTableChildFromTo Entering",item);
    var tbody=item.children[0];
    var children=tbody.children;
    var len=children.length;
    var active=false;
    var tail;
    var active=false;
    for (var ii=len-1;ii>=0;ii--){
	//console.log("RemoveTableChild ",ii,children[ii],len,children.length);
	if (children[ii] !== undefined) {
	    if (children[ii].getAttribute !== undefined) {
		var att=children[ii].getAttribute("id");
		if (att === id) {
		    var grandChildren=children[ii].children;
		    var leng=grandChildren.length;
		    for (var jj=leng-1;jj>=0;jj--){
			children[ii].removeChild(grandChildren[jj]);
		    };
		    return children[ii];
		} 
	    }
	}
    };
    return;
}
function removeTableChildFromTo(item,from,to) {
    //console.log("removeTableChildFromTo Entering",item);
    var tbody=item.children[0];
    var children=tbody.children;
    var len=children.length;
    var active=false;
    var tail;
    var active=false;
    for (var ii=len-1;ii>=0;ii--){
	//console.log("RemoveTableChildFromTo ",ii,children[ii],len,children.length);
	if (children[ii] !== undefined) {
	    if (children[ii].getAttribute !== undefined) {
		var att=children[ii].getAttribute("id");
		//console.log("Child with Attribute:",ii,att);
		if (! active) {
		    if (att !== undefined & att === to) {
			tail=children[ii];
			//console.log("Tail child:",ii,att);
			active=true;
		    }
		} else {
		    if (att !== undefined & att === from) {
			//console.log("Head child:",ii,att);
			active=false;
			ii=-1;
		    } else {
			//console.log("Removing child:",ii,att);
			tbody.removeChild(children[ii]);
		    }
		}
	    } else {
		//console.log("Child:",ii,children[ii]);
		tbody.removeChild(children[ii]);
	    }
	    //children[ii].parentNode.removeChild(children[ii]);
	}
    };
    //console.log("removeTableChildFromTo Done",tail);
    return tail;
}
function removeTableChildFrom(item,from) {
    //console.log("removeTableChildFrom Entering",item);
    var tbody=item.children[0];
    var children=tbody.children;
    var len=children.length;
    var active=false;
    var head;
    var active=true;
    for (var ii=len-1;ii>=0;ii--){
	//console.log("RemoveTableChildFromTo ",ii,children[ii],len,children.length);
	if (children[ii] !== undefined) {
	    if (children[ii].getAttribute !== undefined) {
		var att=children[ii].getAttribute("id");
		//console.log("Child with Attribute:",ii,att);
		if (! active) {
		    if (att !== undefined) {
			//console.log("Keeping child:",ii,att);
		    }
		} else {
		    if (att !== undefined & att === from) {
			//console.log("Head child:",ii,att);
			head=children[ii];
			active=false;
			ii=-1;
		    } else {
			//console.log("Removing child:",ii,att);
			tbody.removeChild(children[ii]);
		    }
		}
	    } else {
		//console.log("Child:",ii,children[ii]);
		tbody.removeChild(children[ii]);
	    }
	    //children[ii].parentNode.removeChild(children[ii]);
	}
    };
    //console.log("removeTableChildFrom Done",head);
    return head;
}
function getChild(item,target) {
    var tbody=item.children[0];
    var children=tbody.children;
    var len=children.length;
    for (var ii=len-1;ii>=0;ii--){
	if (children[ii] !== undefined) {
	    if (children[ii].getAttribute !== undefined) {
		var att=children[ii].getAttribute("id");
		if (att === target) {
		    return children[ii];
		};
	    }
	}
    };
    //console.log("getChild Done",tail);
    return;
}
function addChildButton(item,text,onclick,cls) {
    var btn=document.createElement("BUTTON");
    var t=document.createTextNode(text);
    var br=document.createElement("BR");
    btn.appendChild(t);
    btn.setAttribute("onclick",onclick);
    btn.setAttribute("style","width:100%");
    if (typeof cls !== 'undefined') {btn.setAttribute("class",cls);};
    item.appendChild(btn);
    item.appendChild(br);
};
function addFunctionButtons( item, target) {
    addChildButton(item,"sec1970(,,,,,)","addValue('"+target+"','sec1970(,,,,,)');");
    addChildButton(item,"julian(,,,,,)","addValue('"+target+"','julian(,,,,,)');");
    addChildButton(item,"abs()","addValue('"+target+"','abs()');");
    addChildButton(item,"exp()","addValue('"+target+"','exp()');");
    addChildButton(item,"log10()","addValue('"+target+"','log10()');");
    addChildButton(item,"log()","addValue('"+target+"','log()');");
    addChildButton(item,"sqrt()","addValue('"+target+"','sqrt()');");
    addChildButton(item,"min(...)","addValue('"+target+"','min(,)');");
    addChildButton(item,"max(...)","addValue('"+target+"','max(,)');");
    addChildButton(item,"sin()","addValue('"+target+"','sin()');");
    addChildButton(item,"cos()","addValue('"+target+"','cos()');");
    addChildButton(item,"tan()","addValue('"+target+"','tan()');");
    addChildButton(item,"asin()","addValue('"+target+"','asin()');");
    addChildButton(item,"acos()","addValue('"+target+"','acos()');");
    addChildButton(item,"atan2(,)","addValue('"+target+"','atan2(,)');");
};

function addLogicalButtons( item, target) {
    addChildButton(item,"msgmax(var(:))","addValue('"+target+"','msgmax()');");
    addChildButton(item,"msgmin(var(:))","addValue('"+target+"','msgmin()');");
    addChildButton(item,"msgclosest(var(:),trg1,trg2...)","addValue('"+target+"','msgclosest(,,)');");
    addChildButton(item,"ismember(var,trg1,trg2...)","addValue('"+target+"','ismember(,,)');");
    addChildButton(item,"isabove(var,max1,max2...)","addValue('"+target+"','isabove(,)');");
    addChildButton(item,"isbelow(var,min1,min2...)","addValue('"+target+"','isbelow(,)');");
    addChildButton(item,"isbetween(var,max1,min1,min2...)","addValue('"+target+"','isbetween(,,)');");
    addChildButton(item,"thinned(percent_thinned)","addValue('"+target+"','thinned(0.0)');");
    addChildButton(item,"and(l1,l2,l3...)","addValue('"+target+"','and(,)');");
    addChildButton(item,"or(l1,l2,l3...)","addValue('"+target+"','or(,)');");
    addChildButton(item,"not(l1,l2,l3...)","addValue('"+target+"','not(,)');");
}

function addWildcardButtons( item, target) {
    addChildButton(item,"YY (year)","addValue('"+target+"','YY');");
    addChildButton(item,"MM (month)","addValue('"+target+"','MM');");
    addChildButton(item,"DD (day)","addValue('"+target+"','DD');");
    addChildButton(item,"HH (hour)","addValue('"+target+"','HH');");
    addChildButton(item,"MI (minute)","addValue('"+target+"','MI');");
    addChildButton(item,"SS (second)","addValue('"+target+"','SS');");
};
function showDropdown(target, arg = "") {
    var dropdown=target + 'Dropdown';
    var item=document.getElementById(dropdown);
    console.log("Table='" + item.style.display + "'  target='"+target+"'");
    item.classList.toggle("show");
    if (item.style.display === 'block' ) {return;}
    console.log("Dropdown arg='"+arg+"'");
    if (target === 'modelConfigFile') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent model-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"model",arg:args},function(data, status){
	    var ret=dataToArray(data,status,documentLog);
	    var root=ret[0]||{};
	    console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    if (args.length >0 && looksLikeFile(args[0])) {
		var file=getFile(args[0]);
	    } else {
		var file="";
	    };
	    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
	    console.log("Found entries: ",dirs.length-1,root);
	    var parent=dirs[0];
	    if (parent != null) {
		var dd=parent;
		addChildButton(item,"<up>","model_setConfigFile('"+dd+"');");
	    }
	    if (args.length == 1) {
		//console.log("Arg ret:",ret);
		if (root["type"] == "dir" && root["loc"] != "") {
		    addChildButton(item,"<rmdir>","model_rmdir('"+args[0]+"');");
		} else if (root["type"] == "file") {
		    addChildButton(item,"<rmfile>","model_rmfile('"+args[0]+"');");
		} else if (root["type"] == "unknown") {
		    if (looksLikeFile(args[0])) {
			addChildButton(item,"<mkfile>","model_mkfile('"+args[0]+"');model_show();");
		    } else {
			addChildButton(item,"<mkdir>","model_mkdir('"+args[0]+"');");
		    }
		}
	    } else if (args.length == 2) {
		if (root["type"] == "dir") {
		    addChildButton(item,"<cpdir>","model_cpdir('"+args[0]+"','"+args[1]+"');");
		} else if (root["type"] == "file") {
		    addChildButton(item,"<cpfile>","model_cpfile('"+args[0]+"','"+args[1]+"');model_setConfigFile('"+args[2]+"');model_show();");
		} else if (root["type"] == "unknown") {
		}
	    };
	    //for (var model in model_config) {
		//console.log("Adding config button: ",model);
		//addChildButton(item,model,"model_setConfigFile('"+model+"');model_show();");
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
		console.log("Adding dir button: ",dd);
		addChildButton(item,dd,"model_setConfigFile('"+dd+"');model_show();");
	    }
	    documentLog.innerHTML="";
	});
    } else if (target === 'modelFilterDir') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent model-load request.";
	var file=model_getConfigFile();
	var path=args[0] || "";
	var cls = "data";
	var filter=model_config[file]["filterFile"];
	$.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:cls,path:path,filter:filter},
	      function(data, status){
		  removeChildren(item);
		  var errors=data.getElementsByTagName("error");
		  if (errors.length > 0 ) {
		      console.log("Error:",data);
		      var msg=(errors[0].getAttribute("message")||"");
		      alert("Unable to list: "+path+"\n"+msg);
		  } else {
		      var ls=data.getElementsByTagName("ls");
		      if (ls.length > 0) {
			  var root=ls[0].getAttribute("root");
			  var loc=ls[0].getAttribute("location");
			  var pdirs=getSubDirs(cls,root,loc,"");
			  var parent=pdirs[0];
			  console.log("Found parent: ",root,loc,parent);
			  if (parent != null) {
			      var dd=root+parent;
			      addChildButton(item,"<up>",
					     "model_setArray('filterDir','"+dd+"');model_show();");
			  };
			  var dirs=ls[0].getElementsByTagName("dir");
			  console.log("Found dir entries: ",dirs.length);
			  for (var ii=0; ii< dirs.length; ii++) {
			      var dd = dirs[ii].getAttribute("path");
			      console.log("Adding dir button: ",dd);
			      addChildButton(item,dd,"model_setArray('filterDir','"+dd+"');model_show();");
			  };
			  var patts=ls[0].getElementsByTagName("pattern");
			  console.log("Found file entries: ",patts.length);
			  for (var ii=0; ii< patts.length; ii++) {
			      var rr = getFile(patts[ii].getAttribute("regexp"));
			      var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
			      if (dd !== '') {
				  console.log("Adding file button: ",dd,rr);
				  addChildButton(item,dd,"model_setArray('filterFile','"+rr+"');model_show();",'shaded');
			      };
			  };
			  var fils=ls[0].getElementsByTagName("file");
			  console.log("Found file entries: ",fils.length);
			  for (var ii=0; ii< fils.length; ii++) {
			      var dd = getFile(fils[ii].getAttribute("path"));
			      var size = fils[ii].getAttribute("size")
			      if (dd !== '') {
				  console.log("Adding file button: ",dd);
				  addChildButton(item,size+" "+dd,"model_setArray('filterFile','"+dd+"');model_show();");
			      };
			  };
		      };
		  };
		  documentLog.innerHTML="";
	      });
    } else if (target === 'modelFilterFile') { //***********************************
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
				     },
	      function(data, status){
		  if (status == "success") {
		      var errors=data.getElementsByTagName("error");
		      if (errors.length > 0 ) {
			  console.log("Error:",data);
			  var msg=(errors[0].getAttribute("message")||"");
			  alert("Unable to peek at: "+filterDir+" "+filterFile+" (file:"+file+")\n"+msg);
		      } else {
			  dataToArray(data,status,documentLog);
			  setInnerHTML('modelPatternHits',model_config[file]["hits"]);
			  removeChildren(item);
			  var len=model_config[file]["files"].length;
			  for (var ii=0; ii<len;ii++) {
			      var sfile=model_config[file]["files"][ii][0];
			      var sage=parseFloat(model_config[file]["files"][ii][1]).toFixed(2);
			      var ssize=model_config[file]["files"][ii][2];
			      addChildButton(item,ssize+" "+sfile+" ("+sage+"d)","model_fileFind('"+sfile+"');");
			  }
		      };
		      documentLog.innerHTML="";
		  }
	      }
	     );
    } else if (target === 'modelIndex') { //***********************************
	var file=model_getConfigFile();
	removeChildren(item);
 	if (model_config[file] !== undefined) {
	    var variables=model_config[file]["variables"];
	    if (variables !== undefined) {
		for (var variable in variables) {
		    var fullname=variable;
		    var dims=model_config[file]["variables"][variable];
		    if (dims != null) {fullname=fullname+"("+dims+")";};
		    addChildButton(item,fullname,"model_setArray('indexVariable','"+variable+"');model_show();");
		}
	    }
	}
    } else if (target === 'obsConfigFile') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent obs-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"obs",arg:args},function(data, status){
	    var ret=dataToArray(data,status,documentLog);
	    var root=ret[0]||{};
	    console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    if (args.length >0 && looksLikeFile(args[0])) {
		var file=getFile(args[0]);
	    } else {
		var file="";
	    };
	    // add directories...
	    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
	    console.log("Found entries: ",dirs.length-1,root);
	    var parent=dirs[0];
	    if (parent != null) {
		var dd=parent;
		addChildButton(item,"<up>","obs_setConfigFile('"+dd+"');");
	    }
	    if (args.length == 1) {
		//console.log("Arg ret:",ret);
		if (root["type"] == "dir" && root["loc"] != "") {
		    addChildButton(item,"<rmdir>","obs_rmdir('"+args[0]+"');");
		} else if (root["type"] == "file") {
		    addChildButton(item,"<rmfile>","obs_rmfile('"+args[0]+"');");
		} else if (root["type"] == "unknown") {
		    if (looksLikeFile(args[0])) {
			addChildButton(item,"<mkfile>","obs_mkfile('"+args[0]+"');obs_show();");
		    } else {
			addChildButton(item,"<mkdir>","obs_mkdir('"+args[0]+"');");
		    }
		}
	    } else if (args.length == 2) {
		if (root["type"] == "dir") {
		    addChildButton(item,"<cpdir>","obs_cpdir('"+args[0]+"','"+args[1]+"');");
		} else if (root["type"] == "file") {
		    addChildButton(item,"<cpfile>","obs_cpfile('"+args[0]+"','"+args[1]+"');obs_setConfigFile('"+args[2]+"');obs_show();");
		} else if (root["type"] == "unknown") {
		}
	    };
	    //for (var obs in obs_config) {
		//console.log("Adding config button: ",obs);
		//addChildButton(item,obs,"obs_setConfigFile('"+obs+"');obs_show();");
	    //}
	    for (var ii=1;ii<dirs.length;ii++) {
		var dir=dirs[ii];
		if (root["loc"] == "" || root["loc"] == ".") {
		    var dd = dir;
		} else {
		    var dd = root["loc"]+dir;
		};
		//if (dd.substr(dd.length-1) == "/" || dd == "") {
		    //dd=dd + file;
		//}
		console.log("Adding dir button: ",dd);
		addChildButton(item,dd,"obs_setConfigFile('"+dd+"');obs_show();");
	    }
	    documentLog.innerHTML="";
	});
	// documentLog.innerHTML="Sent obs-load request.";
	// $.get("cgi-bin/fark_load.pl",{type:"obs",arg:args},function(data, status){
	//     dataToArray(data,status,documentLog);
	//     //console.log("Updating dropdown for ",target);
	//     removeChildren(item);
	//     for (var obs in obs_config) {
	// 	addChildButton(item,obs,"obs_setConfigFile('"+obs+"');obs_show();");
	//     };
	//     documentLog.innerHTML="";
	// });
    } else if (target === 'obsFilterDir') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent obs-load request.";
	var file=obs_getConfigFile();
	var path=args[0] || "";
	var cls = "data";
	var filter=obs_config[file]["filterFile"];
	$.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:cls,path:path,filter:filter},
	      function(data, status){
		  removeChildren(item);
		  var errors=data.getElementsByTagName("error");
		  if (errors.length > 0 ) {
		      console.log("Error:",data);
		      var msg=(errors[0].getAttribute("message")||"");
		      alert("Unable to list: "+path+"\n"+msg);
		  } else {
		      var ls=data.getElementsByTagName("ls");
		      if (ls.length > 0) {
			  var root=ls[0].getAttribute("root");
			  var loc=ls[0].getAttribute("location");
			  var pdirs=getSubDirs(cls,root,loc,"");
			  var parent=pdirs[0];
			  console.log("Found parent: ",root,loc,parent);
			  if (parent != null) {
			      var dd=root+parent;
			      addChildButton(item,"<up>",
					     "obs_setArray('filterDir','"+dd+"');obs_show();");
			  };
			  var dirs=ls[0].getElementsByTagName("dir");
			  console.log("Found dir entries: ",dirs.length);
			  for (var ii=0; ii< dirs.length; ii++) {
			      var dd = dirs[ii].getAttribute("path");
			      console.log("Adding dir button: ",dd);
			      addChildButton(item,dd,"obs_setArray('filterDir','"+dd+"');obs_show();");
			  };
			  var patts=ls[0].getElementsByTagName("pattern");
			  console.log("Found file entries: ",patts.length);
			  for (var ii=0; ii< patts.length; ii++) {
			      var rr = getFile(patts[ii].getAttribute("regexp"));
			      var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
			      if (dd !== '') {
				  console.log("Adding file button: ",dd,rr);
				  addChildButton(item,dd,"obs_setArray('filterFile','"+rr+"');obs_show();",'shaded');
			      };
			  };
			  var fils=ls[0].getElementsByTagName("file");
			  console.log("Found file entries: ",fils.length);
			  for (var ii=0; ii< fils.length; ii++) {
			      var dd = getFile(fils[ii].getAttribute("path"));
			      var size = fils[ii].getAttribute("size")
			      if (dd !== '') {
				  console.log("Adding file button: ",dd);
				  addChildButton(item,size+" "+dd,"obs_setArray('filterFile','"+dd+"');obs_show();");
			      };
			  };
		      };
		  };
		  documentLog.innerHTML="";
	      });
    } else if (target === 'obsFilterFile') { //***********************************
	var file=obs_getConfigFile();
	var password=document.getElementById("obsConfigFilePsw").value;
	var filterDir = obs_config[file]["filterDir"];
	var filterDirMin = obs_config[file]["filterDirMin"];
	var filterDirMax = obs_config[file]["filterDirMax"];
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
	$.get("cgi-bin/fark_find.pl",{type:"obs",
				      file:file,
				      password:password,
				      filterDir:filterDir,
				      filterDirMin:filterDirMin,
				      filterDirMax:filterDirMax,
				      filterFile:filterFile,
				      table:obs_config[file]["tablePath"],
				      obsTargets:obsTargets,
				      indexTarget:indexTarget,
				      indexExp:indexExp,
				      bufrType:bufrType,
				      subType:subType,
				      typeInfo:typeInfo},
	      function(data, status){
		  if (status == "success") {
		      var errors=data.getElementsByTagName("error");
		      if (errors.length > 0 ) {
			  console.log("Error:",data);
			  var msg=(errors[0].getAttribute("message")||"");
			  alert("Unable to peek at: "+filterDir+" "+filterFile+" (file:"+file+")\n"+msg);
		      } else {
			  dataToArray(data,status,documentLog);
			  setInnerHTML('obsPatternHits',obs_config[file]["hits"]);
			  removeChildren(item);
			  var len=obs_config[file]["files"].length;
			  for (var ii=0; ii<len;ii++) {
			      var sfile=obs_config[file]["files"][ii][0];
			      var sage=parseFloat(obs_config[file]["files"][ii][1]).toFixed(2);
			      var ssize=obs_config[file]["files"][ii][2];
			      addChildButton(item,ssize+" "+sfile+" ("+sage+"d)","obs_fileFind('"+sfile+"');");
			  }
		      };
		      documentLog.innerHTML="";
		  }
	      }
	     );
    } else if (target === 'obsTablePath') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent obs-load request.";
	var path=args[0] || "";
	var cls = "tables";
	$.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:cls,path:path},
	      function(data, status){
		  removeChildren(item);
		  var errors=data.getElementsByTagName("error");
		  if (errors.length > 0 ) {
		      console.log("Error:",data);
		      var msg=(errors[0].getAttribute("message")||"");
		      alert("Unable to list: "+path+"\n"+msg);
		  } else {
		      var ls=data.getElementsByTagName("ls");
		      if (ls.length > 0) {
			  var root=ls[0].getAttribute("root");
			  var loc=ls[0].getAttribute("location");
			  var pdirs=getSubDirs(cls,root,loc,"");
			  var parent=pdirs[0];
			  console.log("Found parent: ",root,loc,parent);
			  if (parent != null) {
			      var dd=root+parent;
			      addChildButton(item,"<up>",
					     "obs_setArray('tablePath','"+dd+"');obs_show();");
			  };
			  var dirs=ls[0].getElementsByTagName("dir");
			  console.log("Found dir entries: ",dirs.length);
			  for (var ii=0; ii< dirs.length; ii++) {
			      var dd = dirs[ii].getAttribute("path");
			      console.log("Adding dir button: ",dd);
			      addChildButton(item,dd,"obs_setArray('tablePath','"+dd+"');obs_show();");
			  };
			  var patts=ls[0].getElementsByTagName("pattern");
			  console.log("Found file entries: ",patts.length);
			  for (var ii=0; ii< patts.length; ii++) {
			      var rr = getFile(patts[ii].getAttribute("regexp"));
			      var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
			      if (dd !== '') {
				  console.log("Adding file button: ",dd,rr);
				  addChildButton(item,dd,"obs_setArray('filterFile','"+rr+"');obs_show();",'shaded');
			      };
			  };
			  var fils=ls[0].getElementsByTagName("file");
			  console.log("Found file entries: ",fils.length);
			  for (var ii=0; ii< fils.length; ii++) {
			      var dd = getFile(fils[ii].getAttribute("path"));
			      if (dd !== '') {
				  console.log("Adding file button: ",dd);
				  addChildButton(item,dd,"");
			      };
			  };
		      };
		  };
		  documentLog.innerHTML="";
	      });
    } else if (target === 'obsBufrType') {  //***********************************
	var file=obs_getConfigFile();
	removeChildren(item);
	if (obs_config[file] !== undefined) {
	    var bufr=obs_config[file]["bufr"];
	    if (bufr !== undefined) {
		for (var bufrType in bufr) {
		    var info=obs_config[file]["bufr"][bufrType]["info"] || "";
		    var cnt=obs_config[file]["bufr"][bufrType]["cnt"] || "";
		    var ccnt="";
		    if (cnt !== "") {
			ccnt=" ("+cnt+")";
		    }
		    addChildButton(item,bufrType+" "+info+ccnt,"obs_setArray('bufrType','"+bufrType+"');showValue('obsBufrType','"+bufrType+"');");
		}
	    }
	}
    } else if (target === 'obsSubType') {  //***********************************
	var file=obs_getConfigFile();
	var bufrType=obs_config[file]["bufrType"];
	removeChildren(item);
	if (obs_config[file] !== undefined) {
	    var bufr=obs_config[file]["bufr"];
	    if (bufr !== undefined) {
		for (var subType in bufr[bufrType]) {
		    if (subType !== "info" && subType !== "cnt")  {
			var info=bufr[bufrType][subType]["info"];
			var cnt=bufr[bufrType][subType]["cnt"] || "";
			var ccnt="";
			if (cnt !== "") {
			    ccnt=" ("+cnt+")";
			}
			addChildButton(item,subType+" : "+info+ccnt,"obs_setArray('subType','"+subType+"');obs_setArray('typeInfo','"+info+"');showValue('obsSubType','"+subType+"');showValue('obsTypeInfo','"+info+"');");
		    }
		}
	    }
	}
    } else if (target === 'obsIndexPOS') { //***********************************
	var file=obs_getConfigFile();
	var bufrType=obs_config[file]["bufrType"];
	var subType=obs_config[file]["subType"];
	removeChildren(item);
	if (bufrType !== undefined && bufrType !== "" &&
	    subType !== undefined && subType !== "" && subType !== "info" &&subType !== "cnt" &&
	    obs_config[file] !== undefined && 
	    obs_config[file]["bufr"] !== undefined && 
	    obs_config[file]["bufr"][bufrType] !== undefined && 
	    obs_config[file]["bufr"][bufrType][subType] !== undefined) {
	    var bufr=obs_config[file]["bufr"][bufrType][subType];
	    for (var pos in bufr) {
		if (pos !== "info" && pos !== "cnt")  {
		    var descr=bufr[pos]["descr"];
		    var info=bufr[pos]["info"];
		    if (descr == "31001") {
			addChildButton(item,pos+" : "+descr+" "+info,"showValue('obsIndexPOS','"+pos+"');showValue('obsIndexDESCR','"+descr+"');showValue('obsIndexInfo','"+info+"');","shaded");
		    } else {
			addChildButton(item,pos+" : "+descr+" "+info,"showValue('obsIndexPOS','"+pos+"');showValue('obsIndexDESCR','"+descr+"');showValue('obsIndexInfo','"+info+"');");
		    }
		}
	    }
	}
    } else if (target === 'obsIndexExp') { //***********************************
	var file=obs_getConfigFile();
	var bufrType=obs_config[file]["bufrType"];
	var subType=obs_config[file]["subType"];
	removeChildren(item);
	if ( obs_config[file] !== undefined &&
	     obs_config[file]["targets"] !== undefined ) {
	    for (var target in obs_config[file]["targets"]) {
		addChildButton(item,target,"addValue('obsIndexExp','"+target+"');");
	    }
	    addFunctionButtons(item,target);
	}
    } else if (target === 'colocConfigFile') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent coloc-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"coloc",arg:args},function(data, status){
	    var ret=dataToArray(data,status,documentLog);
	    var root=ret[0]||{};
	    console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    if (args.length >0 && looksLikeFile(args[0])) {
		var file=getFile(args[0]);
	    } else {
		var file="";
	    };
	    // add directories...
	    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
	    console.log("Found entries: ",dirs.length-1,root);
	    var parent=dirs[0];
	    if (parent != null) {
		var dd=parent;
		console.log("Adding <up> button: '"+dd+"'");
		//addChildButton(item,"<up>","coloc_setConfigFile('"+dd+"');coloc_show();");
		addChildButton(item,"<up>","coloc_setConfigFile2('"+dd+"');");
	    }
	    if (args.length == 1) {
		//console.log("Arg ret:",ret);
		if (root["type"] == "dir" && root["loc"] != "") {
		    console.log("Adding <rmdir> button: ",args[0]);
		    addChildButton(item,"<rmdir>","coloc_rmdir('"+args[0]+"');");
		} else if (root["type"] == "file") {
		    console.log("Adding <rmfile> button: ",args[0]);
		    addChildButton(item,"<rmfile>","coloc_rmfile('"+args[0]+"');");
		} else if (root["type"] == "unknown") {
		    if (looksLikeFile(args[0])) {
			console.log("Adding <mkfile> button: ",args[0]);
			addChildButton(item,"<mkfile>","coloc_mkfile('"+args[0]+"');coloc_show();");
		    } else {
			console.log("Adding <mkdir> button: ",args[0]);
			addChildButton(item,"<mkdir>","coloc_mkdir('"+args[0]+"');");
		    }
		}
	    } else if (args.length == 2) {
		if (root["type"] == "dir") {
		    console.log("Adding <cpdir> button: ",args[0],args[1]);
		    addChildButton(item,"<cpdir>","coloc_cpdir('"+args[0]+"','"+args[1]+"');");
		} else if (root["type"] == "file") {
		    console.log("Adding <cpfile> button: ",args[0],args[1]);
		    addChildButton(item,"<cpfile>","coloc_cpfile('"+args[0]+"','"+args[1]+"');coloc_setConfigFile('"+args[2]+"');coloc_show();");
		} else if (root["type"] == "unknown") {
		}
	    };
	    //for (var coloc in coloc_config) {
		//console.log("Adding config button: ",coloc);
		//addChildButton(item,coloc,"coloc_setConfigFile('"+coloc+"');coloc_show();");
	    //}
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
		console.log("Adding button: ",dd);
		addChildButton(item,dd,"coloc_setConfigFile('"+dd+"');coloc_show();");
	    }
	    documentLog.innerHTML="";
	});
	// documentLog.innerHTML="Sent coloc-load request.";
	// $.get("cgi-bin/fark_load.pl",{type:"coloc",arg:args},function(data, status){
	//     dataToArray(data,status,documentLog);
	//     //console.log("Updating dropdown for ",target);
	//     removeChildren(item);
	//     for (var coloc in coloc_config) {
	// 	addChildButton(item,coloc,"coloc_setConfigFile('"+coloc+"');coloc_show();");
	//     }
	//     documentLog.innerHTML="";
	// });
    } else if (target === 'colocModelConfigFile') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent model-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"model",arg:args},function(data, status){
	    var ret=dataToArray(data,status,documentLog);
	    var root=ret[0]||{};
	    //console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    if (args.length >0 && looksLikeFile(args[0])) {
		var file=getFile(args[0]);
	    } else {
		var file="";
	    };
	    // add directories...
	    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
	    console.log("Found entries: ",dirs.length-1,root);
	    var parent=dirs[0];
	    if (parent != null) {
		var dd=parent;
		console.log("Adding up button: ",dd);
		addChildButton(item,"<up>","coloc_setConfig('modelConfigFile','file','"+dd+"');coloc_show();");
	    }
	    for (var ii=1;ii<dirs.length;ii++) {
		var dir=dirs[ii];
		if (root["loc"] == "" || root["loc"] == ".") {
		    var dd = dir;
		} else {
		    var dd = root["loc"]+dir;
		};
		// if (dd.substr(dd.length-1) == "/" || dd == "") {
		//     dd=dd + file;
		// }
		console.log("Adding dir button: ",dd);
		addChildButton(item,dd,"coloc_setConfig('modelConfigFile','file','"+dd+"');coloc_updateModelData('"+dd+"');coloc_show();");
	    }
	    // for (var model in model_config) {
	    // 	addChildButton(item,model,"coloc_setConfig('modelConfigFile','file','"+model+"');coloc_show();");
	    // }
	    addChildButton(item,"<none>","coloc_setConfig('modelConfigFile','file','');coloc_show();");
	    documentLog.innerHTML="";
	});
    } else if (target === 'colocModelTargetVariable') { //***********************************
	var file=coloc_getModelConfigFile();
	removeChildren(item);
 	if (model_config[file] !== undefined) {
	    for (var dim in model_config[file]["dimensions"]) {
		var dimname=dim;
		var dimv=model_config[file]["dimensions"][dim];
		if (dimv!= null) {dimname="("+dimname+") 1:"+dimv;};
		addChildButton(item,dimname,"showValue('colocModelTargetVariable','("+dim+")');",'shaded');
	    }
	    for (var variable in model_config[file]["variables"]) {
		var fullname=variable;
		var dims=model_config[file]["variables"][variable];
		if (dims!= null) {fullname=fullname+"("+dims+")";};
		addChildButton(item,fullname,"showValue('colocModelTargetVariable','"+variable+"');");
	    }
	}
    } else if (target === 'colocObsConfigFile') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent obs-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"obs",arg:args},function(data, status){
	    var ret=dataToArray(data,status,documentLog);
	    var root=ret[0]||{};
	    //console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    if (args.length >0 && looksLikeFile(args[0])) {
		var file=getFile(args[0]);
	    } else {
		var file="";
	    };
	    // add directories...
	    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
	    console.log("Found entries: ",dirs.length-1,root);
	    var parent=dirs[0];
	    if (parent != null) {
		var dd=parent;
		console.log("Adding up button: ",dd);
		addChildButton(item,"<up>","coloc_setConfig('obsConfigFile','file','"+dd+"');coloc_show();");
	    }
	    for (var ii=1;ii<dirs.length;ii++) {
		var dir=dirs[ii];
		if (root["loc"] == "" || root["loc"] == ".") {
		    var dd = dir;
		} else {
		    var dd = root["loc"]+dir;
		};
		// if (dd.substr(dd.length-1) == "/" || dd == "") {
		//     dd=dd + file;
		// }
		console.log("Adding dir button: ",dd);
		addChildButton(item,dd,"coloc_setConfig('obsConfigFile','file','"+dd+"');coloc_updateObsData('"+dd+"');coloc_show();");
	    }
	    // for (var obs in obs_config) {
	    // 	addChildButton(item,obs,"coloc_setConfig('obsConfigFile','file','"+obs+"');coloc_show();");
	    // }
	    addChildButton(item,"<none>","coloc_setConfig('obsConfigFile','file','');coloc_show();");
	    documentLog.innerHTML="";
	});
    } else if (target === 'colocXML') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent dir-load request.";
	var path=args[0] || "";
	var cls = "output";
	$.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:cls,path:path},
	      function(data, status){
		  removeChildren(item);
		  addWildcardButtons(item,target);
		  var errors=data.getElementsByTagName("error");
		  if (errors.length > 0 ) {
		      var msg=(errors[0].getAttribute("message")||"");
		      console.log("Error:"+path+"  "+msg);
		      //alert("Unable to list: "+path+"\n"+msg);
		  } else {
		      var ls=data.getElementsByTagName("ls");
		      if (ls.length > 0) {
			  var root=ls[0].getAttribute("root");
			  var loc=ls[0].getAttribute("location");
			  var pdirs=getSubDirs(cls,root,loc,"");
			  var parent=pdirs[0];
			  console.log("Found parent: ",root,loc,parent);
			  if (parent != null) {
			      var dd=root+parent;
			      addChildButton(item,"<up>",
					     "coloc_setArray('xml','"+dd+"');coloc_show();");
			  };
			  var dirs=ls[0].getElementsByTagName("dir");
			  console.log("Found dir entries: ",dirs.length);
			  for (var ii=0; ii< dirs.length; ii++) {
			      var dd = dirs[ii].getAttribute("path");
			      console.log("Adding dir button: ",dd);
			      addChildButton(item,dd,"coloc_setArray('xml','"+dd+"');coloc_show();");
			  };
			  var patts=ls[0].getElementsByTagName("pattern");
			  console.log("Found file entries: ",patts.length);
			  for (var ii=0; ii< patts.length; ii++) {
			      var rr = getFile(patts[ii].getAttribute("regexp"));
			      var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
			      if (dd !== '') {
				  console.log("Adding file button: ",dd,rr);
				  addChildButton(item,dd,"coloc_setArray('xml','"+rr+"');coloc_show();",'shaded');
			      };
			  };
			  var fils=ls[0].getElementsByTagName("file");
			  console.log("Found file entries: ",fils.length);
			  for (var ii=0; ii< fils.length; ii++) {
			      var dd = fils[ii].getAttribute("path");
			      var size = fils[ii].getAttribute("size")
			      if (dd !== '') {
				  console.log("Adding file button: ",dd,ii);
				  addChildButton(item,size+" "+dd,"coloc_setArray('xml','"+dd+"');coloc_show();");
			      };
			  };
		      };
		  };
		  documentLog.innerHTML="";
	      });
    } else if (target === 'colocObsPOS') { //***********************************
	var file=coloc_getObsConfigFile();
	var mfile=coloc_getModelConfigFile();
	if ( obs_config[file] !== undefined) {
	    var bufrType = obs_config[file]["bufrType"];
	    var subType = obs_config[file]["subType"];
	    removeChildren(item);
	    if (bufrType !== undefined && bufrType !== "" &&
		subType !== undefined && subType !== "" && subType !== "info" &&subType !== "cnt" &&
		obs_config[file] !== undefined && 
		obs_config[file]["bufr"] !== undefined && 
		obs_config[file]["bufr"][bufrType] !== undefined && 
		obs_config[file]["bufr"][bufrType][subType] !== undefined ) {
		var bufr=obs_config[file]["bufr"][bufrType][subType];
		for (var pos in bufr) {
		    if (pos !== "info" && pos !== "cnt")  {
			var descr=bufr[pos]["descr"];
			var info=bufr[pos]["info"];
			if (descr == "31001") {
			    addChildButton(item,pos+" : "+descr+" "+info,"showValue('colocObsPOS','"+pos+"');showValue('colocObsDESCR','"+descr+"');showValue('colocObsInfo','"+info+"');","shaded");
			} else {
			    addChildButton(item,pos+" : "+descr+" "+info,"showValue('colocObsPOS','"+pos+"');showValue('colocObsDESCR','"+descr+"');showValue('colocObsInfo','"+info+"');");
			}
		    }
		}
		// add internal variables...
		addChildButton(item," mid (Model File id)","showValue('colocObsPOS','mid');showValue('colocObsDESCR','');showValue('colocObsInfo','Model file id');",'shaded');
		addChildButton(item," oid (Observation File id)","showValue('colocObsPOS','oid');showValue('colocObsDESCR','');showValue('colocObsInfo','Observation file id');",'shaded');
		addChildButton(item," bid (Bufr message id)","showValue('colocObsPOS','bid');showValue('colocObsDESCR','');showValue('colocObsInfo','Bufr message id');",'shaded');
		addChildButton(item," sid (Sub message id)","showValue('colocObsPOS','sid');showValue('colocObsDESCR','');showValue('colocObsInfo','Sub message id');",'shaded');
		addChildButton(item," lid (Location id)","showValue('colocObsPOS','lid');showValue('colocObsDESCR','');showValue('colocObsInfo','Location id');",'shaded');
		// add dimensions...
 		if (model_config[mfile] !== undefined) {
		    for (var dim in model_config[mfile]["dimensions"]) {
			var dimname=dim;
			var dimv=model_config[mfile]["dimensions"][dim];
			if (dimv !=  null) {
			    addChildButton(item,"DUPLICATE (1:"+dimname+"="+dimv+")","showValue('colocObsPOS','');showValue('colocObsDESCR','');"+
					   "showValue('colocObsInfo','DUPLICATE (1:"+dimname+"="+dimv+")');showValue('colocObsMin','1');showValue('colocObsMax','"+dimname+"');");
			}
		    }
		}
	    }
	}
    } else if (target === 'matchModelTargetName') { //***********************************
	var file=coloc_getConfigFile();
	removeChildren(item);
	if ( coloc_config[file] !== undefined &&
	     coloc_config[file]["modelConfigFile"]["targets"] !== undefined 
	   ) {
	    for (var t in coloc_config[file]["modelConfigFile"]["targets"]) {
		addChildButton(item,t,"showValue('matchModelTargetName','"+t+"');");
	    }
	}
    } else if (target.substr(0,15) === 'matchExpression') {
	var cnt = target.substring(15);
	var file = coloc_getConfigFile();
	var mfile = coloc_getModelConfigFile();
	var ofile = coloc_getObsConfigFile();
	console.log("mfile:" + mfile);
	removeChildren(item);
	if ( coloc_config[file] !== undefined &&
	     model_config[mfile] !== undefined &&
	     obs_config[ofile] !== undefined
	   ) {
	    if (cnt == 0) {
		var indexTrg=obs_config[ofile]["indexTarget"];
		addChildButton(item,indexTrg,"addValue('"+target+"','"+indexTrg+"');coloc_addConfig('modelConfigFile','exp','"+indexTrg+"');","shaded");
	    } else {
		var trg=coloc_config[file]["modelConfigFile"]["targeto"][cnt-1];
		for (var t in coloc_config[file]["obsConfigFile"]["targets"]) {
		    addChildButton(item,t,"addValue('"+target+"','"+t+"');coloc_addConfigFilesTarget('modelConfigFile','"+trg+"','exp','"+t+"');");
		}
	    }
	}
	addFunctionButtons(item,target);
    } else if (target.substr(0,13) === 'colocDebugExp') {
	removeChildren(item);
	addLogicalButtons(item,target);
	addFunctionButtons(item,target);
    } else if (target === 'colocObsFilter') {
	var file = coloc_getConfigFile();
	var mfile = coloc_getModelConfigFile();
	var ofile = coloc_getObsConfigFile();
	removeChildren(item);
	if ( coloc_config[file] !== undefined &&
	     obs_config[ofile] !== undefined
	   ) {
	    // add obs-targets from obs-file, including index XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	    for (var t in coloc_config[file]["obsConfigFile"]["targets"]) {
		addChildButton(item,t,"addValue('"+target+"','"+t+"');");
	    }
	};
	addLogicalButtons(item,target);
	addFunctionButtons(item,target);
    } else if (target === 'colocModelFilter') {
	var file = coloc_getConfigFile();
	var mfile = coloc_getModelConfigFile();
	var ofile = coloc_getObsConfigFile();
	removeChildren(item);
	if ( coloc_config[file] !== undefined &&
	     model_config[mfile] !== undefined &&
	     obs_config[ofile] !== undefined
	   ) {
	    // add obs-targets from obs-file, including index XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	    for (var t in coloc_config[file]["obsConfigFile"]["targets"]) {
		addChildButton(item,t,"addValue('"+target+"','"+t+"');");
	    }
	    // add model index XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	    for (var t in coloc_config[file]["modelConfigFile"]["targets"]) {
		addChildButton(item,t,"addValue('"+target+"','"+t+"');");
	    }
	}
	addLogicalButtons(item,target);
	addFunctionButtons(item,target);
    } else if (target === 'plotConfigFile') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent plot-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"plot",arg:args},function(data, status){
	    var ret=dataToArray(data,status,documentLog);
	    var root=ret[0]||{};
	    console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    if (args.length >0 && looksLikeFile(args[0])) {
		var file=getFile(args[0]);
	    } else {
		var file="";
	    };
	    // add directories...
	    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
	    console.log("Found entries: ",dirs.length-1,root);
	    var parent=dirs[0];
	    if (parent != null) {
		var dd=parent;
		console.log("Adding up button: ",dd);
		addChildButton(item,"<up>","plot_setConfigFile('"+dd+"');");
	    }
	    if (args.length == 1) {
		//console.log("Arg ret:",ret);
		if (root["type"] == "dir" && root["loc"] != "") {
		    addChildButton(item,"<rmdir>","plot_rmdir('"+args[0]+"');");
		} else if (root["type"] == "file") {
		    addChildButton(item,"<rmfile>","plot_rmfile('"+args[0]+"');");
		} else if (root["type"] == "unknown") {
		    if (looksLikeFile(args[0])) {
			addChildButton(item,"<mkfile>","plot_mkfile('"+args[0]+"');plot_show();");
		    } else {
			addChildButton(item,"<mkdir>","plot_mkdir('"+args[0]+"');");
		    }
		}
	    } else if (args.length == 2) {
		if (root["type"] == "dir") {
		    addChildButton(item,"<cpdir>","plot_cpdir('"+args[0]+"','"+args[1]+"');");
		} else if (root["type"] == "file") {
		    addChildButton(item,"<cpfile>","plot_cpfile('"+args[0]+"','"+args[1]+"');plot_setConfigFile('"+args[2]+"');");
		} else if (root["type"] == "unknown") {
		}
	    };
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
		console.log("Adding dir button: ",dd);
		addChildButton(item,dd,"plot_setConfigFile('"+dd+"');plot_show();");
	    }
	    documentLog.innerHTML="";
	});
    } else if (target === 'plotCat') { //***********************************
	var args=getArgs(arg);
	//documentLog.innerHTML="Sent cat-load request.";
	//$.get("cgi-bin/fark_load.pl",{type:"cat",arg:args},function(data, status){
	    //var ret=dataToArray(data,status,documentLog);
	    //var root=ret[0];
	    console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    for (var cat in plot_org_cats) {
		console.log("Adding config button: ",cat);
		addChildButton(item,cat,"plot_setCat('"+cat+"');showValue('plotCat','"+cat+"');plot_show()");
	    }
	    //documentLog.innerHTML="";
	//});
    } else if (target === 'plotTable') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent dir-load request.";
	var path=args[0] || "";
	var cls = "output";
	$.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:cls,path:path},
	      function(data, status){
		  removeChildren(item);
		  addWildcardButtons(item,target);
		  var errors=data.getElementsByTagName("error");
		  if (errors.length > 0 ) {
		      var msg=(errors[0].getAttribute("message")||"");
		      console.log("Error:"+path+"  "+msg);
		      //alert("Unable to list: "+path+"\n"+msg);
		  } else {
		      var ls=data.getElementsByTagName("ls");
		      if (ls.length > 0) {
			  var root=ls[0].getAttribute("root");
			  var loc=ls[0].getAttribute("location");
			  var pdirs=getSubDirs(cls,root,loc,"");
			  var parent=pdirs[0];
			  console.log("Found parent: ",root,loc,parent);
			  if (parent != null) {
			      var dd=root+parent;
			      addChildButton(item,"<up>",
					     "plot_setArray('table','"+dd+"');plot_show();");
			  };
			  var dirs=ls[0].getElementsByTagName("dir");
			  console.log("Found dir entries: ",dirs.length);
			  for (var ii=0; ii< dirs.length; ii++) {
			      var dd = dirs[ii].getAttribute("path");
			      console.log("Adding dir button: ",dd);
			      addChildButton(item,dd,"plot_setArray('table','"+dd+"');plot_show();");
			  };
			  var patts=ls[0].getElementsByTagName("pattern");
			  console.log("Found file entries: ",patts.length);
			  for (var ii=0; ii< patts.length; ii++) {
			      var rr = getFile(patts[ii].getAttribute("regexp"));
			      var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
			      if (dd !== '') {
				  console.log("Adding file button: ",dd,rr);
				  addChildButton(item,dd,"plot_setArray('table','"+rr+"');plot_show();",'shaded');
			      };
			  };
			  var fils=ls[0].getElementsByTagName("file");
			  console.log("Found file entries: ",fils.length);
			  for (var ii=0; ii< fils.length; ii++) {
			      var dd = fils[ii].getAttribute("path");
			      var size = fils[ii].getAttribute("size")
			      if (dd !== '') {
				  console.log("Adding file button: ",dd,ii);
				  addChildButton(item,size+" "+dd,"plot_setArray('table','"+dd+"');plot_show();");
			      };
			  };
		      };
		  };
		  documentLog.innerHTML="";
	      });
    } else if (target === 'plotGraphics') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent dir-load request.";
	var path=args[0] || "";
	var cls = "output";
	$.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:cls,path:path},
	      function(data, status){
		  removeChildren(item);
		  addWildcardButtons(item,target);
		  var errors=data.getElementsByTagName("error");
		  if (errors.length > 0 ) {
		      var msg=(errors[0].getAttribute("message")||"");
		      console.log("Error:"+path+"  "+msg);
		      //alert("Unable to list: "+path+"\n"+msg);
		  } else {
		      var ls=data.getElementsByTagName("ls");
		      if (ls.length > 0) {
			  var root=ls[0].getAttribute("root");
			  var loc=ls[0].getAttribute("location");
			  var pdirs=getSubDirs(cls,root,loc,"");
			  var parent=pdirs[0];
			  console.log("Found parent: ",root,loc,parent);
			  if (parent != null) {
			      var dd=root+parent;
			      addChildButton(item,"<up>",
					     "plot_setArray('graphics','"+dd+"');plot_show();");
			  };
			  var dirs=ls[0].getElementsByTagName("dir");
			  console.log("Found dir entries: ",dirs.length);
			  for (var ii=0; ii< dirs.length; ii++) {
			      var dd = dirs[ii].getAttribute("path");
			      console.log("Adding dir button: ",dd);
			      addChildButton(item,dd,"plot_setArray('graphics','"+dd+"');plot_show();");
			  };
			  var patts=ls[0].getElementsByTagName("pattern");
			  console.log("Found file entries: ",patts.length);
			  for (var ii=0; ii< patts.length; ii++) {
			      var rr = getFile(patts[ii].getAttribute("regexp"));
			      var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
			      if (dd !== '') {
				  console.log("Adding file button: ",dd,rr);
				  addChildButton(item,dd,"plot_setArray('graphics','"+rr+"');plot_show();",'shaded');
			      };
			  };
			  var fils=ls[0].getElementsByTagName("file");
			  console.log("Found file entries: ",fils.length);
			  for (var ii=0; ii< fils.length; ii++) {
			      var dd = fils[ii].getAttribute("path");
			      var size = fils[ii].getAttribute("size")
			      if (dd !== '') {
				  console.log("Adding file button: ",dd,ii);
				  addChildButton(item,size+" "+dd,"plot_setArray('graphics','"+dd+"');plot_show();");
			      };
			  };
		      };
		  };
		  documentLog.innerHTML="";
	      });
    } else if (target === 'plotLine') { //***********************************
	var args=getArgs(arg);
	//documentLog.innerHTML="Sent line-load request.";
	//$.get("cgi-bin/fark_load.pl",{type:"cat",arg:args},function(data, status){
	    //var ret=dataToArray(data,status,documentLog);
	    //var root=ret[0];
	    console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    var file=plot_getConfigFile();
	    console.log("Looking for file:",file);
	    var cat=plot_config[file]["cat"];
	    for (var line in plot_cats[cat]["lines"]) {
		console.log("Adding config button: ",line);
		addChildButton(item,line+" ("+plot_cats[cat]["lines"][line]+")","showValue('plotLine','"+line+"');showValue('plotType',plot_cats['"+cat+"'][\"lines\"]['"+line+"']);");
	    }
	//documentLog.innerHTML="";
	//});

    } else if (target === 'plotColoc') { //***********************************
	var args=getArgs(arg);
	documentLog.innerHTML="Sent coloc-load request.";
	$.get("cgi-bin/fark_load.pl",{type:"coloc",arg:args},function(data, status){
	    var ret=dataToArray(data,status,documentLog);
	    var root=ret[0]||{};
	    console.log("Updating dropdown for ",target);
	    removeChildren(item);
	    if (args.length >0 && looksLikeFile(args[0])) {
		var file=getFile(args[0]);
	    } else {
		var file="";
	    };
	    // add directories...
	    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
	    console.log("Found entries: ",dirs.length-1,root);
	    var parent=dirs[0];
	    if (parent != null) {
		var dd=parent;
		console.log("Adding up: ",dd);
		addChildButton(item,"<up>","showValue('plotColoc','"+dd+"');");
	    } else {
		console.log("Adding clear: ",dd);
		addChildButton(item,"<up>","showValue('plotColoc','');");
	    }
	    if (dirs.length > 0) {
		for (var ii=1;ii<dirs.length;ii++) {
		    var dir=dirs[ii];
		    if (root["loc"] == "" || root["loc"] == ".") {
			var dd = dir;
		    } else {
			var dd = root["loc"]+dir;
		    };
		    if (dd !== null && dd !== undefined) {
			//if (dd.substr(dd.length-1) == "/" || dd == "") {
			    //dd=dd + file;
			//}
			console.log("Adding dir button: ",dd,ii);
// colocation file 'dd' must be 'loaded' if it is selected....!!!
			addChildButton(item,dd,"showValue('plotColoc','"+dd+"');plot_loadColoc('"+dd+"');");
		    }
		}
	    }
	    console.log("There: ",dirs);
	    documentLog.innerHTML="";
	});
    } else if (target.substr(0,14) === 'plotExpression') { //***********************************
	var cnt = target.substring(14);
	removeChildren(item);
	var cfile=plot_getColocConfigFile();
	var mfile=plot_getModelConfigFile();
	var ofile=plot_getObsConfigFile();
	// model index
	if (model_config[mfile]!== undefined) {
	    var indexTarget=model_config[mfile]["indexTarget"];
	    addChildButton(item,indexTarget,"addValue('"+target+"','"+indexTarget+"');");
	};
	// list model trgs in coloc_config
 	if (coloc_config[cfile] !== undefined) {
	    var trgs=coloc_config[cfile]["modelConfigFile"]["targets"];
	    for (var trg in trgs) {
		addChildButton(item,trg,"addValue('"+target+"','"+trg+"');");
	    };
	};
	// list obs trgs in obs_config
	if (obs_config[ofile] !== undefined) {
	    var trgs=obs_config[ofile]["targets"];
	    for (var trg in trgs) {
		addChildButton(item,trg,"addValue('"+target+"','"+trg+"');");
	    };
	    trg = obs_config[ofile]["indexTarget"];
	    addChildButton(item,trg,"addValue('"+target+"','"+trg+"');");
	}
	// list obs trgs in coloc_config
 	if (coloc_config[cfile] !== undefined) {
	    var trgs=coloc_config[cfile]["obsConfigFile"]["targets"];
	    for (var trg in trgs) {
		addChildButton(item,trg,"addValue('"+target+"','"+trg+"');");
	    };
	};
	addFunctionButtons(item,target);
    } else if (target.substr(0,13) === 'plotAttribute') { //***********************************
	var attr = target.substring(13);
	var file=plot_getConfigFile();
	var cat=plot_config[file]["cat"];
	if (plot_cats[cat] === undefined) {plot_setCat(cat);}
	var val=plot_cats[cat]["attributes"][attr];
	var radio=val instanceof Array; // should we have radio button?
	var dup=(attr.substr(0,1) === "_");
	removeChildren(item);
	if (radio) {
	    for (var vv=0; vv < val.length;vv++) {
		console.log("Attribute '",attr,"' value  ",vv,val[vv]);
		if (dup) {
		    addChildButton(item,val[vv],"plot_setAttribute('"+attr+"','"+val[vv]+"');plot_setCat('"+cat+"');plot_show();");
		} else {
		    addChildButton(item,val[vv],"plot_setAttribute('"+attr+"','"+val[vv]+"');");
		};
	    };
	}
    } else if (target === 'autoType') { //***********************************
	removeChildren(item);
	addChildButton(item,"model","showValue('"+target+"','model');");
	addChildButton(item,"observation","showValue('"+target+"','obs');");
	addChildButton(item,"colocation","showValue('"+target+"','coloc');");
	addChildButton(item,"plot","showValue('"+target+"','plot');");
    } else if (target === 'autoConfigFile') { //***********************************
	var type=document.getElementById("autoType").value // "obs";
	var args=getArgs(arg);
	documentLog.innerHTML="Sent coloc-load request.";
	$.get("cgi-bin/fark_load.pl",{type:type,arg:args},function(data, status){
	    var ret=dataToArray(data,status,documentLog);
	    var errors=data.getElementsByTagName("error");
	    if (errors.length > 0 ) {
		var msg=(errors[0].getAttribute("message")||"");
		alert("Unable to list '"+arg+"', type '"+type+"' \n"+msg);
	    } else if (ret[0] !== undefined) {
		var root=ret[0]||{};
		console.log("Updating dropdown for ",target);
		removeChildren(item);
		if (args.length >0 && looksLikeFile(args[0])) {
		    var file=getFile(args[0]);
		} else {
		    var file="";
		};
		// add directories...
		var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
		console.log("Found entries: ",dirs.length-1,root);
		var parent=dirs[0];
		if (parent != null) {
		    var dd=parent;
		    console.log("Adding up: ",dd);
		    addChildButton(item,"<up>","showValue('autoConfigFile','"+dd+"');");
		} else {
		    console.log("Adding clear: ",dd);
		    addChildButton(item,"<up>","showValue('autoConfigFile','');");
		}
		if (dirs.length > 0) {
		    for (var ii=1;ii<dirs.length;ii++) {
			var dir=dirs[ii];
			if (root["loc"] == "" || root["loc"] == ".") {
			    var dd = dir;
			} else {
			    var dd = root["loc"]+dir;
			};
			if (dd !== null && dd !== undefined) {
			    //if (dd.substr(dd.length-1) == "/" || dd == "") {
				//dd=dd + file;
			    //}
			    console.log("Adding dir button: ",dd,ii);
			    addChildButton(item,dd,"showValue('autoConfigFile','"+dd+"');");
			}
		    }
		}
	    } else {
		console.log("Undefined root.");
	    }
	    documentLog.innerHTML="";
	});
    } else if (target === 'autoCron') { //***********************************
	var type=document.getElementById("autoType").value // "obs";
	var args=getArgs(arg);
	removeChildren(item);
	for (var ii=0;ii<auto_cron.length;ii++) {
	    var cron=auto_cron[ii];
	    if (cron === "") {
		addChildButton(item,"<never>","showValue('autoCron','"+cron+"');");
	    } else {
		addChildButton(item,cron,"showValue('autoCron','"+cron+"');");
	    }
	}
    } else {
	console.log("Unknown dropdown target:", target);
    }
    //document.getElementById(dropdown).classList.toggle("show");
}
function showValue(target,value) {
    if (document.getElementById(target) == null) {
	console.log("Undefined target:",target," Value:",value);
    } else {
	document.getElementById(target).value=value;
    }
}
function addValue(target,value) {
    //console.log("Item:",value);
    document.getElementById(target).value=document.getElementById(target).value + value;
}
function setInnerHTML(target,value) {
    //console.log("Item:",value);
    document.getElementById(target).innerHTML=value;
}

function dataToArray(data,status,documentLog) {
    var ret=[];
    if (status == "success") {
	dataToCat(data);
	dataToModel(data);
	dataToObs(data);
	dataToColoc(data);
	dataToPlot(data);
	dataToAuto(data);
	ret.extend(dataToMetfark(data));
    }
    documentLog.innerHTML="";
    return ret;
}

function dataToModel(data) {
    // <model_config name="test" filterFile="*"> <variable name="var1"> </model_config>
    var models=data.getElementsByTagName("model_config");
    for (var ii = 0; ii < models.length; ii++) {
	var name=models[ii].getAttribute("file");
	var loc=models[ii].getAttribute("location");
	if (loc === "") {
	    var path = name;
	} else {
	    var path = loc + name;
	}
	if (model_config[path] === undefined) {
	    model_config[path]={variables : {},dims:{}}
	}
	model_config[path]["filterDir"]=
	    set(model_config[path]["filterDir"],models[ii].getAttribute("filterDir"));
	model_config[path]["filterDirMin"]=
	    set(model_config[path]["filterDirMin"],models[ii].getAttribute("filterDirMin"));
	model_config[path]["filterDirMax"]=
	    set(model_config[path]["filterDirMax"],models[ii].getAttribute("filterDirMax"));
	console.log("Filter dir:","'"+model_config[path]["filterDir"]+"'",
		    "'"+model_config[path]["filterDirMin"]+"'",
		    "'"+model_config[path]["filterDirMax"]+"'");
	model_config[path]["filterDirStat"]=
	    set(model_config[path]["filterDirStat"],models[ii].getAttribute("filterDirStat"));
	model_config[path]["filterFile"]=
	    set(model_config[path]["filterFile"],models[ii].getAttribute("filterFile"));
	model_config[path]["hits"]=
	    set(model_config[path]["hits"],models[ii].getAttribute("hits"));
	model_config[path]["indexTarget"]=
	    set(model_config[path]["indexTarget"],models[ii].getAttribute("indexTarget"));
	model_config[path]["indexVariable"]=
	    set(model_config[path]["indexVariable"],models[ii].getAttribute("indexVariable"));
	model_config[path]["start"]=
	    set(model_config[path]["start"],models[ii].getAttribute("start"));
	model_config[path]["stop"]=
	    set(model_config[path]["stop"],models[ii].getAttribute("stop"));
	var variables=models[ii].getElementsByTagName("variable");
	if (variables) {
	    model_config[path]["variables"]={};
	    for (var jj = 0; jj < variables.length; jj++) {
		var name=variables[jj].getAttribute("name");
		var dims=variables[jj].getAttribute("dims")||"";
		model_config[path]["variables"][name]=dims;
	    }
	} else if (model_config[path]["variables"] === undefined) {
	    model_config[path]["variables"]={};
	}
	var dims=models[ii].getElementsByTagName("dimension");
	if (dims) {
	    model_config[path]["dimensions"]={};
	    for (var jj = 0; jj < dims.length; jj++) {
		var name=dims[jj].getAttribute("name");
		var dimv=dims[jj].getAttribute("value")||"";
		model_config[path]["dimensions"][name]=dimv;
	    }
	} else if (model_config[path]["dimensions"] === undefined) {
	    model_config[path]["dimensions"]={};
	}
	var files=models[ii].getElementsByTagName("stack");
	if (files.length>0) {
	    model_config[path]["files"]=[];
	    for (var jj = 0; jj < files.length; jj++) {
		var sname=files[jj].getAttribute("name");
		var sage=files[jj].getAttribute("age");
		var ssize=files[jj].getAttribute("size");
		console.log("Found stack file:",sname,' (',path,')');
		model_config[path]["files"].push([sname,sage,ssize]);
	    }
	    model_config[path]["stack"]=files[0].getAttribute("name");
	} else if (model_config[path]["files"] === undefined) {
	    model_config[path]["files"]=[];
	}
    };
}
function dataToObs(data) {
    // <obs_config name="test" filterFile="*"> <bufr id="var1"> <sub id="2"> ...</obs_config>
    var obs=data.getElementsByTagName("obs_config");
    for (var ii = 0; ii < obs.length; ii++) {
	var name=obs[ii].getAttribute("file");
	var loc=obs[ii].getAttribute("location");
	if (loc === "") {
	    var path = name;
	} else {
	    var path = loc + name;
	}
	if (obs_config[path] === undefined) {
	    obs_config[path]={bufr :{}, filter:{}}
	}
	obs_config[path]["filterDir"]=
	    set(obs_config[path]["filterDir"],obs[ii].getAttribute("filterDir"));
	obs_config[path]["filterDirMin"]=
	    set(obs_config[path]["filterDirMin"],obs[ii].getAttribute("filterDirMin"));
	obs_config[path]["filterDirMax"]=
	    set(obs_config[path]["filterDirMax"],obs[ii].getAttribute("filterDirMax"));
	obs_config[path]["filterDirStat"]=
	    set(obs_config[path]["filterDirStat"],obs[ii].getAttribute("filterDirStat"));
	obs_config[path]["filterFile"]=
	    set(obs_config[path]["filterFile"],obs[ii].getAttribute("filterFile"));
	obs_config[path]["hits"]=
	    set(obs_config[path]["hits"],obs[ii].getAttribute("hits"));
	obs_config[path]["start"]=
	    set(obs_config[path]["start"],obs[ii].getAttribute("start"));
	//console.log("fark.js start:",name,obs_config[path]["start"])
	obs_config[path]["stop"]=
	    set(obs_config[path]["stop"],obs[ii].getAttribute("stop"));
	obs_config[path]["tablePath"]=
	    set(obs_config[path]["tablePath"],obs[ii].getAttribute("tablePath"));
	obs_config[path]["bufrType"]=
	    set(obs_config[path]["bufrType"],obs[ii].getAttribute("bufrType"));
	obs_config[path]["subType"]=
	    set(obs_config[path]["subType"],obs[ii].getAttribute("subType"));
	obs_config[path]["typeInfo"]=
	    set(obs_config[path]["typeInfo"],obs[ii].getAttribute("typeInfo"));
	obs_config[path]["indexTarget"]=
	    set(obs_config[path]["indexTarget"],obs[ii].getAttribute("indexTarget"));
	obs_config[path]["indexExp"]=
	    set(obs_config[path]["indexExp"],obs[ii].getAttribute("indexExp"));
	if (obs_config[path]["bufr"] === undefined) { 
	    obs_config[path]["bufr"]={};
	};
	var bufr=obs[ii].getElementsByTagName("bufr");
	if (bufr.length > 0) {
	    obs_config[path]["bufr"]={};
	    for (var jj = 0; jj < bufr.length; jj++) {
		var bufrType=bufr[jj].getAttribute("bufrType");
		var subType=bufr[jj].getAttribute("subType") || "";
		var info=bufr[jj].getAttribute("info");
		var cnt=bufr[jj].getAttribute("cnt");
		if (obs_config[path]["bufr"][bufrType] === undefined) { 
		    obs_config[path]["bufr"][bufrType]={};
		};
		if (subType !== undefined && subType !== "") {
		    if (obs_config[path]["bufr"][bufrType][subType] === undefined) { 
			obs_config[path]["bufr"][bufrType][subType]={info:info,cnt:cnt};
		    }
		    var seq=bufr[jj].getElementsByTagName("seq");
		    for (var pp = 0; pp < seq.length; pp++) {
			var pos=seq[pp].getAttribute("pos");
			var descr=seq[pp].getAttribute("descr");
			var pinfo=seq[pp].getAttribute("info");
			obs_config[path]["bufr"][bufrType][subType][pos]={descr:descr,info:pinfo};
		    }
		} else {
		    obs_config[path]["bufr"][bufrType]["info"]=info;
		    obs_config[path]["bufr"][bufrType]["cnt"]=cnt;
		}
	    }
	};
	// read targets
	var targets=obs[ii].getElementsByTagName("target");
	if (targets.length > 0) {
	    obs_config[path]["targets"]={};
	    obs_config[path]["targeto"]=[];
	    for (var jj = 0; jj < targets.length; jj++) {
		var target=targets[jj].getAttribute("name");
		if (target !== null) {
		    obs_config[path]["targets"][target]={
			pos:targets[jj].getAttribute("pos"),
			descr:targets[jj].getAttribute("descr"),
			info:targets[jj].getAttribute("info")};
		    obs_config[path]["targeto"].push(target);
		}
	    }
	}
	var files=obs[ii].getElementsByTagName("stack");
	if (files.length > 0) {
	    obs_config[path]["files"]=[];
	    for (var jj = 0; jj < files.length; jj++) {
		var sname=files[jj].getAttribute("name");
		var sage=files[jj].getAttribute("age");
		var ssize=files[jj].getAttribute("size");
		obs_config[path]["files"].push([sname,sage,ssize]);
	    };
	    obs_config[path]["stack"]=files[0].getAttribute("name");
	} else if (obs_config[path]["files"] === undefined) {
	    obs_config[path]["files"]=[];
	}
    }
}

function dataToColoc(data) {
    // <coloc_config name="test" filterFile="*"> <variable name="var1"> </coloc_config>
    var colocs=data.getElementsByTagName("coloc_config");
    for (var ii = 0; ii < colocs.length; ii++) {
	var name=colocs[ii].getAttribute("file");
	var loc=colocs[ii].getAttribute("location");
	if (loc === "") {
	    var path = name;
	} else {
	    var path = loc + name;
	}
	coloc_config[path]={modelConfigFile:{min:"",max:"",exp:"",targets:{},targeto:[],def:[]},
			    obsConfigFile:{filter:"",min:"",max:"",targets:{},targeto:[]},
			    host:"localhost",
			    xml:"",
			    filter:""
			   };
	coloc_config[path]["host"]=
	    set(coloc_config[path]["host"],colocs[ii].getAttribute("host"));
	coloc_config[path]["xml"]=
	    set(coloc_config[path]["xml"],colocs[ii].getAttribute("xml"));
	coloc_config[path]["filter"]=
	    set(coloc_config[path]["filter"],colocs[ii].getAttribute("filter"));
	//console.log("Host:",ii,name,coloc_config[path]["host"],colocs[ii].getAttribute("host"));
	coloc_config[path]["modelConfigFile"]["file"]=
	    set(coloc_config[path]["modelConfigFile"]["file"],colocs[ii].getAttribute("modelFile"));
	coloc_config[path]["modelConfigFile"]["min"]=
	    set(coloc_config[path]["modelConfigFile"]["min"],colocs[ii].getAttribute("modelStart"));
	coloc_config[path]["modelConfigFile"]["max"]=
	    set(coloc_config[path]["modelConfigFile"]["max"],colocs[ii].getAttribute("modelStop"));
	coloc_config[path]["modelConfigFile"]["exp"]=
	    set(coloc_config[path]["modelConfigFile"]["max"],colocs[ii].getAttribute("indexExp"));
	var modelTargets=colocs[ii].getElementsByTagName("modelTarget");
	for (var jj = 0; jj < modelTargets.length; jj++) {
	    var target=modelTargets[jj].getAttribute("name");
	    var variable=modelTargets[jj].getAttribute("variable");
	    var min=modelTargets[jj].getAttribute("min");
	    var max=modelTargets[jj].getAttribute("max");
	    coloc_config[path]["modelConfigFile"]["targets"][target]={variable:variable,min:min,max:max};
	    coloc_config[path]["modelConfigFile"]["targeto"].push(target);
	    console.log("Metfark adding:",target);
	};
	var defs=colocs[ii].getElementsByTagName("modelDefault");
	coloc_config[path]["modelConfigFile"]["def"]=[];
	for (var jj = 0; jj < defs.length; jj++) {
	    var info=defs[jj].getAttribute("info");
	    var targets={targets:{}, info:info};
	    var defTargets=defs[jj].getElementsByTagName("def");
	    for (var kk = 0; kk < defTargets.length; kk++) {
		var target=defTargets[kk].getAttribute("name");
		var value=defTargets[kk].getAttribute("value");
		console.log("metfark: *** loaded default: ",target,value);
		targets["targets"][target]=value;
	    };
	    coloc_config[path]["modelConfigFile"]["def"].push(targets);
	};
	coloc_config[path]["obsConfigFile"]["file"]=
	    set(coloc_config[path]["obsConfigFile"]["file"],colocs[ii].getAttribute("obsFile"));
	//console.log("##### obsFilter:",ii,name,colocs[ii].getAttribute("obsFilter"));
	coloc_config[path]["obsConfigFile"]["filter"]=
	    set(coloc_config[path]["obsConfigFile"]["filter"],colocs[ii].getAttribute("obsFilter"));
	coloc_config[path]["obsConfigFile"]["min"]=
	    set(coloc_config[path]["obsConfigFile"]["min"],colocs[ii].getAttribute("obsStart"));
	coloc_config[path]["obsConfigFile"]["max"]=
	    set(coloc_config[path]["obsConfigFile"]["max"],colocs[ii].getAttribute("obsStop"));
	var obsTargets=colocs[ii].getElementsByTagName("obsTarget");
	for (var jj = 0; jj < obsTargets.length; jj++) {
	    var target=obsTargets[jj].getAttribute("name");
	    var bufrType=obsTargets[jj].getAttribute("bufrType");
	    var subType=obsTargets[jj].getAttribute("subType");
	    var pos=obsTargets[jj].getAttribute("pos");
	    var descr=obsTargets[jj].getAttribute("descr");
	    var info=obsTargets[jj].getAttribute("info");
	    var min=obsTargets[jj].getAttribute("min");
	    var max=obsTargets[jj].getAttribute("max");
	    coloc_config[path]["obsConfigFile"]["targets"][target]={bufrType:bufrType,
								    subType:subType,
								    pos:pos,
								    descr:descr,
								    info:info,
								    min:min,
								    max:max};
	    coloc_config[path]["obsConfigFile"]["targeto"].push(target);
	};
	var matchRules=colocs[ii].getElementsByTagName("matchRules");
	for (var jj = 0; jj < matchRules.length; jj++) {
	    var target=matchRules[jj].getAttribute("name");
	    var expression=matchRules[jj].getAttribute("expression");
	    if (coloc_config[path]["modelConfigFile"]["targets"][target] !==  undefined) {
		coloc_config[path]["modelConfigFile"]["targets"][target]["exp"]=expression;
	    }
	}
	console.log("metfark: loaded coloc: ",name,coloc_config[path]);
    };
    //console.log("metfark: loaded COLOC: ",coloc_config);

}
function dataToPlot(data) {
    // <plot_config name="test"></plot_config>
    var plots=data.getElementsByTagName("plot_config");
    for (var ii = 0; ii < plots.length; ii++) {
	var file=plots[ii].getAttribute("file");
	var loc=plots[ii].getAttribute("location");
	if (loc === "") {
	    var path = file;
	} else {
	    var path = loc + file;
	}
	var table=plots[ii].getAttribute("table");
	var graphics=plots[ii].getAttribute("graphics");
	var cat=plots[ii].getAttribute("cat");
	plot_config[path]={dataset:{}, attributes:{},table:table,graphics:graphics,cat:cat};
	var sets=plots[ii].getElementsByTagName("set");
	console.log("metfark: loading plot file=",path," cat=",cat," sets=",sets.length);
	for (var jj = 0; jj < sets.length; jj++) {
	    var name=sets[jj].getAttribute("name");
	    var coloc=sets[jj].getAttribute("coloc");
	    var legend=sets[jj].getAttribute("legend");
	    var columns=[];
	    var clmns=sets[jj].getElementsByTagName("column");
	    for (var kk = 0; kk < clmns.length; kk++) {
		var expr=clmns[kk].getAttribute("value");
		columns.push(expr);
	    };
	    plot_config[path]["dataset"][name]={coloc:coloc,
						columns:columns,
						legend:legend};
	}
	// set default attributes
	if (plot_cats[cat] != undefined) {
	    for (var attr in plot_cats[cat]["attributes"]) {
		if (plot_config[path]["attributes"][attr] === undefined) {
		    var val=plot_cats[cat]["attributes"][attr];
		    if (val instanceof Array) {
			plot_config[path]["attributes"][attr]=val[0]; // first element
		    } else {
			plot_config[path]["attributes"][attr]=val;
		    }
		}
	    }
	}
	var attrs=plots[ii].getElementsByTagName("attribute");
	for (var jj = 0; jj < attrs.length; jj++) {
	    var name=attrs[jj].getAttribute("name");
	    var value=attrs[jj].getAttribute("value");
	    plot_config[path]["attributes"][name]=value;
	}
    }
}
function dataToCat(data) {
    var cats=data.getElementsByTagName("cat_config");
    if (cats.length>0) {
	plot_org_cats={};
	plot_order=[];
    }
    for (var jj = 0; jj < cats.length; jj++) {
	var name=cats[jj].getAttribute("name");
	var attrs=cats[jj].getElementsByTagName("attr");
	plot_org_cats[name]={"attributes":{},"lines":{},"order":[]};
	plot_order.push(name);
	for (var kk = 0; kk < attrs.length; kk++) {
	    var attr=attrs[kk].getAttribute("name");
	    var value=attrs[kk].getAttribute("value");
	    var choices=value.split(":");
	    if (choices.length>1) {
		for (var i = choices.length - 1; i >= 0; --i) {
		    if (choices[i] == "" || choices[i] == null || choices[i]==undefined) {
			choices.splice(i,1);
		    }
		};
		plot_org_cats[name]["attributes"][attr]=choices;
	    } else {
		plot_org_cats[name]["attributes"][attr]=value;
	    }
	    plot_org_cats[name]["order"].push(attr);
	};
	var types=cats[jj].getElementsByTagName("line");
	if (types.length>0) {plot_org_cats[name]["lines"]={};}
	 for (var kk = 0; kk < types.length; kk++) {
	     var id=types[kk].getAttribute("id");
	     var info=types[kk].getAttribute("name");
	     plot_org_cats[name]["lines"][id]=info;
	     //console.log("metfark: loaded line: ",name,id,info);
	 };
	var clmns=cats[jj].getElementsByTagName("column");
	if (clmns.length>0) {plot_org_cats[name]["columns"]=[];}
	 for (var kk = 0; kk < clmns.length; kk++) {
	     var clmn=clmns[kk].getAttribute("name");
	     plot_org_cats[name]["columns"].push(clmn);
	     console.log("metfark: loaded column: ",name,clmn);
	 };
    };
};
function dataToAuto(data) {
    // <auto_config name="test" filterFile="*"> <variable name="var1"> </auto_config>
    var autos=data.getElementsByTagName("auto_config");
    for (var ii = 0; ii < autos.length; ii++) {
	auto_config["model"]={};
	var models=autos[ii].getElementsByTagName("model");
	for (var jj = 0; jj < models.length; jj++) {
	    var model=models[jj].getAttribute("file");
	    var last=models[jj].getAttribute("last") || "";
	    var info=models[jj].getAttribute("info") || "";
	    var auto=models[jj].getAttribute("auto") || auto_cron[0];
	    if (! isInArray(auto,auto_cron)) {auto=auto_cron[0];};
	    var status=models[jj].getAttribute("status") ||"";
	    auto_config["model"][model]={last:last,info:info,auto:auto,status:status};
	};
	auto_config["obs"]={};
	var obses=autos[ii].getElementsByTagName("obs");
	for (var jj = 0; jj < obses.length; jj++) {
	    var obs=obses[jj].getAttribute("file");
	    var last=obses[jj].getAttribute("last") ||"";
	    var info=obses[jj].getAttribute("info") ||"";
	    var auto=obses[jj].getAttribute("auto")  || auto_cron[0];
	    if (! isInArray(auto,auto_cron)) {auto=auto_cron[0];};
	    var status=obses[jj].getAttribute("status") ||"";
	    auto_config["obs"][obs]={last:last,info:info,auto:auto,status:status};
	};
	auto_config["coloc"]={};
	var coloces=autos[ii].getElementsByTagName("coloc");
	for (var jj = 0; jj < coloces.length; jj++) {
	    var coloc=coloces[jj].getAttribute("file");
	    var last=coloces[jj].getAttribute("last") ||"";
	    var info=coloces[jj].getAttribute("info") ||"";
	    var auto=coloces[jj].getAttribute("auto")  || auto_cron[0];
	    if (! isInArray(auto,auto_cron)) {auto=auto_cron[0];};
	    var status=coloces[jj].getAttribute("status") ||"";
	    auto_config["coloc"][coloc]={last:last,info:info,auto:auto,status:status};
	};
	auto_config["plot"]={};
	var plotes=autos[ii].getElementsByTagName("plot");
	for (var jj = 0; jj < plotes.length; jj++) {
	    var plot=plotes[jj].getAttribute("file");
	    var last=plotes[jj].getAttribute("last") ||"";
	    var info=plotes[jj].getAttribute("info") ||"";
	    var auto=plotes[jj].getAttribute("auto")  || auto_cron[0];
	    if (! isInArray(auto,auto_cron)) {auto=auto_cron[0];};
	    var status=plotes[jj].getAttribute("status") ||"";
	    auto_config["plot"][plot]={last:last,info:info,auto:auto,status:status};
	};
    }
}
function dataToMetfark(data) {
    var ret=[];
    for (var cc = 0; cc < clss.length; cc++) {
	var parents=data.getElementsByTagName(clss[cc]);
	if (parents.length == 1) {
	    var parent=parents[0];
	    metfark_config[clss[cc]]={}; // reset directory tree
	    ret.extend(dataToRoot(parent,clss[cc]));
	    dataToDirs(parent,clss[cc]);
	    dataToFiles(parent,clss[cc]);
	}
    }
    return ret;
}
function dataToRoot(data,cls) {
    var ret = [];
    var loc="Root";
    var nodes=data.getElementsByTagName(loc);
    console.log("dataToRoot Looking for:",loc,", found XML root nodes:",nodes.length);
    for (var ii = 0; ii < nodes.length; ii++) {
	// loop over directories and make sure they exist in metfark_config-structure...
	if (metfark_config[cls] == undefined) { 
	    console.log("dataToRoot Creating internal cls:",cls);
	    metfark_config[cls]={};
	}
	for (var ii = 0; ii < nodes.length; ii++) {
	    var dir=nodes[ii];
	    var cls=dir.getAttribute("class");
	    var root=dir.getAttribute("root");
	    var loc=dir.getAttribute("location");
	    var stat=dir.getAttribute("status");
	    var arg=dir.getAttribute("arg");
	    var file=dir.getAttribute("file");
	    var child=dir.getAttribute("child");
	    var type=dir.getAttribute("type");
	    var item = {cls:cls,
		      type:type,
		      root:root, 
		      loc:loc, 
		      stat:stat, 
		      child:child, 
		      file:file};
	    console.log("Found type:",type,":",root,":",loc,":",stat);
	    ret.push(item);
	};
    }
    return ret;
}
function dataToDirs(data,cls)	{
    var loc="Dir";
    var nodes=data.getElementsByTagName(loc);
    console.log("dataToDirs Looking for:",loc,", found XML root nodes:",nodes.length);
    for (var ii = 0; ii < nodes.length; ii++) {
	// loop over directories and make sure they exist in metfark_config-structure...
	if (metfark_config[cls] == undefined) { 
	    console.log("dataToFiles Creating internal class: '"+cls+"'");
	    metfark_config[cls]={};
	};
	var dir=nodes[ii];
	var root=dir.getAttribute("root");
	var location=dir.getAttribute("location");
	var child=dir.getAttribute("dir");
	var status=dir.getAttribute("status");
	if (metfark_config[cls][root] == undefined) { 
	    console.log("dataToDirs Creating internal root: '"+cls+"'",root);
	    metfark_config[cls][root]={};
	}
	var pos=metfark_config[cls][root];
	var steps = location.split("/");
	for (var ss=0;ss<steps.length;ss++) {
	    if (steps[ss] == "" || steps[ss] == ".") {continue;};
	    if (pos[steps[ss]] == undefined) { 
		console.log("dataToDirs Creating internal sub: '"+steps[ss]+"'",ss);
		pos[steps[ss]]={};
	    }
	    pos=pos[steps[ss]];
	};
	// check that child directory exists...
	if (pos[child] == undefined) {
	    console.log("dataToDirs Adding:",child);
	    pos[child]={};
	};
    }
}
function dataToFiles(data,cls)	{
    var loc="File";
    var nodes=data.getElementsByTagName(loc);
    console.log("dataToFiles Looking for:",loc,", found XML root nodes:",nodes.length);
    for (var ii = 0; ii < nodes.length; ii++) {
	// loop over directories and make sure they exist in metfark_config-structure...
	if (metfark_config[cls] == undefined) { 
	    console.log("dataToFiles Creating internal class: '"+cls+'"');
	    metfark_config[cls]={};
	}
	for (var ii = 0; ii < nodes.length; ii++) {
	    var dir=nodes[ii];
	    var root=dir.getAttribute("root");
	    var location=dir.getAttribute("location");
	    var file=dir.getAttribute("file");
	    var status=dir.getAttribute("status");
	    if (metfark_config[cls][root] == undefined) { 
		metfark_config[cls][root]={};
	    }
	    var pos=metfark_config[cls][root];
	    var steps = location.split("/");
	    for (var ss=0;ss<steps.length;ss++) {
		if (steps[ss] == "" || steps[ss] == ".") {continue;};
		if (pos[steps[ss]] == undefined) { 
		    console.log("dataToFiles Creating internal sub: '"+steps[ss]+"'",ss);
		    pos[steps[ss]]={};
		}
		pos=pos[steps[ss]];
	    };
	    console.log("dataToFiles Adding: '"+cls+"'",file);
	    pos[file]="file";
	};
    }
}

function metfark_updateDir( cls, nodes) {
}
function clone(obj) {
    if (null == obj || "object" != typeof obj) return obj;
    var copy = obj.constructor();
    for (var attr in obj) {
        if (obj.hasOwnProperty(attr)) copy[attr] = obj[attr];
    }
    return copy;
}

function set(target, value) {
    var ret="";
    if (value !== undefined && value !== null) {
	ret=value;
    } else if (target !== undefined)  {
	ret=target;
    };
    return ret;
}

function getDir(path) {
    var dira=path.match(/^.*[\/\\]/);
    if (dira == null) {
	return '';
    } else {
	return dira[0];
    };
};

function getFile(path) {
    var filea=path.match(/[^\/\\]*$/);
    if (filea == null) {
	return '';
    } else {
	return filea[0];
    };
};

function getArgs(arg) {
    return arg.split(" ");
};

function looksLikeFile(arg) {
    var suffix=arg.match(/\.[^\/]+$/);
    return (suffix != null);
}

function getSubDirs(cls,root,loc,child) {
    if (metfark_config[cls] == undefined) { 
	metfark_config[cls]={};
    }
    if (metfark_config[cls][root] == undefined) { 
	metfark_config[cls][root]={};
    }
    var pos=metfark_config[cls][root];
    if (loc === null || loc === undefined) {loc="";};
    console.log("Location:",loc);
    var steps = loc.split("/");
    var parent = null;
    var sub = null;
    console.log("getSubDirs root:'"+root+"' loc: '"+loc+"' sub-dirs:",steps.length);
    for (var ss=0;ss<steps.length;ss++) {
	console.log("getSubDirs sub: '"+ss+"' sub-dirs:",steps[ss]);
	if (steps[ss] == "" || steps[ss] == ".") {continue;};
	if (pos[steps[ss]] == undefined) {pos[steps[ss]]={};};
	if (steps[ss] !=null && steps[ss] != "") {
	    if (looksLikeFile(steps[ss])) {
		console.log("getSubDirs file '"+steps[ss]+"'");
	    } else {
		console.log("getSubDirs cd '"+steps[ss]+"'");
		pos=pos[steps[ss]];
		if (sub != null && sub != "") {
		    parent=parent+sub;
		    if (parent != "") {parent=parent+"/";};
		    }
		if (parent == null) {parent="";};
		sub=steps[ss];
	    }
	}
    };
    if (child != null) {
	if (looksLikeFile(child)) {
	    console.log("getSubDirs child file '"+child+"'");
	} else {
	    if (pos[child] == undefined) {pos[child]={};};
	    console.log("getSubDirs child cd '"+child+"'");
	    pos=pos[child];
	};
    };
    var ret=[];
    console.log("getSubDirs parent '"+parent+"'");
    ret.push(parent);
    for (var key in pos) {
	console.log("getSubDirs Found dir:",key);
	var entry=key;
	if (pos[key] != "file") {
	    entry=entry + "/";
	}
	ret.push(entry);
    }
    return ret;
}

function isInArray(value, array) {
  return array.indexOf(value) > -1;
}

function arrayUp(array,ii) {
    if (ii<array.length && ii>0) {
	var buff = array[ii];
	array[ii]=array[ii-1];
    	array[ii-1]=buff;
    } else if (ii == 0) {
	var buff = array[ii];
	array[ii]=array[array.length-1];
    	array[array.length-1]=buff;
    };
}


Array.prototype.extend = function (other_array) {
    /* you should include a test to check whether other_array really is an array */
    other_array.forEach(function(v) {this.push(v)}, this);    
}
