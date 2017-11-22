plot_file = "default.cfg";
plot_config = { "default.cfg" : { dataset : { 1 : {line:1,
						   coloc:"coloc", 
						   x:"x",
						   y:"y",
						   legend:"legend"}},
				  attributes : { def: "default"},
				  table : "table.ps",
				  graphics : "default.ps",
				  cat : "Text",
				  password: "test"
				}
	      };
plot_cats = { "Text": {"attributes":{"Text" : {xlabel:"X", ylabel:"Y"}}, 
		       "lines": {1:"solid"}}
	    };

plot_configEd = 0;

function plot_allocate(file) {
    if (plot_config[plot_file] === undefined) {
	console.log("Corrupt plot_file:",plot_file);
    } else if (plot_config[file] === undefined) {
	plot_config[file]=clone(plot_config[plot_file]);
	console.log("cloned:",plot_file," -> ",file);
	plot_file=file;
    }
}
function plot_setConfigFile(file) {
    setValue('plotConfigFile',file);
    setValue('plotConfigFileSave',file);
    if (file != "") {
	console.log("Setting plot config file:",file);
	plot_allocate(file);
	plot_show();
    };
}
function plot_getConfigFile() {
    return plot_file;
};
function plot_getColocConfigFile() {
    var item=document.getElementById("newlinePlotDataset");
    var file = item.children[4].children[0].value;
    return file;
};
function plot_getModelConfigFile() {
    var file=plot_getColocConfigFile();
    if (coloc_config[file] !== undefined) {
	return coloc_config[file]["modelConfigFile"]["file"];
    }
};
function plot_getObsConfigFile() {
    var file=plot_getColocConfigFile();
    if (coloc_config[file] !== undefined) {
	return coloc_config[file]["obsConfigFile"]["file"];
    }
};
function plot_setArray(parameter,value) {
    var file=plot_getConfigFile();
    //console.log("File:",file,parameter,plot_config[file]);
    plot_config[file][parameter]=decodeURI(value);
};
function plot_setCat(value) {
    var file=plot_getConfigFile();
    //console.log("File:",file,parameter,plot_config[file]);
    plot_config[file]["cat"]=value;
    // sync file and cat attributes
    for (var attr in plot_config[file]["attributes"]) {
	if (plot_cats[value]===undefined || 
	    plot_cats[value]["attributes"][attr] === undefined) {
	    delete plot_config[file]["attributes"][attr];
	};
    }
    for (var attr in plot_cats[value]["attributes"]) {
	if (plot_config[file]["attributes"][attr] === undefined) {
	    plot_config[file]["attributes"][attr]=plot_cats[value]["attributes"][attr];
	};
    }
};
function plot_setDataset(target,parameter,value) {
    var file=plot_getConfigFile();
    if (plot_config[file]["dataset"][target] == undefined) {
	plot_config[file]["dataset"][target]={};
    }
    plot_config[file]["dataset"][target][parameter]=value;
};
function plot_setAttribute(attr,value) {
    var file=plot_getConfigFile();
    plot_config[file]["attributes"][attr]=value;
};
function plot_show() {
    var file=plot_getConfigFile();
    if (file != "") {
	plot_allocate(file);
	setValue('plotConfigFile',file);
	setValue('plotConfigFileSave',file);
	setValue('plotCat',plot_config[file]["cat"]);
	setValue('plotTable',plot_config[file]["table"]);
	setValue('plotGraphics',plot_config[file]["graphics"]);
	plot_setDatasetTable(file);
	plot_setAttributesTable(file,plot_config[file]['attributes']);
    };
};
// plotervation config methods
function plot_checkPassword() {
    var password=document.getElementById("plotConfigFilePsw").value;
    var file=plot_getConfigFile();
    if (plot_config[file] !== undefined) {
	if (plot_config[file]["password"] !== undefined) {
	    if (plot_config[file]["password"] !== password) {
		alert("Invalid password used when attempting to save file:\n"+file);
		return false;
	    };
	};
    };
    return true;
}
function plot_removeData(set) {
    var file=plot_getConfigFile();
    var item=document.getElementById("newlinePlotDataset");
    item.children[1].children[0].value=set;
    var type=plot_cats[plot_config[file]["cat"]]["lines"][set]//"";
    item.children[3].children[0].value=type;
    item.children[4].children[0].value=plot_config[file]["dataset"][set]["coloc"];
    item.children[6].children[0].value=plot_config[file]["dataset"][set]["x"];
    item.children[8].children[0].value=plot_config[file]["dataset"][set]["y"];
    item.children[10].children[0].value=plot_config[file]["dataset"][set]["legend"];
    delete plot_config[file]["dataset"][set];
    plot_setDatasetTable(file);
};

function plot_isEmpty(obj) {
    for(var prop in obj) {
        if(obj.hasOwnProperty(prop))
            return false;
    }
    return true;
};
function plot_newPlotDataset(item) {
    //if (! plot_checkPassword()) {return;}
    var set=item.parentNode.parentNode.children[1].children[0].value;
    var coloc=item.parentNode.parentNode.children[4].children[0].value;
    var x=item.parentNode.parentNode.children[6].children[0].value;
    var y=item.parentNode.parentNode.children[8].children[0].value;
    var legend=item.parentNode.parentNode.children[10].children[0].value;
    console.log("New: trg:",set," file:",coloc," x:",x," y:",y," leg:",legend);
    if (set !== "" && coloc !== "") {
	var file= plot_getConfigFile();
	if (plot_config[file] === undefined) {
	    plot_config[file]={dataset : {},
			       attributes : {},
			       password: ""};
	};
	plot_config[file]["dataset"][set]={coloc:coloc,x:x,y:y,legend:legend};
	item.parentNode.parentNode.children[1].children[0].value="";
	item.parentNode.parentNode.children[3].children[0].value="";
	item.parentNode.parentNode.children[4].children[0].value="";
	item.parentNode.parentNode.children[6].children[0].value="";
	item.parentNode.parentNode.children[8].children[0].value="";
	item.parentNode.parentNode.children[10].children[0].value="";
	plot_setDatasetTable(file);
    } else {
	alert("Invalid line set/coloc: ('"+set+"'/'"+coloc+"')");
    }
};
function plot_saveConfigFile() {
    var file=plot_getConfigFile();
    var password=document.getElementById("plotConfigFilePsw").value;
    var table=plot_config[file]["table"];
    var graphics=plot_config[file]["graphics"];
    var cat=plot_config[file]["cat"];
    var plotSets="";
    var sets=plot_config[file]["dataset"];
    for (var set in sets) {
	var coloc=sets[set]["coloc"];
	var x=sets[set]["x"];
	var y=sets[set]["y"];
	var legend=sets[set]["legend"];
	if (coloc === undefined) {coloc="";}
	if (x === undefined) {x="";}
	if (y === undefined) {y="";}
	if (legend === undefined) {legend="";}
	plotSets=plotSets + "|" + set + "~" + coloc + "~" + x + "~" + y + "~" + legend;
    };
    var plotAttrs="";
    var attrs=plot_config[file]["attributes"];
    for (var attr in attrs) {
	var value=attrs[attr];
	if (value !== undefined) {
	    plotAttrs=plotAttrs + "|" + attr + "~" + value;
	}
    };
    console.log("Saving: "+file+" "+cat+" "+table+" "+graphics+" "+plotSets+" "+plotAttrs, plot_config[file]);
    plot_configEd++;
    documentLog.innerHTML="Sent plot-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"plot",file:file,password:password,cat:cat,table:table,
				  graphics:graphics,sets:plotSets,attributes:plotAttrs
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
    makeUrl("plot",file);
};
// make new plot-index entry
function plot_setDatasetTable(file) {
    var sets=plot_config[file]["dataset"];
    var item=document.getElementById('plotDatasetTable');
    var tail=removeTableChildFromTo(item,"labelsPlotDataset","newlinePlotDataset");
    for (var set in sets) {
	var cat =plot_config[file]["cat"];
	var type=plot_cats[cat]["lines"][set]//"";
	console.log("Looking for:",file,cat,set,type,sets[set]["coloc"]);
	plot_insertDatasetRow(tail,set,type,sets[set]["coloc"],sets[set]["x"],
				 sets[set]["y"],sets[set]["legend"]);
    }
}
function plot_insertDatasetRow(item,set,type,coloc,x,y,legend) {
    var row = document.createElement("TR");
    var td,inp;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","plot_removeData('"+set+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make set column  ***************************
    td=document.createElement("TD");
    td.innerHTML=set;
    row.appendChild(td);
    // make select-set column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make line column  ***************************
    td=document.createElement("TD");
    td.innerHTML=type;
    row.appendChild(td);
    // make coloc column  ***************************
    td=document.createElement("TD");
    td.innerHTML=coloc;
    row.appendChild(td);
    // make select-coloc column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make x column  ***************************
    td=document.createElement("TD");
    td.innerHTML=x;
    row.appendChild(td);
    // make select-x column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make y column  ***************************
    td=document.createElement("TD");
    td.innerHTML=y;
    row.appendChild(td);
    // make select-y column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make legend column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",legend);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","plot_setDataset('"+set+"','legend',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
function plot_setAttributesTable(file,value) {
    var file=plot_getConfigFile();
    var item=document.getElementById('plotAttributesTable');
    var head=removeTableChildFrom(item,"labelsPlotAttribute");
    for (var attr in value) {
	plot_insertAttributeRow(head,attr,value[attr]);
    }
}
function plot_insertAttributeRow(item,attr,value) {
    var row = document.createElement("TR");
    var td,inp;
    // make attr column  ***************************
    td=document.createElement("TD");
    td.innerHTML=attr;
    row.appendChild(td);
    // make attribute value column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",value);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","plot_setAttribute('"+attr+"',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item.nextSibling);
    return row;
}
function plot_loadColoc(file) {
    if (file != "") {
	var mfile=coloc_getModelConfigFile(file);
	if (coloc_modelIsNotLoaded(mfile)) {coloc_updateModelData(mfile);}
	var ofile=coloc_getObsConfigFile(file);
	if (coloc_obsIsNotLoaded(ofile)) {coloc_updateObsData(ofile);}
    };
};
function plot_updateData() {
    var args=getArgs(plot_getConfigFile());
    documentLog.innerHTML="Sent plot-load request.";
    var types=[];
    types[0]="plot";
    types[1]="cat";
    $.get("cgi-bin/fark_load.pl",{type:types,arg:args},
	  function(data, status){
	      dataToArray(data,status,documentLog);
	      plotLoaded=true;
	      //console.log("Updating dropdown for ",arg);
	      plot_show();
	      documentLog.innerHTML="";
	  });
};
function plot_mkdir(path) {
    var password=document.getElementById("plotConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"plot",
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

function plot_rmfile(path) {
    var password=document.getElementById("plotConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"plot",
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

function plot_rmdir(path) {
    var password=document.getElementById("plotConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"plot",
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

function plot_mkfile(file) {
    console.log("Calling saveConfigFile: '"+file+"'");
    plot_setConfigFile(file);
    plot_saveConfigFile(file);
};

