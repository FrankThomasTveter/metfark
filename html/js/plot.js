
plot_file = "default.cfg";
plot_config = { "default.cfg" : { dataset : { 1 : {line:1,
						   coloc:"coloc", 
						   legend:"legend",
						   columns:["x","y"]
						  }},
				  attributes : { def: "default"},
				  table : "table.ps",
				  graphics : "default.ps",
				  cat : "Text",
				  password: "test"
				}
	      };
plot_org_cats = { "Text": {"attributes":{xlabel:"X", ylabel:"Y"},
		       "order":["xlabel","ylabel"],
		       "lines": {1:"solid"},
                       "columns" : ["X-expression","Y-expression"]}
	    };
plot_cats  ={};
plot_order =["Text"];
plot_configEd = 0;

function plot_print(file) {
    if (plot_config[file]!== undefined) {
	console.log("File:",file," Dataset:",Object.keys(plot_config[file]["dataset"]).length);
    } else {
	console.log("File:",file," Dataset is undefined.");
    }
}

function plot_allocate(file) {
    if (plot_config[plot_file] === undefined) {
	console.log("Corrupt plot_file:",plot_file);
    } else if (plot_config[file] === undefined) {
	plot_config[file]=clone(plot_config[plot_file]);
	console.log("cloned:",plot_file," -> ",file);
    }
}
function plot_setConfigFile(file) {
    showValue('plotConfigFile',file);
    showValue('plotConfigFileSave',file);
    //if (file != "") {
	console.log("Setting plot config file:",file);
	plot_allocate(file);
	plot_file=file;
        plot_setCat();
    //};
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

function plot_expandCat(cat) {
    var file=plot_getConfigFile();
    plot_cats[cat]=goclone(plot_org_cats[cat]);
    for (var attr in plot_org_cats[cat]["attributes"]) {
	console.log("Found org attribute:",attr);
    }
    for (var attr in plot_org_cats[cat]["attributes"]) {
	if (attr.substr(0,1) === "_") {
	    if (plot_config[file]!== undefined &&
		plot_config[file]["attributes"][attr] !== undefined) {
		var nn = plot_config[file]["attributes"][attr];
	    } else {
		var val=plot_org_cats[cat]["attributes"][attr];
		if (val instanceof Array) {
		    var nn=val[0]; // first element
		} else {
		    var nn=val;
		}
	    };
	    console.log("Duplicator attribute '"+attr+"' = ",nn);
	    var re = new RegExp("(\w*)"+RegExp.quote(attr)+"(\w*)", "g");
	    for (var aa in plot_cats[cat]["attributes"]) {
		if (aa.match(re) && aa !== attr) {
		    //console.log("Attribute match '"+aa+"' == '"+attr+"'");
		    // delete aa attribute
		    var val=plot_cats[cat]["attributes"][aa];
		    delete plot_cats[cat]["attributes"][aa];
		    var index = plot_cats[cat]["order"].indexOf(aa);
		    plot_cats[cat]["order"].splice(index, 1);
		    for (var ii=nn;ii>0;ii--) {
			var newattr = aa.replace(re, '$1'+ii.toString()+'$2');
			// add attribute
			console.log("Adding attribute '"+newattr+"' = ",val);
			plot_cats[cat]["attributes"][newattr]=val;
			plot_cats[cat]["order"].splice(index,0,newattr);
		    }
		} else {
		    //console.log("Attribute mismatch '"+aa+"' != '"+attr+"'");
		}
	    }
	    for (var jj = 0; jj <  plot_cats[cat]["columns"].length;jj++) {
		var cc=plot_cats[cat]["columns"][jj];
		if (cc.match(re)) {
		    // delete cc column
		    console.log("Column match '"+cc+"' == '"+attr+"'");
		    var index = plot_cats[cat]["columns"].indexOf(cc);
		    plot_cats[cat]["columns"].splice(index, 1);
		    for (var ii=nn;ii>0;ii--) {
			var newcol = cc.replace(re, '$1'+ii.toString()+'$2');
			console.log("Adding column '"+newattr+"'");
			// add column
			plot_cats[cat]["columns"].splice(index,0,newcol);
		    }
		}else {
		    console.log("Column mismatch '"+cc+"' != '"+attr+"'");
		}
	    }
	}
    }
}

RegExp.quote = function(str) {
    return (str+'').replace(/[.?*+^$[\]\\(){}|-]/g, "\\$&");
};
function goclone(source) {
    if (Object.prototype.toString.call(source) === '[object Array]') {
        var clone = [];
        for (var i=0; i<source.length; i++) {
            clone[i] = goclone(source[i]);
        }
        return clone;
    } else if (typeof(source)=="object") {
        var clone = {};
        for (var prop in source) {
            if (source.hasOwnProperty(prop)) {
                clone[prop] = goclone(source[prop]);
            }
        }
        return clone;
    } else {
        return source;
    }
}

function plot_setCat(value) {
    var file=plot_getConfigFile();
    if (value===undefined) {
	value=plot_config[file]["cat"]
    };
    //console.log("File:",file,parameter,plot_config[file]);
    plot_config[file]["cat"]=value;
    //
    plot_expandCat(value);
    // sync file and cat attributes
    for (var attr in plot_config[file]["attributes"]) {
	if (plot_cats[value]===undefined || 
	    plot_cats[value]["attributes"][attr] === undefined) {
	    delete plot_config[file]["attributes"][attr];
	};
    }
    for (var attr in plot_cats[value]["attributes"]) {
	if (plot_config[file]["attributes"][attr] === undefined) {
	    var val=plot_cats[value]["attributes"][attr];
	    if (val instanceof Array) {
		plot_config[file]["attributes"][attr]=val[0]; // first element
	    } else {
		plot_config[file]["attributes"][attr]=val;
	    }
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
function plot_setDatasetColumn(target,parameter,value) {
    var file=plot_getConfigFile();
    if (plot_config[file]["dataset"][target] == undefined) {
	plot_config[file]["dataset"]["columns"][target]={};
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
	showValue('plotConfigFile',file);
	showValue('plotConfigFileSave',file);
	showValue('plotCat',plot_config[file]["cat"]);
	showValue('plotTable',plot_config[file]["table"]);
	showValue('plotGraphics',plot_config[file]["graphics"]);
	plot_showDatasetTable();
	plot_showAttributesTable();
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
function plot_isEmpty(obj) {
    for(var prop in obj) {
        if(obj.hasOwnProperty(prop))
            return false;
    }
    return true;
};
function plot_newDataset(item) {
    //if (! plot_checkPassword()) {return;}
    var file= plot_getConfigFile();
    var cat =plot_config[file]["cat"];
    var set=item.parentNode.parentNode.children[1].children[0].value;
    var coloc=item.parentNode.parentNode.children[4].children[0].value;
    var clmns=plot_cats[cat]["columns"];
    var columns=[];
    for (var ii =0; ii< clmns.length;ii++) {
	console.log("newDataset, cleaning:",ii);
	columns.push(item.parentNode.parentNode.children[6+ii*2].children[0].value);
	item.parentNode.parentNode.children[6+ii*2].children[0].value="";
    }
    var legend=item.parentNode.parentNode.children[6+clmns.length*2].children[0].value;
    console.log("New: trg:",set," file:",coloc," columns:",columns," leg:",legend);
    if (set !== "" && coloc !== "") {
	if (plot_config[file] === undefined) {
	    plot_config[file]={dataset : {},
			       attributes : {},
			       password: ""};
	};
	plot_config[file]["dataset"][set]={coloc:coloc,columns:columns,legend:legend};
	item.parentNode.parentNode.children[1].children[0].value="";
	item.parentNode.parentNode.children[3].children[0].value="";
	item.parentNode.parentNode.children[4].children[0].value="";
	item.parentNode.parentNode.children[6+clmns.length*2].children[0].value="";
	plot_showDatasetTable();
    } else {
	alert("Invalid line set/coloc: ('"+set+"'/'"+coloc+"')");
    }
    fark_last["coloc"]=coloc;
};
function plot_removeDataset(set) {
    var file=plot_getConfigFile();
    var cat =plot_config[file]["cat"];
    var type=plot_cats[plot_config[file]["cat"]]["lines"][set]||"";
    var coloc=plot_config[file]["dataset"][set]["coloc"];
    var columns=plot_config[file]["dataset"][set]["columns"];
    var item=document.getElementById("newlinePlotDataset");
    var legend=plot_config[file]["dataset"][set]["legend"];
    delete plot_config[file]["dataset"][set];
    plot_showDatasetTable();
    item.children[1].children[0].value=set;
    item.children[3].children[0].value=type;
    item.children[4].children[0].value=coloc;
    var clmns=plot_cats[cat]["columns"];
    for (var ii =0; ii< clmns.length;ii++) {
	if (columns[ii] !== undefined) {
	    item.children[6+ii*2].children[0].value=columns[ii];
	} else {
	    item.children[6+ii*2].children[0].value=0;
	}
    }
    item.children[6+clmns.length*2].children[0].value=legend;
    fark_last["coloc"]=coloc;
};

function plot_saveConfigFile() {
    var file=plot_getConfigFile();
    var password=document.getElementById("plotConfigFilePsw").value;
    var table=plot_config[file]["table"];
    var graphics=plot_config[file]["graphics"];
    var cat=plot_config[file]["cat"];
    var cols=plot_cats[cat]["columns"];
    var plotCols="";
    for (var ii =0; ii< cols.length;ii++) {
	if (plotCols.length==0) {
	    plotCols=cols[ii];
	} else {
	    plotCols=plotCols+"~"+cols[ii];
	}
    }
    var plotSets="";
    var sets=plot_config[file]["dataset"];
    for (var set in sets) {
	var coloc=sets[set]["coloc"];
	var columns="";
	for (var ii =0; ii< cols.length;ii++) {
	    var expr;
	    if (sets[set]["columns"] !== undefined) {
		expr = sets[set]["columns"][ii]||"0";
	    } else {
		expr = "0";
	    }
	    columns=columns + expr + "~";
	}
	var legend=sets[set]["legend"];
	if (coloc === undefined) {coloc="";}
	if (legend === undefined) {legend="";}
	plotSets=plotSets + "|" + set + "~" + coloc + "~" + legend + "~" + columns;
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
				  graphics:graphics,columns:plotCols,sets:plotSets,attributes:plotAttrs
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
function plot_showDatasetTable() {
    var file=plot_getConfigFile();
    var item=document.getElementById('plotDatasetTable');
    var header=clearTableChild(item,"labelsPlotDataset");
    var newline=clearTableChild(item,"newlinePlotDataset");
    var tail=removeTableChildFromTo(item,"labelsPlotDataset","newlinePlotDataset");
    plot_insertDatasetHeader(header,file);
    plot_insertDatasetNewline(newline,file);
    plot_insertDatasetRow(tail,file);
}
function plot_insertDatasetHeader(row,file) {
    var th,bf;
    // make "-" column
    th=document.createElement("TH");
    th.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(th);
    // make Id column
    th=document.createElement("TH");
    th.setAttribute("style","min-width:30px;width:30px");
    bf=document.createElement("BF");
    bf.innerHTML="Id";
    th.appendChild(bf);
    row.appendChild(th);
    // make select-Id column
    th=document.createElement("TH");
    th.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(th);
    // make Line column
    th=document.createElement("TH");
    th.setAttribute("style","min-width:10%");
    bf=document.createElement("BF");
    bf.innerHTML="Line";
    th.appendChild(bf);
    row.appendChild(th);
    // make ColocationFile column
    th=document.createElement("TH");
    th.setAttribute("style","min-width:30%");
    bf=document.createElement("BF");
    bf.innerHTML="Colocation file";
    th.appendChild(bf);
    row.appendChild(th);
    // make select-ColocationFile column
    th=document.createElement("TH");
    th.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(th);
    // expression columns
    var cat=plot_config[file]["cat"];
    var clmns=plot_cats[cat]["columns"];
    for (var ii =0; ii< clmns.length;ii++) {
	var label=plot_cats[cat]["columns"][ii];
	// make expression column
	th=document.createElement("TH");
	bf=document.createElement("BF");
	bf.innerHTML=label;
	th.appendChild(bf);
	row.appendChild(th);
	// make select-expression column
	th=document.createElement("TH");
	th.setAttribute("style","min-width:25px;width:25px");
	row.appendChild(th);
    }
    // make Legend column
    th=document.createElement("TH");
    bf=document.createElement("BF");
    bf.innerHTML="Legend";
    th.appendChild(bf);
    row.appendChild(th);
}
function plot_insertDatasetNewline(row,file) {
    var td,bf,inp,div;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("align","center");
    td.setAttribute("style","min-width:25px;width:25px");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","plot_newDataset(this)");
    btn.setAttribute("style","width:100%");
    btn.innerHTML="&#43";
    td.appendChild(btn);
    row.appendChild(td);
    // make id column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("id","plotLine");
    inp.setAttribute("type","text");
    inp.setAttribute("value","");
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","");
    td.appendChild(inp);
    div=document.createElement("DIV");
    div.setAttribute("id","plotLineDropdown");
    div.setAttribute("class","dropdown-content");
    td.appendChild(div);
    row.appendChild(td);
    // make select-id column
    td=document.createElement("TD");
    td.setAttribute("align","center");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","showDropdown('plotLine',this.parentNode.parentNode.children[1].children[0].value)");
    btn.setAttribute("class","dropbtn");
    //var t=document.createTextNode("&#9776");
    //btn.appendChild(t);
    btn.innerHTML="&#9776";
    td.appendChild(btn);
    row.appendChild(td);
    // make line column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("id","plotType");
    inp.setAttribute("type","text");
    inp.setAttribute("value","");
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","");
    inp.disabled=true;
    td.appendChild(inp);
    row.appendChild(td);
    // make colocationFile column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("id","plotColoc");
    inp.setAttribute("type","text");
    inp.setAttribute("value","");
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","");
    td.appendChild(inp);
    div=document.createElement("DIV");
    div.setAttribute("id","plotColocDropdown");
    div.setAttribute("class","dropdown-content");
    td.appendChild(div);
    row.appendChild(td);
    // make select-colocationFile column
    td=document.createElement("TD");
    td.setAttribute("align","center");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","showDropdown('plotColoc',this.parentNode.parentNode.children[4].children[0].value)");
    btn.setAttribute("class","dropbtn");
    //var t=document.createTextNode("&#9776");
    //btn.appendChild(t);
    btn.innerHTML="&#9776";
    td.appendChild(btn);
    row.appendChild(td);
    // expression columns
    var cat=plot_config[file]["cat"];
    var clmns=plot_cats[cat]["columns"];
    for (var ii =0; ii< clmns.length;ii++) {
	var expr=plot_cats[cat]["columns"][ii];
	// make expression column
	var itemId="plotExpression"+ii;
	td=document.createElement("TD");
	td.setAttribute("class","fill");
	inp=document.createElement("INPUT");
	inp.setAttribute("id",itemId);
	inp.setAttribute("type","text");
	inp.setAttribute("value","");
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","");
	td.appendChild(inp);
	div=document.createElement("DIV");
	div.setAttribute("id",itemId+"Dropdown");
	div.setAttribute("class","dropdown-content");
	td.appendChild(div);
	row.appendChild(td);
	// make select-expression column
	td=document.createElement("TD");
	td.setAttribute("align","center");
	btn=document.createElement("BUTTON");
	btn.setAttribute("onclick","showDropdown('"+itemId+"',this.parentNode.parentNode.children["+(6+ii*2)+"].children[0].value)");
	btn.setAttribute("class","dropbtn");
	//var t=document.createTextNode("&#9776");
	//btn.appendChild(t);
	btn.innerHTML="&#9776";
	td.appendChild(btn);
	row.appendChild(td);
    }
    // make Legend column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("id","plotLegend");
    inp.setAttribute("type","text");
    inp.setAttribute("value","");
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","");
    td.appendChild(inp);
    row.appendChild(td);
}
function plot_insertDatasetRow(item,file) {
    var cat =plot_config[file]["cat"];
    var sets=plot_config[file]["dataset"];
    for (var set in sets) {
	var type=plot_cats[cat]["lines"][set]||"";
	var coloc=sets[set]["coloc"];
	fark_last["coloc"]=coloc;
	var legend=sets[set]["legend"];
	var row = document.createElement("TR");
	var td,bf;
	// make "-" column
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px");
	var btn=document.createElement("BUTTON");
	btn.setAttribute("onclick","plot_removeDataset('"+set+"')");
	btn.setAttribute("style","width:100%");
	var t=document.createTextNode("-");
	btn.appendChild(t);
	td.appendChild(btn);
	row.appendChild(td);
	// make id column
	td=document.createElement("TD");
	td.innerHTML=set;
	row.appendChild(td);
	// make select-id column
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px");
	row.appendChild(td);
	// make line column
	td=document.createElement("TD");
	td.innerHTML=type;
	row.appendChild(td);
	// make colocationFile column
	td=document.createElement("TD");
	td.innerHTML=coloc;
	row.appendChild(td);
	// make select-colocationFile column
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px");
	row.appendChild(td);
	// make expressions
	var clmns=plot_cats[cat]["columns"];
	for (var ii =0; ii< clmns.length;ii++) {
	    var expr;
	    if (sets[set]["columns"] !== undefined) {
		expr = sets[set]["columns"][ii]||"0";
	    } else {
		expr = "0";
	    }
	    // make expression column
	    td=document.createElement("TD");
	    td.innerHTML=expr;
	    row.appendChild(td);
	    // make select-expression column
	    td=document.createElement("TD");
	    td.setAttribute("style","min-width:25px;width:25px");
	    row.appendChild(td);
	}
	// make Legend column
	td=document.createElement("TD");
	td.setAttribute("class","fill");
	inp=document.createElement("INPUT");
	inp.setAttribute("type","text");
	inp.setAttribute("value",legend);
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","plot_setDataset('"+set+"','legend',this.value);");
	td.appendChild(inp);
	row.appendChild(td);
	// add row to table  ***************************
	item.parentNode.insertBefore(row,item);
    }
}
function plot_showAttributesTable() {
    var file=plot_getConfigFile();
    var cat=plot_config[file]["cat"];
    var order=plot_cats[cat]['order'];
    var item=document.getElementById('plotAttributesTable');
    var head=removeTableChildFrom(item,"labelsPlotAttribute");
    var value=plot_config[file]['attributes'];
    for (var ii=order.length-1;ii>=0;ii--) {
	var attr=order[ii];
	var val=plot_cats[cat]["attributes"][attr];
	plot_insertAttributeRow(head,cat,attr,value[attr],val);
    }
}
function plot_insertAttributeRow(item,cat,attr,value,val) {
    var row = document.createElement("TR");
    var td,inp,div;
    var radio=val instanceof Array; // should we have radio button?
    var dup=(attr.substr(0,1) === "_");
    // make attr column  ***************************
    td=document.createElement("TD");
    if (dup) {
	td.innerHTML=attr.substr(1);
    } else {
	td.innerHTML=attr;
    }
    row.appendChild(td);
    // make attribute value column  ***************************
    var itemId="plotAttribute"+attr;
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("id",itemId);
    inp.setAttribute("type","text");
    inp.setAttribute("value",value);
    inp.setAttribute("style","width:100%");
    if (dup) {
	inp.setAttribute("onblur","plot_setAttribute('"+attr+"',this.value);plot_setCat('"+cat+"');plot_show();");
    } else {
	inp.setAttribute("onblur","plot_setAttribute('"+attr+"',this.value);");
    }
    if (radio) {
	inp.disabled=true;
    }
    td.appendChild(inp);
    div=document.createElement("DIV");
    div.setAttribute("id",itemId+"Dropdown");
    div.setAttribute("class","dropdown-content");
    td.appendChild(div);
    row.appendChild(td);
    // make select-expression column
    td=document.createElement("TD");
    if (radio) {
	td.setAttribute("align","center");
	td.setAttribute("style","min-width:25px;width:25px");
	btn=document.createElement("BUTTON");
	btn.setAttribute("onclick","showDropdown('"+itemId+"',this.parentNode.parentNode.children[1].children[0].value)");
	btn.setAttribute("class","dropbtn");
	btn.innerHTML="&#9776";
	td.appendChild(btn);
    } else {
    }
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
function plot_updateData(arg = plot_getConfigFile()) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent plot-load request.";
    var types=[];
    types[0]="plot";
    types[1]="cat";
    console.log("$$$$$ Loading plot+cats with: ", args);
    $.get("cgi-bin/fark_load.pl",{type:types,arg:args},
	  function(data, status){
	      dataToArray(data,status,documentLog);
	      plotLoaded=true;
	      //console.log("Updating dropdown for ",arg);
	      plot_setCat();
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
	      } else {
		  delete plot_config[path];
		  if (plot_file == path) {plot_file="default.cfg";}
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

