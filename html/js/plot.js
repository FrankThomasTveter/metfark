    plot_file = "default.cfg";
plot_config = { "default.cfg" : { dataset : { 1 : {line:1,
						   coloc:"coloc", 
						   legend:"legend",
						   colnames:["X-expression","Y-expression"],
						   columns:["x","y"]
						  }},
				  attributes : { def: "default"},
				  table : "table.ps",
				  graphics : "default.ps",
				  cat : "Text",
				  password: "test"
				}
	      };
plot_org_cats = { "Text": {attributes : {xlabel:"X", ylabel:"Y"},
	  		   order : ["xlabel","ylabel"],
			   lines : {1:"solid"},
			   colnames_ : ["X-expression","Y-expression"]}
		};
plot_cats  ={};
plot_order =["Text"];
plot_configEd = 0;

function plot_print(file) {
    if (plot_config[file]!== undefined) {
	//console.log("File:",file," Dataset:",Object.keys(plot_config[file]["dataset"]).length);
    } else {
	//console.log("File:",file," Dataset is undefined.");
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
   //console.log("Setting plot config file:",file);
    plot_allocate(file);
    plot_file=file;
    //console.log("Cat:",plot_config[file]["cat"]," PLot_file:",plot_file);
    plot_setCat();
    //};
}
function plot_getConfigFile() {
    return plot_file;
};
function plot_getColocConfigFile() {
    var file = document.getElementById("plotColoc").value;
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
    if (plot_org_cats[cat] ===undefined) {
	console.log("Missing category:",cat);
	return;
    };
    for (var attr in plot_org_cats[cat]["attributes"]) {
	//console.log("Found org attribute:",attr);
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
	   //console.log("Duplicator attribute '"+attr+"' = ",nn);
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
			//console.log("Adding attribute '"+newattr+"' = ",val);
			plot_cats[cat]["attributes"][newattr]=val;
			plot_cats[cat]["order"].splice(index,0,newattr);
		    }
		} else {
		    //console.log("Attribute no match '"+aa+"' != '"+attr+"'");
		}
	    }
	    for (var jj = 0; jj <  plot_cats[cat]["colnames_"].length;jj++) {
		var cc=plot_cats[cat]["colnames_"][jj];
		if (cc.match(re)) {
		    // delete cc column
		   //console.log("Column match '"+cc+"' == '"+attr+"'");
		    var index = plot_cats[cat]["colnames_"].indexOf(cc);
		    plot_cats[cat]["colnames_"].splice(index, 1);
		    for (var ii=nn;ii>0;ii--) {
			var newcol = cc.replace(re, '$1'+ii.toString()+'$2');
			//console.log("Adding column '"+newcol+"'");
			// add column
			plot_cats[cat]["colnames_"].splice(index,0,newcol);
		    }
		}else {
		    //console.log("Column no match '"+cc+"' != '"+attr+"'");
		}
	    }
	}
    }
}

RegExp.quote = function(str) {
    var re = new RegExp("[.?*+^$[\]\\(){}|-]", "g");
    return (str+'').replace(re, "\\$&");
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
    plot_expandCat(value);
    if (plot_cats[value] === undefined) {
	console.log("Attempt to set undefined plot-category:",value);
	return;
    }
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
	plot_config[file]["dataset"]["colnames"][target]={};
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
    //console.log("Showing:",file);
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
function plot_newDataset() {
    //if (! plot_checkPassword()) {return;}
    var file= plot_getConfigFile();
    var cat =plot_config[file]["cat"];
    var set=document.getElementById("plotSet");
    var coloc=document.getElementById("plotColoc");
    var legend=document.getElementById("plotLegend");
    var colnames_=plot_cats[cat]["colnames_"];
    var clmns=[];
    for (var ii =0; ii< colnames_.length;ii++) {
	var itemId="plotExpression"+(ii);
	//console.log("newDataset, cleaning:",ii,itemId);
	var item=document.getElementById(itemId);
	if (item !== undefined && item !== null) {
	    clmns.push(item.value);
	    item.value="";
	} else {
	    console.log("NewDataset: Undefined itemId:",itemId);
	}
    }
    fark_last["coloc"]=coloc.value;
   //console.log("New: trg:",set.value," file:",coloc.value," columns:",clmns," leg:",legend.value);
    if (set.value !== "" && coloc.value !== "") {
	if (plot_config[file] === undefined) {
	    plot_config[file]={dataset : {},
			       attributes : {},
			       password: ""};
	};
	plot_config[file]["dataset"][set.value]={coloc:coloc.value,colnames:colnames_,columns:clmns,legend:legend.value};

	set.value="";
	coloc.value="";
	legend.value="";
	plot_showDatasetTable();
    } else {
	alert("Invalid line set/coloc: ('"+set.value+"'/'"+coloc.value+"')");
    }
};
function plot_removeDataset(set) {
    var file=plot_getConfigFile();
    var cat =plot_config[file]["cat"];
    var type=plot_cats[plot_config[file]["cat"]]["lines"][set]||"";
    var coloc=plot_config[file]["dataset"][set]["coloc"];
    var colnames=plot_config[file]["dataset"][set]["colnames"];
    var columns=plot_config[file]["dataset"][set]["columns"];
    var legend=plot_config[file]["dataset"][set]["legend"];
    delete plot_config[file]["dataset"][set];
    plot_showDatasetTable();
    document.getElementById("plotSet").value=set;
    document.getElementById("plotType").value=type;
    document.getElementById("plotColoc").value=coloc;
    document.getElementById("plotLegend").value=legend;
    var colnames_=plot_cats[cat]["colnames_"];
    for (var ii =0; ii< colnames_.length;ii++) {
	var itemId="plotExpression"+(ii);
	var item=document.getElementById(itemId);
	if (item !== null && item !== undefined) {
	    if (columns[ii] !== undefined) {
		item.value=columns[ii];
	    } else {
		item.value=0;
	    }
	} else {
	    console.log("RemoveDataset: Undefined itemId:",itemId);
	}
    }
    fark_last["coloc"]=coloc;
};

function plot_saveConfigFile() {
    var file=plot_getConfigFile();
    var password=document.getElementById("plotConfigFilePsw").value;
    var cat="";
    var table="";
    var graphics="";
    var plotCols="";
    var plotSets="";
    var plotAttrs="";
    if (plot_config[file] != undefined) {
	cat=plot_config[file]["cat"]//"";
	table=plot_config[file]["table"]//"";
	graphics=plot_config[file]["graphics"]//"";
	if (plot_cats[cat] != undefined) {
	    var colnames_=plot_cats[cat]["colnames_"]//[];
	    for (var ii =0; ii< colnames_.length;ii++) {
		if (plotCols.length==0) {
		    plotCols=colnames_[ii];
		} else {
		    plotCols=plotCols+"~"+colnames_[ii];
		}
	    }
	    var sets=plot_config[file]["dataset"]//{};
	    for (var set in sets) {
		var colnames=sets[set]["colnames"]//"";
		var columns=sets[set]["columns"]//"";
		var panick ={};
		for (var ii =0; ii< colnames.length;ii++) {
		    panick[colnames]=columns[ii]||0;
		};
		var coloc=sets[set]["coloc"]//"";
		var clmns="";
		for (var ii =0; ii< colnames_.length;ii++) {
		    var expr;
		    if (columns !== undefined) {
			if (colnames_[ii] == colnames[ii]) {
			    expr = columns[ii]||"0";
			} else {
			    expr = panick[colnames_[ii]]||0;
			}
		    } else {
			expr = "0";
		    }
		    clmns=clmns + expr + "~";
		}
		var legend=sets[set]["legend"]//"";
		if (coloc === undefined) {coloc="";}
		if (legend === undefined) {legend="";}
		plotSets=plotSets + "|" + set + "~" + coloc + "~" + legend + "~" + clmns;
	    };
	    var order=plot_cats[cat]['order']//[];
	    var attrs=plot_config[file]["attributes"]//{};
	    for (var ii=0;ii<order.length;ii++) {
		var attr=order[ii];
		var value=attrs[attr];
		if (value !== undefined) {
		    plotAttrs=plotAttrs + "|" + attr + "~" + value;
		}
	    };
	}
    };
   //console.log("Saving: "+file+" "+cat+" "+table+" "+graphics+" "+plotSets+" "+plotAttrs, plot_config[file]);
    plot_configEd++;
    documentLog.innerHTML="Sent plot-save request.";
    $.get("cgi-bin/fark_save.pl",
	  {type:"plot",
	   file:file,
	   password:password,
	   cat:cat,
	   table:table,
	   graphics:graphics,
	   columns:plotCols,
	   sets:plotSets,
	   attributes:plotAttrs
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
	    function (error) { alert("Plot save request failed (system error)");}
	);
    makeUrl("plot",file);
};
// Transposed function...
function plot_showDatasetTable() {
   //console.log(":::::::::: showDatasetTable");
    var file=plot_getConfigFile();
    var cat=plot_config[file]["cat"];
    var colnames_={};
    var type=[];
    var col1=[];
    if (plot_cats[cat] !== undefined) {
	colnames_=plot_cats[cat]["colnames_"];
	// make column headers
	type=[1,2,3,4,5];
	col1=["Action","Id","Set","Colocation file","Legend"];
	for (var ii =0; ii< colnames_.length;ii++) {
	    col1.push(plot_cats[cat]["colnames_"][ii]);
	    type.push(6); //offset starts with first "6"
	}
    }
    // make data expressions
    var data=[];
    var sets=plot_config[file]["dataset"];
    for (var set in sets) {
	var colnames=sets[set]["colnames"];
	var columns=sets[set]["columns"];
	var panick ={};
	for (var ii =0; ii< colnames.length;ii++) {
	    panick[colnames[ii]]=columns[ii]||0;
	};
	var pset="";
	if (plot_cats[cat] !== undefined) {
	    pset=plot_cats[cat]["lines"][set];
	} else {
	    console.log("Undefined category:",cat);
	};
	var item=[-1,
		  set,
		  pset,
		  sets[set]["coloc"],
		  sets[set]["legend"]
		 ];
	fark_last["coloc"]=sets[set]["coloc"];
	for (var ii =0; ii< colnames_.length;ii++) {
	    var expr;
	    if (columns !== undefined) {
		if (colnames[ii]=="") { colnames[ii]=colnames_[ii];};
		if (colnames_[ii] == colnames[ii]) {
		    expr = columns[ii]||"0";
		} else {
		    expr = panick[colnames_[ii]]||0;
		    colnames[ii]=colnames_[ii];
		    columns[ii]=expr;
		}
	    } else {
		expr = "0";
	    }
	    item.push(expr);
	};
	data.push(item);
	sets[set]["colnames"]=colnames;
	sets[set]["columns"]=columns;
    }
    var item=document.getElementById('plotDatasetTable');
    var tbody=removeTableChildren(item);
    plot_insertDataset(tbody,type,col1,data);
}
//
function plot_insertDataset(item,type,col1,data) {
    // insert rows
    offset=-1;
    for (var ii=0;ii<type.length;ii++) {
	if (type[[ii]] == 6 && offset==-1) {offset=ii;}
    }
   //console.log("Offset=",offset);
    for (var ii=0;ii<type.length;ii++) {
	var row = document.createElement("TR");
	plot_insertHeader(row,type,col1,ii,offset);
	for (var jj=0;jj<data.length;jj++) {
	    plot_insertItem(row,type,data,jj,ii,offset);
	}
	plot_insertNew(row,type,ii,offset);
	item.appendChild(row);
    }
}
//
function plot_insertHeader(row,type,col1,ii,offset) {
    th=document.createElement("TH");
    bf=document.createElement("BF");
    th.setAttribute("bgcolor","#00b9f2");
    bf.innerHTML=col1[[ii]];
    th.appendChild(bf);
    row.appendChild(th);
}
//
function plot_insertItem(row,type,data,jj,ii,offset) {
    if (type[[ii]] == 1) { // action "delete"
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px;");
	var btn=document.createElement("BUTTON");
	btn.setAttribute("title","Remove data-set");
	btn.setAttribute("onclick","plot_removeDataset('"
			 + data[[jj]][[1]]+"')");
	btn.setAttribute("style","width:100%");
	var t=document.createTextNode("-");
	btn.appendChild(t);
	td.appendChild(btn);
	row.appendChild(td);
    } else {
	//console.log("insertItem Inserting:",jj,ii,data[[jj]][[ii]]);
	td=document.createElement("TD");
	td.innerHTML=data[[jj]][[ii]];
	row.appendChild(td);
    }
}
//
function plot_insertNew(row,type,ii,offset) {
    var td;
    var btn;
    var inp;
    if (type[[ii]] == 1 ) { // action "add"
	td=document.createElement("TD");
	td.setAttribute("align","center");
	td.setAttribute("style","width:100%");
	//td.setAttribute("style","min-width:25px;width:25px");
	btn=document.createElement("BUTTON");
	btn.setAttribute("title","Add data-set");
	btn.setAttribute("onclick","plot_newDataset()");
	btn.setAttribute("style","width:100%");
	btn.innerHTML="&#43";
	td.appendChild(btn);
	row.appendChild(td);
	//
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px");
	row.appendChild(td);
    } else if (type[[ii]] == 2) { // Id
	td=document.createElement("TD");
	td.setAttribute("class","fill");
	td.setAttribute("style","width:100%");
	inp=document.createElement("INPUT");
	inp.setAttribute("id","plotSet");
	inp.setAttribute("type","text");
	inp.setAttribute("value","");
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","");
	inp.setAttribute("title","Data set id");
	td.appendChild(inp);
	div=document.createElement("DIV");
	div.setAttribute("id","plotSetDropdown");
	div.setAttribute("class","dropdown-content");
	td.appendChild(div);
	row.appendChild(td);
	// make select-id column
	td=document.createElement("TD");
	td.setAttribute("align","center");
	btn=document.createElement("BUTTON");
	btn.setAttribute("title","Show available identifications");
	btn.setAttribute("onclick","showDropdown('plotSet',document.getElementById('plotSet').value)");
	btn.setAttribute("class","dropbtn");
	btn.innerHTML="&#9776";
	td.appendChild(btn);
	row.appendChild(td);
    } else if (type[[ii]] == 3) { // line
	// make line column
	td=document.createElement("TD");
	td.setAttribute("class","fill");
	td.setAttribute("style","width:100%");
	inp=document.createElement("INPUT");
	inp.setAttribute("id","plotType");
	inp.setAttribute("type","text");
	inp.setAttribute("value","");
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","");
	inp.disabled=true;
	td.appendChild(inp);
	row.appendChild(td);
    } else if (type[[ii]] == 4) { // colocation file
	// make colocationFile column
	td=document.createElement("TD");
	td.setAttribute("class","fill");
	td.setAttribute("style","width:100%");
	inp=document.createElement("INPUT");
	inp.setAttribute("id","plotColoc");
	inp.setAttribute("type","text");
	inp.setAttribute("value","");
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","");
	inp.setAttribute("title","Colocation file");
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
	btn.setAttribute("title","Show available colocation <setup files>");
	btn.setAttribute("onclick","showDropdown('plotColoc',document.getElementById('plotColoc').value)");
	btn.setAttribute("class","dropbtn");
	btn.innerHTML="&#9776";
	td.appendChild(btn);
	row.appendChild(td);
    } else if (type[[ii]] == 5) { // legend
	// make Legend column
	td=document.createElement("TD");
	td.setAttribute("class","fill");
	td.setAttribute("style","width:100%");
	inp=document.createElement("INPUT");
	inp.setAttribute("id","plotLegend");
	inp.setAttribute("type","text");
	inp.setAttribute("value","");
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","");
	inp.setAttribute("title","Plot legend");
	td.appendChild(inp);
	row.appendChild(td);
    } else if (type[[ii]] == 6) { // expression
	// make expression column
	var itemId="plotExpression"+(ii-offset);
	//console.log("insertNew Inserting:",itemId);
	td=document.createElement("TD");
	td.setAttribute("class","fill");
	td.setAttribute("style","width:100%");
	inp=document.createElement("INPUT");
	inp.setAttribute("id",itemId);
	inp.setAttribute("type","text");
	inp.setAttribute("value","");
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","");
	inp.setAttribute("title","Expression using <model targets> or <observation targets>");
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
	btn.setAttribute("title","Show available <model targets>, <observation targets> and functions");
	btn.setAttribute("onclick","showDropdown('"+itemId
			 + "',document.getElementById('"+itemId+"').value)");
	btn.setAttribute("class","dropbtn");
	//var t=document.createTextNode("&#9776");
	//btn.appendChild(t);
	btn.innerHTML="&#9776";
	td.appendChild(btn);
	row.appendChild(td);
    }
}
//
function plot_showAttributesTable() {
    var file=plot_getConfigFile();
    var cat=plot_config[file]["cat"];
    var order=[];
    if (plot_cats[cat] !== undefined) {
	order=plot_cats[cat]['order'];;
    } else {
	console.log("Undefined category:",cat);
    }
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
    inp.setAttribute("title","Attribute value. Visible as a comment in table-file.");
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
	btn.setAttribute("title","Show available attribute values");
	btn.setAttribute("onclick","showDropdown('"+itemId+"',document.getElementById('"+itemId+"').value)");
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
   //console.log("$$$$$ Loading plot+cats with: ", args);
    $.get("cgi-bin/fark_load.pl",{type:types,arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		plotLoaded=true;
		//console.log("Updating dropdown for ",arg);
		plot_setCat();
		plot_show();
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Plot request failed (system error)");}
	);
};
function plot_mkdir(path) {
    var password=document.getElementById("plotConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"plot",
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
	    function (error) { alert("Plot mkdir request failed (system error)");}
	);
    
};

function plot_rmfile(path) {
    var password=document.getElementById("plotConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"plot",
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
			//delete plot_config[path];
			if (plot_file == path) {plot_file="default.cfg";}
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Plot rmfile request failed (system error)");}
	);
    
};

function plot_fgfile(path) { // clear file from internal memory
    if (plot_config[path] != undefined) {
	delete plot_config[path];
    }
};

function plot_rmdir(path) {
    var password=document.getElementById("plotConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"plot",
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
	    function (error) { alert("Plot rmdir request failed (system error)");}
	);
    
};

function plot_mkfile(file) {
   //console.log("Calling saveConfigFile: '"+file+"'");
    plot_setConfigFile(file);
    plot_saveConfigFile(file);
};

