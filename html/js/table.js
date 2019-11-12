table_file = "default.cfg";
table_config = { "default.cfg" : { dataset : { 1 : {line:1,
						    coloc:"coloc", 
						    legend:"legend",
						    colnames:["X-expression","Y-expression"],
						    columns:["x","y"]
						   }},
				   attributes : { def: "default"},
				   table : "table.ps",
				   graphics :"/lustre/storeA",
				   overwrite :"true",
				   cat : "Text",
				   password: "test"
				 }
	       };
table_org_cats = { "Text": {attributes : {xlabel:"X", ylabel:"Y"},
	  		    order : ["xlabel","ylabel"],
			    lines : {1:"solid"},
			    colnames_ : ["X-expression","Y-expression"]}
		 };
table_cats  ={};
table_order =["Text"];
table_configEd = 0;

function table_print(file) {
    if (table_config[file]!== undefined) {
	//console.log("File:",file," Dataset:",Object.keys(table_config[file]["dataset"]).length);
    } else {
	//console.log("File:",file," Dataset is undefined.");
    }
}

function table_allocate(file) {
    if (table_config[table_file] === undefined) {
	console.log("Corrupt table_file:",table_file);
    } else if (table_config[file] === undefined) {
	table_config[file]=clone(table_config[table_file]);
	console.log("cloned:",table_file," -> ",file);
    }
}
function table_setConfigFile(file) {
    showValue('tableConfigFile',file);
    showValue('tableConfigFileSave',file);
    //if (file != "") {
    //console.log("Setting table config file:",file);
    table_allocate(file);
    table_file=file;
    //console.log("Cat:",table_config[file]["cat"]," Table_file:",table_file);
    table_setCat();
    //};
}
function table_getConfigFile() {
    return table_file;
};
function table_getColocConfigFile() {
    var file = document.getElementById("tableColoc").value;
    return file;
};
function table_getModelConfigFile() {
    var file=table_getColocConfigFile();
    if (coloc_config[file] !== undefined) {
	return coloc_config[file]["modelConfigFile"]["file"];
    }
};
function table_getObsConfigFile() {
    var file=table_getColocConfigFile();
    if (coloc_config[file] !== undefined) {
	return coloc_config[file]["obsConfigFile"]["file"];
    }
};
function table_setArray(parameter,value) {
    var file=table_getConfigFile();
    //console.log("File:",file,parameter,table_config[file]);
    table_config[file][parameter]=decodeURI(value);
};

function table_expandCat(cat) {
    var file=table_getConfigFile();
    table_cats[cat]=table_goclone(table_org_cats[cat]);
    if (table_org_cats[cat] ===undefined) {
	console.log("Missing category:",cat);
	return;
    };
    for (var attr in table_org_cats[cat]["attributes"]) {
	//console.log("Found org attribute:",attr);
    }
    for (var attr in table_org_cats[cat]["attributes"]) {
	if (attr.substr(0,1) === "_") {
	    if (table_config[file]!== undefined &&
		table_config[file]["attributes"][attr] !== undefined) {
		var nn = table_config[file]["attributes"][attr];
	    } else {
		var val=table_org_cats[cat]["attributes"][attr];
		if (val instanceof Array) {
		    var nn=val[0]; // first element
		} else {
		    var nn=val;
		}
	    };
	    //console.log("Duplicator attribute '"+attr+"' = ",nn);
	    var re = new RegExp("(\w*)"+table_quote(attr)+"(\w*)", "g");
	    for (var aa in table_cats[cat]["attributes"]) {
		if (aa.match(re) && aa !== attr) {
		    //console.log("Attribute match '"+aa+"' == '"+attr+"'");
		    // delete aa attribute
		    var val=table_cats[cat]["attributes"][aa];
		    delete table_cats[cat]["attributes"][aa];
		    var index = table_cats[cat]["order"].indexOf(aa);
		    table_cats[cat]["order"].splice(index, 1);
		    for (var ii=nn;ii>0;ii--) {
			var newattr = aa.replace(re, '$1'+ii.toString()+'$2');
			// add attribute
			//console.log("Adding attribute '"+newattr+"' = ",val);
			table_cats[cat]["attributes"][newattr]=val;
			table_cats[cat]["order"].splice(index,0,newattr);
		    }
		} else {
		    //console.log("Attribute no match '"+aa+"' != '"+attr+"'");
		}
	    }
	    for (var jj = 0; jj <  table_cats[cat]["colnames_"].length;jj++) {
		var cc=table_cats[cat]["colnames_"][jj];
		if (cc.match(re)) {
		    // delete cc column
		    //console.log("Column match '"+cc+"' == '"+attr+"'");
		    var index = table_cats[cat]["colnames_"].indexOf(cc);
		    table_cats[cat]["colnames_"].splice(index, 1);
		    for (var ii=nn;ii>0;ii--) {
			var newcol = cc.replace(re, '$1'+ii.toString()+'$2');
			//console.log("Adding column '"+newcol+"'");
			// add column
			table_cats[cat]["colnames_"].splice(index,0,newcol);
		    }
		}else {
		    //console.log("Column no match '"+cc+"' != '"+attr+"'");
		}
	    }
	}
    }
}

function table_quote(str) {
    var re = new RegExp("[.?*+^$[](){}|-\\]", "g");
    return (str+'').replace(re, "\\$&");
};
function table_goclone(source) {
    if (Object.prototype.toString.call(source) === '[object Array]') {
        var clone = [];
        for (var i=0; i<source.length; i++) {
            clone[i] = table_goclone(source[i]);
        }
        return clone;
    } else if (typeof(source)=="object") {
        var clone = {};
        for (var prop in source) {
            if (source.hasOwnProperty(prop)) {
                clone[prop] = table_goclone(source[prop]);
            }
        }
        return clone;
    } else {
        return source;
    }
}

function table_setCat(value) {
    var file=table_getConfigFile();
    if (value===undefined) {
	value=table_config[file]["cat"]
    };
    table_expandCat(value);
    if (table_cats[value] === undefined) {
	console.log("Attempt to set undefined table-category:",value);
	return;
    }
    //console.log("File:",file,parameter,table_config[file]);
    table_config[file]["cat"]=value;
    // sync file and cat attributes
    for (var attr in table_config[file]["attributes"]) {
	if (table_cats[value]===undefined || 
	    table_cats[value]["attributes"][attr] === undefined) {
	    delete table_config[file]["attributes"][attr];
	};
    }
    for (var attr in table_cats[value]["attributes"]) {
	if (table_config[file]["attributes"][attr] === undefined) {
	    var val=table_cats[value]["attributes"][attr];
	    if (val instanceof Array) {
		table_config[file]["attributes"][attr]=val[0]; // first element
	    } else {
		table_config[file]["attributes"][attr]=val;
	    }
	};
    }
};
function table_setDataset(target,parameter,value) {
    var file=table_getConfigFile();
    if (table_config[file]["dataset"][target] == undefined) {
	table_config[file]["dataset"][target]={};
    }
    table_config[file]["dataset"][target][parameter]=value;
};
function table_setDatasetColumn(target,parameter,value) {
    var file=table_getConfigFile();
    if (table_config[file]["dataset"][target] == undefined) {
	table_config[file]["dataset"]["colnames"][target]={};
	table_config[file]["dataset"]["columns"][target]={};
    }
    table_config[file]["dataset"][target][parameter]=value;
};
function table_setAttribute(attr,value) {
    var file=table_getConfigFile();
    table_config[file]["attributes"][attr]=value;
};
function table_show() {
    var file=table_getConfigFile();
    //console.log("Showing:",file);
    if (file != "") {
	table_allocate(file);
	showValue('tableConfigFile',file);
	showValue('tableConfigFileSave',file);
	showValue('tableCat',table_config[file]["cat"]);
	showValue('tableTable',table_config[file]["table"]);
	showValue('tableGraphics',table_config[file]["graphics"]);
	setValue('tableOverwrite',table_config[file]["overwrite"]);
	table_showDatasetTable();
	table_showAttributesTable();
    };
};
// tableervation config methods
function table_checkPassword() {
    var password=document.getElementById("tableConfigFilePsw").value;
    var file=table_getConfigFile();
    if (table_config[file] !== undefined) {
	if (table_config[file]["password"] !== undefined) {
	    if (table_config[file]["password"] !== password) {
		alert("Invalid password used when attempting to save file:\n"+file);
		return false;
	    };
	};
    };
    return true;
}
function table_isEmpty(obj) {
    for(var prop in obj) {
        if(obj.hasOwnProperty(prop))
            return false;
    }
    return true;
};
function table_newDataset() {
    //if (! table_checkPassword()) {return;}
    var file= table_getConfigFile();
    var cat =table_config[file]["cat"];
    var set=document.getElementById("tableSet");
    var coloc=document.getElementById("tableColoc");
    var legend=document.getElementById("tableLegend");
    var colnames_=table_cats[cat]["colnames_"];
    var clmns=[];
    for (var ii =0; ii< colnames_.length;ii++) {
	var itemId="tableExpression"+(ii);
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
	if (table_config[file] === undefined) {
	    table_config[file]={dataset : {},
				attributes : {},
				password: ""};
	};
	table_config[file]["dataset"][set.value]={coloc:coloc.value,colnames:colnames_,columns:clmns,legend:legend.value};

	set.value="";
	coloc.value="";
	legend.value="";
	table_showDatasetTable();
    } else {
	alert("Invalid line set/coloc: ('"+set.value+"'/'"+coloc.value+"')");
    }
};
function table_removeDataset(set) {
    var file=table_getConfigFile();
    var cat =table_config[file]["cat"];
    var type=table_cats[table_config[file]["cat"]]["lines"][set]||"";
    var coloc=table_config[file]["dataset"][set]["coloc"];
    var colnames=table_config[file]["dataset"][set]["colnames"];
    var columns=table_config[file]["dataset"][set]["columns"];
    var legend=table_config[file]["dataset"][set]["legend"];
    delete table_config[file]["dataset"][set];
    table_showDatasetTable();
    document.getElementById("tableSet").value=set;
    document.getElementById("tableType").value=type;
    document.getElementById("tableColoc").value=coloc;
    document.getElementById("tableLegend").value=legend;
    var colnames_=table_cats[cat]["colnames_"];
    for (var ii =0; ii< colnames_.length;ii++) {
	var itemId="tableExpression"+(ii);
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

function table_saveConfigFile() {
    var file=table_getConfigFile();
    var password=document.getElementById("tableConfigFilePsw").value;
    var cat="";
    var table="";
    var graphics="";
    var overwrite="";
    var tableCols="";
    var tableSets="";
    var tableAttrs="";
    if (table_config[file] != undefined) {
	cat=table_config[file]["cat"]||"";
	table=table_config[file]["table"]||"";
	graphics=table_config[file]["graphics"]||"";
	overwrite=table_config[file]["overwrite"]||"true";
	if (table_cats[cat] != undefined) {
	    var colnames_=table_cats[cat]["colnames_"]||[];
	    for (var ii =0; ii< colnames_.length;ii++) {
		if (tableCols.length==0) {
		    tableCols=colnames_[ii];
		} else {
		    tableCols=tableCols+"~"+colnames_[ii];
		}
	    }
	    var sets=table_config[file]["dataset"]||{};
	    for (var set in sets) {
		var colnames=sets[set]["colnames"]||"";
		var columns=sets[set]["columns"]||"";
		var panick ={};
		for (var ii =0; ii< colnames.length;ii++) {
		    panick[colnames]=columns[ii]||0;
		};
		var coloc=sets[set]["coloc"]||"";
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
		var legend=sets[set]["legend"]||"";
		if (coloc === undefined) {coloc="";}
		if (legend === undefined) {legend="";}
		tableSets=tableSets + "|" + set + "~" + coloc + "~" + legend + "~" + clmns;
	    };
	    var order=table_cats[cat]['order']||[];
	    var attrs=table_config[file]["attributes"]||{};
	    for (var ii=0;ii<order.length;ii++) {
		var attr=order[ii];
		var value=attrs[attr];
		if (value !== undefined) {
		    tableAttrs=tableAttrs + "|" + attr + "~" + value;
		}
	    };
	}
    };
    //console.log("Saving: "+file+" "+cat+" "+table+" "+tableSets+" "+tableAttrs, table_config[file]);
    table_configEd++;
    documentLog.innerHTML="Sent table-save request.";
    $.get("cgi-bin/fark_save.pl",
	  {type:"table",
	   file:file,
	   password:password,
	   cat:cat,
	   table:table,
	   graphics:graphics,
	   overwrite:overwrite,
	   columns:tableCols,
	   sets:tableSets,
	   attributes:tableAttrs
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
	    function (error) { alert("Table save request failed (system error)");}
	);
    makeUrl("table",file);
};
// Transposed function...
function table_showDatasetTable() {
    //console.log(":::::::::: showDatasetTable");
    var file=table_getConfigFile();
    var cat=table_config[file]["cat"];
    var colnames_={};
    var type=[];
    var col1=[];
    if (table_cats[cat] !== undefined) {
	colnames_=table_cats[cat]["colnames_"];
	// make column headers
	type=[1,2,3,4,5];
	col1=["Action","Id","Set","Colocation file","Legend"];
	for (var ii =0; ii< colnames_.length;ii++) {
	    col1.push(table_cats[cat]["colnames_"][ii]);
	    type.push(6); //offset starts with first "6"
	}
    }
    // make data expressions
    var data=[];
    var sets=table_config[file]["dataset"];
    for (var set in sets) {
	var colnames=sets[set]["colnames"];
	var columns=sets[set]["columns"];
	var panick ={};
	for (var ii =0; ii< colnames.length;ii++) {
	    panick[colnames[ii]]=columns[ii]||0;
	};
	var pset="";
	if (table_cats[cat] !== undefined) {
	    pset=table_cats[cat]["lines"][set];
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
    var item=document.getElementById('tableDatasetTable');
    var tbody=removeTableChildren(item);
    table_insertDataset(tbody,type,col1,data);
}
//
function table_insertDataset(item,type,col1,data) {
    // insert rows
    offset=-1;
    for (var ii=0;ii<type.length;ii++) {
	if (type[[ii]] == 6 && offset==-1) {offset=ii;}
    }
    //console.log("Offset=",offset);
    for (var ii=0;ii<type.length;ii++) {
	var row = document.createElement("TR");
	table_insertHeader(row,type,col1,ii,offset);
	for (var jj=0;jj<data.length;jj++) {
	    table_insertItem(row,type,data,jj,ii,offset);
	}
	table_insertNew(row,type,ii,offset);
	item.appendChild(row);
    }
}
//
function table_insertHeader(row,type,col1,ii,offset) {
    th=document.createElement("TH");
    bf=document.createElement("BF");
    th.setAttribute("bgcolor","#00b9f2");
    bf.innerHTML=col1[[ii]];
    th.appendChild(bf);
    row.appendChild(th);
}
//
function table_insertItem(row,type,data,jj,ii,offset) {
    if (type[[ii]] == 1) { // action "delete"
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px;");
	var btn=document.createElement("BUTTON");
	btn.setAttribute("title","Remove data-set");
	btn.setAttribute("onclick","table_removeDataset('"
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
function table_insertNew(row,type,ii,offset) {
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
	btn.setAttribute("onclick","table_newDataset()");
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
	inp.setAttribute("id","tableSet");
	inp.setAttribute("type","text");
	inp.setAttribute("value","");
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","");
	inp.setAttribute("title","Data set id");
	td.appendChild(inp);
	div=document.createElement("DIV");
	div.setAttribute("id","tableSetDropdown");
	div.setAttribute("class","dropdown-content");
	td.appendChild(div);
	row.appendChild(td);
	// make select-id column
	td=document.createElement("TD");
	td.setAttribute("align","center");
	btn=document.createElement("BUTTON");
	btn.setAttribute("title","Show available identifications");
	btn.setAttribute("onclick","showDropdown('tableSet')");
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
	inp.setAttribute("id","tableType");
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
	inp.setAttribute("id","tableColoc");
	inp.setAttribute("type","text");
	inp.setAttribute("value","");
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","");
	inp.setAttribute("title","Colocation file");
	td.appendChild(inp);
	div=document.createElement("DIV");
	div.setAttribute("id","tableColocDropdown");
	div.setAttribute("class","dropdown-content");
	td.appendChild(div);
	row.appendChild(td);
	// make select-colocationFile column
	td=document.createElement("TD");
	td.setAttribute("align","center");
	btn=document.createElement("BUTTON");
	btn.setAttribute("title","Show available colocation <setup files>");
	btn.setAttribute("onclick","showDropdown('tableColoc')");
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
	inp.setAttribute("id","tableLegend");
	inp.setAttribute("type","text");
	inp.setAttribute("value","");
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","");
	inp.setAttribute("title","Table legend");
	td.appendChild(inp);
	row.appendChild(td);
    } else if (type[[ii]] == 6) { // expression
	// make expression column
	var itemId="tableExpression"+(ii-offset);
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
	btn.setAttribute("onclick","showDropdown('"+itemId+ "')");
	btn.setAttribute("class","dropbtn");
	//var t=document.createTextNode("&#9776");
	//btn.appendChild(t);
	btn.innerHTML="&#9776";
	td.appendChild(btn);
	row.appendChild(td);
    }
}
//
function table_showAttributesTable() {
    var file=table_getConfigFile();
    var cat=table_config[file]["cat"];
    var order=[];
    if (table_cats[cat] !== undefined) {
	order=table_cats[cat]['order'];;
    } else {
	console.log("Undefined category:",cat);
    }
    var item=document.getElementById('tableAttributesTable');
    var head=removeTableChildFrom(item,"labelsTableAttribute");
    var value=table_config[file]['attributes'];
    for (var ii=order.length-1;ii>=0;ii--) {
	var attr=order[ii];
	var val=table_cats[cat]["attributes"][attr];
	table_insertAttributeRow(head,cat,attr,value[attr],val);
    }
}
function table_insertAttributeRow(item,cat,attr,value,val) {
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
    var itemId="tableAttribute"+attr;
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("id",itemId);
    inp.setAttribute("type","text");
    inp.setAttribute("value",value);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("title","Attribute value. Visible as a comment in table-file.");
    if (dup) {
	inp.setAttribute("onblur","table_setAttribute('"+attr+"',this.value);table_setCat('"+cat+"');table_show();");
    } else {
	inp.setAttribute("onblur","table_setAttribute('"+attr+"',this.value);");
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
	btn.setAttribute("onclick","showDropdown('"+itemId+"')");
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
function table_loadColoc(file) {
    if (file != "") {
	var mfile=coloc_getModelConfigFile(file);
	if (coloc_modelIsNotLoaded(mfile)) {coloc_updateModelData(mfile);}
	var ofile=coloc_getObsConfigFile(file);
	if (coloc_obsIsNotLoaded(ofile)) {coloc_updateObsData(ofile);}
    };
};
function table_updateData(arg = table_getConfigFile()) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent table-load request.";
    var types=[];
    types[0]="table";
    types[1]="cat";
    //console.log("$$$$$ Loading table+cats with: ", args);
    $.get("cgi-bin/fark_load.pl",{type:types,arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		tableLoaded=true;
		//console.log("Updating dropdown for ",arg);
		table_setCat();
		table_show();
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Table request failed (system error)");}
	);
};
function table_mkdir(path) {
    var password=document.getElementById("tableConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"table",
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
	    function (error) { alert("Table mkdir request failed (system error)");}
	);
    
};

function table_rmfile(path) {
    var password=document.getElementById("tableConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"table",
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
			//delete table_config[path];
			if (table_file == path) {table_file="default.cfg";}
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Table rmfile request failed (system error)");}
	);
    
};

function table_fgfile(path) { // clear file from internal memory
    if (table_config[path] != undefined) {
	delete table_config[path];
    }
};

function table_rmdir(path) {
    var password=document.getElementById("tableConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"table",
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
	    function (error) { alert("Table rmdir request failed (system error)");}
	);
    
};

function table_mkfile(file) {
    //console.log("Calling saveConfigFile: '"+file+"'");
    table_setConfigFile(file);
    table_saveConfigFile(file);
};


function table_showConfigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent table-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"table",arg:args})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+arg+"'\n"+msg);
		} else {
		    var ret=dataToArray(data,status,documentLog);
		    var root=ret[0]||{};
		    //console.log("Updating dropdown for ",target);
		    removeChildren(item);
		    var added=false;
		    if (args.length >0 && looksLikeFile(args[0])) {
			var file=getFile(args[0]);
		    } else {
			var file="";
		    };
		    // add directories...
		    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
		    //console.log("Found entries: ",dirs.length-1,root);
		    var parent=dirs[0];
		    if (parent != null) {
			var dd=parent;
			//console.log("Adding up button: ",dd);
			addChildButton(item,"<up>","table_setConfigFile('"+dd+"');","Change to parent <directory>");
			added=true;
		    }
		    if (args.length == 1) {
			//console.log("Arg ret:",ret);
			if (root["type"] == "dir" && root["loc"] != "") {
			    addChildButton(item,"<rmdir>","table_rmdir('"+args[0]+"');","Remove <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<rmfile>","table_rmfile('"+args[0]+"');","Remove <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			    if (looksLikeFile(args[0])) {
				addChildButton(item,"<mkfile>","table_mkfile('"+args[0]+"');table_show();","Make <file>");
				if (table_config[args[0]] != undefined) {
				    addChildButton(item,"<fgfile>","table_fgfile('"+args[0]+"');","Forget <file>");
				}
				added=true;
			    } else {
				addChildButton(item,"<mkdir>","table_mkdir('"+args[0]+"');","Make <directory>");
				added=true;
			    }
			}
		    } else if (args.length == 2) {
			if (root["type"] == "dir") {
			    addChildButton(item,"<cpdir>","table_cpdir('"+args[0]+"','"+args[1]+"');","Copy <diretory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<cpfile>","table_cpfile('"+args[0]+"','"+args[1]+"');table_setConfigFile('"+args[2]+"');","Copy <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			}
		    };
		    var configfile=table_getConfigFile();
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
			console.log("Adding dir button: ",dd,":",configfile, dd===file);
			if (dd === configfile) {
			    addChildButtonShaded(item,dd,"table_setConfigFile('"+dd+"');table_show();","Use <file>");
			    added=true;
			} else if (looksLikeFile(dd)) {
			    addChildButton(item,dd,"table_setConfigFile('"+dd+"');table_show();","Use <file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"table_setConfigFile('"+dd+"');table_show();","Change <directory>");
			    added=true;
			}
		    }
		    if (! added) {addChildText(item,"No data available...");}
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Table request failed (system error)");}
	);
};

function table_showCat(item,target,arg) {
    var args=getArgs(arg);
    //documentLog.innerHTML="Sent cat-load request.";
    //$.get("cgi-bin/fark_load.pl",{type:"cat",arg:args})
    //    .success(
    //	function(data, status){
    //var ret=dataToArray(data,status,documentLog);
    //var root=ret[0];
    //console.log("Updating dropdown for ",target);
    removeChildren(item);
    var added=false;
    for (var cat in table_org_cats) {
	console.log("Adding config button: ",cat);
	addChildButton(item,cat,"table_setCat('"+cat+"');showValue('tableCat','"+cat+"');table_show()","Table category");
	added=true;
    }
    //documentLog.innerHTML="";
    //}).error(function (error) { alert("Request failed (system error)");});
    if (! added) {addChildText(item,"No data available...");}
};

function table_showTable(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent dir-load request.";
    var path=args[0] || "";
    var cls = "output";
    $.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:cls,path:path})
	.success(		
	    function(data, status){
		removeChildren(item);
		var added=false;
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+arg+"'\n"+msg);
		} else {
		    addWildcardButtons(item,target);
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			item.classList.toggle("show");
			var msg=getErrorMessage(errors);
			console.log("Error:"+path+"  "+msg);
			//alert("Unable to list '"+path+"'\n"+msg);
		    } else {
			var ls=data.getElementsByTagName("ls");
			if (ls.length > 0) {
			    var root=ls[0].getAttribute("root");
			    var loc=ls[0].getAttribute("location");
			    var pdirs=getSubDirs(cls,root,loc,"");
			    var parent=pdirs[0];
			    //console.log("Found parent: ",root,loc,parent);
			    if (parent != null) {
				var dd=root+parent;
				addChildButton(item,"<up>",
					       "table_setArray('table','"+dd+"');table_show();","Change to parent <directory>");
				added=true;
			    };
			    var dirs=ls[0].getElementsByTagName("dir");
			    //console.log("Found dir entries: ",dirs.length);
			    for (var ii=0; ii< dirs.length; ii++) {
				var dd = dirs[ii].getAttribute("path");
				//console.log("Adding dir button: ",dd);
				addChildButton(item,dd,"table_setArray('table','"+dd+"');table_show();","Change <directory>");
				added=true;
			    };
			    var patts=ls[0].getElementsByTagName("pattern");
			    //console.log("Found file entries: ",patts.length);
			    for (var ii=0; ii< patts.length; ii++) {
				var rr = path+"/"+getFile(patts[ii].getAttribute("regexp"));
				var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
				if (dd !== '') {
				    //console.log("Adding pattern button: ",dd,rr);
				    addChildButtonShaded(item,dd,"table_setArray('table','"+rr+"');table_show();","Use pattern");
				    added=true;
				};
			    };
			    var fils=ls[0].getElementsByTagName("file");
			    //console.log("Found file entries: ",fils.length);
			    for (var ii=0; ii< fils.length; ii++) {
				var dd = fils[ii].getAttribute("path");
				var size = fils[ii].getAttribute("size")
				if (dd !== '') {
				    //console.log("Adding file button: ",dd,ii);
				    addChildButton(item,size+" "+dd,"table_setArray('table','"+dd+"');table_show();","Use <file>");
				    added=true;
				};
			    };
			};
		    };
		};
		if (! added) {addChildText(item,"No data available...");}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Table table request failed (system error)");}
	);
};

function table_showGraphics(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent dir-load request.";
    var path=args[0] || "";
    var cls = "output";
    $.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:cls,path:path})
	.success(
	    function(data, status){
		removeChildren(item);
		var added=false;
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+arg+"'\n"+msg);
		} else {
		    addWildcardButtons(item,target);
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			item.classList.toggle("show");
			var msg=getErrorMessage(errors);
			console.log("Error:"+path+"  "+msg);
			//alert("Unable to list '"+path+"'\n"+msg);
		    } else {
			var ls=data.getElementsByTagName("ls");
			if (ls.length > 0) {
			    var root=ls[0].getAttribute("root");
			    var loc=ls[0].getAttribute("location");
			    var pdirs=getSubDirs(cls,root,loc,"");
			    var parent=pdirs[0];
			    //console.log("Found parent: ",root,loc,parent);
			    if (parent != null) {
				var dd=root+parent;
				addChildButton(item,"<up>",
					       "table_setArray('graphics','"+dd+"');table_show();","Change to parent <directory>");
				added=true;
			    };
			    var dirs=ls[0].getElementsByTagName("dir");
			    //console.log("Found dir entries: ",dirs.length);
			    for (var ii=0; ii< dirs.length; ii++) {
				var dd = dirs[ii].getAttribute("path");
				//console.log("Adding dir button: ",dd);
				addChildButton(item,dd,"table_setArray('graphics','"+dd+"');table_show();","Change <directory>");
				added=true;
			    };
			    var patts=ls[0].getElementsByTagName("pattern");
			    //console.log("Found file entries: ",patts.length);
			    for (var ii=0; ii< patts.length; ii++) {
				var rr = getFile(patts[ii].getAttribute("regexp"));
				var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
				if (dd !== '') {
				    //console.log("Adding file button: ",dd,rr);
				    addChildButtonShaded(item,dd,"table_setArray('graphics','"+rr+"');table_show();","Use <pattern>");
				    added=true;
				};
			    };
			    var fils=ls[0].getElementsByTagName("file");
			    //console.log("Found file entries: ",fils.length);
			    for (var ii=0; ii< fils.length; ii++) {
				var dd = fils[ii].getAttribute("path");
				var size = fils[ii].getAttribute("size")
				if (dd !== '') {
				    //console.log("Adding file button: ",dd,ii);
				    addChildButton(item,size+" "+dd,"table_setArray('graphics','"+dd+"');table_show();","Use <file>");
				    added=true;
				};
			    };
			};
		    };
		};
		if (! added) {addChildText(item,"No data available...");}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Table dir request failed (system error)");}
	);
};

function table_showSet(item,target,arg) {
    var args=getArgs(arg);
    //documentLog.innerHTML="Sent line-load request.";
    //$.get("cgi-bin/fark_load.pl",{type:"cat",arg:args})
    //    .success(
    //	function(data, status){
    //var ret=dataToArray(data,status,documentLog);
    //var root=ret[0];
    //console.log("Updating dropdown for ",target);
    removeChildren(item);
    var added=false;
    var file=table_getConfigFile();
    //console.log("Looking for file:",file);
    var cat=table_config[file]["cat"];
    for (var line in table_cats[cat]["lines"]) {
	var ll=table_cats[cat]["lines"][line];
	var cmd="showValue('tableSet','"+line+"');showValue('tableType',table_cats['"+cat+"'][\"lines\"]['"+line+"']);";
	var str=line+" ("+ll+")"
	console.log("Adding config button: ",str);
	addChildButton(item,str,cmd,"Data set identification");
	added=true;
    }
    if (! added) {addChildText(item,"No data available...");}
    //documentLog.innerHTML="";
    //}).error(function (error) { alert("Table set request failed (system error)");});}
};

function table_showColoc(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent table-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"coloc",arg:args})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+arg+"'\n"+msg);
		} else {
		    var ret=dataToArray(data,status,documentLog);
		    var root=ret[0]||{};
		    //console.log("Updating dropdown for ",target);
		    removeChildren(item);
		    var added=false;
		    if (args.length >0 && looksLikeFile(args[0])) {
			var file=getFile(args[0]);
		    } else {
			var file="";
		    };
		    // add directories...
		    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
		    //console.log("Found entries: ",dirs.length-1,root);
		    var parent=dirs[0];
		    if (parent != null) {
			var dd=parent;
			//console.log("Adding up: ",dd);
			addChildButton(item,"<up>","showValue('tableColoc','"+dd+"');","Change to parent <directory>");
			added=true;
		    } else {
			//console.log("Adding clear: ",dd);
			addChildButton(item,"<up>","showValue('tableColoc','');","Change to root <directory>");
			added=true;
		    }
		    if (dirs.length > 0) {
			var configfile=table_getColocConfigFile();
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
				//console.log("Adding dir button: ",dd,ii);
				// colocation file 'dd' must be 'loaded' if it is selected....!!!
				
				if (dd===configfile) {
				    addChildButtonShaded(item,dd,"showValue('tableColoc','"+dd+"');table_loadColoc('"+dd+"');","Change <directory>");
				}else {
				    addChildButton(item,dd,"showValue('tableColoc','"+dd+"');table_loadColoc('"+dd+"');","Change <directory>");
				};
				added=true;
			    }
			}
		    }
		    if (! added) {addChildText(item,"No data available...");}
		    //console.log("There: ",dirs);
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Table coloc request failed (system error)");}
	);
};

function table_showExpression(item,target,arg) {
    var cnt = target.substring(15);
    removeChildren(item);
    var added=false;
    var cfile=table_getColocConfigFile();
    var mfile=table_getModelConfigFile();
    var ofile=table_getObsConfigFile();
    var mod=(mfile !== "");
    var obs=(ofile !== "");
    // model index
    if (mod) {
	if (model_config[mfile]!== undefined) {
	    var indexTarget=model_config[mfile]["indexTarget"];
	    addTargetButtonShaded(item,target,indexTarget,"model index target (see model index)");
	};
	// list model trgs in coloc_config
 	if (coloc_config[cfile] !== undefined) {
	    var trgs=coloc_config[cfile]["modelConfigFile"]["targets"];
	    for (var trg in trgs) {
		addTargetButton(item,target,trg,"model target");
	    };
	};
    };
    // list obs trgs in obs_config
    if (obs) {
	if (obs_config[ofile] !== undefined) {
	    var trgs=obs_config[ofile]["targets"];
	    for (var trg in trgs) {
		addTargetButton(item,target,trg,"observation target (see observation index)");
	    };
	    trg = obs_config[ofile]["indexTarget"];
	    addTargetButtonShaded(item,target,trg,"observation index target (see observation index)");
	}
	// list obs trgs in coloc_config
 	if (coloc_config[cfile] !== undefined) {
	    var trgs=coloc_config[cfile]["obsConfigFile"]["targets"];
	    for (var trg in trgs) {
		addTargetButton(item,target,trg,"observation target");
	    };
	};
    };
    addFunctionButtons(item,target);
    added=true;
    if (! added) {addChildText(item,"No data available...");}
};

function table_showAttribute(item,target,arg) {
    var attr = target.substring(14);
    var file=table_getConfigFile();
    var cat=table_config[file]["cat"];
    if (table_cats[cat] === undefined) {table_setCat(cat);}
    var val=table_cats[cat]["attributes"][attr];
    var radio=val instanceof Array; // should we have radio button?
    var dup=(attr.substr(0,1) === "_");
    removeChildren(item);
    var added=false;
    if (radio) {
	for (var vv=0; vv < val.length;vv++) {
	    //console.log("Attribute '",attr,"' value  ",vv,val[vv]);
	    if (dup) {
		addChildButton(item,val[vv],"table_setAttribute('"+attr+"','"+val[vv]+"');table_setCat('"+cat+"');table_show();","Use <attribute duplicator>");
		added=true;
	    } else {
		addChildButton(item,val[vv],"table_setAttribute('"+attr+"','"+val[vv]+"');","Use <attribute value>");
		added=true;
	    };
	};
    } else {
	console.log("Invalid cat:", JSON.stringify(val),attr,file);
    }
    if (! added) {addChildText(item,"No data available...");}
};

function table_showDebugExpression(item,target,arg) {
    removeChildren(item);
    var added=false;
    addLogicalButtons(item,target);
    addFunctionButtons(item,target);
    added=true;
    if (! added) {addChildText(item,"No data available...");}
};
