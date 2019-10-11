    join_file = "default.cfg";
join_config = { "default.cfg" : { filterDir: "/opdata",
				  filterDirStat: "",
				  filterDirMin: "",
				  filterDirMax: "",
				  filterFile: ".*\.nc",
				  hits : "?",
				  min: {x:0,y:1},
				  max: {x:2,y:3},
				  attributes : { def: "default"},
				  table : "table.ps",
				  graphics : "default.ps",
				  cat : "Text",
				  password: "test"
				}
	      };
join_org_cats = { "Text": {attributes : {xlabel:"X", ylabel:"Y"},
	  		   order : ["xlabel","ylabel"],
			   lines : {1:"solid"},
			   colnames_ : ["X-expression","Y-expression"]}
		};
join_cats  ={};
join_order =["Text"];
join_configEd = 0;

function join_print(file) {
    if (join_config[file]!== undefined) {
	//console.log("File:",file," Dataset:",Object.keys(join_config[file]["dataset"]).length);
    } else {
	//console.log("File:",file," Dataset is undefined.");
    }
}

function join_allocate(file) {
    if (join_config[join_file] === undefined) {
	console.log("Corrupt join_file:",join_file);
    } else if (join_config[file] === undefined) {
	join_config[file]=clone(join_config[join_file]);
	console.log("cloned:",join_file," -> ",file);
    }
}
function join_setConfigFile(file) {
    showValue('joinConfigFile',file);
    showValue('joinConfigFileSave',file);
    //if (file != "") {
   //console.log("Setting join config file:",file);
    join_allocate(file);
    join_file=file;
    //console.log("Cat:",join_config[file]["cat"]," Join_file:",join_file);
    join_setCat();
    //};
}
function join_getConfigFile() {
    return join_file;
};
function join_getColocConfigFile() {
    var file = document.getElementById("joinColoc").value;
    return file;
};
function join_getModelConfigFile() {
    var file=join_getColocConfigFile();
    if (coloc_config[file] !== undefined) {
	return coloc_config[file]["modelConfigFile"]["file"];
    }
};
function join_getObsConfigFile() {
    var file=join_getColocConfigFile();
    if (coloc_config[file] !== undefined) {
	return coloc_config[file]["obsConfigFile"]["file"];
    }
};
function join_setArray(parameter,value) {
    var file=join_getConfigFile();
    //console.log("File:",file,parameter,join_config[file]);
    join_config[file][parameter]=decodeURI(value);
};

function join_expandCat(cat) {
    var file=join_getConfigFile();
    join_cats[cat]=goclone(join_org_cats[cat]);
    if (join_org_cats[cat] ===undefined) {
	console.log("Missing category:",cat);
	return;
    };
    for (var attr in join_org_cats[cat]["attributes"]) {
	//console.log("Found org attribute:",attr);
    }
    for (var attr in join_org_cats[cat]["attributes"]) {
	if (attr.substr(0,1) === "_") {
	    if (join_config[file]!== undefined &&
		join_config[file]["attributes"][attr] !== undefined) {
		var nn = join_config[file]["attributes"][attr];
	    } else {
		var val=join_org_cats[cat]["attributes"][attr];
		if (val instanceof Array) {
		    var nn=val[0]; // first element
		} else {
		    var nn=val;
		}
	    };
	   //console.log("Duplicator attribute '"+attr+"' = ",nn);
	    var re = new RegExp("(\w*)"+RegExp.quote(attr)+"(\w*)", "g");
	    for (var aa in join_cats[cat]["attributes"]) {
		if (aa.match(re) && aa !== attr) {
		    //console.log("Attribute match '"+aa+"' == '"+attr+"'");
		    // delete aa attribute
		    var val=join_cats[cat]["attributes"][aa];
		    delete join_cats[cat]["attributes"][aa];
		    var index = join_cats[cat]["order"].indexOf(aa);
		    join_cats[cat]["order"].splice(index, 1);
		    for (var ii=nn;ii>0;ii--) {
			var newattr = aa.replace(re, '$1'+ii.toString()+'$2');
			// add attribute
			//console.log("Adding attribute '"+newattr+"' = ",val);
			join_cats[cat]["attributes"][newattr]=val;
			join_cats[cat]["order"].splice(index,0,newattr);
		    }
		} else {
		    //console.log("Attribute no match '"+aa+"' != '"+attr+"'");
		}
	    }
	    for (var jj = 0; jj <  join_cats[cat]["colnames_"].length;jj++) {
		var cc=join_cats[cat]["colnames_"][jj];
		if (cc.match(re)) {
		    // delete cc column
		   //console.log("Column match '"+cc+"' == '"+attr+"'");
		    var index = join_cats[cat]["colnames_"].indexOf(cc);
		    join_cats[cat]["colnames_"].splice(index, 1);
		    for (var ii=nn;ii>0;ii--) {
			var newcol = cc.replace(re, '$1'+ii.toString()+'$2');
			//console.log("Adding column '"+newcol+"'");
			// add column
			join_cats[cat]["colnames_"].splice(index,0,newcol);
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

function join_setCat(value) {
    var file=join_getConfigFile();
    if (value===undefined) {
	value=join_config[file]["cat"]
    };
    join_expandCat(value);
    if (join_cats[value] === undefined) {
	console.log("Attempt to set undefined join-category:",value);
	return;
    }
    //console.log("File:",file,parameter,join_config[file]);
    join_config[file]["cat"]=value;
    // sync file and cat attributes
    for (var attr in join_config[file]["attributes"]) {
	if (join_cats[value]===undefined || 
	    join_cats[value]["attributes"][attr] === undefined) {
	    delete join_config[file]["attributes"][attr];
	};
    }
    for (var attr in join_cats[value]["attributes"]) {
	if (join_config[file]["attributes"][attr] === undefined) {
	    var val=join_cats[value]["attributes"][attr];
	    if (val instanceof Array) {
		join_config[file]["attributes"][attr]=val[0]; // first element
	    } else {
		join_config[file]["attributes"][attr]=val;
	    }
	};
    }
};
function join_setDataset(target,parameter,value) {
    var file=join_getConfigFile();
    if (join_config[file]["dataset"][target] == undefined) {
	join_config[file]["dataset"][target]={};
    }
    join_config[file]["dataset"][target][parameter]=value;
};
function join_setDatasetColumn(target,parameter,value) {
    var file=join_getConfigFile();
    if (join_config[file]["dataset"][target] == undefined) {
	join_config[file]["dataset"]["colnames"][target]={};
	join_config[file]["dataset"]["columns"][target]={};
    }
    join_config[file]["dataset"][target][parameter]=value;
};
function join_setAttribute(attr,value) {
    var file=join_getConfigFile();
    join_config[file]["attributes"][attr]=value;
};
function join_setTypeAttribute(type,attr,value) {
    var file=join_getConfigFile();
    if (join_config[file] === undefined) {join_config[file] ={}};
    if (join_config[file][type] === undefined) {join_config[file][type] ={}};
    join_config[file][type][attr]=value;
};
function join_show() {
    var file=join_getConfigFile();
    //console.log("Showing:",file);
    if (file != "") {
	join_allocate(file);
	showValue('joinConfigFile',file);
	showValue('joinConfigFileSave',file);
	showValue('joinCat',join_config[file]["cat"]);
	showValue('joinTable',join_config[file]["table"]);
	showValue('joinGraphics',join_config[file]["graphics"]);
	join_showDatasetTable();
	join_showAttributesTable();
    };
};
// joinervation config methods
function join_checkPassword() {
    var password=document.getElementById("joinConfigFilePsw").value;
    var file=join_getConfigFile();
    if (join_config[file] !== undefined) {
	if (join_config[file]["password"] !== undefined) {
	    if (join_config[file]["password"] !== password) {
		alert("Invalid password used when attempting to save file:\n"+file);
		return false;
	    };
	};
    };
    return true;
}
function join_isEmpty(obj) {
    for(var prop in obj) {
        if(obj.hasOwnProperty(prop))
            return false;
    }
    return true;
};

function join_saveConfigFile() {
    var file=join_getConfigFile();
    var password=document.getElementById("joinConfigFilePsw").value;
    var filterDir="";
    var filterDirMin="";
    var filterDirMax="";
    var filterFile="";
    var hits="";
    var cat="";
    var table="";
    var graphics="";
    var joinCols="";
    var joinColMin="";
    var joinColMax="";
    var joinAttrs="";
    if (join_config[file] != undefined) {
	filterDir=model_config[file]["filterDir"]//"";
	filterDirMin=model_config[file]["filterDirMin"]//"";
	filterDirMax=model_config[file]["filterDirMax"]//"";
	filterFile=model_config[file]["filterFile"]//"";
	hits=model_config[file]["hits"]//"";
	cat=join_config[file]["cat"]//"";
	table=join_config[file]["table"]//"";
	graphics=join_config[file]["graphics"]//"";
	if (join_cats[cat] != undefined) {
	    var colnames_=join_cats[cat]["colnames_"]//[];
	    for (var ii =0; ii< colnames_.length;ii++) {
		var col=colnames_[ii];
		if (joinCols.length==0) {
		    joinCols=col;
		    joinColMin=join_config[file]["min"][col]||"";
		    joinColMax=join_config[file]["max"][col]||"";
		} else {
		    joinCols=joinCols+"~"+colnames_[ii];
		    joinColMin=joinColMin+"~"+join_config[file]["min"][col]||"";
		    joinColMax=joinColMax+"~"+join_config[file]["max"][col]||"";
		}
	    }
	    var order=join_cats[cat]['order']//[];
	    var attrs=join_config[file]["attributes"]//{};
	    for (var ii=0;ii<order.length;ii++) {
		var attr=order[ii];
		var value=attrs[attr];
		if (value !== undefined) {
		    joinAttrs=joinAttrs + "|" + attr + "~" + value;
		}
	    };
	}
    };
   //console.log("Saving: "+file+" "+cat+" "+table+" "+graphics+" "+joinCols+" "+joinAttrs, join_config[file]);
    join_configEd++;
    documentLog.innerHTML="Sent join-save request.";
    $.get("cgi-bin/fark_save.pl",
	  {type:"join",
	   file:file,
	   password:password,
	   filterDir:filterDir,
	   filterDirMin:filterDirMin,
	   filterDirMax:filterDirMax,
	   filterFile:filterFile,
	   hits:hits,
	   cat:cat,
	   table:table,
	   graphics:graphics,
	   columns:joinCols,
	   columnMin:joinColMin,
	   columnMax:joinColMax,
	   attributes:joinAttrs
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
	    function (error) { alert("Join save request failed (system error)");}
	);
    makeUrl("join",file);
};
// Transposed function...
function join_showDatasetTable() {
   //console.log(":::::::::: showDatasetTable");
    var file=join_getConfigFile();
    var cat=join_config[file]["cat"];
    var colnames_={};
    var col1=[];
    var colmin=[];
    var colmax=[];
    if (join_cats[cat] !== undefined) {
	colnames_=join_cats[cat]["colnames_"];
	// make column headers
	col1=[];
	colmin=[];
	colmax=[];
	for (var ii =0; ii< colnames_.length;ii++) {
	    col1.push(join_cats[cat]["colnames_"][ii]);
	    colmin.push(join_config[file]["min"][colnames_[ii]]||"");
	    colmax.push(join_config[file]["max"][colnames_[ii]]||"");
	}
    }
    var item=document.getElementById('joinDatasetTable');
    var tbody=removeTableChildren(item);
    join_insertDataset(tbody,col1,colmin,colmax);
}
//
function join_insertDataset(item,col1,colmin,colmax) {
    // insert header row
    var row = document.createElement("TR");
    join_insertHeader(row,["Column"],0);
    join_insertHeader(row,["Min"],0);
    join_insertHeader(row,["Max"],0);
    
    item.appendChild(row);
    // insert rows
    for (var ii=0;ii<col1.length;ii++) {
	var row = document.createElement("TR");
	join_insertHeader(row,col1,ii);
	join_insertCell(row,col1,"min",colmin,ii);
	join_insertCell(row,col1,"max",colmax,ii);
	item.appendChild(row);
    }
}
//
function join_insertHeader(row,col1,ii) {
    th=document.createElement("TH");
    bf=document.createElement("BF");
    th.setAttribute("bgcolor","#00b9f2");
    bf.innerHTML=col1[[ii]];
    th.appendChild(bf);
    row.appendChild(th);
}
//
function join_insertCell(row,col1,type,col,ii) {
    //console.log("insertItem Inserting:",jj,ii,data[[jj]][[ii]]);
    var td;
    var inp;
    // make expression column
    var itemId="joinExpression"+(ii);
    //console.log("insertNew Inserting:",itemId);
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    td.setAttribute("style","width:50%");
    inp=document.createElement("INPUT");
    inp.setAttribute("id",itemId);
    inp.setAttribute("type","text");
    inp.setAttribute("value","");
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","join_setTypeAttribute('"+type+"','"+col1[[ii]]+"',this.value);");
    inp.setAttribute("title","Limit expression");
    inp.value=col[[ii]]||"";
    td.appendChild(inp);
    row.appendChild(td);
}
//
function join_showAttributesTable() {
    var file=join_getConfigFile();
    var cat=join_config[file]["cat"];
    var order=[];
    if (join_cats[cat] !== undefined) {
	order=join_cats[cat]['order'];;
    } else {
	console.log("Undefined category:",cat);
    }
    var item=document.getElementById('joinAttributesTable');
    var head=removeTableChildFrom(item,"labelsJoinAttribute");
    var value=join_config[file]['attributes'];
    for (var ii=order.length-1;ii>=0;ii--) {
	var attr=order[ii];
	var val=join_cats[cat]["attributes"][attr];
	join_insertAttributeRow(head,cat,attr,value[attr],val);
    }
}
function join_insertAttributeRow(item,cat,attr,value,val) {
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
    var itemId="joinAttribute"+attr;
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("id",itemId);
    inp.setAttribute("type","text");
    inp.setAttribute("value",value);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("title","Attribute value. Visible as a comment in table-file.");
    if (dup) {
	inp.setAttribute("onblur","join_setAttribute('"+attr+"',this.value);join_setCat('"+cat+"');join_show();");
    } else {
	inp.setAttribute("onblur","join_setAttribute('"+attr+"',this.value);");
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
function join_loadColoc(file) {
    if (file != "") {
	var mfile=coloc_getModelConfigFile(file);
	if (coloc_modelIsNotLoaded(mfile)) {coloc_updateModelData(mfile);}
	var ofile=coloc_getObsConfigFile(file);
	if (coloc_obsIsNotLoaded(ofile)) {coloc_updateObsData(ofile);}
    };
};
function join_updateData(arg = join_getConfigFile()) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent join-load request.";
    var types=[];
    types[0]="join";
    types[1]="cat";
   //console.log("$$$$$ Loading join+cats with: ", args);
    $.get("cgi-bin/fark_load.pl",{type:types,arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		joinLoaded=true;
		//console.log("Updating dropdown for ",arg);
		join_setCat();
		join_show();
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Join request failed (system error)");}
	);
};
function join_mkdir(path) {
    var password=document.getElementById("joinConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"join",
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
	    function (error) { alert("Join mkdir request failed (system error)");}
	);
    
};

function join_rmfile(path) {
    var password=document.getElementById("joinConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"join",
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
			//delete join_config[path];
			if (join_file == path) {join_file="default.cfg";}
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Join rmfile request failed (system error)");}
	);
    
};

function join_fgfile(path) { // clear file from internal memory
    if (join_config[path] != undefined) {
	delete join_config[path];
    }
};

function join_rmdir(path) {
    var password=document.getElementById("joinConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"join",
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
	    function (error) { alert("Join rmdir request failed (system error)");}
	);
    
};

function join_mkfile(file) {
   //console.log("Calling saveConfigFile: '"+file+"'");
    join_setConfigFile(file);
    join_saveConfigFile(file);
};

