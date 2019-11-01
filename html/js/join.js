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
				  graphics :"/lustre/storeA",
				  overwrite:"true",
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
    join_config[file][parameter]=decodeURI(value);
    console.log("File:",file,parameter,value,JSON.stringify(join_config[file]));
};

function join_expandCat(cat) {
    var file=join_getConfigFile();
    join_cats[cat]=join_goclone(join_org_cats[cat]);
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
	    var re = new RegExp("(\w*)"+join_quote(attr)+"(\w*)", "g");
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

function join_quote(str) {
    var re = new RegExp("[.?*+^$[](){}|-\\]", "g");
    return (str+'').replace(re, "\\$&");
};
function join_goclone(source) {
    if (Object.prototype.toString.call(source) === '[object Array]') {
        var clone = [];
        for (var i=0; i<source.length; i++) {
            clone[i] = join_goclone(source[i]);
        }
        return clone;
    } else if (typeof(source)=="object") {
        var clone = {};
        for (var prop in source) {
            if (source.hasOwnProperty(prop)) {
                clone[prop] = join_goclone(source[prop]);
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
	if (join_config[file] !== undefined) { 
	    value=join_config[file]["cat"]
	} else {
	    value=document.getElementById("joinCat").value;
	};
    };
    join_expandCat(value);
    if (join_cats[value] === undefined) {
	console.log("Attempt to set undefined join-category:",value);
	return;
    }
    //console.log("SetCat:",file,value,JSON.stringify(join_cats));
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
    console.log("SetTypeAttribute:",file,type,attr,value);
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
	showValue('joinFilterDir',join_config[file]["filterDir"]);
	showValue('joinFilterDirMin',join_config[file]["filterDirMin"]);
	showValue('joinFilterDirMax',join_config[file]["filterDirMax"]);
	showValue('joinFilterFile',join_config[file]["filterFile"]);
	showValue('joinTable',join_config[file]["table"]);
	showValue('joinGraphics',join_config[file]["graphics"]);
	setValue('joinOverwrite',join_config[file]["overwrite"]);
	join_showAttributesTable();
	join_showDatasetTable();
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
    var overwrite="";
    var joinCols="";
    var joinColMin="";
    var joinColMax="";
    var joinAttrs="";
    if (join_config[file] != undefined) {
	filterDir=join_config[file]["filterDir"]||"";
	filterDirMin=join_config[file]["filterDirMin"]||"";
	filterDirMax=join_config[file]["filterDirMax"]||"";
	filterFile=join_config[file]["filterFile"]||"";
	hits=join_config[file]["hits"]||"";
	cat=join_config[file]["cat"]||"";
	table=join_config[file]["table"]||"";
	graphics=join_config[file]["graphics"]||"";
	overwrite=join_config[file]["overwrite"]||"true";
	if (join_cats[cat] != undefined) {
	    var colnames_=join_cats[cat]["colnames_"]||[];
	    for (var ii =0; ii< colnames_.length;ii++) {
		var col=colnames_[ii];
		if (joinCols.length==0) {
		    joinCols=col;
		    joinColMin=(join_config[file]["min"][col]||"");
		    joinColMax=(join_config[file]["max"][col]||"");
		} else {
		    joinCols=joinCols+"~"+colnames_[ii];
		    joinColMin=joinColMin+"~"+(join_config[file]["min"][col]||"");
		    joinColMax=joinColMax+"~"+(join_config[file]["max"][col]||"");
		}
	    }
	    var order=join_cats[cat]['order']||[];
	    var attrs=join_config[file]["attributes"]||{};
	    for (var ii=0;ii<order.length;ii++) {
		var attr=order[ii];
		var value=attrs[attr];
		if (value !== undefined) {
		    joinAttrs=joinAttrs + "|" + attr + "~" + value;
		}
	    };
	}
    };
    //console.log("Saving: "+file+" "+cat+" "+table+" "+joinCols+" "+joinAttrs, join_config[file]);
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
	   overwrite:overwrite,
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
    var head=removeTableChildFrom(item,"labelsJoinDataset");
    join_insertDataset(head,col1,colmin,colmax);
}
//
function join_insertDataset(item,col1,colmin,colmax) {
    // insert rows
    for (var ii=0;ii<col1.length;ii++) {
	var row = document.createElement("TR");
	join_insertHeader(row,col1,ii);
	join_insertCell(row,col1,"min",colmin,ii);
	join_insertCell(row,col1,"max",colmax,ii);
	item.parentNode.insertBefore(row,item.nextSimbling);
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
    //td.setAttribute("style","width:25%");
    inp=document.createElement("INPUT");
    inp.setAttribute("id",itemId);
    inp.setAttribute("type","text");
    inp.setAttribute("value","");
    inp.setAttribute("style","width:100%");
    //console.log("Added button:",ii,type,col1[[ii]]);
    inp.setAttribute("onblur","join_setTypeAttribute('"+type+"','"+(col1[[ii]]||"")+"',this.value);");
    inp.setAttribute("title","Limit expression");
    inp.value=col[[ii]]||"";
    td.appendChild(inp);
    row.appendChild(td);
}
//
//check if directory exists...
function join_setFilterDir(value) {
    var file=join_getConfigFile();
   //console.log("File:",file,"filterDir",join_config[file],value);
    var val=decodeURI(value);
    join_config[file]["filterDir"]=val;
    $.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:"data",path:val})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length == 0 ) {
		    document.getElementById('joinFilterDir').style.color='black'
		    join_config[file]["filterDirStat"]="";
		   //console.log("Dir ok:",val);
		} else {
		    join_config[file]["filterDirStat"]=val;
		    document.getElementById('joinFilterDir').style.color='red'
		    console.log("Dir NOT ok:",val);
		}
		join_show();
	    })
	.error(
	    function (error) { alert("Join filter dir request failed (system error)");}
	);
};
function join_showAttributesTable() {
    var file=join_getConfigFile();
    var cat=join_config[file]["cat"];
    var order=[];
    if (join_cats[cat] !== undefined) {
	order=join_cats[cat]['order'];;
    } else {
	console.log("Undefined category:",cat);
    }
    //console.log("showAttributesTavble:",JSON.stringify(join_cats[cat]),JSON.stringify(order));
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

function join_fileFind(sfile) {
    var file=join_getConfigFile();
    join_config[file]["stack"]=sfile;
    var password=document.getElementById("joinConfigFilePsw").value;
    documentLog.innerHTML="Sent join-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"joinfile",
				  file:file,
				  password:password,
				  target:sfile})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to scan file: "+sfile+" (file:"+file+")\n"+msg);
		    } else {
			dataToArray(data,status,documentLog);
			join_show();
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Join find request failed (system error)");}
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


function join_showConfigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent join-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"join",arg:args})
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
			addChildButton(item,"<up>","join_setConfigFile('"+dd+"');","Change to parent <directory>");
			added=true;
		    }
		    if (args.length == 1) {
			//console.log("Arg ret:",ret);
			if (root["type"] == "dir" && root["loc"] != "") {
			    addChildButton(item,"<rmdir>","join_rmdir('"+args[0]+"');","Remove <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<rmfile>","join_rmfile('"+args[0]+"');","Remove <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			    if (looksLikeFile(args[0])) {
				addChildButton(item,"<mkfile>","join_mkfile('"+args[0]+"');join_show();","Make <file>");
				if (join_config[args[0]] != undefined) {
				    addChildButton(item,"<fgfile>","join_fgfile('"+args[0]+"');","Forget <file>");
				}
				added=true;
			    } else {
				addChildButton(item,"<mkdir>","join_mkdir('"+args[0]+"');","Make <directory>");
				added=true;
			    }
			}
		    } else if (args.length == 2) {
			if (root["type"] == "dir") {
			    addChildButton(item,"<cpdir>","join_cpdir('"+args[0]+"','"+args[1]+"');","Copy <diretory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<cpfile>","join_cpfile('"+args[0]+"','"+args[1]+"');join_setConfigFile('"+args[2]+"');","Copy <file>");
			    added=true;
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
			//console.log("Adding dir button: ",dd);
			if (looksLikeFile(dd)) {
			    addChildButton(item,dd,"join_setConfigFile('"+dd+"');join_show();","Use <file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"join_setConfigFile('"+dd+"');join_show();","Change <directory>");
			    added=true;
			}
		    }
		    if (! added) {addChildText(item,"No data available...");}
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Join request failed (system error)");}
	);
};

function join_showCat(item,target,arg) {
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
    for (var cat in join_org_cats) {
	console.log("Adding config button: ",cat);
	addChildButton(item,cat,"console.log('Button setcat','"+cat+"');join_setCat('"+cat+"');showValue('joinCat','"+cat+"');join_show()","Join category");
	added=true;
    }
    //documentLog.innerHTML="";
    //}).error(function (error) { alert("Request failed (system error)");});
    if (! added) {addChildText(item,"No data available...");}
};

function join_showTable(item,target,arg) {
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
					       "join_setArray('table','"+dd+"');join_show();","Change to parent <directory>");
				added=true;
			    };
			    var dirs=ls[0].getElementsByTagName("dir");
			    //console.log("Found dir entries: ",dirs.length);
			    for (var ii=0; ii< dirs.length; ii++) {
				var dd = dirs[ii].getAttribute("path");
				//console.log("Adding dir button: ",dd);
				addChildButton(item,dd,"join_setArray('table','"+dd+"');join_show();","Change <directory>");
				added=true;
			    };
			    var patts=ls[0].getElementsByTagName("pattern");
			    //console.log("Found file entries: ",patts.length);
			    for (var ii=0; ii< patts.length; ii++) {
				var rr = path+"/"+ getFile(patts[ii].getAttribute("regexp"));
				var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
				if (dd !== '') {
				    //console.log("Adding pattern button: ",dd,rr);
				    addChildButtonShaded(item,dd,"join_setArray('table','"+rr+"');join_show();","Use pattern");
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
				    addChildButton(item,size+" "+dd,"join_setArray('table','"+dd+"');join_show();","Use <file>");
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
	    function (error) { alert("Join table request failed (system error)");}
	);
};

function join_showGraphics(item,target,arg) {
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
					       "join_setArray('graphics','"+dd+"');join_show();","Change to parent <directory>");
				added=true;
			    };
			    var dirs=ls[0].getElementsByTagName("dir");
			    //console.log("Found dir entries: ",dirs.length);
			    for (var ii=0; ii< dirs.length; ii++) {
				var dd = dirs[ii].getAttribute("path");
				//console.log("Adding dir button: ",dd);
				addChildButton(item,dd,"join_setArray('graphics','"+dd+"');join_show();","Change <directory>");
				added=true;
			    };
			    var patts=ls[0].getElementsByTagName("pattern");
			    //console.log("Found file entries: ",patts.length);
			    for (var ii=0; ii< patts.length; ii++) {
				var rr = getFile(patts[ii].getAttribute("regexp"));
				var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
				if (dd !== '') {
				    //console.log("Adding file button: ",dd,rr);
				    addChildButtonShaded(item,dd,"join_setArray('graphics','"+rr+"');join_show();","Use <pattern>");
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
				    addChildButton(item,size+" "+dd,"join_setArray('graphics','"+dd+"');join_show();","Use <file>");
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
	    function (error) { alert("Join dir request failed (system error)");}
	);
};

function join_showFilterDir(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent join-load request.";
    var file=join_getConfigFile();
    var path=args[0] || "";
    var cls = "data";
    var filter=join_config[file]["filterFile"];
    $.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:cls,path:path,filter:filter})
	.success(
	    function(data, status){
		removeChildren(item);
		var added=false;
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+path+"'\n"+msg);
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
					   "join_setArray('filterDir','"+dd+"');join_show();","Change to parent <directory>");
			    added=true;
			};
			var dirs=ls[0].getElementsByTagName("dir");
			//console.log("Found dir entries: ",dirs.length);
			for (var ii=0; ii< dirs.length; ii++) {
			    var dd = dirs[ii].getAttribute("path");
			    console.log("Adding dir button: ",dd);
			    if (looksLikeFile(dd)) {
				addChildButton(item,dd,"join_setArray('filterDir','"+dd+"');join_show();","Use <file>");
				added=true;
			    } else {
				addChildButton(item,dd,"join_setArray('filterDir','"+dd+"');join_show();","Change <directory>");
				added=true;
			    }
			};
			var patts=ls[0].getElementsByTagName("pattern");
			//console.log("Found file entries: ",patts.length);
			for (var ii=0; ii< patts.length; ii++) {
			    var rr = getFile(patts[ii].getAttribute("regexp"));
			    var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
			    if (dd !== '') {
				//console.log("Adding file button: ",dd,rr);
				addChildButtonShaded(item,dd,"join_setArray('filterFile','"+rr+"');join_show();","Copy <pattern> to filter");
				added=true;
			    };
			};
			var fils=ls[0].getElementsByTagName("file");
			//console.log("Found file entries: ",fils.length);
			for (var ii=0; ii< fils.length; ii++) {
			    var dd = getFile(fils[ii].getAttribute("path"));
			    var size = fils[ii].getAttribute("size")
			    if (dd !== '') {
				//console.log("Adding file button: ",dd,":",size,":");
				addChildButton(item,size+" "+dd,"join_setArray('filterFile','"+dd+"');join_show();","Copy <file name> to filter");
				added=true;
			    };
			};
		    };
		};
		if (! added) {addChildText(item,"No data available...");}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Join dir filter request failed (system error)");}
	);
};

function join_showFilterFile(item,target,arg) {
    var file=join_getConfigFile();
    var password=document.getElementById("joinConfigFilePsw").value;
    var filterDir = join_config[file]["filterDir"];
    var filterDirMin = join_config[file]["filterDirMin"];
    var filterDirMax = join_config[file]["filterDirMax"];
    var filterFile = join_config[file]["filterFile"];
    var indexTarget = join_config[file]["indexTarget"];
    var indexVariable = join_config[file]["indexVariable"];
    documentLog.innerHTML="Sent join-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"join",
				  file:file,
				  password:password,
				  filterDir:filterDir,
				  filterDirMin:filterDirMin,
				  filterDirMax:filterDirMax,
				  filterFile:filterFile,
				  indexTarget:indexTarget,
				  indexVariable:indexVariable
				 })
	.success(	
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			item.classList.toggle("show");
			var msg=getErrorMessage(errors);
			alert("Unable to find files at "+filterDir+" (filter:'"+filterFile+"', Setup file:"+file+")\n"+msg);
		    } else {
			dataToArray(data,status,documentLog);
			setInnerHTML('joinPatternHits',join_config[file]["hits"]);
			removeChildren(item);
			var added=false;
			var len=join_config[file]["files"].length;
			for (var ii=0; ii<len;ii++) {
			    var sfile=join_config[file]["files"][ii][0];
			    var bfile=join_basename(sfile);
			    var sage=parseFloat(join_config[file]["files"][ii][1]).toFixed(2);
			    var ssize=join_config[file]["files"][ii][2];
    			    addChildButton(item,ssize+" "+bfile+" ("+sage+"d)","join_fileFind('"+sfile+"');join_show();","Scan <table> file.");
			    added=true;
			}
			if (! added) {addChildText(item,"No data available...");}
		    };
		    documentLog.innerHTML="";
		}
	    })
	.error(
	    function (error) { alert("Join file filter request failed (system error)");}
	);
};
function join_basename(string) {
    var myRe = /^.*\/([^\/]*)$/g;
    var myArray = myRe.exec(string);
    console.log("Basename:",string,'->',myArray[1]);
    return myArray[1];
};
function join_showSet(item,target,arg) {
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
    var file=join_getConfigFile();
    //console.log("Looking for file:",file);
    var cat=join_config[file]["cat"];
    for (var line in join_cats[cat]["lines"]) {
	//console.log("Adding config button: ",line);
	addChildButton(item,line+" ("+join_cats[cat]["lines"][line]+")","showValue('joinSet','"+line+"');showValue('joinType',join_cats['"+cat+"'][\"lines\"]['"+line+"']);","Data set identification");
	added=true;
    }
    if (! added) {addChildText(item,"No data available...");}
    //documentLog.innerHTML="";
    //}).error(function (error) { alert("Join set request failed (system error)");});}
};

function join_showColoc(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent join-load request.";
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
			addChildButton(item,"<up>","showValue('joinColoc','"+dd+"');","Change to parent <directory>");
			added=true;
		    } else {
			//console.log("Adding clear: ",dd);
			addChildButton(item,"<up>","showValue('joinColoc','');","Change to root <directory>");
			added=true;
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
				//console.log("Adding dir button: ",dd,ii);
				// colocation file 'dd' must be 'loaded' if it is selected....!!!
				addChildButton(item,dd,"showValue('joinColoc','"+dd+"');join_loadColoc('"+dd+"');","Change <directory>");
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
	    function (error) { alert("Join coloc request failed (system error)");}
	);
};

function join_showExpression(item,target,arg) {
    var cnt = target.substring(14);
    removeChildren(item);
    var added=false;
    var cfile=join_getColocConfigFile();
    var mfile=join_getModelConfigFile();
    var ofile=join_getObsConfigFile();
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

function join_showAttribute(item,target,arg) {
    var attr = target.substring(13);
    var file=join_getConfigFile();
    var cat=join_config[file]["cat"];
    if (join_cats[cat] === undefined) {join_setCat(cat);}
    var val=join_cats[cat]["attributes"][attr];
    var radio=val instanceof Array; // should we have radio button?
    var dup=(attr.substr(0,1) === "_");
    removeChildren(item);
    var added=false;
    if (radio) {
	for (var vv=0; vv < val.length;vv++) {
	    //console.log("Attribute '",attr,"' value  ",vv,val[vv]);
	    if (dup) {
		addChildButton(item,val[vv],"join_setAttribute('"+attr+"','"+val[vv]+"');join_setCat('"+cat+"');join_show();","Use <attribute duplicator>");
		added=true;
	    } else {
		addChildButton(item,val[vv],"join_setAttribute('"+attr+"','"+val[vv]+"');","Use <attribute value>");
		added=true;
	    };
	};
    }
    if (! added) {addChildText(item,"No data available...");}
};

function join_showDebugExpression(item,target,arg) {
    removeChildren(item);
    var added=false;
    addLogicalButtons(item,target);
    addFunctionButtons(item,target);
    added=true;
    if (! added) {addChildText(item,"No data available...");}
};
