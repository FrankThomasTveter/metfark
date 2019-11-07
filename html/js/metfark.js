    documentLog = document.getElementById("log");
fark_last = {model:"default.cfg",obs:"default.cfg",coloc:"default.cfg",join:"default.cfg",plot:"default.cfg",rerun:"default.cfg"};
dropdownEd = {};

// directory structure

metfark_config = { "model" : {},
		   "obs"   : {},
		   "coloc" : {},
		   "table"  : {},
		   "join"  : {},
		   "plot"  : {},
		   "rerun"  : {},
		   "clean"  : {}
		 };

var clss = ["model","obs","coloc","table","join","plot","rerun","clean"];


// initialisation function
function load(){
    $.ajaxSetup({timeout:0}); // never timeout a request (and re-send it)...
    var types=["model","obs","coloc","table","join","plot","rerun","clean","exec"];
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
    var types=["model","obs","coloc","table","join","plot","rerun","clean","exec"];
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
    } else if (type === "table") {
	table_setConfigFile(file);
    } else if (type === "join") {
	join_setConfigFile(file);
    } else if (type === "plot") {
	plot_setConfigFile(file);
    } else if (type === "rerun") {
	rerun_setConfigFile(file);
	rerun_updateData();
    } else if (type === "clean") {
	clean_updateData();
    } else if (type === "exec") {
	exec_updateData();
    };
};
function load_updateData(type){
    if (type === "model") {
	model_updateData();
	obs_updateData();
	coloc_updateData();
	table_updateData();
	join_updateData();
	plot_updateData();
	rerun_updateData();
	clean_updateData();
	exec_updateData();
    } else if (type === "obs") {
	model_updateData();
	obs_updateData();
	coloc_updateData();
	table_updateData();
	join_updateData();
	plot_updateData();
	rerun_updateData();
	clean_updateData();
	exec_updateData();
    } else if (type === "coloc") {
	coloc_updateData();
	model_setConfigFile(coloc_getModelConfigFile());
	model_updateData();
	obs_setConfigFile(coloc_getObsConfigFile());
	obs_updateData();
	table_updateData();
	join_updateData();
	plot_updateData();
	rerun_updateData();
	clean_updateData();
	exec_updateData();
    } else if (type === "table") {
	table_updateData();
	join_updateData();
	plot_updateData();
	coloc_updateData();
	model_updateData();
	obs_updateData();
	rerun_updateData();
	clean_updateData();
	exec_updateData();
    } else if (type === "join") {
	table_updateData();
	plot_updateData();
	join_updateData();
	coloc_updateData();
	model_updateData();
	obs_updateData();
	rerun_updateData();
	clean_updateData();
	exec_updateData();
    } else if (type === "plot") {
	plot_updateData();
	table_updateData();
	join_updateData();
	coloc_updateData();
	model_updateData();
	obs_updateData();
	rerun_updateData();
	clean_updateData();
	exec_updateData();
    } else if (type === "rerun") {
	exec_updateData();
	model_updateData();
	obs_updateData();
	coloc_updateData();
	table_updateData();
	join_updateData();
	plot_updateData();
	clean_updateData();
	rerun_updateData();
    } else if (type === "clean") {
	exec_updateData();
	model_updateData();
	obs_updateData();
	coloc_updateData();
	table_updateData();
	join_updateData();
	plot_updateData();
	clean_updateData();
	rerun_updateData();
    } else if (type === "exec") {
	exec_updateData();
	model_updateData();
	obs_updateData();
	coloc_updateData();
	table_updateData();
	join_updateData();
	plot_updateData();
	clean_updateData();
	rerun_updateData();
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
function removeTableChildren(item) {
    //console.log("removeTableChildren Entering",item);
    var tbody=item.children[0];
    var children=tbody.children;
    var len=children.length;
    var active=false;
    var head;
    var active=true;
    for (var ii=len-1;ii>=0;ii--){
	//console.log("RemoveTableChildren ",ii,children[ii],len,children.length);
	if (children[ii] !== undefined) {
	    tbody.removeChild(children[ii]);
	}
    };
    //console.log("removeTableChildren Done",item);
    return tbody;
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
function addChildButton(item,text,onclick,title) {
    var btn=document.createElement("BUTTON");
    var t=document.createTextNode(text);
    var br=document.createElement("BR");
    btn.appendChild(t);
    btn.setAttribute("onclick",onclick);
    btn.setAttribute("style","width:100%");
    if (title !== undefined) {
	btn.setAttribute("title",title);
	//console.log("Title='" + title + "'");
    };
    item.appendChild(btn);
    item.appendChild(br);
};
function addChildButtonShaded(item,text,onclick,title) {
    var btn=document.createElement("BUTTON");
    var t=document.createTextNode(text);
    var br=document.createElement("BR");
    btn.appendChild(t);
    btn.setAttribute("onclick",onclick);
    btn.setAttribute("style","width:100%");
    btn.setAttribute("class","shaded");
    if (title !== undefined) {
	btn.setAttribute("title",title);
	//console.log("Title='" + title + "'");
    };
    item.appendChild(btn);
    item.appendChild(br);
};
function addChildText(item,text) {
    var t=document.createTextNode(text);
    var br=document.createElement("BR");
    item.appendChild(t);
    item.appendChild(br);
};
function addTargetButton( item, target, trg, title) {
    addChildButton(item,trg,"addValue('"+target+"','"+trg+"');",title);
}
function addTargetButtonShaded( item, target, trg, title) {
    addChildButtonShaded(item,trg,"addValue('"+target+"','"+trg+"');",title);
}
function addFunctionButtons( item, target) {
    console.log("addFunctionButtons target='" + target + "'");
    addChildButton(item,"sec1970(<yy>,<mm>,<dd>...)","addValue('"+target+"','sec1970(,,,,,)');","Seconds since 1970 (function)");
    addChildButton(item,"julian(<yy>,<mm>,<dd>...)","addValue('"+target+"','julian(,,,,,)');","Julian days (function)");
    addChildButton(item,"midnight(<offset_days>)","addValue('"+target+"','midnight()');","Midnight in seconds since 1970 (function)");
    addChildButton(item,"now(<offset_days>)","addValue('"+target+"','now()');","Start of request in seconds since 1970 (function)");
    addChildButton(item,"name(value)","addValue('"+target+"','name()');","Name of constant (function)");
    addChildButton(item,"md5(oid)","addValue('"+target+"','md5()');","Md5 of the observation file (function)");
    addChildButton(item,"range(value,min,max)","addValue('"+target+"','range()');","Returns NA if value is outside range (function)");
    addChildButton(item,"precinct(lon_deg,lat_deg)","addValue('"+target+"','precinct(,)');","Find area-identification of location (function)");
    addChildButton(item,"vicinity(lon_deg,lat_deg,range_km)","addValue('"+target+"','vicinity(,,)');","Find area-identification of location\n and its vicinity (function)");
    addChildButton(item,"td2q(td_C,p_hpa)","addValue('"+target+"','td2q(td,p)');","Convert dew-point-temperature to specific-humidity (function)");
    addChildButton(item,"rh2td(rh_%,t_c[,ice_0|1])","addValue('"+target+"','rh2td(rh_%,t_C)');","Convert relative-humidity to dew-point-temperature (function)");
    addChildButton(item,"td2rh(td_c,t_c[,ice_0|1])","addValue('"+target+"','td2rh(td_C,t_C)');","Convert dew-point-temperature to relative-humidity (function)");
    addChildButton(item,"q2rh(q_g/kg,t_k[,p_hpa])","addValue('"+target+"','q2rh(q_gkg,t_K,p_hPa)');","Convert specific-humidity to relative-humidity (function)");
    addChildButton(item,"k2c(t_k)","addValue('"+target+"','k2c(t_K)');","Convert temperature in Kelvin to Celsius (function)");
    addChildButton(item,"c2k(t_C)","addValue('"+target+"','c2k(t_C)');","Convert temperature in Celsius to Kelvin (function)");
    addChildButton(item,"abs()","addValue('"+target+"','abs()');","Absolute  (function)");
    addChildButton(item,"exp()","addValue('"+target+"','exp()');","Exponential (function)");
    addChildButton(item,"log10()","addValue('"+target+"','log10()');","Logarithm base 10 (function)");
    addChildButton(item,"log()","addValue('"+target+"','log()');","Natural logarithm (function)");
    addChildButton(item,"sqrt()","addValue('"+target+"','sqrt()');","Square root (function)");
    addChildButton(item,"min(...)","addValue('"+target+"','min(,)');","Minimum (function)");
    addChildButton(item,"max(...)","addValue('"+target+"','max(,)');","Maximum (function)");
    addChildButton(item,"sin()","addValue('"+target+"','sin()');","Sine (function)");
    addChildButton(item,"cos()","addValue('"+target+"','cos()');","Cosine (function)");
    addChildButton(item,"tan()","addValue('"+target+"','tan()');","Tangens (function)");
    addChildButton(item,"asin()","addValue('"+target+"','asin()');","Arc-sine (function)");
    addChildButton(item,"acos()","addValue('"+target+"','acos()');","Arc-cosine (function)");
    addChildButton(item,"atan2(,)","addValue('"+target+"','atan2(,)');","Arc-tangens (function)");
    addChildButton(item,"Norway","addValue('"+target+"','Norway');","Area identification for Norway (constant)");
    addChildButton(item,"Sweden","addValue('"+target+"','Sweden');","Area identification for Sweden (constant)");
    addChildButton(item,"Denmark","addValue('"+target+"','Denmark');","Area identification for Denmark (constant)");
    addChildButton(item,"Finland","addValue('"+target+"','Finland');","Area identification for Finland (constant)");
};

function addMessageButtons( item, target) {
    addChildButton(item,"msgmax(var(:))","addValue('"+target+"','msgmax()');","Maximum value of target in BUFR message (function)");
    addChildButton(item,"msgmin(var(:))","addValue('"+target+"','msgmin()');","Minimum value of target in BUFR message (function)");
    addChildButton(item,"msgclosest(var(:),trg1,trg2...)","addValue('"+target+"','msgclosest(,,)');","Value of target in BUFR message closest to (trg1,trg2...) (function)");
}
function addLogicalButtons( item, target) {
    addChildButton(item,"ismember(var,trg1,trg2...)","addValue('"+target+"','ismember(,,)');","Check if var equals (trg1,trg2...) (function)");
    addChildButton(item,"isabove(var,max1,max2...)","addValue('"+target+"','isabove(,)');","Check if var is above (max1,max2...) (function)");
    addChildButton(item,"isbelow(var,min1,min2...)","addValue('"+target+"','isbelow(,)');","Check if var is below (min1,min2...) (function)");
    addChildButton(item,"isbetween(var,max1,min1,min2...)","addValue('"+target+"','isbetween(,,)');","Check if var is between (max,min1,min2..) (function)");
    addChildButton(item,"thinned(percent_thinned)","addValue('"+target+"','thinned(0.0)');","Randomly returns 1 or 0 (function)");
    addChildButton(item,"and(l1,l2,l3...)","addValue('"+target+"','and(,)');","Checks if (l1,l2,l3...) all are different from 0 (function)");
    addChildButton(item,"or(l1,l2,l3...)","addValue('"+target+"','or(,)');","Checks if at least one of (l1,l2,l3...) are different from 0 (function)");
    addChildButton(item,"not(l1,l2,l3...)","addValue('"+target+"','not(,)');","Checks if at least one of (l1,l2,l3...) equals 0 (function)");
}

function addWildcardButtons( item, target) {
    addChildButton(item,"YY (year)","addValue('"+target+"','YY');","year wildcard");
    addChildButton(item,"MM (month)","addValue('"+target+"','MM');","month wildcard");
    addChildButton(item,"DD (day)","addValue('"+target+"','DD');","day wildcard");
    addChildButton(item,"HH (hour)","addValue('"+target+"','HH');","hour wildcard");
    addChildButton(item,"MI (minute)","addValue('"+target+"','MI');","minute wildcard");
    addChildButton(item,"SS (second)","addValue('"+target+"','SS');","second wildcard");
};
function showDropdown(target) {
    var arg=document.getElementById(target).value;
    var item=document.getElementById(target + 'Dropdown');
    console.log("Target='"+target+"'  arg='" + arg + "'" );
    item.classList.toggle("show");
    removeChildren(item);
    addChildText(item,"Processing...");
    if (item.style.display === 'block' ) {return;}
    //console.log("Dropdown arg='"+arg+"'");
    if (target === 'modelConfigFile') {
	model_showConfigFile(item,target,arg);
    } else if (target === 'modelFilterDir') {
	model_showFilterDir(item,target,arg);
    } else if (target === 'modelFilterFile') {
	model_showFilterFile(item,target,arg);
    } else if (target === 'modelIndex') { 
	model_showIndex(item,target,arg);
    } else if (target === 'obsConfigFile') {
	obs_showConfigFile(item,target,arg);
    } else if (target === 'obsFilterDir') {
	obs_showFilterDir(item,target,arg);
    } else if (target === 'obsFilterFile') {
	obs_showFilterFile(item,target,arg);
    } else if (target === 'obsTablePath') {
	obs_showTablePath(item,target,arg);
    } else if (target === 'obsBufrType') {
	obs_showBufrType(item,target,arg);
    } else if (target === 'obsSubType') {
	obs_showSubType(item,target,arg);
    } else if (target === 'obsIndexPOS') {
	obs_showIndexPos(item,target,arg);
    } else if (target === 'obsIndexExp') {
	obs_showIndexExp(item,target,arg);
    } else if (target === 'colocConfigFile') {
	coloc_showConfigFile(item,target,arg);
    } else if (target === 'colocModelConfigFile') {
	coloc_showModelConfigFile(item,target,arg);
    } else if (target === 'colocModelTargetVariable') {
	coloc_showModelTargetVariable(item,target,arg);
    } else if (target === 'colocObsConfigFile') {
	coloc_showObsConfigFile(item,target,arg);
    } else if (target === 'colocXML') {
	coloc_showXml(item,target,arg);
    } else if (target === 'colocObsPOS') {
	coloc_showObsPos(item,target,arg);
    } else if (target === 'matchModelTargetName') {
	coloc_showMatchTarget(item,target,arg);
    } else if (target.substr(0,15) === 'matchExpression') {
	coloc_showMatchExpression(item,target,arg);
    } else if (target.substr(0,13) === 'colocDebugExp') {
	coloc_showDebugExpression(item,target,arg);
    } else if (target === 'colocObsFilter') {
	coloc_showObsFilter(item,target,arg);
    } else if (target === 'colocModelFilter') {
	coloc_showModelFilter(item,target,arg);
    } else if (target === 'tableConfigFile') {
	table_showConfigFile(item,target,arg);
	var args=getArgs(arg);
    } else if (target === 'tableCat') {
	table_showCat(item,target,arg);
    } else if (target === 'tableTable') {
	table_showTable(item,target,arg);
    } else if (target === 'tableGraphics') {
	table_showGraphics(item,target,arg);
    } else if (target === 'tableSet') {
	table_showSet(item,target,arg);
    } else if (target === 'tableColoc') {
	table_showColoc(item,target,arg);
    } else if (target.substr(0,15) === 'tableExpression') {
	table_showExpression(item,target,arg);
    } else if (target.substr(0,14) === 'tableAttribute') {
	table_showAttribute(item,target,arg);
    } else if (target.substr(0,13) === 'tableDebugExp') {
	table_showDebugExpression(item,target,arg);
    } else if (target === 'joinConfigFile') {
	join_showConfigFile(item,target,arg);
    } else if (target === 'joinCat') {
	join_showCat(item,target,arg);
    } else if (target === 'joinTable') {
	join_showTable(item,target,arg);
    } else if (target === 'joinGraphics') {
	join_showGraphics(item,target,arg);
    } else if (target === 'joinFilterDir') {
	join_showFilterDir(item,target,arg);
    } else if (target === 'joinFilterFile') {
	join_showFilterFile(item,target,arg);
    } else if (target === 'joinSet') {
	join_showSet(item,target,arg);
    } else if (target === 'joinColoc') {
	join_showColoc(item,target,arg);
    } else if (target === 'joinFilter') {
	join_showFilter(item,target,arg);
    } else if (target.substr(0,14) === 'joinExpression') {
	join_showExpression(item,target,arg);
    } else if (target.substr(0,13) === 'joinAttribute') {
	join_showAttribute(item,target,arg);
    } else if (target.substr(0,12) === 'joinDebugExp') {
	join_showDebugExpression(item,target,arg);
    } else if (target === 'plotConfigFile') {
	plot_showConfigFile(item,target,arg);
    } else if (target === 'plotCat') {
	plot_showCat(item,target,arg);
    } else if (target === 'plotTable') {
	plot_showTable(item,target,arg);
    } else if (target === 'plotGraphics') {
	plot_showGraphics(item,target,arg);
    } else if (target === 'rerunConfigFile' ) {
	rerun_showConfigFile(item,target,arg);
    } else if (target === 'rerunTimeOffset') {
	rerun_showTimeOffset(item,target,arg);
    } else if (target === 'rerunType') {
	rerun_showType(item,target,arg);
    } else if (target === 'rerunSetupFile') {
	rerun_showSetupFile(item,target,arg);
    } else if (target === 'cleanConfigFile') {
	clean_showConfigFile(item,target,arg);
    } else if (target === 'cleanFilterDir') {
	clean_showFilterDir(item,target,arg);
    } else if (target === 'cleanFilterFile') {
	clean_showFilterFile(item,target,arg);
    } else if (target === 'execType') {
	exec_showType(item,target,arg);
    } else if (target === 'execConfigFile') {
	exec_showConfigFile(item,target,arg);
    } else if (target === 'execCron') {
	exec_showCron(item,target,arg);
    } else {
	console.log("Unknown dropdown target:", target);
    }
    //document.getElementById(dropdown).classList.toggle("show");
}
function setValue(target,value) {
    if (document.getElementById(target) == null) {
	console.log("Undefined target:",target," Value:",value);
    } else {
	document.getElementById(target).checked=((value||"true")==="true");
    }
}
function showValue(target,value) {
    if (document.getElementById(target) == null) {
	console.log("Undefined target:",target," Value:",value);
    } else {
	document.getElementById(target).value=value;
    }
}
function getValue(target) {
    if (document.getElementById(target) == null) {
	console.log("Undefined target:",target);
    } else {
	return document.getElementById(target).value;
    }
}
function addValue(target,value) {
    console.log("Target:",target," Item:",value);
    document.getElementById(target).value=document.getElementById(target).value + value;
}
function setInnerHTML(target,value) {
    //console.log("Item:",value);
    var item=document.getElementById(target);
    if (item !== null) {
	item.innerHTML=value;
    } else {
	console.log("Trying to set invalid HTML:",target,value);
    }
}

function dataToArray(data,status,documentLog) {
    var ret=[];
    if (status == "success") {
	dataToCat(data);
	dataToModel(data);
	dataToObs(data);
	dataToColoc(data);
	dataToTable(data);
	dataToJoin(data);
	dataToPlot(data);
	dataToRerun(data);
	dataToClean(data);
	dataToExec(data);
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
	    model_config[path]={variables : {},dims:{},sizes : {}}
	}
	model_config[path]["filterDir"]=
	    set(model_config[path]["filterDir"],models[ii].getAttribute("filterDir"));
	model_config[path]["filterDirMin"]=
	    set(model_config[path]["filterDirMin"],models[ii].getAttribute("filterDirMin"));
	model_config[path]["filterDirMax"]=
	    set(model_config[path]["filterDirMax"],models[ii].getAttribute("filterDirMax"));
	//console.log("Filter dir:","'"+model_config[path]["filterDir"]+"'",
	//	    "'"+model_config[path]["filterDirMin"]+"'",
	//	    "'"+model_config[path]["filterDirMax"]+"'");
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
		var size=variables[jj].getAttribute("size")||"";
		model_config[path]["variables"][name]=dims;
		model_config[path]["sizes"][name]=size;
	    }
	} else if (model_config[path]["variables"] === undefined) {
	    model_config[path]["variables"]={};
	}
	var dims=models[ii].getElementsByTagName("dimension");
	if (dims) {
	    model_config[path]["dimensions"]={};
	    for (var jj = 0; jj < dims.length; jj++) {
		var name=dims[jj].getAttribute("name");
		var dimv=dims[jj].getAttribute("size")||"";
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
		if (sname !== undefined && sname !== null) {
		    var sage=files[jj].getAttribute("age");
		    var ssize=files[jj].getAttribute("size");
		   //console.log("Found stack file:",sname,' (',path,')');
		    model_config[path]["files"].push([sname,sage,ssize]);
		    model_config[path]["stack"]=sname;
		}
	    }
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
			var unit=seq[pp].getAttribute("unit");
			var val1=seq[pp].getAttribute("val1")||"NA";
			obs_config[path]["bufr"][bufrType][subType][pos]={descr:descr,info:pinfo,unit:unit,val1:val1};
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
		if (sname !== undefined && sname !== null) {
		    var sage=files[jj].getAttribute("age");
		    var ssize=files[jj].getAttribute("size");
		    obs_config[path]["files"].push([sname,sage,ssize]);
		    obs_config[path]["stack"]=sname;
		}
	    };
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
	   //console.log("Metfark adding:",target);
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
		//console.log("metfark: *** loaded default: ",target,value);
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
	//console.log("metfark: loaded coloc: ",name,coloc_config[path]);
    };
    //console.log("metfark: loaded COLOC: ",coloc_config);

}
function dataToTable(data) {
    // <table_config name="test"></table_config>
    var tables=data.getElementsByTagName("table_config");
    for (var ii = 0; ii < tables.length; ii++) {
	var file=tables[ii].getAttribute("file");
	var loc=tables[ii].getAttribute("location");
	if (loc === "") {
	    var path = file;
	} else {
	    var path = loc + file;
	}
	var table=tables[ii].getAttribute("table")||"";
	var graphics=tables[ii].getAttribute("graphics")||"";
	var overwrite=tables[ii].getAttribute("overwrite")||"";
	var cat=tables[ii].getAttribute("cat");
	table_config[path]={dataset:{}, attributes:{},table:table,graphics:graphics,overwrite:overwrite,cat:cat};
	colnames=[];
	var cols=tables[ii].getElementsByTagName("column");
	for (var jj = 0; jj < cols.length; jj++) {
	    var name=cols[jj].getAttribute("name")||"";
	    colnames.push(name);
	   //console.log("colname(",jj,")=",name);
	};
	var sets=tables[ii].getElementsByTagName("set");
	//console.log("metfark: loading table file=",path," cat=",cat," sets=",sets.length);
	for (var jj = 0; jj < sets.length; jj++) {
	    var name=sets[jj].getAttribute("name");
	    var coloc=sets[jj].getAttribute("coloc");
	    var legend=sets[jj].getAttribute("legend");
	    var columns=[];
	    var clmns=sets[jj].getElementsByTagName("col");
	    for (var kk = 0; kk < clmns.length; kk++) {
		var expr=clmns[kk].getAttribute("value")||"";
		columns.push(expr);
		//console.log("colname(",kk,"):",colnames[kk],"->",columns[kk]);
	    };
	    table_config[path]["dataset"][name]={coloc:coloc,
						colnames:colnames,
						columns:columns,
						legend:legend};
	}
	// set default attributes
	if (table_cats[cat] != undefined) {
	    for (var attr in table_cats[cat]["attributes"]) {
		if (table_config[path]["attributes"][attr] === undefined) {
		    var val=table_cats[cat]["attributes"][attr];
		    if (val instanceof Array) {
			table_config[path]["attributes"][attr]=val[0]; // first element
		    } else {
			table_config[path]["attributes"][attr]=val;
		    }
		}
	    }
	}
	var attrs=tables[ii].getElementsByTagName("attribute");
	for (var jj = 0; jj < attrs.length; jj++) {
	    var name=attrs[jj].getAttribute("name");
	    var value=attrs[jj].getAttribute("value");
	    table_config[path]["attributes"][name]=value;
	}
    }
}
function dataToJoin(data) {
    // <join_config name="test"></join_config>
    var joins=data.getElementsByTagName("join_config");
    for (var ii = 0; ii < joins.length; ii++) {
	var file=joins[ii].getAttribute("file");
	var loc=joins[ii].getAttribute("location");
	if (loc === "") {
	    var path = file;
	} else {
	    var path = loc + file;
	}
	var table=joins[ii].getAttribute("table")||"";
	var graphics=joins[ii].getAttribute("graphics")||"";
	var filter=joins[ii].getAttribute("filter")||"1";
	var overwrite=joins[ii].getAttribute("overwrite")||"true";
	var cat=joins[ii].getAttribute("cat");
	if (cat) {
	    join_config[path]={dataset:{}, attributes:{},table:table,graphics:graphics,filter:filter,overwrite:overwrite,cat:cat,min:{},max:{}};
	    join_config[path]["filterDir"]=
		set(join_config[path]["filterDir"],joins[ii].getAttribute("filterDir"));
	    join_config[path]["filterDirMin"]=
		set(join_config[path]["filterDirMin"],joins[ii].getAttribute("filterDirMin"));
	    join_config[path]["filterDirMax"]=
		set(join_config[path]["filterDirMax"],joins[ii].getAttribute("filterDirMax"));
	    join_config[path]["filterDirStat"]=
		set(join_config[path]["filterDirStat"],joins[ii].getAttribute("filterDirStat"));
	    join_config[path]["filterFile"]=
		set(join_config[path]["filterFile"],joins[ii].getAttribute("filterFile"));
	    join_config[path]["hits"]=
		set(join_config[path]["hits"],joins[ii].getAttribute("hits"));
	    var cols=joins[ii].getElementsByTagName("column");
	    for (var jj = 0; jj < cols.length; jj++) {
		var name=cols[jj].getAttribute("name")||"";
		var min=cols[jj].getAttribute("min")||"";
		var max=cols[jj].getAttribute("max")||"";
		join_config[path]["min"][name]=min;
		join_config[path]["max"][name]=max;
		//console.log("colname(",jj,")=",name);
	    };
	    if (join_cats[cat] != undefined) {
		for (var attr in join_cats[cat]["attributes"]) {
		    if (join_config[path]["attributes"][attr] === undefined) {
			var val=join_cats[cat]["attributes"][attr];
			if (val instanceof Array) {
			    join_config[path]["attributes"][attr]=val[0]; // first element
			} else {
			    join_config[path]["attributes"][attr]=val;
			}
		    }
		}
	    }
	}
	var attrs=joins[ii].getElementsByTagName("attribute");
	for (var jj = 0; jj < attrs.length; jj++) {
	    var name=attrs[jj].getAttribute("name");
	    var value=attrs[jj].getAttribute("value");
	    join_config[path]["attributes"][name]=value;
	}
	var files=joins[ii].getElementsByTagName("stack");
	if (files.length > 0) {
	    join_config[path]["files"]=[];
	    for (var jj = 0; jj < files.length; jj++) {
		var sname=files[jj].getAttribute("name");
		if (sname !== undefined && sname !== null) {
		    var sage=files[jj].getAttribute("age");
		    var ssize=files[jj].getAttribute("size");
		    join_config[path]["files"].push([sname,sage,ssize]);
		    join_config[path]["stack"]=sname;
		}
	    };
	} else if (join_config[path]["files"] === undefined) {
	    join_config[path]["files"]=[];
	}
    }
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
	var table=plots[ii].getAttribute("table")||"";
	var graphics=plots[ii].getAttribute("graphics")||"";
	var cat=plots[ii].getAttribute("cat");
	plot_config[path]={dataset:{}, attributes:{},table:table,graphics:graphics,cat:cat};
	colnames=[];
	var cols=plots[ii].getElementsByTagName("column");
	for (var jj = 0; jj < cols.length; jj++) {
	    var name=cols[jj].getAttribute("name")||"";
	    colnames.push(name);
	   //console.log("colname(",jj,")=",name);
	};
	var sets=plots[ii].getElementsByTagName("set");
	//console.log("metfark: loading plot file=",path," cat=",cat," sets=",sets.length);
	for (var jj = 0; jj < sets.length; jj++) {
	    var name=sets[jj].getAttribute("name");
	    var coloc=sets[jj].getAttribute("coloc");
	    var legend=sets[jj].getAttribute("legend");
	    var columns=[];
	    var clmns=sets[jj].getElementsByTagName("col");
	    for (var kk = 0; kk < clmns.length; kk++) {
		var expr=clmns[kk].getAttribute("value")||"";
		columns.push(expr);
		//console.log("colname(",kk,"):",colnames[kk],"->",columns[kk]);
	    };
	    plot_config[path]["dataset"][name]={coloc:coloc,
						colnames:colnames,
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
function dataToCatInit(name,type,cls) {
    if (name === undefined) {
	table_org_cats={};
	table_order=[];
	join_org_cats={};
	join_order=[];
    } else if (type === undefined) {
	table_org_cats[name]={"attributes":{},"lines":{},"order":[]};
	table_order.push(name);
	join_org_cats[name]={"attributes":{},"lines":{},"order":[]};
	join_order.push(name);
    } else if (cls === "hash") {
	table_org_cats[name][type]={};
	join_org_cats[name][type]={};
    } else if (cls === "array") {
	table_org_cats[name][type]=[];
	join_org_cats[name][type]=[];
    }
};
function dataToCatSetOrg(name,type,attr,value) {
    table_org_cats[name][type][attr]=value;
    join_org_cats[name][type][attr]=value;
}
function dataToCatPushOrg(name,type,attr) {
    table_org_cats[name][type].push(attr);
    join_org_cats[name][type].push(attr);
}

function dataToCat(data) {
    var cats=data.getElementsByTagName("cat_config");
    if (cats.length>0) {
	dataToCatInit();
    }
    for (var jj = 0; jj < cats.length; jj++) {
	var name=cats[jj].getAttribute("name");
	var attrs=cats[jj].getElementsByTagName("attr");
	dataToCatInit(name);
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
		dataToCatSetOrg(name,"attributes",attr,choices);
	    } else {
		dataToCatSetOrg(name,"attributes",attr,value);
	    }
	    dataToCatPushOrg(name,"order",attr);
	};
	var types=cats[jj].getElementsByTagName("line");
	if (types.length>0) {dataToCatInit(name,"lines","hash");}
	for (var kk = 0; kk < types.length; kk++) {
	    var id=types[kk].getAttribute("id");
	    var info=types[kk].getAttribute("name");
	    dataToCatSetOrg(name,"lines",id,info);
	    //console.log("metfark: loaded line: ",name,id,info);
	};
	var clmns=cats[jj].getElementsByTagName("column");
	if (clmns.length>0) {dataToCatInit(name,"colnames_","array");}
	for (var kk = 0; kk < clmns.length; kk++) {
	    var clmn=clmns[kk].getAttribute("name");
	    dataToCatPushOrg(name,"colnames_",clmn);
	    //console.log("metfark: loaded column: ",name,clmn);
	};
    };
};
function dataToClean(data) {
    // <clean_config name="test" filterFile="*"> <variable name="var1"> </clean_config>
    var cleans=data.getElementsByTagName("clean_config");
    for (var ii = 0; ii < cleans.length; ii++) {
	var name=cleans[ii].getAttribute("file");
	var loc=cleans[ii].getAttribute("location");
	if (loc === "") {
	    var path = name;
	} else {
	    var path = loc + name;
	}
	if (clean_config[path] === undefined) {
	    clean_config[path]={jobs : [],filterDir:"",filterFile:""}
	}
	clean_config[path]["jobs"]=[];
	var jobs=cleans[ii].getElementsByTagName("filter");
	for (var jj = 0; jj < jobs.length; jj++) {
	    var filterDir=jobs[jj].getAttribute("dir") || "";
	    var filterFile=jobs[jj].getAttribute("file") || "";
	    var filterAge=jobs[jj].getAttribute("age") || "";
	    if (filterDir === "" || filterFile === "" || filterAge === "") {
		console.log("Corrupt input:",filterDir,":",filterFile,":",filterAge,":",
			    jj,JSON.stringify(jobs));
	    } else {
		clean_config[path]["jobs"].push([filterDir,filterFile,filterAge]);
	    };
	};
	var files=cleans[ii].getElementsByTagName("stack");
	if (files.length > 0) {
	    clean_config[path]["files"]=[];
	    for (var jj = 0; jj < files.length; jj++) {
		var sname=files[jj].getAttribute("name");
		if (sname !== undefined && sname !== null) {
		    var sage=files[jj].getAttribute("age");
		    var ssize=files[jj].getAttribute("size");
		    clean_config[path]["files"].push([sname,sage,ssize]);
		    clean_config[path]["stack"]=sname;
		}
	    };
	} else if (clean_config[path]["files"] === undefined) {
	    clean_config[path]["files"]=[];
	}
    }
}
function dataToExec(data) {
    // <exec_config name="test" filterFile="*"> <variable name="var1"> </exec_config>
    var execs=data.getElementsByTagName("exec_config");
    for (var ii = 0; ii < execs.length; ii++) {
	exec_config["model"]={};
	var models=execs[ii].getElementsByTagName("model");
	for (var jj = 0; jj < models.length; jj++) {
	    var model=models[jj].getAttribute("file");
	    var last=models[jj].getAttribute("last") || "";
	    var info=models[jj].getAttribute("info") || "";
	    var exec=models[jj].getAttribute("exec") || exec_cron[0];
	    if (! isInArray(exec,exec_cron)) {exec=exec_cron[0];};
	    var status=models[jj].getAttribute("status") ||"";
	    exec_config["model"][model]={last:last,info:info,exec:exec,status:status};
	};
	exec_config["obs"]={};
	var obses=execs[ii].getElementsByTagName("obs");
	for (var jj = 0; jj < obses.length; jj++) {
	    var obs=obses[jj].getAttribute("file");
	    var last=obses[jj].getAttribute("last") ||"";
	    var info=obses[jj].getAttribute("info") ||"";
	    var exec=obses[jj].getAttribute("exec")  || exec_cron[0];
	    if (! isInArray(exec,exec_cron)) {exec=exec_cron[0];};
	    var status=obses[jj].getAttribute("status") ||"";
	    exec_config["obs"][obs]={last:last,info:info,exec:exec,status:status};
	};
	exec_config["coloc"]={};
	var coloces=execs[ii].getElementsByTagName("coloc");
	for (var jj = 0; jj < coloces.length; jj++) {
	    var coloc=coloces[jj].getAttribute("file");
	    var last=coloces[jj].getAttribute("last") ||"";
	    var info=coloces[jj].getAttribute("info") ||"";
	    var exec=coloces[jj].getAttribute("exec")  || exec_cron[0];
	    if (! isInArray(exec,exec_cron)) {exec=exec_cron[0];};
	    var status=coloces[jj].getAttribute("status") ||"";
	    exec_config["coloc"][coloc]={last:last,info:info,exec:exec,status:status};
	};
	exec_config["table"]={};
	var tablees=execs[ii].getElementsByTagName("table");
	for (var jj = 0; jj < tablees.length; jj++) {
	    var table=tablees[jj].getAttribute("file");
	    var last=tablees[jj].getAttribute("last") ||"";
	    var info=tablees[jj].getAttribute("info") ||"";
	    var exec=tablees[jj].getAttribute("exec")  || exec_cron[0];
	    if (! isInArray(exec,exec_cron)) {exec=exec_cron[0];};
	    var status=tablees[jj].getAttribute("status") ||"";
	    exec_config["table"][table]={last:last,info:info,exec:exec,status:status};
	};
	exec_config["plot"]={};
	var plotes=execs[ii].getElementsByTagName("plot");
	for (var jj = 0; jj < plotes.length; jj++) {
	    var plot=plotes[jj].getAttribute("file");
	    var last=plotes[jj].getAttribute("last") ||"";
	    var info=plotes[jj].getAttribute("info") ||"";
	    var exec=plotes[jj].getAttribute("exec")  || exec_cron[0];
	    if (! isInArray(exec,exec_cron)) {exec=exec_cron[0];};
	    var status=plotes[jj].getAttribute("status") ||"";
	    exec_config["plot"][plot]={last:last,info:info,exec:exec,status:status};
	};
	exec_config["join"]={};
	var plotes=execs[ii].getElementsByTagName("join");
	for (var jj = 0; jj < plotes.length; jj++) {
	    var plot=plotes[jj].getAttribute("file");
	    var last=plotes[jj].getAttribute("last") ||"";
	    var info=plotes[jj].getAttribute("info") ||"";
	    var exec=plotes[jj].getAttribute("exec")  || exec_cron[0];
	    if (! isInArray(exec,exec_cron)) {exec=exec_cron[0];};
	    var status=plotes[jj].getAttribute("status") ||"";
	    exec_config["join"][plot]={last:last,info:info,exec:exec,status:status};
	};
	exec_config["rerun"]={};
	var rerunes=execs[ii].getElementsByTagName("rerun");
	for (var jj = 0; jj < rerunes.length; jj++) {
	    var rerun=rerunes[jj].getAttribute("file");
	    var last=rerunes[jj].getAttribute("last") ||"";
	    var info=rerunes[jj].getAttribute("info") ||"";
	    var exec=rerunes[jj].getAttribute("exec")  || exec_cron[0];
	    if (! isInArray(exec,exec_cron)) {exec=exec_cron[0];};
	    var status=rerunes[jj].getAttribute("status") ||"";
	    exec_config["rerun"][rerun]={last:last,info:info,exec:exec,status:status};
	};
	exec_config["clean"]={};
	var cleanes=execs[ii].getElementsByTagName("clean");
	for (var jj = 0; jj < cleanes.length; jj++) {
	    var clean=cleanes[jj].getAttribute("file");
	    var last=cleanes[jj].getAttribute("last") ||"";
	    var info=cleanes[jj].getAttribute("info") ||"";
	    var exec=cleanes[jj].getAttribute("exec")  || exec_cron[0];
	    if (! isInArray(exec,exec_cron)) {exec=exec_cron[0];};
	    var status=cleanes[jj].getAttribute("status") ||"";
	    exec_config["clean"][clean]={last:last,info:info,exec:exec,status:status};
	};
    }
}
function dataToRerun(data) {
    // <rerun_config name="test" filterFile="*"> <variable name="var1"> </rerun_config>
    var reruns=data.getElementsByTagName("rerun_config");
    for (var ii = 0; ii < reruns.length; ii++) {
	var name=reruns[ii].getAttribute("file");
	var loc=reruns[ii].getAttribute("location");
	if (loc === "") {
	    var path = name;
	} else {
	    var path = loc + name;
	}
	if (rerun_config[path] === undefined) {
	    rerun_config[path]={}
	}
	var offset=reruns[ii].getAttribute("offset");
	rerun_config[path]["offset"]=offset;
	var variable=reruns[ii].getAttribute("variable");
	var start=reruns[ii].getAttribute("start");
	var stop=reruns[ii].getAttribute("stop");
	rerun_config[path]["variable"]={name:variable,start:start,stop:stop};
	rerun_config[path]["model"]={};
	var models=reruns[ii].getElementsByTagName("model");
	for (var jj = 0; jj < models.length; jj++) {
	    var model=models[jj].getAttribute("file");
	    var last=models[jj].getAttribute("last") || "";
	    var info=models[jj].getAttribute("info") || "";
	    var status=models[jj].getAttribute("status") ||"";
	    rerun_config[path]["model"][model]={last:last,info:info,status:status};
	};
	rerun_config[path]["obs"]={};
	var obses=reruns[ii].getElementsByTagName("obs");
	for (var jj = 0; jj < obses.length; jj++) {
	    var obs=obses[jj].getAttribute("file");
	    var last=obses[jj].getAttribute("last") ||"";
	    var info=obses[jj].getAttribute("info") ||"";
	    var status=obses[jj].getAttribute("status") ||"";
	    rerun_config[path]["obs"][obs]={last:last,info:info,status:status};
	};
	rerun_config[path]["coloc"]={};
	var coloces=reruns[ii].getElementsByTagName("coloc");
	for (var jj = 0; jj < coloces.length; jj++) {
	    var coloc=coloces[jj].getAttribute("file");
	    var last=coloces[jj].getAttribute("last") ||"";
	    var info=coloces[jj].getAttribute("info") ||"";
	    var status=coloces[jj].getAttribute("status") ||"";
	    rerun_config[path]["coloc"][coloc]={last:last,info:info,status:status};
	};
	rerun_config[path]["table"]={};
	var tables=reruns[ii].getElementsByTagName("table");
	for (var jj = 0; jj < tables.length; jj++) {
	    var table=tables[jj].getAttribute("file");
	    var last=tables[jj].getAttribute("last") ||"";
	    var info=tables[jj].getAttribute("info") ||"";
	    var status=tables[jj].getAttribute("status") ||"";
	    rerun_config[path]["table"][table]={last:last,info:info,status:status};
	};
	rerun_config[path]["join"]={};
	var plotes=reruns[ii].getElementsByTagName("join");
	for (var jj = 0; jj < plotes.length; jj++) {
	    var plot=plotes[jj].getAttribute("file");
	    var last=plotes[jj].getAttribute("last") ||"";
	    var info=plotes[jj].getAttribute("info") ||"";
	    var status=plotes[jj].getAttribute("status") ||"";
	    rerun_config[path]["join"][plot]={last:last,info:info,status:status};
	};
	rerun_config[path]["plot"]={};
	var plots=reruns[ii].getElementsByTagName("plot");
	for (var jj = 0; jj < plots.length; jj++) {
	    var plot=plots[jj].getAttribute("file");
	    var last=plots[jj].getAttribute("last") ||"";
	    var info=plots[jj].getAttribute("info") ||"";
	    var status=plots[jj].getAttribute("status") ||"";
	    rerun_config[path]["plot"][plot]={last:last,info:info,status:status};
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
    //console.log("dataToRoot Looking for:",loc,", found XML root nodes:",nodes.length);
    for (var ii = 0; ii < nodes.length; ii++) {
	// loop over directories and make sure they exist in metfark_config-structure...
	if (metfark_config[cls] == undefined) { 
	   //console.log("dataToRoot Creating internal cls:",cls);
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
	    //console.log("Found type:",type,":",root,":",loc,":",stat);
	    ret.push(item);
	};
    }
    return ret;
}
function dataToDirs(data,cls)	{
    var loc="Dir";
    var nodes=data.getElementsByTagName(loc);
   //console.log("dataToDirs Looking for:",loc,", found XML root nodes:",nodes.length);
    for (var ii = 0; ii < nodes.length; ii++) {
	// loop over directories and make sure they exist in metfark_config-structure...
	if (metfark_config[cls] == undefined) { 
	   //console.log("dataToFiles Creating internal class: '"+cls+"'");
	    metfark_config[cls]={};
	};
	var dir=nodes[ii];
	var root=dir.getAttribute("root");
	var location=dir.getAttribute("location");
	var child=dir.getAttribute("dir");
	var status=dir.getAttribute("status");
	if (metfark_config[cls][root] == undefined) { 
	   //console.log("dataToDirs Creating internal root: '"+cls+"'",root);
	    metfark_config[cls][root]={};
	}
	var pos=metfark_config[cls][root];
	var steps = location.split("/");
	for (var ss=0;ss<steps.length;ss++) {
	    if (steps[ss] == "" || steps[ss] == ".") {continue;};
	    if (pos[steps[ss]] == undefined) { 
		//console.log("dataToDirs Creating internal sub: '"+steps[ss]+"'",ss);
		pos[steps[ss]]={};
	    }
	    pos=pos[steps[ss]];
	};
	// check that child directory exists...
	if (pos[child] == undefined) {
	   //console.log("dataToDirs Adding:",child);
	    pos[child]={};
	};
    }
}
function dataToFiles(data,cls)	{
    var loc="File";
    var nodes=data.getElementsByTagName(loc);
   //console.log("dataToFiles Looking for:",loc,", found XML root nodes:",nodes.length);
    for (var ii = 0; ii < nodes.length; ii++) {
	// loop over directories and make sure they exist in metfark_config-structure...
	if (metfark_config[cls] == undefined) { 
	   //console.log("dataToFiles Creating internal class: '"+cls+'"');
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
		   //console.log("dataToFiles Creating internal sub: '"+steps[ss]+"'",ss);
		    pos[steps[ss]]={};
		}
		pos=pos[steps[ss]];
	    };
	   //console.log("dataToFiles Adding: '"+cls+"'",file);
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
   //console.log("Location:",loc);
    var steps = loc.split("/");
    var parent = null;
    var sub = null;
   //console.log("getSubDirs root:'"+root+"' loc: '"+loc+"' sub-dirs:",steps.length);
    for (var ss=0;ss<steps.length;ss++) {
	//console.log("getSubDirs sub: '"+ss+"' sub-dirs:",steps[ss]);
	if (steps[ss] == "" || steps[ss] == ".") {continue;};
	if (pos[steps[ss]] == undefined) {pos[steps[ss]]={};};
	if (steps[ss] !=null && steps[ss] != "") {
	    if (looksLikeFile(steps[ss])) {
		//console.log("getSubDirs file '"+steps[ss]+"'");
	    } else {
		//console.log("getSubDirs cd '"+steps[ss]+"'");
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
	   //console.log("getSubDirs child file '"+child+"'");
	} else {
	    if (pos[child] == undefined) {pos[child]={};};
	   //console.log("getSubDirs child cd '"+child+"'");
	    pos=pos[child];
	};
    };
   //console.log("getSubDirs parent '"+parent+"'");
    var keys=[];
    for (var k in pos) {
	if (pos.hasOwnProperty(k)) {
	    keys.push(k);
	}
    }
    keys.sort();
    var ret=[];
    ret.push(parent);
    for (var i=0; i< keys.length; i++) {
	key=keys[i];
	//console.log("getSubDirs Found dir:",key);
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

function removeSubstring(str,start,stop) {
    var istart=str.indexOf(start);
   //console.log("RemoveSubstring length:",istart.length);
    if (istart >= 0) {
	var out=str.substr(0,istart);
	//console.log("RemoveSubstring out:",out);
    } else {
	var out=str;
    }
    if (stop !== undefined) {
	var istop=str.indexOf(stop);
	if (istop >= 0) {
	    out=out + str.substr(istop);
	}
    }
   //console.log("RemoveSubstring:",str," '",start,"' ->",out);
    return out;
}

function debugExp(f,t) {
    var fitem=document.getElementById(f);
    var titem=document.getElementById(t);
    var expin=fitem.value;
    titem.innerHTML="";
    documentLog.innerHTML="Sent debug-exp request:"+expin;
    $.get("cgi-bin/fark_exp.pl",{exp:expin})
	.success(
	    function(data, status){
		if (status === "success" && data !== null) {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var dmsg=getErrorMessage(errors);
			var amsg="Unable to evaluate expression:"+expin+"\n"+dmsg;
			console.log("MSG:",amsg);
			alert(amsg);
		    } else {
			var results=data.getElementsByTagName("result");
			if (results.length > 0 ) {
			    var val=(results[0].getAttribute("value")||"");
			    //titem.innerHTML=val;
			    if (isNaN(val)) {
				titem.innerHTML=String(val);
			    } else {
				titem.innerHTML=Number(val).toString();
			    };
			};
		    };
		    documentLog.innerHTML="";
		};
	    })
	.error(
	    function (error) { alert("Debug request failed (system error)");}
	);
};

function getErrorMessage(errors) {
    var emsg=errors[0].getAttribute("message")||"";
    var dmsg=decodeURIComponent(emsg).replace('\\n',"\n").replace("\\n","\n");
    console.log("Error message='",dmsg,"'");
    return dmsg;
}

Array.prototype.extend = function (other_array) {
    /* you should include a test to check whether other_array really is an array */
    other_array.forEach(function(v) {this.push(v)}, this);    
}
