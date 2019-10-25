#__file: 'index.html' 0100644    **DO NOT DELETE**
<!DOCTYPE html>
<html>
  <head>
    <link rel="icon" href="favicon.ico" type="image/x-icon"/>
    <meta charset="UTF-8">
    <title>File ARchive Kit</title>
    <link  rel="stylesheet" href="css/metfark.css" />
    <!-- <meta name="viewport" content="width=device-width, initial-scale=1"> -->
    <link rel="stylesheet" href="bootstrap-3.3.6/css/bootstrap.min.css">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"></script>
    <script src="bootstrap-3.3.6/js/bootstrap.min.js"></script>
  </head>
  <body onLoad="load()">
    <div class="container-fluid">
      <h1 style="color:#09C;">File ARchive Kit</h1>
      <ul class="nav nav-pills">
	<li id="model_tab" class="active" title="Model index setup"><a data-toggle="pill" href="#model">Model</a></li>
	<li id="obs_tab" title="Observation index setup"><a data-toggle="pill" href="#obs">Observation</a></li>
	<li id="coloc_tab" title="Model and observation colocation setup"><a data-toggle="pill" href="#coloc">Colocation</a></li>
	<li id="table_tab" title="Table setup"><a data-toggle="pill" href="#table">Table</a></li>
	<li id="join_tab" title="Join setup"><a data-toggle="pill" href="#join">Join</a></li>
	<li id="plot_tab" title="Plot setup"><a data-toggle="pill" href="#plot">Plot</a></li>
	<li id="rerun_tab" title="Rerun scripts in a loop"><a data-toggle="pill" href="#rerun">Rerun</a></li>
	<li id="exec_tab" title="Execute jobs for making indexes, tables and plots"><a data-toggle="pill" href="#exec">Execute</a></li>
	<li id="clean_tab" title="Clean directory and files"><a data-toggle="pill" href="#clean">Clean</a></li>
      </ul>
      <div class="tab-content">
	<div id="model" class="tab-pane fade in active">
	  <table style="width:100%">
	    <tr style="background:#09C;">
	      <td>
		<div id="farkModel">
		  <button class="request" style="width:100%" onclick="model_updateData()"
			  title="Re-load <setup file> from server"><h1>Model index</h1></button>
		</div>
	      </td>
	    </tr>
	  </table>
	  <table>
	    <tr>
	      <td style="white-space: nowrap;"><em>Setup file:</em></td>
	      <td class="fill" width="100%">
		<input id="modelConfigFile" type="text" value="" style="width:100%" onblur="model_setConfigFile(this.value);model_show();"
		       title="Name of existing or new <setup file>"/>
                <!-- onblur="coloc_setConfigFile(this.value);coloc_show()" -->
		<div id="modelConfigFileDropdown" class="dropdown-content"></div>
	      </td>
	      <td align="center"><button onclick="model_setConfigFile(fark_last['model']);model_updateData();"
					 title="Fetch recently used <setup file>">←</button></td>
	      <td align="center"><button onclick="showDropdown('modelConfigFile')" class="dropbtn"
					 title="Show available <setup files> and actions">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td>Search top directory:</td>
	      <td class="fill" colspan="2">
		<input id="modelFilterDir" type="text" value="" style="width:100%" onblur="model_setFilterDir(this.value);"
		       title="Path to <top directory> used in search of <NetCDF model files>"/> 
		<div id="modelFilterDirDropdown" class="dropdown-content"></div>
	      </td>
	      <td align="center"><button onclick="showDropdown('modelFilterDir')" class="dropbtn"
					 title="Show available <top directories> and patterns">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td>Model file filter (regexp):</td>
	      <td colspan="3" class="fill" width="100%"><table width="100%" border="0">
		  <tr>
		    <td colspan="1" class="fill" width="100%">
		      <input id="modelFilterFile" type="text" value="" style="width:100%" onblur="model_setArray('filterFile',this.value)"
			     title="Regex filter used in search of <NetCDF model files>"/> 
		      <div id="modelFilterFileDropdown" class="dropdown-content"></div>
		    </td>
		    <td align="center"><button onclick="showDropdown('modelFilterFile')" class="dropbtn"
					       title="Show <NETCDF model files> that can be scanned &#13;for <model variables> and <model dimensions>">&#9776;</button></td>
		    <td> Max age:</td> <td><input id="modelFilterDirMax" type="text" value="" style="width:100px"
						  title="Maximum age of <NETCDF model files> in days, or start date YYYY-MM-DD."
						  onblur="model_setArray('filterDirMax',this.value)"/> </td>
		    <td> min:</td> <td><input id="modelFilterDirMin" type="text" value="" style="width:100px"
					      title="Minimum age of <NETCDF model files> in days, or stop date YYYY-MM-DD."
					      onblur="model_setArray('filterDirMin',this.value)"/> </td>
		    <td> Files:</td>
		    <td id="modelPatternHits">0</td>
		  </tr>
	      </table></td>
	    </tr>
	    <tr>
	      <td>Model index target:</td>
	      <td colspan="3">
		<table width="100%" border="0" id="modelIndexTable">
		  <tr>
		    <td class="fill" width="30%">
		      <input  style="color:darkorange;width:100%" id="modelIndexTarget" type="text" value="" 
			      onblur="model_setArray('indexTarget',this.value)"
			      title="Model index target name"/> 
		    </td>
		    <td align="right">Model index variable:</td>
		    <td class="fill" width="70%">
		      <input id="modelIndexVariable" type="text" value="" 
			     style="width:100%" onblur="model_setArray('indexVariable',this.value);model_checkVariable(this)"
			     title="Epoch time:&#13;The value range of this <model variable> is used to &#13;sort the <NETCDF model files> in an index.&#13;This index is used during colocation to find &#13;matching <BUFR observation files>."/>
		      <div id="modelIndexDropdown" class="dropdown-content"></div>
		    </td>
		    <td align="center">
		      <button onclick="showDropdown('modelIndex')" class="dropbtn"
			      title="Show available <model variables>">&#9776;</button>
		    </td>
		  </tr>
		</table>
	      </td>
	    </tr>
	  </table>
	  <table style="width:100%">
	    <tr>
	      <td align="right">
		File:<input id="modelConfigFileSave" type="text" value="" title="Current <setup file>." readonly/>
		Password:<input type="password" id="modelConfigFilePsw" value=""
				title="Password needed to save the <setup file> to server. 
				       A new <setup file> will get this password.">
		<button onclick="model_saveConfigFile()"
			title="Save the <setup file> to server.">Save</button></td>
	    </tr>
	  </table>
	</div>
	<div id="obs" class="tab-pane fade in">
	  <table style="width:100%">
	    <tr style="background:#09C;">
	      <td>
		<div id="farkObs">
		  <button class="request" style="width:100%" onclick="obs_updateData()"
			  title="Re-load <setup file> from server"><h1>Observation index</h1></button>
		</div>
	      </td>
	    </tr>
	  </table>
	  <table style="width:100%">
	    <tr>
	      <td style="white-space: nowrap;"><em>Setup file:</em></td>
	      <td class="fill" width="100%">
		<input id="obsConfigFile" type="text" value="" style="width:100%" onblur="obs_setConfigFile(this.value);obs_show();"
		       title="Name of existing or new <setup file>"/> 
		<div id="obsConfigFileDropdown" class="dropdown-content"></div>
	      </td>
	      <td align="center"><button onclick="obs_setConfigFile(fark_last['obs']);obs_updateData();"
					 title="Fetch recently used <setup file>">←</button></td>
	      <td align="center"><button onclick="showDropdown('obsConfigFile')" class="dropbtn"
					 title="Show available <setup files> and actions">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td>Search top directory:</td>
	      <td class="fill" colspan="2">
		<input id="obsFilterDir" type="text" value="" style="width:100%" onblur="obs_setFilterDir(this.value);"
		       title="Path to <top directory> used in search of <BUFR observation files>"/> 
		<div id="obsFilterDirDropdown" class="dropdown-content"></div>
	      </td>
	      <td align="center"><button onclick="showDropdown('obsFilterDir')" class="dropbtn"
					 title="Show available <top directories> and patterns">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td>Obs file filter (regexp):</td>
	      <td colspan="3" class="fill" width="100%">
		<table width="100%" border="0">
		  <tr>
		    <td colspan="1" class="fill" width="100%">
		      <input id="obsFilterFile" type="text" value="" style="width:100%" onblur="obs_setArray('filterFile',this.value)"
			     title="Regex filter used in search of <BUFR observation files>"/> 
		      <div id="obsFilterFileDropdown" class="dropdown-content"></div>
		    </td>
		    <td align="center"><button onclick="showDropdown('obsFilterFile')" class="dropbtn"
					       title="Show <BUFR observation files> that can be scanned for the BUFR sequence">&#9776;</button></td>
		    <td> Max age:</td> <td><input id="obsFilterDirMax" type="text" value="" style="width:100px"
						  title="Maximum age of <BUFR observation files> in days, or start date YYYY-MM-DD."
						  onblur="obs_setArray('filterDirMax',this.value)"/> </td>
		    <td> min:</td> <td><input id="obsFilterDirMin" type="text" value="" style="width:100px"
					      title="Minimum age of <BUFR observation files> in days, or stop date YYYY-MM-DD."
					      onblur="obs_setArray('filterDirMin',this.value)"/> </td>
		    <td>Files:</td>
		    <td id="obsPatternHits">0</td>
		  </tr>
		</table>
	      </td>
	    </tr>
	    <tr>
	      <td>BUFR table path:</td>
	      <td class="fill" colspan="2">
		<input id="obsTablePath" type="text" value="" style="width:100%" onblur="obs_setArray('tablePath',this.value)"
		       title="Path to <BUFR table directory>"/> 
		<div id="obsTablePathDropdown" class="dropdown-content"></div>
	      </td>
	      <td align="center"><button onclick="showDropdown('obsTablePath')" class="dropbtn"
					 title="Available <BUFR table directories>">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td>Data filter:</td>
	      <td colspan="3">
		<table width="100%" border="0" id="obsFilterTable">
		  <tr>
		    <td align="right">BUFR type:</td>
		    <td class="fill" width="30%">
		      <input id="obsBufrType" type="text" value="" style="width:100%" onblur="obs_setArray('bufrType',this.value)"
			     title="Requested BUFR type"/> 
		      <div id="obsBufrTypeDropdown" class="dropdown-content"></div>
		    </td>
		    <td style="min-width:25px;width:25px" align="center"><button onclick="showDropdown('obsBufrType')" class="dropbtn"
										 title="Show scanned <BUFR types>">&#9776;</button></td>
		    <td align="right">Sub type:</td>
		    <td class="fill" width="30%">
		      <input id="obsSubType" type="text" value="" style="width:100%" onblur="this.value=this.value.replace(/[^\d]/g,'');obs_setArray('subType',this.value)"
			     title="Requested BUFR sub-type"/> 
		      <div id="obsSubTypeDropdown" class="dropdown-content"></div>
		    </td>
		    <td style="min-width:25px;width:25px" align="center"><button onclick="showDropdown('obsSubType')" class="dropbtn"
										 title="Show scanned <BUFR sub-types>">&#9776;</button></td>
		    <td align="right">Info:</td>
		    <td class="fill" width="30%">
		      <input id="obsTypeInfo" type="text" value="" style="width:100%" onblur="obs_setArray('typeInfo',this.value)" title="Information"/> 
		    </td>
		  </tr>
		</table>
	      </td>
	    </tr>
	    <tr id="displayObsIndexTargets"><td valign="top">Observation targets:</td>
	      <td colspan="3">
		<table width="100%" border="1" id="obsIndexTargetTable">
		  <tr style="background-color:#00b9f2" id="labelsObsIndexTarget"> 
		    <th style="min-width:20%;width:20%">Observation target</th>
		    <th width="150px">Position</th>
		    <th style="min-width:25px;width:25px"></th>
		    <th style="min-width:100px;width:100px">Descriptor</th>
		    <th style="width:100%">Info</th>
		    <th style="min-width:25px;width:25px"></th>
		  </tr>
		  <tr id="newlineObsIndexTarget">
		    <td class="fill">
		      <input id="obsObsIndexTargetName" type="text" value="" style="width:100%"
			     title="Target name"/> 
		    </td>
		    <td class="fill">
		      <input id="obsIndexPOS" type="text" value="" style="width:100%"
			     title="Position in BUFR sequence"/> 
		      <div id="obsIndexPOSDropdown" class="dropdown-content"></div>
		    </td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button onclick="showDropdown('obsIndexPOS')" class="dropbtn"
			      title="Show scanned BUFR sequence">&#9776;</button></td>
		    <td class="fill"><input id="obsIndexDESCR" type="text" value="" style="width:100%"
					    title="BUFR descriptor"/></td>
		    <td class="fill"><input id="obsIndexInfo" type="text" value="" style="width:100%"
					    title="Information"/></td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button onclick="obs_newObsIndexTarget(this)"
			      title="Add observation target">&#43;</button>
		    </td>
		  </tr>
		</table>
	      </td>
	    </tr>
	    <tr>
	      <td>Observation index target: </td>
	      <td colspan="3">
		<table width="100%" border="0" id="obsIndexTable">
		  <tr>
		    <td class="fill" width="30%">
		      <input  style="color:blue;width:100%" id="obsIndexTarget" type="text" value="" onblur="obs_setArray('indexTarget',this.value)"
			      title="Observation index target name"/> 
		    </td>
		    <td align="right">Observation index expression:</td>
		    <td class="fill" width="70%">
		      <input id="obsIndexExp" type="text" value="" style="width:100%" onblur="obs_setArray('indexExp',this.value)"
			     title="Epoch time:&#13;The value range of this expression of <observation targets>&#13; is used to sort the <observation BUFR files> in an index.&#13;This index is used during colocation to find matching <NETCDF model files>."/> 
		      <div id="obsIndexExpDropdown" class="dropdown-content"></div>
		    </td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button onclick="showDropdown('obsIndexExp')" class="dropbtn"
			      title="Show available <observation targets> and functions">&#9776;</button></td>
		  </tr>
		</table>
	      </td>
	    </tr>
	  </table>
	  <table style="width:100%">
	    <tr>
	      <td align="right">
		File:<input id="obsConfigFileSave" type="text" value="" title="Current <setup file>." readonly/>
		Password:<input type="password" id="obsConfigFilePsw" value=""
				title="Password needed to save the <setup file> to server. 
				       A new <setup file> will get this password.">
		<button onclick="obs_saveConfigFile()"
			title="Save the <setup file> to server.">Save</button></td>
	    </tr>
	  </table>
	</div>
	<div id="coloc" class="tab-pane fade in">
	  <table style="width:100%">
	    <tr style="background:#09C;">
	      <td>
		<div id="farkColoc">
		  <button class="request" style="width:100%" onclick="coloc_updateData()" title="Re-load <setup file> from server"><h1>Colocation definition</h1></button>
		</div>
	      </td>
	    </tr>
	  </table>
	  <table style="width:100%">
	    <tr>
	      <td style="white-space: nowrap;"><em>Definition file:</em></td>
	      <td class="fill" width="100%">
		<input id="colocConfigFile" type="text" value="" style="width:100%" onblur="coloc_setConfigFile(this.value);coloc_show();"
		       title="Name of existing or new <setup file>"/> 
		<div id="colocConfigFileDropdown" class="dropdown-content"></div>
	      </td>
	      <td align="center"><button onclick="coloc_setConfigFile(fark_last['coloc']);coloc_updateData();"
					 title="Fetch recently used <setup file>">←</button></td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('colocConfigFile')" class="dropbtn"
			title="Show available <setup files> and actions">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td>Model index file:</td>
	      <td class="fill" colspan="2" width="100%">
		<input id="colocModelConfigFile" type="text" value="" style="width:100%" disabled /> 
		<div id="colocModelConfigFileDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('colocModelConfigFile')" class="dropbtn"
			title="Show available model <setup files>">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td>Observation index file:</td>
	      <td width="100%" colspan="2" class="fill">
		<input id="colocObsConfigFile" type="text" value="" style="width:100%" disabled/> 
		<div id="colocObsConfigFileDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('colocObsConfigFile')" class="dropbtn"
			title="Show available model <setup files>">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td style="white-space: nowrap;">Output XML file:</td>
	      <td class="fill" colspan="2" width="100%">
		<input id="colocXML" type="text" value="/elysium/data/default/YYYYMMDDHH.xml" onblur="coloc_setArray('xml',this.value);"
		       title="Output <XML file> path pattern"/> 
		<div id="colocXMLDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('colocXML')" class="dropbtn"
			title="Show directories and wildcards">&#9776;</button></td>
	    </tr>
	    <tr id="displayModelTargets" style="display:none">
	      <td valign="top">Model targets:</td>
	      <td colspan="3">
		<table width="100%" border="1" id="modelTargetTable">
		  <tr style="background-color:#00b9f2" id="labelsModelTarget"> 
		    <th style="min-width:200px;width:200px">Model target</th>
		    <th style="min-width:70%;width:70%">Model variable/(dimension)</th>
		    <th style="min-width:25px;width:25px"></th>
		    <th style="min-width:100px;width:100px">Minimum</th>
		    <th style="min-width:100px;width:100px">Maximum</th>
		    <th style="min-width:25px;width:25px"></th>
		  </tr>
		  <tr id="newlineModelTarget">
		    <td class="fill">
		      <input id="colocModelTargetName" type="text" value="" style="display:block"
			     title="Model target name"/> </td>
		    <td class="fill">
		      <input id="colocModelTargetVariable" type="text" value=""
			     title="<model dimension> or <model variable>"/>
		      <div id="colocModelTargetVariableDropdown" class="dropdown-content"></div>
		    </td>
		    <td align="center">
		      <button onclick="showDropdown('colocModelTargetVariable')" class="dropbtn"
			      title="Show <model dimensions> and <model variables>"> &#9776;</button></td>
		    <td class="fill">
		      <input id="colocModelTargetMin" type="text" value="" style="width:100%" onblur="this.value=this.value.replace(/[^\d\.]/g,'')" title="Minimum <BUFR sequence item> value. Observations with values below this threshold are rejected. &#13; (Duplicator: start value)"/>
		    </td>
		    <td class="fill">
		      <input id="colocModelTargetMax" type="text" value="" style="width:100%" onblur="this.value=this.value.replace(/[^\d\.]/g,'')" title="Maximum <BUFR sequence item> value. Observations with values above this threshold are rejected. &#13; (Duplicator: stop value)"/>
		    </td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button onclick="coloc_newModelTarget(this)" title="Add model target">&#43;</button></td>
		  </tr>
		</table>
	      </td>
	    </tr>
	    <tr id="displayModelDefault" style="display:none">
	      <td valign="top">Default:</td>
	      <td colspan="3">
		<table width="100%" border="1" id="modelDefaultTable">
		  <tr style="background-color:#00b9f2" id="labelsModelDefault"> 
		    <th>Latitude</th>
		    <th>Longitude</th>
		    <th>Information</th>
		    <td style="min-width:25px;width:25px"></td>
		  </tr>
		  <tr id="newlineModelDefault">
		    <td class="fill" id="_latitude">
		      <input type="text" value="" style="width:100%" onblur="this.value=this.value.replace(/[^\d\.]/g,'')"
			     title="Latitude"/>
		    </td>
		    <td class="fill" id="_longitude">
		      <input type="text" value="" style="width:100%" onblur="this.value=this.value.replace(/[^\d\.]/g,'')"
			     title="Longitude"/>
		    </td>
		    <td class="fill" id="information">
		      <input type="text" value="" style="width:100%" title="Information"/>
		    </td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button onclick="coloc_newModelDefault(this)" title="Add model default">&#43;</button></td>
		  </tr>
		</table>
	      </td>
	    </tr>
	    <tr id="displayObsTargets" style="display:none">
	      <td valign="top">Observation targets:</td>
	      <td colspan="3">
		<table width="100%" border="1" id="obsTargetTable">
		  <tr style="background-color:#00b9f2" id="labelsObsTarget"> 
		    <th style="min-width:200px;width:200px">Observation target</th>
		    <th style="min-width:10%;width:10%">Position</th>
		    <th style="min-width:25px;width:25px"></th>
		    <th style="min-width:10%;width:10%">Descriptor</th>
		    <th style="min-width:50%;width:50%">Info</th>
		    <th style="min-width:100px;width:100px">Minimum</th>
		    <th style="min-width:100px;width:100px">Maximum</th>
		    <th style="min-width:25px;width:25px"></th>
		  </tr>
		  <tr id="newlineObsTarget">
		    <td class="fill">
		      <input id="colocObsTargetName" type="text" value="" title="Observation target name"/> 
		    </td>
		    <td class="fill">
		      <input id="colocObsPOS" type="text" value="" title="BUFR sequence position:&#13; * Blank: searches for the next occurence of the descriptor.&#13; * An integer code (or expression): indicates a specific position.&#13; * A single variable: stores the position of the next occurence of the descriptor.&#13;Duplicator:&#13; * Blank&#13;Internal variable:&#13; * name of variable"/> 
		      <div id="colocObsPOSDropdown" class="dropdown-content"></div>
		    </td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button onclick="showDropdown('colocObsPOS')"
			      class="dropbtn" title="Show scanned BUFR sequence, duplicators and internal variables" >&#9776;</button></td>
		    <td class="fill"><input id="colocObsDESCR" type="text" value=""
					    title="BUFR sequence descriptor:&#13; * integer code&#13;Duplicator:&#13; * Blank&#13;Internal variable:&#13; * Blank"/></td>
		    <td class="fill"><input id="colocObsInfo" type="text" value=""
					    title="Information"/></td>
		    <td class="fill"><input id="colocObsMin" type="text" value=""
					    title="Minimum target value:&#13; * Observations with values below this threshold are rejected&#13;Duplicator:&#13; * Duplicator start index&#13;Internal variable:&#13; * Blank"/></td>
		    <td class="fill"><input id="colocObsMax" type="text" value=""
					    title="Maximum target value:&#13; * Observations with values above this threshold are rejected&#13;Duplicator:&#13; * Duplicator stop index&#13;Internal variable:&#13; * Blank"/></td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button onclick="coloc_newObsTarget(this)" title="Add observation target">&#43;</button></td>
		  </tr>
		</table>
	    </td></tr>
	    <tr id="displayColocObsFilter">
	      <td valign="top">
		Observation filter:
	      </td>
	      <td colspan="3">
		<table>
		  <tr>
		    <td width="100%">
		      <textarea id="colocObsFilter" rows="2" style="position:relative;width:100%" onblur="coloc_setArrayPar('obsConfigFile','filter',this.value)" title="Observation filter using <obervation targets> only. This filter is applied to each BUFR message. A BUFR message may contain several observations (locations). The observation is rejected if the filter expression returns 0."></textarea>
		      <div id="colocObsFilterDropdown" class="dropdown-content"></div>
		    </td>
		    <td style="min-width:25px;width:25px" align="right">
		      <button onclick="showDropdown('colocObsFilter')" class="dropbtn"
			      title="Show <observation targets> and functions">&#9776;</button></td>
		  </tr>
		</table>
	    </tr>
	    <tr id="displayMatchRules" style="display:none"><td valign="top">Match rules:</td>
	      <td colspan="3">
		<table width="100%" border="1" id="targetMatchTable">
		  <tr id="labelsTargetMatch" style="background-color:#00b9f2"> 
		    <th style="min-width:225px;width:225px">Model target</th>
		    <th>Observation target expression</th>
		    <th style="min-width:25px;width:25px"></th>
		  </tr>
		  <tr id="newlineTargetMatch" style="display:none">
		    <td></td><td></td><td></td>
		  </tr> 
		</table>
	    </td></tr>
	    <tr id="displayColocModelFilter">
	      <td valign="top">
		Location filter:
	      </td>
	      <td colspan="3">
		<table>
		  <tr>
		    <td width="100%">
		      <textarea id="colocModelFilter" rows="2" style="position:relative;width:100%" onblur="coloc_setArray('filter')"
				title="Location filter using <observation targets> and <model targets>. This filter is applied to each observation (location). The observation is rejected if the filter expression returns 0."></textarea>
		      <div id="colocModelFilterDropdown" class="dropdown-content"></div>
		    </td>
		    <td style="min-width:25px;width:25px" align="right">
		      <button onclick="showDropdown('colocModelFilter')" class="dropbtn" title="Show <model targets>)" class="dropbtn"
			      title="Show available functions">&#9776;</button></td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button class="run" onclick="debugExp('colocDebugExp','colocDebugRes')" title="Send expression to server and display results">&#9654;</button></td>
		    <td width="20%" id="colocDebugRes">0</td>
		  </tr>
		</table>
	      </td>
	    </tr>
	  </table>
	  <table width="100%">
	    <tr>
	      <td align="right">
		File:<input id="colocConfigFileSave" type="text" value="" title="Current <setup file>." readonly/>
		Password:<input type="password" id="colocConfigFilePsw" value=""
				title="Password needed to save the <setup file> to server. 
				       A new <setup file> will get this password.">
		<button onclick="coloc_saveConfigFile('colocConfigFilePsw')"
			title="Save the <setup file> to server.">Save</button></td>
	    </tr>
	  </table>
	</div>
	<div id="table" class="tab-pane fade in">
	  <table style="width:100%">
	    <tr style="background:#09C;">
	      <td>
		<div id="farkTable">
		  <button class="request" style="width:100%" onclick="table_updateData()" title="Re-load <setup file> from server"><h1>Table setup</h1></button>
		</div>
	      </td>
	    </tr>
	  </table>
	  <table style="width:100%">
	    <tr>
	      <td style="white-space: nowrap;"><em>Setup file:</em></td>
	      <td class="fill" width="100%">
		<input id="tableConfigFile" type="text" value="" style="width:100%" onblur="table_setConfigFile(this.value);table_show();"
		       title="Name of existing or new <setup file>"/> 
		<div id="tableConfigFileDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="table_setConfigFile(fark_last['table']);table_updateData();"
			title="Fetch recently used <setup file>">←</button>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('tableConfigFile')" class="dropbtn"
			title="Show available <setup files> and actions">&#9776;</button>
	      </td>
	      <td>Category:</td>
	      <td class="fill" width="25px">
		<input id="tableCat" type="text" value="" style="min-width:100px;width:100%;background-color:#00b9f2"  onblur="" disabled/> 
		<div id="tableCatDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('tableCat')" class="dropbtn"
			title="Show available categories">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td style="white-space: nowrap;">Output table file:</td>
	      <td class="fill" width="100%" colspan="5">
		<input id="tableTable" type="text" value="/elysium/data/default/YYYYMMDDHH.txt" onblur="table_setArray('table',this.value);"
		       title="Output <table file> path pattern. The <table file> contains all data&#13;needed to generate the graphics output."/> 
		<div id="tableTableDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('tableTable')" class="dropbtn"
			title="Show directories and wildcards">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td style="white-space: nowrap;">Output plot directory:</td>
	      <td class="fill" width="100%" colspan="5">
		<input id="tableGraphics" type="text" value="/elysium/data/default/YYYYMMDDHH.ps" onblur="table_setArray('graphics',this.value);"
		       title="Output <graphics file> directory pattern.&#13;This path is used as a prefix to the graphics output files.&#13;This path can be overridden in the plot setup."/> 
		<div id="tableGraphicsDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('tableGraphics')" class="dropbtn"
			title="Show directories and wildcards">&#9776;</button></td>
	    </tr>
	    <tr id="displayTableAttributes" >
	      <td valign="top">Attributes:</td> <!-- style="display:none" -->
	      <td colspan="6">
		<table width="100%" border="1" id="tableAttributesTable">
		  <tr style="background-color:#00b9f2" id="labelsTableAttribute"> 
		    <th width="150px">Name</th>
		    <th>Value</th>
		    <th style="min-width:25px;width:25px"></th>
		  </tr>
		</table>
	      </td>
	    </tr>
	    <tr id="displayTableDataset">
	      <td valign="top">Dataset:</td> <!-- tr style="display:none"-->
	      <td colspan="6"><table border="2" id="tableDatasetTable"><tbody></tbody></table></td>
	    </tr>
	    <tr id="tableDebugExpression">
	      <td>Debug expression:</td>
	      <td colspan="6"> 
		<table width="100%" border="1" id="debugExpression"> 
		  <tr>
		    <td class="fill">
		      <input id="tableDebugExp" value="" title="Expression to execute. Only internal variables are available">
		      <div id="tableDebugExpDropdown" class="dropdown-content"></div>
		    </td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button onclick="showDropdown('tableDebugExp')" class="dropbtn"
			      title="Show available functions">&#9776;</button></td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button class="run" onclick="debugExp('tableDebugExp','tableDebugRes')" title="Send expression to server and display results">&#9654;</button></td>
		    <td width="20%" id="tableDebugRes">0</td>
		  </tr>
		</table>
	      </td>
	    </tr>
	  </table>
	  <table width="100%">
	    <tr>
	      <td align="right">
		File:<input id="tableConfigFileSave" type="text" value="" title="Current <setup file>." readonly/>
		Password:<input type="password" id="tableConfigFilePsw" value=""
				title="Password needed to save the <setup file> to server. 
				       A new <setup file> will get this password.">
		<button onclick="table_saveConfigFile('tableConfigFilePsw')"
			title="Save the <setup file> to server.">Save</button></td>
	    </tr>
	  </table>
	</div>
	<div id="join" class="tab-pane fade in">
	  <table style="width:100%">
	    <tr style="background:#09C;">
	      <td>
		<div id="farkJoin">
		  <button class="request" style="width:100%" onclick="join_updateData()" title="Re-load <setup file> from server"><h1>Join setup</h1></button>
		</div>
	      </td>
	    </tr>
	  </table>
	  <table style="width:100%">
	    <tr>
	      <td style="white-space: nowrap;"><em>Setup file:</em></td>
	      <td class="fill" width="100%">
		<input id="joinConfigFile" type="text" value="" style="width:100%" onblur="join_setConfigFile(this.value);join_show();"
		       title="Name of existing or new <setup file>"/> 
		<div id="joinConfigFileDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="join_setConfigFile(fark_last['join']);join_updateData();"
			title="Fetch recently used <setup file>">←</button>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('joinConfigFile')" class="dropbtn"
			title="Show available <setup files> and actions">&#9776;</button>
	      </td>
	      <td>Category:</td>
	      <td class="fill" width="25px">
		<input id="joinCat" type="text" value="" style="min-width:100px;width:100%;background-color:#00b9f2"  onblur="" disabled/> 
		<div id="joinCatDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('joinCat')" class="dropbtn"
			title="Show available categories">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td style="white-space: nowrap;">Output table file:</td>
	      <td class="fill" width="100%" colspan="5">
		<input id="joinTable" type="text" value="/elysium/data/default/YYYYMMDDHH.txt" onblur="join_setArray('table',this.value);"
		       title="Output <table file> path pattern. The <table file> contains all data&#13;needed to generate the graphics output."/> 
		<div id="joinTableDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('joinTable')" class="dropbtn"
			title="Show directories and wildcards">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td style="white-space: nowrap;">Output plot directory:</td>
	      <td class="fill" width="100%" colspan="5">
		<input id="joinGraphics" type="text" value="/elysium/data/default/YYYYMMDDHH.ps" onblur="join_setArray('graphics',this.value);"
		       title="Output <graphics file> directory pattern.&#13;This path is used as a prefix to the graphics output files.&#13;This path can be overridden in the plot setup."/> 
		<div id="joinGraphicsDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('joinGraphics')" class="dropbtn"
			title="Show directories and wildcards">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td>Search top directory:</td>
	      <td class="fill" colspan="5">
		<input id="joinFilterDir" type="text" value="" style="width:100%" onblur="join_setFilterDir(this.value);"
		       title="Path to <top directory> used in search of <table files>"/> 
		<div id="joinFilterDirDropdown" class="dropdown-content"></div>
	      </td>
	      <td align="center"><button onclick="showDropdown('joinFilterDir')" class="dropbtn"
					 title="Show available <top directories> and patterns">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td>Join file filter (regexp):</td>
	      <td colspan="6" class="fill" width="100%"><table width="100%" border="0">
		  <tr>
		    <td colspan="1" class="fill" width="100%">
		      <input id="joinFilterFile" type="text" value="" style="width:100%" onblur="join_setArray('filterFile',this.value)"
			     title="Regex filter used in search of <table files>"/> 
		      <div id="joinFilterFileDropdown" class="dropdown-content"></div>
		    </td>
		    <td align="center"><button onclick="showDropdown('joinFilterFile')" class="dropbtn"
					       title="Show <table files> that can be scanned &#13;for <join variables> and <join dimensions>">&#9776;</button></td>
		    <td> Max age:</td> <td><input id="joinFilterDirMax" type="text" value="" style="width:100px"
						  title="Maximum age of <table files> in days, or start date YYYY-MM-DD."
						  onblur="join_setArray('filterDirMax',this.value)"/> </td>
		    <td> min:</td> <td><input id="joinFilterDirMin" type="text" value="" style="width:100px"
					      title="Minimum age of <table files> in days, or stop date YYYY-MM-DD."
					      onblur="join_setArray('filterDirMin',this.value)"/> </td>
		    <td> Files:</td>
		    <td id="joinPatternHits">0</td>
		  </tr>
	      </table></td>
	    </tr>
	    <tr id="displayJoinAttributes" >
	      <td valign="top">Attributes:</td> <!-- style="display:none" -->
	      <td colspan="6">
		<table width="100%" border="1" id="joinAttributesTable">
		  <tr style="background-color:#00b9f2" id="labelsJoinAttribute"> 
		    <th width="150px">Name</th>
		    <th>Value</th>
		    <th style="min-width:25px;width:25px"></th>
		  </tr>
		</table>
	      </td>
	    </tr>
	    <tr id="displayJoinDataset">
	      <td valign="top">Dataset:</td> <!-- tr style="display:none"-->
	      <td colspan="6">
		<table border="2" id="joinDatasetTable" width="100%">
		  <tr style="background-color:#00b9f2" id="labelsJoinDataset"> 
		    <th>Column</th>
		    <th width="50%">Minimum</th>
		    <th width="50%">Maximum</th>
		  </tr>
		</table>
	      </td>
	    </tr>
	    <tr id="joinDebugExpression">
	      <td>Debug expression:</td>
	      <td colspan="6"> 
		<table width="100%" border="1" id="debugExpression"> 
		  <tr>
		    <td class="fill">
		      <input id="joinDebugExp" value="" title="Expression to execute. Only internal variables are available">
		      <div id="joinDebugExpDropdown" class="dropdown-content"></div>
		    </td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button onclick="showDropdown('joinDebugExp')" class="dropbtn"
			      title="Show available functions">&#9776;</button></td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button class="run" onclick="debugExp('joinDebugExp','joinDebugRes')" title="Send expression to server and display results">&#9654;</button></td>
		    <td width="20%" id="joinDebugRes">0</td>
		  </tr>
		</table>
	      </td>
	    </tr>
	  </table>
	  <table width="100%">
	    <tr>
	      <td align="right">
		File:<input id="joinConfigFileSave" type="text" value="" title="Current <setup file>." readonly/>
		Password:<input type="password" id="joinConfigFilePsw" value=""
				title="Password needed to save the <setup file> to server. 
				       A new <setup file> will get this password.">
		<button onclick="join_saveConfigFile('joinConfigFilePsw')"
			title="Save the <setup file> to server.">Save</button></td>
	    </tr>
	  </table>
	</div>
	<div id="rerun" class="tab-pane fade in">
	  <table style="width:100%">
	    <tr style="background:#09C;">
	      <td>
		<div id="farkRerun">
		  <button class="request" style="width:100%" onclick="rerun_updateData()" title="Re-load <setup file> from server"><h1>Rerun</h1></button>
		</div>
	      </td>
	    </tr>
	  </table>
	  <table style="width:100%">
	    <tr>
	      <td style="white-sgpace: nowrap;"><em>Setup file:</em></td>
	      <td class="fill" width="100%" colspan="6">
		<input id="rerunConfigFile" type="text" value="" style="width:100%" onblur="rerun_setConfigFile(this.value);rerun_show();"
		       title="Name of existing or new <setup file>"/>
                <!-- onblur="coloc_setConfigFile(this.value);coloc_show()" -->
		<div id="rerunConfigFileDropdown" class="dropdown-content"></div>
	      </td>
	      <td align="center"><button onclick="showDropdown('rerunConfigFile')" class="dropbtn"
					 title="Show available <setup files> and actions">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td title="Rerun variable."> Rerun id (rid):</td>
	      <td>Start:</td> 
	      <td><input id="rerunVariableStart" type="text" value="" style="width:100px"
			 title="Start value (Integer)."
			 onblur="rerun_setArray('start',this.value)"/> </td>
	      <td> Stop:</td> 
	      <td><input id="rerunVariableStop" type="text" value="" style="width:100px"
			 title="Stop value (Integer)."
			 onblur="rerun_setArray('stop',this.value)"/> </td>
	      <td>Time offset expression:</td>
	      <td class="fill" width="100%">
		<input id="rerunTimeOffset" type="text" value="" style="width:100%" onblur="rerun_setOffset(this.value);"
		       title="Time offset in days..."/> 
		<div id="rerunTimeOffsetDropdown" class="dropdown-content"></div>
	      </td>
	      <td align="center"><button onclick="showDropdown('rerunTimeOffset')" class="dropbtn"
					 title="Show available variables and functions.">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td valign="top"> Rerun jobs:</td> 
	      <td class="fill" width="100%" colspan="9">
		<table id="rerunTable"  width="100%" border="1">
		  <tr id="labelsRerun" style="background-color:#00b9f2"> 
		    <th style="min-width:50px;width:50px">Type</th>
		    <th style="min-width:25px;width:25px"></th>
		    <th>Setup file</th>
		    <th style="min-width:25px;width:25px"></th>
		    <th width="200px">Last</th>
		    <th width="200px">Info</th>
		    <th style="min-width:25px;width:25px"></th>
		  </tr>
		  <tr id="newlineRerun">
		    <td class="fill">
		      <input id="rerunType" type="text" value="" style="width:100%" disabled/>
		      <div id="rerunTypeDropdown" class="dropdown-content"></div>
		    </td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button onclick="showDropdown('rerunType')" class="dropbtn"
			      title="Show available types">&#9776;</button></td>
		    <td class="fill">
		      <input id="rerunSetupFile" type="text" value="" style="width:100%" disabled/> 
		      <div id="rerunSetupFileDropdown" class="dropdown-content"></div>
		    </td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button onclick="showDropdown('rerunSetupFile')" class="dropbtn"
			      title="Show available setup files">&#9776;</button></td>
		    <td id="rerunLastTime"></td>
		    <td id="rerunLastInfo"></td>
		    <td style="min-width:25px;width:25px" align="center">
		      <button onclick="rerun_newSetupFile(this)" title="Add job">&#43;</button></td>
		  </tr>
		</table>
	      </td>
	    </tr>
	  </table>
	  <table width="100%">
	    <tr>
	      <td align="right">
		File:<input id="rerunConfigFileSave" type="text" value="" title="Current <setup file>." readonly/>
		Password:<input type="password" id="rerunConfigFilePsw" value="" title="Password needed to save the setup to server.">
		<button onclick="rerun_saveConfigFile()" title="Save the setup to server.">Save</button></td>
	    </tr>
	  </table>
	</div>
	<div id="plot" class="tab-pane fade in">
	  <table style="width:100%">
	    <tr style="background:#09C;">
	      <td>
		<div id="farkPlot">
		  <button class="request" style="width:100%" onclick="plot_updateData()" title="Re-load <setup file> from server"><h1>Plot setup</h1></button>
		</div>
	      </td>
	    </tr>
	  </table>
	  <table style="width:100%">
	    <tr>
	      <td style="white-space: nowrap;"><em>Setup file:</em></td>
	      <td class="fill" width="100%">
		<input id="plotConfigFile" type="text" value="" style="width:100%" onblur="plot_setConfigFile(this.value);plot_show();"
		       title="Name of existing or new <setup file>"/> 
		<div id="plotConfigFileDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="plot_setConfigFile(fark_last['plot']);plot_updateData();"
			title="Fetch recently used <setup file>">←</button>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('plotConfigFile')" class="dropbtn"
			title="Show available <setup files> and actions">&#9776;</button>
	      </td>
	    </tr>
	    <tr>
	      <td style="white-space: nowrap;">Input table file:</td>
	      <td class="fill" width="100%" colspan="2">
		<input id="plotTable" type="text" value="/elysium/data/default/YYYYMMDDHH.txt" onblur="plot_setArray('table',this.value);"
		       title="Output <table file> path pattern. The <table file> contains &#13;R-readable colocation data used to generate the graphics output."/> 
		<div id="plotTableDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('plotTable')" class="dropbtn"
			title="Show directories and wildcards">&#9776;</button></td>
	    </tr>
	    <tr>
	      <td style="white-space: nowrap;">Output directory:</td>
	      <td class="fill" width="100%" colspan="2">
		<input id="plotGraphics" type="text" value="/elysium/data/default/YYYYMMDDHH.ps" onblur="plot_setArray('graphics',this.value);"
		       title="Override the output directory given in the Table/Join setup"/> 
		<div id="plotGraphicsDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('plotGraphics')" class="dropbtn"
			title="Show directories and wildcards">&#9776;</button></td>
	    </tr>
	  </table>
	  <table width="100%">
	    <tr>
	      <td align="right">
		File:<input id="plotConfigFileSave" type="text" value="" title="Current <setup file>." readonly/>
		Password:<input type="password" id="plotConfigFilePsw" value=""
				title="Password needed to save the <setup file> to server. 
				       A new <setup file> will get this password.">
		<button onclick="plot_saveConfigFile('plotConfigFilePsw')"
			title="Save the <setup file> to server.">Save</button></td>
	    </tr>
	  </table>
	</div>
	<div id="exec" class="tab-pane fade in">
	  <table style="width:100%">
	    <tr style="background:#09C;">
	      <td>
		<div id="farkExec">
		  <button class="request" style="width:100%" onclick="exec_updateData()" title="Re-load <setup file> from server"><h1>Execute jobs</h1></button>
		</div>
	      </td>
	    </tr>
	  </table>
	  <table id="execTable"  width="100%" border="1">
	    <tr id="labelsExec" style="background-color:#00b9f2"> 
	      <th style="min-width:50px;width:50px">Type</th>
	      <th style="min-width:25px;width:25px"></th>
	      <th>Setup file</th>
	      <th style="min-width:25px;width:25px"></th>
	      <th style="min-width:50px;width:50px" align="center">Schedule</th>
	      <th style="min-width:25px;width:25px"></th>
	      <th style="min-width:75px;width:75px" align="center">Manual</th>
	      <th width="200px">Last</th>
	      <th width="200px">Info</th>
	      <th style="min-width:25px;width:25px"></th>
	    </tr>
	    <tr id="newlineExec">
	      <td class="fill">
		<input id="execType" type="text" value="" style="width:100%" disabled/> 
		<div id="execTypeDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('execType')" class="dropbtn"
			title="Show available types">&#9776;</button></td>
	      <td class="fill">
		<input id="execConfigFile" type="text" value="" style="width:100%" disabled/> 
		<div id="execConfigFileDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('execConfigFile')" class="dropbtn"
			title="Show available setup files">&#9776;</button></td>
	      <td class="fill">
		<input id="execCron" type="text" value="" style="width:100%" disabled/> 
		<div id="execCronDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('execCron').value)" class="dropbtn"
			title="Show available intervals">&#9776;</button></td>
	      <td style="min-width:75px;width:75px" align="center">
	      <td id="execLastTime"></td>
	      <td id="execLastInfo"></td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="exec_newConfigFile(this)" title="Add execmatic job">&#43;</button></td>
	    </tr>
	  </table>
	  <table width="100%">
	    <tr>
	      <td align="right">
		Password:<input type="password" id="execConfigFilePsw" value="" title="Password needed to save the setup to server.">
		<button onclick="exec_saveConfigFile()" title="Save the setup to server.">Save</button></td>
	    </tr>
	  </table>
	</div>
	<div id="clean" class="tab-pane fade in">
	  <table style="width:100%">
	    <tr style="background:#09C;">
	      <td>
		<div id="farkClean">
		  <button class="request" style="width:100%" onclick="clean_updateData()" title="Re-load <setup file> from server"><h1>Cleaning jobs</h1></button>
		</div>
	      </td>
	    </tr>
	  </table>
	  <table id="cleanTable"  width="100%" border="1">
	    <tr id="labelsClean" style="background-color:#00b9f2"> 
	      <th>Root directory</th>
	      <th style="min-width:25px;width:25px"></th>
	      <th>File filter (regexp)</th>
	      <th style="min-width:25px;width:25px"></th>
	      <th style="min-width:50px;width:50px" align="center">Schedule</th>
	      <th style="min-width:25px;width:25px"></th>
	      <th style="min-width:75px;width:75px" align="center">Manual</th>
	    </tr>
	    <tr id="newlineClean">
	      <td class="fill" width="50%">
		<input id="cleanFilterDir" type="text" value="" style="width:100%" disabled/> 
		<div id="cleanFilterDirDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('cleanFilterDir')" class="dropbtn"
			title="Show available types">&#9776;</button></td>
	      <td class="fill" width="50%">
		<input id="cleanFilterFile" type="text" value="" style="width:100%" disabled/> 
		<div id="cleanFilterFileDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('cleanFilterFile')" class="dropbtn"
			title="Show available setup files">&#9776;</button></td>
	      <td class="fill">
		<input id="cleanCron" type="text" value="" style="width:100%" disabled/> 
		<div id="cleanCronDropdown" class="dropdown-content"></div>
	      </td>
	      <td style="min-width:25px;width:25px" align="center">
		<button onclick="showDropdown('cleanCron').value)" class="dropbtn"
			title="Show available intervals">&#9776;</button></td>
	    </tr>
	  </table>
	  <table width="100%">
	    <tr>
	      <td align="right">
		Password:<input type="password" id="cleanConfigFilePsw" value="" title="Password needed to save the setup to server.">
		<button onclick="clean_saveConfigFile()" title="Save the setup to server.">Save</button></td>
	    </tr>
	  </table>
	</div>
      </div>
      <table style="width:100%">
	<tr>
	  <td>
	    <div id="farkFooter">
	      <small>File ARchive Kit - alpha release</small> 
	    </div>
	  </td>
	</tr>
      </table>
      <div id="log"></div>
      <script src="js/model.js"></script>
      <script src="js/obs.js"></script>
      <script src="js/coloc.js"></script>
      <script src="js/table.js"></script>
      <script src="js/join.js"></script>
      <script src="js/plot.js"></script>
      <script src="js/exec.js"></script>
      <script src="js/clean.js"></script>
      <script src="js/rerun.js"></script>
      <script src="js/metfark.js"></script>
    </div>
  </body>
</html>
#__file: 'js/exec.js' 0100644    **DO NOT DELETE**

// data structure
exec_config = { model :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		obs   :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		coloc :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		table :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		join  :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		plot  :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		rerun :  {"default.cfg" : {last:"",info:"",exec:"",status:""}},
		password: "franktt"
	      };
exec_configEd=0;
exec_cron=["","daily","weekly","monthly","quarterly"];

// exec methods
function exec_checkPassword() {
    var password=document.getElementById("execConfigPsw").value;
    if (exec_config["password"] !== undefined) {
	if (exec_config["password"] !== password) {
	    alert("Invalid password used when attempting to save Exec configuration\n");
	    return false;
	}
    };
    return true;
}
function exec_updateData() {
    documentLog.innerHTML="Sent exec-load request.";
    var root="exec.cfg";
    $.get("cgi-bin/fark_load.pl",{type:"exec",root:root})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		exec_setTable();
	    })
	.error(
	    function (error) { alert("Exec request failed (system error)");}
	);
};
function exec_newConfigFile(item) {
    var type=item.parentNode.parentNode.children[0].children[0].value;
    var file=item.parentNode.parentNode.children[2].children[0].value;
    var exec=item.parentNode.parentNode.children[4].children[0].value;
    showValue('execType',"");
    showValue('execConfigFile',"");
    document.getElementById("execCron").value=exec_cron[0];
    if (file !== "" ) {
	fark_last[type]=file;
	if (type === "model") {
	    if (exec_config["model"][file] === undefined) {
		exec_config["model"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "obs") {
	    if (exec_config["obs"][file] === undefined) {
		exec_config["obs"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "coloc") {
	    if (exec_config["coloc"][file] === undefined) {
		exec_config["coloc"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "table") {
	    if (exec_config["table"][file] === undefined) {
		exec_config["table"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "join") {
	    if (exec_config["join"][file] === undefined) {
		exec_config["join"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "plot") {
	    if (exec_config["plot"][file] === undefined) {
		exec_config["plot"][file]={last:"",info:"",exec:exec};
	    };
	} else if (type === "rerun") {
	    if (exec_config["rerun"][file] === undefined) {
		exec_config["rerun"][file]={last:"",info:"",exec:exec};
	    };
	}
	exec_setTable();
	//console.log("Saving setup file.");
	exec_saveConfigFile();
    } else {
	alert("Invalid: Model config file ('"+file+"')");
    }
    //console.log("Adding ",type,file,exec);
};
function exec_testNow(target,type,file,row) {
    var root="";
    var password=document.getElementById("execConfigFilePsw").value;
    if (target === "") {root="exec.cfg";};
    if (file !== "") {
	fark_last[type]=file;
	row.children[7].innerHTML=""; // last
	row.children[8].innerHTML="# running"; // info
	documentLog.innerHTML="Sent exec-now request ("+file+").";
	$.ajaxSetup({timeout:0}); // never timeout a request (and re-send it)...
	$.get("cgi-bin/fark_exec.pl",{root:root,password:password,type:type,file:file,test:1})
	    .success(
		function(data, status){
		    if (status == "success") {
			var errors=data.getElementsByTagName("error");
			if (errors.length > 0 ) {
			    var msg=getErrorMessage(errors);
			    if (isRunning(msg)) {
				alert(msg+"\nMonitoring has timed out.");
			    } else {
				alert("Unable to process, "+type+" config file: "+file+"\n"+msg);
			    }
			};
			if (target === "") {
			    dataToArray(data,status,documentLog);
			    exec_setTable();
			} else {
			    target.children[5].innerHTML="manual test";
			}
			documentLog.innerHTML="";}
		})
	    .error(
		function (error) { alert("Test request failed (system error)");}
	    );
    };
};
function exec_runNow(target,type,file,row) {
    var root="";
    var password=document.getElementById("execConfigFilePsw").value;
    if (target === "") {root="exec.cfg";};
    if (file !== "") {
	fark_last[type]=file;
	row.children[7].innerHTML=""; // last
	row.children[8].innerHTML="# running"; // info
	documentLog.innerHTML="Sent exec-now request ("+file+").";
	console.log("Sending sever request....",type,file);
	$.ajaxSetup({timeout:0}); // never timeout a request (and re-send it)...
	$.get("cgi-bin/fark_exec.pl",{root:root,password:password,type:type,file:file})
	    .success(
		function(data, status){
		    console.log("Success...");
		    //console.log("Here...");
		    if (status == "success") {
			var errors=data.getElementsByTagName("error");
			if (errors.length > 0 ) {
			    var msg=getErrorMessage(errors);
			    if (isRunning(msg)) {
				alert(msg+"\nMonitoring has timed out.");
			    } else {
				alert("Unable to process, "+type+" config file: "+file+"\n"+msg);
			    }
			};
			if (target === "") {
			    dataToArray(data,status,documentLog);
			    exec_setTable();
			} else {
			    target.children[5].innerHTML="manual run";
			}
			documentLog.innerHTML="";}
		})
	    .error(
		function (error) { documentLog.innerHTML="Run request failed (system error)";
				   alert("Run request failed (system error)");
				 }
	    );
    };
};
function exec_stopNow(target,type,file,row) {
    var root="";
    var password=document.getElementById("execConfigFilePsw").value;
    if (target === "") {root="exec.cfg";};
    if (file !== "") {
	fark_last[type]=file;
	row.children[7].innerHTML=""; // last
	row.children[8].innerHTML="# running"; // info
	documentLog.innerHTML="Sent exec-stop request ("+file+").";
	$.get("cgi-bin/fark_exec.pl",{root:root,password:password,type:type,file:file,abort:1})
	    .success(
		function(data, status){
		    if (status == "success") {
			var errors=data.getElementsByTagName("error");
			if (errors.length > 0 ) {
			    var msg=getErrorMessage(errors);
			    alert("Unable to process, "+type+" config file: "+file+"\n"+msg);
			};
			if (target === "") {
			    dataToArray(data,status,documentLog);
			    exec_setTable();
			} else {
			    target.children[5].innerHTML="manual stop";
			}
			documentLog.innerHTML="";}
		})
	    .error(
		function (error) { alert("Stop request failed (system error)");}
	    );
    };
};
function exec_saveConfigFile() {
    var root="exec.cfg";
    var password=document.getElementById("execConfigFilePsw").value;
    var modelFiles="";
    var obsFiles="";
    var colocFiles="";
    var tableFiles="";
    var joinFiles="";
    var plotFiles="";
    var rerunFiles="";
    exec_setTable();
    for (var model in exec_config["model"]) {
	modelFiles=modelFiles + "|" + model + "~" + 
	    exec_config["model"][model]["last"] + "~" +
 	    exec_config["model"][model]["info"] + "~" +
 	    exec_config["model"][model]["exec"];
    };
    for (var obs in exec_config["obs"]) {
	obsFiles=obsFiles + "|" + obs + "~" + 
	    exec_config["obs"][obs]["last"] + "~" +
	    exec_config["obs"][obs]["info"] + "~" +
	    exec_config["obs"][obs]["exec"];
    }
    for (var coloc in exec_config["coloc"]) {
	colocFiles=colocFiles + "|" + coloc + "~" + 
	    exec_config["coloc"][coloc]["last"] + "~" +
	    exec_config["coloc"][coloc]["info"] + "~";
	//#+
	//	    #exec_config["coloc"][coloc]["exec"];
    }
    for (var table in exec_config["table"]) {
	tableFiles=tableFiles + "|" + table + "~" + 
	    exec_config["table"][table]["last"] + "~" +
	    exec_config["table"][table]["info"] + "~" +
	    exec_config["table"][table]["exec"];
    }
    for (var join in exec_config["join"]) {
	joinFiles=joinFiles + "|" + join + "~" + 
	    exec_config["join"][join]["last"] + "~" +
	    exec_config["join"][join]["info"] + "~" +
	    exec_config["join"][join]["exec"];
    }
    for (var plot in exec_config["plot"]) {
	plotFiles=plotFiles + "|" + plot + "~" + 
	    exec_config["plot"][plot]["last"] + "~" +
	    exec_config["plot"][plot]["info"] + "~" +
	    exec_config["plot"][plot]["exec"];
    }
    for (var rerun in exec_config["rerun"]) {
	rerunFiles=rerunFiles + "|" + rerun + "~" + 
	    exec_config["rerun"][rerun]["last"] + "~" +
	    exec_config["rerun"][rerun]["info"] + "~" +
	    exec_config["rerun"][rerun]["exec"];
    }
    documentLog.innerHTML="Sent exec-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"exec",root:root,password:password,
				  modelFiles:modelFiles,obsFiles:obsFiles,colocFiles:colocFiles,
				  tableFiles:tableFiles,joinFiles:joinFiles,plotFiles:plotFiles,
				  rerunFiles:rerunFiles})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to save exec config file: "+root+"\n"+msg);
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Exec save request failed (system error)");}
	);
    makeUrl("exec",root);
};
function exec_removeFile(item,type,file) {
    var newitem=document.getElementById("newlineExec");
    newitem.children[0].children[0].value=type;
    newitem.children[2].children[0].value=file;
    newitem.children[4].children[0].value=exec_cron[0];
    //if (! checkExecPassword()) {return;}
    //item.parentNode.removeChild(item);
    delete exec_config[type][file];
    exec_setTable();
};

//#F78181

// create exec table
function exec_setTable() {
    var item=document.getElementById('execTable');
    var tail=removeTableChildFromTo(item,"labelsExec","newlineExec");
    var models=[];
    var obss=[];
    var colocs=[];
    var tables=[];
    var joins=[];
    var plots=[];
    var reruns=[];
    for (var ii = 0; ii < exec_cron.length; ++ii) {
	var cron=exec_cron[ii];
	var modell=[];
	var obsl=[];
	var colocl=[];
	var tablel=[];
	var joinl=[];
	var plotl=[];
	var rerunl=[];
	for (var model in exec_config["model"]) {
	    if (exec_config["model"][model]["exec"] === cron) {
		//console.log("*** Found: ",cron, model);
		modell.push(model);
	    }
	}
	for (var obs in exec_config["obs"]) {
	    if (exec_config["obs"][obs]["exec"] == cron) {
		//console.log("*** Found: ",cron,obs);
		obsl.push(obs);
	    }
	}
	for (var coloc in exec_config["coloc"]) {
	    if (exec_config["coloc"][coloc]["exec"] == cron) {
		//console.log("*** Found: ",cron,coloc);
		colocl.push(coloc);
	    }
	}
	for (var table in exec_config["table"]) {
	    if (exec_config["table"][table]["exec"] == cron) {
		//console.log("*** Found: ",cron,table);
		tablel.push(table);
	    }
	}
	for (var join in exec_config["join"]) {
	    if (exec_config["join"][join]["exec"] == cron) {
		//console.log("*** Found: ",cron,join);
		joinl.push(join);
	    }
	}
	for (var plot in exec_config["plot"]) {
	    if (exec_config["plot"][plot]["exec"] == cron) {
		//console.log("*** Found: ",cron,plot);
		plotl.push(plot);
	    }
	}
	for (var rerun in exec_config["rerun"]) {
	    if (exec_config["rerun"][rerun]["exec"] == cron) {
		//console.log("*** Found: ",cron,rerun);
		rerunl.push(rerun);
	    }
	}
	// sort...
	modell.sort();
	obsl.sort();
	colocl.sort();
	tablel.sort();
	joinl.sort();
	plotl.sort();
	rerunl.sort();
	// add to global array...
	models.extend(modell);
	obss.extend(obsl);
	colocs.extend(colocl);
	tables.extend(tablel);
	joins.extend(joinl);
	plots.extend(plotl);
	reruns.extend(rerunl);
    }
    for (var ii = 0; ii < models.length; ++ii) {
	var model=models[ii];
	//console.log("Insert row: ",model);
	exec_insertRow(tail,"model",model,
		       exec_config["model"][model]["last"],
		       exec_config["model"][model]["info"],
		       exec_config["model"][model]["exec"],
		       exec_config["model"][model]["status"],"#0A0"); 
    }
    for (var ii = 0; ii < obss.length; ++ii) {
	var obs=obss[ii];
	//console.log("Insert row: ",obs);
	exec_insertRow(tail,"obs",obs,
		       exec_config["obs"][obs]["last"],
		       exec_config["obs"][obs]["info"],
		       exec_config["obs"][obs]["exec"],
		       exec_config["obs"][obs]["status"],"#AFA");
    }
    for (var ii = 0; ii < colocs.length; ++ii) {
	var coloc=colocs[ii];
	//console.log("Insert row: ",coloc);
	exec_insertRow(tail,"coloc",coloc,
		       exec_config["coloc"][coloc]["last"],
		       exec_config["coloc"][coloc]["info"],
		       exec_config["coloc"][coloc]["exec"],
		       exec_config["coloc"][coloc]["status"],"#FAA");
    }
    for (var ii = 0; ii < tables.length; ++ii) {
	var table=tables[ii];
	//console.log("Insert row: ",table);
	exec_insertRow(tail,"table",table,
		       exec_config["table"][table]["last"],
		       exec_config["table"][table]["info"],
		       exec_config["table"][table]["exec"],
		       exec_config["table"][table]["status"],"#FA0");
    }
    for (var ii = 0; ii < joins.length; ++ii) {
	var join=joins[ii];
	//console.log("Insert row: ",join);
	exec_insertRow(tail,"join",join,
		       exec_config["join"][join]["last"],
		       exec_config["join"][join]["info"],
		       exec_config["join"][join]["exec"],
		       exec_config["join"][join]["status"],"#AF0");
    }
    for (var ii = 0; ii < plots.length; ++ii) {
	var plot=plots[ii];
	//console.log("Insert row: ",plot);
	exec_insertRow(tail,"plot",plot,
		       exec_config["plot"][plot]["last"],
		       exec_config["plot"][plot]["info"],
		       exec_config["plot"][plot]["exec"],
		       exec_config["plot"][plot]["status"],"#0AF");
    }
    for (var ii = 0; ii < reruns.length; ++ii) {
	var rerun=reruns[ii];
	//console.log("Insert row: ",rerun);
	exec_insertRow(tail,"rerun",rerun,
		       exec_config["rerun"][rerun]["last"],
		       exec_config["rerun"][rerun]["info"],
		       exec_config["rerun"][rerun]["exec"],
		       exec_config["rerun"][rerun]["status"],"#F6F");
    }
};
// create exec table row
function exec_insertRow(item,type,file,last,info,exec,status,color) {
    var row = document.createElement("TR");
    row.setAttribute("bgcolor",color);
    var td;
    // make TYPE column
    td=document.createElement("TD");
    td.innerHTML=type;
    td.setAttribute("title","Schedule type.");
    row.appendChild(td);
    // make select-TYPE column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make FILE NAME column
    td=document.createElement("TD");
    td.innerHTML=file;
    if (type == "model") {
	td.setAttribute("title","Maintain <model file index>.");
    } else if (type == "obs") {
	td.setAttribute("title","Maintain <observation file index>.");
    } else if (type == "coloc") {
	td.setAttribute("title","Create <colocation xml> for debugging.");
    } else if (type == "table") {
	td.setAttribute("title","Create <table file>.");
    } else if (type == "join") {
	td.setAttribute("title","Join <table file> into new <table file>.");
    } else if (type == "plot") {
	td.setAttribute("title","Run plotting script using data in <table file>.");
    } else if (type == "rerun") {
	td.setAttribute("title","Rerun a sub-set of jobs.");
    } else {
    };
    row.appendChild(td);
    //console.log("Row file name=",file);
    // make select-FILE NAME column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make EXEC checkbox column
    td=document.createElement("TD");
    td.setAttribute("title","Repetition interval.");
    td.innerHTML=exec;
    row.appendChild(td);
    // make select-TYPE column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make manual column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:75px;width:75px");
    td.setAttribute("align","center");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","exec_testNow('','"+type+"','"+file+"',this.parentNode.parentNode)");
    btn.setAttribute("class","test");
    if (type == "coloc") {
	btn.innerHTML="&#9762"
    } else {
	btn.innerHTML="&#9655"
    }
    btn.setAttribute("title","Test now, stop processing as soon as some output has been produced.");
    //var t=document.createTextNode();
    //btn.appendChild(t);
    td.appendChild(btn);
    // make RUN NOW column
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","exec_runNow('','"+type+"','"+file+"',this.parentNode.parentNode)");
    btn.setAttribute("class","run");
    if (type == "coloc") {
	btn.innerHTML="&#9762"
    } else {
	btn.innerHTML="&#9654"
    }
    btn.setAttribute("title","Run now, process all data completely.");
    //var t=document.createTextNode();
    //btn.appendChild(t);
    td.appendChild(btn);
    // make STOP column
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","exec_stopNow('','"+type+"','"+file+"',this.parentNode.parentNode)");
    btn.setAttribute("class","stop");
    if (type == "coloc") {
	btn.innerHTML="&#9762"
    } else {
	btn.innerHTML="&#9632"
    }
    btn.setAttribute("title","Stop process.");
    //var t=document.createTextNode();
    //btn.appendChild(t);
    td.appendChild(btn);
    // make LAST column
    row.appendChild(td);
    td=document.createElement("TD");
    if (status !== "") {
	td.setAttribute("style","color:blue");
    }
    var a = document.createElement('a');
    var linkText = document.createTextNode(last);
    a.appendChild(linkText);
    a.title = "View last log-file and error-file";
    a.href = "cgi-bin/fark_log.pl?type="+type+"&file="+file;
    a.target ="_blank";
    td.appendChild(a);
    row.appendChild(td);
    // make INFO column
    td=document.createElement("TD");
    if (type =="model") {
	td.title = "Show status of the model-index-file.";
    } else if (type =="obs") {
	td.title = "Show status of the observation-index-file.";
    } else if (type =="table") {
	td.title = "Show status of the table-file.";
    } else if (type =="join") {
	td.title = "Show status of the joined table-file.";
    } else if (type =="plot") {
	td.title = "Show status of the plot files.";
    } else if (type =="rerun") {
	td.title = "Show status of the rerun files.";
    } else {
	td.title = "Show status.";
    };
    td.innerHTML=info;
    row.appendChild(td);
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px;");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","exec_removeFile(this.parentNode.parentNode,'"+type+"','"+file+"')");
    btn.setAttribute("style","width:100%");
    btn.setAttribute("title","Remove scheduled job");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make add row to table
    item.parentNode.insertBefore(row,item);
    return row;
}

function isRunning(msg) {
    return (msg.substring(0,18) === "Process is running");
}

function exec_showType(item,target,arg) {
    removeChildren(item);
    addChildButton(item,"model","showValue('"+target+"','model');","Maintain model index");
    addChildButton(item,"observation","showValue('"+target+"','obs');","Maintain observation index");
    addChildButton(item,"colocation","showValue('"+target+"','coloc');","Make colocation XML (debugging only)");
    addChildButton(item,"table","showValue('"+target+"','table');","Make table file");
    addChildButton(item,"join","showValue('"+target+"','join');","Join table files");
    addChildButton(item,"plot","showValue('"+target+"','plot');","Make plots");
    addChildButton(item,"rerun","showValue('"+target+"','rerun');","Rerun jobs");
};

function exec_showConfigFile(item,target,arg) {
    var type=document.getElementById("execType").value // "obs";
    var args=getArgs(arg);
    documentLog.innerHTML="Sent exec-load request.";
    $.get("cgi-bin/fark_load.pl",{type:type,arg:args})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+arg+"'\n"+msg);
		} else {
		    var ret=dataToArray(data,status,documentLog);
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			item.classList.toggle("show");
			var msg=getErrorMessage(errors);
			alert("Unable to list '"+arg+"', type '"+type+"' \n"+msg);
		    } else if (ret[0] !== undefined) {
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
			    addChildButton(item,"<up>","showValue('execConfigFile','"+dd+"');","Change to parent <directory>");
			    added=true;
			} else {
			    //console.log("Adding clear: ",dd);
			    addChildButton(item,"<up>","showValue('execConfigFile','');","Change to root <directory>");
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
				    if (looksLikeFile(dd)) {
					addChildButton(item,dd,"showValue('execConfigFile','"+dd+"');","Use <file>");
					added=true;
				    } else {
					addChildButton(item,dd,"showValue('execConfigFile','"+dd+"');","Change <directory>");
					added=true;
				    };
				}
			    }
			}
			if (! added) {addChildText(item,"No data available...");}
		    } else {
			console.log("Undefined root.");
		    }
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Exec request failed (system error)");}
	);
};

function exec_showCron(item,target,arg) {
    var args=getArgs(arg);
    removeChildren(item);
    var added=false;
    for (var ii=0;ii<exec_cron.length;ii++) {
	var cron=exec_cron[ii];
	if (cron === "") {
	    addChildButton(item,"<never>","showValue('execCron','"+cron+"');","Never run job");
	    added=true;
	} else {
	    addChildButton(item,cron,"showValue('execCron','"+cron+"');","Run job <interval>");
	    added=true;
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};
#__file: 'js/clean.js' 0100644    **DO NOT DELETE**

// data structure
clean_config = { weekly : [["/tmp",".*.table"]],
		 monthly : [],
		 quarterly : [],
		 yearly : [],
		 password: "franktt"
	       };
clean_configEd=0;
clean_cron=["","weekly","monthly","quarterly","yearly"];

// clean methods
function clean_checkPassword() {
    var password=document.getElementById("cleanConfigPsw").value;
    if (clean_config["password"] !== undefined) {
	if (clean_config["password"] !== password) {
	    alert("Invalid password used when attempting to save Clean configuration\n");
	    return false;
	}
    };
    return true;
}
function clean_updateData() {
    documentLog.innerHTML="Sent clean-load request.";
    var root="clean.cfg";
    $.get("cgi-bin/fark_load.pl",{type:"clean",root:root})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		clean_setTable();
	    })
	.error(
	    function (error) { alert("Clean request failed (system error)");}
	);
};
function clean_newConfigFile(item) {
    var type=item.parentNode.parentNode.children[0].children[0].value;
    var file=item.parentNode.parentNode.children[2].children[0].value;
    var clean=item.parentNode.parentNode.children[4].children[0].value;
    showValue('cleanType',"");
    showValue('cleanConfigFile',"");
    document.getElementById("cleanCron").value=clean_cron[0];
    if (file !== "" ) {
	fark_last[type]=file;
	if (type === "model") {
	    if (clean_config["model"][file] === undefined) {
		clean_config["model"][file]={last:"",info:"",clean:clean};
	    };
	} else if (type === "obs") {
	    if (clean_config["obs"][file] === undefined) {
		clean_config["obs"][file]={last:"",info:"",clean:clean};
	    };
	} else if (type === "coloc") {
	    if (clean_config["coloc"][file] === undefined) {
		clean_config["coloc"][file]={last:"",info:"",clean:clean};
	    };
	} else if (type === "table") {
	    if (clean_config["table"][file] === undefined) {
		clean_config["table"][file]={last:"",info:"",clean:clean};
	    };
	} else if (type === "join") {
	    if (clean_config["join"][file] === undefined) {
		clean_config["join"][file]={last:"",info:"",clean:clean};
	    };
	} else if (type === "plot") {
	    if (clean_config["plot"][file] === undefined) {
		clean_config["plot"][file]={last:"",info:"",clean:clean};
	    };
	} else if (type === "rerun") {
	    if (clean_config["rerun"][file] === undefined) {
		clean_config["rerun"][file]={last:"",info:"",clean:clean};
	    };
	}
	clean_setTable();
	//console.log("Saving setup file.");
	clean_saveConfigFile();
    } else {
	alert("Invalid: Model config file ('"+file+"')");
    }
    //console.log("Adding ",type,file,clean);
};
function clean_testNow(target,type,file,row) {
    var root="";
    var password=document.getElementById("cleanConfigFilePsw").value;
    if (target === "") {root="clean.cfg";};
    if (file !== "") {
	fark_last[type]=file;
	row.children[7].innerHTML=""; // last
	row.children[8].innerHTML="# running"; // info
	documentLog.innerHTML="Sent clean-now request ("+file+").";
	$.ajaxSetup({timeout:0}); // never timeout a request (and re-send it)...
	$.get("cgi-bin/fark_clean.pl",{root:root,password:password,type:type,file:file,test:1})
	    .success(
		function(data, status){
		    if (status == "success") {
			var errors=data.getElementsByTagName("error");
			if (errors.length > 0 ) {
			    var msg=getErrorMessage(errors);
			    if (isRunning(msg)) {
				alert(msg+"\nMonitoring has timed out.");
			    } else {
				alert("Unable to process, "+type+" config file: "+file+"\n"+msg);
			    }
			};
			if (target === "") {
			    dataToArray(data,status,documentLog);
			    clean_setTable();
			} else {
			    target.children[5].innerHTML="manual test";
			}
			documentLog.innerHTML="";}
		})
	    .error(
		function (error) { alert("Test request failed (system error)");}
	    );
    };
};
function clean_runNow(target,type,file,row) {
    var root="";
    var password=document.getElementById("cleanConfigFilePsw").value;
    if (target === "") {root="clean.cfg";};
    if (file !== "") {
	fark_last[type]=file;
	row.children[7].innerHTML=""; // last
	row.children[8].innerHTML="# running"; // info
	documentLog.innerHTML="Sent clean-now request ("+file+").";
	console.log("Sending sever request....",type,file);
	$.ajaxSetup({timeout:0}); // never timeout a request (and re-send it)...
	$.get("cgi-bin/fark_clean.pl",{root:root,password:password,type:type,file:file})
	    .success(
		function(data, status){
		    console.log("Success...");
		    //console.log("Here...");
		    if (status == "success") {
			var errors=data.getElementsByTagName("error");
			if (errors.length > 0 ) {
			    var msg=getErrorMessage(errors);
			    if (isRunning(msg)) {
				alert(msg+"\nMonitoring has timed out.");
			    } else {
				alert("Unable to process, "+type+" config file: "+file+"\n"+msg);
			    }
			};
			if (target === "") {
			    dataToArray(data,status,documentLog);
			    clean_setTable();
			} else {
			    target.children[5].innerHTML="manual run";
			}
			documentLog.innerHTML="";}
		})
	    .error(
		function (error) { documentLog.innerHTML="Run request failed (system error)";
				   alert("Run request failed (system error)");
				 }
	    );
    };
};
function clean_stopNow(target,type,file,row) {
    var root="";
    var password=document.getElementById("cleanConfigFilePsw").value;
    if (target === "") {root="clean.cfg";};
    if (file !== "") {
	fark_last[type]=file;
	row.children[7].innerHTML=""; // last
	row.children[8].innerHTML="# running"; // info
	documentLog.innerHTML="Sent clean-stop request ("+file+").";
	$.get("cgi-bin/fark_clean.pl",{root:root,password:password,type:type,file:file,abort:1})
	    .success(
		function(data, status){
		    if (status == "success") {
			var errors=data.getElementsByTagName("error");
			if (errors.length > 0 ) {
			    var msg=getErrorMessage(errors);
			    alert("Unable to process, "+type+" config file: "+file+"\n"+msg);
			};
			if (target === "") {
			    dataToArray(data,status,documentLog);
			    clean_setTable();
			} else {
			    target.children[5].innerHTML="manual stop";
			}
			documentLog.innerHTML="";}
		})
	    .error(
		function (error) { alert("Stop request failed (system error)");}
	    );
    };
};
function clean_saveConfigFile() {
    var root="clean.cfg";
    var password=document.getElementById("cleanConfigFilePsw").value;
    var modelFiles="";
    var obsFiles="";
    var colocFiles="";
    var tableFiles="";
    var joinFiles="";
    var plotFiles="";
    var rerunFiles="";
    clean_setTable();
    for (var model in clean_config["model"]) {
	modelFiles=modelFiles + "|" + model + "~" + 
	    clean_config["model"][model]["last"] + "~" +
 	    clean_config["model"][model]["info"] + "~" +
 	    clean_config["model"][model]["clean"];
    };
    for (var obs in clean_config["obs"]) {
	obsFiles=obsFiles + "|" + obs + "~" + 
	    clean_config["obs"][obs]["last"] + "~" +
	    clean_config["obs"][obs]["info"] + "~" +
	    clean_config["obs"][obs]["clean"];
    }
    for (var coloc in clean_config["coloc"]) {
	colocFiles=colocFiles + "|" + coloc + "~" + 
	    clean_config["coloc"][coloc]["last"] + "~" +
	    clean_config["coloc"][coloc]["info"] + "~";
	//#+
	//	    #clean_config["coloc"][coloc]["clean"];
    }
    for (var table in clean_config["table"]) {
	tableFiles=tableFiles + "|" + table + "~" + 
	    clean_config["table"][table]["last"] + "~" +
	    clean_config["table"][table]["info"] + "~" +
	    clean_config["table"][table]["clean"];
    }
    for (var join in clean_config["join"]) {
	joinFiles=joinFiles + "|" + join + "~" + 
	    clean_config["join"][join]["last"] + "~" +
	    clean_config["join"][join]["info"] + "~" +
	    clean_config["join"][join]["clean"];
    }
    for (var plot in clean_config["plot"]) {
	plotFiles=plotFiles + "|" + plot + "~" + 
	    clean_config["plot"][plot]["last"] + "~" +
	    clean_config["plot"][plot]["info"] + "~" +
	    clean_config["plot"][plot]["clean"];
    }
    for (var rerun in clean_config["rerun"]) {
	rerunFiles=rerunFiles + "|" + rerun + "~" + 
	    clean_config["rerun"][rerun]["last"] + "~" +
	    clean_config["rerun"][rerun]["info"] + "~" +
	    clean_config["rerun"][rerun]["clean"];
    }
    documentLog.innerHTML="Sent clean-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"clean",root:root,password:password,
				  modelFiles:modelFiles,obsFiles:obsFiles,colocFiles:colocFiles,
				  tableFiles:tableFiles,joinFiles:joinFiles,plotFiles:plotFiles,
				  rerunFiles:rerunFiles})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to save clean config file: "+root+"\n"+msg);
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Clean save request failed (system error)");}
	);
    makeUrl("clean",root);
};
function clean_removeFile(item,type,file) {
    var newitem=document.getElementById("newlineClean");
    newitem.children[0].children[0].value=type;
    newitem.children[2].children[0].value=file;
    newitem.children[4].children[0].value=clean_cron[0];
    //if (! checkCleanPassword()) {return;}
    //item.parentNode.removeChild(item);
    delete clean_config[type][file];
    clean_setTable();
};

//#F78181

// create clean table
function clean_setTable() {
    var item=document.getElementById('cleanTable');
    var tail=removeTableChildFromTo(item,"labelsClean","newlineClean");
    var models=[];
    var obss=[];
    var colocs=[];
    var tables=[];
    var joins=[];
    var plots=[];
    var reruns=[];
    for (var ii = 0; ii < clean_cron.length; ++ii) {
	var cron=clean_cron[ii];
	var modell=[];
	var obsl=[];
	var colocl=[];
	var tablel=[];
	var joinl=[];
	var plotl=[];
	var rerunl=[];
	for (var model in clean_config["model"]) {
	    if (clean_config["model"][model]["clean"] === cron) {
		//console.log("*** Found: ",cron, model);
		modell.push(model);
	    }
	}
	for (var obs in clean_config["obs"]) {
	    if (clean_config["obs"][obs]["clean"] == cron) {
		//console.log("*** Found: ",cron,obs);
		obsl.push(obs);
	    }
	}
	for (var coloc in clean_config["coloc"]) {
	    if (clean_config["coloc"][coloc]["clean"] == cron) {
		//console.log("*** Found: ",cron,coloc);
		colocl.push(coloc);
	    }
	}
	for (var table in clean_config["table"]) {
	    if (clean_config["table"][table]["clean"] == cron) {
		//console.log("*** Found: ",cron,table);
		tablel.push(table);
	    }
	}
	for (var join in clean_config["join"]) {
	    if (clean_config["join"][join]["clean"] == cron) {
		//console.log("*** Found: ",cron,join);
		joinl.push(join);
	    }
	}
	for (var plot in clean_config["plot"]) {
	    if (clean_config["plot"][plot]["clean"] == cron) {
		//console.log("*** Found: ",cron,plot);
		plotl.push(plot);
	    }
	}
	for (var rerun in clean_config["rerun"]) {
	    if (clean_config["rerun"][rerun]["clean"] == cron) {
		//console.log("*** Found: ",cron,rerun);
		rerunl.push(rerun);
	    }
	}
	// sort...
	modell.sort();
	obsl.sort();
	colocl.sort();
	tablel.sort();
	joinl.sort();
	plotl.sort();
	rerunl.sort();
	// add to global array...
	models.extend(modell);
	obss.extend(obsl);
	colocs.extend(colocl);
	tables.extend(tablel);
	joins.extend(joinl);
	plots.extend(plotl);
	reruns.extend(rerunl);
    }
    for (var ii = 0; ii < models.length; ++ii) {
	var model=models[ii];
	//console.log("Insert row: ",model);
	clean_insertRow(tail,"model",model,
		       clean_config["model"][model]["last"],
		       clean_config["model"][model]["info"],
		       clean_config["model"][model]["clean"],
		       clean_config["model"][model]["status"],"#0A0"); 
    }
    for (var ii = 0; ii < obss.length; ++ii) {
	var obs=obss[ii];
	//console.log("Insert row: ",obs);
	clean_insertRow(tail,"obs",obs,
		       clean_config["obs"][obs]["last"],
		       clean_config["obs"][obs]["info"],
		       clean_config["obs"][obs]["clean"],
		       clean_config["obs"][obs]["status"],"#AFA");
    }
    for (var ii = 0; ii < colocs.length; ++ii) {
	var coloc=colocs[ii];
	//console.log("Insert row: ",coloc);
	clean_insertRow(tail,"coloc",coloc,
		       clean_config["coloc"][coloc]["last"],
		       clean_config["coloc"][coloc]["info"],
		       clean_config["coloc"][coloc]["clean"],
		       clean_config["coloc"][coloc]["status"],"#FAA");
    }
    for (var ii = 0; ii < tables.length; ++ii) {
	var table=tables[ii];
	//console.log("Insert row: ",table);
	clean_insertRow(tail,"table",table,
		       clean_config["table"][table]["last"],
		       clean_config["table"][table]["info"],
		       clean_config["table"][table]["clean"],
		       clean_config["table"][table]["status"],"#FA0");
    }
    for (var ii = 0; ii < joins.length; ++ii) {
	var join=joins[ii];
	//console.log("Insert row: ",join);
	clean_insertRow(tail,"join",join,
		       clean_config["join"][join]["last"],
		       clean_config["join"][join]["info"],
		       clean_config["join"][join]["clean"],
		       clean_config["join"][join]["status"],"#AF0");
    }
    for (var ii = 0; ii < plots.length; ++ii) {
	var plot=plots[ii];
	//console.log("Insert row: ",plot);
	clean_insertRow(tail,"plot",plot,
		       clean_config["plot"][plot]["last"],
		       clean_config["plot"][plot]["info"],
		       clean_config["plot"][plot]["clean"],
		       clean_config["plot"][plot]["status"],"#0AF");
    }
    for (var ii = 0; ii < reruns.length; ++ii) {
	var rerun=reruns[ii];
	//console.log("Insert row: ",rerun);
	clean_insertRow(tail,"rerun",rerun,
		       clean_config["rerun"][rerun]["last"],
		       clean_config["rerun"][rerun]["info"],
		       clean_config["rerun"][rerun]["clean"],
		       clean_config["rerun"][rerun]["status"],"#F6F");
    }
};
// create clean table row
function clean_insertRow(item,type,file,last,info,clean,status,color) {
    var row = document.createElement("TR");
    row.setAttribute("bgcolor",color);
    var td;
    // make TYPE column
    td=document.createElement("TD");
    td.innerHTML=type;
    td.setAttribute("title","Schedule type.");
    row.appendChild(td);
    // make select-TYPE column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make FILE NAME column
    td=document.createElement("TD");
    td.innerHTML=file;
    if (type == "model") {
	td.setAttribute("title","Maintain <model file index>.");
    } else if (type == "obs") {
	td.setAttribute("title","Maintain <observation file index>.");
    } else if (type == "coloc") {
	td.setAttribute("title","Create <colocation xml> for debugging.");
    } else if (type == "table") {
	td.setAttribute("title","Create <table file>.");
    } else if (type == "join") {
	td.setAttribute("title","Join <table file> into new <table file>.");
    } else if (type == "plot") {
	td.setAttribute("title","Run plotting script using data in <table file>.");
    } else if (type == "rerun") {
	td.setAttribute("title","Rerun a sub-set of jobs.");
    } else {
    };
    row.appendChild(td);
    //console.log("Row file name=",file);
    // make select-FILE NAME column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make CLEAN checkbox column
    td=document.createElement("TD");
    td.setAttribute("title","Repetition interval.");
    td.innerHTML=clean;
    row.appendChild(td);
    // make select-TYPE column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make manual column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:75px;width:75px");
    td.setAttribute("align","center");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","clean_testNow('','"+type+"','"+file+"',this.parentNode.parentNode)");
    btn.setAttribute("class","test");
    if (type == "coloc") {
	btn.innerHTML="&#9762"
    } else {
	btn.innerHTML="&#9655"
    }
    btn.setAttribute("title","Test now, stop processing as soon as some output has been produced.");
    //var t=document.createTextNode();
    //btn.appendChild(t);
    td.appendChild(btn);
    // make RUN NOW column
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","clean_runNow('','"+type+"','"+file+"',this.parentNode.parentNode)");
    btn.setAttribute("class","run");
    if (type == "coloc") {
	btn.innerHTML="&#9762"
    } else {
	btn.innerHTML="&#9654"
    }
    btn.setAttribute("title","Run now, process all data completely.");
    //var t=document.createTextNode();
    //btn.appendChild(t);
    td.appendChild(btn);
    // make STOP column
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","clean_stopNow('','"+type+"','"+file+"',this.parentNode.parentNode)");
    btn.setAttribute("class","stop");
    if (type == "coloc") {
	btn.innerHTML="&#9762"
    } else {
	btn.innerHTML="&#9632"
    }
    btn.setAttribute("title","Stop process.");
    //var t=document.createTextNode();
    //btn.appendChild(t);
    td.appendChild(btn);
    // make LAST column
    row.appendChild(td);
    td=document.createElement("TD");
    if (status !== "") {
	td.setAttribute("style","color:blue");
    }
    var a = document.createElement('a');
    var linkText = document.createTextNode(last);
    a.appendChild(linkText);
    a.title = "View last log-file and error-file";
    a.href = "cgi-bin/fark_log.pl?type="+type+"&file="+file;
    a.target ="_blank";
    td.appendChild(a);
    row.appendChild(td);
    // make INFO column
    td=document.createElement("TD");
    if (type =="model") {
	td.title = "Show status of the model-index-file.";
    } else if (type =="obs") {
	td.title = "Show status of the observation-index-file.";
    } else if (type =="table") {
	td.title = "Show status of the table-file.";
    } else if (type =="join") {
	td.title = "Show status of the joined table-file.";
    } else if (type =="plot") {
	td.title = "Show status of the plot files.";
    } else if (type =="rerun") {
	td.title = "Show status of the rerun files.";
    } else {
	td.title = "Show status.";
    };
    td.innerHTML=info;
    row.appendChild(td);
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px;");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","clean_removeFile(this.parentNode.parentNode,'"+type+"','"+file+"')");
    btn.setAttribute("style","width:100%");
    btn.setAttribute("title","Remove scheduled job");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make add row to table
    item.parentNode.insertBefore(row,item);
    return row;
}

function isRunning(msg) {
    return (msg.substring(0,18) === "Process is running");
}

function clean_showType(item,target,arg) {
    removeChildren(item);
    addChildButton(item,"model","showValue('"+target+"','model');","Maintain model index");
    addChildButton(item,"observation","showValue('"+target+"','obs');","Maintain observation index");
    addChildButton(item,"colocation","showValue('"+target+"','coloc');","Make colocation XML (debugging only)");
    addChildButton(item,"table","showValue('"+target+"','table');","Make table file");
    addChildButton(item,"join","showValue('"+target+"','join');","Join table files");
    addChildButton(item,"plot","showValue('"+target+"','plot');","Make plots");
    addChildButton(item,"rerun","showValue('"+target+"','rerun');","Rerun jobs");
};

function clean_showConfigFile(item,target,arg) {
    var type=document.getElementById("cleanType").value // "obs";
    var args=getArgs(arg);
    documentLog.innerHTML="Sent clean-load request.";
    $.get("cgi-bin/fark_load.pl",{type:type,arg:args})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+arg+"'\n"+msg);
		} else {
		    var ret=dataToArray(data,status,documentLog);
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			item.classList.toggle("show");
			var msg=getErrorMessage(errors);
			alert("Unable to list '"+arg+"', type '"+type+"' \n"+msg);
		    } else if (ret[0] !== undefined) {
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
			    addChildButton(item,"<up>","showValue('cleanConfigFile','"+dd+"');","Change to parent <directory>");
			    added=true;
			} else {
			    //console.log("Adding clear: ",dd);
			    addChildButton(item,"<up>","showValue('cleanConfigFile','');","Change to root <directory>");
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
				    if (looksLikeFile(dd)) {
					addChildButton(item,dd,"showValue('cleanConfigFile','"+dd+"');","Use <file>");
					added=true;
				    } else {
					addChildButton(item,dd,"showValue('cleanConfigFile','"+dd+"');","Change <directory>");
					added=true;
				    };
				}
			    }
			}
			if (! added) {addChildText(item,"No data available...");}
		    } else {
			console.log("Undefined root.");
		    }
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Clean request failed (system error)");}
	);
};

function clean_showCron(item,target,arg) {
    var args=getArgs(arg);
    removeChildren(item);
    var added=false;
    for (var ii=0;ii<clean_cron.length;ii++) {
	var cron=clean_cron[ii];
	if (cron === "") {
	    addChildButton(item,"<never>","showValue('cleanCron','"+cron+"');","Never run job");
	    added=true;
	} else {
	    addChildButton(item,cron,"showValue('cleanCron','"+cron+"');","Run job <interval>");
	    added=true;
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};

function clean_showFilterDir(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent clean-load request.";
    var file=clean_getConfigFile();
    var path=args[0] || "";
    var cls = "data";
    var filter=clean_config[file]["filterFile"];
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
					   "clean_setArray('filterDir','"+dd+"');clean_show();","Change to parent <directory>");
			    added=true;
			};
			var dirs=ls[0].getElementsByTagName("dir");
			//console.log("Found dir entries: ",dirs.length);
			for (var ii=0; ii< dirs.length; ii++) {
			    var dd = dirs[ii].getAttribute("path");
			    //console.log("Adding dir button: ",dd);
			    if (looksLikeFile(dd)) {
				addChildButton(item,dd,"clean_setArray('filterDir','"+dd+"');clean_show();","Use <file>");
				added=true;
			    } else {
				addChildButton(item,dd,"clean_setArray('filterDir','"+dd+"');clean_show();","Change <directory>");
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
				addChildButtonShaded(item,dd,"clean_setArray('filterFile','"+rr+"');clean_show();","Copy <pattern> to filter");
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
				addChildButton(item,size+" "+dd,"clean_setArray('filterFile','"+dd+"');clean_show();","Copy <file name> to filter");
				added=true;
			    };
			};
		    };
		};
		if (! added) {addChildText(item,"No data available...");}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Clean dir filter request failed (system error)");}
	);
};

function clean_showFilterFile(item,target,arg) {
    var file=clean_getConfigFile();
    var password=document.getElementById("cleanConfigFilePsw").value;
    var filterDir = clean_config[file]["filterDir"];
    var filterDirMin = clean_config[file]["filterDirMin"];
    var filterDirMax = clean_config[file]["filterDirMax"];
    var filterFile = clean_config[file]["filterFile"];
    var indexTarget = clean_config[file]["indexTarget"];
    var indexVariable = clean_config[file]["indexVariable"];
    documentLog.innerHTML="Sent clean-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"clean",
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
			setInnerHTML('cleanPatternHits',clean_config[file]["hits"]);
			removeChildren(item);
			var added=false;
			var len=clean_config[file]["files"].length;
			for (var ii=0; ii<len;ii++) {
			    var sfile=clean_config[file]["files"][ii][0];
			    var sage=parseFloat(clean_config[file]["files"][ii][1]).toFixed(2);
			    var ssize=clean_config[file]["files"][ii][2];
			    addChildButton(item,ssize+" "+sfile+" ("+sage+"d)","clean_fileFind('"+sfile+"');","Scan <clean file>");
			    added=true;
			}
			if (! added) {addChildText(item,"No data available...");}
		    };
		    documentLog.innerHTML="";
		}
	    })
	.error(
	    function (error) { alert("Clean file filter request failed (system error)");}
	);
};

#__file: 'js/coloc.js' 0100644    **DO NOT DELETE**
coloc_file="default.cfg"; // always valid file
coloc_config = { "default.cfg" : { modelConfigFile : { file: "default.cfg",
						       min : "def_min",
						       max : "def_max",
						       exp : "",
						       targets : { "def_model" : { variable : "def",
										   min: "def_min",
										   max : "def_max",
									           exp : "" 
										 } },
						       targeto : ["def_model"],
						       def : [ {targets: {"def_model": 101}, 
								info:"default info"} ]
						     },
				   obsConfigFile : { file: "default.cfg",
						     min: "def_min",
						     max : "def_max",
						     targets : { "def_obs" : {pos:"", 
									      descr:"", 
									      info:"",  
									      min:"", 
									      max:""}
							       },
						     targeto : ["def_obs"]
						   },
				   host:"fark.met.no",
				   filter:"",
				   xml:"",
				   password: ""
				 }
	       };
coloc_configEd = 0;
modelLoaded=false;
obsLoaded=false;
function coloc_modelIsNotLoaded(mfile) {
    return (model_config[mfile] === undefined);
};
function coloc_obsIsNotLoaded(ofile) {
    return (obs_config[ofile]===undefined);
};
function coloc_allocate(file) {
    if (coloc_config[file] === undefined) {
	coloc_config[file]=clone(coloc_config[coloc_file]);
	//console.log("cloned:",file,coloc_file,coloc_config[file]);
    }
}
function coloc_setConfigFile2(file) {
    showValue('colocConfigFile',file);
    showValue('colocConfigFileSave',file);
    //console.log("Setting file= '"+file+"'");
}
function coloc_setConfigFile(file) {
    //console.log("Setting file= '"+file+"'");
    showValue('colocConfigFile',file);
    showValue('colocConfigFileSave',file);
    //if (file != "") {
    coloc_allocate(file);
    coloc_file=file;
    var mfile=coloc_getModelConfigFile();
    fark_last['model']=coloc_getModelConfigFile();
    if (coloc_modelIsNotLoaded(mfile)) {coloc_updateModelData(mfile);}
    var ofile=coloc_getObsConfigFile();
    fark_last['obs']=coloc_getObsConfigFile();
    if (coloc_obsIsNotLoaded(ofile)) {coloc_updateObsData(ofile);}
    //};
    coloc_showCOLOC();
};
function coloc_getConfigFile() {
    return coloc_file;
};
function coloc_getModelConfigFile( file = "") {
    if (file === "") {file=coloc_getConfigFile();}
    if (coloc_config[file] !== undefined) {
	return coloc_config[file]["modelConfigFile"]["file"];
    } else {
	return "";
    }
};
function coloc_getObsConfigFile(file = "") {
    if (file === "") {file=coloc_getConfigFile();}
    if (coloc_config[file] !== undefined) {
	return coloc_config[file]["obsConfigFile"]["file"];
    } else {
	return "";
    }
};
function coloc_setConfig(type,parameter,val) {
    var file=coloc_getConfigFile();
    coloc_config[file][type][parameter]=val;
    // load if we are changing obs or model config files
    if (parameter === "file" && type === "model") {
	documentLog.innerHTML="Sent "+type+"-load request.";
	$.get("cgi-bin/fark_load.pl",{type:type})
	    .success(
		function(data, status){
		    dataToArray(data,status,documentLog);
		    modelLoaded=true;
		    documentLog.innerHTML="";
		})
	    .error(
		function (error) { alert("Coloc model request failed (system error)");}
	    );
    } else if (parameter === "file" && type === "obs") {
	documentLog.innerHTML="Sent "+type+"-load request.";
	$.get("cgi-bin/fark_load.pl",{type:type})
	    .success(function(data, status){
		dataToArray(data,status,documentLog);
		obsLoaded=true;
		documentLog.innerHTML="";
	    })
	    .error(
		function (error) { alert("Coloc obs request failed (system error)");}
	    );
    }
    coloc_showCOLOC();
}
function coloc_addConfig (type,parameter,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	coloc_config[file][type][parameter]=coloc_config[file][type][parameter]+val;
	coloc_showCOLOC();
    }
}
function coloc_setConfigFilesTarget (type,target,parameter,val) {
    //console.log("*** coloc_setConfigFilesTarget ");
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	if (coloc_config[file][type] === undefined || 
	    coloc_config[file][type]['targets'] ===undefined ||
	    coloc_config[file][type]['targets'][target] ===undefined ||
	    coloc_config[file][type]['targets'][target][parameter] ===undefined) {
	    console.log("Undefined:",type,target,parameter,val);
	};
	coloc_config[file][type]['targets'][target][parameter]=val;
	coloc_show();
    }
}
function coloc_addConfigFilesTarget (type,target,parameter,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	if (coloc_config[file][type] === undefined || 
	    coloc_config[file][type]['targets'] ===undefined ||
	    coloc_config[file][type]['targets'][target] ===undefined ||
	    coloc_config[file][type]['targets'][target][parameter] ===undefined) {
	    console.log("Undefined:",type,target,parameter,val);
	};
	coloc_config[file][type]['targets'][target][parameter]=coloc_config[file][type]['targets'][target][parameter]+val;
	coloc_showModelDefaultTable();
	coloc_showCOLOC();
    }
}
function coloc_setArray (type,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	coloc_config[file][type]=decodeURI(val);
    }
}
function coloc_setArrayPar (type,parameter,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	coloc_config[file][type][parameter]=val;
    }
}
function coloc_setConfigFilesDefault (ii,target,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	coloc_config[file]["modelConfigFile"]["def"][ii]["targets"][target]=val;
	coloc_showCOLOC();
    }
}
function coloc_setConfigFilesDefaultInfo (ii,val) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	coloc_config[file]["modelConfigFile"]["def"][ii]["info"]=val;
	coloc_showCOLOC();
    }
}
function coloc_newModelTarget(item) {
    var name=item.parentNode.parentNode.children[0].children[0].value;
    var variable=item.parentNode.parentNode.children[1].children[0].value;
    var minimum=item.parentNode.parentNode.children[3].children[0].value;
    var maximum=item.parentNode.parentNode.children[4].children[0].value;
    if (name !== "" && variable !== "") {
	var file= coloc_getConfigFile();
	if (coloc_config[file] === undefined) {
	    coloc_config[file]={modelConfigFile:{targets:{},targeto:[],def:{}},
				obsConfigFile:{targets:{},targeto:[]},
				host:"fark.met.no",
				password:""};
	};
	if (coloc_config[file]["modelConfigFile"]["targets"][name] === undefined) {
	    coloc_config[file]["modelConfigFile"]["targeto"].push(name);
	};
	coloc_config[file]["modelConfigFile"]["targets"][name]={};
	coloc_config[file]["modelConfigFile"]["targets"][name]["variable"]=(variable || "");
	coloc_config[file]["modelConfigFile"]["targets"][name]["min"]=(minimum || "");
	coloc_config[file]["modelConfigFile"]["targets"][name]["max"]=(maximum || "");
	coloc_config[file]["modelConfigFile"]["targets"][name]["exp"]="";
	coloc_configEd++;
	//coloc_showModelTargetTable();
	//coloc_showModelDefaultTable();
	coloc_show();
	item.parentNode.parentNode.children[0].children[0].value="";	
	item.parentNode.parentNode.children[1].children[0].value="";	
	item.parentNode.parentNode.children[3].children[0].value="";	
	item.parentNode.parentNode.children[4].children[0].value="";	
    } else {
	alert("Invalid: name ('"+name+"'), variable ('"+variable+"')");
    }
};

function coloc_newModelDefault(item) {
    var file= coloc_getConfigFile();
    var mfile=coloc_getModelConfigFile();
    if (model_config[mfile] !== undefined) {
	var indexTarget=model_config[mfile]["indexTarget"];
    } else {
	var indexTarget="";
    }
    var line={targets:{},info:{}};
    var ok=false;
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    var pos=0;
    var val=item.parentNode.parentNode.children[pos].children[0].value
    if (val !== undefined && val !== "") {
	ok=true;
    }
    line["targets"][indexTarget]=(val|| "");
    item.parentNode.parentNode.children[pos].children[0].value="";
    pos=pos+1;
    for (var target in targets) {
	var val=item.parentNode.parentNode.children[pos].children[0].value
	if (val !== undefined && val !== "") {
	    ok=true;
	}
	line["targets"][target]=(val|| "");
	item.parentNode.parentNode.children[pos].children[0].value="";
	pos=pos+1;
    }
    var info=item.parentNode.parentNode.children[pos].children[0].value;
    item.parentNode.parentNode.children[pos].children[0].value="";
    line["info"]=info;
    if (ok) {
	//console.log("coloc_config:",coloc_config[file]["modelConfigFile"]["def"]);
	coloc_config[file]["modelConfigFile"]["def"].push(line);
	//coloc_showModelDefaultTable();
	coloc_show();
    } else {
	alert("Invalid model default value.");
    }
};

function coloc_newObsTarget(item) {
    var ofile=coloc_getObsConfigFile();
    if ( obs_config[ofile] !== undefined) {
	var name=item.parentNode.parentNode.children[0].children[0].value;
	if (obs_config[ofile]["targets"][name] === undefined) {
	    var pos=item.parentNode.parentNode.children[1].children[0].value;
	    var descr=item.parentNode.parentNode.children[3].children[0].value;
	    var info=item.parentNode.parentNode.children[4].children[0].value;
	    var minimum=item.parentNode.parentNode.children[5].children[0].value;
	    var maximum=item.parentNode.parentNode.children[6].children[0].value;
	    var bufrType = obs_config[ofile]["bufrType"];
	    var subType = obs_config[ofile]["subType"];
	    if (name !== "") {
		if ((pos !== "" && bufrType !== "" && subType !== "") || (minimum != "" && maximum != "")) {
		    var file= coloc_getConfigFile();
		    if (coloc_config[file] === undefined) {
			coloc_config[file]={modelConfigFile:{targets:{},targeto:[],def:{}},
					    obsConfigFile:{targets:{},targeto:[]},
					    host:"fark.met.no",
					    password:""};
		    };
		    if (coloc_config[file]["obsConfigFile"]["targets"][name] === undefined) {
			coloc_config[file]["obsConfigFile"]["targeto"].push(name);
		    }
		    coloc_config[file]["obsConfigFile"]["targets"][name]={};
		    coloc_config[file]["obsConfigFile"]["targets"][name]["pos"]=(pos || "");
		    coloc_config[file]["obsConfigFile"]["targets"][name]["descr"]=(descr || "");
		    coloc_config[file]["obsConfigFile"]["targets"][name]["info"]=(info || "");
		    coloc_config[file]["obsConfigFile"]["targets"][name]["min"]=(minimum || "");
		    coloc_config[file]["obsConfigFile"]["targets"][name]["max"]=(maximum || "");
		    coloc_configEd++;
		    //coloc_showObsTargetTable();
		    coloc_show();
		    item.parentNode.parentNode.children[0].children[0].value="";
		    item.parentNode.parentNode.children[1].children[0].value="";
		    item.parentNode.parentNode.children[3].children[0].value="";
		    item.parentNode.parentNode.children[4].children[0].value="";
		    item.parentNode.parentNode.children[5].children[0].value="";
		    item.parentNode.parentNode.children[6].children[0].value="";
		} else {
		    alert("Invalid: position ('"+pos+"'), BUFR type ('"+bufrType+"'), subType ('"+
			  subType+"'), minimum('"+minimum+"'), maximum('"+maximum+"') detected.");
		}
	    } else {
		alert("Invalid: name ('"+name+"') detected.");
	    }
	} else {
	    alert("'"+name+"' already used in obs config file: '"+ofile+"'");
	}
    } else {
	alert("Obs config file not loaded ('"+ofile+"')");
    }
};

// create model target table
function coloc_showModelTargetTable() {
    var item=document.getElementById('modelTargetTable');
    var file=coloc_getConfigFile();
    //console.log("coloc: Showing ",file, coloc_config[file]["modelConfigFile"]["file"],coloc_config);
    var tail=removeTableChildFromTo(item,"labelsModelTarget","newlineModelTarget");
    var targeto=coloc_config[file]["modelConfigFile"]["targeto"];
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    var mfile=coloc_getModelConfigFile();
    //
    if (model_config[mfile] !== undefined) {
	var indexTarget=model_config[mfile]["indexTarget"];
	var indexVariable=model_config[mfile]["indexVariable"];
	var indexMin=coloc_config[file]["modelConfigFile"]["min"];
	var indexMax=coloc_config[file]["modelConfigFile"]["max"];
	var color="green";
	var variables=model_config[mfile]["variables"];
	if (variables !== undefined) {
	    if (variables[indexVariable] === undefined) {
		color="red";
	    };
	} else {
	    color="black";
	};
	coloc_insertModelTargetIndexRow(tail,indexTarget,indexVariable,color,indexMin,indexMax);
	//for (var target in targets) {
	if (model_config[mfile] !== undefined) {
	    var dimensions=model_config[mfile]["dimensions"];
	} else {
	    var dimensions;
	}
	for (var ii =0; ii< targeto.length;ii++) {
	    var target=targeto[ii];
	    var variable = targets[target]["variable"];
	    color="green";
	    var len=variable.length;
	    //console.log("*** Check:",variable);
	    if (variable.substring(0,1) == "(" && 
		variable.substring(len-1,len) == ")") { // dimension
		    var dim =removeSubstring(variable.substring(1,len-1),"[");
		//console.log("*** Dimension:",dim);
		if (dimensions !== undefined) {
		    if (dimensions[dim] === undefined) {
			color="red";
		    };
		} else {
		    color="black";
		};
	    } else {                                    // variable
		if (variables !== undefined) {
		    if (variables[removeSubstring(variable,"[")] === undefined) {
			color="red";
		    };
		} else {
		    color="black";
		};
	    }
	    //console.log("*** Target:",target,targets[target]["variable"],color,variables[target]);
	    coloc_insertModelTargetRow(tail,target,ii,targets[target]["variable"],color,
				       targets[target]["min"],targets[target]["max"]);
	}
    };
};
// create exec table row
function coloc_insertModelTargetIndexRow(item,target,variable,color,min,max) {
    var row = document.createElement("TR");
    var td, inp;
    //
    // make target name column
    td=document.createElement("TD");
    td.setAttribute("style","color:darkorange");
    td.innerHTML=target;
    row.appendChild(td);
    //
    // make model variable column
    td=document.createElement("TD");
    td.setAttribute("colspan","2");
    var tb=document.createElement("TABLE");
    tb.setAttribute("style","width:100%");
    var trow=document.createElement("TR");
    var ttd=document.createElement("TD");
    ttd.setAttribute("style","color:"+color);
    ttd.innerHTML=variable;
    trow.appendChild(ttd);
    // button <-
    var ttd=document.createElement("TD");
    ttd.setAttribute("style","align:right;min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("title","Use lowest scanned value as <Minimum>");
    btn.setAttribute("onclick","coloc_getModelIndexStart('colocModelIndexStart','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("←"); // "→"
    btn.appendChild(t);
    ttd.appendChild(btn);
    trow.appendChild(ttd);
    // button ->
    var ttd=document.createElement("TD");
    ttd.setAttribute("style","align:right;min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("title","Use highest scanned value as <Maximum>");
    btn.setAttribute("onclick","coloc_getModelIndexStop('colocModelIndexStop','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("→"); // "←"
    btn.appendChild(t);
    ttd.appendChild(btn);
    trow.appendChild(ttd);
    tb.appendChild(trow);
    td.appendChild(tb);
    row.appendChild(td);
    // make minimum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    inp.setAttribute("id","colocModelIndexStart");
    inp.setAttribute("style","width:150px");
    inp.setAttribute("onblur","coloc_setConfig('modelConfigFile','min',this.value);coloc_showModelTargetTable();");
    inp.setAttribute("title","Minimum model index value");
    td.appendChild(inp);

    row.appendChild(td);
    // make maximum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT"); // 
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("id","colocModelIndexStop");
    inp.setAttribute("style","width:150px");
    inp.setAttribute("onblur","coloc_setConfig('modelConfigFile','max',this.value);coloc_showModelTargetTable();");
    inp.setAttribute("title","Maximum model index value");
    td.appendChild(inp);
    row.appendChild(td);
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make add row to table
    if (item !== undefined) {
	item.parentNode.insertBefore(row,item);
    } else {
	console.log("coloc Undefined item.",target,variable,min,max);
    };
    return row;
}
// create exec table row
function coloc_insertModelTargetRow(item,target,ii,variable,color,min,max) {
    var row = document.createElement("TR");
    var file = coloc_getModelConfigFile();
    //console.log("coloc: Adding model target row for :",file,target,variable);
    var td, inp;
    // make target name column
    td=document.createElement("TD");
    td.setAttribute("style","");
    td.innerHTML=target;
    row.appendChild(td);
    // make model variable column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("title","Model target name");
    inp.setAttribute("type","text");
    inp.setAttribute("value",variable);
    inp.setAttribute("style","width:100%;color:"+color);
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','variable',this.value);coloc_showModelTargetTable()");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-variable column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    td.setAttribute("align","center");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("title","Move model target up one step");
    btn.setAttribute("onclick","coloc_modelUp('"+ii+"');coloc_show();");
    //btn.setAttribute("style","width:100%");
    //var t=document.createTextNode("--");
    //btn.appendChild(t);
    btn.innerHTML="&uarr;";
    //btn.setAttribute("align","center");
    td.appendChild(btn);
    row.appendChild(td);
    // make minimum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    inp.setAttribute("style","width:150px");
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','min',this.value);");
    inp.setAttribute("title","Minimum model value");
    td.appendChild(inp);
    row.appendChild(td);
    // make maximum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT"); // 
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("style","width:150px");
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);");
    inp.setAttribute("title","Maximum model value");
    td.appendChild(inp);
    row.appendChild(td);
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("title","Remove model target");
    btn.setAttribute("onclick","coloc_removeModelTarget(this.parentNode.parentNode,'"+target+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make add row to table
    if (item !== undefined) {
	item.parentNode.insertBefore(row,item);
    } else {
	console.log("coloc Undefined item.",target,variable,min,max);
    };
    return row;
}
// create model target table
function coloc_showModelDefaultTable() {
    var item=document.getElementById('modelDefaultTable');
    var header=clearTableChild(item,"labelsModelDefault");
    var newline=clearTableChild(item,"newlineModelDefault");
    var file=coloc_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsModelDefault","newlineModelDefault");
    coloc_insertModelDefaultHeader(header,file);
    coloc_insertModelDefaultNewline(newline,file);
    coloc_insertModelDefaultRow(tail,file);
};
// create model default table header
function coloc_insertModelDefaultHeader(row,file) {
    var td,bf;
    var mfile=coloc_getModelConfigFile();
    if (model_config[mfile] !== undefined) {
	var indexTarget=model_config[mfile]["indexTarget"];
    } else {
	var indexTarget="";
    }
    td=document.createElement("TD");
    bf=document.createElement("BF");
    bf.innerHTML=indexTarget;
    td.appendChild(bf);
    row.appendChild(td);
    var targeto=coloc_config[file]["modelConfigFile"]["targeto"];
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    //    for (var target in targets) {
    for (var ii =0; ii< targeto.length;ii++) {
	var target=targeto[ii];
	// make variable names
	td=document.createElement("TD");
	bf=document.createElement("BF");
	bf.innerHTML=target;
	td.appendChild(bf);
	row.appendChild(td);
    }
    td=document.createElement("TD");
    bf=document.createElement("BF");
    bf.innerHTML="information";
    td.appendChild(bf);
    row.appendChild(td);
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
}

function coloc_insertModelDefaultRow(item,file) {
    var td;
    var mfile=coloc_getModelConfigFile();
    if (model_config[mfile] !== undefined) {
	var indexTarget=model_config[mfile]["indexTarget"];
    } else {
	var indexTarget="";
    }
    var defs=coloc_config[file]["modelConfigFile"]["def"];
    var len=defs.length;
    for (var ii=0;ii<len;ii++){
	if (coloc_config[file]["modelConfigFile"]["def"][ii]["targets"] !== undefined) {
	    var row = document.createElement("TR");
	    // insert index column
	    td=document.createElement("TD");
	    td.setAttribute("class","fill");
	    td.setAttribute("trg",indexTarget);
	    inp=document.createElement("INPUT");
	    inp.setAttribute("type","text");
	    inp.setAttribute("value",(coloc_config[file]["modelConfigFile"]["def"][ii]["targets"][indexTarget]||""));
	    inp.setAttribute("style","width:100%");
	    inp.setAttribute("onblur","coloc_setConfigFilesDefault("+ii+",'"+indexTarget+"',this.value);");
	    inp.setAttribute("title","Index value");
	    td.appendChild(inp);
	    row.appendChild(td);
	    // insert targets
	    var targets=coloc_config[file]["modelConfigFile"]["targets"];
	    for (var target in targets) {
		// make value column
		td=document.createElement("TD");
		td.setAttribute("class","fill");
		td.setAttribute("trg",target);
		inp=document.createElement("INPUT");
		inp.setAttribute("type","text");
		inp.setAttribute("value",(coloc_config[file]["modelConfigFile"]["def"][ii]["targets"][target]||""));
		inp.setAttribute("style","width:100%");
		inp.setAttribute("onblur","coloc_setConfigFilesDefault("+ii+",'"+target+"',this.value);");
		inp.setAttribute("title","Model target value");
		td.appendChild(inp);
		row.appendChild(td);
	    }
	    td=document.createElement("TD");
	    td.setAttribute("class","fill");
	    inp=document.createElement("INPUT");
	    inp.setAttribute("type","text");
	    inp.setAttribute("value",(coloc_config[file]["modelConfigFile"]["def"][ii]["info"]||""));
	    inp.setAttribute("style","width:100%");
	    inp.setAttribute("onblur","coloc_setConfigFilesDefaultInfo("+ii+",this.value);");
	    inp.setAttribute("title","Information");
	    td.appendChild(inp);
	    row.appendChild(td);
	    // make "-" column
	    td=document.createElement("TD");
	    td.setAttribute("style","min-width:25px;width:25px");
	    var btn=document.createElement("BUTTON");
	    btn.setAttribute("title","Remove model default");
	    btn.setAttribute("onclick","coloc_removeModelDefault(this.parentNode.parentNode,'"+file+"',"+ii+")");
	    btn.setAttribute("style","width:100%");
	    var t=document.createTextNode("-");
	    btn.appendChild(t);
	    td.appendChild(btn);
	    row.appendChild(td);
	    // make add row to table
	    item.parentNode.insertBefore(row,item);
	};
    }
    return;
}

// create model default table newline
function coloc_insertModelDefaultNewline(row,file) {
    var td,btn,inp;
    var mfile=coloc_getModelConfigFile();
    if (model_config[mfile] !== undefined) {
	var indexTarget=model_config[mfile]["indexTarget"];
    } else {
	var indexTarget="";
    }
    // insert index
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    td.setAttribute("id","_"+indexTarget);
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value","");
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","this.value=this.value.replace(/[^\\d\\.]/g,'')");
    inp.setAttribute("title","Index value");
    td.appendChild(inp);
    row.appendChild(td);
    // insert targets
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    for (var target in targets) {
	// make variable names
	td=document.createElement("TD");
	td.setAttribute("class","fill");
	td.setAttribute("id","_"+target);
	inp=document.createElement("INPUT");
	inp.setAttribute("type","text");
	inp.setAttribute("value","");
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","this.value=this.value.replace(/[^\\d\\.]/g,'')");
	inp.setAttribute("title","Target value");
	td.appendChild(inp);
	row.appendChild(td);
    }
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    td.setAttribute("id","information");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value","");
    inp.setAttribute("style","width:100%");
    inp.setAttribute("title","Information");
    td.appendChild(inp);
    row.appendChild(td);
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    btn=document.createElement("BUTTON");
    btn.setAttribute("title","Add model default");
    btn.setAttribute("onclick","coloc_newModelDefault(this)");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("+");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
}
function coloc_removeModelDefault(item,file,ii) {
    //console.log("removing model default:",file,ii);
    coloc_config[file]["modelConfigFile"]["def"].splice(ii,1);
    //coloc_showModelDefaultTable();
    coloc_show();
};
function coloc_showObsTargetTable() {
    //console.log("*** coloc_showObsargetTable ");
    var item=document.getElementById('obsTargetTable');
    var ofile=coloc_getObsConfigFile();
    var file=coloc_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsObsTarget","newlineObsTarget");
    // insert obs targets from obs-config file
    if (obs_config[ofile] !== undefined) {
	var bufr=obs_config[ofile]["bufr"];
	var bufrType=obs_config[ofile]["bufrType"];
	var subType =obs_config[ofile]["subType"];
	var otargeto=obs_config[ofile]["targeto"];
	var otargets=obs_config[ofile]["targets"];
	// for (var target in otargets) {
	for (var ii =0; ii< otargeto.length;ii++) {
	    var target=otargeto[ii];
	    var pos = otargets[target]["pos"];
	    var color="green";
	    if (bufr !== undefined && 
		bufr[bufrType] !== undefined && 
		bufr[bufrType][subType] !== undefined &&
		bufr[bufrType][subType][pos] !== undefined) {
		var descr=bufr[bufrType][subType][pos]["descr"];
		if (descr!=otargets[target]["descr"]) {
		    color="red";
		};
	    } else {
		color="black";
	    };
	    coloc_insertOTargetRow(tail,target,otargets[target]["pos"],
				   otargets[target]["descr"],color,otargets[target]["info"]);
	}
    };
    // make index row
    if (obs_config[ofile] !== undefined) {
	var target=obs_config[ofile]["indexTarget"];
	var exp = obs_config[ofile]["indexExp"];
    } else {
	var target="";
	var exp="";
    }
    var min = coloc_config[file]["obsConfigFile"]["min"];
    var max = coloc_config[file]["obsConfigFile"]["max"];
    coloc_insertOTargetIndexRow(tail,target,exp,min,max);
    // insert obs targets from coloc-config file
    var targeto=coloc_config[file]["obsConfigFile"]["targeto"];
    var targets=coloc_config[file]["obsConfigFile"]["targets"];
    //for (var target in targets) {
    for (var ii =0; ii< targeto.length;ii++) {
	var target=targeto[ii];
	var pos = targets[target]["pos"];
	var color="green";
	if (obs_config[ofile] !== undefined) {
	    var bufr=obs_config[ofile]["bufr"];
	    var bufrType=obs_config[ofile]["bufrType"];
	    var subType =obs_config[ofile]["subType"];
	    if (bufr !== undefined &&
		bufr[bufrType] !== undefined &&
		bufr[bufrType][subType] !== undefined &&
		bufr[bufrType][subType][pos] !== undefined) {
		var descr=bufr[bufrType][subType][pos]["descr"];
		//console.log("*** Table ",pos,descr,targets[target]["descr"],color)
		if (descr!=targets[target]["descr"]) {
		    color="red";
		};
	    } else {
		color="black";
	    };
	} else {
	    color="black";
	}
	coloc_insertObsTargetRow(tail,target,ii,targets[target]["pos"],targets[target]["descr"],color,
				 targets[target]["info"],targets[target]["min"],targets[target]["max"]);
    }
};
// create obs index target table row
function coloc_insertOTargetIndexRow(item,target,exp,min,max) {
    // insert obs target index expression from obs-config file
    var row = document.createElement("TR");
    // make target NAME column  ***************************
    var td=document.createElement("TD");
    td.setAttribute("style","color:blue");
    td.innerHTML=target;
    row.appendChild(td);
    //
    // make expression column  ***************************
    td=document.createElement("TD");
    td.setAttribute("colspan","4");
    var tb=document.createElement("TABLE");
    tb.setAttribute("style","width:100%");
    var trow=document.createElement("TR");
    var ttd=document.createElement("TD");
    ttd.innerHTML=exp
    trow.appendChild(ttd);
    // button <-
    var ttd=document.createElement("TD");
    ttd.setAttribute("style","align:right;min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("title","Use lowest scanned value as <Minimum>");
    btn.setAttribute("onclick","coloc_getObsIndexStart('colocObsIndexStart','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("←"); // "→"
    btn.appendChild(t);
    ttd.appendChild(btn);
    trow.appendChild(ttd);
    // button ->
    var ttd=document.createElement("TD");
    ttd.setAttribute("style","align:right;min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("title","Use highest scanned value as <Maximum>");
    btn.setAttribute("onclick","coloc_getObsIndexStop('colocObsIndexStop','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("→"); // "←"
    btn.appendChild(t);
    ttd.appendChild(btn);
    trow.appendChild(ttd);
    tb.appendChild(trow);
    td.appendChild(tb);
    row.appendChild(td);
    //
    // make minimum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    inp.setAttribute("id","colocObsIndexStart");
    inp.setAttribute("style","width:150px");
    inp.setAttribute("onblur","coloc_setArrayPar('obsConfigFile','min',this.value);");
    inp.setAttribute("title","Minimum observation value");
    td.appendChild(inp);
    row.appendChild(td);
    // make maximum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("id","colocObsIndexStop");
    inp.setAttribute("style","width:150px");
    inp.setAttribute("onblur","coloc_setArrayPar('obsConfigFile','max',this.value);");
    inp.setAttribute("title","Maximum observation value");
    td.appendChild(inp);
    row.appendChild(td);
    // make "-" column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    //
    item.parentNode.insertBefore(row,item);
}
// create exec table row
function coloc_insertOTargetRow(item,target,pos,descr,color,info) {
    var row = document.createElement("TR");
    var td, inp;
    // make NAME column  ***************************
    td=document.createElement("TD");
    td.innerHTML=target;
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.innerHTML=pos;
    row.appendChild(td);
    // make select-pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make descr column  ***************************
    td=document.createElement("TD");
    td.innerHTML=descr;
    if (color !== "") {
	td.setAttribute("style","color:"+color);
    }
    row.appendChild(td);
    // make info column  ***************************
    td=document.createElement("TD");
    td.setAttribute("colspan","3");
    td.innerHTML=info;
    row.appendChild(td);
    // make "-" column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
// create exec table row
function coloc_insertObsTargetRow(item,target,ii,pos,descr,color,info,min,max) {
    var row = document.createElement("TR");
    var td, inp;
    // make NAME column  ***************************
    td=document.createElement("TD");
    td.innerHTML=target;
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("title","Position in BUFR sequence");
    inp.setAttribute("type","text");
    inp.setAttribute("value",pos);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('obsConfigFile','"+target+"','pos',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-subtype column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    td.setAttribute("align","center");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("title","Move observation target up one step");
    btn.setAttribute("onclick","coloc_obsUp('"+ii+"');coloc_show();");
    //btn.setAttribute("style","width:100%");
    //var t=document.createTextNode("--");
    //btn.appendChild(t);
    btn.innerHTML="&uarr;";
    //btn.setAttribute("align","center");
    td.appendChild(btn);
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",descr);
    if (color !== "") {
	inp.setAttribute("style","width:100%;color:"+color);
    } else {
	inp.setAttribute("style","width:100%");
    }
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('obsConfigFile','"+target+"','descr',this.value);");
    inp.setAttribute("title","BUFR descriptor");
    td.appendChild(inp);
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",info);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('obsConfigFile','"+target+"','info',this.value);");
    inp.setAttribute("title","Information");
    td.appendChild(inp);
    row.appendChild(td);
    // make minimum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('obsConfigFile','"+target+"','min',this.value);");
    inp.setAttribute("title","Minimum target value");
    td.appendChild(inp);
    row.appendChild(td);
    // make maximum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","coloc_setConfigFilesTarget('obsConfigFile','"+target+"','max',this.value);");
    inp.setAttribute("title","Maximum target value");
    td.appendChild(inp);
    row.appendChild(td);
    // make "-" column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("title","Remove observation target");
    btn.setAttribute("onclick","removeObsTarget(this.parentNode.parentNode,'"+target+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
function coloc_showTargetMatchTable() {
    var item=document.getElementById('targetMatchTable');
    var file=coloc_getConfigFile();
    var mfile=coloc_getModelConfigFile();
    var tail=removeTableChildFromTo(item,"labelsTargetMatch","newlineTargetMatch");
    var targeto=coloc_config[file]["modelConfigFile"]["targeto"];
    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    // insert index match
    var cnt=0;
    if (model_config[mfile]!== undefined) {
	var indexTarget=model_config[mfile]["indexTarget"];
    } else {
	var indexTarget="";
    }
    var indexExp=coloc_config[file]["modelConfigFile"]["exp"];;
    coloc_insertTargetMatchRow(tail,cnt,indexTarget,indexExp);
    //for (var target in targets) {
    for (var ii =0; ii< targeto.length;ii++) {
	var target=targeto[ii];
	//console.log("TargetS:",target);
	cnt=cnt+1;
	var exp=(targets[target]["exp"]||"");
	coloc_insertTargetMatchRow(tail,cnt,target,exp);
    };
    for (var target of targeto) {
	//console.log("TargetO:",target);
    }
};
// create exec table row
function coloc_insertTargetMatchRow(item,cnt,target,expr) {
    var row = document.createElement("TR");
    var file = coloc_getConfigFile();
    if (coloc_config[file]!== undefined){
        var mfile = coloc_getModelConfigFile();
        var ofile = coloc_getObsConfigFile();
        var td, inp,div,itemId;
        // make model target column  ***************************
        td=document.createElement("TD");
	if (model_config[mfile]!== undefined) {
	    var indexTarget=model_config[mfile]["indexTarget"];
	} else {
	    var indexTarget="";
	}
	if (target == indexTarget) {
            td.setAttribute("style","color:darkorange");
	}
        td.innerHTML=target;
        row.appendChild(td);
        // make obs target expression column  ***************************
	itemId="matchExpression"+cnt;
        td=document.createElement("TD");
        td.setAttribute("class","fill");
        inp=document.createElement("INPUT");
	inp.setAttribute("id",itemId);
        inp.setAttribute("type","text");
        inp.setAttribute("value",expr);
        inp.setAttribute("style","width:100%");
	if (target == indexTarget) {
            inp.setAttribute("onblur","coloc_setConfig('modelConfigFile','exp',this.value);coloc_showTargetMatchTable();");
	} else {
            inp.setAttribute("onblur","coloc_setConfigFilesTarget('modelConfigFile','"+target+"','exp',this.value);coloc_showTargetMatchTable();");
	}
	inp.setAttribute("title","The <observation target> expression puts a contraint on the <model target>.\nNo constraint is placed on the <model target> if this expression is empty.\nThe user must provide enough constraints so that the system can figure out which\nmodel grid points to interpolate between when calculating the <model targets>.");
	div=document.createElement("DIV");
	div.setAttribute("id",itemId+"Dropdown");
	div.setAttribute("class","dropdown-content");
        td.appendChild(inp);
        td.appendChild(div);
        row.appendChild(td);
	// make select column ***************************************
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px");
	var btn=document.createElement("BUTTON");
	btn.setAttribute("title","Show available <observation targets> and functions");
	btn.setAttribute("onclick","showDropdown('"+itemId+"');");
	btn.setAttribute("class","dropbtn");
	btn.setAttribute("style","width:100%");
	var t=document.createTextNode("\u2630");
	btn.appendChild(t);
	td.appendChild(btn);
	row.appendChild(td);
        // make add row to table  ***************************
        item.parentNode.insertBefore(row,item);
        return row;
    }
}
// COLOC methods
function coloc_show() {
    var item;
    var file=coloc_getConfigFile();
    var mod=(coloc_config[file]["modelConfigFile"]["file"] !== "");
    var obs=(coloc_config[file]["obsConfigFile"]["file"] !== "");
    if (mod) {
	(document.getElementById("displayModelTargets")).setAttribute("style","");
	coloc_showModelTargetTable();
	coloc_showModelDefaultTable();
	(document.getElementById("displayColocModelFilter")).setAttribute("style","");
    } else {
	(document.getElementById("displayModelTargets")).setAttribute("style","display:none");
	(document.getElementById("displayModelDefault")).setAttribute("style","display:none");
	(document.getElementById("displayColocModelFilter")).setAttribute("style","display:none");
    };
    if (obs) {
	coloc_showObsTargetTable();
	(document.getElementById("displayObsTargets")).setAttribute("style","");
	(document.getElementById("displayColocObsFilter")).setAttribute("style","");
    } else {
	(document.getElementById("displayObsTargets")).setAttribute("style","display:none");
	(document.getElementById("displayColocObsFilter")).setAttribute("style","display:none");
    }
    if (mod && obs) {
	(document.getElementById("displayMatchRules")).setAttribute("style","");
	(document.getElementById("displayModelDefault")).setAttribute("style","display:none");
	coloc_showTargetMatchTable();
    }else {
	(document.getElementById("displayMatchRules")).setAttribute("style","display:none");
	if (mod) {
	    (document.getElementById("displayModelDefault")).setAttribute("style","");
	} else {
	    (document.getElementById("displayModelDefault")).setAttribute("style","display:none");
	}
    }
    showValue('colocConfigFile',file);
    showValue('colocConfigFileSave',file);
    showValue('colocModelConfigFile',coloc_config[file]["modelConfigFile"]["file"]);
    showValue('colocObsConfigFile',coloc_config[file]["obsConfigFile"]["file"]);
    showValue('colocXML',coloc_config[file]["xml"]);
    showValue('colocModelFilter',coloc_config[file]["filter"]);
    showValue('colocObsFilter',coloc_config[file]["obsConfigFile"]["filter"]);
    coloc_showCOLOC();
}

function coloc_showCOLOC() {
    var file=coloc_getConfigFile();
    var host=coloc_config[file]["host"];
    var href="http://"+host+"/cgi-bin/fark_coloc.pl?colocFile="+file;
    // if (mod) {
    // 	href=href+"?modelFile="+coloc_config[file]["modelConfigFile"]["file"];
    // 	if (coloc_config[file]["modelConfigFile"]["min"]) {
    // 	    href=href+"?modelStart="+ coloc_config[file]["modelConfigFile"]["min"];
    // 	};
    // 	if (coloc_config[file]["modelConfigFile"]["max"]) {
    // 	    href=href+"?modelStop="+ coloc_config[file]["modelConfigFile"]["max"];
    // 	};
    // 	var targets=coloc_config[file]["modelConfigFile"]["targets"];
    // 	var modelTargets="";
    // 	for (var target in targets) {
    // 	    modelTargets=modelTargets+target+"~"+targets[target]["variable"]+"~"+targets[target]["min"]+"~"+targets[target]["max"]+"|";
    // 	}
    // 	href=href+"?modelTargets="+modelTargets;
    // 	if (! obs) {
    // 	    var modelDefs="";
    // 	    var defs=coloc_config[file]["modelConfigFile"]["def"];
    // 	    var len=defs.length;
    // 	    for (var ii=0;ii<len;ii++){
    // 		var first=true;
    // 		for (var target in defs[ii]["targets"]){
    // 		    if (first) {modelDefs=modelDefs+"[";first=false;}
    // 		    modelDefs=modelDefs+target+"~"+defs[ii]["targets"][target]+"|";
    // 		}
    // 	    };
    // 	    href=href+"?modelDefault="+modelDefs;
    // 	}
    // };
    // if (obs) {
    // 	href=href+"?obsFile="+coloc_config[file]["obsConfigFile"]["file"];
    // 	if (coloc_config[file]["obsConfigFile"]["min"]) {
    // 	    href=href+"?obsStart="+ coloc_config[file]["obsConfigFile"]["min"];
    // 	};
    // 	if (coloc_config[file]["obsConfigFile"]["max"]) {
    // 	    href=href+"?obsStop="+ coloc_config[file]["obsConfigFile"]["max"];
    // 	};
    // 	var targets=coloc_config[file]["obsConfigFile"]["targets"];
    // 	var obsTargets="";
    // 	for (var target in targets) {
    // 	    obsTargets=obsTargets+target+"~"+
    // 		targets[target]["pos"]+"~"+
    // 		targets[target]["descr"]+"~"+
    // 		targets[target]["min"]+"~"+
    // 		targets[target]["max"]+"|";
    // 	}
    // 	href=href+"?obsTargets="+obsTargets;
    // 	if (mod) {
    // 	    var targets=coloc_config[file]["modelConfigFile"]["targets"];
    // 	    var matchRules="";
    // 	    for (var target in targets) {
    // 		matchRules=matchRules+target+"~"+targets[target]["exp"]+"|";
    // 	    }
    // 	    href=href+"?matchRules="+matchRules;
    // 	}
    // };
    //document.getElementById("colocLink").innerHTML=href;
    //document.getElementById("colocLink").href=href;
    //document.getElementById("colocLink").target="_blank";
}

function coloc_removeModelTarget(item,target) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	var item=document.getElementById("newlineModelTarget");
	item.children[0].children[0].value=target;
	item.children[1].children[0].value=coloc_config[file]["modelConfigFile"]["targets"][target]["variable"];
	item.children[3].children[0].value=coloc_config[file]["modelConfigFile"]["targets"][target]["min"];
	item.children[4].children[0].value=coloc_config[file]["modelConfigFile"]["targets"][target]["max"];
	delete coloc_config[file]["modelConfigFile"]["targets"][target];
	if (obs_isEmpty(coloc_config[file]["modelConfigFile"]["targets"])) {
	    delete coloc_config[file]["modelConfigFile"]["def"];
	    coloc_config[file]["modelConfigFile"]["def"]=[];
	}
	//arr=coloc_removeByValue(arr,item1,item2...)
	coloc_config[file]["modelConfigFile"]["targeto"]=
	    coloc_removeByValue(coloc_config[file]["modelConfigFile"]["targeto"],target);
	//coloc_showModelTargetTable();
	//coloc_showModelDefaultTable();
	coloc_show();
    }
};
function removeObsTarget(item,target) {
    var file=coloc_getConfigFile();
    if (coloc_config[file] !== undefined) {
	var item=document.getElementById("newlineObsTarget");
	item.children[0].children[0].value=target;
	item.children[1].children[0].value=coloc_config[file]["obsConfigFile"]["targets"][target]["pos"];
	item.children[3].children[0].value=coloc_config[file]["obsConfigFile"]["targets"][target]["descr"];
	item.children[4].children[0].value=coloc_config[file]["obsConfigFile"]["targets"][target]["info"];
	item.children[5].children[0].value=coloc_config[file]["obsConfigFile"]["targets"][target]["min"];
	item.children[6].children[0].value=coloc_config[file]["obsConfigFile"]["targets"][target]["max"];
	delete coloc_config[file]["obsConfigFile"]["targets"][target];
	if (obs_isEmpty(coloc_config[file]["obsConfigFile"]["targets"])) {
	    delete coloc_config[file]["obsConfigFile"]["def"];
	    coloc_config[file]["obsConfigFile"]["def"]=[];
	}
	coloc_config[file]["obsConfigFile"]["targeto"]=
	    coloc_removeByValue(coloc_config[file]["obsConfigFile"]["targeto"],target);
	//coloc_showObsTargetTable();
	coloc_show();
    }
};


function coloc_saveConfigFile(target) {
    var file=coloc_getConfigFile();
    var password=document.getElementById("colocConfigFilePsw").value;
    var host ="";
    var xml = "";
    var modelFilter = "";
    var modelFile = "";
    var modelStart = "";
    var modelStop = "";
    var exp = "";
    var modelTargets = "";
    var modelTrgo="";
    var modelTrg="";
    var modelDefault = "";
    var indexExp="";
    var matchTrg={};
    if (coloc_config[file] != undefined && coloc_config[file]["modelConfigFile"] != undefined) {
	host = coloc_config[file]["host"];
	xml = coloc_config[file]["xml"];
	modelFilter = coloc_config[file]["filter"];
	modelFile = coloc_config[file]["modelConfigFile"]["file"];
	modelStart = coloc_config[file]["modelConfigFile"]["min"];
	modelStop = coloc_config[file]["modelConfigFile"]["max"];
	exp = coloc_config[file]["modelConfigFile"]["exp"];
	modelTargets = "";
	modelTrgo=coloc_config[file]["modelConfigFile"]["targeto"];
	modelTrg=coloc_config[file]["modelConfigFile"]["targets"];
	//for (var target in modelTrg) {
	for (var ii =0; ii< modelTrgo.length;ii++) {
	    var target=modelTrgo[ii];
	    modelTargets=modelTargets + "|" + target + "~" + 
		modelTrg[target]["variable"] + "~" + 
		modelTrg[target]["min"] + "~" + 
		modelTrg[target]["max"];
	};
	// model defaults
	var modelDef=coloc_config[file]["modelConfigFile"]["def"]//[];
	var len=modelDef.length;
	for (var ii=0; ii<len;ii++) {
	    var info=modelDef[ii]["info"];
	    var defTrg=modelDef[ii]["targets"];
	    var first=true;
	    for (var target in defTrg) {
		if (first) {
		    modelDefault=modelDefault + "[" + info;
		    first=false;
		};
		modelDefault=modelDefault + "|" + 
		    target + "~" + 
		    defTrg[target];
	    };
	};
	indexExp=coloc_config[file]["modelConfigFile"]["exp"]//"";
	matchTrg=coloc_config[file]["modelConfigFile"]["targets"];
    };
    var indexTarget="";
    if (model_config[modelFile] != undefined) {
	indexTarget=model_config[modelFile]["indexTarget"]//"";
    }
    var matchRules = "|" + indexTarget + "~" + indexExp;
    for (var target in matchTrg) {
	matchRules=matchRules + "|" + target + "~" + 
	    matchTrg[target]["exp"];
    };
    var obsFile = "";
    var obsFilter = "";
    var obsStart = "";
    var obsStop = "";
    var obsTargets = "";
    if (coloc_config[file] != undefined && coloc_config[file]["obsConfigFile"] != undefined) {
	obsFile = coloc_config[file]["obsConfigFile"]["file"]//"";
	obsFilter = coloc_config[file]["obsConfigFile"]["filter"]//"";
	obsStart = coloc_config[file]["obsConfigFile"]["min"]//"";
	obsStop = coloc_config[file]["obsConfigFile"]["max"]//"";
	var obsTrgo=coloc_config[file]["obsConfigFile"]["targeto"]//[];
	var obsTrg=coloc_config[file]["obsConfigFile"]["targets"]//{};
	//for (var target in obsTrg) {
	for (var ii =0; ii< obsTrgo.length;ii++) {
	    var target=obsTrgo[ii];
	    obsTargets=obsTargets + "|" + target + "~" + 
		obsTrg[target]["pos"] + "~" + 
		obsTrg[target]["descr"] + "~" + 
		obsTrg[target]["info"] + "~" + 
		obsTrg[target]["min"] + "~" + 
		obsTrg[target]["max"];
	};
    }
    documentLog.innerHTML="Sent coloc-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"coloc",
				  file:file,
				  host:host,
				  xml:xml,
				  filter:modelFilter,
				  password:password,
				  modelFile:modelFile,
				  modelStart:modelStart,
				  modelStop:modelStop,
				  indexExp:exp,
				  obsFile:obsFile,
				  obsFilter:obsFilter,
				  obsStart:obsStart,
				  obsStop:obsStop,
				  obsTargets:obsTargets,
				  modelTargets:modelTargets,
				  modelDefault:modelDefault,
				  matchRules:matchRules})
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
	    function (error) { alert("Coloc save request failed (system error)");}
	);
    makeUrl("coloc",file);
};
function coloc_showConfig() {
    var file=coloc_getConfigFile();
    if (coloc_config[file] === undefined) { // create new entry locally...
	coloc_config[file]={modelConfigFile:{targets:{},targeto:[],def:{}},
			    obsConfigFile:{targets:{},targeto:[]},
			    host:"fark.met.no",
			    password:""};
	coloc_config[file]["modelConfigFile"]["file"]=document.getElementById("colocModelConfigFile").value;
	coloc_config[file]["obsConfigFile"]["file"]=document.getElementById("colocObsConfigFile").value;
	// model targets
	var item=document.getElementById("modelTargetTable");
	var tbody=item.children[0];
	var children=tbody.children;
	var len=children.length;
	coloc_config[file]["modelConfigFile"]["targets"]={};
	for (var ii=len-1;ii>=0;ii--){
	    //console.log("RemoveTableChildFromTo ",ii,children[ii],len,children.length);
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsModelTarget" && att !== "newlineModelTarget") {
			var name=children[ii].children[0].innerHTML;
			var variable=children[ii].children[1].children[0].value;
			var min=children[ii].children[3].children[0].value;
			var max=children[ii].children[4].children[0].value;
			coloc_config[file]["modelConfigFile"]["targets"][name]=
			    {variable:variable,min:min,max:max};;
		    }
		}
	    }
	};
	// model default
	item=document.getElementById("modelDefaultTable");
	tbody=item.children[0];
	children=tbody.children;
	len=children.length;
	coloc_config[file]["modelConfigFile"]["def"]=[];
	for (var ii=len-1;ii>=0;ii--){
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsModelDefault" && att !== "newlineModelDefault") {
			var clen=children[ii].children.length;
			var info=children[ii].children[clen-1].children[0].value;
			var targets={};
			for (var jj=0;jj<clen-1;jj++) {
			    var trg=children[ii].children[jj].getAttribute("trg");
			    if (trg !== undefined) {
				var value=children[ii].children[jj].children[0].value;
				targets[trg]=value;
			    } else {
				console.log("Warning unknown target detected.");
			    }
			}
			coloc_config[file]["modelConfigFile"]["def"].push({targets:targets,info:info});
		    }
		}
	    }
	};
	// obs targets
	item=document.getElementById("obsTargetTable");
	tbody=item.children[0];
	children=tbody.children;
	len=children.length;
	coloc_config[file]["obsConfigFile"]["targets"]={};
	for (var ii=len-1;ii>=0;ii--){
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsObsTarget" && att !== "newlineObsTarget") {
			var name=children[ii].children[0].innerHTML;
			var bufrType=children[ii].children[1].children[0].value;
			var subType=children[ii].children[3].children[0].value;
			var pos=children[ii].children[5].children[0].value;
			var descr=children[ii].children[7].children[0].value;
			var min=children[ii].children[8].children[0].value;
			var max=children[ii].children[9].children[0].value;
			coloc_config[file]["obsConfigFile"]["targets"][name]=
			    {bufrType:bufrType,subType:subType,pos:pos,descr:descr,min:min,max:max};
		    }
		}
	    }
	};
	// match rules
	item=document.getElementById("targetMatchTable");
	tbody=item.children[0];
	children=tbody.children;
	len=children.length;
	for (var ii=len-1;ii>=0;ii--){
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined && expr !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsTargetMatch" && att !== "newlineTargetMatch") {
			var name=children[ii].children[1].innerHTML;
			var expr=children[ii].children[3].children[0].value;
			coloc_config[file]["modelConfigFile"]["targets"][name]=
			    {exp:expr};
		    }
		}
	    }
	};
	coloc_configEd++;
    } else { // load local values to screen
	showValue('colocModelConfigFile',coloc_config[file]["modelConfigFile"]["file"]);
	showValue('colocObsConfigFile',coloc_config[file]["obsConfigFile"]["file"]);
	coloc_show();
    }
};
function coloc_updateModelData(arg = "") {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent model-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"model",arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		modelLoaded=true;
		//console.log("Updating dropdown for ",target);
		coloc_show();
		fark_last['model']=coloc_getModelConfigFile();
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Coloc load model request failed (system error)");}
	);
};
function coloc_updateObsData(arg = "") {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent obs-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"obs",arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		obsLoaded=true;
		//console.log("Updating dropdown for ",target);
		coloc_show();
		fark_last['obs']=coloc_getObsConfigFile();
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Coloc load obs request failed (system error)");}
	);
};
function coloc_updateData() {
    var args=getArgs(coloc_getConfigFile());
    documentLog.innerHTML="Sent coloc-load request.";
    //console.log("coloc: *****loading  ",args);
    $.get("cgi-bin/fark_load.pl",{type:"coloc",arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		documentLog.innerHTML="Sent model-load request.";
		fark_last['model']=coloc_getModelConfigFile();
		args=getArgs(coloc_getModelConfigFile());
		//console.log("coloc: *****loading model ",args);
		$.get("cgi-bin/fark_load.pl",{type:"model",arg:args})
		    .success(function(data, status){
			dataToArray(data,status,documentLog);
			modelLoaded=true;
			fark_last['obs']=coloc_getObsConfigFile();
			args=getArgs(coloc_getObsConfigFile());
			//console.log("coloc: *****loading obs ",args);
			documentLog.innerHTML="Sent obs-load request.";
			$.get("cgi-bin/fark_load.pl",{type:"obs",arg:args})
			    .success(
				function(data, status){
				    dataToArray(data,status,documentLog);
				    obsLoaded=true;
				    coloc_show();
				    documentLog.innerHTML="";
				})
			    .error(
				function (error) { alert("Coloc obs request failed (system error)");}
			    );
		    })
		    .error(
			function (error) { alert("Coloc model request failed (system error)");}
		    );
	    })
	.error(
	    function (error) { alert("Coloc request failed (system error)");}
	);
};
function coloc_getModelIndexStart(inp,target) {
    var file=coloc_getModelConfigFile();
    var item=document.getElementById(inp);
    item.value=Number(model_config[file]["start"]).toString();
    coloc_setConfig('modelConfigFile','min',item.value);
};
function coloc_getModelIndexStop(inp,target) {
    var file=coloc_getModelConfigFile();
    var item=document.getElementById(inp);
    item.value=Number(model_config[file]["stop"]).toString();
    coloc_setConfig('modelConfigFile','max',item.value);
};
function coloc_getObsIndexStart(inp,target) {
    var file=coloc_getObsConfigFile();
    var item=document.getElementById(inp);
    //console.log("fark.js start:'"+file+"' '"+obs_config[file]["start"]+"'")
    item.value=Number(obs_config[file]["start"]).toString();
    coloc_setArrayPar('obsConfigFile','min',item.value);
};
function coloc_getObsIndexStop(inp,target) {
    var file=coloc_getObsConfigFile();
    var item=document.getElementById(inp);
    item.value=Number(obs_config[file]["stop"]).toString();
    coloc_setArrayPar('obsConfigFile','max',item.value);
};
function coloc_getObsTargetBufrType() {
    var item=document.getElementById("obsTargetTable");
    var newline=getChild(item,"newlineObsTarget");
    return newline.children[2].children[0].value;
};
function coloc_getObsTargetSubType() {
    var item=document.getElementById("obsTargetTable");
    var newline=getChild(item,"newlineObsTarget");
    return newline.children[4].children[0].value;
};
function coloc_debugExp(f,t) {
    var fitem=document.getElementById(f);
    var titem=document.getElementById(t);
    var expin=fitem.value;
    titem.innerHTML="";
    documentLog.innerHTML="Sent coloc-exp request:"+expin;
    $.get("cgi-bin/fark_exp.pl",{exp:expin})
	.success(
	    function(data, status){
		if (status === "success" && data !== null) {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to evaluate expression:"+expin+"\n"+msg);
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
	    function (error) { alert("Coloc exp request failed (system error)");}
	);
};

function coloc_mkdir(path) {
    var password=document.getElementById("colocConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"coloc",
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
	    function (error) { alert("Coloc mkdir request failed (system error)");}
	);
    
};

function coloc_rmdir(path) {
    var password=document.getElementById("colocConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"coloc",
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
		    documentLog.innerHTML="";
		}
	    })
	.error(
	    function (error) { alert("Coloc rmdir request failed (system error)");}
	);
};

function coloc_rmfile(path) {
    var password=document.getElementById("colocConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"coloc",
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
			// delete coloc_config[path];
			if (coloc_file == path) {coloc_file="default.cfg";}
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Coloc rmfile request failed (system error)");}
	);
    
};

function coloc_fgfile(path) { // clear file from internal memory
    if (coloc_config[path] != undefined) {
	delete coloc_config[path];
    }
};

function coloc_mkfile(file) {
    //console.log("Calling saveConfigFile: '"+file+"'");
    coloc_setConfigFile(file);
    coloc_saveConfigFile(file);
};


// arr=obs_removeByValue(arr,item1,item2...)
function coloc_removeByValue(arr) {
    var what, a = arguments, ll = a.length, ax;
    while (ll > 1 && arr.length) {
        what = a[--ll];
        while ((ax= arr.indexOf(what)) !== -1) {
            arr.splice(ax, 1);
        }
    }
    return arr;
};


function coloc_modelUp(ii) {
    var file=coloc_getConfigFile()
    if (coloc_config[file] !== undefined && 
	coloc_config[file]["modelConfigFile"] !== undefined && 
	coloc_config[file]["modelConfigFile"]["targeto"] !== undefined) {
	var targeto=coloc_config[file]["modelConfigFile"]["targeto"];
	arrayUp(targeto,ii);
    }
}

function coloc_obsUp(ii) {
    var file=coloc_getConfigFile()
    if (coloc_config[file] !== undefined && 
	coloc_config[file]["obsConfigFile"] !== undefined && 
	coloc_config[file]["obsConfigFile"]["targeto"] !== undefined) {
	var targeto=coloc_config[file]["obsConfigFile"]["targeto"];
	arrayUp(targeto,ii);
    }
};

function coloc_showConfigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent coloc-load request.";
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
			//console.log("Adding <up> button: '"+dd+"'");
			//addChildButton(item,"<up>","coloc_setConfigFile('"+dd+"');coloc_show();");
			addChildButton(item,"<up>","coloc_setConfigFile2('"+dd+"');","Change to parent <directory>");
			added=true;
		    }
		    if (args.length == 1) {
			//console.log("Arg ret:",ret);
			if (root["type"] == "dir" && root["loc"] != "") {
			    //console.log("Adding <rmdir> button: ",args[0]);
			    addChildButton(item,"<rmdir>","coloc_rmdir('"+args[0]+"');","Remove <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    //console.log("Adding <rmfile> button: ",args[0]);
			    addChildButton(item,"<rmfile>","coloc_rmfile('"+args[0]+"');","Remove <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			    if (looksLikeFile(args[0])) {
				//console.log("Adding <mkfile> button: ",args[0]);
				addChildButton(item,"<mkfile>","coloc_mkfile('"+args[0]+"');coloc_show();","Make <file>");
				if (coloc_config[args[0]] != undefined) {
				    addChildButton(item,"<fgfile>","coloc_fgfile('"+args[0]+"');","Forget <file>");
				}
				added=true;
			    } else {
				//console.log("Adding <mkdir> button: ",args[0]);
				addChildButton(item,"<mkdir>","coloc_mkdir('"+args[0]+"');","Make <directory>");
				added=true;
			    }
			}
		    } else if (args.length == 2) {
			if (root["type"] == "dir") {
			    //console.log("Adding <cpdir> button: ",args[0],args[1]);
			    addChildButton(item,"<cpdir>","coloc_cpdir('"+args[0]+"','"+args[1]+"');","Copy <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    //console.log("Adding <cpfile> button: ",args[0],args[1]);
			    addChildButton(item,"<cpfile>","coloc_cpfile('"+args[0]+"','"+args[1]+"');coloc_setConfigFile('"+args[2]+"');coloc_show();","Copy <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			}
		    };
		    //for (var coloc in coloc_config) {
		    //console.log("Adding config button: ",coloc);
		    //addChildButton(item,coloc,"coloc_setConfigFile('"+coloc+"');coloc_show();");
		    //added=true;
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
			//console.log("Adding button: ",dd);
			if (looksLikeFile(dd)) {
			    addChildButton(item,dd,"coloc_setConfigFile('"+dd+"');coloc_show();","Use <file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"coloc_setConfigFile('"+dd+"');coloc_show();","Change <directory>");
			    added=true;
			}
		    }
		    if (! added) {addChildText(item,"No data available...");}
		}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Coloc request failed (system error)");}
	);
    // documentLog.innerHTML="Sent coloc-load request.";
    // $.get("cgi-bin/fark_load.pl",{type:"coloc",arg:args}).success(function(data, status){
    //     dataToArray(data,status,documentLog);
    //     //console.log("Updating dropdown for ",target);
    //     removeChildren(item);
    //     var added=false;
    //     for (var coloc in coloc_config) {
    // 	addChildButton(item,coloc,"coloc_setConfigFile('"+coloc+"');coloc_show();");
    //      added=true;
    //     }
    //     documentLog.innerHTML="";
    // }).error(function (error) { alert("Request failed (system error)");});
};

function coloc_showModelConfigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent model-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"model",arg:args})
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
			addChildButton(item,"<up>","coloc_setConfig('modelConfigFile','file','"+dd+"');coloc_show();","Change to parent <directory");
			added=true;
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
			//console.log("Adding dir button: ",dd);
			if (looksLikeFile(dd)) {
			    addChildButton(item,dd,"coloc_setConfig('modelConfigFile','file','"+dd+"');coloc_updateModelData('"+dd+"');coloc_show();","Use <model setup file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"coloc_setConfig('modelConfigFile','file','"+dd+"');coloc_updateModelData('"+dd+"');coloc_show();","Change <directory>");
			    added=true;
			}
			added=true;
		    }
		    // for (var model in model_config) {
		    // 	addChildButton(item,model,"coloc_setConfig('modelConfigFile','file','"+model+"');coloc_show();");
		    //      added=true;
		    // }
		    addChildButton(item,"<none>","coloc_setConfig('modelConfigFile','file','');coloc_show();","Do not use model data");
		    added=true;
		    if (! added) {addChildText(item,"No data available...");}
		}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Model request failed (system error)");}
	);
};

function coloc_showModelTargetVariable(item,target,arg) {
    var file=coloc_getModelConfigFile();
    removeChildren(item);
    var added=false;
    if (model_config[file] !== undefined) {
	for (var dim in model_config[file]["dimensions"]) {
	    var dimname=dim;
	    var dimv=model_config[file]["dimensions"][dim];
	    if (dimv!= null) {dimname="("+dimname+") 1:"+dimv;};
	    addChildButtonShaded(item,dimname,"showValue('colocModelTargetVariable','("+dim+")');","Use <model dimension>");
	    added=true;
	}
	for (var variable in model_config[file]["variables"]) {
	    var fullname=variable;
	    var dims=model_config[file]["variables"][variable];
	    if (dims!= null) {fullname=fullname+"("+dims+")";};
	    addChildButton(item,fullname,"showValue('colocModelTargetVariable','"+variable+"');","Use <model variable>");
	    added=true;
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};

function coloc_showObsConcigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent obs-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"obs",arg:args})
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
			addChildButton(item,"<up>","coloc_setConfig('obsConfigFile','file','"+dd+"');coloc_show();","Change to parent <directory>");
			added=true;
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
			//console.log("Adding dir button: ",dd);
			if (looksLikeFile(dd)) {
			    addChildButton(item,dd,"coloc_setConfig('obsConfigFile','file','"+dd+"');coloc_updateObsData('"+dd+"');coloc_show();","Use <file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"coloc_setConfig('obsConfigFile','file','"+dd+"');coloc_updateObsData('"+dd+"');coloc_show();","Change <directory>");
			    added=true;
			}
		    }
		    // for (var obs in obs_config) {
		    // 	addChildButton(item,obs,"coloc_setConfig('obsConfigFile','file','"+obs+"');coloc_show();");
		    //      added=true;
		    // }
		    addChildButton(item,"<none>","coloc_setConfig('obsConfigFile','file','');coloc_show();","Do not use observation data");
		    added=true;
		    if (! added) {addChildText(item,"No data available...");}
		}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Obs request failed (system error)");}
	);
};

function coloc_showXml(item,target,arg) {
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
					       "coloc_setArray('xml','"+dd+"');coloc_show();","Change to parent <directory>");
				added=true;
			    };
			    var dirs=ls[0].getElementsByTagName("dir");
			    //console.log("Found dir entries: ",dirs.length);
			    for (var ii=0; ii< dirs.length; ii++) {
				var dd = dirs[ii].getAttribute("path");
				//console.log("Adding dir button: ",dd);
				addChildButton(item,dd,"coloc_setArray('xml','"+dd+"');coloc_show();","Change <directory>");
				added=true;
			    };
			    var patts=ls[0].getElementsByTagName("pattern");
			    //console.log("Found file entries: ",patts.length);
			    for (var ii=0; ii< patts.length; ii++) {
				var rr = getFile(patts[ii].getAttribute("regexp"));
				var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
				if (dd !== '') {
				    //console.log("Adding file button: ",dd,rr);
				    addChildButtonShaded(item,dd,"coloc_setArray('xml','"+rr+"');coloc_show();","Use <pattern>");
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
				    addChildButton(item,size+" "+dd,"coloc_setArray('xml','"+dd+"');coloc_show();","Use <file>");
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
	    function (error) { alert("Coloc XML request failed (system error)");}
	);
};

function coloc_showObsPos(item,target,arg) {
    var file=coloc_getObsConfigFile();
    var mfile=coloc_getModelConfigFile();
    if ( obs_config[file] !== undefined) {
	var bufrType = obs_config[file]["bufrType"];
	var subType = obs_config[file]["subType"];
	removeChildren(item);
	var added=false;
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
		    var info=" "+bufr[pos]["info"];
		    if (bufr[pos]["val1"] !== undefined && bufr[pos]["val1"] != "NA") {
			var value="    ~ "+bufr[pos]["val1"];
		    } else {
			var value="";
		    };
		    if (descr == "31001") {
			addChildButtonShaded(item,pos+" : "+descr + info + value,"showValue('colocObsPOS','"+pos+"');showValue('colocObsDESCR','"+descr+"');showValue('colocObsInfo','"+info+"');","use <BUFR delayed replicator>");
			added=true;
		    } else {
			addChildButton(item,pos+" : "+descr + info + value,"showValue('colocObsPOS','"+pos+"');showValue('colocObsDESCR','"+descr+"');showValue('colocObsInfo','"+info+"');","Use <BUFR sequence element>");
			added=true;
		    }
		}
	    }
	    // add dimensions...
 	    if (model_config[mfile] !== undefined) {
		for (var dim in model_config[mfile]["dimensions"]) {
		    var dimname=dim;
		    var dimv=model_config[mfile]["dimensions"][dim];
		    if (dimv !=  null) {
			addChildButtonShaded(item,dimname + "-duplicator (1:"+dimv+")","showValue('colocObsPOS','');showValue('colocObsDESCR','');"+
					     "showValue('colocObsInfo','"+dimname+"-duplicator (1:"+dimv+")');showValue('colocObsMin','1');showValue('colocObsMax','"+dimname+"');","Duplicate observation");
			added=true;
		    }
		}
	    }
	}
	// add internal variables...
	addChildButton(item," mid: Model file index position","showValue('colocObsPOS','mid');showValue('colocObsDESCR','');showValue('colocObsInfo','Model file index position');",
		       "Model file identification (internal variable)");
	addChildButton(item," oid: Observation file index position","showValue('colocObsPOS','oid');showValue('colocObsDESCR','');showValue('colocObsInfo','Observation file index position');",
		       "Observation file identification (internal variable)");
	addChildButton(item," bid: BUFR message number in observation file","showValue('colocObsPOS','bid');showValue('colocObsDESCR','');showValue('colocObsInfo','BUFR message number in observation file');",
		       "BUFR message identification (internal variable)");
	addChildButton(item," sid: Observation number in BUFR message","showValue('colocObsPOS','sid');showValue('colocObsDESCR','');showValue('colocObsInfo','Observation number in BUFR message');",
		       "Observation identification  (internal variable)");
	addChildButton(item," lid: Location number in BUFR message","showValue('colocObsPOS','lid');showValue('colocObsDESCR','');showValue('colocObsInfo','Location number in BUFR message');",
		       "Location identification (internal variabe)");
	addChildButton(item," rid: rerun variable.","showValue('colocObsPOS','rid');showValue('colocObsDESCR','');showValue('colocObsInfo','Rerun variable.');",
		       "Location identification (internal variabe)");
	added=true;
	if (! added) {addChildText(item,"No data available...");}
    }
};

function coloc_showMatchTarget(item,target,arg) {
    var file=coloc_getConfigFile();
    removeChildren(item);
    var added=false;
    if ( coloc_config[file] !== undefined &&
	 coloc_config[file]["modelConfigFile"]["targets"] !== undefined 
       ) {
	for (var t in coloc_config[file]["modelConfigFile"]["targets"]) {
	    addChildButton(item,t,"showValue('matchModelTargetName','"+t+"');","<model target>");
	    added=true;
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};

function coloc_showMatchExpression(item,target,arg) {
    var cnt = target.substring(15);
    var file = coloc_getConfigFile();
    var mfile = coloc_getModelConfigFile();
    var ofile = coloc_getObsConfigFile();
    //console.log("mfile:" + mfile);
    removeChildren(item);
    var added=false;
    if ( coloc_config[file] !== undefined &&
	 model_config[mfile] !== undefined &&
	 obs_config[ofile] !== undefined
       ) {
	if (cnt == 0) {
	    var indexTrg=obs_config[ofile]["indexTarget"];
	    addChildButtonShaded(item,indexTrg,"addValue('"+target+"','"+indexTrg+"');coloc_addConfig('modelConfigFile','exp','"+indexTrg+"');","Observation index target (see observation index)");
	    added=true;
	} else {
	    var trg=coloc_config[file]["modelConfigFile"]["targeto"][cnt-1];
	    for (var t in coloc_config[file]["obsConfigFile"]["targets"]) {
		addChildButton(item,t,"addValue('"+target+"','"+t+"');coloc_addConfigFilesTarget('modelConfigFile','"+trg+"','exp','"+t+"');","Model target");
		added=true;
	    }
	}
    }
    addFunctionButtons(item,target);
    added=true;
    if (! added) {addChildText(item,"No data available...");}
};

function coloc_showDebugExpression(item,target,arg) {
    removeChildren(item);
    var added=false;
    addLogicalButtons(item,target);
    addFunctionButtons(item,target);
    added=true;
    if (! added) {addChildText(item,"No data available...");}
};

function coloc_showObsFilter(item,target,arg) {
    var file = coloc_getConfigFile();
    var mfile = coloc_getModelConfigFile();
    var ofile = coloc_getObsConfigFile();
    removeChildren(item);
    var added=false;
    if ( obs_config[ofile] !== undefined
       ) {
	for (var t in obs_config[ofile]["targets"]) {
	    addTargetButton(item,target,t,"Observation target (see observation index)");
	}
	addTargetButtonShaded(item,target,obs_config[ofile]["indexTarget"],"Observation index target (see observation index)");
    };
    if ( coloc_config[file] !== undefined ) {
	for (var t in coloc_config[file]["obsConfigFile"]["targets"]) {
	    addTargetButton(item,target,t,"Observation target");
	}
    };
    addLogicalButtons(item,target);
    addFunctionButtons(item,target);
    added=true;
    if (! added) {addChildText(item,"No data available...");}
};

function coloc_showModelFilter(item,target,arg) {
    var file = coloc_getConfigFile();
    var mfile = coloc_getModelConfigFile();
    var ofile = coloc_getObsConfigFile();
    var mod=(mfile !== "");
    var obs=(ofile !== "");
    removeChildren(item);
    var added=false;
    if(mod) {
	if ( model_config[mfile] !== undefined ) {
	    addTargetButtonShaded(item,target,model_config[mfile]["indexTarget"],"model index target (see model index)");
	}
	if ( coloc_config[file] !== undefined ) {
	    for (var t in coloc_config[file]["modelConfigFile"]["targets"]) {
		addChildButton(item,t,"addValue('"+target+"','"+t+"');","Model target");
		added=true;
	    }
	}
    }
    if (obs) {
	if ( obs_config[ofile] !== undefined) {
	    for (var t in obs_config[ofile]["targets"]) {
		addTargetButton(item,target,t,"observation target (see observation index)");
	    }
	    addTargetButtonShaded(item,target,obs_config[ofile]["indexTarget"],"observation index target (see observation index)");
	};
	if ( coloc_config[file] !== undefined ) {
	    for (var t in coloc_config[file]["obsConfigFile"]["targets"]) {
		addChildButton(item,t,"addValue('"+target+"','"+t+"');","Observation target");
		added=true;
	    }
	};
    };
    addLogicalButtons(item,target);
    addFunctionButtons(item,target);
    added=true;
    if (! added) {addChildText(item,"No data available...");}
};
#__file: 'js/join.js' 0100644    **DO NOT DELETE**
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
    var joinCols="";
    var joinColMin="";
    var joinColMax="";
    var joinAttrs="";
    if (join_config[file] != undefined) {
	filterDir=join_config[file]["filterDir"]//"";
	filterDirMin=join_config[file]["filterDirMin"]//"";
	filterDirMax=join_config[file]["filterDirMax"]//"";
	filterFile=join_config[file]["filterFile"]//"";
	hits=join_config[file]["hits"]//"";
	cat=join_config[file]["cat"]//"";
	table=join_config[file]["table"]//"";
	graphics=join_config[file]["graphics"]//"";
	if (join_cats[cat] != undefined) {
	    var colnames_=join_cats[cat]["colnames_"]//[];
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
	btn.setAttribute("onclick","showDropdown('"+itemId+"').value)");
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
			    var sage=parseFloat(join_config[file]["files"][ii][1]).toFixed(2);
			    var ssize=join_config[file]["files"][ii][2];
			    addChildButton(item,ssize+" "+sfile+" ("+sage+"d)","join_fileFind('"+sfile+"');","Scan <join file>");
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
#__file: 'js/metfark.js' 0100644    **DO NOT DELETE**
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
		   "rerun"  : {}
		 };

var clss = ["model","obs","coloc","table","join","plot","rerun"];


// initialisation function
function load(){
    $.ajaxSetup({timeout:0}); // never timeout a request (and re-send it)...
    var types=["model","obs","coloc","table","join","plot","rerun","exec"];
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
    var types=["model","obs","coloc","table","join","plot","rerun","exec"];
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
	exec_updateData();
    } else if (type === "obs") {
	model_updateData();
	obs_updateData();
	coloc_updateData();
	table_updateData();
	join_updateData();
	plot_updateData();
	rerun_updateData();
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
	exec_updateData();
    } else if (type === "table") {
	table_updateData();
	join_updateData();
	plot_updateData();
	coloc_updateData();
	model_updateData();
	obs_updateData();
	rerun_updateData();
	exec_updateData();
    } else if (type === "join") {
	table_updateData();
	plot_updateData();
	join_updateData();
	coloc_updateData();
	model_updateData();
	obs_updateData();
	rerun_updateData();
	exec_updateData();
    } else if (type === "plot") {
	plot_updateData();
	table_updateData();
	join_updateData();
	coloc_updateData();
	model_updateData();
	obs_updateData();
	rerun_updateData();
	exec_updateData();
    } else if (type === "rerun") {
	exec_updateData();
	model_updateData();
	obs_updateData();
	coloc_updateData();
	table_updateData();
	join_updateData();
	plot_updateData();
	rerun_updateData();
    } else if (type === "exec") {
	exec_updateData();
	model_updateData();
	obs_updateData();
	coloc_updateData();
	table_updateData();
	join_updateData();
	plot_updateData();
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

function addLogicalButtons( item, target) {
    addChildButton(item,"msgmax(var(:))","addValue('"+target+"','msgmax()');","Maximum value of target in BUFR message (function)");
    addChildButton(item,"msgmin(var(:))","addValue('"+target+"','msgmin()');","Minimum value of target in BUFR message (function)");
    addChildButton(item,"msgclosest(var(:),trg1,trg2...)","addValue('"+target+"','msgclosest(,,)');","Value of target in BUFR message closest to (trg1,trg2...) (function)");
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
    } else if (target === 'execType') {
	exec_showType(item,target,arg);
    } else if (target === 'execConfigFile') {
	exec_showConfigFile(item,target,arg);
    } else if (target === 'execCron') {
	exec_showCron(item,target,arg);
    } else if (target === 'cleanFilterDir') {
	clean_showFilterDir(item,target,arg);
    } else if (target === 'cleanFilterFile') {
	clean_showFilterFile(item,target,arg);
    } else if (target === 'cleanCron') {
	clean_showFilterFile(item,target,arg);
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
    document.getElementById(target).innerHTML=value;
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
	var cat=tables[ii].getAttribute("cat");
	table_config[path]={dataset:{}, attributes:{},table:table,graphics:graphics,cat:cat};
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
	var cat=joins[ii].getAttribute("cat");
	join_config[path]={dataset:{}, attributes:{},table:table,graphics:graphics,cat:cat,min:{},max:{}};
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
	var attrs=joins[ii].getElementsByTagName("attribute");
	for (var jj = 0; jj < attrs.length; jj++) {
	    var name=attrs[jj].getAttribute("name");
	    var value=attrs[jj].getAttribute("value");
	    join_config[path]["attributes"][name]=value;
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
	var plotes=execs[ii].getElementsByTagName("rerun");
	for (var jj = 0; jj < plotes.length; jj++) {
	    var plot=plotes[jj].getAttribute("file");
	    var last=plotes[jj].getAttribute("last") ||"";
	    var info=plotes[jj].getAttribute("info") ||"";
	    var exec=plotes[jj].getAttribute("exec")  || exec_cron[0];
	    if (! isInArray(exec,exec_cron)) {exec=exec_cron[0];};
	    var status=plotes[jj].getAttribute("status") ||"";
	    exec_config["rerun"][plot]={last:last,info:info,exec:exec,status:status};
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
	rerun_config[path]["plot"]={};
	var plotes=reruns[ii].getElementsByTagName("plot");
	for (var jj = 0; jj < plotes.length; jj++) {
	    var plot=plotes[jj].getAttribute("file");
	    var last=plotes[jj].getAttribute("last") ||"";
	    var info=plotes[jj].getAttribute("info") ||"";
	    var status=plotes[jj].getAttribute("status") ||"";
	    rerun_config[path]["plot"][plot]={last:last,info:info,status:status};
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
#__file: 'js/model.js' 0100644    **DO NOT DELETE**
model_file="default.cfg";
model_config = { "default.cfg" : {filterDir: "/opdata",
				  filterDirStat: "",
				  filterDirMin: "",
				  filterDirMax: "",
				  filterFile: ".*\.nc",
				  hits : "?",
				  indexTarget : "",
				  indexVariable : "",
				  min:"",
				  max:"",
				  variables : {def:""},
				  dims : {},
				  sizes : {},
				  dimensions : {def:1},
				  files : [],
				  stack : "",
				  password: ""
				 }
	       };
model_configEd = 0;


function model_allocate(file) {
    if (model_config[file] === undefined) {
	model_config[file]=clone(model_config[model_file]);
	//console.log("cloned:",file,model_file,model_config[file]);
    }
}

// make new obs-filter entry
function model_setConfigFile(file) {
    showValue('modelConfigFileSave',file);
    showValue('modelConfigFile',file);
    //if (file != "") {
    model_allocate(file);
    model_file=file;
    //};
}
function model_getConfigFile() {
    return model_file;
};
function model_setArray(parameter,value) {
    var file=model_getConfigFile();
   //console.log("File:",file,parameter,model_config[file],value);
    model_config[file][parameter]=decodeURI(value);
};
//check if directory exists...
function model_setFilterDir(value) {
    var file=model_getConfigFile();
   //console.log("File:",file,"filterDir",model_config[file],value);
    var val=decodeURI(value);
    model_config[file]["filterDir"]=val;
    $.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:"data",path:val})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length == 0 ) {
		    document.getElementById('modelFilterDir').style.color='black'
		    model_config[file]["filterDirStat"]="";
		   //console.log("Dir ok:",val);
		} else {
		    model_config[file]["filterDirStat"]=val;
		    document.getElementById('modelFilterDir').style.color='red'
		    console.log("Dir NOT ok:",val);
		}
		model_show();
	    })
	.error(
	    function (error) { alert("Model filter dir request failed (system error)");}
	);
};
function model_show() {
    var file=model_getConfigFile();
    if (file != "" && model_config[file] != undefined) {
	model_allocate(file);
	showValue('modelConfigFile',file);
	showValue('modelConfigFileSave',file);
	showValue('modelFilterDir',model_config[file]["filterDir"]);
	showValue('modelFilterDirMin',model_config[file]["filterDirMin"]);
	showValue('modelFilterDirMax',model_config[file]["filterDirMax"]);
	showValue('modelFilterFile',model_config[file]["filterFile"]);
	showValue('modelIndexTarget',model_config[file]["indexTarget"]);
	showValue('modelIndexVariable',model_config[file]["indexVariable"]);
	model_checkVariable(document.getElementById("modelIndexVariable"));
	setInnerHTML('modelPatternHits',model_config[file]["hits"]);
	// this may seem strange, Stat stores name of dir only if it does not exist...
	if (model_config[file]["filterDirStat"]==model_config[file]["filterDir"]) {
	    document.getElementById('modelFilterDir').style.color='red'
	    console.log("Directory does not exist:",model_config[file]["filterDir"]);
	} else {
	    document.getElementById('modelFilterDir').style.color='black'
	}
    }
};
// model check variable
function model_checkVariable(item) {
    var file=model_getConfigFile();
    var variable=item.value;
    var color="green";
    var variables=model_config[file]["variables"];
    if (variables !== undefined) {
	if (variables[variable] === undefined) {
	    color="red";
	};
    };
    item.setAttribute("style","color:"+color);
}
// model config methods
function model_checkPassword() {
    var password=document.getElementById("modelConfigFilePsw").value;
    var file=model_getConfigFile();
    if (model_config[file] !== undefined) {
	if (model_config[file]["password"] !== undefined) {
	    if (model_config[file]["password"] !== password) {
		alert("Invalid password used when attempting to save file:\n"+file);
		return false;
	    }
	}
    };
    return true;
}
function model_saveConfigFile() {
    var file=model_getConfigFile();
    var password=document.getElementById("modelConfigFilePsw").value;
    // send to server...
    var filterDir="";
    var filterDirMin="";
    var filterDirMax="";
    var filterFile="";
    var hits="";
    var indexTarget="";
    var indexVariable="";
    var stack="";
    var variables="";
    var dims="";
    if (model_config[file]!= undefined) {
	filterDir=model_config[file]["filterDir"]//"";
	filterDirMin=model_config[file]["filterDirMin"]//"";
	filterDirMax=model_config[file]["filterDirMax"]//"";
	filterFile=model_config[file]["filterFile"]//"";
	hits=model_config[file]["hits"]//"";
	indexTarget=model_config[file]["indexTarget"]//"";
	indexVariable=model_config[file]["indexVariable"]//"";
	var sfile=model_config[file]["stack"]//"";
	if (sfile !== "") {
	    stack=stack+"|"+sfile;
	};
	if (model_config[file]["variables"] != undefined) {
	    for (var variable in model_config[file]["variables"]) {
		var dims=model_config[file]["variables"][variable]//"";
		variables=variables+"|"+variable+"~"+dims;
	    };
	};
	if (model_config[file]["dimensions"] != undefined) {
	    for (var dim in model_config[file]["dimensions"]) {
		var dimv=model_config[file]["dimensions"][dim]//"";
		dims=dims+"|"+dim+"~"+dimv;
	    };
	};
    } else {
	console.log("Warning: setup is not stored properly.");
    }
    //console.log("Variables:",variables," dimensions:",dims);
    documentLog.innerHTML="Sent model-save request.";
    $.get("cgi-bin/fark_save.pl",
	  {type:"model",
	   file:file,
	   password:password,
	   filterDir:filterDir,
	   filterDirMin:filterDirMin,
	   filterDirMax:filterDirMax,
	   filterFile:filterFile,
	   hits:hits,
	   indexTarget:indexTarget,
	   indexVariable:indexVariable,
	   stack:stack,
	   variables:variables,
	   dimensions:dims
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
	    function (error) { alert("Model save request failed (system error)");}
	);
    makeUrl("model",file);
};
function model_updateData(arg=model_getConfigFile()) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent model-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"model",arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		modelLoaded=true;
		//console.log("Updating dropdown for ",target);
		model_show();
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Model request failed (system error)");}
	);
};
function model_fileFind(sfile) {
    var file=model_getConfigFile();
    model_config[file]["stack"]=sfile;
    var password=document.getElementById("modelConfigFilePsw").value;
    documentLog.innerHTML="Sent model-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"modelfile",
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
			model_show();
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Model find request failed (system error)");}
	);
};

function model_mkdir(path) {
    var password=document.getElementById("modelConfigFilePsw").value;
    documentLog.innerHTML="Sent mkdir request.";
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"model",
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
	    function (error) { alert("Model mkdir request failed (system error)");}
	);
    
};

function model_rmdir(path) {
    var password=document.getElementById("modelConfigFilePsw").value;
    documentLog.innerHTML="Sent rmdir request.";
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"model",
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
	    function (error) { alert("Model rmdir request failed (system error)");}
	);
    
};

function model_rmfile(path) {
    var password=document.getElementById("modelConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"model",
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
			//delete model_config[path];
			if (model_file == path) {model_file="default.cfg";}
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Model rmfile request failed (system error)");}
	);
    
};

function model_mkfile(file) {
   //console.log("Calling saveConfigFile: '"+file+"'");
    model_setConfigFile(file);
    model_saveConfigFile(file);
};

function model_fgfile(path) { // clear file from internal memory
    if (model_config[path] != undefined) {
	delete model_config[path];
    }
};

function model_showConfigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent model-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"model",arg:args})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+arg+"'\n"+msg);
		} else {
		    //console.log("Updating dropdown for ",target,JSON.stringify(data));
		    var ret=dataToArray(data,status,documentLog);
		    var root=ret[0]||{};
		    //console.log("Got data ",target,JSON.stringify(root));
		    removeChildren(item);
		    var added=false;
		    if (args.length >0 && looksLikeFile(args[0])) {
			var file=getFile(args[0]);
		    } else {
			var file="";
		    };
		    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
		    //console.log("Found entries: ",dirs.length-1,root);
		    var parent=dirs[0];
		    if (parent != null) {
			var dd=parent;
			addChildButton(item,"<up>","model_setConfigFile('"+dd+"');","Change to parent <directory>");
			added=true;
		    }
		    if (args.length == 1) {
			//console.log("Arg ret:",ret);
			if (root["type"] == "dir" && root["loc"] != "") {
			    addChildButton(item,"<rmdir>","model_rmdir('"+args[0]+"');","Remove <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<rmfile>","model_rmfile('"+args[0]+"');","Remove <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			    if (looksLikeFile(args[0])) {
				addChildButton(item,"<mkfile>","model_mkfile('"+args[0]+"');model_show();","Make <file>");
				if (model_config[args[0]] != undefined) {
				    addChildButton(item,"<fgfile>","model_fgfile('"+args[0]+"');","Forget <file>");
				}
				added=true;
			    } else {
				addChildButton(item,"<mkdir>","model_mkdir('"+args[0]+"');","Make <directory>");
				added=true;
			    }
			}
		    } else if (args.length == 2) {
			if (root["type"] == "dir") {
			    addChildButton(item,"<cpdir>","model_cpdir('"+args[0]+"','"+args[1]+"');","Copy <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<cpfile>","model_cpfile('"+args[0]+"','"+args[1]+"');model_setConfigFile('"+args[2]+"');model_show();","Copy <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			}
		    };
		    //for (var model in model_config) {
		    //console.log("Adding config button: ",model);
		    //addChildButton(item,model,"model_setConfigFile('"+model+"');model_show();");
		    // added=true;
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
			//console.log("Adding dir button: ",dd,ii,dirs[ii]);
			if (looksLikeFile(dd)) {
			    addChildButton(item,dd,"model_setConfigFile('"+dd+"');model_show();","Use <file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"model_setConfigFile('"+dd+"');model_show();","Change <directory>");
			    added=true;
			}
		    };
		    if (! added) {addChildText(item,"No data available...");}
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Dropdown model request failed (system error)");}
	);
};

function model_showFilterDir(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent model-load request.";
    var file=model_getConfigFile();
    var path=args[0] || "";
    var cls = "data";
    var filter=model_config[file]["filterFile"];
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
					   "model_setArray('filterDir','"+dd+"');model_show();","Change to parent <directory>");
			    added=true;
			};
			var dirs=ls[0].getElementsByTagName("dir");
			//console.log("Found dir entries: ",dirs.length);
			for (var ii=0; ii< dirs.length; ii++) {
			    var dd = dirs[ii].getAttribute("path");
			    //console.log("Adding dir button: ",dd);
			    if (looksLikeFile(dd)) {
				addChildButton(item,dd,"model_setArray('filterDir','"+dd+"');model_show();","Use <file>");
				added=true;
			    } else {
				addChildButton(item,dd,"model_setArray('filterDir','"+dd+"');model_show();","Change <directory>");
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
				addChildButtonShaded(item,dd,"model_setArray('filterFile','"+rr+"');model_show();","Copy <pattern> to filter");
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
				addChildButton(item,size+" "+dd,"model_setArray('filterFile','"+dd+"');model_show();","Copy <file name> to filter");
				added=true;
			    };
			};
		    };
		};
		if (! added) {addChildText(item,"No data available...");}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Model dir filter request failed (system error)");}
	);
};

function model_showFilterFile(item,target,arg) {
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
			setInnerHTML('modelPatternHits',model_config[file]["hits"]);
			removeChildren(item);
			var added=false;
			var len=model_config[file]["files"].length;
			for (var ii=0; ii<len;ii++) {
			    var sfile=model_config[file]["files"][ii][0];
			    var sage=parseFloat(model_config[file]["files"][ii][1]).toFixed(2);
			    var ssize=model_config[file]["files"][ii][2];
			    addChildButton(item,ssize+" "+sfile+" ("+sage+"d)","model_fileFind('"+sfile+"');","Scan <model file>");
			    added=true;
			}
			if (! added) {addChildText(item,"No data available...");}
		    };
		    documentLog.innerHTML="";
		}
	    })
	.error(
	    function (error) { alert("Model file filter request failed (system error)");}
	);
};

function model_showIndex(item,target,arg) {
    var file=model_getConfigFile();
    removeChildren(item);
    var added=false;
    if (model_config[file] !== undefined) {
	var variables=model_config[file]["variables"];
	if (variables !== undefined) {
	    for (var variable in variables) {
		var fullname=variable;
		var dims=model_config[file]["variables"][variable];
		var size=model_config[file]["sizes"][variable]||1;
		if (dims != null) {fullname=fullname+"("+dims+")";};
		console.log("Index:",fullname,size);
		if (size < 1000) {
		    addChildButton(item,fullname,"model_setArray('indexVariable','"+variable+"');model_show();","Select <model variable>");
		    added=true;
		}
	    }
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};
#__file: 'js/obs.js' 0100644    **DO NOT DELETE**
obs_file = "default.cfg";
obs_config = { "default.cfg" : { filterDir : "/home/www/bufr/",
				 filterDirStat : "",
				 filterDirMin: "",
				 filterDirMax: "",
				 filterFile : ".*\.bufr",
				 tablePath : "/home/www/fark-perl_0.15/bufrtables/",
				 hits : "?",
				 bufr : { 99 : { info : "default", 9 : { 1 :{descr:99999, info: "default info"},
									 info : "more default info"
								       }
					       }
					},
				 bufrType : "99",
				 subType : "9",
				 typeInfo : "default info.",
				 targets : {"yy" : {pos:"", descr:"", info:""}},
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
	//console.log("cloned:",file,obs_file,obs_config[file]);
    }
}
function obs_setConfigFile(file) {
    showValue('obsConfigFile',file);
    showValue('obsConfigFileSave',file);
    //if (file != "") {
    obs_allocate(file);
    obs_file=file;
    //};
}
function obs_getConfigFile() {
    return obs_file;
};
function obs_setArray(parameter,value) {
    var file=obs_getConfigFile();
    //console.log("File:",file,parameter,obs_config[file]);
    obs_config[file][parameter]=decodeURI(value);
};
function obs_setFilterDir(value) {
    var file=obs_getConfigFile();
    //console.log("File:",file,"filterDir",obs_config[file]);
    var val=decodeURI(value);
    obs_config[file]["filterDir"]=val;
    $.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:"data",path:val})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length == 0 ) {
		    document.getElementById('obsFilterDir').style.color='black'
		    obs_config[file]["filterDirStat"]="";
		    //console.log("Dir ok:",val);
		} else {
		    obs_config[file]["filterDirStat"]=val;
		    document.getElementById('obsFilterDir').style.color='red'
		    console.log("Dir NOT ok:",val);
		}
		obs_show();
	    })
	.error(
	    function (error) { alert("Model filter dir request failed (system error)");}
	);
};
function obs_setIndexTarget(target,parameter,value) {
    var file=obs_getConfigFile();
    obs_config[file]["targets"][target][parameter]=value;
};
function obs_show() {
    var file=obs_getConfigFile();
    if (file != "") {
	obs_allocate(file);
	showValue('obsConfigFile',file);
	showValue('obsConfigFileSave',file);
	showValue('obsFilterDir',obs_config[file]["filterDir"]);
	showValue('obsFilterDirMin',obs_config[file]["filterDirMin"]);
	showValue('obsFilterDirMax',obs_config[file]["filterDirMax"]);
	showValue('obsFilterFile',obs_config[file]["filterFile"]);
	showValue('obsTablePath',obs_config[file]["tablePath"]);
	showValue('obsBufrType',obs_config[file]["bufrType"]);
	showValue('obsSubType',obs_config[file]["subType"]);
	showValue('obsTypeInfo',obs_config[file]["typeInfo"]);
	obs_setIndexTargetTable(file);
	showValue('obsIndexTarget',obs_config[file]["indexTarget"]);
	showValue('obsIndexExp',obs_config[file]["indexExp"]);
	setInnerHTML('obsPatternHits',obs_config[file]["hits"]);
	// this may seem strange, Stat stores name of dir only if it does not exist...
	if (obs_config[file]["filterDirStat"]==obs_config[file]["filterDir"]) {
	    document.getElementById('obsFilterDir').style.color='red'
	    console.log("Directory does not exist:",obs_config[file]["filterDir"]);
	} else {
	    document.getElementById('obsFilterDir').style.color='black'
	}
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
    item.children[0].children[0].value=target;
    item.children[1].children[0].value=obs_config[file]["targets"][target]["pos"];
    item.children[3].children[0].value=obs_config[file]["targets"][target]["descr"];
    item.children[4].children[0].value=obs_config[file]["targets"][target]["info"];
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
    var target=item.parentNode.parentNode.children[0].children[0].value;
    var pos=item.parentNode.parentNode.children[1].children[0].value;
    var descr=item.parentNode.parentNode.children[3].children[0].value;
    var info=item.parentNode.parentNode.children[4].children[0].value;
    //    console.log("New: trg:",target," pos:",pos," des:",descr," info:",info);
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
	    obs_config[file]["targeto"].push(target);
	};
	obs_config[file]["targets"][target]["pos"]=pos;
	obs_config[file]["targets"][target]["descr"]=descr;
	obs_config[file]["targets"][target]["info"]=info;
	obs_setIndexTargetTable(file);
	item.parentNode.parentNode.children[0].children[0].value="";
	item.parentNode.parentNode.children[1].children[0].value="";
	item.parentNode.parentNode.children[3].children[0].value="";
	item.parentNode.parentNode.children[4].children[0].value="";
    } else {
	alert("Invalid observation target name: ('"+target+"')");
    }
};
function obs_saveConfigFile() {
    var file=obs_getConfigFile();
    var password=document.getElementById("obsConfigFilePsw").value;
    var filterDir="";
    var filterDirMin="";
    var filterDirMax="";
    var filterFile="";
    var table="";
    var bufrType="";
    var subType="";
    var typeInfo="";
    var indexTarget="";
    var indexExp="";
    var stack="";
    var obsTargets="";
    if (obs_config[file]!= undefined) {
	filterDir=obs_config[file]["filterDir"]//"";
	filterDirMin=obs_config[file]["filterDirMin"]//"";
	filterDirMax=obs_config[file]["filterDirMax"]//"";
	filterFile=obs_config[file]["filterFile"]//"";
	var sfile=obs_config[file]["stack"];
	if (sfile !== "") {
	    stack=stack+"|"+sfile;
	};
	bufrType=obs_config[file]["bufrType"]//"";
	subType=obs_config[file]["subType"]//"";
	typeInfo=obs_config[file]["typeInfo"]//"";
	indexTarget=obs_config[file]["indexTarget"]//"";
	indexExp=obs_config[file]["indexExp"]//"";
	var targeto=obs_config[file]["targeto"];
	var targets=obs_config[file]["targets"];
	//for (var target in targets) {
	for (var ii =0; ii< targeto.length;ii++) {
	    var target=targeto[ii];
	    var pos=targets[target]["pos"];
	    var descr=targets[target]["descr"];
	    var info=targets[target]["info"];
	    obsTargets=obsTargets + "|" + target + "~" + pos + "~" + descr + "~" + info;
	};
	table=obs_config[file]["tablePath"]//"";
    }
    if (obsTargets == "") {obsTargets="none";}
    obs_configEd++;
    documentLog.innerHTML="Sent obs-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"obs",file:file,password:password,
				  filterDir:filterDir,
				  filterDirMin:filterDirMin,
				  filterDirMax:filterDirMax,
				  filterFile:filterFile,
				  stack:stack,
				  table:table,
				  bufrType:bufrType,
				  subType:subType,
				  typeInfo:typeInfo,
				  indexTarget:indexTarget,
				  indexExp:indexExp,
				  obsTargets:obsTargets
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
	    function (error) { alert("Obs save request failed (system error)");}
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
    //
    var bufr=obs_config[file]["bufr"];
    //for (var target in value) {
    for (var ii =0; ii< targeto.length;ii++) {
	var target=targeto[ii];
	var color="green";
	var pos = targets[target]["pos"];
	if (pos !== undefined &&
	    bufr !== undefined && 
	    bufr[bufrType] !== undefined && 
	    bufr[bufrType][subType] !== undefined && 
	    bufr[bufrType][subType][pos] !== undefined) {
	    var descr=bufr[bufrType][subType][pos]["descr"];
	    if (descr!=targets[target]["descr"]) {
		color="red";
	    };
	};
	obs_insertIndexTargetRow(tail,target,ii,
				 targets[target]["pos"],
				 targets[target]["descr"],color,
				 targets[target]["info"]);
    }
}
function obs_insertIndexTargetRow(item,target,ii,pos,descr,color,info) {
    var row = document.createElement("TR");
    var td,inp;
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
    inp.setAttribute("title","Position in BUFR sequence");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    td.setAttribute("align","center");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("title","Move target up");
    btn.setAttribute("onclick","obs_targetUp('"+ii+"');obs_show();");
    //btn.setAttribute("style","width:100%");
    //var t=document.createTextNode("--");
    //btn.appendChild(t);
    btn.innerHTML="&uarr;";
    //btn.setAttribute("align","center");
    td.appendChild(btn);
    row.appendChild(td);
    // make descr column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",descr);
    inp.setAttribute("title","BUFR descriptor");
    if (color !== "") {
	inp.setAttribute("style","color:"+color);
    }
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','descr',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make info column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",info);
    inp.setAttribute("title","Information");
    //inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","obs_setIndexTarget('"+target+"','info',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    td.setAttribute("align","center");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","obs_removeTarget('"+target+"')");
    btn.setAttribute("style","width:100%");
    btn.setAttribute("title","Remove observation target");
    //var t=document.createTextNode("--");
    //btn.appendChild(t);
    btn.innerHTML="&#45";
    //btn.setAttribute("align","center");
    td.appendChild(btn);
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
function obs_updateData(arg=obs_getConfigFile()) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent obs-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"obs",arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		obsLoaded=true;
		//console.log("Updating dropdown for ",target);
		obs_show();
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Obs request failed (system error)");}
	);
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
	    obsTrg[target]["info"];
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
				  typeInfo:typeInfo})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to scan file: "+sfile+" (file:"+file+")\n"+msg);
		    } else {
			dataToArray(data,status,documentLog);
			obs_show();
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Obs find request failed (system error)");}
	);
};

function obs_mkdir(path) {
    var password=document.getElementById("obsConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"obs",
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
	    function (error) { alert("Obs mkdir request failed (system error)");}
	);
    
};

function obs_rmfile(path) {
    var password=document.getElementById("obsConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"obs",
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
			//delete obs_config[path];
			if (obs_file == path) {obs_file="default.cfg";}
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Obs rmfile request failed (system error)");}
	);
    
};

function obs_rmdir(path) {
    var password=document.getElementById("obsConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"obs",
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
	    function (error) { alert("Obs rmdir request failed (system error)");}
	);
    
};

function obs_mkfile(file) {
    //console.log("Calling saveConfigFile: '"+file+"'");
    obs_setConfigFile(file);
    obs_saveConfigFile(file);
};

function obs_fgfile(path) { // clear file from internal memory
    if (obs_config[path] != undefined) {
	delete obs_config[path];
    }
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

function obs_targetUp(ii) {
    var file=obs_getConfigFile();
    if (obs_config[file] !== undefined && 
	obs_config[file]["targeto"] !== undefined) {
	var targeto = obs_config[file]["targeto"];
	arrayUp(targeto,ii);
    }
}

function obs_showConfigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent obs-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"obs",arg:args})
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
			addChildButton(item,"<up>","obs_setConfigFile('"+dd+"');","Change to parent <directory>");
			added=true;
		    }
		    if (args.length == 1) {
			//console.log("Arg ret:",ret);
			if (root["type"] == "dir" && root["loc"] != "") {
			    addChildButton(item,"<rmdir>","obs_rmdir('"+args[0]+"');","Remove <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<rmfile>","obs_rmfile('"+args[0]+"');","Remove <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			    if (looksLikeFile(args[0])) {
				addChildButton(item,"<mkfile>","obs_mkfile('"+args[0]+"');obs_show();","Make <file>");
				if (obs_config[args[0]] != undefined) {
				    addChildButton(item,"<fgfile>","obs_fgfile('"+args[0]+"');","Forget <file>");
				}
				added=true;
			    } else {
				addChildButton(item,"<mkdir>","obs_mkdir('"+args[0]+"');","Make <directory>");
				added=true;
			    }
			}
		    } else if (args.length == 2) {
			if (root["type"] == "dir") {
			    addChildButton(item,"<cpdir>","obs_cpdir('"+args[0]+"','"+args[1]+"');","Copy <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<cpfile>","obs_cpfile('"+args[0]+"','"+args[1]+"');obs_setConfigFile('"+args[2]+"');obs_show();","Copy <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			}
		    };
		    //for (var obs in obs_config) {
		    //console.log("Adding config button: ",obs);
		    //addChildButton(item,obs,"obs_setConfigFile('"+obs+"');obs_show();");
		    //added=true;
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
			//console.log("Adding dir button: ",dd);
			if (looksLikeFile(dd)) {
			    addChildButton(item,dd,"obs_setConfigFile('"+dd+"');obs_show();","Use <file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"obs_setConfigFile('"+dd+"');obs_show();","Change <directory>");
			    added=true;
			}
		    }
		    if (! added) {addChildText(item,"No data available...");}
		}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Obs config request failed (system error)");}
	);
    // documentLog.innerHTML="Sent obs-load request.";
    // $.get("cgi-bin/fark_load.pl",{type:"obs",arg:args},function(data, status){
    //     dataToArray(data,status,documentLog);
    //     //console.log("Updating dropdown for ",target);
    //     removeChildren(item);
    //     var added=false;
    //     for (var obs in obs_config) {
    // 	addChildButton(item,obs,"obs_setConfigFile('"+obs+"');obs_show();");
    //      added=true;
    //     };
    //     documentLog.innerHTML="";
    // });
}

function obs_showFilterDir(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent obs-load request.";
    var file=obs_getConfigFile();
    var path=args[0] || "";
    var cls = "data";
    var filter=obs_config[file]["filterFile"];
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
					   "obs_setArray('filterDir','"+dd+"');obs_show();","Change to parent <directory>");
			    added=true;
			};
			var dirs=ls[0].getElementsByTagName("dir");
			//console.log("Found dir entries: ",dirs.length);
			for (var ii=0; ii< dirs.length; ii++) {
			    var dd = dirs[ii].getAttribute("path");
			    //console.log("Adding dir button: ",dd);
			    addChildButton(item,dd,"obs_setArray('filterDir','"+dd+"');obs_show();","Change <directory>");
			    added=true;
			};
			var patts=ls[0].getElementsByTagName("pattern");
			//console.log("Found file entries: ",patts.length);
			for (var ii=0; ii< patts.length; ii++) {
			    var rr = getFile(patts[ii].getAttribute("regexp"));
			    var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
			    if (dd !== '') {
				//console.log("Adding file button: ",dd,rr);
				addChildButtonShaded(item,dd,"obs_setArray('filterFile','"+rr+"');obs_show();","Copy <pattern> to filter");
				added=true;
			    };
			};
			var fils=ls[0].getElementsByTagName("file");
			//console.log("Found file entries: ",fils.length);
			for (var ii=0; ii< fils.length; ii++) {
			    var dd = getFile(fils[ii].getAttribute("path"));
			    var size = fils[ii].getAttribute("size")
			    if (dd !== '') {
				//console.log("Adding file button: ",dd);
				addChildButton(item,size+" "+dd,"obs_setArray('filterFile','"+dd+"');obs_show();","Copy <file name> to filter");
				added=true;
			    };
			};
		    };
		};
		if (! added) {addChildText(item,"No data available...");}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Obs filter request failed (system error)");}
	);
};

function obs_showFilterFile(item,target,arg) {
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
				  typeInfo:typeInfo})
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
			setInnerHTML('obsPatternHits',obs_config[file]["hits"]);
			removeChildren(item);
			var added=false;
			var len=obs_config[file]["files"].length;
			for (var ii=0; ii<len;ii++) {
			    var sfile=obs_config[file]["files"][ii][0];
			    var sage=parseFloat(obs_config[file]["files"][ii][1]).toFixed(2);
			    var ssize=obs_config[file]["files"][ii][2];
			    addChildButton(item,ssize+" "+sfile+" ("+sage+"d)","obs_fileFind('"+sfile+"');","Scan <observation file>");
			    added=true;
			}
			if (! added) {addChildText(item,"No data available...");}
		    };
		    documentLog.innerHTML="";
		}
	    })
	.error(
	    function (error) { alert("Obs filter file request failed (system error)");}
	);
};

function obs_showTablePath(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent obs-load request.";
    var path=args[0] || "";
    var cls = "tables";
    $.get("cgi-bin/fark_dir.pl",{cmd:"ls",cls:cls,path:path})
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
					   "obs_setArray('tablePath','"+dd+"');obs_show();","Change to parent <directory>");
			    added=true;
			};
			var dirs=ls[0].getElementsByTagName("dir");
			//console.log("Found dir entries: ",dirs.length);
			for (var ii=0; ii< dirs.length; ii++) {
			    var dd = dirs[ii].getAttribute("path");
			    //console.log("Adding dir button: ",dd);
			    addChildButton(item,dd,"obs_setArray('tablePath','"+dd+"');obs_show();","Change <directory>");
			    added=true;
			};
		    };
		};
		if (! added) {addChildText(item,"No data available...");}
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Table request failed (system error)");}
	);
};

function obs_showBufrType(item,target,arg) {
    var file=obs_getConfigFile();
    removeChildren(item);
    var added=false;
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
		addChildButton(item,bufrType+" "+info+ccnt,"obs_setArray('bufrType','"+bufrType+"');showValue('obsBufrType','"+bufrType+"');","Use <BUFR type>");
		added=true;
	    }
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};

function obs_showSubType(item,target,arg) {
    var file=obs_getConfigFile();
    var bufrType=obs_config[file]["bufrType"];
    removeChildren(item);
    var added=false;
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
		    addChildButton(item,subType+" : "+info+ccnt,"obs_setArray('subType','"+subType+"');obs_setArray('typeInfo','"+info+"');showValue('obsSubType','"+subType+"');showValue('obsTypeInfo','"+info+"');","Use <BUFR sub type>");
		    added=true;
		}
	    }
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};

function obs_showIndexPos(item,target,arg) {
    var file=obs_getConfigFile();
    var bufrType=obs_config[file]["bufrType"];
    var subType=obs_config[file]["subType"];
    removeChildren(item);
    var added=false;
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
		var info=" "+bufr[pos]["info"];
		if (bufr[pos]["val1"] !== undefined && bufr[pos]["val1"] != "NA") {
		    var value="    ~ "+bufr[pos]["val1"];
		} else {
		    var value="";
		};
		if (descr == "31001") {
		    addChildButtonShaded(item,pos+" : "+descr + info + value,"showValue('obsIndexPOS','"+pos+"');showValue('obsIndexDESCR','"+descr+"');showValue('obsIndexInfo','"+info+"');","use <BUFR delayed replicator>");
		    added=true;
		} else {
		    addChildButton(item,pos+" : "+descr + info + value,"showValue('obsIndexPOS','"+pos+"');showValue('obsIndexDESCR','"+descr+"');showValue('obsIndexInfo','"+info+"');","Use <BUFR sequence element>");
		    added=true;
		}
	    }
	}
    }
    if (! added) {addChildText(item,"No data available...");}
};

function obs_showIndexExp(item,target,arg) {
    console.log("showIndexExp target='" + target + "'  arg='"+arg+"'");
    var file=obs_getConfigFile();
    var bufrType=obs_config[file]["bufrType"];
    var subType=obs_config[file]["subType"];
    removeChildren(item);
    var added=false;
    if ( obs_config[file] !== undefined &&
	 obs_config[file]["targets"] !== undefined ) {
	for (var trg in obs_config[file]["targets"]) {
	    addChildButton(item,trg,"addValue('obsIndexExp','"+trg+"');","observation target");
	    added=true;
	}
	addFunctionButtons(item,target);
    }
    if (! added) {addChildText(item,"No data available...");}
};
#__file: 'js/plot.js' 0100644    **DO NOT DELETE**
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
    plot_cats[cat]=plot_goclone(plot_org_cats[cat]);
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
	    var re = new RegExp("(\w*)"+plot_quote(attr)+"(\w*)", "g");
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

function plot_quote(str) {
    var re = new RegExp("[.?*+^$[](){}|-\\]", "g");
    return (str+'').replace(re, "\\$&");
};
function plot_goclone(source) {
    if (Object.prototype.toString.call(source) === '[object Array]') {
        var clone = [];
        for (var i=0; i<source.length; i++) {
            clone[i] = plot_goclone(source[i]);
        }
        return clone;
    } else if (typeof(source)=="object") {
        var clone = {};
        for (var prop in source) {
            if (source.hasOwnProperty(prop)) {
                clone[prop] = plot_goclone(source[prop]);
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
	//showValue('plotCat',plot_config[file]["cat"]);
	showValue('plotTable',plot_config[file]["table"]);
	showValue('plotGraphics',plot_config[file]["graphics"]);
	//plot_showDatasetTable();
	//plot_showAttributesTable();
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
	btn.setAttribute("onclick","showDropdown('plotSet').value)");
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
	btn.setAttribute("onclick","showDropdown('plotColoc').value)");
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
	btn.setAttribute("onclick","showDropdown('"+itemId+ "').value)");
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
	btn.setAttribute("onclick","showDropdown('"+itemId+"').value)");
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


function plot_showConfigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent plot-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"plot",arg:args})
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
			addChildButton(item,"<up>","plot_setConfigFile('"+dd+"');","Change to parent <directory>");
			added=true;
		    }
		    if (args.length == 1) {
			//console.log("Arg ret:",ret);
			if (root["type"] == "dir" && root["loc"] != "") {
			    addChildButton(item,"<rmdir>","plot_rmdir('"+args[0]+"');","Remove <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<rmfile>","plot_rmfile('"+args[0]+"');","Remove <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			    if (looksLikeFile(args[0])) {
				addChildButton(item,"<mkfile>","plot_mkfile('"+args[0]+"');plot_show();","Make <file>");
				if (plot_config[args[0]] != undefined) {
				    addChildButton(item,"<fgfile>","plot_fgfile('"+args[0]+"');","Forget <file>");
				}
				added=true;
			    } else {
				addChildButton(item,"<mkdir>","plot_mkdir('"+args[0]+"');","Make <directory>");
				added=true;
			    }
			}
		    } else if (args.length == 2) {
			if (root["type"] == "dir") {
			    addChildButton(item,"<cpdir>","plot_cpdir('"+args[0]+"','"+args[1]+"');","Copy <diretory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<cpfile>","plot_cpfile('"+args[0]+"','"+args[1]+"');plot_setConfigFile('"+args[2]+"');","Copy <file>");
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
			    addChildButton(item,dd,"plot_setConfigFile('"+dd+"');plot_show();","Use <file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"plot_setConfigFile('"+dd+"');plot_show();","Change <directory>");
			    added=true;
			}
		    }
		    if (! added) {addChildText(item,"No data available...");}
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Plot request failed (system error)");}
	);
};

function plot_showCat(item,target,arg) {
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
    for (var cat in plot_org_cats) {
	//console.log("Adding config button: ",cat);
	addChildButton(item,cat,"plot_setCat('"+cat+"');showValue('plotCat','"+cat+"');plot_show()","Plot category");
	added=true;
    }
    //documentLog.innerHTML="";
    //}).error(function (error) { alert("Request failed (system error)");});
    if (! added) {addChildText(item,"No data available...");}
};

function plot_showTable(item,target,arg) {
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
					       "plot_setArray('table','"+dd+"');plot_show();","Change to parent <directory>");
				added=true;
			    };
			    var dirs=ls[0].getElementsByTagName("dir");
			    //console.log("Found dir entries: ",dirs.length);
			    for (var ii=0; ii< dirs.length; ii++) {
				var dd = dirs[ii].getAttribute("path");
				//console.log("Adding dir button: ",dd);
				addChildButton(item,dd,"plot_setArray('table','"+dd+"');plot_show();","Change <directory>");
				added=true;
			    };
			    var patts=ls[0].getElementsByTagName("pattern");
			    //console.log("Found file entries: ",patts.length);
			    for (var ii=0; ii< patts.length; ii++) {
				var rr = getFile(patts[ii].getAttribute("regexp"));
				var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
				if (dd !== '') {
				    //console.log("Adding pattern button: ",dd,rr);
				    addChildButtonShaded(item,dd,"plot_setArray('table','"+rr+"');plot_show();","Use pattern");
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
				    addChildButton(item,size+" "+dd,"plot_setArray('table','"+dd+"');plot_show();","Use <file>");
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
	    function (error) { alert("Plot table request failed (system error)");}
	);
};

function plot_showGraphics(item,target,arg) {
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
					       "plot_setArray('graphics','"+dd+"');plot_show();","Change to parent <directory>");
				added=true;
			    };
			    var dirs=ls[0].getElementsByTagName("dir");
			    //console.log("Found dir entries: ",dirs.length);
			    for (var ii=0; ii< dirs.length; ii++) {
				var dd = dirs[ii].getAttribute("path");
				//console.log("Adding dir button: ",dd);
				addChildButton(item,dd,"plot_setArray('graphics','"+dd+"');plot_show();","Change <directory>");
				added=true;
			    };
			    var patts=ls[0].getElementsByTagName("pattern");
			    //console.log("Found file entries: ",patts.length);
			    for (var ii=0; ii< patts.length; ii++) {
				var rr = getFile(patts[ii].getAttribute("regexp"));
				var dd = decodeURI(getFile(patts[ii].getAttribute("struct")));
				if (dd !== '') {
				    //console.log("Adding file button: ",dd,rr);
				    addChildButtonShaded(item,dd,"plot_setArray('graphics','"+rr+"');plot_show();","Use <pattern>");
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
				    addChildButton(item,size+" "+dd,"plot_setArray('graphics','"+dd+"');plot_show();","Use <file>");
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
	    function (error) { alert("Plot dir request failed (system error)");}
	);
};
#__file: 'js/rerun.js' 0100644    **DO NOT DELETE**

// data structure
rerun_file="default.cfg";
rerun_config = { "default.cfg" : {model :  {"default.cfg" : {last:"",info:"",rerun:"",status:""}},
				  obs   :  {"default.cfg" : {last:"",info:"",rerun:"",status:""}},
				  coloc :  {"default.cfg" : {last:"",info:"",rerun:"",status:""}},
				  table :  {"default.cfg" : {last:"",info:"",rerun:"",status:""}},
				  join  :  {"default.cfg" : {last:"",info:"",rerun:"",status:""}},
				  plot  :  {"default.cfg" : {last:"",info:"",rerun:"",status:""}},
				  variable: {name: 'rid',start: '0',stop: '0'},
				  offset:'-cnt'
				 }
	       };
rerun_configEd=0;

// rerun methods
function rerun_run() {
    var file=rerun_getConfigFile();
    var password=document.getElementById("rerunConfigFilePsw").value;
    documentLog.innerHTML="Sent rerun-now request ("+file+").";
    $.ajaxSetup({timeout:0}); // never timeout a request (and re-send it)...
    $.get("cgi-bin/fark_rerun.pl",{file:file,password:password})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			if (isRunning(msg)) {
			    alert(msg+"\nMonitoring has timed out.");
			} else {
			    alert("Unable to process config file: "+file+"\n"+msg);
			}
		    } else {
			dataToArray(data,status,documentLog);
			rerun_setTable();
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Request failed (system error)");}
	);
}

function rerun_allocate(file) {
    if (rerun_config[file] === undefined) {
	rerun_config[file]=clone(rerun_config[rerun_file]);
	//console.log("cloned:",file,rerun_file,rerun_config[file]);
    }
}
function rerun_getConfigFile() {
    return rerun_file;
};
function rerun_setOffset(value) {
    var file=rerun_getConfigFile();
    if (value === undefined) {
	value=getValue('rerunTimeOffset');
    };
    rerun_config[file]["offset"]=decodeURI(value);
};
function rerun_setArray(parameter,value) {
    var file=rerun_getConfigFile();
    rerun_config[file]["variable"][parameter]=decodeURI(value);
};
function rerun_updateData(arg=rerun_getConfigFile()) {
    console.log("Loading rerun-data:",JSON.stringify(arg));
    var args=getArgs(arg);
    documentLog.innerHTML="Sent rerun-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"rerun",arg:args})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		rerun_setConfigFile(args[0]);
		rerun_show();
	    })
	.error(
	    function (error) { alert("Rerun request failed (system error)");}
	);
};
function rerun_newSetupFile(item) {
    var file=rerun_getConfigFile();
    var type=item.parentNode.parentNode.children[0].children[0].value;
    var setup=item.parentNode.parentNode.children[2].children[0].value;
    showValue('rerunType',"");
    showValue('rerunSetupFile',"");
    if (setup !== "" ) {
	fark_last[type]=setup;
	if (type === "model") {
	    if (rerun_config[file]["model"][setup] === undefined) {
		rerun_config[file]["model"][setup]={last:"",info:""};
	    };
	} else if (type === "obs") {
	    if (rerun_config[file]["obs"][setup] === undefined) {
		rerun_config[file]["obs"][setup]={last:"",info:""};
	    };
	} else if (type === "coloc") {
	    if (rerun_config[file]["coloc"][setup] === undefined) {
		rerun_config[file]["coloc"][setup]={last:"",info:""};
	    };
	} else if (type === "table") {
	    if (rerun_config[file]["table"][setup] === undefined) {
		rerun_config[file]["table"][setup]={last:"",info:""};
	    };
	} else if (type === "join") {
	    if (rerun_config[file]["join"][setup] === undefined) {
		rerun_config[file]["join"][setup]={last:"",info:""};
	    };
	} else if (type === "plot") {
	    if (rerun_config[file]["plot"][setup] === undefined) {
		rerun_config[file]["plot"][setup]={last:"",info:""};
	    };
	}
	rerun_setTable();
	//console.log("Saving setup file.");
	rerun_saveConfigFile();
    } else {
	alert("Invalid: Model config file ('"+setup+"')");
    }
    //console.log("Adding ",type,setup,rerun);
};
function rerun_saveConfigFile() {
    var file=rerun_getConfigFile();
    var offset=rerun_config[file]["offset"];
    var variable=rerun_config[file]["variable"]["name"];
    var start=rerun_config[file]["variable"]["start"];
    var stop=rerun_config[file]["variable"]["stop"];
    var password=document.getElementById("rerunConfigFilePsw").value;
    var modelFiles="";
    var obsFiles="";
    var colocFiles="";
    var tableFiles="";
    var joinFiles="";
    var plotFiles="";
    rerun_setTable();
    for (var model in rerun_config[file]["model"]) {
	modelFiles=modelFiles + "|" + model + "~" + 
	    rerun_config[file]["model"][model]["last"] + "~" +
 	    rerun_config[file]["model"][model]["info"] ;
    };
    for (var obs in rerun_config[file]["obs"]) {
	obsFiles=obsFiles + "|" + obs + "~" + 
	    rerun_config[file]["obs"][obs]["last"] + "~" +
	    rerun_config[file]["obs"][obs]["info"] ;
    }
    for (var coloc in rerun_config[file]["coloc"]) {
	colocFiles=colocFiles + "|" + coloc + "~" + 
	    rerun_config[file]["coloc"][coloc]["last"] + "~" +
	    rerun_config[file]["coloc"][coloc]["info"];
	//#+
	//	    #rerun_config[file]["coloc"][coloc]["rerun"];
    }
    for (var table in rerun_config[file]["table"]) {
	tableFiles=tableFiles + "|" + table + "~" + 
	    rerun_config[file]["table"][table]["last"] + "~" +
	    rerun_config[file]["table"][table]["info"] ;
    }
    for (var join in rerun_config[file]["join"]) {
	joinFiles=joinFiles + "|" + join + "~" + 
	    rerun_config[file]["join"][join]["last"] + "~" +
	    rerun_config[file]["join"][join]["info"] ;
    }
    for (var plot in rerun_config[file]["plot"]) {
	plotFiles=plotFiles + "|" + plot + "~" + 
	    rerun_config[file]["plot"][plot]["last"] + "~" +
	    rerun_config[file]["plot"][plot]["info"] ;
    }
    documentLog.innerHTML="Sent rerun-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"rerun",file:file,password:password,
				  variable:variable,start:start,stop:stop,offset:offset,
				  modelFiles:modelFiles,
				  obsFiles:obsFiles,
				  colocFiles:colocFiles,
				  tableFiles:tableFiles,
				  joinFiles:joinFiles,
				  plotFiles:plotFiles})
	.success(
	    function(data, status){
		if (status == "success") {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to save rerun config file: "+file+"\n"+msg);
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Rerun save request failed (system error)");}
	);
    makeUrl("rerun",file);
};
function rerun_removeFile(item,type,setup) {
    var file=rerun_getConfigFile();
    var newitem=document.getElementById("newlineRerun");
    newitem.children[0].children[0].value=type;
    newitem.children[2].children[0].value=setup;
    //if (! checkRerunPassword()) {return;}
    //item.parentNode.removeChild(item);
    delete rerun_config[file][type][setup];
    rerun_setTable();
};

//#F78181
function rerun_show() {
    var file=rerun_getConfigFile();
    if (file != "") {
	rerun_allocate(file);
	rerun_setConfigFile(file);
	showValue('rerunTimeOffset',rerun_config[file]["offset"]);
	showValue('rerunVariableStart',rerun_config[file]["variable"]["start"]);
	showValue('rerunVariableStop',rerun_config[file]["variable"]["stop"]);
	rerun_setTable();
    };
};
// make new obs-filter entry
function rerun_setConfigFile(file) {
    showValue('rerunConfigFileSave',file);
    showValue('rerunConfigFile',file);
    //if (file != "") {
    rerun_allocate(file);
    rerun_file=file;
    //};
}
// create rerun table
function rerun_setTable() {
    var file=rerun_getConfigFile();
    console.log("Rerun set table:",file,JSON.stringify(rerun_config));
    var item=document.getElementById('rerunTable');
    var tail=removeTableChildFromTo(item,"labelsRerun","newlineRerun");
    var modell=[];
    var obsl=[];
    var colocl=[];
    var tablel=[];
    var joinl=[];
    var plotl=[];
    for (var model in rerun_config[file]["model"]) {
	//console.log("*** Found: ",cron, model);
	modell.push(model);
    }
    for (var obs in rerun_config[file]["obs"]) {
	//console.log("*** Found: ",cron,obs);
	obsl.push(obs);
    }
    for (var coloc in rerun_config[file]["coloc"]) {
	//console.log("*** Found: ",cron,coloc);
	colocl.push(coloc);
    }
    for (var table in rerun_config[file]["table"]) {
	//console.log("*** Found: ",cron,table);
	tablel.push(table);
    }
    for (var join in rerun_config[file]["join"]) {
	//console.log("*** Found: ",cron,join);
	joinl.push(join);
    }
    for (var plot in rerun_config[file]["plot"]) {
	//console.log("*** Found: ",cron,plot);
	plotl.push(plot);
    }
    // sort...
    modell.sort();
    obsl.sort();
    colocl.sort();
    tablel.sort();
    joinl.sort();
    plotl.sort();
    for (var ii = 0; ii < modell.length; ++ii) {
	var model=modell[ii];
	//console.log("Insert row: ",model);
	rerun_insertRow(tail,"model",model,
			rerun_config[file]["model"][model]["last"],
			rerun_config[file]["model"][model]["info"],
			rerun_config[file]["model"][model]["status"],"#0A0"); 
    }
    for (var ii = 0; ii < obsl.length; ++ii) {
	var obs=obsl[ii];
	//console.log("Insert row: ",obs);
	rerun_insertRow(tail,"obs",obs,
			rerun_config[file]["obs"][obs]["last"],
			rerun_config[file]["obs"][obs]["info"],
			rerun_config[file]["obs"][obs]["status"],"#AFA");
    }
    for (var ii = 0; ii < colocl.length; ++ii) {
	var coloc=colocl[ii];
	//console.log("Insert row: ",coloc);
	rerun_insertRow(tail,"coloc",coloc,
			rerun_config[file]["coloc"][coloc]["last"],
			rerun_config[file]["coloc"][coloc]["info"],
			rerun_config[file]["coloc"][coloc]["status"],"#FAA");
    }
    for (var ii = 0; ii < tablel.length; ++ii) {
	var table=tablel[ii];
	//console.log("Insert row: ",table);
	rerun_insertRow(tail,"table",table,
			rerun_config[file]["table"][table]["last"],
			rerun_config[file]["table"][table]["info"],
			rerun_config[file]["table"][table]["status"],"#FA0");
    }
    for (var ii = 0; ii < joinl.length; ++ii) {
	var join=joinl[ii];
	//console.log("Insert row: ",join);
	rerun_insertRow(tail,"join",join,
			rerun_config[file]["join"][join]["last"],
			rerun_config[file]["join"][join]["info"],
			rerun_config[file]["join"][join]["status"],"#AF0");
    }
    for (var ii = 0; ii < plotl.length; ++ii) {
	var plot=plotl[ii];
	//console.log("Insert row: ",plot);
	rerun_insertRow(tail,"plot",plot,
			rerun_config[file]["plot"][plot]["last"],
			rerun_config[file]["plot"][plot]["info"],
			rerun_config[file]["plot"][plot]["status"],"#0AF");
    }
};

// create rerun table row
function rerun_insertRow(item,type,setup,last,info,status,color) {
    var row = document.createElement("TR");
    row.setAttribute("bgcolor",color);
    var td;
    // make TYPE column
    td=document.createElement("TD");
    td.innerHTML=type;
    td.setAttribute("title","Schedule type.");
    row.appendChild(td);
    // make select-TYPE column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make FILE NAME column
    td=document.createElement("TD");
    td.innerHTML=setup;
    if (type == "model") {
	td.setAttribute("title","Maintain <model file index>.");
    } else if (type == "obs") {
	td.setAttribute("title","Maintain <observation file index>.");
    } else if (type == "coloc") {
	td.setAttribute("title","Create <colocation xml> for debugging.");
    } else if (type == "table") {
	td.setAttribute("title","Create <table file>.");
    } else if (type == "join") {
	td.setAttribute("title","Join <table file> into new <table file>.");
    } else if (type == "plot") {
	td.setAttribute("title","Run plotting script using data in <table file>.");
    } else {
    };
    row.appendChild(td);
    //console.log("Row file name=",setup);
    // make select-FILE NAME column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make LAST column
    td=document.createElement("TD");
    if (status !== "") {
	td.setAttribute("style","color:blue");
    }
    var a = document.createElement('a');
    var linkText = document.createTextNode(last);
    a.appendChild(linkText);
    a.title = "View last log-file and error-file";
    a.href = "cgi-bin/fark_log.pl?type="+type+"&file="+setup;
    a.target ="_blank";
    td.appendChild(a);
    row.appendChild(td);
    // make INFO column
    td=document.createElement("TD");
    if (type =="model") {
	td.title = "Show status of the model-index-file.";
    } else if (type =="obs") {
	td.title = "Show status of the observation-index-file.";
    } else if (type =="table") {
	td.title = "Show status of the table-file.";
    } else if (type =="join") {
	td.title = "Show status of the joined table-file.";
    } else if (type =="plot") {
	td.title = "Show status of the table-file.";
    } else {
	td.title = "Show status.";
    };
    td.innerHTML=info;
    row.appendChild(td);
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px;");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","rerun_removeFile(this.parentNode.parentNode,'"+type+"','"+setup+"')");
    btn.setAttribute("style","width:100%");
    btn.setAttribute("title","Remove scheduled job");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make add row to table
    item.parentNode.insertBefore(row,item);
    return row;
}

function isRunning(msg) {
    return (msg.substring(0,18) === "Process is running");
}

function rerun_checkPassword() {
    var file=rerun_getConfigFile();
    var password=document.getElementById("rerunConfigFilePsw").value;
    if (rerun_config[file]["password"] !== undefined) {
	if (rerun_config[file]["password"] !== password) {
	    alert("Invalid password used when attempting to save Rerun configuration\n");
	    return false;
	}
    };
    return true;
}
function rerun_fileFind(sfile) {
    var file=rerun_getConfigFile();
    rerun_config[file]["stack"]=sfile;
    var password=document.getElementById("rerunConfigFilePsw").value;
    documentLog.innerHTML="Sent rerun-find request.";
    $.get("cgi-bin/fark_find.pl",{type:"rerunfile",
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
			rerun_show();
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Rerun find request failed (system error)");}
	);
};

function rerun_mkdir(path) {
    var password=document.getElementById("rerunConfigFilePsw").value;
    documentLog.innerHTML="Sent mkdir request.";
    $.get("cgi-bin/fark_dir.pl",{cmd:"mk",
				 cls:"rerun",
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
	    function (error) { alert("Rerun mkdir request failed (system error)");}
	);
    
};

function rerun_rmdir(path) {
    var password=document.getElementById("rerunConfigFilePsw").value;
    documentLog.innerHTML="Sent rmdir request.";
    $.get("cgi-bin/fark_dir.pl",{cmd:"rm",
				 cls:"rerun",
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
	    function (error) { alert("Rerun rmdir request failed (system error)");}
	);
    
};

function rerun_rmfile(path) {
    var password=document.getElementById("rerunConfigFilePsw").value;
    $.get("cgi-bin/fark_dir.pl",{cmd:"rf",
				 cls:"rerun",
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
			//delete rerun_config[path];
			if (rerun_file == path) {rerun_file="default.cfg";}
		    };
		    documentLog.innerHTML="";}
	    })
	.error(
	    function (error) { alert("Rerun rmfile request failed (system error)");}
	);
    
};

function rerun_mkfile(file) {
    //console.log("Calling saveConfigFile: '"+file+"'");
    rerun_setConfigFile(file);
    rerun_saveConfigFile(file);
};

function rerun_fgfile(path) { // clear file from internal memory
    if (rerun_config[path] != undefined) {
	delete rerun_config[path];
    }
};

function rerun_showConfigFile(item,target,arg) {
    var args=getArgs(arg);
    documentLog.innerHTML="Sent rerun-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"rerun",arg:args})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+arg+"'\n"+msg);
		} else {
		    //console.log("Updating dropdown for ",target,JSON.stringify(data));
		    var ret=dataToArray(data,status,documentLog);
		    var root=ret[0]||{};
		    //console.log("Got data ",target,JSON.stringify(root));
		    removeChildren(item);
		    var added=false;
		    if (args.length >0 && looksLikeFile(args[0])) {
			var file=getFile(args[0]);
		    } else {
			var file="";
		    };
		    var dirs=getSubDirs(root["cls"],root["root"],root["loc"],root["child"]);
		    //console.log("Found entries: ",dirs.length-1,root);
		    var parent=dirs[0];
		    if (parent != null) {
			var dd=parent;
			addChildButton(item,"<up>","rerun_setConfigFile('"+dd+"');","Change to parent <directory>");
			added=true;
		    }
		    if (args.length == 1) {
			//console.log("Arg ret:",ret);
			if (root["type"] == "dir" && root["loc"] != "") {
			    addChildButton(item,"<rmdir>","rerun_rmdir('"+args[0]+"');","Remove <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<rmfile>","rerun_rmfile('"+args[0]+"');","Remove <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			    if (looksLikeFile(args[0])) {
				addChildButton(item,"<mkfile>","rerun_mkfile('"+args[0]+"');rerun_show();","Make <file>");
				if (rerun_config[args[0]] != undefined) {
				    addChildButton(item,"<fgfile>","rerun_fgfile('"+args[0]+"');","Forget <file>");
				}
				added=true;
			    } else {
				addChildButton(item,"<mkdir>","rerun_mkdir('"+args[0]+"');","Make <directory>");
				added=true;
			    }
			}
		    } else if (args.length == 2) {
			if (root["type"] == "dir") {
			    addChildButton(item,"<cpdir>","rerun_cpdir('"+args[0]+"','"+args[1]+"');","Copy <directory>");
			    added=true;
			} else if (root["type"] == "file") {
			    addChildButton(item,"<cpfile>","rerun_cpfile('"+args[0]+"','"+args[1]+"');rerun_setConfigFile('"+args[2]+"');rerun_show();","Copy <file>");
			    added=true;
			} else if (root["type"] == "unknown") {
			}
		    };
		    //for (var rerun in rerun_config) {
		    //console.log("Adding config button: ",rerun);
		    //addChildButton(item,rerun,"rerun_setConfigFile('"+rerun+"');rerun_show();");
		    // added=true;
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
			//console.log("Adding dir button: ",dd,ii,dirs[ii]);
			if (looksLikeFile(dd)) {
			    addChildButton(item,dd,"rerun_setConfigFile('"+dd+"');rerun_show();","Use <file>");
			    added=true;
			} else {
			    addChildButton(item,dd,"rerun_setConfigFile('"+dd+"');rerun_show();","Change <directory>");
			    added=true;
			}
		    };
		    if (! added) {addChildText(item,"No data available...");}
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Dropdown rerun request failed (system error)");}
	);
};

function rerun_showTimeOffset(item,target,arg) {
    var file=rerun_getConfigFile();
    var variable=rerun_config[file]["variable"]["name"];
    removeChildren(item);
    addChildButton(item,'rid',"addValue('"+target+"','-rid');rerun_setOffset();","Rerun variable.");
    addFunctionButtons(item,target);
};

function rerun_showType(item,target,arg) {
    removeChildren(item);
    addChildButton(item,"model","showValue('"+target+"','model');","Maintain model index");
    addChildButton(item,"observation","showValue('"+target+"','obs');","Maintain observation index");
    addChildButton(item,"colocation","showValue('"+target+"','coloc');","Make colocation XML (debugging only)");
    addChildButton(item,"table","showValue('"+target+"','table');","Make table file");
    addChildButton(item,"join","showValue('"+target+"','join');","Join table files");
    addChildButton(item,"plot","showValue('"+target+"','plot');","Make plot table and graphics");
};

function rerun_showSetupFile(item,target,arg) {
    var type=document.getElementById("rerunType").value // "obs";
    var args=getArgs(arg);
    documentLog.innerHTML="Sent rerun-load request.";
    $.get("cgi-bin/fark_load.pl",{type:type,arg:args})
	.success(
	    function(data, status){
		var errors=data.getElementsByTagName("error");
		if (errors.length > 0 ) {
		    item.classList.toggle("show");
		    var msg=getErrorMessage(errors);
		    alert("Unable to list '"+arg+"'\n"+msg);
		} else {
		    var ret=dataToArray(data,status,documentLog);
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			item.classList.toggle("show");
			var msg=getErrorMessage(errors);
			alert("Unable to list '"+arg+"', type '"+type+"' \n"+msg);
		    } else if (ret[0] !== undefined) {
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
			    addChildButton(item,"<up>","showValue('rerunSetupFile','"+dd+"');","Change to parent <directory>");
			    added=true;
			} else {
			    //console.log("Adding clear: ",dd);
			    addChildButton(item,"<up>","showValue('rerunSetupFile','');","Change to root <directory>");
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
				    if (looksLikeFile(dd)) {
					addChildButton(item,dd,"showValue('rerunSetupFile','"+dd+"');","Use <file>");
					added=true;
				    } else {
					addChildButton(item,dd,"showValue('rerunSetupFile','"+dd+"');","Change <directory>");
					added=true;
				    };
				}
			    }
			}
			if (! added) {addChildText(item,"No data available...");}
		    } else {
			console.log("Undefined root.");
		    }
		};
		documentLog.innerHTML="";
	    })
	.error(
	    function (error) { alert("Rerun request failed (system error)");}
	);
};
#__file: 'js/table.js' 0100644    **DO NOT DELETE**
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
    var tableCols="";
    var tableSets="";
    var tableAttrs="";
    if (table_config[file] != undefined) {
	cat=table_config[file]["cat"]//"";
	table=table_config[file]["table"]//"";
	graphics=table_config[file]["graphics"]//"";
	if (table_cats[cat] != undefined) {
	    var colnames_=table_cats[cat]["colnames_"]//[];
	    for (var ii =0; ii< colnames_.length;ii++) {
		if (tableCols.length==0) {
		    tableCols=colnames_[ii];
		} else {
		    tableCols=tableCols+"~"+colnames_[ii];
		}
	    }
	    var sets=table_config[file]["dataset"]//{};
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
		tableSets=tableSets + "|" + set + "~" + coloc + "~" + legend + "~" + clmns;
	    };
	    var order=table_cats[cat]['order']//[];
	    var attrs=table_config[file]["attributes"]//{};
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
	btn.setAttribute("onclick","showDropdown('tableSet').value)");
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
	btn.setAttribute("onclick","showDropdown('tableColoc').value)");
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
	btn.setAttribute("onclick","showDropdown('"+itemId+ "').value)");
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
	btn.setAttribute("onclick","showDropdown('"+itemId+"').value)");
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
	//console.log("Adding config button: ",line);
	addChildButton(item,line+" ("+table_cats[cat]["lines"][line]+")","showValue('tableSet','"+line+"');showValue('tableType',table_cats['"+cat+"'][\"lines\"]['"+line+"']);","Data set identification");
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
				addChildButton(item,dd,"showValue('tableColoc','"+dd+"');table_loadColoc('"+dd+"');","Change <directory>");
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
#__file: 'js/url.js' 0100644    **DO NOT DELETE**
    url_file="default.cfg";
url_config = { "default.cfg" : { modelConfigFile : { file: "default.cfg",
						     targets : { "def_model" : { variable : "def",
										 min: "def_min",
										 max : "def_max"} },
						     def : [ {targets: {"def_model": 101}, 
							      info:"default info"} ]
						   },
				 obsConfigFile : { file: "default.cfg",
						   start: "def_start",
						   stop : "def_stop",
						   targets : { "def_obs" : {pos:"", descr:"", info:"",  min:"", max:""}
							     }
						 },
				 matchRules : { targets : {"def_target" : {exp:""}}},
				 host:"localhost",
				 password: ""
			       }
	     };
url_configEd = 0;
function url_getConfigFile() {
    return url_file;
};
function url_setConfigFile(value) {
    if (url_config[value] === undefined) {
	url_config[value]=clone(url_config[url_file]);
    }
    url_file=value;
    url_showURL();
}
function url_getModelConfigFile() {
    var file=url_getConfigFile();
    return url_config[file]["modelConfigFile"]["file"];
};
function url_getObsConfigFile() {
    var file=url_getConfigFile();
    return url_config[file]["obsConfigFile"]["file"];
};
function url_setConfig(type,parameter,val) {
    var file=url_getConfigFile();
    url_config[file][type][parameter]=val;
    // load if we are changing obs or model config files
    if (parameter === "file" && type === "model" && !modelLoaded) {
	documentLog.innerHTML="Sent "+type+"-load request.";
	$.get("cgi-bin/fark_load.pl",{type:type})
	    .success(
		function(data, status){
		    dataToArray(data,status,documentLog);
		    modelLoaded=true;
		    documentLog.innerHTML="";
		})
	    .error(
		function (error) { alert("URL model request failed (system error)");}
	    );
    } else if (parameter === "file" && type === "obs" && !obsLoaded) {
	documentLog.innerHTML="Sent "+type+"-load request.";
	$.get("cgi-bin/fark_load.pl",{type:type})
	    .success(
		function(data, status){
		    dataToArray(data,status,documentLog);
		    obsLoaded=true;
		    documentLog.innerHTML="";
		})
	    .error(
		function (error) { alert("URL obs request failed (system error)");}
	    );
    }
    url_showURL();
}
function url_setConfigFilesTarget (type,target,parameter,val) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	if (url_config[file][type] === undefined || 
	    url_config[file][type]['targets'] ===undefined ||
	    url_config[file][type]['targets'][target] ===undefined ||
	    url_config[file][type]['targets'][target][parameter] ===undefined) {
	    console.log("Undefined:",type,target,parameter,val);
	};
	url_config[file][type]['targets'][target][parameter]=val;
	url_showModelDefaultTable();
	url_showURL();
    }
}
function url_setArray (type,parameter,val) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	url_config[file][type][parameter]=val;
    }
}
function url_setConfigFilesDefault (ii,target,val) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	url_config[file]["modelConfigFile"]["def"][ii]["targets"][target]=val;
	url_showURL();
    }
}
function url_setConfigFilesDefaultInfo (ii,val) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	url_config[file]["modelConfigFile"]["def"][ii]["info"]=val;
	url_showURL();
    }
}

function url_newModelTarget(item) {
    var name=item.parentNode.parentNode.children[1].children[0].value;
    var variable=item.parentNode.parentNode.children[2].children[0].value;
    var minimum=item.parentNode.parentNode.children[4].children[0].value;
    var maximum=item.parentNode.parentNode.children[5].children[0].value;
    if (name !== "" & variable !== "") {
	var file= url_getConfigFile();
	if (url_config[file] === undefined) {
	    url_config[file]={modelConfigFile:{targets:{},def:{}},
			      obsConfigFile:{targets:{}},
			      matchRules:{ targets: {}},
			      host:"localhost",
			      password:""};
	};
	url_config[file]["modelConfigFile"]["targets"][name]={};
	url_config[file]["modelConfigFile"]["targets"][name]["variable"]=(variable || "");
	url_config[file]["modelConfigFile"]["targets"][name]["min"]=(minimum || "");
	url_config[file]["modelConfigFile"]["targets"][name]["max"]=(maximum || "");
	url_configEd++;
	//url_showModelTargetTable();
	//url_showModelDefaultTable();
	url_show();
	item.parentNode.parentNode.children[1].children[0].value="";	
	item.parentNode.parentNode.children[2].children[0].value="";	
	item.parentNode.parentNode.children[4].children[0].value="";	
	item.parentNode.parentNode.children[5].children[0].value="";	
    } else {
	alert("Invalid: name ('"+name+"'), variable ('"+variable+"')");
    }
};

function url_newModelDefault(item) {
    var file= url_getConfigFile();
    var line={targets:{},info:{}};
    var ok=false;
    var targets=url_config[file]["modelConfigFile"]["targets"];
    var pos=1;
    for (var target in targets) {
	var val=item.parentNode.parentNode.children[pos].children[0].value
	if (val !== undefined & val !== "") {
	    ok=true;
	}
	line["targets"][target]=(val|| "");
	item.parentNode.parentNode.children[pos].children[0].value="";
	pos=pos+1;
    }
    var info=item.parentNode.parentNode.children[pos].children[0].value;
    item.parentNode.parentNode.children[pos].children[0].value="";
    line["info"]=info;
    if (ok) {
	//console.log("url_config:",url_config[file]["modelConfigFile"]["def"]);
	url_config[file]["modelConfigFile"]["def"].push(line);
	//url_showModelDefaultTable();
	url_show();
    } else {
	alert("Invalid model default value.");
    }
};

function url_newObsTarget(item) {
    var ofile=url_getObsConfigFile();
    if ( obs_config[ofile] !== undefined) {
	var name=item.parentNode.parentNode.children[1].children[0].value;
	if (obs_config[ofile]["targets"][name] === undefined) {
	    var pos=item.parentNode.parentNode.children[2].children[0].value;
	    var descr=item.parentNode.parentNode.children[4].children[0].value;
	    var info=item.parentNode.parentNode.children[5].children[0].value;
	    var minimum=item.parentNode.parentNode.children[6].children[0].value;
	    var maximum=item.parentNode.parentNode.children[7].children[0].value;
	    var bufrType = obs_config[ofile]["bufrType"];
	    var subType = obs_config[ofile]["subType"];
	    if (name !== "" & bufrType !== ""& subType !== "") {
		var file= url_getConfigFile();
		if (url_config[file] === undefined) {
		    url_config[file]={modelConfigFile:{targets:{},def:{}},
				      obsConfigFile:{targets:{}},
				      matchRules:{ targets:{}},
				      host:"localhost",
				      password:""};
		};
		url_config[file]["obsConfigFile"]["targets"][name]={};
		url_config[file]["obsConfigFile"]["targets"][name]["pos"]=(pos || "");
		url_config[file]["obsConfigFile"]["targets"][name]["descr"]=(descr || "");
		url_config[file]["obsConfigFile"]["targets"][name]["info"]=(info || "");
		url_config[file]["obsConfigFile"]["targets"][name]["min"]=(minimum || "");
		url_config[file]["obsConfigFile"]["targets"][name]["max"]=(maximum || "");
		url_configEd++;
		//url_showObsTargetTable();
		url_show();
		item.parentNode.parentNode.children[1].children[0].value="";
		item.parentNode.parentNode.children[2].children[0].value="";
		item.parentNode.parentNode.children[4].children[0].value="";
		item.parentNode.parentNode.children[5].children[0].value="";
		item.parentNode.parentNode.children[6].children[0].value="";
		item.parentNode.parentNode.children[7].children[0].value="";
	    } else {
		alert("Invalid: name ('"+name+"'), BUFR type ('"+bufrType+"'), subType ('"+subType+"')");
	    }
	} else {
	    alert("'"+name+"' already used in obs config file: '"+ofile+"'");
	}
    } else {
	alert("Obs config file not loaded ('"+ofile+"')");
    }
};

function url_newTargetMatch(item) {
    var name=item.parentNode.parentNode.children[1].children[0].value;
    var expr=item.parentNode.parentNode.children[3].children[0].value;
    if (name !== "" & expr !== "" ) {
	var file= url_getConfigFile();
	if (url_config[file] === undefined) {
	    url_config[file]={modelConfigFile:{targets:{},def:{}},
			      obsConfigFile:{targets:{}},
			      matchRules:{targets:{}},
			      host:"localhost",
			      password:""};
	};
	url_config[file]["matchRules"]["targets"][name]={};
	url_config[file]["matchRules"]["targets"][name]["exp"]=(expr || "");
	//url_showTargetMatchTable();
	item.parentNode.parentNode.children[1].children[0].value="";
	item.parentNode.parentNode.children[3].children[0].value="";
	url_show();
    } else {
	alert("Invalid: model target ('"+name+"'), observation target expression ('"+expr+"')");
    }
};

// create model target table
function url_showModelTargetTable() {
    var item=document.getElementById('modelTargetTable');
    var file=url_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsModelTarget","newlineModelTarget");
    var targets=url_config[file]["modelConfigFile"]["targets"];
    for (var target in targets) {
	url_insertModelTargetRow(tail,target,targets[target]["variable"],targets[target]["min"],targets[target]["max"]);
    }
};
// create exec table row
function url_insertModelTargetRow(item,target,variable,min,max) {
    var row = document.createElement("TR");
    var file = url_getModelConfigFile();
    var mark=(variable === model_config[file]["index"]);
    var td, inp;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","url_removeModelTarget(this.parentNode.parentNode,'"+target+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("-");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make FILE NAME column
    td=document.createElement("TD");
    if (mark) {
	td.setAttribute("style","color:blue");
    } else {
	td.setAttribute("style","");
    }
    td.innerHTML=target;
    row.appendChild(td);
    // make variable column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",variable);
    if (mark) {
	inp.setAttribute("style","width:100%;color:blue");
    } else {
	inp.setAttribute("style","width:100%");
    }
    inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','variable',this.value);url_showModelTargetTable()");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-variable column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make minimum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    if (mark) {
	inp.setAttribute("id","urlModelIndexStart");
	inp.setAttribute("style","width:125px");
	inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','min',this.value);url_setArray('modelConfigFile','start',this.value);url_showModelTargetTable();");
    } else {
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','min',this.value);");
    }
    td.appendChild(inp);
    if (mark) {
	var btn=document.createElement("BUTTON");
	btn.setAttribute("onclick","url_getModelIndexStart('urlModelIndexStart','"+target+"')");
	btn.setAttribute("style","width:25px");
	var t=document.createTextNode("←"); // "→"
	btn.appendChild(t);
	td.appendChild(btn);
    }
    row.appendChild(td);
    // make maximum column
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    if (mark) {
	inp.setAttribute("id","urlModelIndexStop");
	inp.setAttribute("style","width:125px");
	inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);");
	inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);url_setArray('modelConfigFile','stop',this.value);url_showModeltargetTable();");
    } else {
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","url_setConfigFilesTarget('modelConfigFile','"+target+"','max',this.value);");
    }
    td.appendChild(inp);
    if (mark) {
	var btn=document.createElement("BUTTON");
	btn.setAttribute("onclick","url_getModelIndexStop('urlModelIndexStop','"+target+"')");
	btn.setAttribute("style","width:25px");
	var t=document.createTextNode("→"); // "←"
	btn.appendChild(t);
	td.appendChild(btn);
    }
    row.appendChild(td);
    // make add row to table
    item.parentNode.insertBefore(row,item);
    return row;
}
// create model target table
function url_showModelDefaultTable() {
    var item=document.getElementById('modelDefaultTable');
    var header=clearTableChild(item,"labelsModelDefault");
    var newline=clearTableChild(item,"newlineModelDefault");
    var file=url_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsModelDefault","newlineModelDefault");
    url_insertModelDefaultHeader(header,file);
    url_insertModelDefaultNewline(newline,file);
    url_insertModelDefaultRow(tail,file);
};
// create model default table header
function url_insertModelDefaultHeader(row,file) {
    var td,bf;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    var targets=url_config[file]["modelConfigFile"]["targets"];
    for (var target in targets) {
	// make variable names
	td=document.createElement("TD");
	bf=document.createElement("BF");
	bf.innerHTML=target;
	td.appendChild(bf);
	row.appendChild(td);
    }
    td=document.createElement("TD");
    bf=document.createElement("BF");
    bf.innerHTML="information";
    td.appendChild(bf);
    row.appendChild(td);
}

function url_insertModelDefaultRow(item,file) {
    var td;
    // make "-" column
    var defs=url_config[file]["modelConfigFile"]["def"];
    var len=defs.length;
    for (var ii=0;ii<len;ii++){
	if (url_config[file]["modelConfigFile"]["def"][ii]["targets"] !== undefined) {
	    var row = document.createElement("TR");
	    td=document.createElement("TD");
	    td.setAttribute("style","min-width:25px;width:25px");
	    var btn=document.createElement("BUTTON");
	    btn.setAttribute("onclick","url_removeModelDefault(this.parentNode.parentNode,'"+file+"',"+ii+")");
	    btn.setAttribute("style","width:100%");
	    var t=document.createTextNode("-");
	    btn.appendChild(t);
	    td.appendChild(btn);
	    row.appendChild(td);
	    var targets=url_config[file]["modelConfigFile"]["targets"];
	    for (var target in targets) {
		// make value column
		td=document.createElement("TD");
		td.setAttribute("class","fill");
		td.setAttribute("trg",target);
		inp=document.createElement("INPUT");
		inp.setAttribute("type","text");
		inp.setAttribute("value",(url_config[file]["modelConfigFile"]["def"][ii]["targets"][target]||""));
		inp.setAttribute("style","width:100%");
		inp.setAttribute("onblur","url_setConfigFilesDefault("+ii+",'"+target+"',this.value);");
		td.appendChild(inp);
		row.appendChild(td);
	    }
	    td=document.createElement("TD");
	    td.setAttribute("class","fill");
	    inp=document.createElement("INPUT");
	    inp.setAttribute("type","text");
	    inp.setAttribute("value",(url_config[file]["modelConfigFile"]["def"][ii]["info"]||""));
	    inp.setAttribute("style","width:100%");
	    inp.setAttribute("onblur","url_setConfigFilesDefaultInfo("+ii+",this.value);");
	    td.appendChild(inp);
	    row.appendChild(td);
	    // make add row to table
	    item.parentNode.insertBefore(row,item);
	};
    }
    return;
}

// create model default table newline
function url_insertModelDefaultNewline(row,file) {
    var td,btn,inp;
    // make "-" column
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","url_newModelDefault(this)");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("+");
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    var targets=url_config[file]["modelConfigFile"]["targets"];
    for (var target in targets) {
	// make variable names
	td=document.createElement("TD");
	td.setAttribute("class","fill");
	td.setAttribute("id","_"+target);
	inp=document.createElement("INPUT");
	inp.setAttribute("type","text");
	inp.setAttribute("value","");
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","this.value=this.value.replace(/[^\\d\\.]/g,'')");
	td.appendChild(inp);
	row.appendChild(td);
    }
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    td.setAttribute("id","information");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value","");
    inp.setAttribute("style","width:100%");
    td.appendChild(inp);
    row.appendChild(td);
}
function url_removeModelDefault(item,file,ii) {
   //console.log("removing model default:",file,ii);
    url_config[file]["modelConfigFile"]["def"].splice(ii,1);
    //url_showModelDefaultTable();
    url_show();
};
function url_showObsTargetTable() {
    var item=document.getElementById('obsTargetTable');
    var ofile=url_getObsConfigFile();
    var file=url_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsObsTarget","newlineObsTarget");
    // insert obs targets from obs-config file
    var otargets=obs_config[ofile]["targets"];
    for (var target in otargets) {
	url_insertOTargetRow(tail,target,otargets[target]["pos"],otargets[target]["descr"],
			     otargets[target]["info"],otargets[target]["min"],otargets[target]["max"]);
    }
    // insert obs target index expression from obs-config file
    // make "-" column  ***************************
    var row = document.createElement("TR");
    var td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make NAME column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","color:green");
    var target=obs_config[ofile]["indexTarget"];
    td.innerHTML=target;
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("colspan","4");
    td.innerHTML=obs_config[ofile]["indexExp"];
    row.appendChild(td);
    // make minimum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",url_config[file]["obsConfigFile"]["start"]);
    inp.setAttribute("id","urlObsIndexStart");
    inp.setAttribute("style","width:125px");
    inp.setAttribute("onblur","url_setArray('obsConfigFile','start',this.value);");
    td.appendChild(inp);
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","url_getObsIndexStart('urlObsIndexStart','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("←"); // "→"
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    // make maximum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",url_config[file]["obsConfigFile"]["stop"]);
    inp.setAttribute("id","urlObsIndexStop");
    inp.setAttribute("style","width:125px");
    inp.setAttribute("onblur","url_setArray('obsConfigFile','stop',this.value);");
    td.appendChild(inp);
    btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","url_getObsIndexStop('urlObsIndexStop','"+target+"')");
    btn.setAttribute("style","width:25px");
    var t=document.createTextNode("→"); // "←"
    btn.appendChild(t);
    td.appendChild(btn);
    row.appendChild(td);
    tail.parentNode.insertBefore(row,tail);
    // insert obs targets from url-config file
    var targets=url_config[file]["obsConfigFile"]["targets"];
    for (var target in targets) {
	url_insertObsTargetRow(tail,target,targets[target]["pos"],targets[target]["descr"],
			       targets[target]["info"],targets[target]["min"],targets[target]["max"]);
    }
};
// create exec table row
function url_insertOTargetRow(item,target,pos,descr,info,min,max) {
    var row = document.createElement("TR");
    var td, inp;
    // make "-" column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make NAME column  ***************************
    td=document.createElement("TD");
    td.innerHTML=target;
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.innerHTML=pos;
    row.appendChild(td);
    // make select-pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make descr column  ***************************
    td=document.createElement("TD");
    td.innerHTML=descr;
    row.appendChild(td);
    // make info column  ***************************
    td=document.createElement("TD");
    td.innerHTML=info;
    row.appendChild(td);
    // make min column  ***************************
    td=document.createElement("TD");
    td.innerHTML=min;
    row.appendChild(td);
    // make max column  ***************************
    td=document.createElement("TD");
    td.innerHTML=max;
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
// create exec table row
function url_insertObsTargetRow(item,target,pos,descr,info,min,max) {
    var row = document.createElement("TR");
    var td, inp;
    // make "-" column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    var btn=document.createElement("BUTTON");
    btn.setAttribute("onclick","removeObsTarget(this.parentNode.parentNode,'"+target+"')");
    btn.setAttribute("style","width:100%");
    var t=document.createTextNode("-");
    btn.appendChild(t);
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
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('obsConfigFile','"+target+"','pos',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make select-subtype column  ***************************
    td=document.createElement("TD");
    td.setAttribute("style","min-width:25px;width:25px");
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",descr);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('obsConfigFile','"+target+"','descr',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make pos column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",info);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('obsConfigFile','"+target+"','info',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make minimum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",min);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('obsConfigFile','"+target+"','min',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make maximum column  ***************************
    td=document.createElement("TD");
    td.setAttribute("class","fill");
    inp=document.createElement("INPUT");
    inp.setAttribute("type","text");
    inp.setAttribute("value",max);
    inp.setAttribute("style","width:100%");
    inp.setAttribute("onblur","url_setConfigFilesTarget('obsConfigFile','"+target+"','max',this.value);");
    td.appendChild(inp);
    row.appendChild(td);
    // make add row to table  ***************************
    item.parentNode.insertBefore(row,item);
    return row;
}
function url_showTargetMatchTable() {
    var item=document.getElementById('targetMatchTable');
    var file=url_getConfigFile();
    var tail=removeTableChildFromTo(item,"labelsTargetMatch","newlineTargetMatch");
    var targets=url_config[file]["matchRules"]["targets"];
    for (var target in targets) {
	url_insertTargetMatchRow(tail,target,targets[target]["exp"]);
    };
    url_check('matchExpression','urlDebugExpression');

};
// create exec table row
function url_insertTargetMatchRow(item,target,expr) {
    var row = document.createElement("TR");
    var file = url_getConfigFile();
    if (url_config[file]!== undefined){
	var mfile = url_getModelConfigFile();
	var ofile = url_getObsConfigFile();
	var td, inp;
	// make "-" column  ***************************
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px");
	var btn=document.createElement("BUTTON");
	btn.setAttribute("onclick","url_removeTargetMatch(this.parentNode.parentNode,'"+target+"')");
	btn.setAttribute("style","width:100%");
	var t=document.createTextNode("-");
	btn.appendChild(t);
	td.appendChild(btn);
	row.appendChild(td);
	// make model target column  ***************************
	td=document.createElement("TD");
	if (model_config[mfile] !== undefined) {
	    if (url_config[file]["modelConfigFile"]["targets"][target] !== undefined) {
		var variable=url_config[file]["modelConfigFile"]["targets"][target]["variable"];
		var mark=(variable === model_config[mfile]["index"]);
		if (mark) {
		    td.setAttribute("style","color:blue");
		};
	    } else {
		td.setAttribute("style","color:red");
	    }
	};
	td.innerHTML=target;
	row.appendChild(td);
	// make select-model target column  ***************************
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px");
	row.appendChild(td);
	// make obs target expression column  ***************************
	td=document.createElement("TD");
	td.setAttribute("class","fill");
	if (obs_config[ofile] !== undefined) {
	    var variable=obs_config[ofile]["indexTarget"];
	    var mark=(expr.indexOf(variable) > -1);
	    if (mark) {
		td.setAttribute("style","color:green");
	    };
	};
	inp=document.createElement("INPUT");
	inp.setAttribute("type","text");
	inp.setAttribute("value",expr);
	inp.setAttribute("style","width:100%");
	inp.setAttribute("onblur","url_setConfigFilesTarget('matchRules','"+target+"','exp',this.value);url_showTargetMatchTable();");
	td.appendChild(inp);
	row.appendChild(td);
	// make select-obs target expression column  ***************************
	td=document.createElement("TD");
	td.setAttribute("style","min-width:25px;width:25px");
	row.appendChild(td);
	// make add row to table  ***************************
	item.parentNode.insertBefore(row,item);
	return row;
    }
};
function url_removeTargetMatch(item,target) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	var item=document.getElementById("newlineTargetMatch");
	item.children[1].children[0].value=target;
	item.children[3].children[0].value=url_config[file]["matchRules"]["targets"][target]["exp"];
	delete url_config[file]["matchRules"]["targets"][target];
	//url_showTargetMatchTable();
	url_show();
    };
};
// URL methods
function url_show() {
    var item;
    var file=url_getConfigFile();
    var mod=(url_config[file]["modelConfigFile"]["file"] !== "");
    var obs=(url_config[file]["obsConfigFile"]["file"] !== "");
    if (mod) {
	(document.getElementById("displayModelTargets")).setAttribute("style","");
	url_showModelTargetTable();
	url_showModelDefaultTable();
    } else {
	(document.getElementById("displayModelTargets")).setAttribute("style","display:none");
	(document.getElementById("displayModelDefault")).setAttribute("style","display:none");
    };
    if (obs) {
	url_showObsTargetTable();
	(document.getElementById("displayObsTargets")).setAttribute("style","");
    } else {
	(document.getElementById("displayObsTargets")).setAttribute("style","display:none");
    }
    if (mod & obs) {
	(document.getElementById("displayMatchRules")).setAttribute("style","");
	(document.getElementById("displayModelDefault")).setAttribute("style","display:none");
	url_showTargetMatchTable();
	url_check('matchExpression','urlDebugExpression');
    }else {
	(document.getElementById("displayMatchRules")).setAttribute("style","display:none");
	(document.getElementById("urlDebugExpression")).setAttribute("style","display:none");
	if (mod) {
	    (document.getElementById("displayModelDefault")).setAttribute("style","");
	} else {
	    (document.getElementById("displayModelDefault")).setAttribute("style","display:none");
	}
	url_check('matchExpression','urlDebugExpression');
    }
    showValue('urlConfigFile',file);
    showValue('urlModelConfigFile',url_config[file]["modelConfigFile"]["file"]);
    showValue('urlObsConfigFile',url_config[file]["obsConfigFile"]["file"]);
    url_showURL();
}

function url_showURL() {
    var file=url_getConfigFile();
    var host=url_config[file]["host"];
    var href="http://"+host+"/cgi-bin/fark_url.pl?urlFile="+file;
    // if (mod) {
    // 	href=href+"?modelFile="+url_config[file]["modelConfigFile"]["file"];
    // 	if (url_config[file]["modelConfigFile"]["start"]) {
    // 	    href=href+"?modelStart="+ url_config[file]["modelConfigFile"]["start"];
    // 	};
    // 	if (url_config[file]["modelConfigFile"]["stop"]) {
    // 	    href=href+"?modelStop="+ url_config[file]["modelConfigFile"]["stop"];
    // 	};
    // 	var targets=url_config[file]["modelConfigFile"]["targets"];
    // 	var modelTargets="";
    // 	for (var target in targets) {
    // 	    modelTargets=modelTargets+target+"~"+targets[target]["variable"]+"~"+targets[target]["min"]+"~"+targets[target]["max"]+"|";
    // 	}
    // 	href=href+"?modelTargets="+modelTargets;
    // 	if (! obs) {
    // 	    var modelDefs="";
    // 	    var defs=url_config[file]["modelConfigFile"]["def"];
    // 	    var len=defs.length;
    // 	    for (var ii=0;ii<len;ii++){
    // 		var first=true;
    // 		for (var target in defs[ii]["targets"]){
    // 		    if (first) {modelDefs=modelDefs+"[";first=false;}
    // 		    modelDefs=modelDefs+target+"~"+defs[ii]["targets"][target]+"|";
    // 		}
    // 	    };
    // 	    href=href+"?modelDefault="+modelDefs;
    // 	}
    // };
    // if (obs) {
    // 	href=href+"?obsFile="+url_config[file]["obsConfigFile"]["file"];
    // 	if (url_config[file]["obsConfigFile"]["start"]) {
    // 	    href=href+"?obsStart="+ url_config[file]["obsConfigFile"]["start"];
    // 	};
    // 	if (url_config[file]["obsConfigFile"]["stop"]) {
    // 	    href=href+"?obsStop="+ url_config[file]["obsConfigFile"]["stop"];
    // 	};
    // 	var targets=url_config[file]["obsConfigFile"]["targets"];
    // 	var obsTargets="";
    // 	for (var target in targets) {
    // 	    obsTargets=obsTargets+target+"~"+
    // 		targets[target]["pos"]+"~"+
    // 		targets[target]["descr"]+"~"+
    // 		targets[target]["min"]+"~"+
    // 		targets[target]["max"]+"|";
    // 	}
    // 	href=href+"?obsTargets="+obsTargets;
    // 	if (mod) {
    // 	    var targets=url_config[file]["matchRules"]["targets"];
    // 	    var matchRules="";
    // 	    for (var target in targets) {
    // 		matchRules=matchRules+target+"~"+targets[target]["exp"]+"|";
    // 	    }
    // 	    href=href+"?matchRules="+matchRules;
    // 	}
    // };
    document.getElementById("urlLink").innerHTML=href;
    document.getElementById("urlLink").href=href;
    document.getElementById("urlLink").target="_blank";
}

function url_removeModelTarget(item,target) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	var item=document.getElementById("newlineModelTarget");
	item.children[1].children[0].value=target;
	item.children[2].children[0].value=url_config[file]["modelConfigFile"]["targets"][target]["variable"];
	item.children[4].children[0].value=url_config[file]["modelConfigFile"]["targets"][target]["min"];
	item.children[5].children[0].value=url_config[file]["modelConfigFile"]["targets"][target]["max"];
	delete url_config[file]["modelConfigFile"]["targets"][target];
	if (obs_isEmpty(url_config[file]["modelConfigFile"]["targets"])) {
	    delete url_config[file]["modelConfigFile"]["def"];
	    url_config[file]["modelConfigFile"]["def"]=[];
	}
	//url_showModelTargetTable();
	//url_showModelDefaultTable();
	url_show();
    }
};
function removeObsTarget(item,target) {
    var file=url_getConfigFile();
    if (url_config[file] !== undefined) {
	var item=document.getElementById("newlineObsTarget");
	item.children[1].children[0].value=target;
	item.children[2].children[0].value=url_config[file]["obsConfigFile"]["targets"][target]["pos"];
	item.children[4].children[0].value=url_config[file]["obsConfigFile"]["targets"][target]["descr"];
	item.children[5].children[0].value=url_config[file]["obsConfigFile"]["targets"][target]["info"];
	item.children[6].children[0].value=url_config[file]["obsConfigFile"]["targets"][target]["min"];
	item.children[7].children[0].value=url_config[file]["obsConfigFile"]["targets"][target]["max"];
	delete url_config[file]["obsConfigFile"]["targets"][target];
	if (obs_isEmpty(url_config[file]["obsConfigFile"]["targets"])) {
	    delete url_config[file]["obsConfigFile"]["def"];
	    url_config[file]["obsConfigFile"]["def"]=[];
	}
	//url_showObsTargetTable();
	url_show();
    }
};


function url_saveConfigFile(target) {
    var file=url_getConfigFile();
    var password=document.getElementById("urlConfigFilePsw").value;
    var host = url_config[file]["host"];
    var modelFile = url_config[file]["modelConfigFile"]["file"];
    var modelStart = url_config[file]["modelConfigFile"]["start"];
    var modelStop = url_config[file]["modelConfigFile"]["stop"];
    var modelTargets = "";
    var modelTrg=url_config[file]["modelConfigFile"]["targets"];
    for (var target in modelTrg) {
	modelTargets=modelTargets + "|" + target + "~" + 
	    modelTrg[target]["variable"] + "~" + 
	    modelTrg[target]["min"] + "~" + 
	    modelTrg[target]["max"];
    };
    var modelDefault = "";
    var modelDef=url_config[file]["modelConfigFile"]["def"];
    var len=modelDef.length;
    for (var ii=len-1; ii>=0;ii--) {
	var info=modelDef[ii]["info"];
	var defTrg=modelDef[ii]["targets"];
	var first=true;
	for (var target in defTrg) {
	    if (first) {
		modelDefault=modelDefault + "[" + info;
		first=false;
	    };
	    modelDefault=modelDefault + "|" + 
		target + "~" + 
		defTrg[target];
	};
    };
    var obsFile = url_config[file]["obsConfigFile"]["file"];
    var obsStart = url_config[file]["obsConfigFile"]["start"];
    var obsStop = url_config[file]["obsConfigFile"]["stop"];
    var obsTargets = "";
    var obsTrg=url_config[file]["obsConfigFile"]["targets"];
    for (var target in obsTrg) {
	obsTargets=obsTargets + "|" + target + "~" + 
	    obsTrg[target]["pos"] + "~" + 
	    obsTrg[target]["descr"] + "~" + 
	    obsTrg[target]["info"] + "~" + 
	    obsTrg[target]["min"] + "~" + 
	    obsTrg[target]["max"];
    };
    var matchRules = "";
    var matchTrg=url_config[file]["matchRules"]["targets"];
    for (var target in matchTrg) {
	matchRules=matchRules + "|" + target + "~" + 
	    matchTrg[target]["exp"];
    };
    documentLog.innerHTML="Sent url-save request.";
    $.get("cgi-bin/fark_save.pl",{type:"url",
				  file:file,
				  host:host,
				  password:password,
				  modelFile:modelFile,
				  modelStart:modelStart,
				  modelStop:modelStop,
				  obsFile:obsFile,
				  obsStart:obsStart,
				  obsStop:obsStop,
				  obsTargets:obsTargets,
				  modelTargets:modelTargets,
				  modelDefault:modelDefault,
				  matchRules:matchRules})
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
	    function (error) { alert("URL save request failed (system error)");}
	);
};
function url_showConfig() {
    var file=url_getConfigFile();
    if (url_config[file] === undefined) { // create new entry locally...
	url_config[file]={modelConfigFile:{targets:{},def:{}},
			  obsConfigFile:{targets:{}},
			  matchRules:{targets:{}},
			  host:"localhost",
			  password:""};
	url_config[file]["modelConfigFile"]["file"]=document.getElementById("urlModelConfigFile").value;
	url_config[file]["obsConfigFile"]["file"]=document.getElementById("urlObsConfigFile").value;
	// model targets
	var item=document.getElementById("modelTargetTable");
	var tbody=item.children[0];
	var children=tbody.children;
	var len=children.length;
	url_config[file]["modelConfigFile"]["targets"]={};
	for (var ii=len-1;ii>=0;ii--){
	    //console.log("RemoveTableChildFromTo ",ii,children[ii],len,children.length);
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsModelTarget" & att !== "newlineModelTarget") {
			var name=children[ii].children[1].innerHTML;
			var variable=children[ii].children[2].children[0].value;
			var min=children[ii].children[4].children[0].value;
			var max=children[ii].children[5].children[0].value;
			url_config[file]["modelConfigFile"]["targets"][name]=
			    {variable:variable,min:min,max:max};;
		    }
		}
	    }
	};
	// model default
	item=document.getElementById("modelDefaultTable");
	tbody=item.children[0];
	children=tbody.children;
	len=children.length;
	url_config[file]["modelConfigFile"]["def"]=[];
	for (var ii=len-1;ii>=0;ii--){
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsModelDefault" & att !== "newlineModelDefault") {
			var clen=children[ii].children.length;
			var info=children[ii].children[clen-1].children[0].value;
			var targets={};
			for (var jj=0;jj<clen-1;jj++) {
			    var trg=children[ii].children[jj].getAttribute("trg");
			    if (trg !== undefined) {
				var value=children[ii].children[jj].children[0].value;
				targets[trg]=value;
			    } else {
				console.log("Warning unknown target detected.");
			    }
			}
			url_config[file]["modelConfigFile"]["def"].push({targets:targets,info:info});
		    }
		}
	    }
	};
	// obs targets
	item=document.getElementById("obsTargetTable");
	tbody=item.children[0];
	children=tbody.children;
	len=children.length;
	url_config[file]["obsConfigFile"]["targets"]={};
	for (var ii=len-1;ii>=0;ii--){
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsObsTarget" & att !== "newlineObsTarget") {
			var name=children[ii].children[1].innerHTML;
			var bufrType=children[ii].children[2].children[0].value;
			var subType=children[ii].children[4].children[0].value;
			var pos=children[ii].children[6].children[0].value;
			var descr=children[ii].children[8].children[0].value;
			var min=children[ii].children[9].children[0].value;
			var max=children[ii].children[10].children[0].value;
			url_config[file]["obsConfigFile"]["targets"][name]=
			    {bufrType:bufrType,subType:subType,pos:pos,descr:descr,min:min,max:max};
		    }
		}
	    }
	};
	// match rules
	item=document.getElementById("targetMatchTable");
	tbody=item.children[0];
	children=tbody.children;
	len=children.length;
	url_config[file]["matchRules"]["targets"]={};
	for (var ii=len-1;ii>=0;ii--){
	    if (children[ii] !== undefined) {
		if (children[ii].getAttribute !== undefined) {
		    var att=children[ii].getAttribute("id");
		    if (att !== "labelsTargetMatch" & att !== "newlineTargetMatch") {
			var name=children[ii].children[1].innerHTML;
			var expr=children[ii].children[3].children[0].value;
			url_config[file]["matchRules"]["targets"][name]=
			    {exp:expr};
		    }
		}
	    }
	};
	url_configEd++;
    } else { // load local values to screen
	showValue('urlModelConfigFile',url_config[file]["modelConfigFile"]["file"]);
	showValue('urlObsConfigFile',url_config[file]["obsConfigFile"]["file"]);
	url_show();
    }
};
function url_updateData() {
    documentLog.innerHTML="Sent url-load request.";
    $.get("cgi-bin/fark_load.pl",{type:"url"})
	.success(
	    function(data, status){
		dataToArray(data,status,documentLog);
		if (! modelLoaded) {
		    documentLog.innerHTML="Sent model-load request.";
		    $.get("cgi-bin/fark_load.pl",{type:"model"})
			.success(
			    function(data, status){
				dataToArray(data,status,documentLog);
				modelLoaded=true;
				if (! obsLoaded) {
				    documentLog.innerHTML="Sent obs-load request.";
				    $.get("cgi-bin/fark_load.pl",{type:"obs"})
					.success(
					    function(data, status){
						dataToArray(data,status,documentLog);
						obsLoaded=true;
						url_show();
						documentLog.innerHTML="";
					    });
				} else {
				    url_show();
				    documentLog.innerHTML="";
				}
			    });
		} else if (!obsLoaded) {
		    documentLog.innerHTML="Sent obs-load request.";
		    $.get("cgi-bin/fark_load.pl",{type:"obs"})
			.success(
			    function(data, status){
				dataToArray(data,status,documentLog);
				obsLoaded=true;
				url_show();
				documentLog.innerHTML="";
			    });
		} else {
		    url_show();
		    documentLog.innerHTML="";
		}
	    })
	.error(
	    function (error) { alert("URL update request failed (system error)");}
	);
};
function url_getModelIndexStart(inp,target) {
    var file=url_getModelConfigFile();
    var item=document.getElementById(inp);
    item.value=Number(model_config[file]["start"]).toString();
    url_setConfigFilesTarget('modelConfigFile',target,'min',model_config[file]["start"]);
};
function url_getModelIndexStop(inp,target) {
    var file=url_getModelConfigFile();
    var item=document.getElementById(inp);
    item.value=Number(model_config[file]["stop"]).toString();
    url_setConfigFilesTarget('modelConfigFile',target,'max',model_config[file]["stop"]);
};
function url_getObsIndexStart(inp,target) {
    var file=url_getObsConfigFile();
    var item=document.getElementById(inp);
   //console.log("fark.js start:",file,obs_config[file]["start"])
    item.value=obs_config[file]["start"];
    url_setArray('obsConfigFile','start',obs_config[file]["start"]);
};
function url_getObsIndexStop(inp,target) {
    var file=url_getObsConfigFile();
    var item=document.getElementById(inp);
    item.value=obs_config[file]["stop"];
    url_setArray('obsConfigFile','stop',obs_config[file]["stop"]);
};
function url_getObsTargetBufrType() {
    var item=document.getElementById("obsTargetTable");
    var newline=getChild(item,"newlineObsTarget");
    return newline.children[2].children[0].value;
};
function url_getObsTargetSubType() {
    var item=document.getElementById("obsTargetTable");
    var newline=getChild(item,"newlineObsTarget");
    return newline.children[4].children[0].value;
};
function url_copyExp(f,t) {
    var fitem=document.getElementById(f);
    var titem=document.getElementById(t);
    titem.value=fitem.value;
};
function url_check(f,t) {
    var fitem=document.getElementById(f);
    var titem=document.getElementById(t);
    var exp=fitem.value||"";
    if (exp === "") {
	titem.setAttribute("style","display:none");
    } else {
	titem.setAttribute("style","");
    }
};
function url_debugExp(f,t) {
    var fitem=document.getElementById(f);
    var titem=document.getElementById(t);
    var expin=fitem.value;
    documentLog.innerHTML="Sent url-exp request:"+expin;
    $.get("cgi-bin/fark_exp.pl",{exp:expin})
	.success(
	    function(data, status){
		if (status === "success" && data !== null) {
		    var errors=data.getElementsByTagName("error");
		    if (errors.length > 0 ) {
			var msg=getErrorMessage(errors);
			alert("Unable to evaluate expression:"+expin+"\n"+msg);
		    } else {
			var results=data.getElementsByTagName("result");
			if (results.length > 0 ) {
			    var val=(results[0].getAttribute("value")||"");
			    //titem.innerHTML=val;
			    titem.innerHTML=Number(val).toString();
			};
		    };
		    documentLog.innerHTML="";
		};
	    })
	.error(
	    function (error) { alert("URL debug request failed (system error)");}
	);
};
