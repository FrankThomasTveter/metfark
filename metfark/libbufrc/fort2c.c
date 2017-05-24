/**
* Copyright 1981-2012 ECMWF. 
* 
* This software is licensed under the terms of the GNU Lesser 
* General Public License Version 3 which can be obtained at 
* http://www.gnu.org/licenses/lgpl.html.  
* 
* In applying this licence, ECMWF does not waive the privileges 
* and immunities granted to it by virtue of its status as an 
* intergovernmental organisation nor does it submit to any
* jurisdiction. 
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
/*
	fort2c.c
*/
#include "fort2c.h"

char *fcd2char(fortchar)
_fcd fortchar;
{
#ifndef VAX
	char *name = _fcdtocp(fortchar);
	int   len  = _fcdlen(fortchar);
#else
        char * name =  fortchar->address;
        short len =  fortchar->length;
#endif

        char * newp = (char *) malloc(len+1);
 
	if(!newp) 
	{
		perror("malloc");
		return((char*)0);
	}

	strncpy(newp,name,len);

	while( len && newp[--len] == ' ');

	newp[++len]='\0';

	return(newp);
}


