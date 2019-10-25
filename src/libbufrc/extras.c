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

/*
	extras.c
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pbio-if.h"
 
static char *putfstr(char *target, char *source, int length_target)
{
int i;
 
/*  Move characters from C source to Fortran target */
    strcpy(target,source);

/*  Pad Fortran target to end with spaces */
    for(i = strlen(source); i<length_target; i++)
        target[i] = ' ';
 
    return target;
}

long f_getenv_(char * env_string, char * env_info,
              int len_env_string, int len_env_info)
{
char *e, *env_copy;

/*  Copy Fortran string and remove trailing blanks */
    env_copy = (char *) malloc(len_env_string+1);
    if( env_copy == NULL )
    {
        perror("F_GETENV: malloc error");
        exit(1);
    }
    strncpy(env_copy, env_string, len_env_string);
    for( e = (env_copy+len_env_string-1); *e == ' '; e-- ) *e = '\0';
    
/*  Get the environment information and return it to the Fortran caller */
    if(e = (char*)getenv(env_copy))
    {
        putfstr(env_info, e, len_env_info);
        free( env_copy );
        return 1;
    }
    else
    {
        free( env_copy );
        return 0;
    }
}

long f_getenv(char * env_string, char * env_info,
              int len_env_string, int len_env_info) {

  return f_getenv_(env_string,env_info,len_env_string,len_env_info);
}

long f_getenv__(char * env_string, char * env_info,
              int len_env_string, int len_env_info) {

  return f_getenv_(env_string,env_info,len_env_string,len_env_info);
}
