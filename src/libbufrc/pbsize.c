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

#include <stdio.h>
#include <stdlib.h>
#ifndef VAX
#include <unistd.h>
#endif
#include "bufrgrib.h"
#include "pbio-if.h"
#include "fortint.h"

#define BUFFLEN 4096

#include "fileRead.h"

void PBSIZE_(FILE ** in, fortint * plen) {
     PBSIZE(in,plen);
}

void PBSIZE__(FILE ** in, fortint * plen) {
     PBSIZE(in,plen);
}

void PBSIZE(FILE ** in, fortint * plen)
/*
    Returns the size in bytes of the next GRIB, BUFR, TIDE, BUDG, DIAG
    product.

    Called from FORTRAN:
        CALL PBSIZE( KUNIT, LENGTH)

    in    = file id returned from PBOPEN.
    plen  = size in bytes of the next product.
          = -2 if error allocating memory for internal buffer.

    The input file is left positioned where it started.
*/
{
fortint iret;
char statbuff[BUFFLEN];
char * buff;
long offset, loop = 1;

/*  Use a smallish buffer for processing; this should suffice for all cases
    except versions -1 and 0 of GRIB and large BUFR products */

    offset = (fortint) fileTell( *in);
    *plen = BUFFLEN;
    iret = readprod(NULL,statbuff,plen,fileRead,fileSeek,fileTell,*in);
    if( iret == -2 )
    {
        printf("readprod error %d\n", iret);
        *plen = -2;
        return;
    }

/*  If the smallish buffer is too small, progressively increase it until 
    big enough */

    while ( iret == -4 )
    {
        loop++;
        buff = (char *) malloc( BUFFLEN*loop);
        if( buff == NULL) 
        {
            perror("malloc failed in PBSIZE");
            *plen = -2;
            return;
        }
        *plen = BUFFLEN*loop;
        offset = (fortint) fileSeek( *in, offset, SEEK_SET);
        offset = (fortint) fileTell( *in);
        iret = readprod(NULL,buff,plen,fileRead,fileSeek,fileTell,*in);
        free(buff);
    }

    if( iret == -2 )
    {
        printf("readprod error %d\n", iret);
        *plen = -2;
    }
        
/*  Put the file pointer back where it started */

    offset = (fortint) fileSeek( *in, offset, SEEK_SET);

    return ;
}
