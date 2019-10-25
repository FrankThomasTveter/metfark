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
#include "fortint.h"
#include "fileRead.h"

#define BUFFLEN 4096

void lwsize_(
  fortint ** unit,
  fortint * plen) {
/*
//  Returns the size in bytes of the next GRIB, BUFR, TIDE, BUDG, DIAG
//  product.
//
//  Called from C:
//      LWSIZE((fortint **) &unit, &plen);
//
//  unit  = file id returned from C fopen64.
//  plen  = size in bytes of the next product.
//        = 0 if no product found
//
//  The input file is left positioned where it started.
*/
fortint iret;
char statbuff[BUFFLEN];
long offset;

/*
//  Record the current position of the file
*/
    offset = (fortint) fileTell(*unit);
    if( offset < 0 ) {
      perror("LWSIZE: fileTell error");
      exit(1);
    }
/*
//  Use a smallish buffer for processing; this should suffice for all cases
//  except versions -1 and 0 of GRIB and large BUFR products
*/
    *plen = BUFFLEN;

    iret = readprod(NULL,statbuff,plen,fileRead,fileSeek,fileTell,*unit);
    if( (iret < 0) && (iret != -3) && (iret != -1) ) {
      *plen = 0;
      return;
    }
/*
//  Put the file pointer back where it started
*/
    offset = (fortint) fileSeek( *unit, offset, SEEK_SET);

    return ;
}

void lwsize(fortint** unit,fortint* plen) {

  lwsize_(unit,plen);
}

void lwsize__(fortint** unit,fortint* plen) {

  lwsize_(unit,plen);
}
