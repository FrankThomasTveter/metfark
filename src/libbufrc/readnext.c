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
	readnext.c
*/
#include "bufrgrib.h"
#include "fortint.h"
#include "pbio-if.h"
#ifndef NULL
#define NULL 0
#endif

#include "fileRead.h"


fortint readnext(char * buffer, fortint * grib_prod_len, 
              fortint (*read_func)(char *, fortint , void *), void * stream)
/*

    buffer        = buffer big enough to hold the next product.
                    If buffer = NULL, the function will return the 
                    length of the product, but not the product itself.

    grib_prod_len = size of the buffer on input, becomes size in BYTES of 
                    the next product read.  If the end-of-file is hit, the
                    value is returned unchanged (ie. when the function return
                    code is -1).

    read_func     = function to read input stream

    stream        = data for read_function, eg. file pointer returned 
                    from PBOPEN

    Function returns:

        0  if a product has been successfully read

       -1  if end-of-file is hit before a product is read

       -2  if there is an error in the file-handling 
	   (eg. if the file contains a truncated product)

       -3  if the size of buffer is not sufficient for the product

       -4  if buffer too small to start in on product

*/
{

/* Read the product */
fortint length;
fortint original_len;

    original_len = *grib_prod_len;
    length =  readprod(NULL,buffer,&original_len,fileRead,fileSeek,fileTell,
                       stream);
    *grib_prod_len = original_len;

    if ( buffer == NULL )
        return ( length == -1 ? length : -3 );
    else
        return ( length > 0 ? 0 : length );

}
