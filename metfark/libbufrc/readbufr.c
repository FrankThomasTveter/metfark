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
	readbufr.c
*/
#include "bufrgrib.h"
#include "fortint.h"
#include "pbio-if.h"
#include "fileRead.h"


fortint readbufr(FILE * file, char * buffer, fortint * bufr_prod_len)
/*

    file          =  file pointer returned from PBOPEN

    buffer        = buffer big enough to hold the BUFR product

    bufr_prod_len = size of the buffer on input, becomes size in BYTES of 
                    the BUFR product read.  If the end-of-file is hit, the
                    value is returned unchanged (ie. when the function return
                    code is -1).

    Function returns:

        0  if a BUFR product has been successfully read

       -1  if end-of-file is hit before a BUFR product is read

       -2  if there is an error in the file-handling 
	   (eg. if the file contains a truncated BUFR product)

       -3  if the size of buffer is not sufficient for the BUFR product

*/
{

/* Read the BUFR product */
fortint length;
fortint original_len;

    original_len = *bufr_prod_len;
    length =  readprod("BUFR",buffer,&original_len,fileRead,fileSeek,
                       fileTell,file);
    *bufr_prod_len = original_len;

    if ( buffer == NULL )
        return ( length == -1 ? length : -3 );
    else
        return ( length > 0 ? 0 : length );

}
