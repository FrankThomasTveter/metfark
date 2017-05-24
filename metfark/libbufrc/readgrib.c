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
	readgrib.c
*/
#include "bufrgrib.h"
#include "fortint.h"
#include "pbio-if.h"
#include "fileRead.h"

#include "sizeRoutines.h"


fortint readgrib(FILE * file, char * buffer, fortint * grib_prod_len)
/*

    file          =  file pointer returned from PBOPEN

    buffer        = buffer big enough to hold the GRIB product

    grib_prod_len = size of the buffer on input, becomes size in BYTES of 
                    the GRIB product read.  If the end-of-file is hit, the
                    value is returned unchanged (ie. when the function return
                    code is -1).

    Function returns:

        0  if a GRIB product has been successfully read

       -1  if end-of-file is hit before a GRIB product is read

       -2  if there is an error in the file-handling 
	   (eg. if the file contains a truncated GRIB product)

       -3  if the size of buffer is not sufficient for the GRIB product

*/
{

/* Read the GRIB product */
fortint length;
fortint original_len;

    original_len = *grib_prod_len;
    length =  readprod("GRIB",buffer,&original_len,fileRead,fileSeek,
                        fileTell,file);
    *grib_prod_len = original_len;

    if ( buffer == NULL )
        return ( length == -1 ? length : -3 );
    else
        return ( length > 0 ? 0 : length );

}
