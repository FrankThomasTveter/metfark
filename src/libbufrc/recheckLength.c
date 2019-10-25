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
#include <string.h>
#include "pbio-if.h"

long len3oct(char *p) {
unsigned char A, B, C;

  A = *p;
  B = *(p+1);
  C = *(p+2);

  return( ((long)(A)*65536) + ((long)(B)*256) + (long)(C) );
}

#define GRIB 0x47524942
#define BIT1 0x80
#define BIT2 0x40

static int grab(char * , char * , long ,long ,long * );

long recheckLength(char * buffer)
{
/*
//  On entry, buffer contains a GRIB edition 1 product (somewhere).
//
//  The value returned is the length of the GRIB product calculated
//  from the individual lengths of sections 0, 1, 2, 3, 4 and 5;
//  sections 2 and 3 are optional and may or may not be present.
//
//  If there is a problem processing the product, or if GRIB edition
//  -1 or 0 is encountered, the return value is -1.
*/
int large = 0;
int found = 0;
int code = 0;
long bytes_read = 0, advance;
char p, edit_num, flag23;
char size[3];
int section0 = 8, section1, section2, section3, section4, section5 = 4;
long total;
char grp_7777[5];
long s0;

/*  Read bytes until "GRIB" found */

    do
    {
        if( grab(buffer, &p, 1, 1, &bytes_read) != 0) return (-1);
        code = ( (code << 8) + p ) & 0xFFFFFFFF;
        if (code == GRIB ) found = 1;
    } while ( ! found );
    s0 = bytes_read - 4;
    bytes_read = 4;

    if( grab(buffer, size, 3, 1, &bytes_read) != 0) return (-1);
    total = len3oct(size);

    if( total > 0x800000 ) {
        total = (total&0x7fffff) * 120;
        large = 1;
    }

/*  Check the edition number */

    if( grab(buffer, &edit_num, 1, 1, &bytes_read) != 0) return (-1);
    if( edit_num != 1 ) {
      printf("Cannot handle GRIB edition 0\n");
      return (-1);
    }                             /* reject edition 0 */

    if( (*(buffer+21-s0) == '\0') && (*(buffer+22-s0) == '\0') ) {
      printf("Cannot handle GRIB edition -1\n");
      return (-1);
    }                             /* reject edition -1 */

/*  Read length of section 1 */
    if( grab(buffer, size, 3, 1, &bytes_read) != 0) return (-1);
    section1 = len3oct(size);

/*  Now figure out if sections 2/3 are present */

    advance = 4;
    bytes_read += advance;
    if( grab(buffer, &flag23, 1, 1, &bytes_read) != 0) return (-1);
    section2 = flag23 & BIT1;
    section3 = flag23 & BIT2;

/*  Advance to end of section 1 */

    advance = section1 - (bytes_read - section0);
    bytes_read += advance;

/*  Read section 2 length if it is given*/

    if( section2 )
    {
        if( grab(buffer, size, 3, 1, &bytes_read) != 0) return (-1);
        section2 = len3oct(size);
        advance = section2 - (bytes_read - section0 - section1);
        bytes_read += advance;
    }

/*  Read section 3 length if it is given*/

    if( section3 )
    {
        if( grab(buffer, size, 3, 1, &bytes_read) != 0) return (-1);
        section3 = len3oct(size);
        advance = section3 - (bytes_read - section0 - section1 - section2);
        bytes_read += advance;
    }

/*  Read section 4 length */

    if( grab(buffer, size, 3, 1, &bytes_read) != 0) return (-1);
    section4 = len3oct(size);
    if( large ) section4 = total + 3 - bytes_read - section4; 
    advance = section4 - (bytes_read - section0 - section1 - section2 - section3);
    bytes_read += advance;

/*  Check 7777 group is in the expected place */

    if( grab(buffer, grp_7777, 4, 1, &bytes_read) != 0) return (-1);
    bytes_read += 4;
  

/*  Success! */
    return (section0+section1+section2+section3+section4+section5);
}

static int grab(char * buffer, char * where, long size,long cnt,long * num_bytes_read)
{
long number = size*cnt;

    memcpy(where, (buffer+(*num_bytes_read)), number);
    *num_bytes_read += number;

    return 0;
}
