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
//      readprod_alpha.c
*/
#include "bufrgrib.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <memory.h>

#define THREE_BYTE_LONG(p) ((((long)*(p))<<16) + (((long)*(p+1))<<8) + (long)*(p+2))
#define CHECK(stat,message,code) if((stat)) {perror(message);return(code);}

#define WMOBIT1 0x80
#define WMOBIT2 0x40
#define WMOBIT7 0x02
#define WMOBIT8 0x01

#define END_OF_FILE      -1
#define INTERNAL_ERROR   -2
#define BUFFER_TOO_SMALL -3
#define USER_BUFFER_TINY -4
#define MISPLACED_7777   -5

#define SECT_0_LEN 4
#define LEN_7777 4
#define SMALL 40
#define LARGEBUF 200000

#define GRIB 0x47524942
#define BUFR 0x42554652
#define BUDG 0x42554447
#define TIDE 0x54494445
#define DIAG 0x44494147

#include "fortint.h"
#include "fileRead.h"

#include "sizeRoutines.h"


fortint readprod( char * prod_id, char * buffer, fortint * size, 
              fortint (*fileRead)(char *, fortint , void *),
              fortint (*fileSeek)(void *, fortint, fortint),
              fortint (*fileTell)(void *),
               void * stream)
/*
//  Reads a BUFR, GRIB, BUDG, TIDE, DIAG product.
//
//  prod_id = "BUFR", "GRIB", "BUDG", "TIDE", "DIAG" ...
//  buffer = buffer to hold product,
//  size = on input, size of buffer in bytes,
//         on output, number of bytes read.
//  fileRead = function used to read the product,
//  fileSeek = function used to reposition within the product byte stream,
//  fileTell = function used to report the current byte position within
//             the product byte stream,
//  stream = data describing the input stream (eg. FILE *).
//
//  Returns 
//    -1 if can't find product  (eg. end-of-file)
//    -2 if internal processing error (malloc fail, file seek error)
//    -3 if buffer too small for whole product.
//    -4 if user buffer too small to even start product processing.
//    -5 if the 7777 group is in the wrong place
//
*/
{
static char * shold = NULL;           /* buffer used to read product */
char * hold = NULL;
fortint holdsize = SMALL, numread;
fortint found = 0, num = 0, length, prodlen;
fortint given_buflen = *size;
char p;
unsigned fortint code=0, wanted = 0;
int status = 0, i;

/* See if user gave a buffer for holding the product
*/
  if ( buffer == NULL ) {         /* No buffer given, get some */
    if ( shold == NULL ) {        /* but only first time round */
      shold = (char *) malloc( LARGEBUF);
      CHECK((shold == NULL),"malloc failed in readnext",INTERNAL_ERROR);
    }
    given_buflen = LARGEBUF;
    hold = shold;
  }
  else                             /* User buffer given */
    hold = buffer;

/* Make sure the user gave some buffer
*/
  if ( given_buflen < holdsize ) return USER_BUFFER_TINY;

/* Look for product identifier
*/
  if ( prod_id != NULL )
    for ( i = 0; i < 4; i++ )
      wanted = ( (wanted << 8) + *(prod_id+i) ) & 0xFFFFFFFF;

/* Read some bytes from the product
*/
  do {
    numread = fileRead( &p, 1, stream );
    if ( numread <= 0 ) {
      *size = 0;
      return END_OF_FILE;
    }

    code = ( (code << 8) + p ) & 0xFFFFFFFF;

    if ( prod_id == NULL ) {
      switch(code) {
        case BUDG:
        case BUFR:
        case GRIB:
        case TIDE:
        case DIAG: found = 1;
      }
    }
    else
      if ( code == wanted ) found = 1;

  } while ( ! found );

/* Find the product length
*/
  prodlen = prodsize( code, hold, given_buflen, &holdsize, fileRead, stream);

  if( prodlen == -4 ) return USER_BUFFER_TINY;

/* Move to end of product and check position of 7777 group
   in the case that the user gave a NULL buffer and zero length
*/
  if ( *size == 0 ) {
    char grp_7777[5];

    *size = prodlen;             /* report the actual product length */

    status = fileSeek( (FILE *)stream, prodlen - holdsize - LEN_7777, SEEK_CUR);
    CHECK(status,"fileSeek error in readprod",INTERNAL_ERROR);

    numread = fileRead( grp_7777, LEN_7777 ,stream );
    grp_7777[4] = '\0';
    if( strcmp(grp_7777,"7777") != 0 ) return MISPLACED_7777;

    return BUFFER_TOO_SMALL;
  }

/* Otherwise read as much of the product as possible into the buffer, then
   move to end of product and check position of 7777 group
*/
  *size = prodlen;                /* report the actual product length */

  length = (given_buflen > prodlen) ? prodlen : given_buflen ;

  if ( length > holdsize ) {
    numread = fileRead( hold+holdsize, length-holdsize ,stream );
    if ( given_buflen < prodlen ) {
      char grp_7777[5];

      status = fileSeek((FILE*)stream,prodlen-given_buflen - LEN_7777,SEEK_CUR);
      CHECK(status,"fileSeek error in readprod",INTERNAL_ERROR);

      numread = fileRead( grp_7777, LEN_7777 ,stream );
      grp_7777[4] = '\0';
      if( strcmp(grp_7777,"7777") != 0 ) return MISPLACED_7777;

      return BUFFER_TOO_SMALL;
    }
  }

  if(strncmp(hold+prodlen-LEN_7777,"7777",(size_t)4)!=0) return MISPLACED_7777;

  return *size;

}


static fortint gribsize(char * hold, fortint leng, fortint * holdsize,
                     fortint (*fileRead)(), void * stream)
/*
//  Calculates the size in bytes of a GRIB product.
//
//  hold = buffer to hold product,
//  leng = length of buffer,
//  holdsize = number of bytes of the product in the hold buffer.
//             Note that this increases if necessary as more bytes are read.
//  fileRead = function used to read the product,
//  stream = data describing the input stream (eg. FILE *).
*/
{
fortint length, numread, num, hsize = *holdsize;
fortint section2, section3 ;
unsigned char * phold = (unsigned char *) hold;

/* Need more bytes to decide on which version of GRIB
*/
  if ( leng < 24 ) return USER_BUFFER_TINY;

/* Put first 24 bytes in buffer
*/
  num = 24;
  if ( hsize < num) {
    numread = fileRead( hold + hsize, num - hsize, stream);
    if ( numread <= 0 ) {          /* eg. on END_OF_FILE */
      *holdsize -= numread;
      return *holdsize;
    }
    hsize = num;
  }
  *holdsize = hsize;

/* See if the GRIB version is 0 ...
*/
  if ( THREE_BYTE_LONG(phold+4) == 24 ) {
    length = 28;		     /* GRIB + section 1 */

/* Check for presence of sections 2 and 3
*/
    section2 = ( hold[11] & WMOBIT1 ) ;
    section3 = ( hold[11] & WMOBIT2 ) ;

/* Add up all lengths of sections
*/
    return lentotal(hold, holdsize, leng, length, section2, section3, 
                    fileRead, stream);
  }

/* ... or version 1 ...
*/

  if ( ( hold[21] != 0 ) || ( hold[22] != 0 ) ) {

/* Nightmare fixup for very large GRIB products (eg 2D wave spectra).
  
   If the most-significant of the 24 bits is set, this indicates a
   very large product; the size has to be rescaled by a factor of 120.
   This is a fixup to get round the GRIB code practice of representing
   a product length by 24 bits. It is only possible because the
   (default) rounding for GRIB products is 120 bytes.
  
*/
    fortint fixlen;

    fixlen = THREE_BYTE_LONG(phold+4);
    if( fixlen > 0x7fffff )
      return ((fixlen & 0x7fffff) * 120);
    else
      return fixlen;
  }

  length = 24;		     /* GRIB + section 1 */

/* Check for presence of sections 2 and 3
*/
  section2 = ( hold[7] & WMOBIT8 ) ;
  section3 = ( hold[7] & WMOBIT7 ) ;

/* Add up all lengths of sections
*/
  return lentotal(hold, holdsize, leng, length, section2, section3, 
                    fileRead, stream);

}


static fortint lentotal(char *hold, fortint *holdsize, fortint leng, fortint length,
                     fortint section2, fortint section3,
                     fortint (*fileRead)(), void *stream)
/*
//  Returns the total length in bytes of all sections of the GRIB product.
//
//  hold = buffer to hold product,
//  leng = length of buffer,
//  holdsize = number of bytes of the product in the hold buffer.
//             Note that this increases if necessary as more bytes
//             are read.
//  length = length of (section 0 + section 1).
//  section2 is TRUE if section 2 is present in the product.
//  section3 is TRUE if section 3 is present in the product.
//  fileRead = function used to read the product,
//  stream = data describing the input stream (eg. FILE *).
*/
{
fortint numread, hsize = *holdsize;
unsigned char * phold = (unsigned char *) hold;
fortint next, next_sec = 4;

/* Adjust count of sections to check
*/
  if ( section2 ) next_sec--;
  if ( section3 ) next_sec--;

/* Get the size of the next section
*/
  if ( leng < length ) return USER_BUFFER_TINY;
  if ( hsize < length) {
    numread = fileRead( hold + hsize, length - hsize, stream);
    if ( numread <= 0 ) {        /* eg. on END_OF_FILE */
      *holdsize -= numread;
      return *holdsize;
    }
    hsize = length;
  } 
  *holdsize = hsize;

/* Get the size of remaining sections
*/
  for ( next = next_sec; next < 5 ; next++ ) {
    if ( leng < (length+4) ) return USER_BUFFER_TINY;
    if ( hsize < (length+4)) {
      numread = fileRead( hold+hsize, (length+4)-hsize, stream);
      if ( numread <= 0 ) {    /* eg. on END_OF_FILE */
        *holdsize -= numread;
        return *holdsize;
      }
      hsize = length + 4;
    } 
    *holdsize = hsize;
    length += THREE_BYTE_LONG(phold+length);
  }

/* Add on the size of section 5
*/
  length += LEN_7777;

  return length;
}


static fortint bufrsize(char * hold, fortint leng, fortint * holdsize,
                     fortint (*fileRead)(), void * stream)
/*
//  Returns the size in bytes of the BUFR code product.
//
//  hold = buffer to hold product,
//  leng = length of buffer,
//  holdsize = number of bytes of the product in the hold buffer.
//         Note that this increases if necessary as more bytes are read.
//  fileRead = function used to read the product,
//  stream = data describing the input stream (eg. FILE *).
*/
{
unsigned char * phold = (unsigned char *) hold;
fortint numread, hsize = *holdsize;
fortint num, length;
fortint next, next_len, next_sec = 3;


/* Need more bytes to decide on which version of BUFR
*/
  if ( leng < 24 ) return USER_BUFFER_TINY;
  num = 24;                        /* put first 24 bytes in buffer*/
  if ( hsize < num) {
    numread = fileRead( hold + hsize, num - hsize, stream);
    if ( numread <= 0 ) {        /* eg. on END_OF_FILE */
      *holdsize -= numread;
      return *holdsize;
    }
    hsize = num;
  }
  *holdsize = hsize;

/* If it's Edition 2, or later, octets 5-7 give full product size
*/
  if ( hold[7] > 1 ) return THREE_BYTE_LONG(phold+4);

/* Otherwise, we have to step through the individual sections
   adding up the lengths
*/

/* Add on the length of section 1 and ensure enough of product is in 
   memory to continue
*/
  length = SECT_0_LEN + THREE_BYTE_LONG(phold+4);
  if ( leng < (length+4) ) return USER_BUFFER_TINY;
  if ( hsize < (length+4)) {
    numread = fileRead( hold+hsize, (length+4)-hsize, stream);
    if ( numread <= 0 ) {        /* eg. on END_OF_FILE */
      *holdsize -= numread;
      return *holdsize;
    }
    hsize = length + 4;
    *holdsize = hsize;
  } 

/* Check for presence of section 2
*/
  if ( hold[11] & WMOBIT1 ) next_sec = 2;

/* Get the size of remaining sections
*/
  for ( next = next_sec; next < 5 ; next++ ) {
    length += THREE_BYTE_LONG(phold+length);
    if ( leng < (length+4) ) return USER_BUFFER_TINY;
    if ( hsize < (length+4)) {
      numread = fileRead( hold+hsize,(length+4)-hsize, stream);
      if ( numread <= 0 ) {    /* eg. on END_OF_FILE */
        *holdsize -= numread;
        return *holdsize;
      }
      hsize = length + 4;
      *holdsize = hsize;
    } 
  }

/* Add on the size of section 5
*/
  length += LEN_7777;
  if ( leng < length ) return USER_BUFFER_TINY;

  return length;

}


static fortint tide_budg_size(char * hold, fortint leng, fortint * holdsize,
                           fortint (*fileRead)(), void * stream)
/*
//  Returns the size in bytes of the TIDE/BUDG/DIAG code product.
//
//  hold = buffer to hold product,
//  leng = length of buffer,
//  holdsize = number of bytes of the product in the hold buffer.
//             Note that this increases if necessary as more bytes are read.
//  fileRead = function used to read the product,
//  stream = data describing the input stream (eg. FILE *).
*/
{
unsigned char * phold = (unsigned char *) hold;
fortint numread, hsize = *holdsize;
fortint num, length;
fortint next, next_len, next_sec = 3;

/* Need more bytes to get length of section 1
*/
  num = 8;                        /* put first 8 bytes in buffer */
  if ( leng < num ) return USER_BUFFER_TINY;
  if ( hsize < num) {
    numread = fileRead( hold + hsize, num - hsize, stream);
    if ( numread <= 0 ) {       /* eg. on END_OF_FILE */
      *holdsize -= numread;
      return *holdsize;
    }
    hsize = num;
  }
  *holdsize = hsize;

/* Have to step through individual sections adding up the lengths
*/

/* Add on the length of section 1 and ensure enough of product is in 
   memory to continue
*/
  length = SECT_0_LEN + THREE_BYTE_LONG(phold+4);
  if ( leng < (length+4) ) return USER_BUFFER_TINY;
  if ( hsize < (length+4)) {
    numread = fileRead( hold+hsize, (length+4)-hsize, stream);
    if ( numread <= 0 ) {        /* eg. on END_OF_FILE */
      *holdsize -= numread;
      return *holdsize;
    }
    hsize = length + 4;
    *holdsize = hsize;
  } 

/* Get the size of remaining section
*/
  length += THREE_BYTE_LONG(phold+length);

/* Add on the size of section 5
*/
  length += LEN_7777;

  if ( leng < length ) return USER_BUFFER_TINY;
  return length;
}

static fortint prodsize(fortint code, char * hold, fortint leng, fortint * holdsize, 
                     fortint (*fileRead)(), void * stream)
/*
//  Returns size of BUFR, GRIB, BUDG, TIDE, DIAG product in bytes.
//
//  hold = buffer holding product,
//  leng = size of buffer in bytes,
//  holdsize = number of bytes of product already read into hold.
//  fileRead = function used to read the product,
//  stream = data describing the input stream (eg. FILE *).
*/
{
  *holdsize = 4;
  switch( code ) 
  {
     case BUFR: memcpy(hold,"BUFR",4);
                return bufrsize( hold, leng, holdsize, fileRead, stream);

     case BUDG: memcpy(hold,"BUDG",4);
                return tide_budg_size( hold, leng, holdsize, fileRead, stream);

     case GRIB: memcpy(hold,"GRIB",4);
                return gribsize( hold, leng, holdsize, fileRead, stream);

     case TIDE: memcpy(hold,"TIDE",4);
                return tide_budg_size( hold, leng, holdsize, fileRead, stream);

     case DIAG: memcpy(hold,"DIAG",4);
                return tide_budg_size( hold, leng, holdsize, fileRead, stream);

     default: return 0;
    }
}

