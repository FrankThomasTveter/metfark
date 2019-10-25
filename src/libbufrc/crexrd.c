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

#define END_OF_FILE -1
#define FILE_READ_ERROR -2
#define USER_BUFFER_TOO_SMALL -3
#define FILE_TOO_SMALL -5
#define MINIMUM_CREX_SIZE 13
#define CREX 0x43524558

typedef char * String;

void CREXRD(String buffer, int* bufflen, int* size, int* status, FILE** in) {
/*
//  Called from FORTRAN:
//    CALL CREXRD( KARRAY, KINLEN, NREAD, IRET, KUNIT )
*/
int loop, foundPosition;
int number, crexFound = 0, endFound = 0;
String endBuffer;
String next;
char plplcrcrlf7777[10] = {0,0,0,0,0,0,0,0,0,0};
char PlPlCrCrLf7777[10] = {0x2b,0x2b,0x0d,0x0d,0x0a,0x37,0x37,0x37,0x37,0x00};

/*
// Check buffer big enough for CREX search
*/
  if( *bufflen < MINIMUM_CREX_SIZE ) {
    *status = USER_BUFFER_TOO_SMALL;
    return;
  }

/*
// Look for CREX
*/
  for( loop = 0; loop <= 4; loop++) buffer[loop] = '\0';

  while( !crexFound ) {
    buffer[0] = buffer[1];
    buffer[1] = buffer[2];
    buffer[2] = buffer[3];
    number = fread((buffer+3), 1, 1, *in);
    if( feof(*in) ) {
      *status = END_OF_FILE;
      return;
    }
    if( (number != 1) || ferror(*in) ) {
      perror("crexrd file read error");
      *status = FILE_READ_ERROR;
      return;
    }
    if( strcmp(buffer,"CREX") == 0 ) {
      crexFound = 1;
      foundPosition = ftell(*in) - 4;
    }
  }

/*
// Read some more characters into the buffer
*/
  number = fread((buffer+4), 1, ((*bufflen)-4), *in);
  if( ferror(*in) ) {
    perror("crexrd file read error");
    *status = FILE_READ_ERROR;
    return;
  }
  endBuffer = buffer + number + 3;
    
/*
// Look for ++CrCrLf7777 at end of product
*/
  next = buffer+4;
  endFound = 0;
  for( loop = 0; loop < 8; loop++ )
    plplcrcrlf7777[loop] = *(next++);
  plplcrcrlf7777[9] = '\0';

  while( (!endFound) && (next<=endBuffer) ) {
    plplcrcrlf7777[8] = *(next++);
    if( strcmp(plplcrcrlf7777,PlPlCrCrLf7777) == 0 ) {
      endFound = 1;
      *size = (int) (next - buffer);
    }

    for( loop = 0; loop < 8; loop++ )
      plplcrcrlf7777[loop] = plplcrcrlf7777[loop+1];
  }

  if( !endFound ) {
    if( feof(*in) ) 
      *status = END_OF_FILE;
    else
      *status = USER_BUFFER_TOO_SMALL;
    return;
  }

/*
// Position file at end of CREX product
*/
  *status = fseek(*in, (foundPosition+(*size)), 0);

}

void CREXRD_(String buffer, int* bufflen, int* size, int* status, FILE** in) {
	CREXRD(buffer,bufflen,size,status,in);
}

void CREXRD__(String buffer, int* bufflen, int* size, int* status, FILE** in) {
	CREXRD(buffer,bufflen,size,status,in);
}
