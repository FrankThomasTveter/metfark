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
#include <limits.h>
static FILE** fptable = NULL;
static int fptableSize = 0;
#define BUFFLEN 4096

#include "fortint.h"
#include "fileRead.h"

/*
//      pbio_alpha.c
*/
#include "fort2c.h"
#include "bufrgrib.h"
 
#define NAMEBUFFLEN 256
#define MODEBUFFLEN 10

void PBOPEN_(fortint* unit, _fcd name, _fcd mode, fortint* iret, int l1, int l2){
     PBOPEN(unit,name,mode,iret,l1,l2);
}

void PBOPEN__(fortint* unit, _fcd name, _fcd mode, fortint* iret, int l1, int l2){
     PBOPEN(unit,name,mode,iret,l1,l2);
}

/*************************************************************************
// FUNCTION:  pbopen - Open file (from FORTRAN)
**************************************************************************
*/
void PBOPEN(fortint* unit, _fcd name, _fcd mode, fortint* iret, int l1, int l2){
/*
// Purpose:
//  Opens file, returns the index of a UNIX FILE pointer held in 
//  an internal table (fptable).
//
//  Function  accepts:
//    name = filename
//    mode = r, w
//
//    Note: l1 and l2 are the lengths of the FORTRAN character strings
//          in name and mode.
//
//  Function returns:
//   INTEGER iret:
//     -1 = Could not open file.
//     -2 = Invalid file name.
//     -3 = Invalid open mode specified
//      0 = OK.
*/
char *fname;
char *modes;
char *p;
char flags[4];
char namebuff[NAMEBUFFLEN], modebuff[MODEBUFFLEN];

int n;

/*
// Put the character strings into buffers and ensure that there is a
// null terminator (for case when FORTRAN CHARACTER variable is full
// right to end with characters.
*/
    strncpy( namebuff, name, NAMEBUFFLEN - 1);
    strncpy( modebuff, mode, MODEBUFFLEN - 1);
    namebuff[l1] = '\0';
    modebuff[l2] = '\0';

    strcpy(flags,"");

    *unit = NULL;
    *iret = 0;

/*
// Convert Fortran to C string: file name.
*/
    if(!(fname = fcd2char(name))) {
      *iret = -2;
      return;
    }

/*
// Strip trailing blanks.
*/
    p  = fname;
    while(*p) {
      if(*p == ' ') *p = 0;
      p++;
    }
                
/*
// Convert Fortran to C string: open modes.
*/
    if(!(modes = fcd2char(mode))) {
      free(fname);
      *iret = -2;
      return;
    }

/*
// Build open flags from "modes".
*/
    p = modes;
      switch(*p) {
        case 'a':
        case 'A': strcat(flags, "a");
                  break;

        case 'c':
        case 'C':
        case 'w':
        case 'W': strcat(flags, "w");
                  break;

        case 'r':
        case 'R': strcat(flags, "r");
                  break;

        default:  *iret = -3;
                  return;

      }

/*
// If read/write, change flags.
*/
    if( !strcmp(flags,"wr") || !strcmp(flags, "rw") ) strcpy(flags, "r+w" );

/*
// Look for a free slot in fptable.
// (Create the table the first time through).
*/
    n = 0;
    if( fptableSize == 0 ) {
      int i;
      fptableSize = 2;
      fptable = (FILE **) malloc(fptableSize*sizeof(FILE *));
      if( fptable == NULL ) {
        perror("Unable to allocate space for table of FILE pointers");
        exit(1);
      }
      for( i = 0; i < fptableSize; i++ )
        fptable[i] = 0;
    }
    else {
      while( n < fptableSize ) {
        if (fptable[n]==0) {
          *unit = n;
          break;
        }
        n++;
      }
    }
/*
// If the table overflows, double its size.
*/
    if( n == fptableSize) {
      int i;
      fptableSize = 2*fptableSize;
      fptable = (FILE **) realloc(fptable, fptableSize*sizeof(FILE *));
      if( fptable == NULL ) {
        perror("Unable to reallocate space for table of FILE pointers");
        exit(1);
      }
      n = fptableSize/2;
      for( i = n; i < fptableSize; i++ )
        fptable[i] = 0;
        *unit = n;
    }

    fptable[n] = fopen(fname, flags );

    if(fptable[n] == NULL) {
      perror(fname);
      *iret = -1;
    }

    free(fname);
    free(modes);

    return;
}

void PBSEEK_(fortint* unit, fortint* offset, fortint* whence, fortint* iret) {
     PBSEEK(unit,offset,whence,iret); 
}

void PBSEEK__(fortint* unit, fortint* offset, fortint* whence, fortint* iret) {
     PBSEEK(unit,offset,whence,iret); 
}

/*************************************************************************
//  FUNCTION:  pbseek_ - Seek (from FORTRAN)
**************************************************************************
*/
void PBSEEK(fortint* unit, fortint* offset, fortint* whence, fortint* iret) {
/*
//
// Purpose:
//   Seeks to a specified location in file.
//
//  Function  accepts:
//    unit = the index of a UNIX FILE pointer held in
//           an internal table (fptable).
//
//    offset = byte count
//
//    whence  = 0, from start of file
//            = 1, from current position
//            = 2, from end of file.  
//
//  Returns:
//    iret:
//      -2 = error in handling file,
//      -1 = end-of-file
//      otherwise,  = byte offset from start of file.
*/
fortint my_offset;
fortint my_whence;

    my_offset = *offset;
    my_whence = *whence;

/*
// Must use negative offset if working from end-of-file
*/
    if( my_whence == 2) my_offset = - abs(my_offset);

    *iret = fileTell(fptable[*unit]);
    if( *iret == my_offset && my_whence == 0)
      *iret = 0;
    else
      *iret = fileSeek(fptable[*unit], my_offset, my_whence);

    if( *iret != 0 ) {
      if ( ! feof(fptable[*unit]) ) {
        *iret = -2;             /* error in file-handling */
        perror("pbseek");
      }
      else
        *iret = -1;             /* end-of-file  */

      clearerr(fptable[*unit]);
      return;
    }

/*
// Return the byte offset from start of file
*/
    *iret = fileTell(fptable[*unit]);

    return;
}

void PBREAD_(fortint* unit, char * buffer, fortint* nbytes, fortint* iret) {
     PBREAD(unit,buffer,nbytes,iret);
}
 
void PBREAD__(fortint* unit, char * buffer, fortint* nbytes, fortint* iret) {
     PBREAD(unit,buffer,nbytes,iret);
}
 
/*************************************************************************
*  FUNCTION:  pbread_ - Read (from FORTRAN)
**************************************************************************
*/
void PBREAD(fortint* unit, char * buffer, fortint* nbytes, fortint* iret) {
/*
// Purpose:
//  Reads a block of bytes from a file..
//
//  Function  accepts:
//    unit = the index of a UNIX FILE pointer held in
//           an internal table (fptable).
//
//    nbytes = number of bytes to read.
//
//  Returns:
//    iret:
//      -2 = error in reading file,
//      -1 = end-of-file,
//      otherwise, = number of bytes read.
*/
    if( (*iret = fread(buffer, 1, *nbytes, fptable[*unit]) ) != *nbytes) {
      if( ! feof(fptable[*unit]) ) {
        *iret = -2;             /*  error in file-handling  */
        perror("pbread");
        clearerr(fptable[*unit]);
        return;
      }
      else {
        *iret = -1;             /*  end-of-file */
        clearerr(fptable[*unit]);
      }
    }

    return;
}
 
void PBREAD2_(fortint* unit, char * buffer, fortint* nbytes, fortint* iret) {
     PBREAD2(unit,buffer,nbytes,iret); 
}

void PBREAD2__(fortint* unit, char * buffer, fortint* nbytes, fortint* iret) {
     PBREAD2(unit,buffer,nbytes,iret); 
}

/*************************************************************************
*  FUNCTION:  pbread2_ - Read (from FORTRAN)
**************************************************************************
*/
void PBREAD2(fortint* unit, char * buffer, fortint* nbytes, fortint* iret) {
/*
// Purpose:
//  Reads a block of bytes from a file..
//
//  Function  accepts:
//    unit = the index of a UNIX FILE pointer held in
//           an internal table (fptable).
//
//    nbytes = number of bytes to read.
//
//  Returns:
//    iret:
//      -2 = error in reading file,
//      -1 = end-of-file,
//      otherwise, = number of bytes read.
*/
    if( (*iret = fread(buffer, 1, *nbytes, fptable[*unit]) ) != *nbytes) {
      if( ! feof(fptable[*unit]) ) {
        *iret = -2;             /*  error in file-handling  */
        perror("pbread");
        clearerr(fptable[*unit]);
        return;
      }
      else {
        *iret = -1;             /*  end-of-file */
        clearerr(fptable[*unit]);
      }
    }

    return;
}

void PBWRITE_(fortint* unit, char * buffer, fortint* nbytes, fortint* iret) {
     PBWRITE(unit,buffer,nbytes,iret); 
}

void PBWRITE__(fortint* unit, char * buffer, fortint* nbytes, fortint* iret) {
     PBWRITE(unit,buffer,nbytes,iret); 
}

/*************************************************************************
*  FUNCTION:  pbwrite_ - Write (from FORTRAN)
**************************************************************************
*/
void PBWRITE(fortint* unit, char * buffer, fortint* nbytes, fortint* iret) {
/*
// Purpose:
//  Writes a block of bytes to a file.
//
//  Function  accepts:
//    unit = the index of a UNIX FILE pointer held in
//           an internal table (fptable).
//
//    nbytes = number of bytes to write.
//
//  Returns:
//    iret:
//      -1 = Could not write to file.
//     >=0 = Number of bytes written.
*/
    if ( (*iret = fwrite(buffer, 1, *nbytes, fptable[*unit]) ) != *nbytes) {
      perror("pbwrite");
      *iret = -1;
    }

    return;
}

fortint PBCLOSE_(fortint* unit, fortint* iret)
	return PBCLOSE(unit,iret);
}

fortint PBCLOSE__(fortint* unit, fortint* iret)
	return PBCLOSE(unit,iret);
}

/*************************************************************************
*  FUNCTION:  pbclos_ - clos (from FORTRAN)
**************************************************************************
*/
fortint PBCLOSE(fortint* unit, fortint* iret)
/*
// Purpose:
//  Closes file.
//
//  Function  accepts:
//    unit = the index of a UNIX FILE pointer held in
//           an internal table (fptable).
////  Returns:
//    iret:
//      0 = OK.
//      otherwise = error in handling file.
*/
{
    *iret = fclose(fptable[*unit]);
    fptable[*unit] = 0;

    if(*iret != 0) {
      perror("pbclose");
      return (1);
    }

    return (0);

}

void GRIBREAD__(char * buffer, fortint* buffsize, fortint* readsize,
              fortint* status, fortint* stream) {
      GRIBREAD(buffer,buffsize,readsize,status,stream); 
}

void GRIBREAD_(char * buffer, fortint* buffsize, fortint* readsize,
              fortint* status, fortint* stream) {
      GRIBREAD(buffer,buffsize,readsize,status,stream); 
}

void GRIBREAD(char * buffer, fortint* buffsize, fortint* readsize,
              fortint* status, fortint* stream) {
/*
//  Called as a FORTRAN subroutine:
//
//    CALL GRIBREAD( KARRAY, KINLEN, KOUTLEN, IRET, KUNIT )
//
*/
fortint holdsize = *buffsize;

/*
// Read GRIB product
*/
    *status = readprod("GRIB",buffer,&holdsize,fileRead,fileSeek,fileTell,
                       fptable[*stream]);
    *readsize = abs(holdsize );
    return;
}

void BUFRREAD__(char * buffer, fortint* buffsize, fortint* readsize,
              fortint* status, fortint* stream) {
     BUFRREAD(buffer,buffsize,readsize,status,stream); 
}

void BUFRREAD_(char * buffer, fortint* buffsize, fortint* readsize,
              fortint* status, fortint* stream) {
     BUFRREAD(buffer,buffsize,readsize,status,stream); 
}

void BUFRREAD(char * buffer, fortint* buffsize, fortint* readsize,
              fortint* status, fortint* stream) {
/*
//  Called as a FORTRAN subroutine:
//
//    CALL BUFRREAD( KARRAY, KINLEN, KOUTLEN, IRET, KUNIT )
//
*/
fortint holdsize = *buffsize;

/*
// Read BUFR product
*/
    *status = readprod("BUFR",buffer,&holdsize,fileRead,fileSeek,fileTell,
                       fptable[*stream]);
    *readsize =  abs(holdsize );
    return;
}

void PSEUREAD__(char * buffer, fortint* buffsize, fortint* readsize,
              fortint* status, fortint* stream) {
     PSEUREAD(buffer,buffsize,readsize,status,stream); 
}

void PSEUREAD_(char * buffer, fortint* buffsize, fortint* readsize,
              fortint* status, fortint* stream) {
     PSEUREAD(buffer,buffsize,readsize,status,stream); 
}

void PSEUREAD(char * buffer, fortint* buffsize, fortint* readsize,
              fortint* status, fortint* stream) {
/*
//  Called as a FORTRAN subroutine:

//    CALL PSEUREAD( KARRAY, KINLEN, KOUTLEN, IRET, KUNIT )

*/
fortint holdsize = *buffsize;

/*
// Read GRIB product
*/
    *status = readprod(NULL,buffer,&holdsize,fileRead,fileSeek,fileTell,
                       fptable[*stream]);
    *readsize = abs(holdsize );
    return;
}

void PBSIZE_(fortint* unit, fortint* plen) {
     PBSIZE(unit,plen); 
}

void PBSIZE__(fortint* unit, fortint* plen) {
     PBSIZE(unit,plen); 
}

/*************************************************************************
// FUNCTION:  pbopen - Open file (from FORTRAN)
**************************************************************************
*/
void PBSIZE(fortint* unit, fortint* plen) {
/*
// Purpose:
//  Returns the size in bytes of the next GRIB, BUFR, TIDE, BUDG, DIAG
//  product.
//
//  Called from FORTRAN:
//      CALL PBSIZE( KUNIT, LENGTH)
//
//  Function  accepts:
//    unit = the index of a UNIX FILE pointer held in
//           an internal table (fptable).
//
//  Returns:
//    plen  = size in bytes of the next product.
//          = -2 if error allocating memory for internal buffer.
//
//  The input file is left positioned where it started.
*/
fortint iret;
char statbuff[BUFFLEN];
char * buff;
fortint offset, loop = 1;
FILE *in = fptable[*unit];

/*
// Use a smallish buffer for processing; this should suffice for all cases
// except versions -1 and 0 of GRIB and large BUFR products
*/
    offset = (fortint) fileTell( in);
    *plen = BUFFLEN;
    iret = readprod(NULL,statbuff,plen,fileRead,fileSeek,fileTell,in);
    if( iret == -2 ) {
      printf("readprod error %d\n", iret);
      *plen = -2;
      return;
    }
/*
// If the smallish buffer is too small, progressively increase it until 
// big enough */

  while ( iret == -4 ) {
    loop++;
    buff = (char *) malloc( BUFFLEN*loop);
    if( buff == NULL) {
      perror("malloc failed in PBSIZE");
      *plen = -2;
      return;
    }
    *plen = BUFFLEN*loop;
    offset = (fortint) fileSeek( in, offset, SEEK_SET);
    offset = (fortint) fileTell( in);
    iret = readprod(NULL,buff,plen,fileRead,fileSeek,fileTell,in);
    free(buff);
  }

  if( iret == -2 ) {
    printf("readprod error %d\n", iret);
    *plen = -2;
  }
/*
// Put the file pointer back where it started
*/
    offset = (fortint) fileSeek( in, offset, SEEK_SET);

    return;
}

#define PBOPEN3  pbopen3_
#define PBREAD3  pbread3_
#define PBWRITE3 pbwrite3_
#define PBSEEK3  pbseek3_
#define PBCLOSE3 pbclose3_


void PBREAD3__(fortint  *unit, char * buffer, fortint * nbytes, fortint * iret) {
     PBREAD3(unit,buffer,nbytes,iret);
}

void PBREAD3_(fortint  *unit, char * buffer, fortint * nbytes, fortint * iret) {
     PBREAD3(unit,buffer,nbytes,iret);
}

void PBREAD3(fortint  *unit, char * buffer, fortint * nbytes, fortint * iret)
/*
*
* Purpose:      Reads a block of bytes from a file.
*
* Function returns:          status :   -2 = error in reading file,
*                                       -1 = end-of-file,
*                               otherwise, = number of bytes read.
*/
{
 
    *iret = read(*unit, buffer, *nbytes);

                                                /*      Read problem */
    if (*iret == -1)
    {
        *iret = -2;             /*  error in file-handling  */
        perror("pbread3");
        return;
    }
    else if (*iret != *nbytes)
    {
        *iret = -1;
        printf("EOF; pbread3; bytes requested %d; read in: %d\n",
        *nbytes,*iret);
        return;
    }
 
}
 
static oct_bin3(int onum)
{
   char tmp[20];
   int  rc;

   sprintf(tmp,"%d",onum);
   sscanf(tmp,"%o",&rc);
   return rc; 
}

void PBOPEN3_(fortint *unit, _fcd name, _fcd mode, fortint * iret,
            fortint l1, fortint l2) {
     PBOPEN3(unit,name,mode,iret,l1,l2);
}

void PBOPEN3__(fortint *unit, _fcd name, _fcd mode, fortint * iret,
            fortint l1, fortint l2) {
     PBOPEN3(unit,name,mode,iret,l1,l2);
}

void PBOPEN3(fortint *unit, _fcd name, _fcd mode, fortint * iret,
            fortint l1, fortint l2)
/*
* Purpose:  Opens file, return UNIX FILE pointer.
*
* Function returns:      iret:  -1 = Could not open file.
*                               -2 = Invalid file name.
*                               -3 = Invalid open mode specified
*                                0 = OK.
*
*    Note: l1 and l2 are the lengths of the character strings in 
*          name and mode on SGI.
*/
{
char *fname;
char *modes;
char *p;
int oflag;
int dmas;
int filemode;

#if (!defined CRAY) && (!defined VAX)
char namebuff[NAMEBUFFLEN], modebuff[MODEBUFFLEN];

/* Put the character strings into buffers and ensure that there is a
   null terminator (for SGI case when FORTRAN CHARACTER variable is full
   right to end with characters */
    strncpy( namebuff, name, NAMEBUFFLEN - 1);
    strncpy( modebuff, mode, MODEBUFFLEN - 1);
    namebuff[l1] = '\0';
    modebuff[l2] = '\0';
#endif


    *unit = 0;
    *iret = 0;

    /* convert fortran to c string : file name */

#if (!defined CRAY) && (!defined VAX)
    if(!(fname = fcd2char(namebuff)))
#else
    if(!(fname = fcd2char(name)))
#endif
    {
        *iret = -2;
        return;
    }

    /* strip trailing blanks */

    p  = fname + strlen(fname) - 1 ;
    while(*p == ' ')
    {
        *p = 0;
    p--;
    }
            
    /* convert fortran to c string : open modes  */
    
#if (!defined CRAY) && (!defined VAX)
    if(!(modes = fcd2char(modebuff)))
#else
    if(!(modes = fcd2char(mode)))
#endif
    {
        free(fname);
        *iret = -3;
        return;
    }

    /* build open flags from "modes" */

    p = modes;
    while(*p)
    {
        switch(*p)
        {
            case '+': break;

            case 'a':
            case 'A': oflag = 0x100 | 2 | 0x08;
                      filemode = 766;
                      break;

            case 'c':
            case 'C':
            case 'w':
            case 'W': oflag = 0x100 | 1;
                      filemode = 766;
                      break;

            case 'r':
            case 'R': oflag = 0;
                      filemode = 444;
                      break;

            default:  *iret = -3;
                      return;

         }
         p++;
    }


    dmas = umask(000);
    *unit = open(fname, oflag, oct_bin3(filemode));
    umask(dmas);

    if(*unit == -1)
    {
        perror(fname);
        perror("pbopen3");
        *iret = -2;
    }


    free(fname);
    free(modes);

}

void PBCLOSE3_( fortint * unit, fortint * iret) {
     PBCLOSE3(unit,iret);
}

void PBCLOSE3__( fortint * unit, fortint * iret) {
     PBCLOSE3(unit,iret);
}

void PBCLOSE3( fortint * unit, fortint * iret)
/*
*
* Purpose:  Closes file.
*
* Function returns:      status : non-0 = error in handling file.
*                                             0 = OK.
*/
{
    *iret = close(*unit);

    if(*iret != 0) perror("pbclose3");

}

void PBSEEK3_(fortint * unit, fortint * offset, fortint * whence, fortint * iret) {
     PBSEEK3(unit,offset,whence,iret);
}

void PBSEEK3__(fortint * unit, fortint * offset, fortint * whence, fortint * iret) {
     PBSEEK3(unit,offset,whence,iret);
}

void PBSEEK3(fortint * unit, fortint * offset, fortint * whence, fortint * iret)
/*
*
* Purpose:  Seeks to specified location in file.
*
* Function returns: status :        -2 = error in handling file,
*                                   -1 = end-of-file
*                       otherwise,         = byte offset from start of file.
*
*   whence  = 0, from start of file
*           = 1, from current position
*           = 2, from end of file.  
*/
{
fortint my_offset = *offset;
int my_whence;

    if ( *whence == 2)
    {
         my_offset = - abs(my_offset);
         my_whence = 2;
    }
    else if (*whence == 0)
    {
         my_whence = 0;
    }
    else
    {
         my_whence = 1;
    }
     
                            /* must use negative offset if working
                               from end-of-file     */

   if ((*iret=lseek(*unit, my_offset, my_whence)) < 0)
   {
      perror("pbseek3;");
      *iret = -1;           /* end-of-file  */
   }

}
            
void PBWRITE3_( fortint * unit, char * buffer, fortint * nbytes, fortint *iret) {
     PBWRITE3(unit,buffer,nbytes,iret);
}

void PBWRITE3__( fortint * unit, char * buffer, fortint * nbytes, fortint *iret) {
     PBWRITE3(unit,buffer,nbytes,iret);
}

void PBWRITE3( fortint * unit, char * buffer, fortint * nbytes, fortint *iret)
/*
* Purpose:  Writes a block of bytes to a file.
*
* Function returns:      status : -1 = Could not write to file.
*                                    >=0 = Number of bytes written.
*/
{
   if ((*iret = write(*unit, buffer, *nbytes)) != *nbytes)
   {
       perror("pbwrite3: ");
        *iret = -1;
   }

}

