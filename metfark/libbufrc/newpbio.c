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
	newpbio.c
*/
#include "bufrgrib.h"
#include "fort2c.h"
#include "fortint.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifndef CRAY
#ifdef FORTRAN_NO_UNDERSCORE
#define PBOPEN3  pbopen3
#define PBREAD3  pbread3
#define PBWRITE3 pbwrite3
#define PBSEEK3  pbseek3
#define PBCLOSE3 pbclose3
#else
#define PBOPEN3  pbopen3_
#define PBREAD3  pbread3_
#define PBWRITE3 pbwrite3_
#define PBSEEK3  pbseek3_
#define PBCLOSE3 pbclose3_
#endif
#endif

#define NAMEBUFFLEN 256
#define MODEBUFFLEN 10

/*************************************************************************
*  FUNCTION:  pbopen - Open file (from FORTRAN)
**************************************************************************
*/
void PBOPEN(FILE ** unit, _fcd name, _fcd mode, fortint * iret,
if defined hpR64 || defined hpiaR64
            long l1, long l2)
#else
            fortint l1, fortint l2)
#endif
/*
* Purpose:	Opens file, return UNIX FILE pointer.
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
char  flags[4];

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

    strcpy(flags,"");

    *unit = NULL;
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
        *iret = -2;
        return;
    }


    /* build open flags from "modes" */

    p = modes;
    while(*p && ( strlen(flags) < 3 ) )
    {
        switch(*p)
        {
            case '+': strcat(flags, "+");
                      break;

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
         p++;
    }

    /* if read/write change flags */

    if ( !strcmp(flags,"wr") || !strcmp(flags, "rw") )
        strcpy(flags, "r+w" );

    *unit = fopen(fname, flags );

    if(*unit == NULL)
    {
        perror(fname);
        perror("pbopen");
        *iret = -1;
    }


    free(fname);
    free(modes);

}

/*************************************************************************
*  FUNCTION:  pbseek - Seek (from FORTRAN)
**************************************************************************
*/
void PBSEEK(FILE ** unit, fortint * offset, fortint * whence, fortint * iret)
/*
*
* Purpose:	Seeks to specified location in file.
*
* Function returns:	status : 	-2 = error in handling file,
*					-1 = end-of-file
*                       otherwise,         = byte offset from start of file.
*
*	whence	= 0, from start of file
*		= 1, from current position
*		= 2, from end of file.	
*/
{
int my_offset = (int) *offset;
int my_whence = (int) *whence;
int my_iret;

    if ( my_whence == 2) my_offset = - abs(my_offset);
				/* must use negative offset if working
				   from end-of-file	*/

    my_iret = fseek(*unit, my_offset, my_whence);

    if(my_iret != 0)
    {
        if ( ! feof(*unit) )
        {
            my_iret = -2;		/* error in file-handling */
            perror("pbseek");
        }
        else
            my_iret = -1;		/* end-of-file	*/

        clearerr(*unit);
        *iret = (fortint) my_iret;
        return;
    }

    my_iret = ftell(*unit);		/* byte offset from start of file */
    *iret = (fortint) my_iret;
    return;

}
		
 
/*************************************************************************
*  FUNCTION:  pbread - Read (from FORTRAN)
**************************************************************************
*/
void PBREAD(FILE ** unit, char * buffer, fortint * nbytes, fortint * iret)
/*
*
* Purpose:      Reads a block of bytes from a file.
*
* Function returns:          status :   -2 = error in reading file,
*                                       -1 = end-of-file,
*                               otherwise, = number of bytes read.
*/
{
int my_nbytes = (int) *nbytes;
int my_iret;
 
    if ( (my_iret = fread(buffer, 1, my_nbytes, *unit) ) != my_nbytes)
    {
                                                /*      Read problem */
        if ( ! feof(*unit) )
        {
            my_iret = -2;             /*  error in file-handling  */
            perror("pbread");
            clearerr(*unit);
        }
        else
        {
            my_iret = -1;             /*  end-of-file */
            clearerr(*unit);
        }
    }
    *iret = my_iret;
    return;
 
}
 

/*************************************************************************
*  FUNCTION:  pbread2 - Read (from FORTRAN)
**************************************************************************
*/
void PBREAD2(FILE ** unit, char * buffer, fortint * nbytes, fortint * iret)
/*
*
* Purpose:	Reads a block of bytes from a file.
*
* Function returns:	      status : 	-2 = error in reading file,
*                               otherwise, = number of bytes read.
*/
{
int my_nbytes = (int) *nbytes;
int my_iret;

    if ( (my_iret = fread(buffer, 1, my_nbytes, *unit) ) != my_nbytes)
    {
						/*	Read problem */
        if ( ! feof(*unit) )
        {
            my_iret = -2;             /*  error in file-handling  */
            perror("pbread");
            clearerr(*unit);
        }
    }
    *iret = my_iret;
    return;
 
}

/*************************************************************************
*  FUNCTION:  pbwrite - Write (from FORTRAN)
**************************************************************************
*/
void PBWRITE( FILE ** unit, char * buffer, fortint * nbytes, fortint *iret)
/*
* Purpose:	Writes a block of bytes to a file.
*
* Function returns:	     status : -1 = Could not write to file.
*                                    >=0 = Number of bytes written.
*/
{
int my_nbytes = (int) *nbytes;
int my_iret;

    if ( (my_iret = fwrite(buffer, 1, my_nbytes, *unit) ) != my_nbytes)
    {				/* Problem with write */   
        perror("pbwrite");
        my_iret = -1;
    }
    *iret = my_iret;
    return;
    
}



/*************************************************************************
*  FUNCTION:  pbclose - close (from FORTRAN)
**************************************************************************
*/
void PBCLOSE( FILE ** unit, fortint * iret)
/*
*
* Purpose:	Closes file.
*
* Function returns:	     status : non-0 = error in handling file.
*                                     	  0 = OK.
*/
{
int my_iret;

    my_iret = fclose(*unit);

    if(my_iret != 0) perror("pbclose");
    *iret = my_iret;
    return;

}


/*************************************************************************
*  FUNCTION:  pbflush - flush (from FORTRAN)
**************************************************************************
*/
void PBFLUSH( FILE ** unit)
/*
*
* Purpose:	Flushes file.
*
*/
{

    fflush(*unit);
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

