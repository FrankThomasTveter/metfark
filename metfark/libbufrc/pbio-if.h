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
 * $Id: src/pbio/pbio-if.h, Fri May 13 17:36:07 BST 2011, mao $
 * 
 * Header file for src/pbio
 */
#if !defined PBIOIF_H
#define PBIOIF_H

#include <stdio.h>

#include "fortint.h"
#include "fort2c.h"
typedef char * String;

/* PBGroutines.c */
/*void pbgindx(fortint thisFile);
void gribdata(unsigned char *header, integer index, integer *parameter, integer *level, integer *date, integer *time, integer *timestep, integer *localDefinitionNumber, integer *type, integer *stream, integer *repres, integer *levtype, integer *number, integer *vdate, integer *vtime, integer *tcNumber, integer *tcTotal, integer *clusterMethod, integer *tcStep, integer *clusterStepEnd, integer *tcNorth, integer *tcWest, integer *tcSouth, integer *tcEast, integer *clusterOpFcNumber, integer *clusterControlFcNumber, integer *tcNumOfFcs, integer *probScale, integer *probIndicator, integer *probLower, integer *probUpper, integer *ni, integer *nj, integer *nlat, integer *nlon, integer *slat, integer *slon, integer *di, integer *dj, integer *splat, integer *splon, integer *quasi, integer *directionNumber, integer *frequencyNumber, integer *optimisationTime, integer *leadTime, integer *sensitiveAreaDomain, integer *sensitiveAreaMethod, integer *verifyingMonth, integer *averagingPeriod, integer *forecastMonth, integer *referenceDate, integer *climateDateFrom, integer *climateDateTo, integer *thresholdUnitsScale, integer *thresholdIndicator, integer *lowerThreshold, integer *upperThreshold);
fortint pbgtotl_(_fcd filename, fortint filename_len);
fortint pbgtotl(_fcd filename, fortint filename_len);
fortint pbgleng_(_fcd filename, fortint *n, fortint filename_len);
fortint pbgleng(_fcd filename, fortint *n, fortint filename_len);
fortint pbgoffs_(_fcd filename, fortint *n, fortint filename_len);
fortint pbgoffs(_fcd filename, fortint *n, fortint filename_len);
fortint pbgparm_(_fcd filename, fortint *n, fortint filename_len);
fortint pbgparm(_fcd filename, fortint *n, fortint filename_len);
fortint pbglevl_(_fcd filename, fortint *n, fortint filename_len);
fortint pbglevl(_fcd filename, fortint *n, fortint filename_len);
fortint pbgdate_(_fcd filename, fortint *n, fortint filename_len);
fortint pbgdate(_fcd filename, fortint *n, fortint filename_len);
fortint pbgtime_(_fcd filename, fortint *n, fortint filename_len);
fortint pbgtime(_fcd filename, fortint *n, fortint filename_len);
fortint pbgstep_(_fcd filename, fortint *n, fortint filename_len);
fortint pbgstep(_fcd filename, fortint *n, fortint filename_len);
fortint pbgtype_(_fcd filename, fortint *n, fortint filename_len);
fortint pbgtype(_fcd filename, fortint *n, fortint filename_len);
fortint pbgstrm_(_fcd filename, fortint *n, fortint filename_len);
fortint pbgstrm(_fcd filename, fortint *n, fortint filename_len);
fortint pbgrepr_(_fcd filename, fortint *n, fortint filename_len);
fortint pbgrepr(_fcd filename, fortint *n, fortint filename_len);
fortint pbglevt_(_fcd filename, fortint *n, fortint filename_len);
fortint pbglevt(_fcd filename, fortint *n, fortint filename_len);
fortint pbgfind_(_fcd filename, fortint *param, fortint *level, fortint *date, fortint *time, fortint *step, fortint *n, fortint filename_len);
fortint pbgfind(_fcd filename, fortint *param, fortint *level, fortint *date, fortint *time, fortint *step, fortint *n, fortint filename_len);
fortint pbgvfind_(_fcd filename, fortint *param, fortint *level, fortint *vdate, fortint *vtime, fortint *status, fortint *n, fortint filename_len);
fortint pbgvfind(_fcd filename, fortint *param, fortint *level, fortint *vdate, fortint *vtime, fortint *status, fortint *n, fortint filename_len);
fortint pbgafind_(_fcd filename, fortint *list, fortint *n, fortint filename_len);
fortint pbgafind(_fcd filename, fortint *list, fortint *n, fortint filename_len);
fortint pbgbfind_(_fcd filename, fortint *list, fortint *n, fortint filename_len);
fortint pbgbfind(_fcd filename, fortint *list, fortint *n, fortint filename_len);
*/
/* crexrd.c */
void CREXRD(String buffer, int *bufflen, int *size, int *status, FILE **in);

/* extras.c */
long f_getenv_(char *env_string, char *env_info, int len_env_string, int len_env_info);
long f_getenv(char *env_string, char *env_info, int len_env_string, int len_env_info);

/* fort2c.c | fort2c_hppa.c*/
/*char *fcd2char(_fcd fortchar);*/

/* gbyte.c | gbyte_alpha.c | gbyte_le.c */
void gbytes_(void *Source, void *Destination, fortint *startSkip, fortint *bitsPerValue, fortint *skipBetweenValues, fortint *numberOfValues);
void gbytes(void *Source, void *Destination, fortint *startSkip, fortint *bitsPerValue, fortint *skipBetweenValues, fortint *numberOfValues);
void gbyte_(void *Source, void *Destination, fortint *nextValueFirstBit, fortint *bitsPerValue);
void gbyte(void *Source, void *Destination, fortint *nextValueFirstBit, fortint *bitsPerValue);
void sbytes_(void *Destination, void *Source, fortint *startSkip, fortint *bitsPerValue, fortint *skipBetweenValues, fortint *numberOfValues);
void sbytes(void *Destination, void *Source, fortint *startSkip, fortint *bitsPerValue, fortint *skipBetweenValues, fortint *numberOfValues);
void sbyte_(void *Destination, void *Source, fortint *nextValueFirstBit, fortint *bitsPerValue);
void sbyte(void *Destination, void *Source, fortint *nextValueFirstBit, fortint *bitsPerValue);
void GBYTES(void *Source, void *Destination, int *startSkip, int *bitsPerValue, int *skipBetweenValues, int *numberOfValues);
void GBYTE(void *Source, void *Destination, int *startSkip, int *bitsPerValue);
void SBYTES(void *Destination, void *Source, int *startSkip, int *bitsPerValue, int *skipBetweenValues, int *numberOfValues);
void SBYTE(void *destination, void *source, int *startSkip, int *bitsPerValue);

/* lwsize.c */
void lwsize_(fortint **unit, fortint *plen);
void lwsize(fortint **unit, fortint *plen);

/* mvchars.c */
fortint mvchars_(fortint *k, char *s, long ls);
fortint mvchars(fortint *k, char *s, long ls);

/* pbio.c */
FILE *pbfp(long index);
void pbopen_(fortint *unit, _fcd name, _fcd mode, fortint *iret, fortint l1, fortint l2);
void pbopen(fortint *unit, _fcd name, _fcd mode, fortint *iret, fortint l1, fortint l2);
void pbseek_(fortint *unit, fortint *offset, fortint *whence, fortint *iret);
void pbseek(fortint *unit, fortint *offset, fortint *whence, fortint *iret);
void pbseek64_(fortint *unit, long long *offset, fortint *whence, long long *iret);
void pbseek64(fortint *unit, long long *offset, fortint *whence, long long *iret);
void pbtell_(fortint *unit, fortint *iret);
void pbtell(fortint *unit, fortint *iret);
void pbtell64_(fortint *unit, long long *iret);
void pbtell64(fortint *unit, long long *iret);
void pbread_(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void pbread(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void pbread2_(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void pbread2(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void pbwrite_(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void pbwrite(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void pbclose_(fortint *unit, fortint *iret);
void pbclose(fortint *unit, fortint *iret);
void pbflush_(fortint *unit);
void pbflush(fortint *unit);
void pbread3_(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void pbread3(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void pbopen3_(fortint *unit, _fcd name, _fcd mode, fortint *iret, fortint l1, fortint l2);
void pbopen3(fortint *unit, _fcd name, _fcd mode, fortint *iret, fortint l1, fortint l2);
void pbclose3_(fortint *unit, fortint *iret);
void pbclose3(fortint *unit, fortint *iret);
void pbseek3_(fortint *unit, fortint *offset, fortint *whence, fortint *iret);
void pbseek3(fortint *unit, fortint *offset, fortint *whence, fortint *iret);
void pbwrite3_(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void pbwrite3(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void gribread_(char *buffer, fortint *buffsize, fortint *readsize, fortint *status, fortint *unit);
void gribread(char *buffer, fortint *buffsize, fortint *readsize, fortint *status, fortint *unit);
void bufrread_(char *buffer, fortint *buffsize, fortint *readsize, fortint *status, fortint *unit);
void bufrread(char *buffer, fortint *buffsize, fortint *readsize, fortint *status, fortint *unit);
void pseuread_(char *buffer, fortint *buffsize, fortint *readsize, fortint *status, fortint *unit);
void pseuread(char *buffer, fortint *buffsize, fortint *readsize, fortint *status, fortint *unit);
void pbsize_(fortint *unit, fortint *plen);
void pbsize(fortint *unit, fortint *plen);
void crexrd_(String buffer, int *bufflen, int *size, int *status, fortint *unit);
void crexrd(String buffer, int *bufflen, int *size, int *status, fortint *unit);

/* pbio_alpha.c */
void PBOPEN(fortint *unit, _fcd name, _fcd mode, fortint *iret, int l1, int l2);
void PBSEEK(fortint *unit, fortint *offset, fortint *whence, fortint *iret);
void PBREAD(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void PBREAD2(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void PBWRITE(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
fortint PBCLOSE(fortint *unit, fortint *iret);
void GRIBREAD(char *buffer, fortint *buffsize, fortint *readsize, fortint *status, fortint *stream);
void BUFRREAD(char *buffer, fortint *buffsize, fortint *readsize, fortint *status, fortint *stream);
void PSEUREAD(char *buffer, fortint *buffsize, fortint *readsize, fortint *status, fortint *stream);
void PBREAD3(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);
void PBOPEN3(fortint *unit, _fcd name, _fcd mode, fortint *iret, fortint l1, fortint l2);
void PBCLOSE3(fortint *unit, fortint *iret);
void PBSEEK3(fortint *unit, fortint *offset, fortint *whence, fortint *iret);
void PBWRITE3(fortint *unit, char *buffer, fortint *nbytes, fortint *iret);

/* Conflicting types 
void PBSIZE(fortint *unit, fortint *plen);
void PBSIZE(FILE **in, fortint *plen);
*/
/* readany.c */
fortint readany(FILE *file, char *buffer, fortint *prod_len);

/* readbufr.c */
fortint readbufr(FILE *file, char *buffer, fortint *bufr_prod_len);

/* readcrex.c */
fortint readcrex(FILE *file, char *buffer, fortint *crex_prod_len);

/* readgrib.c */
fortint readgrib(FILE *file, char *buffer, fortint *grib_prod_len);

/* readnext.c | */
fortint readnext(char *buffer, fortint *grib_prod_len, fortint (*read_func )(char *, fortint, void *), void *stream);

#include "fileRead.h"
/* readprod.c | readprod_alpha.c*/
/* Conflicting types
 * fortint readprod(char *prod_id, char *buffer, fortint *size, fortint (*fileRead )(char *, fortint, void *), fortint (*fileSeek )(void *, fortint, fortint ), fortint (*fileTell )(void *), void *stream);
 * fortint readprod(char *prod_id, char *buffer, fortint *size, fortint (*fileRead )(char *, fortint, void *), OFF_T (*fileSeek )(void *, OFF_T, fortint ), OFF_T (*fileTell )(void *), void *stream);
*/
fortint readprod_decode_unsigned_byte_long(const unsigned char *p, long o, int l);
/* recheckLength.c */
long len3oct(char *p);
long recheckLength(char *buffer);

/* setpar.c */
void setpar_(fortint *kbit, fortint *kneg, fortint *kpar);
void setpar(fortint *kbit, fortint *kneg, fortint *kpar);
#endif
