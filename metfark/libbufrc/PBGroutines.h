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
#ifndef PBG_ROUTINES_H
#define PBG_ROUTINES_H

#include <stdio.h>
#include <sys/types.h>

#include "fortint.h"

#ifdef FOPEN64
extern FILE *fopen64(const char *,const char *);
#define OFF_T off64_t
extern OFF_T ftello64(FILE *);
#else
#define OFF_T off_t
#endif

#define _fcd char *
/*#define fortint int*/
#define integer int
#define bool int

#define MAX_NUMBER_OF_GRIBS 100

typedef struct gribfile {

  FILE *  fp;
  _fcd    fname;
  char readwriteflag;
  integer max;
  integer count;
  OFF_T * offset;
  integer * length;
  integer * parameter;
  integer * level;
  integer * date;
  integer * time;
  integer * timestep;
  integer * localDefinitionNumber;
  integer * type;
  integer * stream;
  integer * repres;
  integer * levtype;
  integer * number;
  integer * vdate;
  integer * vtime;
  integer * tcNumber;
  integer * tcTotal;
  integer * clusterMethod;
  integer * tcStep;
  integer * clusterStepEnd;
  integer * tcNorth;
  integer * tcWest;
  integer * tcSouth;
  integer * tcEast;
  integer * clusterOpFcNumber;
  integer * clusterControlFcNumber;
  integer * tcNumOfFcs;
  integer * probScale;
  integer * probIndicator;
  integer * probLower;
  integer * probUpper;
  integer * ni;
  integer * nj;
  integer * nlat;
  integer * nlon;
  integer * slat;
  integer * slon;
  integer * di;
  integer * dj;
  integer * splat;
  integer * splon;
  integer * quasi;
  integer * directionNumber;
  integer * frequencyNumber;
  integer * optimisationTime;
  integer * leadTime;
  integer * sensitiveAreaDomain;
  integer * sensitiveAreaMethod;
  integer * verifyingMonth;
  integer * averagingPeriod;
  integer * forecastMonth;
  integer * referenceDate;
  integer * climateDateFrom;
  integer * climateDateTo;
  integer * thresholdUnitsScale;
  integer * thresholdIndicator;
  integer * lowerThreshold;
  integer * upperThreshold;

  struct gribfile *next;

}gribfile;

typedef struct collection {

  integer (* exists)(_fcd , fortint);
  integer (* addRead)(_fcd , fortint);
  integer (* addWrite)(_fcd , fortint);
  void (* removeFile)(_fcd , fortint);

  integer count;
  integer max;
  struct gribfile * files;

}collection;


/*
// Prototypes
*/

#ifdef __cplusplus
extern "C" {
#endif

fortint pbgtotl(_fcd filename, fortint filename_len);
fortint pbgtotl_(_fcd filename, fortint filename_len);
fortint pbgleng(_fcd filename, fortint * n, fortint filename_len);
fortint pbgleng_(_fcd filename, fortint * n, fortint filename_len);
fortint pbgoffs(_fcd filename, fortint * n, fortint filename_len);
fortint pbgoffs_(_fcd filename, fortint * n, fortint filename_len);
fortint pbgparm(_fcd filename, fortint * n, fortint filename_len);
fortint pbgparm_(_fcd filename, fortint * n, fortint filename_len);
fortint pbglevl(_fcd filename, fortint * n, fortint filename_len);
fortint pbglevl_(_fcd filename, fortint * n, fortint filename_len);
fortint pbgdate(_fcd filename, fortint * n, fortint filename_len);
fortint pbgdate_(_fcd filename, fortint * n, fortint filename_len);
fortint pbgtime(_fcd filename, fortint * n, fortint filename_len);
fortint pbgtime_(_fcd filename, fortint * n, fortint filename_len);
fortint pbgstep(_fcd filename, fortint * n, fortint filename_len);
fortint pbgstep_(_fcd filename, fortint * n, fortint filename_len);
fortint pbgtype(_fcd filename, fortint * n, fortint filename_len);
fortint pbgtype_(_fcd filename, fortint * n, fortint filename_len);
fortint pbgstrm(_fcd filename, fortint * n, fortint filename_len);
fortint pbgstrm_(_fcd filename, fortint * n, fortint filename_len);
fortint pbgrepr(_fcd filename, fortint * n, fortint filename_len);
fortint pbgrepr_(_fcd filename, fortint * n, fortint filename_len);
fortint pbglevt(_fcd filename, fortint * n, fortint filename_len);
fortint pbglevt_(_fcd filename, fortint * n, fortint filename_len);
fortint pbgfind(_fcd filename,
                fortint * param, fortint * level, fortint * date,
                fortint * time,  fortint * step,  fortint * n,
                fortint filename_len);
fortint pbgfind_(_fcd filename,
                fortint * param, fortint * level, fortint * date,
                fortint * time,  fortint * step,  fortint * n,
                fortint filename_len);
fortint pbgafind(_fcd filename, fortint * list, fortint * n,
                fortint filename_len);
fortint pbgafind_(_fcd filename, fortint * list, fortint * n,
                fortint filename_len);
fortint pbgbfind(_fcd filename, fortint * list, fortint * n,
                fortint filename_len);
fortint pbgbfind_(_fcd filename, fortint * list, fortint * n,
                fortint filename_len);
fortint pbgvfind(_fcd filename,
                fortint * param, fortint * level, fortint * vdate,
                fortint * vtime, fortint * status, fortint * n,
                fortint filename_len);
fortint pbgvfind_(_fcd filename,
                fortint * param, fortint * level, fortint * vdate,
                fortint * vtime, fortint * status, fortint * n,
                fortint filename_len);
fortint pbgxfind_(fortint * grib1, _fcd filename, fortint filename_len);
fortint pbgxfind(fortint * grib1, _fcd filename, fortint filename_len);

fortint pbgget(_fcd filename, fortint * buffer, fortint * bufflen,
               fortint * n, fortint filename_len);
fortint pbgget_(_fcd filename, fortint * buffer, fortint * bufflen,
               fortint * n, fortint filename_len);

fortint pbgput(_fcd filename, fortint * buffer, fortint * bufflen,
               fortint filename_len);
fortint pbgput_(_fcd filename, fortint * buffer, fortint * bufflen,
               fortint filename_len);

void pbgclos(_fcd filename, fortint filename_len);
void pbgclos_(_fcd filename, fortint filename_len);

#ifdef __cplusplus
}
#endif

#endif /* end of  PBG_ROUTINES_H */
