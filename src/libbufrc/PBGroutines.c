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
#include <ctype.h>

#ifdef linux
# if !defined __off64_t_defined
typedef __off64_t off64_t;
#define __off64_t_defined
#endif
#endif 

#include "PBGroutines.h"

#ifdef gribAPI
long date_to_Julian(long ddate);
long Julian_to_date(long jdate);
#else
long date_to_julian(long ddate);
long julian_to_date(long jdate);
#endif

integer pbginitInput(_fcd filename, fortint filename_len);
integer pbginitOutput(_fcd filename, fortint filename_len);
void dsgnbt_(fortint *, fortint *, fortint *, fortint *);
void copyName(_fcd * , _fcd , fortint );

fortint readgrib(FILE * file, unsigned char * buffer, fortint * prod_len);

void pbgindx(fortint);

void gribdata( unsigned char * header,
               integer number_of_fields,
               integer * parameter,
               integer * level,
               integer * date,
               integer * time,
               integer * timestep,
               integer * localDefinitionNumber,
               integer * type,
               integer * stream,
               integer * repres,
               integer * levtype,
               integer * number,
               integer * vdate,
               integer * vtime,
               integer * tcNumber,
               integer * tcTotal,
               integer * clusterMethod,
               integer * tcStep,
               integer * clusterStepEnd,
               integer * tcNorth,
               integer * tcWest,
               integer * tcSouth,
               integer * tcEast,
               integer * clusterOpFcNumber,
               integer * clusterControlFcNumber,
               integer * tcNumOfFcs,
               integer * probScale,
               integer * probIndicator,
               integer * probLower,
               integer * probUpper,
               integer * ni,
               integer * nj,
               integer * nlat,
               integer * nlon,
               integer * slat,
               integer * slon,
               integer * di,
               integer * dj,
               integer * splat,
               integer * splon,
               integer * quasi,
               integer * directionNumber,
               integer * frequencyNumber,
               integer * optimisationTime,
               integer * leadTime,
               integer * sensitiveAreaDomain,
               integer * sensitiveAreaMethod,
               integer * verifyingMonth,
               integer * averagingPeriod,
               integer * forecastMonth,
               integer * referenceDate,
               integer * climateDateFrom,
               integer * climateDateTo,
               integer * thresholdUnitsScale,
               integer * thresholdIndicator,
               integer * lowerThreshold,
               integer * upperThreshold
             );

static gribfile * latestFile(collection);
static gribfile * currentFile(collection, integer);

/*
// 
*/
#define DEBUGOFF 1
#define DEBUG1 (debugSet > DEBUGOFF )
#define DEBUG2 (debugSet > (DEBUGOFF + 1) )
static char * debugLevel;
static int debugSet = 0;

#define ONEBYTELONG(a)   (long) ( *(a) )
#define TWOBYTELONG(a)   (long) ( (*(a))<<8 | (*((a)+1))<<0 )
#define THREEBYTELONG(a) (long) (TWOBYTELONG((a))<<8 | (*((a)+2))<<0 )
#define FOURBYTELONG(a)  (long) (THREEBYTELONG((a))<<8 | (*((a)+3))<<0 )

#define MISSING -1
#define NOTGIVEN(x) ((x) == MISSING )
#define MATCH(a,b)  (NOTGIVEN((a)) || ((a) == (b)))

#define HEADLEN 10000

integer exists(_fcd , fortint );
integer addRead(_fcd , fortint );
integer addWrite(_fcd , fortint );
void removeFile(_fcd , fortint );

collection openFiles = {exists,addRead,addWrite,removeFile,-1,-1,NULL};

void pbgindx(fortint thisFile) {
/*
// Fills in details of current file
*/
unsigned char header[HEADLEN];
fortint status;
fortint headerlen;
OFF_T space;
integer number_of_fields = 0;
gribfile * file = currentFile(openFiles,thisFile);

/*
// Loop through products in the file
*/
  file->offset[0] = 0;
  do {
    headerlen = HEADLEN;
    status = readgrib( file->fp, header, &headerlen);

/*
// Accept product if OK or if only problem is 'buffer too small' or
// on EOF
*/
    if( (status!=0) && (status!=-3) && (status!=-1) ) exit(1);
    if( status == -1 ) break;

/*
// Note the product byte offset in the file
*/
    file->length[number_of_fields] = (OFF_T) headerlen;
#ifdef FOPEN64
    file->offset[number_of_fields] =
      ftello64(file->fp) - file->length[number_of_fields];
#else
    file->offset[number_of_fields] =
      (OFF_T) ftell(file->fp) - file->length[number_of_fields];
#endif

/*
// If it's a GRIB edition 1 product, pick up more information
// (parameter,level,..)
*/
    if( strncmp((char *)header,"GRIB", 4)==0 ) {
/*
//    Eliminate GRIB editions 0 and -1
*/
      if( (header[ 7] != '\1') ) break;
      if( (header[19] == '\0') && (header[20] == '\0') &&
          (header[21] == '\0') && (header[22] == '\0') &&
          (header[23] == '\0') ) break;

      gribdata( header,
                number_of_fields,
                file->parameter,
                file->level,
                file->date,
                file->time,
                file->timestep,
                file->localDefinitionNumber,
                file->type,
                file->stream,
                file->repres,
                file->levtype,
                file->number,
                file->vdate,
                file->vtime,
                file->tcNumber,
                file->tcTotal,
                file->clusterMethod,
                file->tcStep,
                file->clusterStepEnd,
                file->tcNorth,
                file->tcWest,
                file->tcSouth,
                file->tcEast,
                file->clusterOpFcNumber,
                file->clusterControlFcNumber,
                file->tcNumOfFcs,
                file->probScale,
                file->probIndicator,
                file->probLower,
                file->probUpper,
                file->ni,
                file->nj,
                file->nlat,
                file->nlon,
                file->slat,
                file->slon,
                file->di,
                file->dj,
                file->splat,
                file->splon,
                file->quasi,
                file->directionNumber,
                file->frequencyNumber,
                file->optimisationTime,
                file->leadTime,
                file->sensitiveAreaDomain,
                file->sensitiveAreaMethod,
                file->verifyingMonth,
                file->averagingPeriod,
                file->forecastMonth,
                file->referenceDate,
                file->climateDateFrom,
                file->climateDateTo,
                file->thresholdUnitsScale,
                file->thresholdIndicator,
                file->lowerThreshold,
                file->upperThreshold
              );

#define REALLOC(a) (a)  =  realloc((a) , (size_t) space)

      number_of_fields++;
      if( number_of_fields == file->max ) {
        integer newSize = (file->max)*2;
        file->max = newSize;

        space = (OFF_T) (sizeof(OFF_T)*newSize);
        REALLOC(file->offset);

        space = (OFF_T) (sizeof(integer)*newSize);
        REALLOC(file->length);
        REALLOC(file->parameter);
        REALLOC(file->level);
        REALLOC(file->date);
        REALLOC(file->time);
        REALLOC(file->timestep);
        REALLOC(file->localDefinitionNumber);
        REALLOC(file->type);
        REALLOC(file->stream);
        REALLOC(file->repres);
        REALLOC(file->levtype);
        REALLOC(file->number);
        REALLOC(file->vdate);
        REALLOC(file->vtime);
        REALLOC(file->tcNumber);
        REALLOC(file->tcTotal);
        REALLOC(file->clusterMethod);
        REALLOC(file->tcStep);
        REALLOC(file->clusterStepEnd);
        REALLOC(file->tcNorth);
        REALLOC(file->tcWest);
        REALLOC(file->tcSouth);
        REALLOC(file->tcEast);
        REALLOC(file->clusterOpFcNumber);
        REALLOC(file->clusterControlFcNumber);
        REALLOC(file->tcNumOfFcs);
        REALLOC(file->probScale);
        REALLOC(file->probIndicator);
        REALLOC(file->probLower);
        REALLOC(file->probUpper);
        REALLOC(file->ni);
        REALLOC(file->nj);
        REALLOC(file->nlat);
        REALLOC(file->nlon);
        REALLOC(file->slat);
        REALLOC(file->slon);
        REALLOC(file->di);
        REALLOC(file->dj);
        REALLOC(file->splat);
        REALLOC(file->splon);
        REALLOC(file->quasi);
        REALLOC(file->directionNumber);
        REALLOC(file->frequencyNumber);
        REALLOC(file->optimisationTime);
        REALLOC(file->leadTime);
        REALLOC(file->sensitiveAreaDomain);
        REALLOC(file->sensitiveAreaMethod);
        REALLOC(file->verifyingMonth);
        REALLOC(file->averagingPeriod);
        REALLOC(file->forecastMonth);
        REALLOC(file->referenceDate);
        REALLOC(file->climateDateFrom);
        REALLOC(file->climateDateTo);
        REALLOC(file->thresholdUnitsScale);
        REALLOC(file->thresholdIndicator);
        REALLOC(file->lowerThreshold);
        REALLOC(file->upperThreshold);
      }
    }
        
  } while( (!feof(file->fp)) &&
           (number_of_fields < file->max)
         );

  file->count = number_of_fields;

  return;
}

void gribdata( unsigned char * header,
               integer index,
               integer * parameter,
               integer * level,
               integer * date,
               integer * time,
               integer * timestep,
               integer * localDefinitionNumber,
               integer * type,
               integer * stream,
               integer * repres,
               integer * levtype,
               integer * number,
               integer * vdate,
               integer * vtime,
               integer * tcNumber,
               integer * tcTotal,
               integer * clusterMethod,
               integer * tcStep,
               integer * clusterStepEnd,
               integer * tcNorth,
               integer * tcWest,
               integer * tcSouth,
               integer * tcEast,
               integer * clusterOpFcNumber,
               integer * clusterControlFcNumber,
               integer * tcNumOfFcs,
               integer * probScale,
               integer * probIndicator,
               integer * probLower,
               integer * probUpper,
               integer * ni,
               integer * nj,
               integer * nlat,
               integer * nlon,
               integer * slat,
               integer * slon,
               integer * di,
               integer * dj,
               integer * splat,
               integer * splon,
               integer * quasi,
               integer * directionNumber,
               integer * frequencyNumber,
               integer * optimisationTime,
               integer * leadTime,
               integer * sensitiveAreaDomain,
               integer * sensitiveAreaMethod,
               integer * verifyingMonth,
               integer * averagingPeriod,
               integer * forecastMonth,
               integer * referenceDate,
               integer * climateDateFrom,
               integer * climateDateTo,
               integer * thresholdUnitsScale,
               integer * thresholdIndicator,
               integer * lowerThreshold,
               integer * upperThreshold
             )
/*
// Returns parameter, level, date, time , etc information 
// for a GRIB product.
*/
{

/*
// Decode the GRIB product
*/

#define POLAR_STEREO 5

#define SEC1 7
#define LEN1 8
#define TABLE 4
#define PARAM 9
#define LEVELTYPE 10
#define LEVEL1 11
#define LEVEL2 12
#define YY 13
#define MM 14
#define DD 15
#define HOUR 16
#define MIN 17
#define TIMEUNIT 18
#define STEP1 19
#define STEP2 20
#define TIMERANGE 21
#define CENTURY 25
#define LOCALDEF 41
#define CLASS 42
#define TYPE 43
#define STREAM 44
#define NUMBER 50
#define REPRES 6
#define NI 7
#define NJ 9
#define NLAT 11
#define NLON 14
#define SLAT 18
#define SLON 21
#define DI 24
#define DJ 26
#define DI_PS 21
#define DJ_PS 24
#define SPLAT 33
#define SPLON 36

#define ISOTHERMAL_LEVEL   20
#define ISOBARIC_LEVEL     100
#define ALTITUDE           103
#define HEIGHT             105
#define SIGMA_LEVEL        107
#define HYBRID_LEVEL       109
#define DEPTH_BELOW_LAND   111
#define ISENTROPIC_LEVEL   113
#define PRESSURE_DIFF      115
#define VORTICITY_SURF     117
#define HEIGHT_HIGH_PREC   125
#define SATELLITE_BAND     127
#define DEPTH_BELOW_SEA    160
#define ENTIRE_ATMOSPHERE  200
#define ENTIRE_OCEAN       201
#define ISOBARIC_HIGH_PREC 210

#define PROBABILITY 16
#define PROBSCALE   52
#define PROBINDIC   53
#define PROBLOWER   54
#define PROBUPPER   56

#define CLUSTERMEAN         14
#define CLUSTERSD           15
#define CLUSTERNUMBER       50
#define CLUSTERTOTAL        51
#define CLUSTERMETHOD       53
#define CLUSTERSTEP         54
#define CLUSTERSTEPEND      56
#define CLUSTERNORTH        58
#define CLUSTERWEST         61
#define CLUSTERSOUTH        64
#define CLUSTEREAST         67
#define CLUSTEROPFCNUM      70
#define CLUSTERCONTROLFCNUM 71
#define CLUSTERFORECASTS    72

#define TUBETYPE      24
#define TUBENUMBER    50
#define TUBETOTAL     51
#define TUBESTEP      71
#define TUBENORTH     55
#define TUBEWEST      58
#define TUBESOUTH     61
#define TUBEEAST      64
#define TUBEFORECASTS 79

#define CLUSTERMEAN         14
#define CLUSTERSD           15
#define CLUSTERNUMBER       50
#define CLUSTERTOTAL        51
#define CLUSTERMETHOD       53
#define CLUSTERSTEP         54
#define CLUSTERSTEPEND      56
#define CLUSTERNORTH        58
#define CLUSTERWEST         61
#define CLUSTERSOUTH        64
#define CLUSTEREAST         67
#define CLUSTEROPFCNUM      70
#define CLUSTERCONTROLFCNUM 71
#define CLUSTERFORECASTS    72

#define DIRECTIONNUMBER 52
#define FREQUENCYNUMBER 53

#define OPTIMISATIONTIME    92
#define LEADTIME            93
#define SENSITIVEAREADOMAIN 94
#define SENSITIVEAREAMETHOD 95

#define VERIFYINGMONTH      56
#define AVERAGINGPERIOD     60
#define FORECASTMONTH       61
#define REFERENCEDATE       63
#define CLIMATEDATEFROM     67
#define CLIMATEDATETO       71
#define THRESHOLDUNITSSCALE 75
#define THRESHOLDINDICATOR  76
#define LOWERTHRESHOLD      77
#define UPPERTHRESHOLD      79

integer table, leveltype, timeunit, timerange, multiplier, century, year;
integer twoByteNumber, ECclass, ensembleNumber;
integer section2Offset;
integer status, eight = 8, sixteen = 16, twentyFour = 24;

  table = header[SEC1+TABLE];
  parameter[index] = table*1000 + header[SEC1+PARAM];

  leveltype = header[SEC1+LEVELTYPE];
  levtype[index] = leveltype;

  switch( leveltype ) {

    case ISOTHERMAL_LEVEL  :
    case ISOBARIC_LEVEL    :
    case ALTITUDE          :
    case HEIGHT            :
    case SIGMA_LEVEL       :
    case HYBRID_LEVEL      :
    case DEPTH_BELOW_LAND  :
    case ISENTROPIC_LEVEL  :
    case PRESSURE_DIFF     :
    case VORTICITY_SURF    :
    case HEIGHT_HIGH_PREC  :
    case SATELLITE_BAND    :
    case DEPTH_BELOW_SEA   :
    case ENTIRE_ATMOSPHERE :
    case ENTIRE_OCEAN      :
    case ISOBARIC_HIGH_PREC:
/*            level[index] = header[SEC1+LEVEL1]*256 + header[SEC1+LEVEL2]; */
              level[index] = TWOBYTELONG(&header[SEC1+LEVEL1]);
              break;

    default: level[index] = header[SEC1+LEVEL1];
  }

  century = header[SEC1+CENTURY];
  year = (century-1)*100 + header[SEC1+YY];
  date[index] = year*10000 + header[SEC1+MM]*100 + header[SEC1+DD];

  time[index] = header[SEC1+HOUR]*100 + header[SEC1+MIN];

  timeunit = header[SEC1+TIMEUNIT];

#define DAY          2
#define MONTH        3
#define THREE_HOURS  10
#define SIX_HOURS    11
#define TWELVE_HOURS 12

  switch( timeunit ) {

    case DAY :
             multiplier = 24;
             break;

    case MONTH :
             multiplier = 30*24;
             break;

    case THREE_HOURS :
             multiplier = 3;
             break;

    case SIX_HOURS :
             multiplier = 6;
             break;

    case TWELVE_HOURS :
             multiplier = 12;
             break;

    default: multiplier = 1;
  }

#define RANGE_P1_TO_P2 2
#define ACCUMULATION 4
#define DIFFERENCE   5
#define TWOBYTE      10

  timerange = header[SEC1+TIMERANGE];

  switch( timerange ) {

    case TWOBYTE :
             timestep[index] = TWOBYTELONG(&header[SEC1+STEP1])*multiplier;
             break;

    default: timestep[index] = header[SEC1+STEP2]*multiplier*10000 +
                               header[SEC1+STEP1]*multiplier;
  }

  type[index] = header[SEC1+TYPE];

#define ANALYSIS    2

  if( type[index] == ANALYSIS ) {
    vdate[index] = date[index];
    vtime[index] = time[index];
  }
  else {
    long iverd  = (time[index] + timestep[index]*100) / 2400;
#ifdef gribAPI
    long ijvday = date_to_Julian(date[index]) + iverd;
    vdate[index] = Julian_to_date(ijvday);
#else
    long ijvday = date_to_julian(date[index]) + iverd;
    vdate[index] = julian_to_date(ijvday);
#endif
    vtime[index] = (time[index] + timestep[index]*100) - (iverd*2400);
  }

  localDefinitionNumber[index] = header[SEC1+LOCALDEF];
  ECclass = header[SEC1+CLASS];
  stream[index] = TWOBYTELONG(&header[SEC1+STREAM]);

  twoByteNumber = ( (localDefinitionNumber[index] == 4) && (stream[index] == 1090) ) ||
                  (localDefinitionNumber[index] == 9) ||
                  (localDefinitionNumber[index] == 15) ||
                  (localDefinitionNumber[index] == 16) ||
                  (localDefinitionNumber[index] == 21) ||
                  (localDefinitionNumber[index] == 23);

  if( twoByteNumber )
    ensembleNumber = TWOBYTELONG(&header[SEC1+NUMBER]);
  else
    ensembleNumber = header[SEC1+NUMBER];
  number[index] = ensembleNumber;

/*
// Extra information for tubes
*/
  if( type[index] == TUBETYPE ) {
    tcNumber[index]    = header[SEC1+TUBENUMBER];
    tcTotal[index]     = header[SEC1+TUBETOTAL];
    tcStep[index]      = TWOBYTELONG(&header[SEC1+TUBESTEP]);
    tcNorth[index]     = THREEBYTELONG(&header[SEC1+TUBENORTH]);
    dsgnbt_(&tcNorth[index],&tcNorth[index],&twentyFour,&status);
    tcWest[index]      = THREEBYTELONG(&header[SEC1+TUBEWEST]);
    dsgnbt_(&tcWest[index],&tcWest[index],&twentyFour,&status);
    tcSouth[index]     = THREEBYTELONG(&header[SEC1+TUBESOUTH]);
    dsgnbt_(&tcSouth[index],&tcSouth[index],&twentyFour,&status);
    tcEast[index]      = THREEBYTELONG(&header[SEC1+TUBEEAST]);
    dsgnbt_(&tcEast[index],&tcEast[index],&twentyFour,&status);
    tcNumOfFcs[index] = header[SEC1+TUBEFORECASTS];
  }

/*
// Extra information for clusters
*/
  if( (type[index] == CLUSTERMEAN) || (type[index] == CLUSTERSD) ) {
    tcNumber[index]    = header[SEC1+CLUSTERNUMBER];
    tcTotal[index]     = header[SEC1+CLUSTERTOTAL];
    clusterMethod[index]     = header[SEC1+CLUSTERMETHOD];
    tcStep[index]      = TWOBYTELONG(&header[SEC1+CLUSTERSTEP]);
    clusterStepEnd[index]      = TWOBYTELONG(&header[SEC1+CLUSTERSTEPEND]);
    tcNorth[index]     = THREEBYTELONG(&header[SEC1+CLUSTERNORTH]);
    dsgnbt_(&tcNorth[index],&tcNorth[index],&twentyFour,&status);
    tcWest[index]      = THREEBYTELONG(&header[SEC1+CLUSTERWEST]);
    dsgnbt_(&tcWest[index],&tcWest[index],&twentyFour,&status);
    tcSouth[index]     = THREEBYTELONG(&header[SEC1+CLUSTERSOUTH]);
    dsgnbt_(&tcSouth[index],&tcSouth[index],&twentyFour,&status);
    tcEast[index]      = THREEBYTELONG(&header[SEC1+CLUSTEREAST]);
    dsgnbt_(&tcEast[index],&tcEast[index],&twentyFour,&status);
    clusterOpFcNumber[index] = header[SEC1+CLUSTEROPFCNUM];
    clusterControlFcNumber[index] = header[SEC1+CLUSTERCONTROLFCNUM];
    tcNumOfFcs[index] = header[SEC1+CLUSTERFORECASTS];
  }
/*
// Extra information for probabilities
*/
  if( type[index] == PROBABILITY ) {
    probScale[index] = header[SEC1+PROBSCALE];
    dsgnbt_(&probScale[index],&probScale[index],&eight,&status);
    probIndicator[index] = header[SEC1+PROBINDIC];
    probLower[index] = 65535;
    if( probIndicator[index] != 2 ) {
      probLower[index] = TWOBYTELONG(&header[SEC1+PROBLOWER]);
      dsgnbt_(&probLower[index],&probLower[index],&sixteen,&status);
    }
    probUpper[index] = 65535;
    if( probIndicator[index] != 1 ) {
      probUpper[index] = TWOBYTELONG(&header[SEC1+PROBUPPER]);
      dsgnbt_(&probUpper[index],&probUpper[index],&sixteen,&status);
    }
  }
/*
// Extra information for wave 2D spectra
*/
  if( localDefinitionNumber[index] == 13 ) {
    directionNumber[index] = header[SEC1+DIRECTIONNUMBER];
    frequencyNumber[index] = header[SEC1+FREQUENCYNUMBER];
  }
/*
// Extra information for sensitive area forecasts
*/
  if( localDefinitionNumber[index] == 21 ) {
    optimisationTime[index]    = header[SEC1+OPTIMISATIONTIME];
    leadTime[index]            = header[SEC1+LEADTIME];
    sensitiveAreaDomain[index] = header[SEC1+SENSITIVEAREADOMAIN];
    sensitiveAreaMethod[index] =
      TWOBYTELONG(&header[SEC1+SENSITIVEAREAMETHOD]);
  }
/*
// Extra information for coupled atmospheric, wave and ocean models
*/
  if( localDefinitionNumber[index] == 23 ) {
    unsigned char scale;

    verifyingMonth[index]     = FOURBYTELONG(&header[SEC1+VERIFYINGMONTH]);
    averagingPeriod[index]    = ONEBYTELONG(&header[SEC1+AVERAGINGPERIOD]);
    forecastMonth[index]      = TWOBYTELONG(&header[SEC1+FORECASTMONTH]);
    referenceDate[index]      = FOURBYTELONG(&header[SEC1+REFERENCEDATE]);
    climateDateFrom[index]    = FOURBYTELONG(&header[SEC1+CLIMATEDATEFROM]);
    climateDateTo[index]      = FOURBYTELONG(&header[SEC1+CLIMATEDATETO]);
    scale = header[SEC1+THRESHOLDUNITSSCALE];
    if( scale & 0x80 ) {
      scale &= 0x7f;
      thresholdUnitsScale[index] = - ONEBYTELONG(&scale);
    }
    else
      thresholdUnitsScale[index] = ONEBYTELONG(&scale);
    thresholdIndicator[index] = ONEBYTELONG(&header[SEC1+THRESHOLDINDICATOR]);
    lowerThreshold[index]     = TWOBYTELONG(&header[SEC1+LOWERTHRESHOLD]);
    upperThreshold[index]     = TWOBYTELONG(&header[SEC1+UPPERTHRESHOLD]);
  }

/*
// Section 2
*/
  section2Offset = THREEBYTELONG(&header[LEN1]);
  repres[index] = header[LEN1+section2Offset-1+REPRES];

  ni[index] = TWOBYTELONG(&header[LEN1+section2Offset-1+NI]);
  nj[index] = TWOBYTELONG(&header[LEN1+section2Offset-1+NJ]);

  nlat[index] = THREEBYTELONG(&header[LEN1+section2Offset-1+NLAT]);
  dsgnbt_(&nlat[index],&nlat[index],&twentyFour,&status);

  nlon[index] = THREEBYTELONG(&header[LEN1+section2Offset-1+NLON]);
  dsgnbt_(&nlon[index],&nlon[index],&twentyFour,&status);

  slat[index] = THREEBYTELONG(&header[LEN1+section2Offset-1+SLAT]);
  dsgnbt_(&slat[index],&slat[index],&twentyFour,&status);

/*
// Section 2 is different for polar stereographic
*/
  if( repres[index] == POLAR_STEREO ) {
    di[index] = THREEBYTELONG(&header[LEN1+section2Offset-1+DI_PS]);
    dj[index] = THREEBYTELONG(&header[LEN1+section2Offset-1+DJ_PS]);
  }
  else {
    slon[index] = THREEBYTELONG(&header[LEN1+section2Offset-1+SLON]);
    dsgnbt_(&slon[index],&slon[index],&twentyFour,&status);

    di[index] = TWOBYTELONG(&header[LEN1+section2Offset-1+DI]);
    dj[index] = TWOBYTELONG(&header[LEN1+section2Offset-1+DJ]);

    splat[index] = THREEBYTELONG(&header[LEN1+section2Offset-1+SPLAT]);
    dsgnbt_(&splat[index],&splat[index],&twentyFour,&status);

    splon[index] = THREEBYTELONG(&header[LEN1+section2Offset-1+SPLON]);
    dsgnbt_(&splon[index],&splon[index],&twentyFour,&status);
  }

  quasi[index] = ( (ni[index] == 0xFFFF) || (nj[index] == 0xFFFF) );

  return;

}

/*
// ****************************************************************
*/
fortint pbgtotl_(_fcd filename, fortint filename_len) {
/*
// Returns number of GRIB products in the file.
*/
integer thisFile;
gribfile * file;

  thisFile = pbginitInput(filename,filename_len);
  file = currentFile(openFiles, thisFile);

  if( DEBUG1 ) {
    _fcd pfile;
    copyName(&pfile, filename, filename_len);
    printf("PBGTOTL: Number of GRIBs in file %s = %d\n", pfile, file->count);
    free(pfile);
  }
  return ( file->count );
}

fortint pbgtotl(_fcd filename, fortint filename_len) {
  return pbgtotl_(filename,filename_len);
}

fortint pbgtotl__(_fcd filename, fortint filename_len) {
  return pbgtotl_(filename,filename_len);
}

/*
// ****************************************************************
*/
fortint pbgleng_(_fcd filename, fortint * n, fortint filename_len) {
/*
// Returns length (in bytes) of GRIB product n or -1 if n is greater
// than the number of products in the file, or less than 1.
*/
integer index = (*n)-1;
integer thisFile;
gribfile * file;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {
      if( DEBUG1 ) {
        _fcd pfile;
        copyName(&pfile, filename, filename_len);
        printf("PBGLENG: length of GRIB %d in file %s = %d\n",
               (index+1), pfile, file->length[index]);
        free(pfile);
      }
      return ( file->length[index] );
    }
  }

  return (-1);
}

fortint pbgleng(_fcd filename, fortint * n, fortint filename_len) {
  return pbgleng_(filename,n,filename_len);
}

fortint pbgleng__(_fcd filename, fortint * n, fortint filename_len) {
  return pbgleng_(filename,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbgoffs_(_fcd filename, fortint * n, fortint filename_len) {
/*
// Returns offset (in bytes) of GRIB product n or -1 if n is greater
// than the number of products in the file, or less than 1.
*/
integer index = (*n)-1;
integer thisFile;
gribfile * file;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {
      if( DEBUG1 ) {
        _fcd pfile;
        copyName(&pfile, filename, filename_len);
        printf("PBGOFFS: offset of GRIB %d in file %s = %ld\n",
               (index+1), pfile, file->offset[index]);
        free(pfile);
      }
      return ( file->offset[index] );
    }
  }

  return (-1);
}

fortint pbgoffs(_fcd filename, fortint * n, fortint filename_len) {
  return pbgoffs_(filename,n,filename_len);
}

fortint pbgoffs__(_fcd filename, fortint * n, fortint filename_len) {
  return pbgoffs_(filename,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbgparm_(_fcd filename, fortint * n, fortint filename_len) {
/*
// Returns parameter number of GRIB product n or -1 if n is greater
// than the number of products in the file, or less than 1.
*/
integer index = (*n)-1;
integer thisFile;
gribfile * file;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {
      if( DEBUG1 ) {
        _fcd pfile;
        copyName(&pfile, filename, filename_len);
        printf("PBGPARM: parameter number of GRIB %d in file %s = %d\n",
               (index+1), pfile, file->parameter[index]);
        free(pfile);
      }
      return ( file->parameter[index] );
    }
  }

  return (-1);
}

fortint pbgparm(_fcd filename, fortint * n, fortint filename_len) {
  return pbgparm_(filename,n,filename_len);
}

fortint pbgparm__(_fcd filename, fortint * n, fortint filename_len) {
  return pbgparm_(filename,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbglevl_(_fcd filename, fortint * n, fortint filename_len) {
/*
// Returns level of GRIB product n or -1 if n is greater
// than the number of products in the file, or less than 1.
*/
integer index = (*n)-1;
integer thisFile;
gribfile * file;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {
      if( DEBUG1 ) {
        _fcd pfile;
        copyName(&pfile, filename, filename_len);
        printf("PBGLEVL: level of GRIB %d in file %s = %d\n",
               (index+1), pfile, file->level[index]);
        free(pfile);
      }
      return ( file->level[index] );
    }
  }

  return (-1);
}

fortint pbglevl(_fcd filename, fortint * n, fortint filename_len) {
  return pbglevl_(filename,n,filename_len);
}

fortint pbglevl__(_fcd filename, fortint * n, fortint filename_len) {
  return pbglevl_(filename,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbgdate_(_fcd filename, fortint * n, fortint filename_len) {
/*
// Returns date (as YYMMDD) of GRIB product n or -1 if n is greater
// than the number of products in the file, or less than 1.
*/
integer index = (*n)-1;
integer thisFile;
gribfile * file;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {
      if( DEBUG1 ) {
        _fcd pfile;
        copyName(&pfile, filename, filename_len);
        printf("PBGDATE: date of GRIB %d in file %s = %d\n",
               (index+1), pfile, file->date[index]);
        free(pfile);
      }
      return ( file->date[index] );
    }
  }

  return (-1);
}

fortint pbgdate(_fcd filename, fortint * n, fortint filename_len) {
  return pbgdate_(filename,n,filename_len);
}

fortint pbgdate__(_fcd filename, fortint * n, fortint filename_len) {
  return pbgdate_(filename,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbgtime_(_fcd filename, fortint * n, fortint filename_len) {
/*
// Returns time (as HHMM) of GRIB product n or -1 if n is greater
// than the number of products in the file, or less than 1.
*/
integer index = (*n)-1;
integer thisFile;
gribfile * file;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {
      if( DEBUG1 ) {
        _fcd pfile;
        copyName(&pfile, filename, filename_len);
        printf("PBGTIME: time of GRIB %d in file %s = %d\n",
               (index+1), pfile, file->time[index]);
        free(pfile);
      }
      return ( file->time[index] );
    }
  }

  return (-1);
}

fortint pbgtime(_fcd filename, fortint * n, fortint filename_len) {
  return pbgtime_(filename,n,filename_len);
}

fortint pbgtime__(_fcd filename, fortint * n, fortint filename_len) {
  return pbgtime_(filename,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbgstep_(_fcd filename, fortint * n, fortint filename_len) {
/*
// Returns timestep of GRIB product n or -1 if n is greater
// than the number of products in the file, or less than 1.
*/
integer index = (*n)-1;
integer thisFile;
gribfile * file;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {
      if( DEBUG1 ) {
        _fcd pfile;
        copyName(&pfile, filename, filename_len);
        printf("PBGSTEP: timestep of GRIB %d in file %s = %d\n",
               (index+1), pfile, file->timestep[index]);
        free(pfile);
      }
      return ( file->timestep[index] );
    }
  }

  return (-1);
}

fortint pbgstep(_fcd filename, fortint * n, fortint filename_len) {
  return pbgstep_(filename,n,filename_len);
}

fortint pbgstep__(_fcd filename, fortint * n, fortint filename_len) {
  return pbgstep_(filename,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbgtype_(_fcd filename, fortint * n, fortint filename_len) {
/*
// Returns field type of GRIB product n or -1 if n is greater
// than the number of products in the file, or less than 1.
*/
integer index = (*n)-1;
integer thisFile;
gribfile * file;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {
      if( DEBUG1 ) {
        _fcd pfile;
        copyName(&pfile, filename, filename_len);
        printf("PBGTYPE: type of GRIB %d in file %s = %d\n",
               (index+1), pfile, file->type[index]);
        free(pfile);
      }
      return ( file->type[index] );
    }
  }

  return (-1);
}

fortint pbgtype(_fcd filename, fortint * n, fortint filename_len) {
  return pbgtype_(filename,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbgstrm_(_fcd filename, fortint * n, fortint filename_len) {
/*
// Returns stream of GRIB product n or -1 if n is greater
// than the number of products in the file, or less than 1.
*/
integer index = (*n)-1;
integer thisFile;
gribfile * file;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {
      if( DEBUG1 ) {
        _fcd pfile;
        copyName(&pfile, filename, filename_len);
        printf("PBGSTRM: stream of GRIB %d in file %s = %d\n",
               (index+1), pfile, file->stream[index]);
        free(pfile);
      }
      return ( file->stream[index] );
    }
  }

  return (-1);
}

fortint pbgstrm(_fcd filename, fortint * n, fortint filename_len) {
  return pbgstrm_(filename,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbgrepr_(_fcd filename, fortint * n, fortint filename_len) {
/*
// Returns repres of GRIB product n or -1 if n is greater
// than the number of products in the file, or less than 1.
*/
integer index = (*n)-1;
integer thisFile;
gribfile * file;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {
      if( DEBUG1 ) {
        _fcd pfile;
        copyName(&pfile, filename, filename_len);
        printf("PBGREPR: repres of GRIB %d in file %s = %d\n",
               (index+1), pfile, file->repres[index]);
        free(pfile);
      }
      return ( file->repres[index] );
    }
  }

  return (-1);
}

fortint pbgrepr(_fcd filename, fortint * n, fortint filename_len) {
  return pbgrepr_(filename,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbglevt_(_fcd filename, fortint * n, fortint filename_len) {
/*
// Returns levtype of GRIB product n or -1 if n is greater
// than the number of products in the file, or less than 1.
*/
integer index = (*n)-1;
integer thisFile;
gribfile * file;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {
      if( DEBUG1 ) {
        _fcd pfile;
        copyName(&pfile, filename, filename_len);
        printf("PBGLEVT: levtype of GRIB %d in file %s = %d\n",
               (index+1), pfile, file->levtype[index]);
        free(pfile);
      }
      return ( file->levtype[index] );
    }
  }

  return (-1);
}

fortint pbglevt(_fcd filename, fortint * n, fortint filename_len) {
  return pbglevt_(filename,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbgfind_(
  _fcd filename,
  fortint * param,
  fortint * level,
  fortint * date,
  fortint * time,
  fortint * step, 
  fortint * n,
  fortint filename_len) {
/*
// Returns the index of the GRIB product with the specified param, level, ..
// Searching starts after GRIB product n.
*/
integer index = (*n);
integer p = *param, l = *level, d = *date, t = *time, s = *step;
integer loop;
integer thisFile;
gribfile * file;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);

    for( loop = index; loop < file->count; loop++ ) {
      if( p == file->parameter[loop] )
        if( l == file->level[loop] )
          if( d == file->date[loop] )
            if( t == file->time[loop] )
              if( s == file->timestep[loop] )
                return (loop+1);
    }
  }

  return (-1);
}

fortint pbgfind(
  _fcd filename,
  fortint * param,
  fortint * level,
  fortint * date,
  fortint * time,
  fortint * step, 
  fortint * n,
  fortint filename_len) {
  return pbgfind_(filename,param,level,date,time,step,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbgvfind_(
  _fcd filename,
  fortint * param,
  fortint * level,
  fortint * vdate,
  fortint * vtime,
  fortint * status,
  fortint * n,
  fortint filename_len) {
/*
// Returns the index of the GRIB product with the specified param, level,
// verifying data and time (vdate & vtime).
// Searching starts after GRIB product n.
//
// Returns the index of the first match found, or -1 if no match is found.
// If more than one match is found (eg analysis and forecast fields),
// status returns the index of the second match; otherwise status = 0.
*/
integer index = (*n);
integer p = *param, l = *level, d = *vdate, t = *vtime;
integer loop;
integer thisFile;
gribfile * file;

  *status = 0;

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);

    for( loop = index; loop < file->count; loop++ ) {
      if( p == file->parameter[loop] )
        if( l == file->level[loop] )
          if( d == file->vdate[loop] )
            if( t == file->vtime[loop] ) {
              long innerloop;
              for( innerloop = loop+1; innerloop < file->count; innerloop++ ) {
                if( p == file->parameter[innerloop] )
                  if( l == file->level[innerloop] )
                    if( d == file->vdate[innerloop] )
                      if( t == file->vtime[innerloop] )
                        *status = innerloop+1 ;
              }
              return (loop+1);
            }
    }
  }

  return (-1);
}

fortint pbgvfind(
  _fcd filename,
  fortint * param,
  fortint * level,
  fortint * vdate,
  fortint * vtime,
  fortint * status,
  fortint * n,
  fortint filename_len) {
  return pbgvfind_(filename,param,level,vdate,vtime,status,n,filename_len);
}

/*
// ****************************************************************
*/
#define L_PARAM    0
#define L_LEVEL    1
#define L_DATE     2
#define L_TIME     3
#define L_STEP     4
#define L_ENSEMBLE 5
#define L_TYPE     6
#define L_STREAM   7
#define L_REPRES   8
#define L_LEVTYPE  9
#define L_VDATE    10
#define L_VTIME    11
#define L_NI       12
#define L_NJ       13
#define L_NLAT     14
#define L_NLON     15
#define L_SLAT     16
#define L_SLON     17
#define L_DI       18
#define L_DJ       19
#define L_SPLAT    20
#define L_SPLON    21
#define L_QUASI    22

#define L_PROBSCALE 23
#define L_PROBINDIC 24
#define L_PROBLOWER 25
#define L_PROBUPPER 26

#define L_TUBENUM  23
#define L_TUBETOT  24
#define L_TUBEN    25
#define L_TUBEW    26
#define L_TUBES    27
#define L_TUBEE    28
#define L_TUBESTP  29
#define L_TUBEFC   30

#define L_CLUSNUM  23
#define L_CLUSTOT  24
#define L_CLUSMETH 25
#define L_CLUSSTP  26
#define L_CLUSSTPE 27
#define L_CLUSN    28
#define L_CLUSW    29
#define L_CLUSS    30
#define L_CLUSE    31
#define L_CLUSOPFC 32
#define L_CLUSCFFC 33
#define L_CLUSFC   34

#define L_DIRECTIONNUMBER 23
#define L_FREQUENCYNUMBER 24

#define L_OPTIMISATIONTIME    23
#define L_LEADTIME            24
#define L_SENSITIVEAREADOMAIN 25
#define L_SENSITIVEAREAMETHOD 26

#define L_VERIFYINGMONTH      23
#define L_AVERAGINGPERIOD     24
#define L_FORECASTMONTH       25
#define L_REFERENCEDATE       26
#define L_CLIMATEDATEFROM     27
#define L_CLIMATEDATETO       28
#define L_THRESHOLDUNITSSCALE 29
#define L_THRESHOLDINDICATOR  30
#define L_LOWERTHRESHOLD      31
#define L_UPPERTHRESHOLD      32

#define L_LOCALDEF 35

#define L_ALIST_LENGTH 12
#define L_BLIST_LENGTH 36

fortint pbgafind_(
  _fcd filename,
  fortint * list,
  fortint * n,
  fortint filename_len) {
/*
// Returns the index of the GRIB product with the specified param, level, ..
// Searching starts after GRIB product n.
*/
integer index = (*n);
integer param    = list[L_PARAM],
        level    = list[L_LEVEL],
        date     = list[L_DATE],
        time     = list[L_TIME],
        step     = list[L_STEP],
        ensemble = list[L_ENSEMBLE],
        type     = list[L_TYPE],
        stream   = list[L_STREAM],
        repres   = list[L_REPRES],
        levtype  = list[L_LEVTYPE],
        vdate    = list[L_VDATE],
        vtime    = list[L_VTIME];
integer loop;
integer thisFile;
gribfile * file;

  if( DEBUG1 ) {
    _fcd pfile;
    integer loop;

    copyName(&pfile, filename, filename_len);
    printf("PBGAFIND: searching file %s\n", pfile);
    free(pfile);

    for( loop = 0; loop < L_ALIST_LENGTH ; loop++ ) {
      if( list[loop] != MISSING )
        printf("PBGAFIND: ilist[%d] = %d\n", (loop+1), list[loop] );
    }
  }

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);

    for( loop = index; loop < file->count; loop++ ) {
      if( MATCH(param,file->parameter[loop]) )
      if( MATCH(level,file->level[loop]) )
      if( MATCH(date,file->date[loop]) )
      if( MATCH(time,file->time[loop]) )
      if( MATCH(step,file->timestep[loop]) )
      if( MATCH(ensemble,file->number[loop]) )
      if( MATCH(type,file->type[loop]) )
      if( MATCH(stream,file->stream[loop]) )
      if( MATCH(repres,file->repres[loop]) )
      if( MATCH(levtype,file->levtype[loop]) )
      if( MATCH(vdate,file->vdate[loop]) )
      if( MATCH(vtime,file->vtime[loop]) ) {
        if( DEBUG1 ) {
          _fcd pfile;
          copyName(&pfile, filename, filename_len);
          printf("PBGAFIND: matching GRIB found at position %d in file %s\n",
                 (loop+1), pfile);
          free(pfile);
        }
        return (loop+1);
      }
    }
  }

  return (-1);
}

fortint pbgafind(_fcd filename,fortint* list,fortint* n,fortint filename_len) {
  return pbgafind_(filename,list,n,filename_len);
}

/*
// ****************************************************************
*/
fortint pbgbfind_(
  _fcd filename,
  fortint * list,
  fortint * n,
  fortint filename_len) {
/*
// Returns the index of the GRIB product with the specified param, level, ..
// Searching starts after GRIB product n.
*/
integer index = (*n);
integer param         =  list[L_PARAM],
        level         =  list[L_LEVEL],
        date          =  list[L_DATE],
        time          =  list[L_TIME],
        step          =  list[L_STEP],
        localDef      =  list[L_LOCALDEF],
        ensemble      =  list[L_ENSEMBLE],
        type          =  list[L_TYPE],
        stream        =  list[L_STREAM],
        repres        =  list[L_REPRES],
        levtype       =  list[L_LEVTYPE],
        vdate         =  list[L_VDATE],
        vtime         =  list[L_VTIME],
        ni            =  list[L_NI],
        nj            =  list[L_NJ],
        nlat          =  list[L_NLAT],
        nlon          =  list[L_NLON],
        slat          =  list[L_SLAT],
        slon          =  list[L_SLON],
        di            =  list[L_DI],
        dj            =  list[L_DJ],
        splat         =  list[L_SPLAT],
        splon         =  list[L_SPLON],
        quasi         =  list[L_QUASI];
integer tcNumber               = MISSING,
        tcTotal                = MISSING,
        clusterMethod          = MISSING,
        tcStep                 = MISSING,
        clusterStepEnd         = MISSING,
        tcNorth                = MISSING,
        tcWest                 = MISSING,
        tcSouth                = MISSING,
        tcEast                 = MISSING,
        clusterOpFcNumber      = MISSING,
        clusterControlFcNumber = MISSING,
        tcNumOfFcs             = MISSING,
        probScale              = MISSING,
        probIndicator          = MISSING,
        probLower              = MISSING,
        probUpper              = MISSING,
        directionNumber        = MISSING,
        frequencyNumber        = MISSING,
        optimisationTime       = MISSING,
        leadTime               = MISSING,
        sensitiveAreaDomain    = MISSING,
        sensitiveAreaMethod    = MISSING,
        verifyingMonth         = MISSING,
        averagingPeriod        = MISSING,
        forecastMonth          = MISSING,
        referenceDate          = MISSING,
        climateDateFrom        = MISSING,
        climateDateTo          = MISSING,
        thresholdUnitsScale    = MISSING,
        thresholdIndicator     = MISSING,
        lowerThreshold         = MISSING,
        upperThreshold         = MISSING;
integer loop;
integer thisFile;
gribfile * file;

 if( DEBUG1 ) {
   _fcd pfile;
   integer loop;

   copyName(&pfile, filename, filename_len);
   printf("PBGBFIND: searching file %s\n", pfile);
   free(pfile);

   for( loop = 0; loop < L_BLIST_LENGTH ; loop++ ) {
     if( list[loop] != MISSING )
       printf("PBGBFIND: ilist[%d] = %d\n", (loop+1), list[loop] );
   }
 }

 if( (type == CLUSTERMEAN) || (type == CLUSTERSD) ) {
   tcNumber               =  list[L_CLUSNUM];
   tcTotal                =  list[L_CLUSTOT];
   clusterMethod          =  list[L_CLUSMETH];
   tcStep                 =  list[L_CLUSSTP];
   clusterStepEnd         =  list[L_CLUSSTPE];
   tcNorth                =  list[L_CLUSN];
   tcWest                 =  list[L_CLUSW];
   tcSouth                =  list[L_CLUSS];
   tcEast                 =  list[L_CLUSE];
   clusterOpFcNumber      =  list[L_CLUSOPFC];
   clusterControlFcNumber =  list[L_CLUSCFFC];
   tcNumOfFcs             =  list[L_CLUSFC];
 }

 if( type == TUBETYPE) {
   tcNumber    =  list[L_TUBENUM];
   tcTotal     =  list[L_TUBETOT];
   tcStep      =  list[L_TUBESTP];
   tcNorth     =  list[L_TUBEN];
   tcWest      =  list[L_TUBEW];
   tcSouth     =  list[L_TUBES];
   tcEast      =  list[L_TUBEE];
   tcNumOfFcs  =  list[L_TUBEFC];
 }

 if( type == PROBABILITY) {
   probScale     = list[L_PROBSCALE];
   probIndicator = list[L_PROBINDIC];
   probLower     = list[L_PROBLOWER];
   probUpper     = list[L_PROBUPPER];
 }

 if( localDef == 13 ) {
   directionNumber = list[L_DIRECTIONNUMBER];
   frequencyNumber = list[L_FREQUENCYNUMBER];
 }

 if( localDef == 21 ) {
   optimisationTime    = list[L_OPTIMISATIONTIME];
   leadTime            = list[L_LEADTIME];
   sensitiveAreaDomain = list[L_SENSITIVEAREADOMAIN];
   sensitiveAreaMethod = list[L_SENSITIVEAREAMETHOD];
 }

 if( localDef == 23 ) {
   verifyingMonth      = list[L_VERIFYINGMONTH];
   averagingPeriod     = list[L_AVERAGINGPERIOD];
   forecastMonth       = list[L_FORECASTMONTH];
   referenceDate       = list[L_REFERENCEDATE];
   climateDateFrom     = list[L_CLIMATEDATEFROM];
   climateDateTo       = list[L_CLIMATEDATETO];
   thresholdUnitsScale = list[L_THRESHOLDUNITSSCALE];
   thresholdIndicator  = list[L_THRESHOLDINDICATOR];
   lowerThreshold      = list[L_LOWERTHRESHOLD];
   upperThreshold      = list[L_UPPERTHRESHOLD];
 }

 if( index >= 0 ) {
  thisFile = pbginitInput(filename,filename_len);
  file = currentFile(openFiles, thisFile);

  for( loop = index; loop < file->count; loop++ ) {
   if( MATCH(param,file->parameter[loop]) )
   if( MATCH(level,file->level[loop]) )
   if( MATCH(step,file->timestep[loop]) )
   if( MATCH(time,file->time[loop]) )
   if( MATCH(date,file->date[loop]) )
   if( MATCH(ensemble,file->number[loop]) )
   if( MATCH(type,file->type[loop]) )
   if( MATCH(stream,file->stream[loop]) )
   if( MATCH(repres,file->repres[loop]) )
   if( MATCH(levtype,file->levtype[loop]) )
   if( MATCH(vdate,file->vdate[loop]) )
   if( MATCH(vtime,file->vtime[loop]) )
   if( MATCH(ni,file->ni[loop]) )
   if( MATCH(nj,file->nj[loop]) )
   if( MATCH(nlat,file->nlat[loop]) )
   if( MATCH(nlon,file->nlon[loop]) )
   if( MATCH(slat,file->slat[loop]) )
   if( MATCH(slon,file->slon[loop]) )
   if( MATCH(di,file->di[loop]) )
   if( MATCH(dj,file->dj[loop]) )
   if( MATCH(splat,file->splat[loop]) )
   if( MATCH(splon,file->splon[loop]) )
   if( MATCH(quasi,file->quasi[loop]) )
   if( MATCH(tcNumber,file->tcNumber[loop]) )
   if( MATCH(tcTotal,file->tcTotal[loop]) )
   if( MATCH(clusterMethod,file->clusterMethod[loop]) )
   if( MATCH(tcStep,file->tcStep[loop]) )
   if( MATCH(clusterStepEnd,file->clusterStepEnd[loop]) )
   if( MATCH(tcNorth,file->tcNorth[loop]) )
   if( MATCH(tcWest,file->tcWest[loop]) )
   if( MATCH(tcSouth,file->tcSouth[loop]) )
   if( MATCH(tcEast,file->tcEast[loop]) )
   if( MATCH(clusterOpFcNumber,file->clusterOpFcNumber[loop]) )
   if( MATCH(clusterControlFcNumber,file->clusterControlFcNumber[loop]) )
   if( MATCH(tcNumOfFcs,file->tcNumOfFcs[loop]) )
   if( MATCH(probScale,file->probScale[loop]) )
   if( MATCH(probIndicator,file->probIndicator[loop]) )
   if( MATCH(probLower,file->probLower[loop]) )
   if( MATCH(probUpper,file->probUpper[loop]) )
   if( MATCH(directionNumber,file->directionNumber[loop]) )
   if( MATCH(frequencyNumber,file->frequencyNumber[loop]) )
   if( MATCH(optimisationTime,file->optimisationTime[loop]) )
   if( MATCH(leadTime,file->leadTime[loop]) )
   if( MATCH(sensitiveAreaDomain,file->sensitiveAreaDomain[loop]) )
   if( MATCH(sensitiveAreaMethod,file->sensitiveAreaMethod[loop]) )
   if( MATCH(verifyingMonth,file->verifyingMonth[loop]) )
   if( MATCH(averagingPeriod,file->averagingPeriod[loop]) )
   if( MATCH(forecastMonth,file->forecastMonth[loop]) )
   if( MATCH(referenceDate,file->referenceDate[loop]) )
   if( MATCH(climateDateFrom,file->climateDateFrom[loop]) )
   if( MATCH(climateDateTo,file->climateDateTo[loop]) )
   if( MATCH(thresholdUnitsScale,file->thresholdUnitsScale[loop]) )
   if( MATCH(thresholdIndicator,file->thresholdIndicator[loop]) )
   if( MATCH(lowerThreshold,file->lowerThreshold[loop]) )
   if( MATCH(upperThreshold,file->upperThreshold[loop]) ) {
     if( DEBUG1 ) {
       _fcd pfile;
       copyName(&pfile, filename, filename_len);
       printf("PBGBFIND: matching GRIB found at position %d in file %s\n",
       (loop+1), pfile);
       free(pfile);
     }
     return (loop+1);
   }
  }
 }

 return (-1);
}

fortint pbgbfind(_fcd filename,fortint* list,fortint* n,fortint filename_len) {
  return pbgbfind_(filename,list,n,filename_len);
}

/*
// ****************************************************************
*/

#define INDEX_TOO_BIG  -1
#define FSEEK_ERROR    -2
#define FREAD_ERROR    -2
#define BUFF_TOO_SMALL -3

fortint pbgget_(_fcd filename, fortint * buffer, fortint * bufflen,
               fortint * n, fortint filename_len) {
/*
// Gets the nth GRIB product.
*/
integer index = (*n)-1;
integer length, status;
OFF_T offset;
integer thisFile;
FILE * fp;
gribfile * file;

  if( DEBUG1 ) {
    _fcd pfile;
    copyName(&pfile, filename, filename_len);
    printf("PBGGET: getting GRIB number %d in file %s\n",
    (index+1), pfile);
    free(pfile);
  }

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {

      length = file->length[index];

      if( DEBUG1 ) printf("PBGGET: length of GRIB number %d = %d\n",
                          (index+1), length);

      if( *bufflen < length ) {
        fprintf(stderr,
          "PBGGET: user buffer too small, %d bytes required\n", length);
        return BUFF_TOO_SMALL;
      }

      offset = file->offset[index];

      if( DEBUG1 ) printf("PBGGET: offset of GRIB number %d = %ld\n",
                          (index+1), offset);

      fp = file->fp;
#ifdef FOPEN64
      if( fseeko64( fp, offset, 0) ) {
#else
      if( fseek( fp, offset, 0) ) {
#endif
        perror("PBGGET: error in fseek");
        return FSEEK_ERROR;
      }

      status = fread(buffer, 1, length, fp);
      if( status != length ) {
        fprintf(stderr,"PBGGET: error in fread\n");
        return FREAD_ERROR;
      }
      
      return length;
    }
  }

  return INDEX_TOO_BIG;

}

fortint pbgget(_fcd filename, fortint * buffer, fortint * bufflen,
               fortint * n, fortint filename_len) {
  return pbgget_(filename,buffer,bufflen,n,filename_len);
}

/*
// ****************************************************************
*/

#define FWRITE_ERROR -1

fortint pbgput_(_fcd filename, fortint * buffer, fortint * bufflen,
               fortint filename_len) {
/*
// Write an array to a file.
*/
integer length;
integer thisFile;
FILE * fp;
gribfile * file;

  if( DEBUG1 ) {
    _fcd pfile;
    copyName(&pfile, filename, filename_len);
    printf("PBGPUT: putting %d bytes to file %s\n", *bufflen, pfile);
    free(pfile);
  }

  thisFile = pbginitOutput(filename,filename_len);
  file = currentFile(openFiles, thisFile);

  fp = file->fp;
  length = fwrite(buffer, 1, *bufflen, fp);

  if( DEBUG1 ) printf("PBGPUT: number of bytes written = %d\n", length);

  if( length!=(*bufflen) )
    return FWRITE_ERROR;
  else
    return (*bufflen);
}

fortint pbgput(_fcd filename, fortint * buffer, fortint * bufflen,
               fortint filename_len) {
  return pbgput_(filename,buffer,bufflen,filename_len);
}

/*
// ****************************************************************
*/
void pbgclos_(_fcd filename, fortint filename_len) {

  openFiles.removeFile(filename, filename_len);
  return;

}

void pbgclos(_fcd filename, fortint filename_len) {
  pbgclos_(filename,filename_len);
}

/*
// ****************************************************************
*/
integer pbginitInput(_fcd filename, fortint filename_len) {
/*
// Initialise indices for current file if necessary.
*/
integer thisFile;

  if( DEBUG2 ) {
    _fcd pfile;
    copyName(&pfile, filename, filename_len);
    printf("PBG_pbginitInput: checking if file %s already open\n", pfile);
    free(pfile);
  }

  thisFile = openFiles.exists(filename,filename_len);

  if( thisFile == -1 ) {
    if( DEBUG2 ) printf("PBG_pbginitInput: file not yet open\n");
    return ( openFiles.addRead(filename, filename_len) );
  }

  if( DEBUG2 ) printf("PBG_pbginitInput: file has open slot %d\n", thisFile);
  return thisFile;

}

integer pbginitOutput(_fcd filename, fortint filename_len) {
/*
// Initialise output file if necessary.
*/
integer thisFile;

  if( DEBUG2 ) {
    _fcd pfile;
    copyName(&pfile, filename, filename_len);
    printf("PBG_pbginitOutput: checking if file %s already open\n", pfile);
    free(pfile);
  }

  thisFile = openFiles.exists(filename,filename_len);

  if( thisFile == -1 ) {
    if( DEBUG2 ) printf("PBG_pbginitOutput: file not yet open\n");
    return ( openFiles.addWrite(filename, filename_len) );
  }

  if( DEBUG2 ) printf("PBG_pbginitOutput: file has open slot %d\n", thisFile);
  return thisFile;

}

integer exists(_fcd filename , fortint filename_len) {
integer n;
_fcd pfile;
gribfile * file;


/*
// See if DEBUG switched on.
*/
    if( ! debugSet ) {
      debugLevel = getenv("PBG_DEBUG");
      if( debugLevel == NULL )
        debugSet = DEBUGOFF;              /* off */
      else {
        int loop;
        for( loop = 0; loop < strlen(debugLevel) ; loop++ ) {
          if( ! isdigit(debugLevel[loop]) ) {
            printf("Invalid number string in PBG_DEBUG: %s\n", debugLevel);
            printf("PBG_DEBUG must comprise only digits [0-9].\n");
            debugSet = DEBUGOFF;
          }
        }
        debugSet = DEBUGOFF + atol( debugLevel );
      }
      if( DEBUG2 ) printf("PBG_exists: PBG_DEBUG switched on\n");
    }

  if( openFiles.count > -1 ) {

    copyName(&pfile, filename, filename_len);

    if( DEBUG2 ) printf("PBG_exists: looking for filename = %s\n", pfile);

    for( n = 0; n <= openFiles.count; n++ ) {
      file = currentFile(openFiles, n);
      if( file != 0 ) {
        if( strcmp( file->fname, pfile ) == 0 ) {
          free(pfile);
          if( DEBUG2 ) printf("PBG_exists: file found in slot= %d\n", n);
          return n;
        }
      }
    }

    free(pfile);
  }
  return -1;
}

integer addFile(_fcd filename , fortint filename_len, char readwriteflag) {
_fcd pfile;
char mode[2] = " ";
FILE * in;
gribfile * previousLatest, * latest;

  openFiles.count++;

  copyName(&pfile, filename, filename_len);

  if( DEBUG2 ) printf("PBG_addFile: adding filename = %s\n", pfile);

  mode[0] = readwriteflag;
#ifdef FOPEN64
  in = fopen64(pfile,mode);
#else
  in = fopen(pfile,mode);
#endif
  if( in == NULL ) {
    perror("Error opening file");
    exit(1);
  }

  if( openFiles.count == 0 ) {
    openFiles.files = (gribfile *)malloc(sizeof(gribfile));
    latest = openFiles.files;
  }
  else {
    previousLatest = latestFile(openFiles);
    previousLatest->next = (gribfile *)malloc(sizeof(gribfile));
    latest = previousLatest->next;
  }

  latest->fp = in;
  copyName(&(latest->fname),filename,filename_len);
  latest->readwriteflag = readwriteflag;
  latest->max = MAX_NUMBER_OF_GRIBS;
  latest->count = 0;

#define MALLOC(a) (a) = (void *) malloc(space)

  if( readwriteflag == 'r' ) {
    integer space;

    space = sizeof(OFF_T)*MAX_NUMBER_OF_GRIBS;
    MALLOC(latest->offset);

    space = sizeof(integer)*MAX_NUMBER_OF_GRIBS;
    MALLOC(latest->length);
    MALLOC(latest->parameter);
    MALLOC(latest->level);
    MALLOC(latest->date);
    MALLOC(latest->time);
    MALLOC(latest->timestep);
    MALLOC(latest->localDefinitionNumber);
    MALLOC(latest->type);
    MALLOC(latest->stream);
    MALLOC(latest->repres);
    MALLOC(latest->levtype);
    MALLOC(latest->number);
    MALLOC(latest->vdate);
    MALLOC(latest->vtime);
    MALLOC(latest->tcNumber);
    MALLOC(latest->tcTotal);
    MALLOC(latest->clusterMethod);
    MALLOC(latest->tcStep);
    MALLOC(latest->clusterStepEnd);
    MALLOC(latest->tcNorth);
    MALLOC(latest->tcWest);
    MALLOC(latest->tcSouth);
    MALLOC(latest->tcEast);
    MALLOC(latest->clusterOpFcNumber);
    MALLOC(latest->clusterControlFcNumber);
    MALLOC(latest->tcNumOfFcs);
    MALLOC(latest->probScale);
    MALLOC(latest->probIndicator);
    MALLOC(latest->probLower);
    MALLOC(latest->probUpper);
    MALLOC(latest->ni);
    MALLOC(latest->nj);
    MALLOC(latest->nlat);
    MALLOC(latest->nlon);
    MALLOC(latest->slat);
    MALLOC(latest->slon);
    MALLOC(latest->di);
    MALLOC(latest->dj);
    MALLOC(latest->splat);
    MALLOC(latest->splon);
    MALLOC(latest->quasi);
    MALLOC(latest->directionNumber);
    MALLOC(latest->frequencyNumber);
    MALLOC(latest->optimisationTime);
    MALLOC(latest->leadTime);
    MALLOC(latest->sensitiveAreaDomain);
    MALLOC(latest->sensitiveAreaMethod);
    MALLOC(latest->verifyingMonth);
    MALLOC(latest->averagingPeriod);
    MALLOC(latest->forecastMonth);
    MALLOC(latest->referenceDate);
    MALLOC(latest->climateDateFrom);
    MALLOC(latest->climateDateTo);
    MALLOC(latest->thresholdUnitsScale);
    MALLOC(latest->thresholdIndicator);
    MALLOC(latest->lowerThreshold);
    MALLOC(latest->upperThreshold);
  }

  latest->next = 0;

  if( DEBUG2 ) printf("PBG_addFile: adding file %s in slot = %d\n",
                       pfile, openFiles.count);

  free(pfile);
  return openFiles.count;

}

void removeFile(_fcd filename , fortint filename_len) {
integer thisFile;
int status;

  if( DEBUG2 ) {
    _fcd pfile;
    copyName(&pfile, filename, filename_len);
    printf("PBG_removeFile: trying to remove filename = %s\n", pfile);
    free(pfile);
  }
 
  thisFile = openFiles.exists(filename,filename_len);

  if( thisFile != -1 ) {
    integer index;
    gribfile * previous = openFiles.files;
    gribfile * current;

    current = previous;

    for( index = 0; index < thisFile; index++ ) {
      previous = current;
      current = current->next;
    }

    status = fclose(current->fp);
    if( status != 0 ) {
      perror("Error closing file");
      exit(1);
    }
    if( DEBUG2 ) printf("PBG_removeFile: removing file %s from slot %d\n",
                         current->fname, thisFile);
    free(current->fname);

/*
//  Input files have arrays of values
*/
    if( current->readwriteflag == 'r' ) {
      free(current->offset);
      free(current->length);
      free(current->parameter);
      free(current->level);
      free(current->date);
      free(current->time);
      free(current->timestep);
      free(current->localDefinitionNumber);
      free(current->type);
      free(current->stream);
      free(current->repres);
      free(current->levtype);
      free(current->number);
      free(current->vdate);
      free(current->vtime);
      free(current->tcNumber);
      free(current->tcTotal);
      free(current->clusterMethod);
      free(current->tcStep);
      free(current->clusterStepEnd);
      free(current->tcNorth);
      free(current->tcWest);
      free(current->tcSouth);
      free(current->tcEast);
      free(current->clusterOpFcNumber);
      free(current->clusterControlFcNumber);
      free(current->tcNumOfFcs);
      free(current->probScale);
      free(current->probIndicator);
      free(current->probLower);
      free(current->probUpper);
      free(current->ni);
      free(current->nj);
      free(current->nlat);
      free(current->nlon);
      free(current->slat);
      free(current->slon);
      free(current->di);
      free(current->dj);
      free(current->splat);
      free(current->splon);
      free(current->quasi);
      free(current->directionNumber);
      free(current->frequencyNumber);
      free(current->optimisationTime);
      free(current->leadTime);
      free(current->sensitiveAreaDomain);
      free(current->sensitiveAreaMethod);
      free(current->verifyingMonth);
      free(current->averagingPeriod);
      free(current->forecastMonth);
      free(current->referenceDate);
      free(current->climateDateFrom);
      free(current->climateDateTo);
      free(current->thresholdUnitsScale);
      free(current->thresholdIndicator);
      free(current->lowerThreshold);
      free(current->upperThreshold);
    }

    if( thisFile == 0 ) 
      openFiles.files = (current->next);
    else
       previous->next = (current->next);
    free(current);
    
    openFiles.count--;
  }
}

integer addRead(_fcd filename , fortint filename_len) {
integer thisFile;

  if( DEBUG2 ) {
    _fcd pfile;
    copyName(&pfile, filename, filename_len);
    printf("PBG_addRead: add for reading filename = %s\n", pfile);
    free(pfile);
  }

  thisFile = addFile(filename,filename_len,'r');

  pbgindx( thisFile);

  return thisFile;
}

integer addWrite(_fcd filename , fortint filename_len) {
integer thisFile;

  if( DEBUG2 ) {
    _fcd pfile;
    copyName(&pfile, filename, filename_len);
    printf("PBG_addWrite: add for writing filename = %s\n", pfile);
    free(pfile);
  }

  thisFile = addFile(filename,filename_len,'w');

  return thisFile;
}

void copyName(_fcd * pfile, _fcd filename, fortint filename_len) {
/*
// Copies FORTRAN filename to C string.
*/
char * blankCharacterPointer;
integer space;

  *pfile = (char *) malloc((size_t) (filename_len+1) );
  memcpy(*pfile, filename, (size_t) filename_len);
  space = (integer) filename_len;
  (*pfile)[space] = '\0';

  blankCharacterPointer = strchr(filename, ' ');
  if( blankCharacterPointer != NULL ) {
    space = (integer) (blankCharacterPointer - filename);
    (*pfile)[space] = '\0';
  }

  return;
}

gribfile * latestFile(collection openFiles) {
integer index;
gribfile * last = openFiles.files;

  for( index = 1; index < openFiles.count; index++ )
    last =  last->next;

  return last;

}

gribfile * currentFile(collection openFiles, integer thisFile) {
integer index;
gribfile * current = openFiles.files;

  for( index = 0; index < thisFile; index++ )
    current = current->next;

  return current;
}

#ifdef gribAPI
long Julian_to_date(long jdate) {
#else
long julian_to_date(long jdate) {
#endif
long x,y,d,m,e;
long day,month,year;

    x = 4 * jdate - 6884477;
    y = (x / 146097) * 100;
    e = x % 146097;
    d = e / 4;

    x = 4 * d + 3;
    y = (x / 1461) + y;
    e = x % 1461;
    d = e / 4 + 1;

    x = 5 * d - 3;
    m = x / 153 + 1;
    e = x % 153;
    d = e / 5 + 1;

    if( m < 11 )
    	month = m + 2;
    else
    	month = m - 10;

    day = d;
    year = y + m / 11;

    return year * 10000 + month * 100 + day;
}

#ifdef gribAPI
long date_to_Julian(long ddate) {
#else
long date_to_julian(long ddate) {
#endif
long  m1,y1,a,b,c,d,j1;
long month,day,year;

    year = ddate / 10000;
    ddate %= 10000;
    month  = ddate / 100;
    ddate %= 100;
    day = ddate;


    if (year < 100) year = year + 1900;

    if (month > 2)
    {
    	m1 = month - 3;
    	y1 = year;
    }
    else
    {
    	m1 = month + 9;
    	y1 = year - 1;
    }

    a = 146097*(y1/100)/4;
    d = y1 % 100;
    b = 1461*d/4;
    c = (153*m1+2)/5+day+1721119;
    j1 = a+b+c;

    return(j1);
}

fortint pbggeth012_(_fcd,fortint*,fortint,fortint,fortint);
fortint soffset012_(unsigned char*,fortint*,fortint*,fortint*);

#define XFIND_ERROR -1

fortint pbgxfind_(fortint * grib1, _fcd filename, fortint filename_len)
{
unsigned char* buffer1 = (unsigned char*) grib1;
static fortint* ibuffer2 = NULL;
static fortint buffer2Length = 0;
fortint status, totalLength, headerLength, numberOfGribs, loop;
fortint is0, is1, is2;
fortint js0, js1, js2;

  numberOfGribs = pbgtotl_(filename,filename_len);
  if( numberOfGribs < 1 ) {
    if( DEBUG1 ) {
      _fcd pfile;
      copyName(&pfile, filename, filename_len);
      printf("PBXFIND: No GRIBs in file %s\n", pfile);
      free(pfile);
    }
    return XFIND_ERROR;
  }

  status = soffset012_(buffer1, &is0, &is1, &is2);
  if( status ) return XFIND_ERROR;

  totalLength = THREEBYTELONG(buffer1+is0+4);
  headerLength = (is1 - is0);
  headerLength += THREEBYTELONG(buffer1+is1);
  if( is2 != 0 ) headerLength += THREEBYTELONG(buffer1+is2);

  for( loop = 0; loop < numberOfGribs; loop++) {
    fortint next, totalLength2, headerLength2;
    unsigned char * buffer2;

    next = loop + 1;

    if( ibuffer2 != NULL ) {
      if( buffer2Length < headerLength ) {
        ibuffer2 = (fortint*) realloc(ibuffer2,headerLength);
        if( ibuffer2 == NULL ) {
          printf("pbgxfind_: realloc failed\n");
          return XFIND_ERROR;
        }
        buffer2Length = headerLength;
      }
    }
    else {
      ibuffer2 = (fortint*) malloc(headerLength);
      if( ibuffer2 == NULL ) {
        printf("pbgxfind_: malloc failed\n");
        return XFIND_ERROR;
      }
      buffer2Length = headerLength;
    }

    totalLength2 =
      pbggeth012_(filename,ibuffer2,buffer2Length,next,filename_len);

    if( totalLength2 > 1 ) {

      buffer2 = (unsigned char*) ibuffer2;
      status = soffset012_(buffer2, &js0, &js1, &js2);
      if( status ) return XFIND_ERROR;

      if( totalLength == totalLength2 ) {
        headerLength2 = (js1 - js0);
        headerLength2 += THREEBYTELONG(buffer2+js1);
        if( js2 != 0 ) headerLength2 += THREEBYTELONG(buffer2+js2);

        if( headerLength == headerLength2 ) {
          if( (memcmp((buffer1+is0),(buffer2+js0),(size_t)headerLength) == 0) )
            return next;
        }
      }
    }

  }

  return XFIND_ERROR;
}

fortint pbgxfind(fortint * grib1, _fcd filename, fortint filename_len) {
  return pbgxfind_(grib1,filename,filename_len);
}

#define GRIB_TOO_SMALL -4

fortint pbggeth012_(_fcd filename, fortint* buffer, fortint bufflen,
                    fortint n, fortint filename_len) {
/*
// Gets section 0,1 and 2 for the nth GRIB product.
*/
integer index = n - 1;
integer length, status;
OFF_T offset;
integer thisFile;
FILE * fp;
gribfile * file;

  if( DEBUG1 ) {
    _fcd pfile;
    copyName(&pfile, filename, filename_len);
    printf("pbggeth012: getting GRIB number %d in file %s\n",
    (index+1), pfile);
    free(pfile);
  }

  if( index >= 0 ) {
    thisFile = pbginitInput(filename,filename_len);
    file = currentFile(openFiles, thisFile);
    if( index < file->count ) {

      length = file->length[index];

      if( DEBUG1 ) printf("pbggeth012: length of GRIB number %d = %d\n",
                          (index+1), length);

      if( length < bufflen ) return GRIB_TOO_SMALL;

      offset = file->offset[index];

      if( DEBUG1 ) printf("pbggeth012: offset of GRIB number %d = %ld\n",
                          (index+1), offset);

      fp = file->fp;
#ifdef FOPEN64
      if( fseeko64( fp, offset, 0) ) {
#else
      if( fseek( fp, offset, 0) ) {
#endif
        perror("pbggeth012: error in fseek");
        return FSEEK_ERROR;
      }

      status = fread(buffer, 1, bufflen, fp);
      if( status != bufflen ) {
        fprintf(stderr,"pbggeth012: error in fread\n");
        return FREAD_ERROR;
      }
      
      return length;
    }
  }

  return INDEX_TOO_BIG;

}

#define ERROR(a,b) {perror(a);return b;}
#define GRIB 0x47524942
#define len3oct(p) ((((long)*(p))<<16) + (((long)*(p+1))<<8) + (long)*(p+2))
#define BIT1 0x80
#define BIT2 0x40
#define BIT3 0x20
#define BIT4 0x10
#define BIT5 0x08
#define BIT6 0x04
#define BIT7 0x02
#define BIT8 0x01

static int grab(unsigned char * , unsigned char * , long ,long ,long * );

fortint soffset012_(
  unsigned char * buffer,
  fortint* is0,
  fortint* is1,
  fortint* is2 ) {
long s0, s1, s2, edition;
int large = 0;
int found = 0;
int code = 0;
long bytes_read = 0, advance;
unsigned char p, edit_num, flag23;
unsigned char size[3];
int section0 = 8, section1, section2;
long total;

/*
// Read bytes until "GRIB" found
*/
    do {
      if( grab(buffer, &p, 1, 1, &bytes_read) != 0) return 1;
      code = ( (code << 8) + p ) & 0xFFFFFFFF;
      if (code == GRIB ) found = 1;
    } while ( ! found );
    s0 = bytes_read - 4;
    bytes_read = 4;
/*
// Now find out which edition of GRIB is present (default is 1)
*/
    edition = 1;
    s1 = s0 + 8;
    if( (*(buffer+21-s0) == '\0') && (*(buffer+22-s0) == '\0') ) {
      edition = -1;                            /* GRIB edition -1 */
      s1 = s0;
      section1 = 20;
      section0 = 4;
    }
    else {
      if( grab(buffer, size, 3, 1, &bytes_read) != 0) return 1;
      total = len3oct(size);
      if( total == 24 ) {
/*
// Move past the edition number
*/
        if( grab(buffer, &edit_num, 1, 1, &bytes_read) != 0) return 1;
        edition = 0;                         /* GRIB edition 0 */
        section1 = 24;
        s1 = s0 + 4;
        section0 = 4;
      }
    }

    if( edition == 1 ) {
/*
// See if it is an extra large (wave) product
*/
      if( total > 0x800000 ) {
        total = (total&0x7fffff) * 120;
        large = 1;
      }
/*
// Move past the edition number
*/
      if( grab(buffer, &edit_num, 1, 1, &bytes_read) != 0) return 1;
/*
// Read length of section 1
*/
      if( grab(buffer, size, 3, 1, &bytes_read) != 0) return 1;
      section1 = len3oct(size);
    }
/*
// Now figure out if section 2 is present
*/
    advance = 4;
    bytes_read += advance;
    if( grab(buffer, &flag23, 1, 1, &bytes_read) != 0) return 1;
    section2 = flag23 & BIT1;

/*
// Advance to end of section 1
*/
    advance = section1 - (bytes_read - section0);
    bytes_read += advance;
/*
// Read section 2 length if it is given
*/
    if( section2 ) {
      s2 = s0 + bytes_read;
      if( grab(buffer, size, 3, 1, &bytes_read) != 0) return 1;
      section2 = len3oct(size);
      advance = section2 - (bytes_read - section0 - section1);
      bytes_read += advance;
    }
    else {
      section2 = 0;
      s2 = 0;
    }
/*
// Success!
*/
    *is0 = (fortint) s0;
    *is1 = (fortint) s1;
    *is2 = (fortint) s2;
    return 0;
}

static int grab(unsigned char * buffer, unsigned char * where, long size,long cnt,long * num_bytes_read)
{
long number = size*cnt;

    memcpy(where, (buffer+(*num_bytes_read)), number);
    *num_bytes_read += number;

    return 0;
}
