C Copyright 1981-2012 ECMWF. 
C
C This software is licensed under the terms of the GNU Lesser 
C General Public License Version 3 which can be obtained at 
C http://www.gnu.org/licenses/lgpl.html.  
C 
C In applying this licence, ECMWF does not waive the privileges 
C and immunities granted to it by virtue of its status as an 
C intergovernmental organisation nor does it submit to any
C jurisdiction. 
C 

C
      COMMON /BCMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,N40,N07,N08,
     1               NDWINC07,NSCAM07,NAFDWA(100),NFD,NREP,OREP,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  DATA WIDTH INCREMENT
C             NSCAM    -  SCALE MULTIPLIER
C             NAFDW    -  AUGMENTED FIELD DATA WIDTH
C             NWT      -  POINTER TO WORKING TABLE
C             NSTACK   -  LIST OF DATA DESCRIPTORS
C             ODREPF   -  REPLICATION (LOGICAL)
C             N221     -  DATA NOT PRESENT FOR N221 ELEMENTS
C             MREL     -  POINTER TO THE LAST DATA ELEMENT
C             NFCM     -  FIRST COMPRESSED MESSAGE
C             MBMP     -  POINTER TO THE BEGINING OF BIT MAP
C             NWTR     -  WORKING TABLE REFERENCE
C             NWTS     -  WORKING SCALE
C             NWTRV    -  WORKING REFERENCE VALUE
C             NWTDW    -  WORKING DATA WIDTH
C             NWTIW    -  WORKING DATA WIDTH OF INCREMENTS
C             NWTIWS   -  WORKING TOTAL DATA WIDTH OF ELEMENT SET
C

