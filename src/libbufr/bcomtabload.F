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
      COMMON /BCOMTABLOAD/ MTABBTR(JTAB,JTMAX),MTABBS (JTAB,JTMAX),
     1                MTABBRV(JTAB,JTMAX),
     1                MTABBDW(JTAB,JTMAX),MTABDTR(JTAB,JTMAX),
     2                MTABDST(JTAB,JTMAX),
     2                MTABDL (JTAB,JTMAX),MTABDSQ(JTAB*20,JTMAX),
     2                MTABP(JTCLAS,JTEL,JTMAX), NTC,NTT,
     3                MNREF(JCTAB,JTMAX),MNSTART(JCTAB,JTMAX),
     3                MNLEN(JCTAB,JTMAX),MNCODNUM(JCTST,JTMAX),
     3                MNSTARTC(JCTST,JTMAX),MNLENC(JCTST,JTMAX)
C     
C             MTABBTR    - TABLE B,  TABLE REFERENCE              ARRAY
C             MTABBS     - TABLE B,  SCALE                        ARRAY
C             MTABBRF    - TABLE B,  REFERENCE VALUE              ARRAY
C             MTABBDW    - TABLE B,  DATA WIDTH                   ARRAY
C             MTABDTR    - TABLE D,  TABLE REFERENCE              ARRAY
C             MTABDST    - TABLE D,  STARTING POINTERS            ARRAY
C             MTABDL     - TABLE D,  LENGTHS                      ARRAY
C             MTABDSQ    - TABLE D,  LIST OF SEQUENCE DESCRIPTORS ARRAY
C             NTT        - NUMBER OF TABLES
C             NTC        - CURRENT TABLE
C             MNREF      - TABLE C   REFERENCE NUMBER             ARRAY
C             MNSTART    - START OF CODES                         ARRAY
C             MNLEN      - NUMBER OF CODES                        ARRAY
C             MNSTARTC   - START OF TEXT                          ARRAY
C             MNLENC     - NUMBER OF CODE STRINGS                 ARRAY
C
C
