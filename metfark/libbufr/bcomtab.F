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
      COMMON /BCOMTAB/ NTABBTR(JTAB),NTABBS (JTAB),NTABBRV(JTAB),
     1                NTABBDW(JTAB),NTABDTR(JTAB),NTABDST(JTAB),
     2                NTABDL (JTAB),NTABDSQ(JTAB*20),NTABP(64,256)
C     
C             NTABBTR    - TABLE B,  TABLE REFERENCE              ARRAY
C             NTABBS     - TABLE B,  SCALE                        ARRAY
C             NTABBRF    - TABLE B,  REFERENCE VALUE              ARRAY
C             NTABBDW    - TABLE B,  DATA WIDTH                   ARRAY
C             NTABDTR    - TABLE D,  TABLE REFERENCE              ARRAY
C             NTABDST    - TABLE D,  STARTING POINTERS            ARRAY
C             NTABDL     - TABLE D,  LENGTHS                      ARRAY
C             NTABDSQ    - TABLE D,  LIST OF SEQUENCE DESCRIPTORS ARRAY
C
C

