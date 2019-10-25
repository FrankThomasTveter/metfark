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

      COMMON /BCMWORK/ RVIND,EPS,NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP
     2,                NZZP,NSSP,NEDN
C
C             NBPW          -  NUMBER OF BITS PER COMPUTER WORD
C             NWPT          -  WORD POINTER
C             NBPT          -  BIT POINTER
C             NWPTB         -  POINTER TO WORD AT THE BEGINING OF NEXT SECTION
C             NBPTB         -  POINTER TO BIT AT THE BEGINING OF NEXT SECTION
C             NMASK         -  BIT MASK ARRAY
C             NVIND         -  MISSING VALUE INDICATOR (INTEGER)
C             RVIND         -  MISSING VALUE INDICATOR (REAL)
C             NBENP         -  PREVIOUS BUFR EDITION NUMBER
C             NLTVNP        -  PREVIOUS LOCAL TABLE VERSION NUMBER
C             NXXP          -  BUFR MASTER TABLE USED
C             NYYP          -  VERSION NUMBER OF MASTER TABLE USED
C             NZZP          -  VERSION NUMBER OF LOCAL TABLE USED
C             NEDN          -  BUFR EDITION

