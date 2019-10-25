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

      SUBROUTINE BUFREX(KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     2                  KELEM,CNAMES,CUNITS,KVALS,VALUES,CVALS,KERR)
C
C**** *BUFREX*
C
C
C     PURPOSE.
C     --------
C          DECODE BUFR MESSAGE INTO FULLY EXPANDED FORM; RETURNING
C     INFORMATION RELEVANT FOR ALL BUFR SECTIONS, EXPANDED VALUES,
C     THEIR NAMES AND UNITS.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUFREX(KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
C     1                   KELEM,CNAMES,CUNITS,KVALS,VALUES,CVALS,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  INTEGER ARRAY CONTAINING BUFR MESSAGE
C               *KELEM*   -  INTEGER (EXPECTED NUMBER OF EXPANDED ELEMENTS)
C               *KVALS*   -  INTEGER (EXPECTED NUMBER OF DATA VALUES)
C        OUTPUT:
C               *KSUP*    -  INTEGER ARRAY OF 9 WORDS CONTAINING
C                            SUPLEMENTARY INFORMATION
C                         -  KSUP( 1) -- IDIM1, DIMENSION OF KSEC1
C                         -  KSUP( 2) -- IDIM2, DIMENSION OF KSEC2
C                         -  KSUP( 3) -- IDIM3, DIMENSION OF KSEC3
C                         -  KSUP( 4) -- IDIM4, DIMENSION OF KSEC4
C                         -  KSUP( 5) -- M (NUMBER OF ELEMENTS IN VALUES
C                                           ARRAY, FIRST INDEX)
C                         -  KSUP( 6) -- N (NUMBER OF SUBSETS,SECOND INDEX
C                                           OF VALUES ARRAY)
C                         -  KSUP( 7) -- JVC (NUMBER OF ELEMENTS IN CVAL ARRAY)
C                         -  KSUP( 8) -- TOTAL BUFR MESSAGE LENGTH IN BYTES
C                         -  KSUP( 9) -- IDIM0, DIMENSION OF KSEC0
C
C               *KSEC0*   -  INTEGER ARRAY OF 3 WORDS CONTAINING
C                            BUFR SECTION 0 INFORMATION
C                            KSEC0( 1)-- LENGTH OF SECTION 0 (BYTES)
C                            KSEC0( 2)-- TOTAL LENGTH OF BUFR MESSAGE (BYTES)
C                            KSEC0( 3)-- BUFR EDITION NUMBER
C
C               *KSEC1*   -  INTEGER ARRAY OF AT LEAST 40 WORDS
C                            CONTAINING BUFR SECTION 1 INFORMATION
C                            KSEC1( 1)-- LENGTH OF SECTION 1 (BYTES)
C                            KSEC1( 2)-- BUFR EDITION NUMBER
C                            KSEC1( 3)-- ORIGINATING CENTRE
C                            KSEC1( 4)-- UPDATE SEQUENCE NUMBER
C                            KSEC1( 5)-- FLAG (PRESENCE OF SECTION 2)
C                            KSEC1( 6)-- BUFR MESSAGE TYPE
C                            KSEC1( 7)-- BUFR MESSAGE SUBTYPE
C                            KSEC1( 8)-- VERSION NUMBER OF LOCAL TABLE USED
C                            KSEC1( 9)-- YEAR
C                            KSEC1(10)-- MONTH
C                            KSEC1(11)-- DAY
C                            KSEC1(12)-- HOUR
C                            KSEC1(13)-- MINUTE
C                            KSEC1(14)-- BUFR MASTER TABLE
C                            KSEC1(15)-- VERSION NUMBER OF MASTER TABLE USED
C                            KSEC1(16) - KSEC1(40) -- LOCAL ADP CENTRE
C                                        INFORMATION(BYTE BY BYTE)
C
C
C                            FOR BUFR EDITION 3 ONWARD
C
C                            KSEC1(16)-- ORIGINATING SUB-CENTRE
C                            KSEC1(17)-- NOT USED
C                            KSEC1(18) TO KSEC1(JSEC1) - LOCAL ADP CENTRE
C                                        INFORMATION(PACKED FORM)
C
C                            FOR BUFR EDITION 4 ONWARD
C
C                            KSEC1(16)-- ORIGINATING SUB-CENTRE
C                            KSEC1(17)-- INTERNATIONAL SUB-CATEGORY (SEE COMMON TABLE C-13
C                            KSEC1(18)-- SECOND
C                            KSEC1(19)-- YEAR
C                            KSEC1(20)-- MONTH
C                            KSEC1(21)-- DAY
C                            KSEC1(22)-- HOUR
C                            KSEC1(23)-- MINUTE
C                            KSEC1(24)-- SECOND
C                            KSEC1(25)-- YEAR
C                            KSEC1(26)-- MONTH
C                            KSEC1(27)-- DAY
C                            KSEC1(28)-- HOUR
C                            KSEC1(29)-- MINUTE
C                            KSEC1(30)-- SECOND
C                            KSEC1(31)-- MOST SOUTHERN LATITUDE  (-90 to 90)
C                            KSEC1(32)-- MOST WESTERN LONGITUDE  ( 0-360)
C                            KSEC1(33)-- MOST NORTHEN LATITUDE   (-90 to 90)
C                            KSEC1(34)-- MOST EASTERN LONGITUDE  ( 0-360)
C                            KSEC1(35) TO KSEC1(JSEC1) - LOCAL ADP CENTRE INFORMATION(PACKED FORM)
C
C               *KSEC2*   -  INTEGER ARRAY OF AT LEAST 64 WORDS
C                            CONTAINING BUFR SECTION 2 INFORMATION
C                            KSEC2( 1)-- LENGTH OF SECTION 2 (BYTES)
C                            KSEC2( 2) TO KSEC2(47) RDB KEY
C
C               *KSEC3*   -  INTEGER ARRAY OF 4 WORDS CONTAINING
C                            BUFR SECTION 3 INFORMATION
C                            KSEC3( 1)-- LENGTH OF SECTION 3 (BYTES)
C                            KSEC3( 2)-- RESERVED
C                            KSEC3( 3)-- NUMBER OF SUBSETS
C                            KSEC3( 4)-- FLAG (DATA TYPE,DATA COMPRESSION)
C
C               *KSEC4*   -  INTEGER ARRAY OF 2 WORDS CONTAINING
C                            BUFR SECTION 4 INFORMATION
C                            KSEC4( 1)-- LENGTH OF SECTION 4 (BYTES)
C                            KSEC4( 2)-- RESERVED
C
C               *CNAMES*  -  CHARACTER*64  ARRAY OF KELEM CONTAINING
C                            BUFR TABLE B ELEMENT NAMES
C               *CUNITS*  -  CHARACTER*24 ARRAY OF KELEM CONTAINIG
C                            BUFR TABLE B UNITS
C               *VALUES*  -  REAL ARRAY OF KVALS CONTAINING EXPANDED
C                            DATA VALUES
C               *CVALS*   -  CHARACTER*80 ARRAY OF KVALS CONTAINING
C                            BUFR CODE TABLE OR CCITTIA5 BUFR ELEMENTS
C                            ENTRIES
C               *KERR*    -  RETURNED ERROR CODE
C
C
C     METHOD.
C     -------
C
C          BUFR MESSAGE PASSED AS ARGUMENT TO THIS ROUTINE IS DECODED
C     SECTION BY SECTION. SUPLEMENTARY INFORMATION AND EXPANDED DATA
C     ARE RETURNED AS WELL AS ERROR CODE. DURING BIT PATTERN UNPACKING
C     GBYTE AND GBYTES ROUTINES (VMS VERSION) ARE USED.
C
C
C     EXTERNALS.
C     ----------
C
C          BUEXS0   -  EXPANDS SECTION 0 OF BUFR MESSAGE
C          BUEXS1   -  EXPANDS SECTION 1 OF BUFR MESSAGE
C          BUEXS2   -  EXPANDS SECTION 2 OF BUFR MESSAGE
C          BUEXS3   -  EXPANDS SECTION 3 OF BUFR MESSAGE
C          BUGBTS   -  LOAD REQUIRED BUFR TABLES
C          BUEXS4   -  EXPANDS SECTION 4 OF BUFR MESSAGE
C          BUEXS5   -  EXPANDS SECTION 5 OF BUFR MESSAGE
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
#     include "parameter.F"
#     include "bcomunit.F"
#     include "bcomwork.F"
#     include "bcomwt.F"
#     include "bcomdefc.F"
#     include "bcomreq.F"
#     include "bcomoff.F"
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSUP(JSUP),KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
C
#ifndef R_4
      REAL*8 VALUES(KVALS)
      REAL*8 RVIND
      REAL*8 EPS
      REAL*8 RQVAL
      REAL*8 VAL
#else
      REAL   VALUES(KVALS)
      REAL   RVIND
      REAL   EPS
      REAL   RQVAL
      REAL   VAL
#endif
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*4  CECMWF,CUSER
C
      SAVE NWPTB1,NBPTB1
C     ------------------------------------------------------------------
C
C*          1.   SET CONSTANTS.
C                --------------
 100  CONTINUE
C
      KERR=0
C
      M0=1
C
      DO 101 I=1,JSEC0
      KSEC0(I)=0
 101  CONTINUE
C
      DO 102 I=1,JSEC1
      KSEC1(I)=0
 102  CONTINUE
C
      DO 103 I=1,JSEC3
      KSEC3(I)=0
 103  CONTINUE
C
      DO 104 I=1,JSUP
      KSUP(I)=0
 104  CONTINUE
C
      DO 105 I=1,JSEC4
      KSEC4(I)=0
 105  CONTINUE
C
      DO 106 I=1,JSEC2
      KSEC2(I)=0
 106  CONTINUE
C
      IF(CECMWF.NE.'ECMF') THEN
         CALL BUEVAR(KERR)
         CECMWF='ECMF'
      END IF
C
      IF(CUSER.NE.'USER') THEN
         NREQ(1)=0
         NREQ(2)=0
         NRQL=0
      END IF
C     -----------------------------------------------------------------
C*          2.  EXPAND SECTION 0.
C               -----------------
 200  CONTINUE
C
      CALL BUEXS0( KBUFL,KBUFF,KSUP,KSEC0,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C*          3.   EXPAND SECTION 1.
C                ------------------
 300  CONTINUE
C
      CALL BUEXS1( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C*          4.  EXPAND SECTION 2.
C               -----------------
 400  CONTINUE
C
      CALL BUEXS2( KBUFL,KBUFF,KSUP,KSEC1,KSEC2,KERR )
      IF(KERR.GT.0) RETURN
C
      CALL BUEXS33(KBUFL,KBUFF,KSEC3,KERR)
      IF(KERR.GT.0) RETURN
      
      NWPTB1=NWPTB
      NBPTB1=NBPTB
C     ------------------------------------------------------------------
C*          5.   LOAD BUFR TABLES.
C                -----------------
 500  CONTINUE
C
      CALL BUGBTS(KSEC1,KERR)
      IF(KERR.GT.0) RETURN
C
      OMULTI=.FALSE.
      NSUBSET=1
      IF(IAND(KSEC3(4),64).EQ.0.AND.KSEC3(3).GT.1) THEN
C
         OMULTI=.TRUE.
         DO I=1,KSEC3(3)
         NWT=0
         M=0
         NWPTB=NWPTB1
         NBPTB=NBPTB1
         NSUBSET=I
         CALL BUEXS3(KBUFL,KBUFF,KSUP,KSEC3,KELEM,CNAMES,CUNITS,
     1               KERR)
         IF(KERR.GT.0) RETURN
C
         CALL BUEXS4(KBUFL ,KBUFF ,KSUP  ,KSEC3,KSEC4,
     1               KELEM ,CNAMES,CUNITS,KVALS,VALUES,CVALS,KERR)
         IF(KERR.GT.0) RETURN
         END DO

         GO TO 800
      END IF
    
C     ------------------------------------------------------------------
C*          6.  EXPAND SECTION 3.
C               -----------------
 600  CONTINUE
C
      CALL BUEXS3(KBUFL,KBUFF,KSUP,KSEC3,KELEM,CNAMES,CUNITS,KERR)
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C*          7.   EXPAND SECTION 4.
C                -----------------
 700  CONTINUE
C
      CALL BUEXS4(KBUFL ,KBUFF ,KSUP  ,KSEC3,KSEC4,
     1            KELEM ,CNAMES,CUNITS,KVALS,VALUES,CVALS,KERR)
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C*          8.   EXPAND SECTION 5.
C                -----------------
 800  CONTINUE
C
      CALL BUEXS5(KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C     -----------------------------------------------------------------
C*          9.   SET TOTAL BUFR MESSAGE LENGTH.
C                ------------------------------
 900  CONTINUE
C
      KSUP(8)=KSEC0(1)+KSEC1(1)+KSEC2(1)+KSEC3(1)+KSEC4(1)+4
C
C     -----------------------------------------------------------------
      RETURN
C
      END
