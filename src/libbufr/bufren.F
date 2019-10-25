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

      SUBROUTINE BUFREN(KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     1                  KTDLEN,KTDLST,KDLEN,KDATA,KELEM,KVALS,
     2                  VALUES,CVALS,KBUFL,KBUFF,KERR)
C
C**** *BUFREN*
C
C
C     PURPOSE.
C     --------
C          ENCODE BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C               *CALL BUFREN(KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
C                            KTDLEN,KTDLST,KDLEN,KDATA,KELEM,KVALS,
C                            VALUES,CVALS,KBUFL,KBUFF,KERR)
C
C        INPUT :
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
C                            KSEC1(16) TO KSEC1(40) - LOCAL ADP CENTRE
C                                        INFORMATION(PACKED FORM)
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
C                            KSEC2( 2) TO KSEC2(64) LOCAL ADP CENTRE
C                                        INFORMATION(PACKED FORM)
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
C               *KTDLEN*  -  INTEGER NUMBER OF DATA DESCRIPTORS IN SECTION 3
C               *KTDLST*  -  INTEGER ARRAY OF AT LEAST KTDLEN WORDS
C                            CONTAINING DATA DESCRIPTORS FOR BUFR SECTION 3
C               *KDLEN*   -  INTEGER (DIMENSION OF KDATA ARRAY)
C               *KDATA*   -  INTEGER ARRAY CONTAINING DATA NEEDED FOR DATA
C                            DESCRIPTOR EXPANSION (DELAYED REPLICATION FACTORS)
C                            WHICH APPEAR IN THE VALUES ARRAY
C
C               *KELEM*   -  INTEGER NUMBER OF ELEMENTS IN BUFR TEMPLATE.
C               *KVALS*   -  INTEGER (DIMENSION OF VALUES ARRAY)
C               *VALUES*  -  REAL ARRAY OF KVALS WORDS (EXPANDED DATA )
C               *CVALS*   -  CHARACTER*80  ARRAY OF KVALS
C
C        OUTPUT:
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  INTEGER ARRAY CONTAINING BUFR MESSAGE
C               *KERR*    -  RETURN ERROR CODE
C
C
C     METHOD.
C     -------
C
C          BUFR MESSAGE SECTIONS CONTENET, AND DATA ARE PASSED FOR
C     FOR PACKING INTO FM-94 BUFR DATA.DURING UNPACKING A BIT PATHERN
C     GBYTE AND GBYTES ROUTINES (VMS VERSION) ARE USED.
C
C
C     EXTERNALS.
C     ----------
C
C          BUENS0   -  PACK SECTION 0 OF BUFR MESSAGE
C          BUENS1   -  PACK SECTION 1 OF BUFR MESSAGE
C          BUENS2   -  PACK SECTION 2 OF BUFR MESSAGE
C          BUENS3   -  PACK SECTION 3 OF BUFR MESSAGE
C          BUETAB    -  LOAD REQUIRED BUFR TABLES
C          BUENS4   -  PACK SECTION 4 OF BUFR MESSAGE
C          BUENS5   -  PACK SECTION 5 OF BUFR MESSAGE
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
#     include "bcmwork.F"
#     include "bcmdefc.F"
#     include "bcmoff.F"
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSEC0(JSEC0),KSEC1(JSEC1),KSEC2(JSEC2)
     1,         KSEC3(JSEC3),KSEC4(JSEC4)
C
#ifndef R_4
      REAL*8 VALUES(KVALS)
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   VALUES(KVALS)
      REAL   RVIND
      REAL   EPS
#endif
      DIMENSION  KTDLST(KTDLEN)
      DIMENSION  KDATA(KDLEN)
C
      CHARACTER*4   CECMWF,CUSER
      CHARACTER*80  CVALS(KVALS)
C
      SAVE NWPTB1,NBPTB1
C
C     ------------------------------------------------------------------
C
C*          1.   SET CONSTANTS.
C                --------------
 100  CONTINUE
C
      KERR=0
      IF(CECMWF.NE.'ECMF') THEN
         CALL BUIVAR(KERR)
         KPT   = 0
         CECMWF='ECMF'
      END IF
C
      NEDN=KSEC0(3)
C
C     -----------------------------------------------------------------
C*          2.  PACK SECTION 0.
C               ---------------
 200  CONTINUE
C
      CALL BUENS0( KSEC0,KBUFL,KBUFF,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C
C*          3.   PACK SECTION 1.
C                ---------------
 300  CONTINUE
C
      CALL BUENS1( KSEC0,KSEC1,KBUFL,KBUFF,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C
C*          4.  PACK SECTION 2.
C               ---------------
 400  CONTINUE
C
      CALL BUENS2( KSEC1,KSEC2,KBUFL,KBUFF,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C
C*          5.   LOAD BUFR TABLES.
C                -----------------
 500  CONTINUE
C
      CALL BUETAB(KSEC1,KERR)
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C          6.  PACK SECTION 3.
C              ---------------
 600  CONTINUE
C
      CALL BUENS3( KSEC3,KTDLEN,KTDLST,KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C     -----------------------------------------------------------------
C
      OMULTI=.FALSE.
      NSUBSET=1
      IF(IAND(KSEC3(4),64).EQ.0.AND.KSEC3(3).GT.1) THEN
C
C        Multi-subset uncompressed data
C
         NSUBSET=0
         OMULTI=.TRUE.
         DO I=1,KSEC3(3)
         NWT=0
         M=0
         NSUBSET=I
         KERR=0
         CALL BUETD(KPT,KTDLEN,KTDLST,KDLEN,KDATA,KSEC3,
     1              KVALS,VALUES,KELEM,KERR)
         IF(KERR.GT.0) RETURN
C
          CALL BUENS4(KSEC3,KSEC4,KELEM,KVALS,VALUES,CVALS,
     1                KBUFL,KBUFF,KERR)
         IF(KERR.GT.0) RETURN
         END DO

         GO TO 800
      END IF
C     ------------------------------------------------------------------
C          6.1  EXPAND DATA DESCRIPTORS.
C               ------------------------
 610  CONTINUE
      CALL BUETD(KPT,KTDLEN,KTDLST,KDLEN,KDATA,KSEC3,
     1           KVALS,VALUES,KELEM,KERR)
      IF(KERR.GT.0) RETURN
C
C
C     ------------------------------------------------------------------
C
C*          7.   PACK SECTION 4.
C                ---------------
 700  CONTINUE
C
      CALL BUENS4(KSEC3,KSEC4,KELEM,KVALS,VALUES,CVALS,
     1            KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C
C     ------------------------------------------------------------------
C
C*          8.   PACK SECTION 5.
C                ---------------
 800  CONTINUE
C
      CALL BUENS5(KSEC0,KSEC1,KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C     -----------------------------------------------------------------
C
C*          9.   SET TOTAL BUFR MESSAGE LENGTH.
C                ------------------------------
 900  CONTINUE
C
C
C     -----------------------------------------------------------------
      RETURN
C
      END
