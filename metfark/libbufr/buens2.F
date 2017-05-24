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

      SUBROUTINE BUENS2( KSEC1,KSEC2,KBUFL,KBUFF,KERR )
C
C**** *BUENS2*
C
C
C     PURPOSE.
C     --------
C          PACK SECTION 2 OF BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUENS2(KSEC1,KSEC2,KBUFL,KBUFF,KERR)*
C
C
C        INPUT :
C               *KSEC1*   -  ARRAY CONTAINING SECTION 1 INFORMATION
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
C                            KSEC1(16) TO KSEC1(JSEC1) - LOCAL ADP CENTRE
C                                        INFORMATION(PACKED FORM)
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
C               *KSEC2*   -  ARRAY CONTAINING SECTION 2 INFORMATION
C                            KSEC2( 1)-- LENGTH OF SECTION 2 (BYTES)
C                            KSEC2( 2) TO KSEC2(JSEC2) LOCAL ADP CENTRE
C                                         INFORMATION (PACKED FORM)
C        OUTPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C     -------
C
C
C
C     EXTERNALS.
C     ----------
C
C          BUPCK          - PACK BIT PATHERN
C          BUOCTN         - SET LENGTH OF SECTION
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       17/01/91.
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
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSEC1(JSEC1),KSEC2(JSEC2)
      DIMENSION IDUM(8),     ISEC2(JSEC2)
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
C
      DATA IDUM/8*0/
C
C     ------------------------------------------------------------------
C*          1.  EXPAND SECTION 2.
C               -----------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      DO 101 I=1,JSEC2
      ISEC2(I)=0
 101  CONTINUE
C
      IF(KSEC1(5) .NE. 0 .AND. KSEC1(5) .NE.128) THEN
         KERR=50
         CALL BUERR(KERR)
         RETURN
      END IF
C
      IF( KSEC1(5).EQ.0) RETURN
C
C*          1.1  KEEP POINTERS TO THE BEGINING OF THE SECTION.
C                ---------------------------------------------
 110  CONTINUE
C
      IWPTB = NWPT
      IBPTB = NBPT
C
C*          1.2  PACK LENGTH OF SECTION 2.
C                -------------------------
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING LENGTH OF SECTION 2.'
         RETURN
      END IF
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) RETURN
C
C
C*          1.3  LOCAL ADP CENTRE INFORMATION.
C                -----------------------------
C
      IOFF=KSEC2(1)-4
      IF(IOFF.GT.0) THEN
         IW=2
         IB=0
         CALL BUNPKS(NBPW,KSEC2,ISEC2,IW,IB,8,0,IOFF,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING LOCAL ADP CENTRE INFORMATION'
            WRITE(KNTN,*) 'IN SECTION 2.'
            RETURN
         END IF
C
         CALL BUPKS(NBPW,KBUFF(NWPT),ISEC2,NWPT,NBPT,8,0,IOFF,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING LOCAL ADP CENTRE INFORMATION'
            WRITE(KNTN,*) 'IN SECTION 2.'
            RETURN
         END IF
      END IF
C
C
C*          1.5  SET UP LENGTH OF THE SECTION 2.
C                --------------------------------
 150  CONTINUE
C
      CALL BUOCTN(IWPTB,IBPTB,KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
      RETURN
      END
