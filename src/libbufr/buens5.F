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

      SUBROUTINE BUENS5( KSEC0,KSEC1,KBUFL,KBUFF,KERR)
C
C
C
C
C     PURPOSE.
C     --------
C          PACK SECTION 5 OF BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUENS5( KSEC0,KSEC1,KBUFL,KBUFF,KERR)*
C
C        INPUT :
C               *KSEC0*   -  ARRAY CONTAINING SECTION 0 INFORMATION
C                            KSEC0( 1)-- LENGTH OF SECTION 0 (BYTES)
C                            KSEC0( 2)-- TOTAL LENGTH OF BUFR MESSAGE (BYTES)
C                            KSEC0( 3)-- BUFR EDITION NUMBER
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
C
C        OUTPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C     --------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUPKS         - PACK BIT PATHERN IN REPEATED WAY
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/91.
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
C
#     include "parameter.F"
#     include "bcomunit.F"
#     include "bcmwork.F"
#     include "bcprq.F"
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION IBUFR(4)
      DIMENSION KSEC0(JSEC0),KSEC1(JSEC1)
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
C
      DATA IBUFR/55,55,55,55/
C
C     ------------------------------------------------------------------
C*          1.   EXPAND SECTION 5.
C                -----------------
 100  CONTINUE
C
      IF( KERR.GT.0 ) RETURN
C
C*          1.2  PACK LAST FOUR OCTETS CONTAINING '7777'.
C                ----------------------------------------
C
      CALL BUPKS(NBPW,KBUFF(NWPT),IBUFR,NWPT,NBPT,8,0,4,KERR)
      IF(KERR.GT.0) RETURN
C
C*          1.3  SET TOTAL LENGTH OF BUFR MESSAGE.
C                ---------------------------------
 130  CONTINUE
C
      IBYTES=(NWPT-1)*NBPW/8+NBPT/8
      IWORDS=IBYTES*8/NBPW
      IF(IWORDS*NBPW.LT.IBYTES*8) IWORDS=IWORDS+1
      KBUFL =IWORDS
C
C*          1.4  SET TOTAL LENGTH OF BUFR MESSAGE IN SECTION 0.
C                ----------------------------------------------
 140  CONTINUE
C
      KSEC0(2)=IBYTES
C
      IF(KSEC0(3).GT.1) THEN
C
C        FOR BUFR EDITION 2 LENGTH OF MESSAGE STARTS AT 5 TH BYTE.
C
         IWPT= 32/NBPW + 1
         IBPT= 32 - (IWPT-1)* NBPW
C
         CALL BUPCK(NBPW,KBUFF(IWPT),IBYTES,IWPT,IBPT,24,KERR)
         IF(KERR.GT.0) THEN
          WRITE(KNTN,*)  'ERROR PACKING TOTAL LENGTH OF BUFR MESSAGE
     1 IN SECTION 0.'
         END IF
      END IF
C
C*          1.5 SET TOTAL BUFR LENGTH IN SECTION 2
C
 150  CONTINUE
C
C     RDB KEY PACKED ONLY FOR ECMWF DATA AND SECTION 2 PRESENT.
C
      IF(NOKEY.EQ.0.AND.KSEC1(5).NE.0) THEN
         IBTS=4
         IF(KSEC0(3).GT.1) IBTS=8
C
C        SIZE OF SECTION 0 AND 1 PLUS 36 BYTES FROM BEGINING
C        OF SECTION 2.
C
         IBTS=IBTS+KSEC1(1)+36
         IBIT=IBTS*8
C
         IWPT= IBIT/NBPW + 1
         IBPT= IBIT - (IWPT-1)* NBPW
C
         IF(IBYTES .GT. 65535 ) IBYTES=65535
C
         CALL BUPCK(NBPW,KBUFF(IWPT),IBYTES,IWPT,IBPT,16,KERR)
         IF(KERR.GT.0) THEN
          WRITE(KNTN,*)  'ERROR PACKING TOTAL LENGTH OF BUFR MESSAGE
     1 IN SECTION 2.'
         END IF
      END IF
C
      RETURN
      END
