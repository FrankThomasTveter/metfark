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

      SUBROUTINE BUENS1( KSEC0,KSEC1,KBUFL,KBUFF,KERR)
C
C**** *BUENS1*
C
C
C     PURPOSE.
C     --------
C          PACK SECTION 1 OF BUFR MESSAGE.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUENS1( KSEC0,KSEC1,KBUFL,KBUFF,KERR)*
C
C
C        INPUT :
C               *KSEC0*   -  ARRAY CONTAINING SECTION 0 INFORMATION
C                            KSEC0( 1)-- LENGTH OF SECTION 0 (BYTES)
C                            KSEC0( 2)-- TOTAL LENGTH OF BUFR MESSAGE (BYTES)
C                            KSEC0( 3)-- BUFR EDITION NUMBER
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
C        OUTPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KERR*    -  RETURNED ERROR CODE
C
C*     METHOD.
C      -------
C
C           NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          BUPCK         -  PACK BIT PATHERN
C          BUOCTN        - SET LENGTH OF SECTION
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       16/01/91.
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
      DIMENSION KBUFF(KBUFL),KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION ISEC1(JSEC1)
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
C
C     ------------------------------------------------------------------
C*          1.  PACK SECTION 1.
C               ---------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      DO 101 I=1,JSEC1
      ISEC1(I)=0
 101  CONTINUE
C
C*          1.1  KEEP POINTERS TO THE BEGINING OF THE SECTION.
C                ---------------------------------------------
 110  CONTINUE
C
      IWPTB = NWPT
      IBPTB = NBPT
C
C
C*          1.2 PACK LENGTH OF SECTION 1.
C               -------------------------
 120  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING LENGTH OF SECTION 1.'
         RETURN
      END IF
C
C
C*          1.3  PACK BUFR EDITION NUMBER/MASTER TABLE USED.
C                -------------------------------------------
 130  CONTINUE
C
      IF(KSEC0(3).LE.1) THEN
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(2),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING BUFR EDITION NUMBER' 
            WRITE(KNTN,*) 'IN SECTION 1.'
            RETURN
         END IF
      ELSE
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(14),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING BUFR MASTER TABLE NUMBER'
            WRITE(KNTN,*) 'IN SECTION 1.'
            RETURN
         END IF
      END IF
C
C*          1.4  PACK ORIGINATING CENTRE.
C                ------------------------
 140  CONTINUE
C
      IF(KSEC0(3).LT.3) THEN
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(3),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING ORIGINATING CENTRE IN SECTION 1.'
            RETURN
         END IF
      ELSEIF(KSEC0(3).EQ.3) THEN
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(16),NWPT,NBPT,8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING ORIGINATING ',
     1                    'SUB-CENTRE IN SECTION 1.'
            RETURN
         END IF
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(3),NWPT,NBPT,8,KERR)
         IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING ORIGINATING CENTRE IN SECTION 1.'
            RETURN
         END IF
      ELSEIF(KSEC0(3).EQ.4) THEN
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(3),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING ORIGINATING CENTRE IN SECTION 1.'
            RETURN
         END IF
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(16),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING ORIGINATING ',
     1                    'SUB-CENTRE IN SECTION 1.'
            RETURN
         END IF
      END IF
C
C*          1.5  PACK UPDATE SEQUENCE NUMBER.
C                ----------------------------
 150  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(4),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING UPDATE SEQUENCE NUMBER ',
     1                 'IN SECTION 1.'
         RETURN
      END IF
C
C*          1.6  PACK INTEGER VALUE OF THE OCTET CONTAINING
C                ------------------------------------------
C                FLAG BITS(ZERO IF SECTION TWO IS NOT PRESENT).
C                ----------------------------------------------
 160  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(5),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING FLAG IN SECTION 1.'
         RETURN
      END IF
C
C*          1.7  PACK *BUFR* MESSAGE TYPE.
C                -------------------------
 170  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(6),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
      WRITE(KNTN,*) 'ERROR PACKING BUFR MESSAGE TYPE IN SECTION 1.'
         RETURN
      END IF
C
C           1.7.1 PACK BUFR INTERNATIONAL SUB-CATEGORY
C                 -------------------------------------
      IF(KSEC0(3).EQ.4) THEN
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(17),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING KSEC1(17) IN SECTION 1.'
            RETURN
         END IF
      END IF
C
C*          1.8  PACK *BUFR* MESSAGE SUB-TYPE.
C                -----------------------------
 180  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(7),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING BUFR MESSAGE SUBTYPE.'
         RETURN
      END IF
C
C*          1.9  PACK LOCAL TABLE VERSION NUMBER OR
C                ----------------------------------
C                VERSION NUMBER OF MASTER TABLE USED.
C                ------------------------------------
 190  CONTINUE
C
      IF(KSEC0(3).LE.1) THEN
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(8),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING VERSION NUMBER OF ',
     1                    'LOCAL TABLE USED.'
            RETURN
         END IF
      ELSE
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(15),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING VERSION NUMBER OF ',
     1                    'MASTER TABLE USED.'
            RETURN
         END IF
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1( 8),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING VERSION NUMBER OF ',
     1                    'LOCAL TABLE USED.'
            RETURN
         END IF
      END IF
C
C*          2.0  PACK YEAR.
C                ----------
 200  CONTINUE
C
      IF(KSEC0(3).LE.3) THEN
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(9),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING KSEC1(9) IN SECTION 1.'
            RETURN
         END IF
      ELSE
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(9),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING KSEC1(9) IN SECTION 1.'
            RETURN
         END IF
      END IF
C
C*          2.1  PACK MONTH.
C                -----------
 210  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(10),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING KSEC1(10) IN SECTION 1.'
         RETURN
      END IF
C
C*          2.2 PACK DAY.
C               -----------
 220  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(11),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING KSEC1(11) IN SECTION 1.'
         RETURN
      END IF
C
C*          2.3 PACK HOUR.
C               ------------
 230  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(12),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING KSEC1(12) IN SECTION 1.'
         RETURN
      END IF
C
C*          2.4 PACK MINUTE.
C               --------------
 240  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(13),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING KSEC1(13) IN SECTION 1.'
         RETURN
      END IF
C
C          2.4.1 PACK SECOND
C                -----------
      IF(KSEC0(3).EQ.4) THEN
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(18),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING KSEC1(18) IN SECTION 1.'
            RETURN
         END IF
      END IF

C
C          2.5 PACK LOCAL ADP CENTRE INFORMATION.
C              ----------------------------------
 250  CONTINUE
C
      IF(KSEC0(3).LT.3) THEN
         IOFF=KSEC1(1)-17
         IW=16
         IB=0
      ELSEIF(KSEC0(3).EQ.3) THEN
         IOFF=KSEC1(1)-17
         IF(IOFF.LT.1) THEN
           WRITE(KNTN,*) 'ERROR IN KSEC1(1)'
           WRITE(KNTN,*) 'THE SIZE SHALL BE AT LEAST 18 BYTES'
           KERR=55
           RETURN
         END IF
         IW=18
         IB=0
      ELSEIF(KSEC0(3).EQ.4) THEN
C        IOFF=KSEC1(1)-52
         IOFF=KSEC1(1)-22
         IF(IOFF.LT.0) THEN
           WRITE(KNTN,*) 'ERROR IN KSEC1(1)'
           WRITE(KNTN,*) 'THE SIZE SHALL BE AT LEAST 22 BYTES'
           KERR=55
           RETURN
         END IF
C        IW=35      ! index for local info
         IW=19      ! index for local info
         IB=0
      END IF
      IF(IOFF.GT.0) THEN
         CALL BUNPKS(NBPW,KSEC1,ISEC1,IW,IB,8,0,IOFF,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING LOCAL ADP CENTRE INFORMATION'
            WRITE(KNTN,*) 'IN SECTION 1.'
            RETURN
         END IF
C
         CALL BUPKS(NBPW,KBUFF(NWPT),ISEC1,NWPT,NBPT,8,0,IOFF,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING LOCAL ADP CENTRE INFORMATION'
            WRITE(KNTN,*) 'IN SECTION 1.'
            RETURN
         END IF
      END IF
C     ------------------------------------------------------------------
C*          2.6  SET UP LENGTH OF THE SECTION 1.
C                --------------------------------
 260  CONTINUE
C
      CALL BUOCTN(IWPTB,IBPTB,KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C
      RETURN
      END
