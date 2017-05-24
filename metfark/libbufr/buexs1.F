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

      SUBROUTINE BUEXS1( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KERR)
C
C**** *BUEXS1*
C
C
C     PURPOSE.
C     --------
C          EXPANDS SECTION 1 OF BUFR MESSAGE. SAVES EXPANDED INFORMATION
C     IN THE ARRAY KSEC1.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS1( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C        OUTPUT:
C               *KSUP*    -  ARRAY CONTAINING SUPLEMENTARY INFORMATION
C                         -  KSUP( 1) -- IDIM1, DIMENSION OF KSEC1
C                         -  KSUP( 2) -- IDIM2, DIMENSION OF KSEC2
C                         -  KSUP( 3) -- IDIM3, DIMENSION OF KSEC3
C                         -  KSUP( 4) -- IDIM4, DIMENSION OF KSEC4
C                         -  KSUP( 5) -- M (NUMBER OF ELEMENTS IN VALUES ARRAY,
C                                           FIRST INDEX)
C                         -  KSUP( 6) -- N (NUMBER OF SUBSETS,SECOND INDEX OF
C                                           VALUES ARRAY)
C                         -  KSUP( 7) -- JVC (NUMBER OF ELEMENTS IN CVAL ARRAY)
C                         -  KSUP( 8) -- TOTAL BUFR MESSAGE LENGTH IN BYTES
C                         -  KSUP( 9) -- IDIM0, DIMENSION OF KSEC0
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
C                            KSEC1(16) - KSEC1(JSEC1) -- LOCAL ADP CENTRE
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
C
C     *METHOD.
C      -------
C
C           NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          -  UNPACK BIT PATTERN
C          BUNEXS         -  SET WORD AND BIT POINTERS AT THE BEGINING OF
C                            NEXT SECTION
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
#     include "bcomwork.F"
C
      DIMENSION KBUFF(KBUFL),KSUP(JSUP),KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION ISEC1(JSEC1)
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
C
C
C     ------------------------------------------------------------------
C*          1.  EXPAND SECTION 1.
C               ------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C     SAVE WORD AND BIT POINTERS OF THR BRGINING OF SECTION 1.
C
      IWPTB=NWPT
      IBPTB=NBPT
C
C*          1.1 UNPACK LENGTH OF SECTION 1.
C               ----------------------------
 110  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(1),NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(1).'
         RETURN
      END IF
C     IF(KSEC1(1).GT.18) THEN
C        WRITE(KNTN,*)  'LOCAL ADP CENTRE INFORMATION PRESENT 
C    1 IN THE SECTION 1.'
C     END IF
C
C*          1.1.1 SET THE POINTERS NWPTB AND NBPTB.
C                 ---------------------------------
C                 TO BEGINING OF THE NEXT SECTION.
C                 --------------------------------
 1110 CONTINUE
C
      CALL BUNEXS(KSEC1(1))
C
C
C*          1.2  UNPACK BUFR EDITION NUMBER/MASTER TABLE USED.
C                ---------------------------------------------
 120  CONTINUE
C
      IF(KSEC0(3).LE.1) THEN
         CALL BUNPCK(NBPW,KBUFF,KSEC1(2),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(2).'
            RETURN
         END IF
      ELSE
         CALL BUNPCK(NBPW,KBUFF,KSEC1(14),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(14).'
            RETURN
         END IF
C
         KSEC1(2)=KSEC0(3)
      END IF
C
C*          1.3  UNPACK ORIGINATING CENTRE.
C                --------------------------
 130  CONTINUE
C
      IF(KSEC0(3).LT.3) THEN
         CALL BUNPCK(NBPW,KBUFF,KSEC1(3),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(3).'
            RETURN
         END IF
      ELSEIF(KSEC0(3).EQ.3) THEN
C        BUFR Edition 3
         CALL BUNPCK(NBPW,KBUFF,KSEC1(16),NWPT,NBPT,8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(3).'
            RETURN
         END IF
         CALL BUNPCK(NBPW,KBUFF,KSEC1(3),NWPT,NBPT,8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(3).'
            RETURN
         END IF
      ELSEIF(KSEC0(3).EQ.4) THEN
C        BUFR Edition 4
         CALL BUNPCK(NBPW,KBUFF,KSEC1(3),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(3).'
            RETURN
         END IF
         CALL BUNPCK(NBPW,KBUFF,KSEC1(16),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(16).'
            RETURN
         END IF
      END IF
      
C
C*          1.4  UNPACK UPDATE SEQUENCE NUMBER.
C                ------------------------------
 140  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(4),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(4).'
         RETURN
      END IF
C
C*          1.5  UNPACK INTEGER VALUE OF THE OCTET CONTAINING
C                --------------------------------------------
C                 FLAG BITS(ZERO IF SECTION TWO IS NOT PRESENT).
C                 ----------------------------------------------
 150  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(5),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(5).'
         RETURN
      END IF
C
C*          1.6  UNPACK *BUFR* MESSAGE TYPE.
C                ---------------------------
 160  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(6),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(6).'
         RETURN
      END IF
C
C*          1.7  UNPACK *BUFR* MESSAGE SUB-TYPE.
C                -------------------------------
 170  CONTINUE
C
      IF(KSEC0(3).EQ.4) THEN
         CALL BUNPCK(NBPW,KBUFF,KSEC1(17),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(17).'
            RETURN
         END IF
         CALL BUNPCK(NBPW,KBUFF,KSEC1(7),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(7).'
            RETURN
         END IF
      ELSE
         CALL BUNPCK(NBPW,KBUFF,KSEC1(7),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(7).'
            RETURN
         END IF
      END IF
C
C*          1.8  UNPACK LOCAL TABLE VERSION NUMBER OR
C                ------------------------------------
C                VERSION NUMBER OF MASTER TABLE USED.
C                 -----------------------------------
 180  CONTINUE
C
      IF(KSEC0(3).LE.1) THEN
         CALL BUNPCK(NBPW,KBUFF,KSEC1(8),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(8).'
            RETURN
         END IF
      ELSE
         CALL BUNPCK(NBPW,KBUFF,KSEC1(15),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(15).'
            RETURN
         END IF
         CALL BUNPCK(NBPW,KBUFF,KSEC1( 8),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(8).'
            RETURN
         END IF
      END IF
C
C*          1.9  UNPACK YEAR.
C                ------------
 190  CONTINUE
C
      IF(KSEC0(3).EQ.4) THEN
         CALL BUNPCK(NBPW,KBUFF,KSEC1(9),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(9).'
            RETURN
         END IF
      ELSE
         CALL BUNPCK(NBPW,KBUFF,KSEC1(9),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(9).'
            RETURN
         END IF
      END IF
C
C*          2. UNPACK MONTH.
C              -------------
 200  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(10),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(10).'
         RETURN
      END IF
C
C*          2.1 UNPACK DAY.
C               -----------
 210  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(11),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(11).'
         RETURN
      END IF
C
C*          2.2 UNPACK HOUR.
C               ------------
 220  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(12),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(12).'
         RETURN
      END IF
C
C*          2.3 UNPACK MINUTE.
C               --------------
 230  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(13),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(13).'
         RETURN
      END IF
C
C           2.3.1 UNPACK SECOND
C                 -------------
      IF(KSEC0(3).EQ.4) THEN
         CALL BUNPCK(NBPW,KBUFF,KSEC1(18),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(13).'
            RETURN
         END IF
C
C                 METADATA
C
C                 YEAR
C
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(19),NWPT,NBPT,16,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(19).'
c           RETURN
c        END IF
C
C*             UNPACK MONTH.
C              -------------
C
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(20),NWPT,NBPT, 8,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(20).'
c           RETURN
c        END IF
C
C*              UNPACK DAY.
C               -----------
C
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(21),NWPT,NBPT, 8,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(21).'
c           RETURN
c        END IF
C
C*              UNPACK HOUR.
C               ------------
C
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(22),NWPT,NBPT, 8,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(22).'
c           RETURN
c        END IF
C
C*              UNPACK MINUTE.
C               --------------
C
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(23),NWPT,NBPT, 8,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(23).'
c           RETURN
c        END IF
C
C               UNPACK SECOND
C                 -------------
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(24),NWPT,NBPT, 8,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(24).'
c           RETURN
c        END IF
C
C                 YEAR
C
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(25),NWPT,NBPT,16,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(25).'
c           RETURN
c        END IF
C
C*             UNPACK MONTH.
C              -------------
C
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(26),NWPT,NBPT, 8,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(26).'
c           RETURN
c        END IF
C
C*              UNPACK DAY.
C               -----------
C
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(27),NWPT,NBPT, 8,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(27).'
c           RETURN
c        END IF
C
C*              UNPACK HOUR.
C               ------------
C
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(28),NWPT,NBPT, 8,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(28).'
c           RETURN
c        END IF
C
C*              UNPACK MINUTE.
C               --------------
C
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(29),NWPT,NBPT, 8,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(29).'
c           RETURN
c        END IF
C
C               UNPACK SECOND
C                 -------------
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(30),NWPT,NBPT, 8,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(30).'
c           RETURN
c        END IF
C
C
C               UNPACK  MOST SOUTHERN LATITUDE
C               ------------------------------
C        CALL BUNPCK(NBPW,KBUFF,ISG,NWPT,NBPT,1,KERR)
C        IF(KERR.GT.0) THEN
C           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(31).'
C           RETURN
C        END IF
C
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(31),NWPT,NBPT,32,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(31).'
c           RETURN
c        END IF

C        IF(ISG.EQ.1)  KSEC1(31)=-KSEC1(31)
C
C               UNPACK  MOST WESTERN LONGITUDE
C               ------------------------------
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(32),NWPT,NBPT,32,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(32).'
c           RETURN
c        END IF
C
C               UNPACK  MOST NORTHERN LATITUDE
C               ------------------------------
C        CALL BUNPCK(NBPW,KBUFF,ISG,NWPT,NBPT,1,KERR)
C        IF(KERR.GT.0) THEN
C           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(33).'
C           RETURN
C        END IF

c        CALL BUNPCK(NBPW,KBUFF,KSEC1(33),NWPT,NBPT,32,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(33).'
c           RETURN
c        END IF

C        IF(ISG.EQ.1) KSEC1(33)=-KSEC1(33)
C
C               UNPACK  MOST EASTERN LONGITUDE
C               ------------------------------
c        CALL BUNPCK(NBPW,KBUFF,KSEC1(34),NWPT,NBPT,32,KERR)
c        IF(KERR.GT.0) THEN
c           WRITE(KNTN,*)  'ERROR UNPACKING KSEC1(34).'
c           RETURN
c        END IF
C
      END IF
C
C*          2.4 UNPACK LOCAL ADP CENTRE INFORMATION IF ANY.
C               -------------------------------------------
 240  CONTINUE
C
      IF(KSEC0(3).LT.3) THEN
         IOFF=KSEC1(1)-17
         IW=16
         IBT=0
      ELSEIF(KSEC0(3).EQ.3) THEN
         IOFF=KSEC1(1)-17
         IW=18
         IBT=0
      ELSEIF(KSEC0(3).EQ.4) THEN
C        IOFF=KSEC1(1)-52
         IOFF=KSEC1(1)-22
c        IW=35     ! index for local info
         IW=19     ! index for local info
         IBT=0
      END IF
      IF(IOFF.LE.JSEC1) THEN
         CALL BUNPKS(NBPW,KBUFF,ISEC1(1),NWPT,NBPT,
     1   8,0,IOFF,KERR)
         IF(KERR.GT.0) THEN
          WRITE(KNTN,*)  'ERROR UNPACKING LOCAL ADP CENTRE INFORMATION'
            WRITE(KNTN,*)  'IN SECTION 1.'
            RETURN
         END IF
C
C                PACK LOCAL ADP CENTRE INFORMATION INTO KSE1(16) ONWARD.
C
         CALL BUPKS(NBPW,KSEC1(IW),ISEC1,IW,IBT,8,0,IOFF,KERR)
         IF(KERR.GT.0) THEN
          WRITE(KNTN,*)  'ERROR PACKING LOCAL ADP CENTRE INFORMATION'
            RETURN
         END IF
      ELSE
         IOFF=0
         KERR=4
         CALL BUERR(KERR)
         GO TO 300
      END IF
C
C
C*          3.  SET SUPPLEMENTARY INFORMATION.
C               ------------------------------
 300  CONTINUE
C
      KSUP(1)=15+IOFF/4+1
C
C     ------------------------------------------------------------------
C
      RETURN
      END
