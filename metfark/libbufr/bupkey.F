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

      SUBROUTINE BUPKEY( KEY,KSEC1,KSEC2,KERR )
C
C**** *BUPKEY*
C
C
C     PURPOSE.
C     --------
C          PACK LOCAL ECMWF INFORMATION (RDB KEY) INTO KSEC2 ARRAY.
C
C
C**   INTERFACE.
C     ----------
C          *CALL* *BUPKEY(KEY,KSEC1,KSEC2,KERR)*
C
C        INPUT :
C               *KEY*     -  ARRAY CONTAINING RDB INFORMATION
C                            KEY( 1)-- LENGTH OF SECTION 2 (BYTES)
C                            KEY( 2)-- RDB TYPE
C                            KEY( 3)-- RDB SUBTYPE
C                            KEY( 4)-- YEAR
C                            KEY( 5)-- MONTH
C                            KEY( 6)-- DAY
C                            KEY( 7)-- HOUR
C                            KEY( 8)-- MINUTE
C                            KEY( 9)-- SECOND
C                            KEY(10)-- LONGITUDE1
C                            KEY(11)-- LATITUDE1
C                            KEY(12)-- LONGITUDE2
C                            KEY(13)-- LATITUDE2
C                            KEY(14)-- NUMBER OF SUBSETS
C                            KEY(15)-- IDENT (NUMERIC)
C                            KEY(16)-- IDENT ( CCITTIA5) ONE CHARACTER
C                            KEY(17)-- IDENT ( CCITTIA5) ONE CHARACTER
C                            KEY(18)-- IDENT ( CCITTIA5) ONE CHARACTER
C                            KEY(19)-- IDENT ( CCITTIA5) ONE CHARACTER
C                            KEY(20)-- IDENT ( CCITTIA5) ONE CHARACTER
C                            KEY(21)-- IDENT ( CCITTIA5) ONE CHARACTER
C                            KEY(22)-- IDENT ( CCITTIA5) ONE CHARACTER
C                            KEY(23)-- IDENT ( CCITTIA5) ONE CHARACTER
C                            KEY(24)-- IDENT ( CCITTIA5) ONE CHARACTER
C                            KEY(25)-- TOTAL BUFR MESSAGE LENGTH
C                            KEY(26)-- DAY    (RDB INSERTION)
C                            KEY(27)-- HOUR   (RDB INSERTION)
C                            KEY(28)-- MINUTE (RDB INSERTION)
C                            KEY(29)-- SECOND (RDB INSERTION)
C                            KEY(30)-- DAY    (MDB INSERTION)
C                            KEY(31)-- HOUR   (MDB INSERTION)
C                            KEY(32)-- MINUTE (MDB INSERTION)
C                            KEY(33)-- SECOND (MDB INSERTION)
C                            KEY(34)-- CORRECTION NUMBER
C                            KEY(35)-- PART
C                            KEY(36)-- 0
C                            KEY(37)-- CORRECTION NUMBER
C                            KEY(38)-- PART
C                            KEY(39)-- 0
C                            KEY(40)-- CORRECTION NUMBER
C                            KEY(41)-- PART
C                            KEY(42)-- 0
C                            KEY(43)-- CORRECTION NUMBER
C                            KEY(44)-- PART
C                            KEY(45)-- 0
C                            KEY(46)-- THE LOWEST Q/C % CONFIDENCE
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
C        OUTPUT:
C               *KSEC2*   -  ARRAY CONTAINING SECTION 2 INFORMATION
C                            KSEC2( 1)-- LENGTH OF SECTION 2 (BYTES)
C                            KSEC2( 2) TO KSEC2(JSEC2) LOCAL ADP CENTRE
C                                        INFORMATION(PACKED FORM)
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
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
#     include "bcmdefc.F"
C
      CHARACTER*4 CECMWF,CUSER
C
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
      DIMENSION KSEC1(JSEC1),KSEC2(JSEC2),KEY(JKEY)
      DIMENSION IDUM(8),KSEC3(JSEC3)
C
      DATA IDUM/8*0/

C
C*          1.  PACK LOCAL ADP CENTRE INFORMATION INTO KSEC2 ARRAY.
C                --------------------------------------------------
C
      KERR=0
C     
      IF(CECMWF.NE.'ECMF') THEN
         CALL BUIVAR(KERR)
         CECMWF='ECMF'
      END IF

C
C      IF(KSEC1(3).EQ.98) THEN
         IW=2
         IB=0
         CALL BUPCK(NBPW,KSEC2(IW),KEY(2)       ,IW,IB, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(2).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(3)       ,IW,IB, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(3).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(4)       ,IW,IB,12,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(4).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(5)       ,IW,IB, 4,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(5).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(6)       ,IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(6).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(7)       ,IW,IB, 5,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(7).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(8)       ,IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(8).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(9)       ,IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(9).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),0            ,IW,IB, 1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUPCK(NBPW,KSEC2(IW),KEY(10)      ,IW,IB,26,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(10).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),0            ,IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUPCK(NBPW,KSEC2(IW),KEY(11)      ,IW,IB,25,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(11).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),0            ,IW,IB, 7,KERR)
         IF(KERR.GT.0) RETURN
C
         IF(KEY(14).GT.1.OR.KEY(2).EQ.2.OR.KEY(2).EQ.3.OR.
     1      KEY(2).EQ.12.OR.KEY(2).EQ.8) THEN
            CALL BUPCK(NBPW,KSEC2(IW),KEY(12)   ,IW,IB,26,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)  'ERROR PACKING KEY(12).'
               RETURN
            END IF
            CALL BUPCK(NBPW,KSEC2(IW),0         ,IW,IB, 6,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUPCK(NBPW,KSEC2(IW),KEY(13)   ,IW,IB,25,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)  'ERROR PACKING KEY(13).'
               RETURN
            END IF
            CALL BUPCK(NBPW,KSEC2(IW),0         ,IW,IB, 7,KERR)
            IF(KERR.GT.0) RETURN
            IF(KEY(14).GT.255.OR.KEY(3).GE.121.AND.
     1         KEY(3).LE.130.OR.KEY(3).EQ.31) THEN
               CALL BUPCK(NBPW,KSEC2(IW),KEY(14),IW,IB,16,KERR)
               IF(KERR.GT.0) THEN
                  WRITE(KNTN,*)  'ERROR PACKING KEY(14).'
                  RETURN
               END IF
               CALL BUPCK(NBPW,KSEC2(IW),KEY(15),IW,IB,16,KERR)
               IF(KERR.GT.0) THEN
                  WRITE(KNTN,*)  'ERROR PACKING KEY(15).'
                  RETURN
               END IF
               CALL BUPKS(NBPW,KSEC2(IW),IDUM  ,IW,IB,8,0,4,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUPCK(NBPW,KSEC2(IW),0      ,IW,IB, 8,KERR)
               IF(KERR.GT.0) RETURN
            ELSE
               CALL BUPCK(NBPW,KSEC2(IW),KEY(14),IW,IB, 8,KERR)
               IF(KERR.GT.0) THEN
                  WRITE(KNTN,*)  'ERROR PACKING KEY(14).'
                  RETURN
               END IF
               CALL BUPCK(NBPW,KSEC2(IW),KEY(15),IW,IB,16,KERR)
               IF(KERR.GT.0) THEN
                  WRITE(KNTN,*)  'ERROR PACKING KEY(15).'
                  RETURN
               END IF
               CALL BUPKS(NBPW,KSEC2(IW),IDUM      ,IW,IB,8,0,4,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUPCK(NBPW,KSEC2(IW),0      ,IW,IB,16,KERR)
               IF(KERR.GT.0) RETURN
            END IF
            GO TO 140
         ELSE
            CALL BUPKS(NBPW,KSEC2(IW),KEY(16),IW,IB,8,0,9,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)  'ERROR PACKING KEY(16).'
               RETURN
            END IF
            CALL BUPKS(NBPW,KSEC2(IW),IDUM(1),IW,IB,8,0,8,KERR)
            IF(KERR.GT.0) RETURN
         END IF
C
C
C*          1.4 SUB KEY INFORMATION.
C               --------------------
 140  CONTINUE
C
         CALL BUPCK(NBPW,KSEC2(IW),KEY(25),IW,IB,16,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(25).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(26),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(26).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(27),IW,IB, 5,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(27).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(28),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(28).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(29),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(29).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),0      ,IW,IB, 1,KERR)
         IF(KERR.GT.0) RETURN
C
         CALL BUPCK(NBPW,KSEC2(IW),KEY(30),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(30).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(31),IW,IB, 5,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(31).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(32),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(32).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(33),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(33).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),0      ,IW,IB, 1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUPCK(NBPW,KSEC2(IW),KEY(34),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(34).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(35),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(35).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(36),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(36).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(37),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(37).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(38),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(38).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(39),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(39).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(40),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(40).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(41),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(41).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(42),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(42).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(43),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(43).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(44),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(44).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(45),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(45).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(46),IW,IB, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)  'ERROR PACKING KEY(46).'
            RETURN
         END IF
C
C      ELSE
C         WRITE(KNTN,'(1H )')
C         WRITE(KNTN,'(1H ,A)') 'BUPKEY : KEY DEFINITION NOT PACKED.'
C         WRITE(KNTN,'(1H )')
C      END IF
C
      RETURN
      END
