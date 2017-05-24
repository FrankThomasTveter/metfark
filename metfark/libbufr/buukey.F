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

      SUBROUTINE BUUKEY( KSEC1,KSEC2,KEY,KSUP,KERR )
C
C**** *BUUKEY*
C
C
C     PURPOSE.
C     --------
C          EXPANDS LOCAL ECMWF INFORMATION FROM SECTION 2.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)*
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
C               *KSEC2*   -  ARRAY CONTAINING SECTION 2 INFORMATION
C                            KSEC2( 1)-- LENGTH OF SECTION 2 (BYTES)
C                            KSEC2( 2) TO KSEC2(JSEC2) LOCAL ADP CENTRE
C                                        INFORMATION(PACKED FORM)
C        OUTPUT:
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
C          BUNPCK          - UNPACK BIT PATTERN
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
#     include "bcomwork.F"
#     include "bcomdefc.F"
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
      DIMENSION IDUM(8)     ,KSUP (JSUP ),KSEC3(JSEC3)
C
C
C*          1. UNPACK LOCAL ADP CENTRE INFORMATION.
C              ------------------------------------
 100  CONTINUE
C
      KERR=0
C
      IF(CECMWF.NE.'ECMF') THEN
         CALL BUEVAR(KERR)
         CECMWF='ECMF'
      END IF
C
      DO 101 I=1,JKEY
      KEY(I)=0
 101  CONTINUE
C
      KEY(1)=KSEC2(1)
      IW=2
      IB=0
      IF(KSEC2(1).EQ.52) THEN
         CALL BUNPCK(NBPW,KSEC2,KEY(2),IW,IB, 8,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(3),IW,IB, 8,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(4),IW,IB,12,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(5),IW,IB, 4,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(6),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(7),IW,IB, 5,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(8),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(9),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,IDUMMY  ,IW,IB, 1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(10),IW,IB,26,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,IDUMMY  ,IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(11),IW,IB,25,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB, 7,KERR)
         IF(KERR.GT.0) RETURN
C
         IF(KSUP(6).GT.1.OR.KEY(2).EQ.2.OR.KEY(2).EQ.3.OR.
     1      KEY(2).EQ.12.OR.KEY(2).EQ.8) THEN
C
            CALL BUNPCK(NBPW,KSEC2,KEY(12),IW,IB,26,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB, 6,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(13),IW,IB,25,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB, 7,KERR)
            IF(KERR.GT.0) RETURN
            IF(KSUP(6).GT.255.OR.KEY(3).GE.121.AND.
     1         KEY(3).LE.130.OR.KEY(3).EQ.31) THEN
               CALL BUNPCK(NBPW,KSEC2,KEY(14),IW,IB,16,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,KEY(15),IW,IB,16,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPKS(NBPW,KSEC2,IDUM(1),IW,IB,8,0,4,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB, 8,KERR)
               IF(KERR.GT.0) RETURN
            ELSE
               CALL BUNPCK(NBPW,KSEC2,KEY(14),IW,IB, 8,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,KEY(15),IW,IB,16,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPKS(NBPW,KSEC2,IDUM(1),IW,IB,8,0,4,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB,16,KERR)
               IF(KERR.GT.0) RETURN
            END IF
            GO TO 140
         ELSE
            CALL BUNPKS(NBPW,KSEC2,KEY(16),IW,IB,8,0,9,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPKS(NBPW,KSEC2,IDUM(1)  ,IW,IB,8,0,8,KERR)
            IF(KERR.GT.0) RETURN
         END IF
C
C
C*          1.4 SUB KEY INFORMATION.
C               --------------------
 140  CONTINUE
C
         CALL BUNPCK(NBPW,KSEC2,KEY(25),IW,IB,16,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(26),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(27),IW,IB, 5,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(28),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(29),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB, 1,KERR)
         IF(KERR.GT.0) RETURN
C
         CALL BUNPCK(NBPW,KSEC2,KEY(30),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(31),IW,IB, 5,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(32),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(33),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB, 1,KERR)
         IF(KERR.GT.0) RETURN
C
         CALL BUNPCK(NBPW,KSEC2,KEY(34),IW,IB,6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(35),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(36),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(37),IW,IB,6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(38),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(39),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(40),IW,IB,6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(41),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(42),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(43),IW,IB,6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(44),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(45),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(46),IW,IB,8,KERR)
         IF(KERR.GT.0) RETURN
         KSUP( 2)=46
      ELSE
         IF(KSEC2(1).EQ.28) THEN
            CALL BUNPCK(NBPW,KSEC2,KEY(2),IW,IB, 8,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(3),IW,IB, 8,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(4),IW,IB,12,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(5),IW,IB, 4,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(6),IW,IB, 6,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(7),IW,IB, 5,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(8),IW,IB, 6,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(9),IW,IB, 6,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,IDUMMY  ,IW,IB, 1,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(10),IW,IB,26,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(11),IW,IB,25,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,IDUMMY  ,IW,IB, 5,KERR)
            IF(KERR.GT.0) RETURN
            IF(KSUP(6).GT.1.OR.KEY(2).EQ.2.OR.KEY(2).EQ.3) THEN
               CALL BUNPCK(NBPW,KSEC2(IW),KEY(12),IW,IB,26,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,KEY(13),IW,IB,25,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,IDUMMY  ,IW,IB, 5,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,KEY(14),IW,IB, 8,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,KEY(15),IW,IB,16,KERR)
               IF(KERR.GT.0) RETURN
            ELSE
               CALL BUNPKS(NBPW,KSEC2,KEY(16),IW,IB,8,0,9,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPKS(NBPW,KSEC2,IDUM(1)  ,IW,IB,8,0,8,KERR)
               IF(KERR.GT.0) RETURN
            END IF
         ELSE
            WRITE(KNTN,'(1H )')
            WRITE(KNTN,'(1H ,A)') 'BUUKEY : KEY DEFINITION NOT KNOWN'
            WRITE(KNTN,'(1H )')
         END IF
      END IF
C
C     CHECK IF IDENT IS RIGHT JUSTIFIED.
C
      IF(KEY(16).EQ.32) THEN
         J=15
         DO 102 I=16,24
         IF(KEY(I).EQ.32) GO TO 102
         J=J+1
         KEY(J)=KEY(I)
         KEY(I)=32
 102     CONTINUE
      END IF
C
      RETURN
      END
