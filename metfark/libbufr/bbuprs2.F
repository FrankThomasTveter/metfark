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

      SUBROUTINE BBUPRS2(KNT,KSUP,KEY)
C
C**** *BUPRS2*
C
C
C     PURPOSE.
C     --------
C           PRINT SECTION 2 OF BUFR MESSAGE (EXPANDED RDB KEY).
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRS2(KNT,KSUP,KEY)*
C
C        INPUT :
C               *KNT*   -  UNIT NUMBER FOR IO
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
C               *KEY*     -  ARRAY CONTAINING SECTION 2 INFORMATION
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
C
C
C
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
#     include "parameter.F"
C
      DIMENSION KSUP(JSUP),KEY(JKEY)
C
#ifndef R_4
      REAL*8 RLAT1
      REAL*8 RLON1
      REAL*8 RLAT2
      REAL*8 RLON2
#else
      REAL   RLAT1
      REAL   RLON1
      REAL   RLAT2
      REAL   RLON2
#endif
C
      CHARACTER*9 CIDENT
      CHARACTER*13 YFM
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 2.
C                ----------------
 100  CONTINUE
C
      YFM='(1H ,A,TR0,A)'
C
      IF(KSUP(2).LE.1) THEN
         WRITE(KNT) 'PRTKEY : RDB KEY NOT DEFINED IN SECTION 2.'
         RETURN
      END IF
C
      WRITE(KNT,'(1H1)')
C
      WRITE(KNT,'(1H ,A)')       '        BUFR SECTION 2    '
      WRITE(KNT,'(1H )')
      WRITE(KNT,'(1H ,A,I9)')    'LENGTH OF SECTION 2       ', KEY(1)
      WRITE(KNT,'(1H )')
      WRITE(KNT,'(1H ,A)')       '      REPORT DATA BASE KEY  '
      WRITE(KNT,'(1H )')
C
      IKTYPE=0
      IF(KEY(2).EQ.2) IKTYPE=2
      IF(KEY(2).EQ.3) IKTYPE=2
      IF(KEY(2).EQ.12)IKTYPE=2
      IF(KEY(2).EQ.08)IKTYPE=2
      IF(IKTYPE.EQ.0.AND.KSUP(6).GT.1) IKTYPE=2
C
      IF(IKTYPE.EQ.2) THEN
C      IF(KEY(2).EQ.2.OR.KEY(2).EQ.3.OR.KEY(2).EQ.12) THEN
C
         WRITE(KNT,'(1H ,A,I9)') 'RDB DATA TYPE             ', KEY(2)
         WRITE(KNT,'(1H ,A,I9)') 'RDB DATA SUBTYPE          ', KEY(3)
         WRITE(KNT,'(1H ,A,I9)') 'YEAR                      ', KEY(4)
         WRITE(KNT,'(1H ,A,I9)') 'MONTH                     ', KEY(5)
         WRITE(KNT,'(1H ,A,I9)') 'DAY                       ', KEY(6)
         WRITE(KNT,'(1H ,A,I9)') 'HOUR                      ', KEY(7)
         WRITE(KNT,'(1H ,A,I9)') 'MINUTE                    ', KEY(8)
         WRITE(KNT,'(1H ,A,I9)') 'SECOND                    ', KEY(9)
         RLAT1=(KEY(11)-9000000)/100000.
         RLON1=(KEY(10)-18000000)/100000.
         WRITE(KNT,'(1H ,A,F9.2)')'LATITUDE  1               ', RLAT1
         WRITE(KNT,'(1H ,A,F9.2)')'LONGITUDE 1               ', RLON1
         RLAT2=(KEY(13)-9000000)/100000.
         RLON2=(KEY(12)-18000000)/100000.
         WRITE(KNT,'(1H ,A,F9.2)')'LATITUDE  2               ', RLAT2
         WRITE(KNT,'(1H ,A,F9.2)')'LONGITUDE 2               ', RLON2
         WRITE(KNT,'(1H ,A,I9)') 'NUMBER OF OBSERVATIONS    ', KEY(14)
         WRITE(KNT,'(1H ,A,I9)') 'IDENTIFIER                ', KEY(15)
         WRITE(KNT,'(1H ,A,I9)') 'TOTAL BUFR MESSAGE LENGTH ', KEY(25)
         WRITE(KNT,'(1H ,A,I9)') 'DAY    (RDB INSERTION)    ', KEY(26)
         WRITE(KNT,'(1H ,A,I9)') 'HOUR   (RDB INSERTION)    ', KEY(27)
         WRITE(KNT,'(1H ,A,I9)') 'MINUTE( (RDB INSERTION)   ', KEY(28)
         WRITE(KNT,'(1H ,A,I9)') 'SECOND (RDB INSERTION)    ', KEY(29)
         WRITE(KNT,'(1H ,A,I9)') 'DAY    (MDB ARRIVAL)      ', KEY(30)
         WRITE(KNT,'(1H ,A,I9)') 'HOUR   (MDB ARRIVAL)      ', KEY(31)
         WRITE(KNT,'(1H ,A,I9)') 'MINUTE (MDB ARRIVAL)      ', KEY(32)
         WRITE(KNT,'(1H ,A,I9)') 'SECOND (MDB ARRIVAL       ', KEY(33)
         WRITE(KNT,'(1H ,A,I9)') 'CORRECTION NUMBER         ', KEY(34)
         WRITE(KNT,'(1H ,A,I9)') 'PART OF MESSAGE           ', KEY(35)
         WRITE(KNT,'(1H ,A,I9)') 'CORRECTION NUMBER         ', KEY(37)
         WRITE(KNT,'(1H ,A,I9)') 'PART OF MESSAGE           ', KEY(38)
         WRITE(KNT,'(1H ,A,I9)') 'CORRECTION NUMBER         ', KEY(40)
         WRITE(KNT,'(1H ,A,I9)') 'PART OF MESSAGE           ', KEY(41)
         WRITE(KNT,'(1H ,A,I9)') 'CORRECTION NUMBER         ', KEY(43)
         WRITE(KNT,'(1H ,A,I9)') 'PART OF MESSAGE           ', KEY(44)
         WRITE(KNT,'(1H ,A,I9)') 'QUALITY CONTROL % CONF    ', KEY(46)
      ELSE
         WRITE(KNT,'(1H ,A,I9)') 'RDB DATA TYPE             ', KEY(2)
         WRITE(KNT,'(1H ,A,I9)') 'RDB DATA SUBTYPE          ', KEY(3)
         WRITE(KNT,'(1H ,A,I9)') 'YEAR                      ', KEY(4)
         WRITE(KNT,'(1H ,A,I9)') 'MONTH                     ', KEY(5)
         WRITE(KNT,'(1H ,A,I9)') 'DAY                       ', KEY(6)
         WRITE(KNT,'(1H ,A,I9)') 'HOUR                      ', KEY(7)
         WRITE(KNT,'(1H ,A,I9)') 'MINUTE                    ', KEY(8)
         WRITE(KNT,'(1H ,A,I9)') 'SECOND                    ', KEY(9)
         RLAT1=(KEY(11)-9000000)/100000.
         RLON1=(KEY(10)-18000000)/100000.
         WRITE(KNT,'(1H ,A,F9.2)')'LATITUDE  1               ', RLAT1
         WRITE(KNT,'(1H ,A,F9.2)')'LONGITUDE 1               ', RLON1
         IDD=0
         CIDENT=' '
         DO 201 ID=16,24
         IDD=IDD+1
         CIDENT(IDD:IDD)=CHAR(KEY(ID))
 201     CONTINUE
         IDD=INDEX(CIDENT,' ')
         IF(IDD.EQ.0) IDD=10
         IDD=10-IDD
         WRITE(YFM(10:10),'(I1)',ERR=202) IDD
         GO TO 203
 202     YFM(10:10)='9'
 203     WRITE(KNT,FMT=YFM)      'IDENTIFER                 ', CIDENT
         WRITE(KNT,'(1H ,A,I9)') 'TOTAL BUFR MESSAGE LENGTH ', KEY(25)
         WRITE(KNT,'(1H ,A,I9)') 'DAY    (RDB INSERTION)    ', KEY(26)
         WRITE(KNT,'(1H ,A,I9)') 'HOUR   (RDB INSERTION)    ', KEY(27)
         WRITE(KNT,'(1H ,A,I9)') 'MINUTE (RDB INSERTION)    ', KEY(28)
         WRITE(KNT,'(1H ,A,I9)') 'SECOND (RDB INSERTION)    ', KEY(29)
         WRITE(KNT,'(1H ,A,I9)') 'DAY    (MDB ARRIVAL)      ', KEY(30)
         WRITE(KNT,'(1H ,A,I9)') 'HOUR   (MDB ARRIVAL)      ', KEY(31)
         WRITE(KNT,'(1H ,A,I9)') 'MINUTE (MDB ARRIVAL)      ', KEY(32)
         WRITE(KNT,'(1H ,A,I9)') 'SECOND (MDB ARRIVAL       ', KEY(33)
         WRITE(KNT,'(1H ,A,I9)') 'CORRECTION NUMBER         ', KEY(34)
         WRITE(KNT,'(1H ,A,I9)') 'PART OF MESSAGE           ', KEY(35)
         WRITE(KNT,'(1H ,A,I9)') 'CORRECTION NUMBER         ', KEY(37)
         WRITE(KNT,'(1H ,A,I9)') 'PART OF MESSAGE           ', KEY(38)
         WRITE(KNT,'(1H ,A,I9)') 'CORRECTION NUMBER         ', KEY(40)
         WRITE(KNT,'(1H ,A,I9)') 'PART OF MESSAGE           ', KEY(41)
         WRITE(KNT,'(1H ,A,I9)') 'CORRECTION NUMBER         ', KEY(43)
         WRITE(KNT,'(1H ,A,I9)') 'PART OF MESSAGE           ', KEY(44)
         WRITE(KNT,'(1H ,A,I9)') 'QUALITY CONTROL % CONF    ', KEY(46)
      END IF
C
      RETURN
      END
