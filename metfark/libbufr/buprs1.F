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

      SUBROUTINE BUPRS1(KSEC1)
C
C**** *BUPRS1*
C
C
C     PURPOSE.
C     --------
C           PRINT SECTION 1 OF BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRS1(KSEC1)*
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
C                                        INFORMATION(BYTE BY BYTE)
C
C                            FOR BUFR EDITION >= 3
C
C                            KSEC1(16) - ORIGINATING SUB-CENTRE
C                            KSEC1(18) - KSEC1(JSEC1) -- LOCAL ADP CENTRE
C                                        INFORMATION(BYTE BY BYTE)
C                            
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
#     include "bcomunit.F"
C
      DIMENSION KSEC1(JSEC1)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 1.
C                ----------------
 100  CONTINUE
C
      WRITE(KNTN,'(1H1)')
C
      IF(KSEC1( 2).LE.3) THEN
      WRITE(KNTN,'(1H ,A)')    '        BUFR SECTION 1    '
      WRITE(KNTN,'(1H )')
      WRITE(KNTN,'(1H ,A,I5)')'LENGTH OF SECTION 1 (BYTES)   ',KSEC1( 1)
      WRITE(KNTN,'(1H ,A,I5)')'BUFR EDITION NUMBER           ',KSEC1( 2)
      IF(KSEC1(2).GE.3) THEN
      WRITE(KNTN,'(1H ,A,I5)')'ORIGINATING SUB-CENTRE        ',KSEC1(16)
      END IF
      WRITE(KNTN,'(1H ,A,I5)')'ORIGINATING CENTRE            ',KSEC1( 3)
      WRITE(KNTN,'(1H ,A,I5)')'UPDATE SEQUENCE NUMBER        ',KSEC1( 4)
      WRITE(KNTN,'(1H ,A,I5)')'FLAG (PRESENCE OF SECTION 2)  ',KSEC1( 5)
      WRITE(KNTN,'(1H ,A,I5)')'BUFR MESSAGE TYPE             ',KSEC1( 6)
      WRITE(KNTN,'(1H ,A,I5)')'BUFR MESSAGE SUBTYPE          ',KSEC1( 7)
      WRITE(KNTN,'(1H ,A,I5)')'VERSION NUMBER OF LOCAL TABLE ',KSEC1( 8)
      WRITE(KNTN,'(1H ,A,I5)')'YEAR                          ',KSEC1( 9)
      WRITE(KNTN,'(1H ,A,I5)')'MONTH                         ',KSEC1(10)
      WRITE(KNTN,'(1H ,A,I5)')'DAY                           ',KSEC1(11)
      WRITE(KNTN,'(1H ,A,I5)')'HOUR                          ',KSEC1(12)
      WRITE(KNTN,'(1H ,A,I5)')'MINUTE                        ',KSEC1(13)
      WRITE(KNTN,'(1H ,A,I5)')'VERSION NUMBER OF MASTER TABLE',KSEC1(15)
      WRITE(KNTN,'(1H ,A,I5)')'BUFR MASTER TABLE             ',KSEC1(14)

      ELSEIF(KSEC1( 2).EQ.4) THEN

      WRITE(KNTN,'(1H ,A)')    '        BUFR SECTION 1    '
      WRITE(KNTN,'(1H )')
      WRITE(KNTN,'(1H ,A,I9)')'LENGTH OF SECTION 1 (BYTES)   ',KSEC1( 1)
      WRITE(KNTN,'(1H ,A,I9)')'BUFR MASTER TABLE             ',KSEC1(14)
      WRITE(KNTN,'(1H ,A,I9)')'ORIGINATING CENTRE            ',KSEC1( 3)
      WRITE(KNTN,'(1H ,A,I9)')'ORIGINATING SUB-CENTRE        ',KSEC1(16)
      WRITE(KNTN,'(1H ,A,I9)')'UPDATE SEQUENCE NUMBER        ',KSEC1( 4)
      WRITE(KNTN,'(1H ,A,I9)')'FLAG (PRESENCE OF SECTION 2)  ',KSEC1( 5)
      WRITE(KNTN,'(1H ,A,I9)')'DATA CATEGORY                 ',KSEC1( 6)
      WRITE(KNTN,'(1H ,A,I9)')'DATA SUB-CATEGORY             ',KSEC1(17)
      WRITE(KNTN,'(1H ,A,I9)')'LOCAL DATA SUB-CATEGORY       ',KSEC1( 7)
      WRITE(KNTN,'(1H ,A,I9)')'VERSION NUMBER OF MASTER TABLE',KSEC1(15)
      WRITE(KNTN,'(1H ,A,I9)')'VERSION NUMBER OF LOCAL TABLE ',KSEC1( 8)
      WRITE(KNTN,'(1H ,A,I9)')'YEAR                          ',KSEC1( 9)
      WRITE(KNTN,'(1H ,A,I9)')'MONTH                         ',KSEC1(10)
      WRITE(KNTN,'(1H ,A,I9)')'DAY                           ',KSEC1(11)
      WRITE(KNTN,'(1H ,A,I9)')'HOUR                          ',KSEC1(12)
      WRITE(KNTN,'(1H ,A,I9)')'MINUTE                        ',KSEC1(13)
      WRITE(KNTN,'(1H ,A,I9)')'SECOND                        ',KSEC1(18)
c     WRITE(KNTN,'(1H ,A,I9)')'YEAR    (EARLIEST TIME)       ',KSEC1(19)
c     WRITE(KNTN,'(1H ,A,I9)')'MONTH   (EARLIEST TIME)       ',KSEC1(20)
c     WRITE(KNTN,'(1H ,A,I9)')'DAY     (EARLIEST TIME)       ',KSEC1(21)
c     WRITE(KNTN,'(1H ,A,I9)')'HOUR    (EARLIEST TIME)       ',KSEC1(22)
c     WRITE(KNTN,'(1H ,A,I9)')'MINUTE  (EARLIEST TIME)       ',KSEC1(23)
c     WRITE(KNTN,'(1H ,A,I9)')'SECOND  (EARLIEST TIME)       ',KSEC1(28)
c     WRITE(KNTN,'(1H ,A,I9)')'YEAR    (LATEST TIME)         ',KSEC1(25)
c     WRITE(KNTN,'(1H ,A,I9)')'MONTH   (LATEST TIME)         ',KSEC1(26)
c     WRITE(KNTN,'(1H ,A,I9)')'DAY     (LATEST TIME)         ',KSEC1(27)
c     WRITE(KNTN,'(1H ,A,I9)')'HOUR    (LATEST TIME)         ',KSEC1(28)
c     WRITE(KNTN,'(1H ,A,I9)')'MINUTE  (LATEST TIME)         ',KSEC1(29)
c     WRITE(KNTN,'(1H ,A,I9)')'SECOND  (LATEST TIME)         ',KSEC1(30)
c     WRITE(KNTN,'(1H ,A,I9)')'MOST SOUTHERN LATITUDE        ',KSEC1(31)
c     WRITE(KNTN,'(1H ,A,I9)')'MOST WESTERN LONGITUDE        ',KSEC1(32)
c     WRITE(KNTN,'(1H ,A,I9)')'MOST NORTHERN LATITUDE        ',KSEC1(33)
c     WRITE(KNTN,'(1H ,A,I9)')'MOST EASTERN LONGITUDE        ',KSEC1(34)

      END IF

C
      RETURN
      END
