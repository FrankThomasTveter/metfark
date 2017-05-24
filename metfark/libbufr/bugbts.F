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

      SUBROUTINE BUGBTS( KSEC1,KERR )
C
C**** *BUGBTS*
C
C
C     PURPOSE.
C     --------
C          LOAD BUFR TABLE B, D AND C ACCORDING TO EDITION AND VERSION
C     OF BUFR CODE.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUGBTS(KSEC1,KERR)*
C
C        OUTPUT:
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
C          NONE.
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
#     include "bcomtab.F"
#     include "bcomtabc.F"
#     include "bcomct.F"
#     include "bcomctc.F"
#     include "bcomroot.F"
#     include "bcomtabload.F"
#     include "bcomtabloadc.F"
C
      CHARACTER*256 YTAB ,YTAC ,YTAD
      CHARACTER*24 YTABB,YTABC,YTABD, CTABLE_LIST
      CHARACTER*64 CTABBEN
      CHARACTER*24 CTABBU
      CHARACTER*64 CCTABBEN
      CHARACTER*24 CCTABBU
      CHARACTER*256 CROOT
      CHARACTER*64 CTEXT
C
      LOGICAL*4  OPN
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
C
      DIMENSION KSEC1(JSEC1)
C
      SAVE OFIRST
C
C     ------------------------------------------------------------------
C
C*          1.   GET BUFR TABLES/LOCAL BUFR TABLES.
C                ----------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0) RETURN
C
C*          2.   SET UP BUFR TABLE FILE NAME.
C                ----------------------------
 200  CONTINUE
C
C
C             BUFR EDITION 2 NAMING CONVENTION
C
C             BXXXXXYYZZ , CXXXXXYYZZ , DXXXXXYYZZ
C
C             B      - BUFR TABLE 'B'
C             C      - BUFR TABLE 'C'
C             D      - BUFR TABLE 'D'
C             XXXXX  - ORIGINATING CENTRE
C             YY     - VERSION NUMBER OF MASTER TABLE
C                      USED( CURRENTLY 2 )
C             ZZ     - VERSION NUMBER OF LOCAL TABLE USED
C
C             BUFR EDITION 3 NAMING CONVENTION
C
C             BWWWXXXYYZZ , CWWWXXXYYZZ , DWWWXXXYYZZ
C
C             B      - BUFR TABLE 'B'
C             C      - BUFR TABLE 'C'
C             D      - BUFR TABLE 'D'
C             WWW    - ORIGINATING SUB-CENTRE
C             XXX    - ORIGINATING CENTRE
C             YY     - VERSION NUMBER OF MASTER TABLE
C                      USED( CURRENTLY 2 )
C             ZZ     - VERSION NUMBER OF LOCAL TABLE USED
C
C
C             BUFR EDITION 4 NAMING CONVENTION
C
C             BSSSWWWWWXXXXXYYYZZZ , CSSSWWWWWXXXXXYYYZZZ , DSSSWWWWWXXXXXYYYZZZ
C
C             B      - BUFR TABLE 'B'
C             C      - BUFR TABLE 'C'
C             D      - BUFR TABLE 'D'
C             SSS    - MASTER TABLE NUMBER ( zero for WMO tables)
C             WWWWWW - ORIGINATING SUB-CENTRE
C             XXXXXX - ORIGINATING CENTRE
C             YYY    - VERSION NUMBER OF MASTER
C                      TABLE USED( CURRENTLY 12 )
C             ZZZ    - VERSION NUMBER OF LOCAL TABLE USED
C
            IXX=KSEC1(3)
            IYY=KSEC1(15)
            IZZ=KSEC1(08)
C           
            IF(KSEC1(2).EQ.3) THEN
               IWW=KSEC1(16)
               ISS=KSEC1(14)
            ELSEIF(KSEC1(2).EQ.4) THEN
               IWW=KSEC1(16)
               ISS=KSEC1(14)
            ELSE
               IWW=0
               ISS=0
            END IF
C
C        IF STANDARD TABLES USED, USE WMO ORIGINATING CENTRE ID
C
         IF(KSEC1(8).EQ.0.OR.KSEC1(8).EQ.255) THEN
            IXX=0
            IWW=0
            IZZ=0
         ENDIF
C
         IF(OFIRST) THEN
            IF(IWW.EQ.NWWP.AND.IXX.EQ.NXXP.AND.IYY.EQ.NYYP.AND.
     1         IZZ.EQ.NZZP.AND.ISS.EQ.NSSP) RETURN
         END IF
C
         OFIRST=.TRUE.
C
         NSSP=ISS
         NXXP=IXX
         NYYP=IYY
         NZZP=IZZ
         NWWP=IWW
C
       WRITE(YTABB,'(A1,I3.3,2(I5.5),2(I3.3),A4)')
     1             'B',ISS,IWW,IXX,IYY,IZZ,'.TXT'
       WRITE(YTABC,'(A1,I3.3,2(I5.5),2(I3.3),A4)')
     1             'C',ISS,IWW,IXX,IYY,IZZ,'.TXT'
       WRITE(YTABD,'(A1,I3.3,2(I5.5),2(I3.3),A4)')
     1             'D',ISS,IWW,IXX,IYY,IZZ,'.TXT'
C
C
C      Find if table is already used
C
       DO I=1,JTMAX
         IF(CTABLE_LIST(I).EQ.YTABB) THEN
            CALL GET_TABLES(I)
            RETURN
         END IF
       END DO
       IF(OPRINT) THEN
          IF(OCTABLE) THEN
             WRITE(KNTN,'(A,1X,A,A,A,A,A)')'BUFR TABLES TO BE LOADED ',
     1       YTABB,',',YTABC,',',YTABD
          ELSE
             WRITE(KNTN,'(A,1X,A,A,A)') 'BUFR TABLES TO BE LOADED ',
     1       YTABB,',',YTABD
          END IF

       END IF
C     ----------------------------------------------------------------
C*          3. OPEN AND READ FILES CONTAINING BUFR TABLES.
C              -------------------------------------------
 300  CONTINUE
C
      I=INDEX(CROOT,' ')
      IF(I.NE.0) I=I-1
C
C*          3.1 READ BUFR TABLE B.
C               ------------------
 310  CONTINUE
C
      YTAB=CROOT(1:I)//YTABB
C
      CALL BTABLE(YTAB,YTABB,KERR)
      IF(KERR.NE.0) RETURN
C
C
C*          3.2 READ BUFR TABLE C.
C               ------------------
 320  CONTINUE
C
       IF(OCTABLE) THEN
          YTAC=CROOT(1:I)//YTABC
C
          CALL CTABLE(YTAC,KERR)
          IF(KERR.NE.0) RETURN
       END IF
C
C
C*          3.3 READ BUFR TABLE D.
C               ------------------
 330  CONTINUE
C
      YTAD=CROOT(1:I)//YTABD
C
      CALL DTABLE(YTAD,KERR)
      IF(KERR.NE.0) RETURN
C
      RETURN
C
      END
