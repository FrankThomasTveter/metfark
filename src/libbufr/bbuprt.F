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

      SUBROUTINE BBUPRT(KNT,K,KSUB1,KSUB2,KELEM,CNAMES,CUNITS,
     1                 CVALS,KVALS,VALUES,KSUP,KSEC1,KERR)
C
C**** *BUPRT*
C
C
C     PURPOSE.
C     --------
C           PRINT EXPANDED BUFR MESSAG.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRT(KNT,K,KSUB1,KSUB2,KELEM,CNAMES,CUNITS,
C                         CVALS,KVALS,VALUES,KSUP,KSEC1,KERR)*
C
C        INPUT :
C               *KNT*     -  UNIT NUMBER FOR IO
C               *K*       -  SWITCH TO PRINT WITH/WITOUT CONTENT OF CODE TABLES
C                            0  - NO  CODE TABLE CONTENT
C                            1  - YES CODE TABLE CONTENT
C               *KSUB1*   -  STARTING SUBSET
C               *KSUB2*   -  ENDING SUBSET
C               *KELEM*   -  DIMENSION OF CNAMES, CUNITS ARRAY
C               *CNAMES*  -  CHARACTER ARRAY CONTAINING ELEMENT NAMES
C               *CUNITS*  -  CHARACTER ARRAY CONTAINIG UNITS
C               *CVALS*   -  CHARACTER ARRAY CONTAINING BUFR CODE TABLE
C                            ENTRIES
C               *KVALS*   -  DIMENSION OF VALUES ARRAY
C               *VALUES*  -  REAL ARRAY (EXPANDED DATA VALUES)
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
C        OUTPUT:
C               *KERR*    -  RETURNED ERROR CODE
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
#     include "bcomwork.F"
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER YCHAR*30,YLONG*320
C
      DIMENSION KSUP(JSUP),KSEC1(JSEC1)
#ifndef R_4
      REAL*8 VALUES(KVALS)
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   VALUES(KVALS)
      REAL   RVIND
      REAL   EPS
#endif
C
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT BUFR MESSAGE.
C                -------------------
 100  CONTINUE
C
      KERR=0
C
      ISUB1=KSUB1
      ISUB2=KSUB2
      IF(ISUB1.LE.0.OR.ISUB2.LE.0) THEN
         WRITE(KNT,'(A)')    ' WARNING - NEGATIVE KSUB1 OR KSUB2.'
         WRITE(KNT,'(A,I5)') ' WARNING - NUMBER OF SUBSETS IS ',KSUP(6)
         RETURN
      END IF
      IF(ISUB1.GT.KSUP(6)) THEN
         WRITE(KNT,'(A,I5)') ' WARNING - NUMBER OF SUBSETS IS ',KSUP(6)
         RETURN
      END IF
      IF(ISUB2.GT.KSUP(6)) THEN
         ISUB2=KSUP(6)
         WRITE(KNT,'(A,I5)') ' WARNING - KSUB2 REPLACED BY ',KSUP(6)
      END IF
C
      JQCP1= 0
C
      IF(K.EQ.0) THEN
         JQPR=0
         JQUA=0
         JQC=0
C          DO 171 J171=1,KSUP(5)
C          IF(CNAMES(J171)(1:8).EQ.'DATA PRE') THEN
C            JQPR=J171
C            GO TO 172
C          END IF
C  171     CONTINUE
C  172     DO 173 J173=1,KSUP(5)
C          IF(CNAMES(J173)(1:9).EQ.'QUALITY I') JQUA=J173
C  173     CONTINUE
C          DO 174 J174=1,KSUP(5)
C          IF(CNAMES(J174)(1:3).EQ.'% C') THEN
C            JQC =J174
C            GO TO 175
C          END IF
C  174     CONTINUE
C 
C  175     CONTINUE
C
C         WRITE(KNT,'(1H1)')
C
C         WRITE(KNT,'(1H ,A)')    'EXPANDED BUFR MESSAGE  '
C         WRITE(KNT,'(1H ,//)')
C         WRITE(KNT,'(1H ,A,I6)') 'BUFR MESSAGE  DATA TYPE   ',KSEC1(6)
C         WRITE(KNT,'(1H ,A,I6)') 'RDB DATA SUBTYPE          ',KSEC1(7)
C         WRITE(KNT,'(1H ,A,I6)') 'TOTAL BUFR LENGTH (BYTES) ',KSUP(8)
C
         NTYPE=KSEC1(7)
         IF(JQUA.EQ.0) THEN      !IF(JQUA.NE.0) THEN
            JQUA=KSUP(5)
C
            DO 103 JB=ISUB1,ISUB2
C
            ILN=0
            WRITE(KNT,'(1H )')
C
            DO 104 JA=1,JQUA
C
            ILN=ILN+1
            JAJB=JA+(JB-1)*KELEM
C
            IF(ABS(VALUES(JAJB)-RVIND)/RVIND.LT.EPS) THEN
               WRITE(KNT,9918) ILN,CNAMES(JA),CUNITS(JA)
            ELSE
               IF(CUNITS(JA)(1:4).EQ.'CCIT') THEN
                  I=NINT(VALUES(JAJB)/1000)
                  NCHAR=VALUES(JAJB)-I*1000
                  NW=NCHAR/80
                  NWOFF=NCHAR-NW*80
                  IF(NWOFF.NE.0) NW=NW+1
C
                  YLONG=' '
                  YLONG(1:80)=CVALS(I)
C
                  II=I
                  DO 125 JC=1,NW-1
                  II=II+1
                  KF=JC*80+1
                  KL=(JC+1)*80
                  YLONG(KF:KL)=CVALS(II)
 125              CONTINUE
C
                  NLINE=NCHAR/30
                  IDIF =NCHAR-NLINE*30
                  IF(IDIF.NE.0) NLINE=NLINE+1
                  YCHAR=' '
                  YCHAR=YLONG(1:30)
C
                  WRITE(KNT,9919)ILN,CNAMES(JA),VALUES(JAJB),
     1                         CUNITS(JA),YCHAR
C
                  IF(NLINE.GT.1) THEN
                     DO 130 JJ=1,NLINE-1
C
                     K2=JJ*30+1
                     K1=(JJ+1)*30
                     YCHAR=' '
                     YCHAR=YLONG(K2:K1)
C
                     WRITE(KNT,9920) YCHAR
 130                 CONTINUE
C
                   END IF
               ELSE
                  WRITE(KNT,9917) ILN,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA)
               END IF
            END IF
C
 104        CONTINUE
 103        CONTINUE
C
         ELSE
            JQPRM1=JQPR-1
            JQC=JQC-1
C
            DO 101 JB=ISUB1,ISUB2
C
            ILN=0
            JQCP1=0
C
            WRITE(KNT,'(1H )')
C
            DO 102 JA=1,JQUA-1
C
            ILN=ILN+1
            JAJB=JA+(JB-1)*KELEM
            JQPJB=JQPRM1+JA+(JB-1)*KELEM
C
            IF(VALUES(JQPJB).EQ.0.0) THEN
               JQCP1=JQCP1+1
               JQCPP1=JQC+JQCP1+(JB-1)*KELEM
               IF(ABS(VALUES(JAJB)-RVIND)/RVIND.LT.EPS) THEN
                  WRITE(KNT,9918) ILN,CNAMES(JA),CUNITS(JA)
               ELSE
                  WRITE(KNT,9916) ILN,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA),
     1            CNAMES(JQC+JQCP1),VALUES(JQCPP1),
     1            CUNITS(JQC+JQCP1)
               END IF
            ELSE
               IF(ABS(VALUES(JAJB)-RVIND)/RVIND.LT.EPS) THEN
                  WRITE(KNT,9918) ILN,CNAMES(JA),CUNITS(JA)
                  IF(NTYPE.EQ.5.OR.NTYPE.EQ.3) JQCP1=JQCP1+1
               ELSE
                  WRITE(KNT,9917) ILN,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA)
                  IF(NTYPE.EQ.5.OR.NTYPE.EQ.3) JQCP1=JQCP1+1
               END IF
            END IF
C
C
 102        CONTINUE
 101        CONTINUE
C
         END IF
      END IF
C
      IF(K.EQ.1) THEN
C
C---------------------------------------------------------------------
          WRITE(KNT,'(1H1)')
C
          WRITE(KNT,'(1H ,A)') 'WARNING : PRINTING CONTENT OF CODE'//
     1   ' TABLES NOT YET IMPLEMENTED.'
          RETURN
C---------------------------------------------------------------------
C
C          WRITE(KNT,'(1H ,A)')    'EXPANDED BUFR MESSAGE  '
C          WRITE(KNT,'(1H ,//)')
C          WRITE(KNT,'(1H ,A,I6)') 'RDB DATA TYPE             ',KSEC1(6)
C          WRITE(KNT,'(1H ,A,I6)') 'RDB DATA SUBTYPE          ',KSEC1(7)
C          WRITE(KNT,'(1H ,A,I6)') 'TOTAL BUFR LENGTH (BYTES) ',KSUP(8)
C
C          DO 150 JB=1,KSUP(6)
C
C          WRITE(KNT,'(1H )')
C
C          DO 160 JA=1,KSUP(5)
C
C          JAJB=JA+(JB-1)*KELEM
C
C          IF(ABS(VALUES(JAJB)-RVIND)/RVIND.LT.EPS) THEN
C             WRITE(KNT,9903) CNAMES(JA)(1:32),CUNITS(JA)
C             WRITE(KNT,9903) CNAMES(JA)(33:64)
C          ELSE
C             IF(CUNITS(JA)(1:10).EQ.'CODE TABLE'.OR.
C     1         CUNITS(JA)(1:9) .EQ.'CCITTIA5'     ) THEN
C                I=NINT(VALUES(JAJB)/1000)
C                NCHAR=VALUES(JAJB)-I*1000
C                NW=NCHAR/80
C                NWOFF=NCHAR-NW*80
C                IF(NWOFF.NE.0) NW=NW+1
CC
C                YLONG(1:80)=CVALS(I)
C
C                II=I
C                DO 165 JC=1,NW-1
C                II=II+1
C                KF=JC*80+1
C                KL=(JC+1)*80
C                YLONG(KF:KL)=CVALS(II)
C  165           CONTINUE
C
C                NLINE=NCHAR/30
C                IDIF =NCHAR-NLINE*30
C                IF(IDIF.NE.0) NLINE=NLINE+1
C                YCHAR=YLONG(1:30)
C
C                WRITE(KNT,9904)CNAMES(JA)(1:32),VALUES(JAJB),
C     1         CUNITS(JA),YCHAR
C                WRITE(KNT,9904)CNAMES(JA)(33:64)
C
C                IF(NLINE.GT.1) THEN
C                   DO 170 JJ=1,NLINE-1
C
C                   K2=JJ*30+1
C                   K1=(JJ+1)*30
C                   YCHAR=YLONG(K2:K1)
C
C                   WRITE(KNT,9905) YCHAR
C  170              CONTINUE
C
C                END IF
C             ELSE
C                WRITE(KNT,9906) CNAMES(JA)(1:32),VALUES(JAJB),
C     1                       CUNITS(JA)
C                WRITE(KNT,9906) CNAMES(JA)(33:64)
C             END IF
C          END IF
C
C  160     CONTINUE
C  150     CONTINUE
C
        END IF
C
C
C       RETURN
C
C     ------------------------------------------------------------------
C
 200  CONTINUE
C
C     ------------------------------------------------------------------
 9903 FORMAT(1H ,A,'     MISSING',2X,A)
 9904 FORMAT(1H ,A,E20.10,2X,A,2X,A)
 9905 FORMAT(1H ,100X,A)
 9906 FORMAT(1H ,I5,1X,A,E14.10,2X,A)
 9916 FORMAT(1H ,I5,1X,A15,1X,E20.10,1X,A20,1X,A15,1X,F3.0,1X,A15)
 9917 FORMAT(1H ,I5,1X,A15,1X,E20.10,1X,A24)
 9918 FORMAT(1H ,I5,1X,A15,1X,'             MISSING',1X,A24)
 9919 FORMAT(1H ,I5,1X,A15,1X,E20.10,1X,A24,1X,A)
 9920 FORMAT(1H ,62X,A)
      END
