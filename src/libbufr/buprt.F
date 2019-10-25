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

      SUBROUTINE BUPRT(K,KSUB1,KSUB2,KELEM,CNAMES,CUNITS,
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
C           *CALL* *BUPRT(K,KSUB1,KSUB2,KELEM,CNAMES,CUNITS,
C                         CVALS,KVALS,VALUES,KSUP,KSEC1,KERR)*
C
C        INPUT :
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
#     include "bcomunit.F"
#     include "bcomwork.F"
#     include "bcomroot.F"
C
      DIMENSION KTDLST(JELEM)
      DIMENSION KTDEXP(JELEM)
C
      CHARACTER*256 YCODE
      CHARACTER*64 YFLAG(32)
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER YCHAR*30,YLONG*320
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
      REAL*8 VALUES(KVALS)
#else
      REAL   RVIND
      REAL   EPS
      REAL   VALUES(KVALS)
#endif 
C
      DIMENSION KSUP(JSUP),KSEC1(JSEC1)
C
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT BUFR MESSAGE.
C                -------------------
 100  CONTINUE
C
      KERR=0
      YCODE=' '
C
      ISUB1=KSUB1
      ISUB2=KSUB2
      IF(ISUB1.LE.0.OR.ISUB2.LE.0) THEN
         WRITE(KNTN,'(A)')    ' WARNING - NEGATIVE KSUB1 OR KSUB2.'
         WRITE(KNTN,'(A,I5)') ' WARNING - NUMBER OF SUBSETS IS ',KSUP(6)
         RETURN
      END IF
      IF(ISUB1.GT.KSUP(6)) THEN
         WRITE(KNTN,'(A,I5)') ' WARNING - NUMBER OF SUBSETS IS ',KSUP(6)
         RETURN
      END IF
      IF(ISUB2.GT.KSUP(6)) THEN
         ISUB2=KSUP(6)
         WRITE(KNTN,'(A,I5)') ' WARNING - KSUB2 REPLACED BY ',KSUP(6)
      END IF
C
      IF(.NOT. OCTABLE) K=0
C
      IF(K.EQ.0) THEN
C
            DO 103 JB=ISUB1,ISUB2
            CALL BUSEL2(JB,KELEM,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,
     1                  CUNITS,KERR)
C
            ILN=0
            WRITE(KNTN,'(1H )')
C
            DO 104 JA=1,KTDEXL
C
            ILN=ILN+1
            JAJB=JA+(JB-1)*KELEM
C
            IF(ABS(VALUES(JAJB)-RVIND)/RVIND.LT.EPS) THEN
               WRITE(KNTN,9918) ILN,CNAMES(JA),CUNITS(JA)
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
                  WRITE(KNTN,9919)ILN,CNAMES(JA),VALUES(JAJB),
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
                     WRITE(KNTN,9920) YCHAR
 130                 CONTINUE
C
                   END IF
               ELSE
                  WRITE(KNTN,9917) ILN,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA)
               END IF
            END IF
C
 104        CONTINUE
 103        CONTINUE
C
      END IF
C
      IF(K.EQ.1) THEN
C
C---------------------------------------------------------------------
          WRITE(KNTN,'(1H1)')
C
C---------------------------------------------------------------------
C
          DO 150 JB=ISUB1,ISUB2
          CALL BUSEL2(JB,KELEM,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,
     1                  CUNITS,KERR)
C
          ILN=0
          WRITE(KNTN,'(1H )')

C
          DO 160 JA=1,KTDEXL
C
          ILN=ILN+1
C
          JAJB=JA+(JB-1)*KELEM
C
          IF(ABS(VALUES(JAJB)-RVIND)/RVIND.LT.EPS) THEN
             WRITE(KNTN,9918) ILN,CNAMES(JA),CUNITS(JA)
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
                  DO 1125 JC=1,NW-1
                  II=II+1
                  KF=JC*80+1
                  KL=(JC+1)*80
                  YLONG(KF:KL)=CVALS(II)
1125              CONTINUE
C
                  NLINE=NCHAR/30
                  IDIF =NCHAR-NLINE*30
                  IF(IDIF.NE.0) NLINE=NLINE+1
                  YCHAR=' '
                  YCHAR=YLONG(1:30)
C
                  WRITE(KNTN,9919)ILN,CNAMES(JA),VALUES(JAJB),
     1                         CUNITS(JA),YCHAR
C
                  IF(NLINE.GT.1) THEN
                     DO 1130 JJ=1,NLINE-1
C
                     K2=JJ*30+1
                     K1=(JJ+1)*30
                     YCHAR=' '
                     YCHAR=YLONG(K2:K1)
C
                     WRITE(KNTN,9920) YCHAR
1130                 CONTINUE
C
                  END IF

             ELSEIF(CUNITS(JA)(1:4).EQ.'CODE') THEN
                 KCODE=NINT(VALUES(JAJB))
                 CALL GETCODE(KTDEXP(JA),KCODE,YCODE,IERR)
                 DO IZ=256,1,-1
                 IF(YCODE(IZ:IZ).NE.' ') THEN
                    IZZ=IZ
                    GO TO 161
                 END IF
                 END DO
 161             CONTINUE
                 IKK=IZZ/64+1
                 WRITE(KNTN,9919) ILN,CNAMES(JA),VALUES(JAJB),
     1           CUNITS(JA), YCODE(1:64)

                 IF(IKK.GT.1) THEN
                 IIII=65
                 DO IK=2,IKK
                 WRITE(KNTN,9920) YCODE(iiii:iiii+64-1)
                 IIII=IIII+64
                 END DO
                 END IF
             ELSEIF(CUNITS(JA)(1:4).EQ.'FLAG') THEN
                 KCODE=NINT(VALUES(JAJB))
                 CALL GETFLAG(KTDEXP(JA),KCODE,KFLAG,YFLAG,IERR)
                 WRITE(KNTN,9919) ILN,CNAMES(JA),VALUES(JAJB),
     1           CUNITS(JA), YFLAG(1)
                 IF(KFLAG.GT.1) THEN
                 DO IZ=2,KFLAG
                 WRITE(KNTN,9920) YFLAG(IZ)
                 END DO
                 END IF
             ELSE
                 WRITE(KNTN,9917) ILN,CNAMES(JA),VALUES(JAJB),
     1           CUNITS(JA)
             END IF
          END IF
 
  160     CONTINUE
  150     CONTINUE
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
 9917 FORMAT(1H ,I6,1X,A32,1X,E23.14E3,1X,A24)
 9918 FORMAT(1H ,I6,1X,A32,1X,'                MISSING',1X,A24)
 9919 FORMAT(1H ,I6,1X,A32,1X,E23.14E3,1X,A24,1X,A)
 9920 FORMAT(1H ,89X,A)
      END
