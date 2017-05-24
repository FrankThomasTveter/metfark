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

      SUBROUTINE BUBOX(KSUB,KSUP,KELEM,KWTR,CNAMES,CUNITS,KVALS,VALUES,
     1                 KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU,KERR)
C
C**** *BUBOX*
C
C
C     PURPOSE.
C     --------
C
C
C
C**   INTERFACE.
C     ----------
C
C               *CALL BUBOX(KSUB,KSUP,KELEM,KWTR,CNAMES,CUNITS,KVALS,VALUES,
C                           KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU,KERR)*
C
C        INPUT :
C               *KSUB*    -  SUBSET NUMBER
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
C               *KELEM*   -  EXPECTED NUMBER OF EXPANDED TABLE B ELEMENTS
C               *KWTR*    -  ARRAY CONTAINING BUFR TABLE B REFERENCE NUMBERS
C               *CNAMES*  -  ARRAY CONTAINING ELEMENT NAMES
C               *CUNITS*  -  ARRAY CONTAINING ELEMENT UNITS
C               *KVALS*   -  DIMENSION OF VALUES ARRAY
C               *VALUES*  -  ARRAY CONTAINING UNPACKED VALUES
C               
C        OUTPUT :
C
C               *KBOX*    -  NUMBER OF ROWS      
C               *KAPP*    -  NUMBER OF COLUMNS
C               *KLEN*    -  OFFSET FOR START OF NEXT COLUMN
C               *KBOXR*   -  ARRAY CONTAINING BUFR TABLE B REFERENCE NUMBERS
C               *VALS*    -  ARRAY CONTAINING UNPACKED VALUES
C               *CBOXN*   -  ARRAY CONTAINING ELEMENT NAMES
C               *CBOXU*   -  ARRAY CONTAINING ELEMENT UNITS
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C     -------
C
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
C          M. DRAGOSAVAC    *ECMWF*       01/02/94.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
#     include "parameter.F"
C
#     include "bcomunit.F"
C
      CHARACTER*64 CNAMES(*)
      CHARACTER*24 CUNITS(*)
C
C
      DIMENSION KSUP(9),KWTR(*)
C
      DIMENSION IOPER(100),RPRINT(60)
C
      DIMENSION KBOXR(JWORK)
      DIMENSION IBVAL(JELEM),IBPRINT(60)
      CHARACTER*64 CBOXN(JELEM)
      CHARACTER*24 CBOXU(JELEM)
C
#ifndef R_4
      REAL*8 VALUES(*)
      REAL*8 VALS(JWORK)
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   VALUES(*)
      REAL   VALS(JWORK)
      REAL   RVIND
      REAL   EPS
#endif
C     ------------------------------------------------------------------
C
C*          1.  GET ALL ELEMENTS UNTIL FIRST OPERATOR APPEAR.
C               ---------------------------------------------
 100  CONTINUE
C

      IF(KERR.GT.0) RETURN
C
      RVIND=1.7D38
      EPS=10.D-7
      NVIND=2147483647
C
      IF(KELEM.GT.JELEM) THEN
         KERR=47
         CALL BUERR(KERR)
         RETURN
      END IF

C
C     JUST TO KEEP PROGRAM FOR FUTURE MULTY SUBSET BOXING
C
      KSUB1=KSUB
      KSUB2=KSUB
C
      IF(KSUB1.LT.1.OR.KSUB1.GT.KSUP(6)) THEN
         KERR=44
         CALL BUERR(KERR)
         RETURN
      END IF
C
      IF(KSUB1.NE.KSUB2) THEN
         WRITE(KNTN,*) 'ONLY ONE SUBSET AT A TIME',
     1' WILL BE BOXED.'
         KSUB2=KSUB1
      END IF
C
      KBOXR(1)=2147483647
      CBOXN(1)='OPERATOR'
      CBOXU(1)=' '
      KBOXR(2)=2147483647
      CBOXN(2)='GENERATING CENTRE( CODE TABLE 001031)'
      CBOXU(2)='CODE TABLE'
      KBOXR(3)=2147483647
      CBOXN(3)='GENERATING APPLICATION (CODE TABLE 001032)'
      CBOXU(3)='CODE TABLE'
      KBOXR(4)=2147483647
      CBOXN(4)='STATISTICS (008024/008023) '
      CBOXU(4)='CODE TABLE'
      KBOXR(5)=2147483647
      CBOXN(5)='INCREMENTAL UPDATE NUMBER'
      CBOXU(5)='NUMERIC'
      KBOXR(6)=2147483647
      CBOXN(6)='MINIMISATION SIMULATION NUMBER'
      CBOXU(6)='NUMERIC'
C
      MREL=0
      J=0
      DO 103 I=1,KSUP(5)
      IF(KWTR(I).EQ.222000.OR.
     1   KWTR(I).EQ.223000.OR.
     1   KWTR(I).EQ.224000.OR.
     1   KWTR(I).EQ.225000.OR.
     1   KWTR(I).EQ.235000.OR.
     1   KWTR(I).EQ.237255.OR.
     1   KWTR(I).EQ.232000) THEN
         J=J+1
         IOPER(J)=I
      END IF
 103  CONTINUE
C
      NOPER=J
C
      IOPER(J+1)=KSUP(5)
C
      NOPP1=NOPER+1
      INI=IOPER(1)*8+20
      IF(INI*NOPP1.GT.JWORK) THEN
         KERR=47
         WRITE(KNTN,*) 'TOO MANY DATA FOR BOXING.'
         RETURN
      END IF
C
      DO 105 I=1,INI*NOPP1
      VALS(I)=RVIND
      KBOXR(I)=NVIND
 105  CONTINUE
C
      IF(NOPER.EQ.0) THEN
         KAPP =1                            ! NUMBER OF APPLICATIONS
         KAPPL=1
         KLEN=KELEM
         J=6
         DO 101 I=1,KSUP(5)
         J=J+1
         KBOXR(J)=KWTR(I)
         CBOXN(J)=CNAMES(I)
         CBOXU(J)=CUNITS(I)
         DO 191 KK=KSUB1,KSUB2
         JJ=I+(KK-1)*KELEM                     ! POINTER TO VALUES ARRAY
         IP=J  !+(KAPP-1)*KLEN  ! +(KK-1)*KAPPL*KLEN ! POINTER TO VALS ARRAY
         VALS (IP)=VALUES(JJ)
 191     CONTINUE
 101     CONTINUE
C
         KBOX=J                             ! TOTAL NUMBER OF ELEMENTS
         MREL=J
         GO TO 2000
      END IF
C
      O236=.FALSE.
C
      LFIRST=1
      J=6
C
C
      LAST=IOPER(1)-1
      IF(MREL.EQ.0) MREL=LAST+6
      KLEN=LAST*8+20
C
 102  CONTINUE
C
C*          1.1 MOVE ELEMENTS FROM LFIRST TO LAST INTO BOX.
C               -------------------------------------------
 110  CONTINUE
C
      KAPP =1                            ! NUMBER OF APPLICATIONS
      KAPPL=60
      DO 111 I=LFIRST,LAST
      J=J+1
      KBOXR(J)=KWTR(I)
      CBOXN(J)=CNAMES(I)
      CBOXU(J)=CUNITS(I)
      DO 192 KK=KSUB1,KSUB2
      JJ=I+(KK-1)*KELEM
      IP=J ! +(KAPP-1)*KLEN   ! +(KK-1)*KAPPL*KLEN
      VALS (IP)=VALUES(JJ)
 192  CONTINUE
 111  CONTINUE
C
      KBOX=J                             ! TOTAL NUMBER OF ELEMENTS
C
      DO 1000 LL=1,NOPER
C
      K=IOPER(LL)
C
      IF(KWTR(K).EQ.235000) THEN
         MREL=0
         K=K+1
C
         KL=IOPER(LL+1)-1
         DO 112 I=K,KL
         J=J+1
         KBOXR(J)=KWTR(I)
         CBOXN(J)=CNAMES(I)
         CBOXU(J)=CUNITS(I)
         DO 190 KK=KSUB1,KSUB2
         JJ=I+(KK-1)*KELEM
         IP=J  !+(KAPP-1)*KLEN              ! +(KK-1)*KAPPL*KLEN
         VALS (IP)=VALUES(JJ)
 190     CONTINUE
 112     CONTINUE
         GO TO 1000
      END IF
      IF(KWTR(K).EQ.222000) THEN
         K=K+1
         IMARK=222000
         IF(MREL.EQ.0) MREL=J
         GO TO 119
      END IF
      IF(KWTR(K).EQ.223000) THEN
         K=K+1
         IMARK=223000
         IF(MREL.EQ.0) MREL=J
         GO TO 119
      END IF
      IF(KWTR(K).EQ.224000) THEN
         K=K+1
         IMARK=224000
         IF(MREL.EQ.0) MREL=J
         GO TO 119
      END IF
      IF(KWTR(K).EQ.225000) THEN
         K=K+1
         IMARK=225000
         IF(MREL.EQ.0) MREL=J
         GO TO 119
      END IF
      IF(KWTR(K).EQ.232000) THEN
         K=K+1
         IMARK=232000
         IF(MREL.EQ.0) MREL=J
      END IF
C
 119  CONTINUE
C
C     CHECK IF NEXT ELEMENT IS OPERATOR
C
      IF(KWTR(K).EQ.236000) THEN
         O236=.TRUE.
         K=K+1
      END IF
C
      IF(KWTR(K).EQ.237000) THEN
         K=K+1
         GO TO 120
      END IF
C
      IF(KWTR(K).EQ.237255) THEN
         O236=.FALSE.
         K=K+1
         GO TO 1000
      END IF
C
C     SKIP IF DELAYED REPLICATION FACTOR FOLLOW
C
      IF(KWTR(K).EQ.31002.OR.KWTR(K).EQ.31001.OR.
     1   KWTR(K).EQ.31000) K=K+1
C
C     NEXT ELEMENT MUST BE DATA PRESENT INDICATOR
C
      IF(KWTR(K).NE.31031.AND.KWTR(K).NE.31192) THEN
         KERR=42
         CALL BUERR(KERR)
         WRITE(KNTN,*) 'ELEMENT ',K,' MUST BE',
     1' DATA PRESENT INDICATOR.'
         RETURN
      END IF
C
C     COUNT NUMBER OF DATA PRESENT INDICATORS
C
C     IBITS - POINTER TO THE FIRST DATA PRESENT INDICATOR
C     IDPI  - NUMBER OF DATA PRESENT INDICATORS
C
      IBITS=K
      IDPI=0
      DO 113 I=K,IOPER(LL+1)
      IF(KWTR(I).EQ.31031) THEN
         IDPI=IDPI+1 
      ELSE
         GO TO 114
      END IF
 113  CONTINUE
C
      KERR=43
      CALL BUERR(KERR)
      RETURN
C
 114  CONTINUE
C
C     RESET CURRENT POINTER
C
      K=I
C
C     GET BIT MAP FROM VALUES
C
      IBP=IBITS-1
      DO 115 I=1,IDPI
      IBP=IBP+1
      IF(ABS(VALUES(IBP)-RVIND)/RVIND.LT.EPS) THEN
         IBVAL(I)=NVIND
      ELSE 
         IBVAL(I)=VALUES(IBP)
      END IF
 115  CONTINUE
C
C
 120  CONTINUE
C
C     NEXT 3 ELEMENTS MUST BE GENERATING CENTRE AND APPLICATION 
C     AND CLASS 8 ELEMENT.
C
      KAPP=KAPP+1
      IF(KAPP.GT.60) THEN
         WRITE(KNTN,*) 'THERE IS MORE THAN 60 APPLICATIONS',
     1' IN THE DATA.'
         RETURN
      END IF
      IF(KWTR(K).EQ.1031.OR.KWTR(K).EQ.1033) THEN
         KP=(KAPP-1)*KLEN
         KP1=KP+1
         KBOXR(KP1)=IMARK
C         CBOXN(KP1)=' '
C         CBOXU(KP1)=' '
         DO 194 KK=KSUB1,KSUB2
         IP=1+(KAPP-1)*KLEN             ! +(KK-1)*KAPPL*KLEN
         VALS(IP)=IMARK
 194     CONTINUE
         KP2=KP+2
         KBOXR(KP2)=KWTR(K)
C         CBOXN(KP2)=CNAMES(K)
C         CBOXU(KP2)=CUNITS(K)
         DO 195 KK=KSUB1,KSUB2
         JJ=K+(KK-1)*KELEM
         IP=2+(KAPP-1)*KLEN             ! +(KK-1)*KAPPL*KLEN
         VALS(IP)=VALUES(JJ)  
 195     CONTINUE
         K=K+1
      END IF
      IF(KWTR(K).EQ.1032.OR.KWTR(K).EQ.1201.OR.KWTR(K).EQ.63191) THEN
         KP=(KAPP-1)*KLEN
         KP3=KP+3
         KBOXR(KP3)=KWTR(K)
C         CBOXN(KP3)=CNAMES(K)
C         CBOXU(KP3)=CUNITS(K)
         DO 196 KK=KSUB1,KSUB2
         JJ=K+(KK-1)*KELEM
         IP=3+(KAPP-1)*KLEN             ! +(KK-1)*KAPPL*KLEN
         VALS(IP)=VALUES(JJ)
 196     CONTINUE
         K=K+1
      END IF
      IF(KWTR(K)/1000.EQ.8) THEN
         KP=(KAPP-1)*KLEN
         KP4=KP+4
         KBOXR(KP4)=KWTR(K)
C         CBOXN(KP4)=CNAMES(K)
C         CBOXU(KP4)=CUNITS(K)
         DO 197 KK=KSUB1,KSUB2
         JJ=K+(KK-1)*KELEM
         IP=4+(KAPP-1)*KLEN             !  +(KK-1)*KAPPL*KLEN
         VALS(IP)=VALUES(JJ)
 197     CONTINUE
         K=K+1
      END IF
      IF(KWTR(K).EQ.33210) THEN
         KP=(KAPP-1)*KLEN
         KP5=KP+5
         KBOXR(KP5)=KWTR(K)
C         CBOXN(KP5)=CNAMES(K)
C         CBOXU(KP5)=CUNITS(K)
         DO 201 KK=KSUB1,KSUB2
         JJ=K+(KK-1)*KELEM
         IP=5+(KAPP-1)*KLEN             !  +(KK-1)*KAPPL*KLEN
         VALS(IP)=VALUES(JJ)
 201     CONTINUE
         K=K+1
      END IF
      IF(KWTR(K).EQ.33211) THEN
         KP=(KAPP-1)*KLEN
         KP6=KP+6
         KBOXR(KP6)=KWTR(K)
C         CBOXN(KP6)=CNAMES(K)
C         CBOXU(KP6)=CUNITS(K)
         DO 202 KK=KSUB1,KSUB2
         JJ=K+(KK-1)*KELEM
         IP=6+(KAPP-1)*KLEN             !  +(KK-1)*KAPPL*KLEN
         VALS(IP)=VALUES(JJ)
 202     CONTINUE
         K=K+1
      END IF
C
C     NEXT ELEMENT CAN BE DELAYED/EXTENDED DELAYED REPLICATION
C     SKIP IT
C
      IF(KWTR(K).EQ.31002.OR.KWTR(K).EQ.31001.OR.
     1   KWTR(K).EQ.31000) K=K+1
C
C
C     MOVE CORESPONDING QUALITY CONTROL INTO BOX
C
      KQ=MREL-IDPI+1
      KAPPK=(KAPP-1)*KLEN  
      DO 121 I=1,IDPI
      IF(IBVAL(I).NE.0) THEN
         KKQ=KQ+KAPPK
         VALS(KKQ)=RVIND 
         KQ=KQ+1
      ELSE
         KKQ=KQ+KAPPK
         KBOXR(KKQ)=KWTR(K)
C         CBOXN(KKQ)=CNAMES(K)
C         CBOXU(KKQ)=CUNITS(K)
         DO 198 KK=KSUB1,KSUB2
         JJ=K+(KK-1)*KELEM                ! +(KK-1)*KAPPL*KLEN
         VALS(KKQ)=VALUES(JJ)
 198     CONTINUE
         K=K+1 
         KQ=KQ+1
      END IF
 121  CONTINUE
C
C     CHECK IF SOME NEW DATA FOLLOW.
C
      IF(K.LT.IOPER(LL+1)-1) THEN
C
C        MOVE THESE ELEMENTS INTO BOX
C
         IF(KWTR(IOPER(LL+1)).GT.200000.AND.
     1      KWTR(IOPER(LL+1)).NE.999999) THEN
            ILL=IOPER(LL+1)-1
         ELSE
            ILL=IOPER(LL+1)
         END IF
         DO 122 I=K,ILL
         J=J+1
         KBOXR(J)=KWTR(I)
         CBOXN(J)=CNAMES(I)
         CBOXU(J)=CUNITS(I)
         DO 199 KK=KSUB1,KSUB2
         JJ=I+(KK-1)*KELEM
         IP=J   !+(KAPP-1)*KLEN              !  +(KK-1)*KAPPL*KLEN
         VALS(IP)=VALUES(JJ)
 199     CONTINUE
 122     CONTINUE
         KBOX=J
      END IF
C
 1000 CONTINUE
C
 2000 CONTINUE
C
      KBOX=J
      IF(KBOX.LE.6) KBOX=0
C
      RETURN
      END
