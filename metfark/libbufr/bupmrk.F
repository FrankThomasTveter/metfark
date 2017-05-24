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

      SUBROUTINE BUPMRK(KBUFL,KBUFF,KSEC3,KELEM,KERR)
C
C**** *BUPMRK*
C
C
C     PURPOSE.
C     --------
C
C          PROCESS MARKER OPERATOR, RELACING IT WITH CORRESPONDING
C         TABLE B ELEMENT DESCRIPTOR.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUPMRK(KBUFL,KBUFF,KSEC3,KELEM,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KSEC3*   -  ARRAY CONTAINING SECTION 3 INFORMATION
C                            KSEC3( 1)-- LENGTH OF SECTION 3 (BYTES)
C                            KSEC3( 2)-- RESERVED
C                            KSEC3( 3)-- NUMBER OF SUBSETS
C                            KSEC3( 4)-- FLAG (DATA TYPE,DATA COMPRESSION)
C               *KELEM*   -  DIMENSION OF CNAMES, CUNITS ARRAY
C        OUTPUT:
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C     -------
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          - UNPACKS BIT PATTERN
C          BUNPKS         - UNPACKS BIT PATTERN IN REPEATED WAY
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
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
#     include "bcombef.F"
#     include "bcomwt.F"
#     include "bcomwtc.F"
#     include "bcomrq.F"
C
      CHARACTER CWTEN*64,CWTU*24
C
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
      DIMENSION KSEC3(JSEC3)
      DIMENSION KBUFF(KBUFL),IBV(JELEM),ILIST(JELEM)
      DIMENSION IMASK(8)
      DATA IMASK/1,2,4,8,16,32,64,128/
C
      SAVE IBV
C
C     ------------------------------------------------------------------
C*          1.  FINED OPERATOR 223000 TO 237255.
C               --------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
C           1.1 CHECK IF DATA ARE COMRESSED.
C               ----------------------------
 110  CONTINUE
C
      IB=0
      IF(IAND(KSEC3(4),IMASK(7)).NE.0) IB=1
C
C*          1.2 FIND POSITION OF OPREATORS.
C               ---------------------------
 120  CONTINUE
C
      M0OLD=M0
      DO 121 J=M0OLD,M
C
      NR223=0
      NR224=0
      NR225=0
      NR232=0
C
      IF(NWTR(J).EQ.222000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         M0=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 122 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
               I222=MBMP
               NR222=MBMPL-MBMP+1
C
               CALL BUGETBM(KBUFL,KBUFF,KSEC3,I222,NR222,IBV,KERR)
               IF(KERR.GT.0) RETURN
C
               GO TO 121
            END IF
 122        CONTINUE
         END IF
         GO TO 121
      END IF
C
      IF(NWTR(J).EQ.223000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         M0=J
         NP223000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 123 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
C
               I223=MBMP
               NR223=MBMPL-MBMP+1
C
               CALL BUGETBM(KBUFL,KBUFF,KSEC3,I223,NR223,IBV,KERR)
               IF(KERR.GT.0) RETURN
C
               GO TO 200
            END IF
 123        CONTINUE
         END IF
         GO TO 200
      END IF
      IF(NWTR(J).EQ.224000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         M0=J
         NP224000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 124 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
C
               I224=MBMP
               NR224=MBMPL-MBMP+1
C
               CALL BUGETBM(KBUFL,KBUFF,KSEC3,I224,NR224,IBV,KERR)
               IF(KERR.GT.0) RETURN
C
               GO TO 300
            END IF
 124        CONTINUE
         END IF
         GO TO 300
      END IF
      IF(NWTR(J).EQ.225000) THEN
C
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         M0=J
         NP225000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 125 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
C
               I225=MBMP
               NR225=MBMPL-MBMP+1
C
               CALL BUGETBM(KBUFL,KBUFF,KSEC3,I225,NR225,IBV,KERR)
               IF(KERR.GT.0) RETURN
C
               GO TO 400
            END IF
 125        CONTINUE
         END IF
         GO TO 400
      END IF
C
      IF(NWTR(J).EQ.232000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         M0=J
         NP232000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 126 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
C
               I232=MBMP
               NR232=MBMPL-MBMP+1
C
               CALL BUGETBM(KBUFL,KBUFF,KSEC3,I232,NR232,IBV,KERR)
               IF(KERR.GT.0) RETURN
C
               GO TO 500
            END IF
 126        CONTINUE
         END IF
         GO TO 500
      END IF
C
C
      IF(NWTR(J).EQ.235000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         MREL=0
         MBMP=0
         MBMPL=0
         M0=J
C
      END IF
C
      IF(NWTR(J).EQ.237255) THEN
C
C        CANCEL BACKWARD BIT MAP REFERENCE.
C
         MBMP=0
         MBMPL=0
C
      END IF

      GO TO 121
C
C     -----------------------------------------------------------------
C*          2.  PROCESS SUBSTITUTED VALUES OPERATOR.
C               ------------------------------------
C
 200  CONTINUE
C
C*          2.1 FIND FIRST ACCURANCE OF 223255.
C               -------------------------------
 210  CONTINUE
C
      NP223255=0
      DO 211 I=NP223000,M
      IF(NWTR(I).EQ.223255) THEN
         NP223255=I
         GO TO 220
      END IF
 211  CONTINUE
C
      GO TO 121
C
C*          2.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 220  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I223=0
         OF223=.TRUE.
         DO 221 I=NP223000,NP223255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF223) THEN
               I223=I
               OF223=.FALSE.
            END IF
            NR223=NR223+1
         END IF
 221     CONTINUE
      ELSE
         I223=MBMP
         NR223=MBMPL-MBMP+1
         GO TO 250
      END IF
C
C*          2.3 CALCULATE WORD AND BIT POINTER TO FIRST
C               ---------------------------------------
C               DATA PRESENT INDICATOR.
C               -----------------------
 230  CONTINUE
C
      CALL BUGETBM(KBUFL,KBUFF,KSEC3,I223,NR223,IBV,KERR)
      IF(KERR.GT.0) RETURN
C
C*          2.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 250  CONTINUE
C
      ISUBST=MREL-NR223+1
C
C*          2.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 260  CONTINUE
C
      DO 261 I=1,NR223
      IF(IBV(I).EQ.0) THEN
C
 262     CONTINUE
C
         IF(NWTR(NP223255).EQ.223255) THEN
            NWTDW (NP223255)=NWTDW(ISUBST)
            NWTR  (NP223255)=NWTR (ISUBST)
            NWTRV (NP223255)=NWTRV(ISUBST)
            NWTS  (NP223255)=NWTS (ISUBST)
            CWTEN (NP223255)=CWTEN(ISUBST)
            CWTU  (NP223255)=CWTU (ISUBST)
            NWTEN (NP223255)=NWTEN(ISUBST)
            ISUBST=ISUBST+1
            NP223255=NP223255+1
         ELSE
            NP223255=NP223255+1
            GO TO 262
         END IF
      ELSE
         ISUBST=ISUBST+1
      END IF
 261  CONTINUE
C
      GO TO 121
C
C     -----------------------------------------------------------------
C*          3.  PROCESS FIRST ORDER STATISTICS OPERATOR.
C               ----------------------------------------
 300  CONTINUE
C
C*          3.1 FIND FIRST ACCURANCE OD 224255.
C               -------------------------------
 310  CONTINUE
C
      NP224255=0
      DO 311 I=NP224000,M
      IF(NWTR(I).EQ.224255) THEN
         NP224255=I
         GO TO 320
      END IF
 311  CONTINUE
C
      GO TO 121
C
C*          3.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 320  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I224=0
         OF224=.TRUE.
         DO 321 I=NP224000,NP224255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF224) THEN
               I224=I
               OF224=.FALSE.
            END IF
            NR224=NR224+1
         END IF
 321     CONTINUE
      ELSE
         I224=MBMP
         NR224=MBMPL-MBMP+1
         GO TO 350
      END IF
C
C*          3.3 CALCULATE WORD AND BIT POINTER TO FIRST
C               ---------------------------------------
C               DATA PRESENT INDICATOR.
C               -----------------------
 330  CONTINUE
C
      CALL BUGETBM(KBUFL,KBUFF,KSEC3,I224,NR224,IBV,KERR)
      IF(KERR.GT.0) RETURN
C
C*          3.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 350  CONTINUE
C
      ISUBST=MREL-NR224+1
C
C*          3.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 360  CONTINUE
C
      DO 361 I=1,NR224
      IF(IBV(I).EQ.0) THEN
C
 362     CONTINUE
C
         IF(NWTR(NP224255).EQ.224255) THEN
            NWTDW (NP224255)=NWTDW(ISUBST)
            NWTR  (NP224255)=NWTR (ISUBST)
            NWTRV (NP224255)=NWTRV(ISUBST)
            NWTS  (NP224255)=NWTS (ISUBST)
            CWTEN (NP224255)=CWTEN(ISUBST)
            CWTU  (NP224255)=CWTU (ISUBST)
            NWTEN (NP224255)=NWTEN(ISUBST)
            ISUBST=ISUBST+1
            NP224255=NP224255+1
         ELSE
            NP224255=NP224255+1
            GO TO 362
         END IF
      ELSE
         ISUBST=ISUBST+1
      END IF
 361  CONTINUE
C
      GO TO 121
C
C
C     -----------------------------------------------------------------
C*          4.  PROCESS DIFFERENCE STATISTICS OPERATOR.
C               ---------------------------------------
 400  CONTINUE
C
C
C*          4.1 FIND FIRST ACCURANCE OD 223255.
C               -------------------------------
 410  CONTINUE
C
      NP225255=0
      DO 411 I=NP225000,M
      IF(NWTR(I).EQ.225255) THEN
         NP225255=I
         GO TO 420
      END IF
 411  CONTINUE
C
      GO TO 121
C
C*          4.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 420  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I225=0
         OF225=.TRUE.
         DO 421 I=NP225000,NP225255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF225) THEN
               I225=I
               OF225=.FALSE.
            END IF
            NR225=NR225+1
         END IF
 421     CONTINUE
      ELSE
         I225=MBMP
         NR225=MBMPL-MBMP+1
         GO TO 450
      END IF
C
C*          4.3 CALCULATE WORD AND BIT POINTER TO FIRST
C               ---------------------------------------
C               DATA PRESENT INDICATOR.
C               -----------------------
 430  CONTINUE
C
      CALL BUGETBM(KBUFL,KBUFF,KSEC3,I225,NR225,IBV,KERR)
      IF(KERR.GT.0) RETURN
C
C*          4.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 450  CONTINUE
C
      ISUBST=MREL-NR225+1
C
C*          4.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 460  CONTINUE
C
      DO 461 I=1,NR225
      IF(IBV(I).EQ.0) THEN
C
 462     CONTINUE
C
         IF(NWTR(NP225255).EQ.225255) THEN
            NWTR  (NP225255)=NWTR (ISUBST)
C
C           CHANGE REFERENCE VALUE TO BE CENTRED AROUND ZERO
C           AND INCREASE DATA WIDTH BY 1
C
            IF(NWTEN(ISUBST).NE.-999) THEN
               NWTRV (NP225255)=-2**NWTDW(ISUBST)
               NWTDW (NP225255)=NWTDW(ISUBST)+1
            ELSE
               NWTRV (NP225255)=NWTDW(ISUBST)
               NWTDW (NP225255)=NWTDW(ISUBST)
               NWTEN (NP225255)=NWTEN(ISUBST)
            END IF
C
            NWTS  (NP225255)=NWTS (ISUBST)
            CWTEN (NP225255)=CWTEN(ISUBST)
            CWTU  (NP225255)=CWTU (ISUBST)
            ISUBST=ISUBST+1
            NP225255=NP225255+1
         ELSE
            NP225255=NP225255+1
            GO TO 462
         END IF
      ELSE
         ISUBST=ISUBST+1
      END IF
 461  CONTINUE
C
      GO TO 121
C
C     -----------------------------------------------------------------
C*          5.  PROCESS REPLACE/RETAINED OPERATOR.
C               ----------------------------------
 500  CONTINUE
C
C
C*          5.1 FIND FIRST ACCURANCE OD 232255.
C               -------------------------------
 510  CONTINUE
C
      NP232255=0
      DO 511 I=NP232000,M
      IF(NWTR(I).EQ.232255) THEN
         NP232255=I
         GO TO 520
      END IF
 511  CONTINUE
C
      GO TO 121
C
C*          5.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 520  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I232=0
         OF232=.TRUE.
         DO 521 I=NP232000,NP232255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF232) THEN
               I232=I
               OF232=.FALSE.
            END IF
            NR232=NR232+1
         END IF
 521     CONTINUE
      ELSE
         I232=MBMP
         NR232=MBMPL-MBMP+1
         GO TO 550
      END IF
C
C*          5.3 CALCULATE WORD AND BIT POINTER TO FIRST
C               ---------------------------------------
C               DATA PRESENT INDICATOR.
C               -----------------------
 530  CONTINUE
C
      CALL BUGETBM(KBUFL,KBUFF,KSEC3,I232,NR232,IBV,KERR)
      IF(KERR.GT.0) RETURN
C
C*          5.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 550  CONTINUE
C
      ISUBST=MREL-NR232+1
C
C*          5.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 560  CONTINUE
C
      DO 561 I=1,NR232
      IF(IBV(I).EQ.0) THEN
C
 562     CONTINUE
C
         IF(NWTR(NP232255).EQ.232255) THEN
            NWTDW (NP232255)=NWTDW(ISUBST)
            NWTR  (NP232255)=NWTR (ISUBST)
            NWTRV (NP232255)=NWTRV(ISUBST)
            NWTS  (NP232255)=NWTS (ISUBST)
            CWTEN (NP232255)=CWTEN(ISUBST)
            CWTU  (NP232255)=CWTU (ISUBST)
            ISUBST=ISUBST+1
            NP232255=NP232255+1
         ELSE
            NP232255=NP232255+1
            GO TO 562
         END IF
      ELSE
         ISUBST=ISUBST+1
      END IF
 561  CONTINUE
C
C     -----------------------------------------------------------------
 121  CONTINUE
C
      RETURN
      END
