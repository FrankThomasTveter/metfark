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

      SUBROUTINE BUUATB(KBUFL,KBUFF,KJ,KY,KSTACK,KELEM,KERR)
C
C**** *BUUATB*
C
C
C     PURPOSE.
C     --------
C          UPDATE AUGMENTED BUFR TABLE B.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUUATB(KBUFL,KBUFF,KJ,KY,KSTACK,KELEM,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KJ*      -  POINTER TO KSTACK ARRAY
C               *KY*      -  OPERAND OF THE DATA DESCRIPTOR OPERATOR
C               *KELEM*   -
C        OUTPUT:
C               *KSTACK* - LIST OF ELEMENTS
C               *KERR*   - RETURN ERROR CODE
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
C          GBYTE        - PACK BIT PATTERN
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
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
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
C
#     include "parameter.F"
#     include "bcomunit.F"
#     include "bcomwork.F"
#     include "bcomatb.F"
#     include "bcomatbc.F"
#     include "bcomtab.F"
#     include "bcomtabc.F"
#     include "bcomwt.F"
#     include "bcomwtc.F"
#     include "bcombef.F"
#     include "bcomcom.F"
#     include "bcomoff.F"
C
      CHARACTER CATBEN*64,CWTEN*64,CTABBEN*64
      CHARACTER CATBU*24,CWTU*24,CTABBU*24
C
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSTACK(*)
      DIMENSION IMASK(8)
      DATA IMASK/1,2,4,8,16,32,64,128/
C     ------------------------------------------------------------------
C
C*          1.   UPDATE AUGMENTED TABLE B .
C                --------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C*          1.1 Y = 0 ?
C               -------
 110  CONTINUE
C
      IF( KY.EQ.0) THEN
C
C*          1.1.1 CLEAR AUGMENTED TABLE B.
C                 ------------------------
         NJA= 0
C
         DO 111 J=1,JTAB
C
         NATBTR(J)= 0
         NATBS (J)= 0
         NATBRV(J)= 0
         NATBDW(J)= 0
         CATBEN(J)=' '
         CATBU (J)=' '
C
 111     CONTINUE
C
         GO TO 300
      END IF
C
C*          1.2  GET NEXT DESCRIPTOR FROM STACK.
C                -------------------------------
 120  CONTINUE
C
      KJ=KJ + 1
      KDD = KSTACK(KJ)
C
C*          1.3  ELEMENT DESCRIPTOR  ?
C                ---------------------
 130  CONTINUE
C
      IF  = KDD /100000
      IDIF= KDD -IF*100000
      IX  = IDIF/1000
      IY  = IDIF-IX*1000
C
      IF(IF.EQ.0) THEN
C
C*          1.3.1 ADD SPECIAL ENTRY TO WORKING TABLE.
C                 -----------------------------------
         NWT = NWT + 1
         NWTEN(NWT)=836970
         CWTEN(NWT)='REFERENCE VALUE'
         CWTU (NWT)=' '
         NWTDW(NWT)= KY
         NWTS (NWT)=0
         NWTRV(NWT)=0
         M=M+1
C
C*          1.3.2 ADD ENTRY TO AUGMENTED TABLE B .
C                 --------------------------------
         DO 131 J=1,JTAB
C
         IF(NTABBTR(J).EQ.KDD) THEN
            I=J
            GO TO 133
         END IF
C
 131     CONTINUE
C
         KERR=23
         WRITE(KNTN,*)'BUUATB :'
         CALL BUERR(KERR)
C
         GO TO 300
C
 133  CONTINUE
C
         NJA=NJA + 1
C
         NATBTR(NJA)=NTABBTR(I)
         NATBS (NJA)=NTABBS (I)
         NATBRV(NJA)=NTABBRV(I)
         NATBDW(NJA)=NTABBDW(I)
         CATBEN(NJA)=CTABBEN(I)
         CATBU (NJA)=CTABBU (I)
C
C*          1.3.3 COMPLITE ENTRY WITH NEW REFERENCE VALUE
C                 ----------------------------------------
C                 FROM DATA SECTION.
C                 ------------------
C
         IB=0
         IF(IAND(ICOMP,IMASK(7)).NE.0) IB=1

         IF(IB.EQ.0) THEN
C
            IBIT=0
C
            DO 134 J=1,NWT-1
C
            IBIT =IBIT + NWTDW(J)
C
 134        CONTINUE
C
            IF(NSUBSET.EQ.1) THEN
               IBIT = IBIT +32 + NBPTB
            ELSE
               IBIT = IBIT + NBP
            END IF
            IWORD= IBIT/NBPW
            ISKIP= IBIT - IWORD*NBPW
            IF(NSUBSET.EQ.1) THEN
               IWORD= IWORD + NWPTB
            ELSE
               IWORD= IWORD + NWP
            END IF
C
C
C           1.3.4 CHECK IF REFERENCE VALUE NEGATIVE
C
            CALL GBYTE(KBUFF(IWORD),ISGN,ISKIP,1)
            IF(ISGN.EQ.1) THEN
               IBIT=IBIT+1
               IWORD= IBIT/NBPW
               ISKIP= IBIT - IWORD*NBPW
               IWORD= IWORD + NWPTB
C
               CALL GBYTE(KBUFF(IWORD),NATBRV(NJA),ISKIP,KY-1)
               NATBRV(NJA)=-NATBRV(NJA)
            ELSE
               CALL GBYTE(KBUFF(IWORD),NATBRV(NJA),ISKIP,KY)
            END IF
C
         ELSE
C            COMPRESSED DATA
C
            IBIT=32+NBPTB
            IWORD=IBIT/NBPW
C
            IWORDP=NWPTB+IWORD
            IBITP =IBIT-IWORD*NBPW
C
            DO 402 I=2,NWT
            IWRD=IWORDP
            IBTP=IBITP
C
            IF(NWTDW(I-1).EQ.0) THEN
               IBITP =IBITP
               IWORDP=IWORDP
               GO TO 402
            END IF
C
            IBTP=IBTP+NWTDW(I-1)
            IF(IBTP.GE.NBPW) THEN
               IW=IBTP/NBPW
               IBTP=IBTP-IW*NBPW
               IWRD=IWRD+IW
            END IF
C
            CALL BUNPCK(NBPW,KBUFF,IDWINC,IWRD,IBTP,6,KERR)
            IF(KERR.GT.0) RETURN
            IF(IDWINC.GT.JBPW) THEN
               KERR=15
               WRITE(KNTN,*) 'BUUATB:'
               CALL BUERR(KERR)
               RETURN
            END IF
            IF(IWRD.GT.KBUFL) THEN
               KERR=26
               WRITE(KNTN,*) 'BUUATB:'
               CALL BUERR(KERR)
               RETURN
            END IF
C
            IF(CWTU(I-1)(1:3).EQ.'CCI') THEN
               IWTIWS=NWTDW(I-1)+6+N*IDWINC*8
            ELSE
               IWTIWS=NWTDW(I-1)+6+N*IDWINC
            END IF
C
            IBIT = IBITP + IWTIWS
            IWORD= IBIT/NBPW
C
            IBITP = IBIT - IWORD*NBPW
            IWORDP= IWORDP + IWORD
C
            IF(IWORDP.GT.KBUFL) THEN
               KERR=26
               WRITE(KNTN,*) 'BUUATB :'
               CALL BUERR(KERR)
               RETURN
            END IF
 402        CONTINUE
C
C
            IW=IWORDP
            IBT=IBITP
            CALL BUNPCK(NBPW,KBUFF,ISG_REF,IW,IBT,1,KERR)
            CALL BUNPCK(NBPW,KBUFF,IR0,IW,IBT,NWTDW(NWT)-1,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KBUFF,IDWINC,IW,IBT,6,KERR)
            IF(KERR.GT.0) RETURN
            IF(IDWINC.GT.JBPW) THEN
               KERR=15
               WRITE(KNTN,*) ' BUUATB :'
               CALL BUERR(KERR)
               RETURN
            END IF
C
            IF(IDWINC.NE.0) THEN
               CALL BUNPCK(NBPW,KBUFF,INCR,IW,IBT,IDWINC,KERR)
               IF(KERR.GT.0) RETURN
            END IF
C
            IF(IR0.EQ.NMASK(NWTDW(NWT))) THEN
               JR=NMASK(NWTDW(NWT))
               KERR=19
               WRITE(KNTN,*) 'BUUATB :'
               CALL BUERR(KERR)
               RETURN
            ELSE
               IWTPR0=NWTRV(NWT)+IR0
C
               IF(IDWINC.EQ.0) THEN
                  JREF=IWTPR0
               ELSE
                  IF(INCR.EQ.NMASK(IDWINC)) THEN
                     JREF=NMASK(NWTDW(NWT))
                     KERR=19
                     WRITE(KNTN,*) 'BUUATB :'
                     CALL BUERR(KERR)
                     RETURN
                  ELSE
                     JREF= IWTPR0 + INCR
                  END IF
               END IF
            END IF
C
            NATBRV(NJA)=JREF
            IF(ISG_REF.EQ.1) NATBRV(NJA)=- NATBRV(NJA)
         END IF
C
C           1.3.5 UPDATA WORKING TABLE ENTRIES.
C                 -----------------------------
 135     CONTINUE
C
c        IF(KDD.EQ.31031.OR.KDD.EQ.31192) THEN
c           NWT=NWT+1
c           NWTR(NWT)=KDD
c           NWTS(NWT)=0
c           NWTRV(NWT)=0
c           NWTDW(NWT)=1
c           CWTEN(NWT)='DATA PRESENT INDICATOR'
c           CWTU (NWT)='NUMERIC'
c           M=M+1
c        ELSEIF(KDD.EQ.33007.OR.KDD.EQ.63192) THEN
c           NWT=NWT+1
c           NWTR(NWT)=KDD
c           NWTS(NWT)=0
c           NWTRV(NWT)=0
c           NWTDW(NWT)=7
c           CWTEN(NWT)='% CONFIDENCE'
c           CWTU (NWT)='NUMERIC'
c           M=M+1
c        ELSE
c           CALL BUUPWT(KDD,KELEM,KERR)
c           IF(KERR.GT.0) RETURN
c        END IF
C
         GO TO 120
C
      END IF
C
C     ------------------------------------------------------------------
C
C*           1.4   CHANGE REFERENCE VALUE ?
C                  ------------------------
 140  CONTINUE
C
      IF( IF.EQ.2.AND.IX.EQ.3) THEN
         IF(IY.EQ.255) GO TO 300
         KY=IY
         GO TO 120
      END IF
C     ------------------------------------------------------------------
 200  CONTINUE
C
      KERR=23
      WRITE(KNTN,*)'BUUATB :'
      CALL BUERR(KERR)
C
C     ------------------------------------------------------------------
 300  CONTINUE
C
      RETURN
      END
