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

      SUBROUTINE BUSRP(KBUFL,KBUFF,KSEC3,KJ,KJ1,KDD,KSTACK,KELEM,KERR)
C
C**** *BUSRP*
C
C
C     PURPOSE.
C     --------
C          RESOLVE DATA DESCRIPTOR REPLICATION PROBLEM.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUSRP(KBUFL,KBUFF,KSEC3,KJ,KJ1,KDD,KSTACK,KELEM,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KSEC3*   -  ARRAY CONTAINIG SECTION 3 INFORMATION
C               *KELEM*   -  DIMENSION OF CNAMES, CUNITS ARRAY
C               *KDD*      - DATA DESCRIPTOR
C        OUTPUT:
C               *KJ*       - POINTER TO KSTACK ARRAY
C               *KJ1*      - POINTER TO LAST ELEMENT IN KSTACK
C               *KSTACK*   - LIST OF DATA DESCRIPTORS
C               *KERR*     - RETURN CODE
C
C     METHOD.
C     -------
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUSTDR            - RESOLVE TABLE D REFERENCE
C          BUUPWT              - UPDATE WORKING TABLES
C          GBYTE             - UNPACK BIT PATTERN
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
      CHARACTER CWTEN*64,CWTU*24
C
C
#     include "parameter.F"
#     include "bcomunit.F"
#     include "bcomwork.F"
#     include "bcomwt.F"
#     include "bcomwtc.F"
#     include "bcombef.F"
#     include "bcomoff.F"
C
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
      DIMENSION KBUFF(KBUFL),ILIST(JELEM)
      DIMENSION KSTACK(*),KSEC3(JSEC3),IMASK(8)
C
      DATA IMASK/1,2,4,8,16,32,64,128/
C
C     ------------------------------------------------------------------
C
C*          1.   STORE K, NUMBER OF DESCRIPTORS TO BE REPLICATED.
C                ------------------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      IF  = KDD / 100000
      IDIF= KDD - IF * 100000
      IX  = IDIF / 1000
      IY  = IDIF - IX * 1000
      K   = IX
C
      IF(IY.EQ.0) ODREPF=.TRUE.
C
C*          1.1   DELAYED REPLICATION ?
C                 ---------------------
 110  CONTINUE
C
      IF( IY .NE. 0 ) THEN
C
C*          1.2   STORE NUMBER OF DESCRIPTORS, K, AND REPLICATION
C                 -----------------------------------------------
C                 FACTOR JR.
C                 ----------
 120     CONTINUE
C
C         K  = IX
         JR = IY
         GO TO 500
      END IF
C
C     ------------------------------------------------------------------
C
C*          2.   GET NEXT DESCRIPTOR.
C                --------------------
 200  CONTINUE
C
      KJ =KJ + 1
      KDD= KSTACK(KJ)
C
C     ------------------------------------------------------------------
C
C*          2.1  REPLICATION FACTOR ?
C                --------------------
 210  CONTINUE
C
      IF(KDD.NE.31001.AND.KDD.NE.31002.AND.
     1   KDD.NE.31000.AND.
     1   KDD.NE.31011.AND.KDD.NE.31012 )THEN

C
C*          2.1.1  SEQUENCE DESCRIPTOR ?
C                  ---------------------
C
         IF=KDD/100000
C
         IF( IF.EQ.3) THEN
C
C*          2.1.1.1  SOLVE TABLE D REFERENCE.
C                    ------------------------
            CALL BUSTDR(KJ,KJ1,KDD,KSTACK,KERR)
            IF(KERR.GT.0) THEN
               DO 252 IQ=1,JELEM
               NSTACK(IQ)=0.
 252           CONTINUE
               RETURN
            END IF
            GO TO 200
         END IF
C
         IF( IF.EQ.2) THEN
            CALL BUPRCO(KBUFL,KBUFF,KJ,KDD,KSTACK,KELEM,KERR)
            IF(KERR.GT.0) THEN
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 200
         END IF
C
         KERR=36
         CALL BUERR(KERR)
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C
C*          3.  UPDATE WORKING TABLE.
C               ---------------------
 300  CONTINUE
C
c     IF(KDD.EQ.31031.OR.KDD.EQ.31192) THEN
c        NWT=NWT+1
c        NWTR(NWT)=KDD
c        NWTS(NWT)=0
c        NWTRV(NWT)=0
c        NWTDW(NWT)=1
c        CWTEN(NWT)='DATA PRESENT INDICATOR'
c        CWTU (NWT)='NUMERIC'
c        M=M+1
c     ELSEIF(KDD.EQ.33007.OR.KDD.EQ.63192) THEN
c        NWT=NWT+1
c        NWTR(NWT)=KDD
c        NWTS(NWT)=0
c        NWTRV(NWT)=0
c        NWTDW(NWT)=7
c        CWTEN(NWT)='% CONFIDENCE'
c        CWTU (NWT)='NUMERIC'
c        M=M+1
c     ELSE
         CALL BUUPWT(KDD,KELEM,KERR)
         IF(KERR.GT.0) RETURN
         IF(KDD.EQ.031011.or.kdd.eq.031012) return
c     END IF
C
C     CHECK IF NEXT DESCRIPTOR CANCEL OPERATOR FOR DELAYED
C     REPLICATION
C
      IIIF=KSTACK(KJ+1)/100000
      IIII=KSTACK(KJ+1)-IIIF*100000
      IIIX=IIII/1000
      IIIY=IIII-IIIX*1000
C
      IF(IIIF.EQ.2.AND.IIIY.EQ.0) THEN
         KJ=KJ+1
         KDD=KSTACK(KJ)
         CALL BUPRCO(KBUFL,KBUFF,KJ,KDD,KSTACK,KELEM,KERR)
         IF(KERR.GT.0) THEN
            CALL BUERR(KERR)
            RETURN
         END IF
      END IF
C
C     ------------------------------------------------------------------
C
C*          4.  LOCATE AND STORE JR, THE REPLICATION FACTOR FROM DATA.
C               ------------------------------------------------------
 400  CONTINUE
C
      IB=0
      IF(IAND(KSEC3(4),IMASK(7)).NE.0) IB=1
C
C     RESOLVE MARKERS
C
      IF(OMARKER) THEN
         CALL BUPMRK(KBUFL,KBUFF,KSEC3,KELEM,KERR)
         IF(KERR.GT.0) RETURN
      END IF
C
      IF(IB.EQ.0) THEN
C
C        UNCOMPRESSED DATA
C
         IBIT=0
C
         DO 401 J=1,NWT-1
C
         IBIT=IBIT+NWTDW(J)
C
 401     CONTINUE
C
         IF(NSUBSET.EQ.1) THEN
            IBIT = IBIT + 32 + NBPTB
         ELSE
            IBIT=IBIT+NBP
         END IF
C
         IWORD= IBIT/NBPW
         ISKIP= IBIT - IWORD*NBPW
         IF(NSUBSET.EQ.1) THEN
            IWORD= IWORD + NWPTB
         ELSE
            IWORD= IWORD + NWP
         END IF
C
         CALL GBYTE(KBUFF(IWORD),JR,ISKIP,NWTDW(NWT))

         IF(JR.EQ.0) THEN
            KJ=KJ+K
            GO TO 640
         END IF

c        IF(NWTR(NWT).eq.31011.or.NWTR(NWT).eq.31012) then
c           NREP=JR
c        END IF
      ELSE
C
C        COMPRESSED DATA
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
            WRITE(KNTN,*) 'BUSRP:'
            CALL BUERR(KERR)
            RETURN
         END IF
         IF(IWRD.GT.KBUFL) THEN
            KERR=26
            WRITE(KNTN,*) 'BUSRP:'
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
            WRITE(KNTN,*) 'BUSRP :'
            CALL BUERR(KERR)
            RETURN
         END IF
 402     CONTINUE
C
C        UNPACK JR DELAYED REPLICATION FACTOR
C
         IW=IWORDP
         IBT=IBITP
         CALL BUNPCK(NBPW,KBUFF,IR0,IW,IBT,NWTDW(NWT),KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KBUFF,IDWINC,IW,IBT,6,KERR)
         IF(KERR.GT.0) RETURN
         IF(IDWINC.GT.JBPW) THEN
            KERR=15
            WRITE(KNTN,*) ' BUSRP :'
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
            WRITE(KNTN,*) 'BUSRP :'
            CALL BUERR(KERR)
            RETURN
         ELSE
            IWTPR0=NWTRV(NWT)+IR0
C
            IF(IDWINC.EQ.0) THEN
               JR=IWTPR0
            ELSE
               IF(INCR.EQ.NMASK(IDWINC)) THEN
                  JR=NMASK(NWTDW(NWT))
                  KERR=19
                  WRITE(KNTN,*) 'BUSRP :'
                  CALL BUERR(KERR)
                  RETURN
               ELSE
                  JR= IWTPR0 + INCR
               END IF
            END IF
         END IF
      END IF
C
      IF(JR.EQ.0) THEN
         KJ=KJ+K
         GO TO 640
      END IF
C
      JRTK=JR*K+KJ1-K
      IF(JRTK.GT.JELEM) THEN
         KERR=19
         WRITE(KNTN,*) 'BUSRP :'
         CALL BUERR(KERR)
         WRITE(KNTN,*)'REPLICATION FACTOR =',JR
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C*          5.  GET NEXT K DESCRIPTORS.
C               -----------------------
 500  CONTINUE
C
      DO 501 J=1,K
C
      ILIST(J)=KSTACK(KJ+J)
C
 501  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.  ADD JR TIMES K DESCRIPTORS IN PLACE OF K
C               ----------------------------------------
C               DESCRIPTORS OBTAINED.
C               ---------------------
 600  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.1  PUSH DOWN DESCRIPTORS IN KSTACK FOR (JR-1)*K PLACES
C                ---------------------------------------------------
C                STARTING AT KJ1 AND ENDING AT KJ+K.
C                -----------------------------------
 610  CONTINUE
C
      JRKM1=(JR-1)*K
C
      DO 611 J=KJ1,KJ+K,-1
C
      KSTACK(J+JRKM1)=KSTACK(J)
C
 611  CONTINUE
C
C*          6.2  INSERT LIST IN THE STACK.
C                -------------------------
 620  CONTINUE
C
      DO 622 J=1,JR
C
      KJJM1K=KJ+(J-1)*K
C
      DO 623 J1=1,K
C
      KSTACK(KJJM1K+J1)=ILIST(J1)
C
 623  CONTINUE
 622  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.3  ADJUST DESCRIPTOR COUNT FOR LIST LENGTH.
C                ----------------------------------------
 630  CONTINUE
C
      KJ1 = KJ1  + (JR-1)*K
C
C     ------------------------------------------------------------------
C*          6.4  ADJUST NUMBER OF DATA DESCRIPTORS NOT PRESENT.
C                ----------------------------------------------
 640  CONTINUE
C
      IF(N221.NE.0)  N221= KJ1  - KJ + 1
C
C     ------------------------------------------------------------------
 700  CONTINUE
C
      RETURN
      END
