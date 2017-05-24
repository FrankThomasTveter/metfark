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

      SUBROUTINE BUREPC(KPT,KDLEN,KDATA,KJ,KJ1,KDD,KSTACK,KERR)
C
C**** *BUREPC*
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
C          *CALL* *BUREPC(KPT,KDLEN,KDATA,KJ,KJ1,KDD,KSTACK,KERR)*
C
C        INPUT :
C               *KPT*      - POINTER TOO KDATA ARRAY
C               *KDLEN*    -  DIMENSION OF KDATA ARRAY
C               *KDATA*    -  ARRAY CONTAINING DATA NEEDED FOR DATA DESCRIPTOR
C                            EXPANSION
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
C          BUETDR            - RESOLVE TABLE D REFERENCE
C          BUEPWT            - UPDATE WORKING TABLES
C          GBYTE             - UNPACK BIT PATHERN
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
C
C
#     include "parameter.F"
#     include "bcomunit.F"
#     include "bcmwork.F"
#     include "bcmwt.F"
#     include "bcmwtc.F"
#     include "bcmbef.F"
C
      CHARACTER CWTEN*64,CWTU*24
      DIMENSION ILIST(JELEM)
      DIMENSION KSTACK(*)
      DIMENSION KDATA(KDLEN)
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
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
            CALL BUETDR(KJ,KJ1,KDD,KSTACK,KERR)
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
            CALL BUOPERC(KPT,KDLEN,KDATA,KJ,KDD,KSTACK,KERR)
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
      IF(KDD.EQ.31031.OR.KDD.EQ.31192) THEN
         NWT=NWT+1
         NWTR(NWT)=KDD
         NWTS(NWT)=0
         NWTRV(NWT)=0
         NWTDW(NWT)=1
         M=M+1
      ELSEIF(KDD.EQ.33007.OR.KDD.EQ.63192) THEN
         NWT=NWT+1
         NWTR(NWT)=KDD
         NWTS(NWT)=0
         NWTRV(NWT)=0
         NWTDW(NWT)=7
         M=M+1
      ELSE
         CALL BUEPWTC (KDD,KERR)
         IF(KERR.GT.0) RETURN
         IF(KDD.EQ.031011.OR.KDD.EQ.031012) RETURN
      END IF
C
C     ------------------------------------------------------------------
C
C*          4.  STORE JR, THE REPLICATION FACTOR FROM DATA.
C               ------------------------------------------------------
 400  CONTINUE
C
      KPT=KPT+1
      IF(KPT.GT.KDLEN) THEN
         KERR=31
         CALL BUERR(KERR)
         RETURN
      END IF
      JR=KDATA(KPT)
C      IF(JR.EQ.0) THEN
C         KERR=18
C         CALL BUERR(KERR)
C         RETURN
C      END IF
C
      IF(JR.EQ.0) THEN
         KJ=KJ+K
         GO TO 640
      END IF
C
      JRTK=JR*K+KJ1-K
      IF(JRTK.GT.JELEM) THEN
         KERR=30
         CALL BUERR(KERR)
         RETURN
      END IF
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
         CALL BUOPERC(KPT,KDLEN,KDATA,KJ,KSTACK(KJ),KSTACK,KERR)
         IF(KERR.GT.0) THEN
            CALL BUERR(KERR)
            RETURN
         END IF
      END IF
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
