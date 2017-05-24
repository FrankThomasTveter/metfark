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

      SUBROUTINE BUUPWT(KDD,KELEM,KERR)
C
C**** *BUUPWT*
C
C
C     PURPOSE.
C     --------
C          UPDATES WORKING TABLES SETTING ELEMENT NAME,UNIT,SCALE,
C     REFERENCE VALUE AND DATA WIDTH.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUUPWT(KDD,KELEM,KERR)*
C
C        INPUT :
C               *KDD*     -  DATA DESCRIPTOR
C               *KELEM*   -  DIMENSION OF CNAMES, CUNITS ARRAY
C        OUTPUT:
C               *KERR*    -  RETURN ERROR CODE
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
#     include "bcomwt.F"
#     include "bcomwtc.F"
#     include "bcomtab.F"
#     include "bcomtabc.F"
#     include "bcomatb.F"
#     include "bcomatbc.F"
#     include "bcombef.F"
C
      CHARACTER CATBEN*64,CWTEN*64,CTABBEN*64
      CHARACTER CATBU*24,CWTU*24,CTABBU*24
      CHARACTER YCH6*6
C
C
C     ------------------------------------------------------------------
C*          1.   UPDATE WORKING TABLE.
C                ---------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      ICLASS=KDD/1000
      IYYY  =KDD-ICLASS*1000+1
      ICLASS=ICLASS+1
C
C*          1.1  ASSOCIATED FIELD ?
C                ------------------
 110  CONTINUE
C
      IF(NFD.EQ.0) GO TO 140
C
C*          1.2  UNITS ELEMENT DESCRIPTOR ?
C                --------------------------
 120  CONTINUE
C
      I=NTABP(ICLASS,IYYY)
      IF(I.EQ.0) THEN
         KERR=23
         WRITE(KNTN,*)'BUUPWT : ',KDD
         CALL BUERR(KERR)
         DO 1 IQ=1,JELEM
         NSTACK(IQ)=0.
 1       CONTINUE
         RETURN
      END IF
C
      IF(NTABBTR(I)/1000.EQ.31)  GO TO 140
c     IF(CTABBU(I)(1:4).EQ.'CODE') GO TO 140
c     IF(CTABBU(I)(1:4).EQ.'FLAG') GO TO 140
c     IF(CTABBU(I)(1:3).EQ.'NUM' ) GO TO 140
C
C*          1.3   ADD SPECIAL ENTRY TO WORKING TABLE.
C                 -----------------------------------
 130  CONTINUE
C
      IF(N40.NE.0) THEN
         NWT=NWT+1
         CWTEN(NWT)='ASSOCIATED FIELD'
         CWTU (NWT)=' '
         NWTDW(NWT)=N40
         NWTR (NWT)= 999999
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
         NWTRV(NWT)= 0
      ELSE
         DO I=1,NFD
         NWT=NWT+1
         CWTEN(NWT)='ASSOCIATED FIELD'
         CWTU (NWT)=' '
         NWTDW(NWT)=NAFDWA(I)
         NWTR (NWT)= 999999
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
         NWTRV(NWT)= 0
         END DO
      END IF
C
C*                UPDATE CNAMES AND CUNITS
C
      M=M+1
      IF(M.GT.KELEM) THEN
         KERR=25
         WRITE(KNTN,*)'BUUPWT:'
         CALL BUERR(KERR)
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C*          1.4   SEARCH AUGMENTED TABLE *B ENTRIES .
C                 -----------------------------------
 140  CONTINUE
C
      DO 141 J=1,NJA
C
      IF(NATBTR(J).EQ.KDD) THEN
         II=J
C
C*             MODIFY ENTRY FOR OPERATOR IN FORCE.
C              -----------------------------------
C
C*             ADD ENTRY TO WORKING TABLE.
C              ---------------------------
C
         IX=KDD/1000
         IF(N40.NE.0.AND.IX.NE.31) THEN
            NWT=NWT+1
            NWTR (NWT) = KDD
            NWTS (NWT) = 0
            NWTRV(NWT) = 0
            NWTDW(NWT) = N40
         ELSE
            NWT=NWT+1
            NWTR (NWT) = KDD
            NWTS (NWT) = NATBS (II) + NSCAM + NSCAM07
            NWTRV(NWT) = NINT(NATBRV(II)*10.**N07)
            NWTDW(NWT) = NATBDW(II) + NDWINC + NDWINC07
            NWTEN(NWT) = 0

         END IF
C
C        CHECK IF DATA ARE PRESENT IN DATA SECTION.
C
         IF(N221.NE.0) THEN
            IX=KDD/1000
            IF(IX.GT.9.AND.IX.NE.31) NWTDW(NWT)=0
            N221=N221-1
         END IF
C
c        IF(NWTR(NWT).EQ.31011.OR.NWTR(NWT).EQ.31012) THEN
c           OREP=.true.
c        END IF
c        if(OREP) then
c           if(NREP.GT.1.and.NWTR(NWT-1).ne.31011.and.
c    1         NWTR(NWT-1).ne.31012) then
c              NWTDW(NWT)=0
c              NREP=NREP-1
c              IF(NREP.le.1) then
c                 NREP=0
c                 OREP=.false.
c              END IF
c           END IF
c        END IF

C
C*            UPDATE M, CNAMES, CUNITS.
C             -------------------------
C
         CWTEN(NWT) = CATBEN(II)
         CWTU (NWT) = CATBU (II)
         NWTEN(NWT) = 0
         IF(CATBU(II)(1:3).EQ.'CCI') THEN
            NWTEN(NWT)=658367
         ELSEIF(N40.NE.0.AND.IX.NE.31.AND.
     1      CATBU(II)(1:3).NE.'COD'.AND.
     2      CATBU(II)(1:3).NE.'FLA') THEN
            NWTEN(NWT) = -999
         ENDIF
C
         M = M + 1
         IF(M.GT.KELEM) THEN
            KERR=25
            WRITE(KNTN,*)'BUUPWT:'
            CALL BUERR(KERR)
            RETURN
         END IF
C
         RETURN
      END IF
C
 141  CONTINUE
C
C*          1.5  GET TABLE *B ENTRY .
C                ---------------------
 150  CONTINUE
C
      I=NTABP(ICLASS,IYYY)
      IF(I.EQ.0) THEN
         KERR=23
         WRITE(KNTN,*)'BUUPWT : ',KDD
         CALL BUERR(KERR)
         DO 2 IQ=1,JELEM
         NSTACK(IQ)=0.
 2       CONTINUE
         RETURN
      END IF
C
 155  CONTINUE
C
C     -----------------------------------------------------------------
C*          1.6   MODIFY ENTRY FOR OPERATOR IN FORCE.
C                 -----------------------------------
 160  CONTINUE
C
C*                ADD ENTRY TO WORKING TABLE.
C                 ---------------------------
C
      IF(CTABBU(I)(1:4).EQ.'CODE'.OR.
     1   CTABBU(I)(1:4).EQ.'FLAG') THEN
         NWT=NWT+1
         NWTR (NWT) = KDD
         NWTS (NWT) = NTABBS (I) 
         NWTRV(NWT) = NTABBRV(I)
         NWTDW(NWT) = NTABBDW(I)
      ELSEIF(N40.NE.0.AND.IX.NE.31) THEN
         NWT=NWT+1
         NWTR (NWT) = KDD
         NWTS (NWT) = 0
         NWTRV(NWT) = 0
         NWTDW(NWT) = N40
      ELSE 
         NWT=NWT+1
         NWTR (NWT) = KDD
         NWTS (NWT) = NTABBS (I) + NSCAM + NSCAM07
         NWTRV(NWT) = NINT(NTABBRV(I)*10.**N07)
         NWTDW(NWT) = NTABBDW(I) + NDWINC + NDWINC07

      END IF
C
C     CHECK IF DATA ARE PRESENT IN DATA SECTION.
C
      IF(N221.NE.0) THEN
         IX=KDD/1000
         IF(IX.GT.9.AND.IX.NE.31) NWTDW(NWT)=0
         N221=N221-1
      END IF
C
c     IF(NWTR(NWT).EQ.31011.OR.NWTR(NWT).EQ.31012) THEN
c        OREP=.true.
c     END IF
c     IF(OREP) THEN
c        if(NREP.GT.1.and.NWTR(NWT-1).ne.31011.and.
c    1      NWTR(NWT-1).ne.31012) THEN
c           NWTDW(NWT)=0
c           NREP=NREP-1
c           IF(NREP.le.1) THEN
c              NREP=0
c              OREP=.false.
c           END IF
c        END IF
c      END IF

C
 175  CONTINUE
C
C     ------------------------------------------------------------------
C*          1.8  UPDATE M, CNAMES, CUNITS.
C                -------------------------
 180  CONTINUE
C
      CWTEN(NWT) = CTABBEN(I)
      CWTU (NWT) = CTABBU(I)
      NWTEN(NWT) = 0
      IF(CTABBU(I)(1:3).EQ.'CCI') THEN
         NWTEN(NWT)=658367
         IF(N08.NE.0) THEN
            NWTDW(NWT) = N08*8
         END IF
      ELSEIF(N40.NE.0.AND.IX.NE.31.AND.
     1       CTABBU(I)(1:3).NE.'COD'.AND.
     2       CTABBU(I)(1:3).NE.'FLA') THEN
         NWTEN(NWT) = -999
      ENDIF
C
       M = M + 1
       IF(M.GT.KELEM) THEN
          KERR=25
          WRITE(KNTN,*) 'BUUPWT:'
          CALL BUERR(KERR)
          RETURN
       END IF
C
C     -----------------------------------------------------------------
C
 200  CONTINUE
C
      RETURN
      END
