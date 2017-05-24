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

      SUBROUTINE BUAUG(KPT,KDLEN,KDATA,KJ,KY,KSTACK,KERR)
C
C**** *BUAUG*
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
C          *CALL* *BUAUG(KPT,KDLEN,KDATA,KJ,KY,KSTACK,KER)
C
C        INPUT :
C               *KPT*      - POINTER TOO KDATA ARRAY
C               *KDLEN*    -  DIMENSION OF KDATA ARRAY
C               *KDATA*    -  ARRAY CONTAINING DATA NEEDED FOR DATA DESCRIPTOR
C                            EXPANSION
C               *KJ*       - POINTER TO KSTACK ARRAY
C               *KY*       - OPERAND OF THE DATA DESCRIPTOR OPERATOR
C
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
C          GBYTE        - PACK BIT PATHERN
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
#     include "parameter.F"
#     include "bcomunit.F"
#     include "bcmwork.F"
#     include "bcmatb.F"
#     include "bcmatbc.F"
#     include "bcmtab.F"
#     include "bcmtabc.F"
#     include "bcmwt.F"
#     include "bcmwtc.F"
#     include "bcmbef.F"
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
      DIMENSION KSTACK(*)
      DIMENSION KDATA(KDLEN)
C     ------------------------------------------------------------------
C
C*          1.   UPDATE AUGMENTED TABLE B .
C                --------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
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
         M   = M   + 1
         NWTEN(NWT)=836970
         CWTEN(NWT)='REFERENCE VALUE'
         CWTU (NWT)='  '
         NWTDW(NWT)= KY
         NWTS (NWT)=0
         NWTRV(NWT)=0
         NWTR (NWT)=0
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
         KERR= 23
         WRITE(KNTN,*) ' BUAUG :'
         CALL BUERR(KERR)
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
         KPT=KPT+1
         NATBRV(NJA)=KDATA(KPT)
C
C           1.3.4 UPDATE WORKING TABLES
C                 ---------------------
 135     CONTINUE
C
C        CALL BUEPWT(KDD,KERR)
C        IF(KERR.GT.0) RETURN
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
      WRITE(KNTN,*) ' BUAUG :'
      CALL BUERR(KERR)
C
C     ------------------------------------------------------------------
 300  CONTINUE
C
      RETURN
C
      END
