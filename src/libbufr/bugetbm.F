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

       SUBROUTINE BUGETBM(KBUFL,KBUFF,KSEC3,KBMP,KBMPL,KBV,KERR)
C
C**** *BUGETBM*
C
C
C     PURPOSE.
C     --------
C
C          CREATE BIT MAP TO RESOLVE MARKER OPERATORS.
C
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUGETBM(KBUFL,KBUFF,KSEC3,KBMP,KBMPL,KBV,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KSEC3*   -  ARRAY CONTAINING SECTION 3 INFORMATION
C                            KSEC3( 1)-- LENGTH OF SECTION 3 (BYTES)
C                            KSEC3( 2)-- RESERVED
C                            KSEC3( 3)-- NUMBER OF SUBSETS
C                            KSEC3( 4)-- FLAG (DATA TYPE,DATA COMPRESSION)
C               *KBMP     -  BIT MAP POINTER TO THE FIRST DATA PRESENT
C                            INDICATOR
C               *KBMPL    -  NUMBER OF  DATA PRESENT INDICATORS
C
C        OUTPUT:
C               *KBV*     -  BIT MAP ARRAY
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C     -------
C
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
      DIMENSION KBUFF(KBUFL),KBV(*)
      DIMENSION KSEC3(JSEC3)
C
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
      DIMENSION IMASK(8),ILIST(JELEM)
C
      DATA IMASK /1,2,4,8,16,32,64,128/
C
C
C     ------------------------------------------------------------------
C*          1.  CALCULATE WORD AND BIT POINTER TO FIRST
C               ---------------------------------------
C               DATA PRESENT INDICATOR.
C               -----------------------
 100  CONTINUE
C
C
      IF(KERR.GT.0) RETURN
C
      IB=0
      IF(IAND(KSEC3(4),IMASK(7)).NE.0) IB=1
C
      IF(IB.EQ.0) THEN
C
C        FOR UNCOMPRESSED DATA
C
         IBIT=NWPTB*NBPW+32+NBPTB
         DO 101 I=1,KBMP-1
         IBIT=IBIT+NWTDW(I)
 101     CONTINUE
C
         IWPT=IBIT/NBPW
         IBPT=IBIT-IWPT*NBPW
      ELSE
C
C        FOR COMPRESSED DATA
C
         IBIT=32+NBPTB
         IWORD=IBIT/NBPW
C
         NWORDP(1)=NWPTB+IWORD
         NBITP (1)=IBIT-IWORD*NBPW
C
         DO 102 I=2,KBMP
C
         IF(NWTDW(I-1).EQ.0) THEN
            NBITP(I)=NBITP(I-1)
            NWORDP(I)=NWORDP(I-1)
            GO TO 102
         END IF
C
         IWRD=NWORDP(I-1)
         IBTP=NBITP (I-1)
C
         IBTP=IBTP+NWTDW(I-1)
         IF(IBTP.GE.NBPW) THEN
            IW=IBTP/NBPW
            IBTP=IBTP-IW*NBPW
            IWRD=IWRD+IW
         END IF
         CALL BUNPCK(NBPW,KBUFF,IDWINC,IWRD,IBTP,6,KERR)
         IF(KERR.GT.0) RETURN
         IF(IDWINC.GT.JBPW) THEN
            KERR=15
            WRITE(KNTN,*)  'BUPMRK :'
            CALL BUERR(KERR)
            RETURN
         END IF
C
         IF(CWTU(I-1).EQ.'CCITTIA5') THEN
            NWTIWS(I-1)=NWTDW(I-1)+6+N*IDWINC*8
         ELSEIF(NWTEN(I-1).EQ.-999) THEN
            NWTIWS(I-1)=NWTDW(I-1)+6+N*IDWINC*8
         ELSE
            NWTIWS(I-1)=NWTDW(I-1)+6+N*IDWINC
         END IF
C
         IBIT = NBITP(I-1) + NWTIWS(I-1)
         IWORD= IBIT/NBPW
C
         NBITP (I)= IBIT - IWORD*NBPW
         NWORDP(I)= NWORDP(I-1) + IWORD
 102     CONTINUE
C
         IBPT=NBITP (KBMP)
         IWPT=NWORDP(KBMP)
      END IF
C
C*          2.  GET BIT MAP FROM DATA SECTION.
C               ------------------------------
 200  CONTINUE
C
      IF(IB.EQ.0) THEN
         CALL GBYTES(KBUFF(IWPT),KBV,IBPT,1,0,KBMPL)
      ELSE
         DO 201 I=1,KBMPL
         CALL BUNPCK(NBPW,KBUFF,IR0,IWPT,IBPT,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KBUFF,IDWINC,IWPT,IBPT,6,KERR)
         IF(KERR.GT.0) RETURN
         IF(IDWINC.GT.JBPW) THEN
            KERR=15
            WRITE(KNTN,*)  'BUPMRK:'
            CALL BUERR(KERR)
            RETURN
         END IF
C
         DO 202 K=1,KSEC3(3)
         ILIST(K)=0
 202     CONTINUE
C
         IF(IDWINC.NE.0) THEN
C
C           UNPACK INCREMENTS
C
            CALL BUNPKS(NBPW,KBUFF,ILIST,IWPT,IBPT,
     1                  IDWINC,0,KSEC3(3),KERR)
            IF(KERR.GT.0) RETURN
         END IF
C
         KBV(I)=IR0
         IF(IDWINC.NE.0) KBV(I)=IR0+ILIST(1)
 201     CONTINUE
C
      END IF
C
      RETURN
      END
