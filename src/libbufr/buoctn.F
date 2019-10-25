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

      SUBROUTINE BUOCTN(KWPTB,KBPTB,KBUFL,KBUFF,KERR)
C
C**** *BUOCTN*
C
C
C     PURPOSE.
C     --------
C             CALCULATE NUMBER OF OCTETS FROM BIT POSITION DEFINED BY
C     KWPT,KBPT  AND KWPTB,KBPTB; NUMBER OF OCTETS MUST BE EVEN. IF IT IS
C     NEEDED PADING WITH 0 BIT PERFORMS. NUMBER OF OCTETS IS WRITTEN AT
C     BEGINIG OF THE SECTION.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUOCTN(KWPTB,KBPTB,KBUFL,KBUFF,KERR)*
C
C        INPUT :
C               *KWPTB*    - WORD POINTER TO THE BEGINING OF SECTION
C               *KBPTB*    - BIT  POINTER TO THE BEGINING OF SECTION
C        OUTPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KERR*    -  RETURNED ERROR CODE
C
C
C     *METHOD.
C      -------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUPCK       - PACK BIT PATHERN AND RESETS POINTERS
C          SBYTE       - PACK BIT PATHERN
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
C          M. DRAGOSAVAC    *ECMWF*       07/10/87.
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
C
      DIMENSION KBUFF(KBUFL)
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
C
C     ------------------------------------------------------------------
C*          1.    CALCULATE NUMBER OF OCTETS.
C                 ---------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
      IB   =(NWPT-1) * NBPW + NBPT
      IBB  =(KWPTB-1)* NBPW + KBPTB
C
      IDIFB= IB - IBB
C
      NOCT = IDIFB/8
      IBDW = IDIFB - NOCT*8
C
      IF(IBDW .NE.0)THEN
         NOCT=NOCT+1
         IBDW=8-IBDW
         CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,IBDW,KERR)
         IF(KERR.GT.0) THEN
            CALL BUERR(KERR)
            RETURN
         END IF
      END IF
C
C     ------------------------------------------------------------------
C*          2.  CHECK IF THERE ARE EVEN NUMBER OF OCTETS IN BLOCK.
C               --------------------------------------------------
 200  CONTINUE
C
      IF(NEDN.LE.3) THEN
         IF(MOD(NOCT,2).NE.0) THEN
             IBDW = 8
             CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,IBDW,KERR)
             IF(KERR.GT.0) THEN
               CALL BUERR(KERR)
               RETURN
             END IF
             NOCT = NOCT+1
         END IF
      END IF
C     ------------------------------------------------------------------
C*          3.  WRITE NUMBER OF OCTETS AT BEGINING OF BLOCK.
C               --------------------------------------------
 300  CONTINUE
C
      IBDW  = 24
      CALL SBYTE(KBUFF(KWPTB),NOCT,KBPTB,IBDW)
C
      RETURN
      END
