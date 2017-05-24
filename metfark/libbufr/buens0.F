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

      SUBROUTINE BUENS0( KSEC0,KBUFL,KBUFF,KERR)
C
C**** *BUENS0*
C
C
C     PURPOSE.
C     --------
C          PACK SECTION 0 OF BUFR MESSAGE.
C     PACKING BUFR EDITION 2.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUENS0( KSEC0,KBUFL,KBUFF,KERR)*
C
C
C        INPUT:
C               *KSEC0*   -  ARRAY CONTAINING SECTION 0 INFORMATION
C                            KSEC0( 1)-- LENGTH OF SECTION 0 (BYTES)
C                            KSEC0( 2)-- TOTAL LENGTH OF BUFR MESSAGE (BYTES)
C                            KSEC0( 3)-- BUFR EDITION NUMBER
C        OUTPUT:
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C     --------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUPKS         - PACK BIT PATHERN IN REPEATED WAY
C          BUPCK         - PACK BIT PATHERN
C          BUOCTN        - SET LENGTH OF SECTION
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/91.
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
      DIMENSION KSEC0(JSEC0)
      DIMENSION IBUFR(4)
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
C
      DATA IBUFR/66,85,70,82/
C
C     B IS CCITT.5 DECIMAL 66
C     U IS CCITT.5 DECIMAL 85
C     F IS CCITT.5 DECIMAL 70
C     R IS CCITT.5 DECIMAL 82
C
C
C     ------------------------------------------------------------------
C*          1. PACK SECTION 0.
C              ---------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C
C*          1.1  INITIALIZE WORKING POINTERS NWPT AND NBPT.
C                ------------------------------------------
      NWPT = 1
      NBPT = 0
C
C*          1.2  PACK FIRST FOUR OCTETS CONTAINING *BUFR*.
C                -------------------------------------------
C
      CALL BUPKS(NBPW,KBUFF(NWPT),IBUFR,NWPT,NBPT,8,0,4,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)'ERROR PACKING FIRST FOUR OCTETS',
     1' OF BUFR MESSAGE.'
         RETURN
      END IF
C
      IF(KSEC0(3).LE.1) GO TO 200
C
C*          1.2 PACK TOTAL LENGTH OF BUFR MESSAGE.
C               ------------------------------------
 120  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING TOTAL LENGTH OF BUFR MESSAGE'
         WRITE(KNTN,*) 'IN SECTION 0.'
         RETURN
      END IF
C
C*          1.3 PACK BUFR EDITION NUMBER (IT IS 8TH BYTE ).
C               ---------------------------------------------
 130  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC0(3),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING BUFR EDITION NUMBER IN SECTION 0.'
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C
 200  CONTINUE
C
      RETURN
      END
