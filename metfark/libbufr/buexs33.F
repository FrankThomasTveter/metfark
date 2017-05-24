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

      SUBROUTINE BUEXS33(KBUFL,KBUFF,KSEC3,KERR)
C
C**** *BUEXS33*
C
C
C     PURPOSE.
C     --------
C
C          EXPAND SECTION 3 OF BUFR MESSAGE.
C               (PRELIMINARY ITEMS)
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS33( KBUFL,KBUFF,KSEC3,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C        OUTPUT:
C               *KSEC3*
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C      -------
C
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUNEXS        - SET WORD AND BIT POINTERS AT THE BEGINING OF
C                          NEXT SECTION
C          BUNPCK        - UNPACKS BIT PATTERN
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
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSEC3(*)
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
C*          1.   EXPAND PRELIMINARY ITEMS OF SECTION 3.
C                --------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C*          1.1   SET THE POINTERS NWPT AND NBPT TO THE
C                 -------------------------------------
C                 BEGINING OF THE SECTION 3.
C                 --------------------------
 110  CONTINUE
C
      NWPT = NWPTB
      NBPT = NBPTB
C
C*          1.2   UNPACK LENGTH OF SECTION 3.
C                 ---------------------------
 120  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(1),NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) RETURN
C
C
C*          1.3    UNPACK ZERO BYTE AND PUT IT IN KSEC3(2).
C                  ----------------------------------------
 130  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(2),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) RETURN
C
C*          1.4    UNPACK NUMBER OF DATA SUB-SETS.
C                  -------------------------------
 140  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(3),NWPT,NBPT,16,KERR)
      IF(KERR.GT.0) RETURN
      IF(KSEC3(3).LE.0) THEN
         KERR=32
         WRITE(KNTN,*)  ' BUEXS3 :'
         CALL BUERR(KERR)
         RETURN
      END IF
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(4),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) RETURN
C     -----------------------------------------------------------------
C
      NWPT = NWPTB
      NBPT = NBPTB      
C
      RETURN
      END
