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

      SUBROUTINE BUEXS5( KBUFL,KBUFF,KERR)
C
C**** *BUEXS5*
C
C
C     PURPOSE.
C     --------
C          EXPANDS SECTION 5 OF BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS5( KBUFL,KBUFF,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C        OUTPUT:
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
C          BUNPKS         - UNPACK BIT PATTERN IN REPEATED WAY
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
#     include "bcomwork.F"
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION IBUFR(4)
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
C
      CHARACTER*4 YBUFR
C
C     ------------------------------------------------------------------
C*          1.   EXPAND SECTION 5.
C                -----------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      NWPT = NWPTB
      NBPT = NBPTB
C
C
C*          1.2  UNPACK LAST FOUR OCTETS CONTAINING '7777'.
C                ------------------------------------------
C
      CALL BUNPKS(NBPW,KBUFF,IBUFR,NWPT,NBPT,8,0,4,KERR)
C
C*          1.3  CHECK IF THE LAST FOUR OCTETS ARE '7777'.
C                --------------------------------------
      YBUFR=CHAR(IBUFR(1))//CHAR(IBUFR(2))//
     1      CHAR(IBUFR(3))//CHAR(IBUFR(4))
      IF(YBUFR.NE.'7777') THEN
         KERR = 2
         WRITE(KNTN,'(1H ,A)') 'BUEXS5 :'
         CALL BUERR(KERR)
         RETURN
      END IF
C
C     -----------------------------------------------------------------
      RETURN
      END
