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

      SUBROUTINE BUNEXS( KLEN)
C
C**** *BUNEXS*
C
C
C     PURPOSE.
C     --------
C           SETS WORD (NWPT) AND BIT(NBPT) POINTERS AT THE BEGINING
C     OF NEXT SECTION OF BUFR MESSAGE.
C
C**   INTERFACE.
C     ----------
C
C            *CALL* *BUNEXS( KLEN)*
C
C        INPUT :
C                 *KLEN*     - LENGTH OF SECTION IN BYTES
C
C     METHOD.
C     -------
C
C          LENGTH OF EACH SECTION IS ADDED UP. WORD AND BIT POINTERS
C     POINTING TO THE BEGINING OF NEXT SECTION ARE CALCULATED.
C
C     EXTERNALS.
C     ----------
C
C           NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       16/01/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
C
#     include "parameter.F"
#     include "bcomunit.F"
#     include "bcomwork.F"
C
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
C*          1.   SET UP POINTERS AT THE BEGINING OF THE NEXT SECTION.
C                -----------------------------------------------------
 100  CONTINUE
C
      IBIT = (NWPT - 1)*NBPW +NBPT +KLEN * 8 - 24
C
      NWPTB = IBIT/NBPW + 1
      NBPTB = IBIT - (NWPTB - 1)*NBPW
C
      RETURN
      END
