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

      SUBROUTINE BUSET(KPMISS,KPRUS,KOKEY,KOFL,K1,K2,K3,K4)
C
C**** *BUSET*
C
C
C     PURPOSE.
C     --------
C            SETS VARIABLE KPMISS,KPRUS,KOKEY,KOFL INTO COMMON BLOCK.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUSET(KPMISS,KPRUS,KOKEY,KOFL,K1,K2,K3,K4)*
C
C        INPUT :
C               *KPMISS*   -  INTEGER VARIABLE
C                            0 - DEFAULT, PAKS MAX VALUE FOR DATA WIDTH -1 BITS.
C
C                            1 - PAKS VALUE AS MISSING VALUE
C               *KPRUS*   -   AN INTEGER
C                            1 - ALWAYS RECALCULATE POINTERS
C               *KOKEY*    - 0 - DEFAULT, PACKS ECMWF RDB KEY
C                            1 - PACKS SECTION 2 IF NEEDED BUT NOT LENGTH
C                                OF BUFR MESSAGE IN KEY
C               *KOFL*    -  0 - NO CHECK ON VALUES
C                            1 - CHECK ON VALUES
C
C
C     METHOD.
C     -------
C
C           DURING PACKING THE VALUE TO BE PACKED CAN HAPPEN TO BE TOO BIG
C           TO FIT INTO CORRESPONDING DATA WIDTH. THIS SUBROUTINE  ALLOWS USER
C           TO CHOSE BETWEEN:
C
C            1)  PACK BIG VALUE AS MAX VALUE REPRESENTED WITH DATA WIDTH -1
C                BITS (DEFAULT)
C            2)  PACK BIG VALUE AS MISSING VALUE
C
C     EXTERNALS.
C     ----------
C
C          NONE
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
C          M. DRAGOSAVAC    *ECMWF*       15/01/95.
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
#     include "bcprq.F"
C
C     ------------------------------------------------------------------
C
C*          1.   SET CONSTANTS.
C                --------------
 100  CONTINUE
C
      IF(KPMISS.LT.0.AND.KPMISS.GT.1) KPMISS=0
      IF(KPRUS.LT.0.AND.KPRUS.GT.1)   KPRUS=0
      IF(KOKEY.LT.0.AND.KOKEY.GT.1)   NOKEY=0
      IF(KOFL.LT.0.AND.KOFL.GT.1)   NOFL=0
      NPMISS=KPMISS
      NPRUS=KPRUS
      NOKEY=KOKEY
      NOFL=KOFL
C
      RETURN
      END
