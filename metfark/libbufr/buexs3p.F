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

      SUBROUTINE BUEXS3P(KBUFL,KBUFF,KSUP,KSEC3,KERR)
C
C**** *BUEXS3P*
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
C          *CALL* *BUEXS3P( KBUFL,KBUFF,KSUP,KSEC3,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C        OUTPUT:
C               *KSUP*    -  ARRAY CONTAINING SUPLEMENTARY INFORMATION
C                         -  KSUP( 1) -- IDIM1, DIMENSION OF KSEC1
C                         -  KSUP( 2) -- IDIM2, DIMENSION OF KSEC2
C                         -  KSUP( 3) -- IDIM3, DIMENSION OF KSEC3
C                         -  KSUP( 4) -- IDIM4, DIMENSION OF KSEC4
C                         -  KSUP( 5) -- M (NUMBER OF ELEMENTS IN VALUES ARRAY,
C                                           FIRST INDEX)
C                         -  KSUP( 6) -- N (NUMBER OF SUBSETS,SECOND INDEX OF
C                                           VALUES ARRAY)
C                         -  KSUP( 7) -- JVC (NUMBER OF ELEMENTS IN CVAL ARRAY)
C                         -  KSUP( 8) -- TOTAL BUFR MESSAGE LENGTH IN BYTES
C                         -  KSUP( 9) -- IDIM0, DIMENSION OF KSEC0
C               *KSEC3*   -  ARRAY KSEC3 
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
      DIMENSION KBUFF(KBUFL),KSEC3(JSEC3)
      DIMENSION KSUP(JSUP)
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
C*          1.2.1  SET THE POINTERS NWPTB AND NBPTB TO
C                  -----------------------------------
C                  THE BEGINNING OF THE NEXT SECTION.
C                  ----------------------------------
      CALL BUNEXS(KSEC3(1))
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
C
C*          1.5    UNPACK INTEGER VALUE OF THE OCTET
C                  ---------------------------------
C                  CONTAINING FLAG BITS.
C                  --------------------
 150  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(4),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC3(4).'
         RETURN
      END IF

C
C     -----------------------------------------------------------------
C
      KSUP(3)= 4
      KSUP(5)= 0
      KSUP(6)= KSEC3(3)
C
      RETURN
      END
