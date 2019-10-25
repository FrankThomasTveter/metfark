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

      SUBROUTINE BUEXS0( KBUFL,KBUFF,KSUP,KSEC0,KERR)
C
C**** *BUEXS0*
C
C
C     PURPOSE.
C     --------
C          EXPANDS SECTION 0 OF BUFR MESSAGE. SAVES BUFR EDITION NUMBER
C     AND TOTAL LENGTH OF BUFR MESSAGE (BYTES).
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS0( KBUFL,KBUFF,KSUP,KSEC0,KERR)*
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
C               *KSEC0*   -  ARRAY CONTAINING SECTION 0 INFORMATION
C                            KSEC0( 1)-- LENGTH OF SECTION 0 (BYTES)
C                            KSEC0( 2)-- TOTAL LENGTH OF BUFR MESSAGE (BYTES)
C                            KSEC0( 3)-- BUFR EDITION NUMBER
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
C          BUNPKS          - UNPACK BIT PATTERN IN REPEATED WAY
C          BUNPCK          - UNPACK BIT PATTERN
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
      DIMENSION KBUFF(KBUFL),KSUP(*),KSEC0(*)
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
C*          1.   EXPAND SECTION 0.
C                -----------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C
C*          1.1  INITIALIZE WORKING POINTERS NWPT AND NBPT.
C                ------------------------------------------
      IWPT = 0
      IBPT = 0
      NWPT = 1
      NBPT = 0
C
C*          1.2  UNPACK FIRST FOUR OCTETS CONTAINING *BUFR*.
C                -------------------------------------------
C
      CALL BUNPKS(NBPW,KBUFF,IBUFR,NWPT,NBPT,8,0,4,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING FIRST FOUR OCTETS OF BUFR
     1 MESSAGE.'
         RETURN
      END IF
C
C*          1.3  CHECK IF FIRST FOUR OCTETS ARE 'BUFR'.
C                --------------------------------------
      YBUFR=CHAR(IBUFR(1))//CHAR(IBUFR(2))//
     1      CHAR(IBUFR(3))//CHAR(IBUFR(4))
      IF(YBUFR.NE.'BUFR') THEN
         KERR = 1
         CALL BUERR(KERR)
         RETURN
      END IF
C
C*          1.4 UNPACK BUFR EDITION NUMBER (IT IS 8TH BYTE ).
C               ---------------------------------------------
 140  CONTINUE
C
      IWPT=56/NBPW+1
      IBPT=56-(IWPT-1)*NBPW
C
      CALL BUNPCK(NBPW,KBUFF,KSEC0(3),IWPT,IBPT,8,KERR)
      IF(KERR.GT.0) THEN
      	     WRITE(KNTN,*)  'ERROR UNPACKING KSEC0(3).'
         RETURN
      END IF
C
      IF(KSEC0(3).LE.1) GO TO 170
C
C*          1.5 UNPACK TOTAL LENGTH OF BUFR MESSAGE.
C               ------------------------------------
 150  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC0(2),NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC0(2).'
         RETURN
      END IF
C
C*          1.6 UNPACK BUFR EDITION NUMBER.
C               ---------------------------
 160  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC0(3),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC0(3).'
         RETURN
      END IF
C
C
C*          1.7 SET LENGTH OF SECTION 0.
C               ------------------------
 170  CONTINUE
C
      KSEC0(1)= 4
      IF(KSEC0(3).GT.1) KSEC0(1)= 8
C
C*          1.8 SET SUPPLEMENTARY INFORMATION.
C               ------------------------------
 180  CONTINUE
C
      KSUP (9)= 3
C
      RETURN
C     -----------------------------------------------------------------
C
      END
