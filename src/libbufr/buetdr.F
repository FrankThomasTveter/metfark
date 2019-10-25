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

      SUBROUTINE BUETDR(KJ,KJ1,KDD,KSTACK,KERR)
C
C**** *BUETDR*
C
C
C     PURPOSE.
C     --------
C          SOLVE BUFR TABLE D REFERENCE.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUETDR(KJ,KJ1,KDD,KSTACK,KERR)*
C
C        INPUT :
C                 *KDD*      - DATA DESCRIPTOR
C        OUTPUT:
C                 *KJ*       - POINTER TO KSTACK ARRAY
C                 *KJ1*      - POINTER TO LAST ELEMENT IN KSTACK
C                 *KSTACK*   - LIST OF DATA DESCRIPTORS
C                 *KERR*     - RETURN ERROR CODE
C
C
C     METHOD.
C     -------
C
C           NONE.
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
#     include "bcmwt.F"
#     include "bcmwtc.F"
#     include "bcmtab.F"
#     include "bcmtabc.F"
C
      CHARACTER CTABBEN*64,CTABBU*24
C
      DIMENSION ILIST(JELEM),KSTACK(*)
C
C     ------------------------------------------------------------------
C
C*          1.   OBTAIN LIST OF DESCRIPTORS FROM BUFR TABLE D.
C                ---------------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      DO 110 J=1,JTAB
C
      IF(KDD.EQ.NTABDTR(J)) THEN
         I=J
         GO TO 120
      END IF
C
 110  CONTINUE
C
      KERR=20
      CALL BUERR(KERR)
      WRITE(KNTN,*)  'REFERENCE INVOLVED ',KDD
      RETURN
C
 120  CONTINUE
C
      J1=NTABDST(I)
      J2=NTABDL (I)
      J3=0
C
      DO 121 J=J1,J1+J2-1
C
      J3 = J3 +1
      ILIST(J3) = NTABDSQ(J)
C
 121  CONTINUE
C
C     ------------------------------------------------------------------
C*          2.  PUSH DOWN DATA DESCRIPTION DESCRIPTORS
C               --------------------------------------
C               TO MAKE ROOM FOR LIST.
C               ----------------------
 200  CONTINUE
C
      J2M1=J2-1
C
      DO 210 J=KJ1,KJ+1,-1
C
      KSTACK(J+J2M1) = KSTACK(J)
C
 210  CONTINUE
C
C     ------------------------------------------------------------------
C*          3.  INSERT LIST IN PLACE OF SEQUENCE DESCRIPTORS.
C               ---------------------------------------------
 300  CONTINUE
C
      KJM1=KJ-1
C
      DO 310 J=1,J3
C
      KSTACK(KJM1+J)= ILIST(J)
C
 310  CONTINUE
C
C     ------------------------------------------------------------------
C*          4.  ADJUST DESCRIPTOR COUNT FOR LIST LENGTH.
C               ----------------------------------------
 400  CONTINUE
C
      KJ  = KJ  - 1
      KJ1 = KJ1 +J3 -1
C     ------------------------------------------------------------------
C*          4.1  ADJUST NUMBER OF DATA DESCRIPTORS NOT PRESENT.
C                ----------------------------------------------
 610  CONTINUE
C
      IF(N221.NE.0)  N221= KJ1  - KJ + 1
C     -----------------------------------------------------------------
 500  CONTINUE
C
      RETURN
C
      END
