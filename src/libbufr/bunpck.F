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

      SUBROUTINE BUNPCK(KBPW,KSOURC,KDEST,KWPT,KBPT,KSIZE,KERR)
C
C**** *BUNPCK*
C
C
C     PURPOSE.
C     --------
C          PURPOSE OF THIS ROUTINE IS TO UNPACK BIT STRING OF
C     KSIZE BITS, STARTED AT WORD KWPT OF ARRAY KSOURC AFTER
C     SKIPPING KBPT BITS. RESULT IS PUT INTO KDEST. AT THE END
C     POINTERS KWPT AND KBPT ARE ADJUSTED.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUNPCK(KBPW,KSOURC,KDEST,KWPT,KBPT,KSIZE,KERR)*
C
C        INPUT :
C              *KBPW*      - NUMBER OF BITS IN COMPUTER WORD
C              *KSOURC*    - SOURCE (CONTINUOUS BIT STRING OF
C                            ARBITRARY LENGTH)
C              *KWPT*      - WORD POINTER
C              *KBPT*      - BIT POINTER
C              *KSIZE*     - NUMBER OF BITS TO BE EXTRACTED
C        OUTPUT:
C              *KDEST*     - DESTINATION
C              *KERR*      - RETURN ERROR CODE
C
C     METHOD.
C     -------
C
C            NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C          GBYTE     - UNPACK BIT PATTERN
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
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
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
      DIMENSION KSOURC(*)
C
C     ------------------------------------------------------------------
C*          1.   EXTRACT BIT PATTERN.
C                --------------------
 100  CONTINUE
C
      IF(KERR.NE.0) RETURN
C
      IF(KSIZE.GT.KBPW) THEN
         KERR=13
         CALL BUERR(KERR)
         RETURN
      END IF
C
      CALL GBYTE(KSOURC(KWPT),KDEST,KBPT,KSIZE)
C
C     ------------------------------------------------------------------
C*          1.1  UPDATE WORD AND BIT POINTERS.
C                -----------------------------
 110  CONTINUE
C
      KBPT = KBPT + KSIZE
C
      IF(KBPT.GE.KBPW) THEN
         KBPT= KBPT -  KBPW
         KWPT= KWPT +1
      END IF
C
      RETURN
      END
