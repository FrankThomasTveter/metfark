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

      SUBROUTINE BUPKS(KBPW,KDEST,KSOURC,KWPT,KBPT,KSIZE,KSKIPB,K,KERR)
C
C**** *BUPKS*
C
C
C     PURPOSE.
C     --------
C          PURPOSE OF THIS ROUTINE IS TO PACK BIT STRING OF
C     KSIZE BITS, STARTED AT WORD KWPT OF ARRAY KSOURC AFTER
C     SKIPPINH KBPT BITS. RESULT IS PUT INTO KDEST. AT THE END
C     POINTERS KWPT AND KBPT ARE ADJUSTED.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUPKS(KBPW,KDEST,KSOURC,KWPT,KBPT,KSIZE,KSKIPB,K,KERR)*
C
C
C        INPUT :
C            KBPW      - NUMBER OF BITS PER COMPUTER WORD
C            KSOURC    - SOURCE (CONTINUOUS BIT STRING OF
C                          ARBITRARY LENGTH)
C            KWPT      - WORD POINTER
C            KBPT      - BIT POINTER
C            KSIZE     - NUMBER OF BITS USED FOR PACKING
C            KSKIPB    - NUMBER OF BITS TO SKIP BETWEEN ELEMENTS
C            K         - ITERATION
C
C        OUTPUT :
C            KDEST     - DESTINATION
C            KERR      - RETURN ERROR CODE
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
C          SBYTES     - PACK BIT PATHERN
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
      DIMENSION KDEST(*),KSOURC(*)
C
C     ------------------------------------------------------------------
C*          1.   EXTRACT BIT PATTERN.
C                --------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
      IF(KSIZE.GT.KBPW) THEN
         KERR= 34
         WRITE(KNTN,*)  'BUPKS :'
         CALL BUERR(KERR)
         RETURN
      END IF
C
      CALL SBYTES(KDEST,KSOURC,KBPT,KSIZE,KSKIPB,K)
C
C     ------------------------------------------------------------------
C*          1.1  UPDATE WORD AND BIT POINTERS.
C                -----------------------------
 110  CONTINUE
C
      KBPT = KBPT + K*(KSIZE+KSKIPB)
C
      IF(KBPT.GE.KBPW) THEN
         IW  = KBPT/ KBPW
         KBPT= KBPT - IW * KBPW
         KWPT= KWPT +IW
      END IF
C
      RETURN
      END
