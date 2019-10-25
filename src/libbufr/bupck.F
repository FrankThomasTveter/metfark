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

      SUBROUTINE BUPCK(KBPW,KD,KS,KWPT,KBPT,KSI,KERR)
C
C**** *BUPCK*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO PACK VALUE *KS* IN
C         *KSI* BITS, STARTED AT WORD KWPT OF ARRAY *KD* AFTER
C         SKIPPING NBPT BITS.  AT THE END
C         POINTERS *KWPT* AND *KBPT* ARE ADJUSTED.
C
C**   INTERFACE.
C     ----------
C
C     *CALL* *BUPCK(KBPW,KD,KS,KWPT,KBPT,KSI,KERR)*
C
C        INPUT :
C            *KS*    - SOURCE
C            *KWPT*  - WORD POINTER
C            *KWPT*  - BIT POINTER
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
C
C        OUTPUT :
C            *KD*    - DESTINATION ARRAY.
C            *KERR*  - RETURN ERROR CODE
C
C     *METHOD.
C      -------
C
C            NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C            *CALL SBYTE(KD,KS,KBPT,KSI)*
C
C            *KD*    - DESTINATION ARRAY.
C            *KS*    - SOURCE
C            *KBPT*  - POINTER TO BIT IN THE KD(KWPT)
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
C
C
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
C          M. DRAGOSAVAC    *ECMWF*       09/06/86.
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
#     include "bcmwork.F"
C
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
      DIMENSION KD(*)
      DIMENSION IMAXV(32)
C
      DATA IMAXV/1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,
     1  16383,32767,65535,131071,262143,524287,1048575,2097151,
     2  4194305,8388607,16777215,33554431,671108863,134217727,
     3  268435455,536870911,1073741823,2147483647,2147483647/
C
C     ------------------------------------------------------------------
C*          1.   SET UP BIT PATTERN.
C                -------------------
C
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
      IF(KS.EQ.NVIND) KS=IMAXV(KSI)
C
      IF(KS.GT.IMAXV(KSI)) THEN
         KERR=28
         RETURN
      END IF
C
      CALL SBYTE(KD,KS,KBPT,KSI)
C
C     ------------------------------------------------------------------
C*          1.1  UPDATE WORD AND BIT POINTERS.
C                -----------------------------
 110  CONTINUE
C
      KBPT = KBPT + KSI
C
      IF(KBPT.GE.KBPW) THEN
         KBPT= KBPT - KBPW
         KWPT= KWPT + 1
      END IF
C
      RETURN
      END
