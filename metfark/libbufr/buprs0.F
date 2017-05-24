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

      SUBROUTINE BUPRS0(KSEC0)
C
C**** *BUPRS0*
C
C
C     PURPOSE.
C     --------
C           PRINT SECTION 0 OF BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRS0(KSEC0)*
C
C        INPUT :
C               *KSEC0*   -  ARRAY CONTAINING SECTION 0 INFORMATION
C                            KSEC0( 1)-- LENGTH OF SECTION 0 (BYTES)
C                            KSEC0( 2)-- TOTAL LENGTH OF BUFR MESSAGE (BYTES)
C                            KSEC0( 3)-- BUFR EDITION NUMBER
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
#     include "parameter.F"
#     include "bcomunit.F"
C
      DIMENSION KSEC0(JSEC0)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 0.
C                ----------------
 100  CONTINUE
C
      WRITE(KNTN,'(1H1)')
C
      WRITE(KNTN,'(1H ,A)')    '         BUFR SECTION 0    '
      WRITE(KNTN,'(1H )')
      WRITE(KNTN,'(1H ,A,I10)') 'LENGTH OF SECTION 0 (BYTES)         ',
     1                       KSEC0(1)
      WRITE(KNTN,'(1H ,A,I10)') 'TOTAL LENGTH OF BUFR MESSAGE (BYTES)',
     1                       KSEC0(2)
      WRITE(KNTN,'(1H ,A,I10)') 'BUFR EDITION NUMBER                 ',
     1                       KSEC0(3)
C
      RETURN
      END
