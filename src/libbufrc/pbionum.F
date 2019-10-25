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

      INTEGER FUNCTION PBIONUM(KONOFF)
C
C---->
C**** PBIONUM
C
C     Purpose
C     -------
C
C     Returns current PBIO_CYCLE version number.
C
C
C     Interface
C     ---------
C
C     INUM = PBIONUM(KONOFF)
C
C     Input
C     -----
C
C     KONOFF - switch for displayed message
C              = 0 if display is required on the first call
C              = non-zero if display is not required
C
C
C     Output
C     ------
C
C     Returns a 6-digit version number, aaabbc, where:
C       aaa = 3-digit major number
C       bb  = 2-digit minor number
C       c   = 1-digit spare number (normally 0)
C     
C
C     Method
C     ------
C
C     Reads a 6-digit version number from the environment variable
C     PBIO_CYCLE. If this does not give a 6-digit number, an internal
C     hard-coded default value is used.
C
C     On the first call, the function (optionally) displays a message:
C
C       **************************************
C       * PBIO_CYCLE version number = aaabbc *
C       **************************************
C     
C
C     Externals
C     ---------
C
C     None.
C     
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     May 1998
C
C
C----<
C ------------------------------------------------------------------
C*    Section 0.   Variables.
C ------------------------------------------------------------------
C
C
      IMPLICIT NONE
C
#include "grprs.h"
C
C     Function arguments
C
      INTEGER KONOFF
C
C     Local variables
C
      INTEGER INUMBER, ICOUNT, IOFFSET
      SAVE INUMBER, ICOUNT
      CHARACTER*38 CMESS
      CHARACTER*20 YNUMBER
C
      DATA INUMBER/000010/, ICOUNT/0/
      DATA CMESS/'* PBIO_CYCLE version number = ****** *'/
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
      IF( ICOUNT.EQ.0 ) THEN
C
C       See if the environment variable has an override value
C
        CALL GETENV( 'PBIO_CYCLE', YNUMBER)
        IOFFSET = INDEX( YNUMBER, ' ')
        IF( IOFFSET.EQ.7 ) THEN
          READ(YNUMBER,'(I6.6)') INUMBER
        ENDIF
C
C       First time through, display the message if required
C
        IF( KONOFF.EQ.0 ) THEN
          WRITE(CMESS(31:36),'(I6.6)') INUMBER
          WRITE(GRPRSM,*) '**************************************'
          WRITE(GRPRSM,*) CMESS
          WRITE(GRPRSM,*) '**************************************'
        ENDIF
        ICOUNT = 1
      ENDIF
C
      PBIONUM = INUMBER
C
      RETURN
      END
