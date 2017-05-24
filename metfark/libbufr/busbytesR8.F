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

       SUBROUTINE BUSBYTESR8(DESTINATION,KSOURCE,KSKIP,KWIDTH,
     1                       KSKIPBETWEEN,KNUMBER)
C
C**** *BUSBYTESR8*
C
C
C     PURPOSE.
C     --------
C          
C
C**   INTERFACE.
C     ----------
C          NONE.
C
C
C
C
C     *METHOD.
C      -------
C          NONE.
C
C
C
C     EXTERNALS.
C     ----------
C          NONE.
C
C
C
C
C     REFERENCE.
C     ----------
C
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC       *ECMWF*       JULY 2008
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)

      REAL*8 DESTINATION

      CALL SBYTES(DESTINATION,KSOURCE,KSKIP,KWIDTH,KSKIPBETWEEN,KNUMBER)
      RETURN
      END
