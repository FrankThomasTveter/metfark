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

      SUBROUTINE BUPRTBOX(KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU)
C
C**** *BUPRTBOX*
C
C
C     PURPOSE.
C     --------
C
C
C
C**   INTERFACE.
C     ----------
C
C               *CALL* *BUPRTBOX(KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU)*
C
C        INPUT :
C               *KBOX*    -  NUMBER OF ROWS      
C               *KAPP*    -  NUMBER OF COLUMNS
C               *KLEN*    -  OFFSET FOR START OF NEXT COLUMN
C               *KBOXR*   -  ARRAY CONTAINING BUFR TABLE B REFERENCE NUMBERS
C               *VALS*    -  ARRAY CONTAINING UNPACKED VALUES
C               *CBOXN*   -  ARRAY CONTAINING ELEMENT NAMES
C               *CBOXU*   -  ARRAY CONTAINING ELEMENT UNITS
C
C     METHOD.
C     -------
C
C
C
C     EXTERNALS.
C     ----------
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
C          M. DRAGOSAVAC    *ECMWF*       01/02/94.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
#     include "parameter.F"
#     include "bcomunit.F"
C
      DIMENSION IOPER(100)
#ifndef R_4
      REAL*8 RPRINT(60)
      REAL*8 VALS(JWORK)
#else
      REAL   RPRINT(60)
      REAL   VALS(JWORK)
#endif
      DIMENSION KBOXR(JWORK),IBVAL(JELEM),IBPRINT(60)
      CHARACTER*64 CBOXN(JELEM)
      CHARACTER*24 CBOXU(JELEM)
C
C
C     ------------------------------------------------------------------
C*                 1. PRINT BOXED EXPANDED BUFR MESSAGE
C                     ---------------------------------
 100  CONTINUE
C
      IF(KBOX.LE.6) THEN
         WRITE(KNTN,*)  'THERE IS NO USEFULL DATA TO BE PRINTED.'
         KBOX=0
        RETURN
      END IF
C
      IF(KAPP.GT.60) THEN
         WRITE(KNTN,*)  'THERE IS MORE THAN 60 APPLICATIONS IN THE DATA'
         WRITE(KNTN,*)  'ONLY FIRST 60 WILL BE PROCESSED'
         KAPP=60
      END IF
C
      IF(KAPP.GT.1) THEN
         IREP=(KAPP-1)/10
         IOFF=(KAPP-1)-IREP*10
         IF(IOFF.NE.0) IREP=IREP+1
      ELSE
         IREP=1
         IOFF=0
      END IF
C
      IST=2
      IEND=11
C      IF(IREP.EQ.1.AND.KAPP.EQ.1) IEND=IOFF+1
C     IF(IREP.EQ.1) IEND=IOFF+1
      IF(IREP.EQ.1 .AND. IOFF .EQ.0) THEN
         IEND=1 
      ELSEIF(IREP.EQ.1) THEN
         IEND=IOFF+1
      END IF
C
      DO 2005 J=1,IREP
C
      WRITE(KNTN,*)  ' '
      DO 2002 I=1,KBOX
      IIII=1
      RPRINT(IIII)=VALS(I)
C
      DO 2003 II=IST,IEND
      IIII=IIII+1
      III=I+(II-1)*KLEN      
      RPRINT(IIII)=VALS(III)
      IBPRINT(IIII)=KBOXR(III)
 2003 CONTINUE
C     WRITE(KNTN,'(1H ,I4,1X,A32,1X,15(1X,I6,1X,F8.1))') 
C    1        I,CBOXN(I),(IBPRINT(NN),RPRINT(NN),NN=1,KAPP)
      WRITE(KNTN,'(1H ,I4,1X,A32,1X,F14.1,30(1X,F8.1))')
     1        I,CBOXN(I),(RPRINT(NN),NN=1,IIII)
 2002 CONTINUE
C
      IF(IOFF.NE.0.AND.J.EQ.(IREP-1)) THEN
         IST=IEND+1
         IEND=IEND+IOFF
      ELSE
         IST=IEND+1
         IEND=IEND+10
      END IF
C
 2005 CONTINUE
C
C
      RETURN
      END
