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

      SUBROUTINE GET_NAME_UNIT(KREFERENCE,YNAME,YUNIT)
C
C**** *GET_NAME_UNIT*
C
C
C     PURPOSE.
C     --------
C          FIND NAME AND THE UNIT FOR THE PARTICULAR REFERENCE NUMBER.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *GET_NAME_UNIT(KREFERENCE,YNAME,YUNIT)*
C
C
C        INPUT : *KREFERENCE* - TABLE B REFERENCE NUMBER
C        OUTPUT:
C                *CNAMES*     -  ARRAY CONTAINING ELEMENT NAMES
C                *CUNITS*     -  ARRAY CONTAINING ELEMENT UNITS
C                *KERR*       -  RETURN CODE
C
C
C     METHOD.
C     -------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/2006.
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
#     include "parameter.F"

      CHARACTER*64 YNAME
      CHARACTER*24 YUNIT
C
#     include "bcomtab.F"
#     include "bcomtabc.F"
C
      CHARACTER*64 CTABBEN
      CHARACTER*24 CTABBU
      DIMENSION KREF(11)
      CHARACTER*64  CN(11)

C
      DATA KREF/222000,235000,236000,237000,224000,225000,223000,
     1          232000,235000,999999,0/ 
      DATA CN/'QUALITY INFORMATION FOLLOW',
     1        'CANCEL BACKWARD DATA REFERENCE',
     2        'BACKWARD REFERENCE BIT MAP',
     3        'USE PREVIOUSLY DEFINED BIT MAP',
     4        'FIRST ORDER STATISTICS FOLLOW',
     5        'DIFFERENCE STATISTICAL VALUES FOLLOW',
     6        'SUBSTITUTED VALUES FOLLOW',
     7        'REPLACE/RETAINED VALUES FOLLOW',
     8        'CANCEL BACKWARD DATA REFERENCE',
     9        'ASSOCIATED FIELD',
     1        'REFERENCE VALUE'/ 
C
      KERR=0
      DO I=1,11
      IF(KREFERENCE.EQ.KREF(I)) THEN
         YNAME=CN(I)
         YUNIT=' '
         RETURN 
      END IF
      END DO
      IF(KREFERENCE/1000.EQ.205) THEN
        YNAME='CHARACTERS'
        YUNIT='CCITT IA5 '
        RETURN
      END IF
C
      ICLASS=KREFERENCE/1000
      IYYY  =KREFERENCE-ICLASS*1000+1
      ICLASS=ICLASS+1
C
      if(ICLASS.GT.64.or.IYYY.gt.256) then
         print*,'KREFERENCE=',KREFERENCE
         print*,'ICLASS=',ICLASS
         print*,'IYYY=',IYYY
         KERR=57
         RETURN
      end if
      I=NTABP(ICLASS,IYYY)
      YNAME=CTABBEN(I)
      YUNIT=CTABBU(I)
C
      RETURN
      END
