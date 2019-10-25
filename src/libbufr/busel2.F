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

      SUBROUTINE BUSEL2(KSUBSET,KELEM,KTDLEN,KTDLST,KTDEXL,KTDEXP,
     1                  CNAMES,CUNITS,KERR)
C
C**** *BUSEL2*
C
C
C     PURPOSE.
C     --------
C          RETURNS LIST OF DATA DESCRIPTORS AS IN SECTION 3  OF BUFR
C          MESSAGE AND TOTAL/REQUESTED LIST OF ELEMENTS, NAMES, UNITS.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUSEL2(KSUBSET,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,
C                         CUNITS,CVALS,KERR)*
C
C
C        OUTPUT:
C               *KTDLEN*  -  NUMBER OF DATA DESCRIPTORS IN SECTION 3
C               *KTDLST*  -  ARRAY CONTAINING DATA DESCRIPTORS IN SECTION 3
C               *KTDEXL*  -  NUMBER OF ENTRIES IN LIST OF EXPANDED DATA
C                            DESCRIPTORS
C               *KTDEXP*  -  ARRAY CONTAINIG EXPANDED DATA DESCRIPTORS
C               *CNAMES*  -  ARRAY CONTAINING ELEMENT NAMES
C               *CUNITS*  -  ARRAY CONTAINING ELEMENT UNITS
C               *KERR*    -  RETURNED ERROR CODE
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
C
#     include "parameter.F"
#     include "bcomunit.F"
#     include "bcomel.F"
#     include "bcomoff.F"
C
      DIMENSION  KTDLST(*),KTDEXP(*)
C
      CHARACTER*64 CNAMES(*)
      CHARACTER*24 CUNITS(*)
C
C     -----------------------------------------------------------------

C*          1.  PUT LIST OF ELEMENTS FROM COMMON BLOCK
C               --------------------------------------
C               TO REQUESTED ARRAYS.
C               --------------------
 100  CONTINUE
C
      KERR=0
C
      KTDLEN=NTDLEN
C
      DO 101 I=1,NTDLEN
      KTDLST(I)=NTDLST(I)
 101  CONTINUE
C
      IF(OMULTI) THEN
         NTDEXL=NSIZE(KSUBSET)
         KTDEXL=NTDEXL
         IJ=(KSUBSET-1)*KELEM
      ELSE
         NTDEXL=NSIZE(1)
         KTDEXL=NTDEXL
         IJ=0
      END IF

C
      DO 102 I=1,NTDEXL
      IIJ=I+IJ
      KTDEXP(I)=NWTRG(IIJ)
      CALL GET_NAME_UNIT(KTDEXP(I),CNAMES(I),CUNITS(I))
 102  CONTINUE
C
      RETURN
      END
