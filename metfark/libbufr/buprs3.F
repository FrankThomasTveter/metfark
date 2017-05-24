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

      SUBROUTINE BUPRS3(KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,KELEM,CNAMES)
C
C**** *BUPRS3*
C
C
C     PURPOSE.
C     --------
C           PRINT SECTION 3 OF BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRS3(KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,
C                          KELEM,CNAMES)*
C
C        INPUT :
C               *KSEC3*   -  ARRAY CONTAINING SECTION 3 INFORMATION
C                            KSEC3( 1)-- LENGTH OF SECTION 3 (BYTES)
C                            KSEC3( 2)-- RESERVED
C                            KSEC3( 3)-- NUMBER OF SUBSETS
C                            KSEC3( 4)-- FLAG (DATA TYPE,DATA COMPRESSION)
C               *KTDLEN*  -  NUMBER OF DATA DESCRIPTORS IN SECTION 3
C               *KTDLST*  -  ARRAY CONTAINING DATA DESCRIPTORS IN SECTION 3
C               *KTDEXL*  -  NUMBER OF ENTRIES IN LIST OF EXPANDED DATA
C                            DESCRIPTORS
C               *KTDEXP*  -  ARRAY CONTAINIG EXPANDED DATA DESCRIPTORS
C               *KELEM*   -  DIMENSION OF CNAMES, CUNITS ARRAY
C               *CNAMES*  -  CHARACTER ARRAY CONTAINING ELEMENT NAMES
C
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
C
      DIMENSION KSEC3(JSEC3)
      DIMENSION KTDLST(KTDLEN),KTDEXP(KTDEXL)
C
      CHARACTER*64 CNAMES(KELEM)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 3.
C                ----------------
 100  CONTINUE
C
      WRITE(KNTN,'(1H1)')
C
      WRITE(KNTN,'(1H ,A)')    '         BUFR SECTION 3    '
      WRITE(KNTN,'(1H )')
      WRITE(KNTN,'(1H ,A,I5)') 'LENGTH OF SECTION 3 (BYTES)         ',
     1                       KSEC3(1)
      WRITE(KNTN,'(1H ,A,I5)') 'RESERVED                            ',
     1                       KSEC3(2)
      WRITE(KNTN,'(1H ,A,I5)') 'NUMBER OF DATA SUBSETS              ',
     1                       KSEC3(3)
      WRITE(KNTN,'(1H ,A,I5)') 'FLAG (DATA TYPE/DATA COMPRESSION)   ',
     1                       KSEC3(4)
C
      WRITE(KNTN,'(1H ,//)')
      WRITE(KNTN,'(1H ,A)')    '       DATA DESCRIPTORS (UNEXPANDED)'
C
      WRITE(KNTN,'(1H )')
      DO 110 I=1,KTDLEN
       WRITE(KNTN,'(1H ,I4,2X,I6.6)') I,KTDLST(I)
 110  CONTINUE
C
      WRITE(KNTN,'(1H ,/)')
      WRITE(KNTN,'(1H ,A)')    '       DATA DESCRIPTORS (EXPANDED)'
      WRITE(KNTN,'(1H )')
      DO 120 I=1,KTDEXL
       WRITE(KNTN,'(1H ,I5,2X,I6.6,2X,A)') I,KTDEXP(I),CNAMES(I)
 120  CONTINUE

      RETURN
      END
