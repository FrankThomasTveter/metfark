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

      SUBROUTINE BUENS3(KSEC3,KTDLEN,KTDLST,KBUFL,KBUFF,KERR)
C
C**** *BUENS3*
C
C
C     PURPOSE.
C     --------
C
C          PACK SECTION 3 OF BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUENS3( KSEC3,KTDLEN,KTDLST,KBUFL,KBUFF,KERR)*
C
C        INPUT :
C               *KSEC3*   -  ARRAY CONTAINING SECTION 3 INFORMATION
C                            KSEC3( 1)-- LENGTH OF SECTION 3 (BYTES)
C                            KSEC3( 2)-- RESERVED
C                            KSEC3( 3)-- NUMBER OF SUBSETS
C                            KSEC3( 4)-- FLAG (DATA TYPE,DATA COMPRESSION)
C               *KTDLEN*  -  NUMBER OF DATA DESCRIPTORS IN SECTION 3
C               *KTDLST*  -  ARRAY CONTAINING DATA DESCRIPTORS IN SECTION 3
C
C        OUTPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C      -------
C
C
C
C
C     EXTERNALS.
C     ----------
C
C          BUPCK        - PACKS BIT PATHERN
C          BUOCTN       - SET LENGTH OF SECTION
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
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
      DIMENSION KBUFF(KBUFL)
      DIMENSION KTDLST(KTDLEN)
C
      DIMENSION KSEC3(JSEC3)
C
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
C     ------------------------------------------------------------------
C*          1.   PACK PRELIMINARY ITEMS OF SECTION 3.
C                --------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C*          1.1  KEEP POINTERS TO THE BEGINING OF THE SECTION.
C                ---------------------------------------------
 110  CONTINUE
C
      IWPTB = NWPT
      IBPTB = NBPT
C
C
C*          1.2   PACK LENGTH OF SECTION 3.
C                 -------------------------
 120  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING LENGTH OF SECTION 3.'
         RETURN
      END IF
C
C
C*          1.3    PACK ZERO BYTE AND PUT IT IN KSEC3(2).
C                  --------------------------------------
 130  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING RESERVED BYTE IN SECTION 3.'
         RETURN
      END IF
C
C*          1.4    PACK NUMBER OF DATA SUB-SETS.
C                  -----------------------------
 140  CONTINUE
C
      IF(KSEC3(3).GT.65535) THEN
         KERR=27
         CALL BUERR(KERR)
         RETURN
      END IF
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC3(3),NWPT,NBPT,16,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING KSEC3(3) IN SECTION 3.'
         RETURN
      END IF
C
C
C*          1.5    PACK INTEGER VALUE OF THE OCTET
C                  ---------------------------------
C                  CONTAINIG FLAG BITS.
C                  --------------------
 150  CONTINUE
C
      IF(KSEC3(3).LE.1) THEN
         KSEC3(4)=IBCLR(KSEC3(4),6)
      END IF
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC3(4),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING KSEC3(4) IN SECTION 3.'
         RETURN
      END IF
C
C     -----------------------------------------------------------------
C*          1.6  PACK DATA DESCRIPTORS.
C                ----------------------
 160  CONTINUE
C
C
      DO 161 I=1,KTDLEN
      IFIXIY=KTDLST(I)
C
      II=IFIXIY/1000
      IY=IFIXIY-II*1000
      IF=II/100
      IX=II-IF*100
C
      CALL BUPCK(NBPW,KBUFF(NWPT),IF,NWPT,NBPT,2,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING ',I,' DESCRIPTOR IN SECTION 3.'
         RETURN
      END IF
      CALL BUPCK(NBPW,KBUFF(NWPT),IX,NWPT,NBPT,6,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING ',I,' DESCRIPTOR IN SECTION 3.'
         RETURN
      END IF
      CALL BUPCK(NBPW,KBUFF(NWPT),IY,NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING ',I,' DESCRIPTOR IN SECTION 3.'
         RETURN
      END IF
C
C
 161  CONTINUE
C
C*          1.7  SET UP LENGTH OF THE SECTION 1.
C               --------------------------------
 170  CONTINUE
C
      CALL BUOCTN(IWPTB,IBPTB,KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C
      RETURN
      END
