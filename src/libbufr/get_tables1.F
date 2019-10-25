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

      SUBROUTINE GET_TABLES1(K)
C
C**** *GET_TABLES1*
C
C
C     PURPOSE.
C     --------
C          GET TABLES FROM THE ARRAY OF TABLES FOR
C          ENCODING
C
C**   INTERFACE.
C     ----------
C          NONE.
C
C
C     *METHOD.
C      -------
C          NONE.
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
C          BINARY UNIVERSAL FORM FOR DATA REPRESENTATION, FM 94 BUFR.
C
C          J.K.GIBSON AND M.DRAGOSAVAC,1987: DECODING DATA 
C          REPRESENTATION FM 94 BUFR,TECHNICAL MEMORANDUM NO.
C
C          J.K.GIBSON,1986:EMOS 2 - STANDARDS FOR SOFTWARE DEVELOPMENT
C          AND MAINTANANCE ,TECHICAL MEMORANDUM NO.       ECMWF.
C
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC       *ECMWF*       JANUARY 2007
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
#     include "bcmtab.F"
#     include "bcmtabc.F"
#     include "bcmtabload.F"
#     include "bcmtabloadc.F"

C
      CHARACTER*64 CTABBEN,CCTABBEN
      CHARACTER*24 CTABBU,CCTABBU,CTABLE_LIST
C
C     ------------------------------------------------------------------
C*          1.   INITIALIZE CONSTANTS AND VARIABLES.
C                -----------------------------------
 100  CONTINUE
C
C
      IF(K.LT.1.OR.K.GT.JTMAX) THEN
         print*,'Table number > JTMAX'
         call exit(2)
      END IF
      DO 101 I=1,64
      DO 101 ii=1,255
      NTABP(I,II)=MTABP(I,II,K)
 101  CONTINUE
C
      DO 102 I=1,JTAB
      NTABBTR(I)=MTABBTR(I,K)
      NTABBS (I)=MTABBS (I,K)
      NTABBRV(I)=MTABBRV(I,K)
      NTABBDW(I)=MTABBDW(I,K)
      CTABBEN(I)=CCTABBEN(I,K)
      CTABBU (I)=CCTABBU(I,K)
      NTABDTR(I)=MTABDTR(I,K)
      NTABDL (I)=MTABDL(I,K)
      NTABDST(I)=MTABDST(I,K)
 102  CONTINUE

      DO I=1,JTAB*20
      NTABDSQ(I)=MTABDSQ(I,K)
      END DO
C
C     ------------------------------------------------------------------
      RETURN
      END
