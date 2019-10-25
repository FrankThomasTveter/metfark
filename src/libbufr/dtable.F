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

      SUBROUTINE DTABLE(YNAME,KERR)
C
C**** *DTABLE*
C
C
C     PURPOSE.
C     --------
C          THE MAIN PURPOSE OF THIS PROGRAMME IS TO CREATE WORKING
C          TABLE OF SEQUENCE DESCRIPTORS FOR *BUFR* DECODING.
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
C          BINARY UNIVERSAL FORM FOR DATA REPRESENTATION, *FM 94 BUFR*.
C
C          J.K.GIBSON AND *M.DRAGOSAVAC,1987:* DECODING *DATA *REPRESENTATION
C                          *FM 94 BUFR*,*TECHNICAL *MEMORANDUM *NO.
C
C          J.K.GIBSON,1986:*EMOS 2 - *STANDARDS FOR SOFTWARE DEVELOPMENT
C                           AND MAINTANANCE *,*TECHICAL MEMORANDUM *NO.
C                           *ECMWF*.
C
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC       *ECMWF*       JANUARY 1991.
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
#     include "bcomwork.F"
#     include "bcomtab.F"
#     include "bcomtabc.F"
#     include "bcomct.F"
#     include "bcomctc.F"
#     include "bcomroot.F"
#     include "bcomtabload.F"
#     include "bcomtabloadc.F"

C
      CHARACTER*64 CTABBEN,CCTABBEN
      CHARACTER*24 CTABBU,CCTABBU,CTABLE_LIST
      CHARACTER*120 YENTRY
      CHARACTER*256 YFNAME
      CHARACTER*(*) YNAME
      REAL*8 RVIND,EPS
      logical lfirst=.true.
C     
C     ------------------------------------------------------------------
C*          1.   SET INITIAL CONSTANTS.
C                ----------------------
 100  CONTINUE
C
      KERR=0
      J  =0
      IST=1
      YFNAME=' '
C
      DO 101 I=1,JTAB
      NTABDTR(I)=999999
      NTABDL (I)=0
      NTABDST(I)=0
 101  CONTINUE
C
      DO 102 I=1,JTAB*20
      NTABDSQ(I)=0
 102  CONTINUE
C
      II=INDEX(YNAME,' ')
      II=II-1
      YFNAME=YNAME(1:II)
      II=INDEX(YFNAME,' ')
      II=II-1
      CALL GET_FREE_UNIT(IUNIT)
      OPEN(UNIT=IUNIT,iostat=ios,FILE=YFNAME(1:II),STATUS='OLD')
      IF(IOS.NE.0) THEN
         if (lfirst) then
            print*,'Open error on ',YFNAME(1:II)
            lfirst=.false.
         end if
         kerr=63
         return
      END IF
C
C     ------------------------------------------------------------------
C*          2.   READ IN TABLE D
C                ---------------
C
 200  CONTINUE
C
      READ(IUNIT,'(A)',IOSTAT=IOS,END=300) YENTRY
      IF(IOS.NE.0) THEN
         print*,'Read error ',ios
         kerr=8
         return
      END IF
C
      J=J+1
C
      IF(J.GT.JTAB) THEN
         PRINT*,' DIMENSION TOO SMALL J=',J
         KERR=64
         RETURN
      END IF 
C
C     ------------------------------------------------------------------
C*          2.1  SET ARRAYS FOR TABLE REFERENCE, DATA LENGTH,
C*               STARTING POINTER AND SEQUENCE DESCRIPTORS.
C
 210  CONTINUE
C
C
      READ(YENTRY,'(1X,I6,I3)') NTABDTR(J),NTABDL (J)
C
      IF(J.EQ.1) THEN
         IST=1
         NTABDST(J)=IST
      ELSE
         IST=IST + NTABDL(J-1)
         NTABDST(J)=IST
      END IF
C
      IF(NTABDL(J).GT.1) THEN
         READ(YENTRY,'(11X,I6)') NTABDSQ(IST)
         IIST=IST
C
         DO 220 JA=1,NTABDL(J)-1
         IIST=IIST+1
         READ(IUNIT,'(A)',END=300) YENTRY
         READ(YENTRY,'(11X,I6)') NTABDSQ(IIST)
 220     CONTINUE
C
      ELSE
         READ(YENTRY,'(11X,I6)') NTABDSQ(IST)
      END IF
C
C
C     WRITE(*,1000) NTABDTR(J),NTABDL(J),NTABDST(J),
C    1              (NTABDSQ(I),I=NTABDST(J),NTABDL(J)+NTABDST(J)-1)
C
C1000 FORMAT(1H ,I6,I3,1X,I6,1X,I6/ (18X,I6))
C
      GO TO 200
C
C     ------------------------------------------------------------------
 300  CONTINUE
C
      IF(NTT.EQ.JTMAX.AND.NTC.NE.0) THEN
         DO J=1,JTAB
            MTABDTR(J,NTC)=NTABDTR(J)
            MTABDL (J,NTC)=NTABDL(J)
            MTABDST(J,NTC)=NTABDST(J)
         END DO
         DO J=1,JTAB*20
            MTABDSQ(J,NTC)=NTABDSQ(J)
         END DO
      ELSE
         DO J=1,JTAB
            MTABDTR(J,NTT)=NTABDTR(J)
            MTABDL (J,NTT)=NTABDL(J)
            MTABDST(J,NTT)=NTABDST(J)
         END DO
         DO J=1,JTAB*20
            MTABDSQ(J,NTT)=NTABDSQ(J)
         END DO
      END IF
C
      CLOSE(IUNIT)
C
c     WRITE(*,'(1h )')
c     WRITE(*,'(1h ,a,i4)')'Total number of entries in Table D is ',j
C
      RETURN
C     -----------------------------------------------------------------
 400  CONTINUE
C
      RETURN
C
      END
