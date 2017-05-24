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

      SUBROUTINE BTABLE(YNAME,YTABB,KERR)
C
C**** *BTABLE*
C
C
C     PURPOSE.
C     --------
C          CREATE BUFR TABLE B IN BINARY FORM.
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
      CHARACTER*24 CTABBU,CCTABBU,CTABLE_LIST,YTABB
      CHARACTER*120 YENTRY
      CHARACTER*(*) YNAME
      CHARACTER*256  YFNAME
      REAL*8 RVIND,EPS
C
C     DATA CTABBEN/JTAB*' '/,CTABBU/JTAB*' '/
C
C     ------------------------------------------------------------------
C*          1.   INITIALIZE CONSTANTS AND VARIABLES.
C                -----------------------------------
 100  CONTINUE
C
      J=0
      ICLASS0=0
      IVIND=2147483647
      YFNAME=' '
C
      DO 101 I=1,64
      do 101 ii=1,255
      NTABP(I,ii)=0
 101  CONTINUE
C
      DO 102 i=1,JTAB
      NTABBTR(I)=IVIND
      NTABBS (I)=IVIND
      NTABBRV(I)=IVIND
      NTABBDW(I)=IVIND
      CTABBEN(I)=' '
      CTABBU (I)=' '
 102  CONTINUE
C
      II=INDEX(YNAME,' ')
      II=II-1
      YFNAME=YNAME(1:II)
      II=INDEX(YFNAME,' ')
      II=II-1
      CALL GET_FREE_UNIT(IUNIT)
      OPEN(UNIT=IUNIT,FILE=YFNAME(1:II),
     2            IOSTAT=IOS,      
     1            STATUS='OLD')
      IF(IOS.NE.0) THEN
         print*,'open error on ',YFNAME(1:II)
         kerr=61
         return
      END IF
C     ------------------------------------------------------------------
C*          2.   READ IN TABLE B ELEMENT.
C                ------------------------
C
 200  CONTINUE
C
      YENTRY=' '
      READ(IUNIT,'(A)',ERR=402,END=300) YENTRY
C
      J=J+1
      IF(J.GT.JTAB) THEN
         PRINT*,' DIMENSION TOO SMALL J=',J
         kerr=6
         return
      END IF 
C
C     ------------------------------------------------------------------
C*          2.1  SET ARRAYS FOR TABLE REFERENCE, ELEMENT NAME, UNITS,
C*               REFERENCE VALUE AND DATA WIDTH.
C
 210  CONTINUE
C
C
      READ(YENTRY,'(1X,I6,1x,64x,1x,24x,1x,I3,1x,I12,1x,I3)')
     1                                         NTABBTR(J),NTABBS (J),
     1                                         NTABBRV(J),NTABBDW(J)
C
      CTABBEN(J)=YENTRY( 9:72)
      CTABBU (J)=YENTRY(74:97)
C
      ICLASS=NTABBTR(J)/1000
      IYYY  =NTABBTR(J)-ICLASS*1000+1
C
      ICLASS=ICLASS+1
      NTABP(ICLASS,IYYY)=J
C
c      WRITE(*,1000) NTABBTR(J),CTABBEN(J),CTABBU(J),NTABBS(J),
c     1              NTABBRV(J),NTABBDW(J)
c 1000 FORMAT(1H ,1X,I6,1x,64x,1x,24x,1x,,I3,1x,I12,1x,I3)
C
      GO TO 200
C
C     ------------------------------------------------------------------
 300  CONTINUE
C
      NTT=NTT+1
      IF(NTT.GT.JTMAX) THEN
         NTT=NTT-1
         NTC=NTC+1
         IF(NTC.GT.10) NTC=1
         DO J=1,JTAB
         MTABBTR(J,NTC)=NTABBTR(J)
         MTABBS (J,NTC)=NTABBS (J)
         MTABBRV(J,NTC)=NTABBRV(J)
         MTABBDW(J,NTC)=NTABBDW(J)
         CCTABBEN(J,NTC)=CTABBEN(J)
         CCTABBU (J,NTC)=CTABBU (J)
         END DO
         DO I=1,64
         DO K=1,255
         MTABP(I,K,NTC)=NTABP(I,K)
         END DO
         END DO
         CTABLE_LIST(NTC)=YTABB
      ELSE
         DO J=1,JTAB
         MTABBTR(J,NTT)=NTABBTR(J)
         MTABBS (J,NTT)=NTABBS (J)
         MTABBRV(J,NTT)=NTABBRV(J)
         MTABBDW(J,NTT)=NTABBDW(J)
         CCTABBEN(J,NTT)=CTABBEN(J)
         CCTABBU (J,NTT)=CTABBU (J)
         END DO
         DO I=1,64
         DO K=1,255
         MTABP(I,K,NTT)=NTABP(I,K)
         END DO
         END DO
         CTABLE_LIST(NTT)=YTABB
      END IF
C
      CLOSE(IUNIT)
C
C     WRITE(*,'(1h )')
C     WRITE(*,'(1h ,a,i4)') 'Total number of entries in Table B is ',j
C
      RETURN
C
402   CONTINUE
      KERR=6
      WRITE(*,4402) IOS,YFNAME
 4402 FORMAT(1H ,'Read error',i4,' on ',a)
      RETURN
C
 401  CONTINUE
C
      KERR=61
      WRITE(*,4401) IOS,YFNAME
 4401 FORMAT(1H ,'Open error',i4,' on ',a)
C     
      RETURN
      END
