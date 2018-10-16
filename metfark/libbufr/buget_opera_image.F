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

      SUBROUTINE BUGET_OPERA_IMAGE(KSEC1,KTDEXL,KTDEXP,
     1      CNAMES,CUNITS, KELEM,KVALS,VALUES,CVALS,KTDEXL_IMG,
     2      KTDEXP_IMG,CNAMES_IMG,CUNITS_IMG,KVALS_IMG,VALUES_IMG,
     3      CVALS_IMG,KSIZE_IMG_BYTES,IMAGE,KPIXEL_SIZE,KERR)
C
C**** *BUGET_OPERA_IMAGE*
C
C
C     PURPOSE.
C     --------
C          APPLY DELAYED REPETITIONS TO GET FULL IMAGE
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUGET_OPERA_IMAGE(KSEC1,KTDEXL,KTDEXP,CNAMES,CUNITS,
C                  KELEM,,KVALS,VALUES,CVALS,
C                  KTDEXL_IMG,KTDEXP_IMG,CNAMES_IMG,CUNITS_IMG,
C                  KVALS_IMG,VALUES_IMG,CVALS_IMG,IMAGE,KPIXEL_SIZE,KERR)
C
C        INPUT :
C               *KSEC1*      -  An integer array of at least 40 words containing Bufr Section 1
C               *KTDEXL*     -  An integer variable containing number of expanded data descriptors
C               *KTDEXP*     -  An integer array containing the list of KTDEXL data descriptors
C               *CNAMES*     -  A CHARACTER*64  array of kelem containing element names
C               *CUNITS*     -  A CHARACTER*24 array of kelem containig bufr table B units
C               *KELEM*      -  An integer containing expected number of expanded elements
C               *KVALS*      -  An integer containing expected number of data elelemnts
C               *VALUES*     -  A REAL*8 array containing expanded values
C               *CVALS*      -  A CHARACTER*80 array containing character values
C        OUTPUT:
C               *KTDEXL_IMG* - An integer variable containing number of expanded data descriptors
C               *KTDEXP_IMG* - An integer array containing the list of KTDEXL_IMG data descriptors
C               *CNAMES_IMG* - A CHARACTER*64  array of kelem containing element names
C               *CUNITS_IMG* - A CHARACTER*24 array of kelem containig bufr table B units
C               *KVALS_IMG*  - An integer containing  number pixels in IMAGE
C               *VALUES_IMG* - A REAL*8 array containing expanded values
C               *CVALS_IMG*  - A CHARACTER*80 array containing character values
C               *IMAGE*      - Integer array containing image ( pixel values)
C               *KPIXEL_SIZE*- Integer containing pixel size in bytes
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C     -------
C
C           NONE.
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
C          M. DRAGOSAVAC    *ECMWF*       02/04/2008
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
C
      PARAMETER (JPIXEL=10000000)
C
      DIMENSION KSEC1(JSEC1)
      INTEGER*4 IMAGE(*)
      INTEGER*4 IPIXELS(JPIXEL)
C     
      DIMENSION KTDEXP(*)
      DIMENSION KTDEXP_IMG(*)
      REAL*8    VALUES_IMG(*)
C
      CHARACTER*64 CNAMES(*),CNAMES_IMG(*)
      CHARACTER*24 CUNITS(*),CUNITS_IMG(*)
      CHARACTER*80 CVALS(*) ,CVALS_IMG(*)
C
      CHARACTER*256 COUT1,COUT2,COUT3
C     
#ifndef R_4
      REAL*8 VALUES(KVALS)
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   VALUES(KVALS)
      REAL   RVIND
      REAL   EPS
#endif
C
      logical lfirst=.true.
C     ------------------------------------------------------------------
C
      RVIND=1.7D38
      EPS=10.D-8
C     Create image
c
      IROW=0
      DO I=1,KTDEXL
      IF(KTDEXP(i).eq.005031) THEN
         IROW=IROW+1
      END IF
      END DO

C
      I_N_BYTE=0
      DO I=1,KTDEXL
      IF(KTDEXP(i).eq.030001) THEN
         I_N_BYTE=1
      ELSEIF(KTDEXP(i).eq.030002) THEN
         I_N_BYTE=1
      ELSEIF(KTDEXP(i).eq.030004) THEN
         I_N_BYTE=2 
      END IF
      END DO


C     print*,'Size of pixel in bytes=',I_N_BYTE
C     print*,'total number of rows=',irow

      DO I=1,KTDEXL
       IF(KTDEXP(I).EQ.005031.AND.KTDEXP(i-1).EQ.031002) THEN
         IMAGE_START=I-1
         GO TO 100
       END IF
      END DO
c
 100  CONTINUE
c
      DO I=1,IMAGE_START-1
      KTDEXP_IMG(I)=KTDEXP(I)
      END DO

      KTDEXL_IMG=IMAGE_START-1
c
C     print*,'KTDEXL_IMG=',KTDEXL_IMG
C     print*,'image_start=',image_start
C
      DO I=1,IMAGE_START-1
      VALUES_IMG(I)=VALUES(I)
      CNAMES_IMG(I)=CNAMES(I)
      CUNITS_IMG(I)=CUNITS(I)
      END DO
C
C     DO I=1,IMAGE_START-1
C     WRITE(IUNIT2,'(I6,1X,A64,E23.14E3,1x,a24)') I,CNAMES(I),
C    c                                VALUES(I),CUNITS(I)
C     END DO
C
      NBPW=JBPW
      NWPT=1
      NBPT=0
      IMX=0
      MAX=0
      i0=0
      DO I=IMAGE_START,KTDEXL
      IF(KTDEXP(I).EQ.005031) THEN
      END IF
      IF(KTDEXP(I).EQ.030001.AND.KTDEXP(I-1).EQ.031012) THEN
            I_REPEAT=NINT(VALUES(I-1))
            DO J=1,I_REPEAT
            IMX=IMX+1
            if(abs(VALUES(I)-rvind)/rvind.lt.eps) then
               ipixels(IMX)=255
            else
               IF(NINT(VALUES(I)).GT.MAX) MAX=NINT(VALUES(I))
               IVAL=NINT(VALUES(I))
               ipixels(IMX)=ival
            end if
            END DO
      ELSEIF(KTDEXP(I).EQ.030002.AND.KTDEXP(I-1).EQ.031012) THEN
            I_REPEAT=NINT(VALUES(I-1))
            DO J=1,I_REPEAT
            IMX=IMX+1
            if(abs(VALUES(I)-rvind)/rvind.lt.eps) then
               ipixels(IMX)=255
               CALL BUPCK(NBPW,IMAGE(NWPT),255,NWPT,NBPT,8,IERR)
            else
               IF(NINT(VALUES(I)).GT.MAX) MAX=NINT(VALUES(I))
               IVAL=NINT(VALUES(I))
               ipixels(IMX)=ival
            end if
            END DO
      ELSEIF(KTDEXP(i).EQ.030004.AND.KTDEXP(I-1).EQ.031012) THEN
            I_REPEAT=NINT(VALUES(I-1))
            DO J=1,I_REPEAT
            IMX=IMX+1
            if(abs(VALUES(I)-rvind)/rvind.lt.eps) then
               ipixels(IMX)=65535
            else
               IF(NINT(VALUES(I)).GT.MAX) MAX=NINT(VALUES(I))
               IVAL=NINT(VALUES(I))
               ipixels(IMX)=ival
            end if
            END DO
      ELSEIF(KTDEXP(I).EQ.030001.AND.KTDEXP(I-1).EQ.031011) THEN
            I_REPEAT=NINT(VALUES(I-1))
            DO J=1,I_REPEAT
            IMX=IMX+1
            if(abs(VALUES(I)-rvind)/rvind.lt.eps) then
               ipixels(IMX)=255
            else
               IF(NINT(VALUES(I)).GT.MAX) MAX=NINT(VALUES(I))
               IVAL=NINT(VALUES(I))
               ipixels(IMX)=ival
            end if
            END DO
      ELSEIF(KTDEXP(I).EQ.030002.AND.KTDEXP(I-1).EQ.031011) THEN
            I_REPEAT=NINT(VALUES(I-1))
            DO J=1,I_REPEAT
            IMX=IMX+1
            if(abs(VALUES(I)-rvind)/rvind.lt.eps) then
               ipixels(IMX)=255
            else
               IF(NINT(VALUES(I)).GT.MAX) MAX=NINT(VALUES(I))
               IVAL=NINT(VALUES(I))
               ipixels(IMX)=ival
            end if
            END DO
      ELSEIF(KTDEXP(I).EQ.030004.and.KTDEXP(I-1).EQ.031011) THEN
            I_REPEAT=NINT(VALUES(I-1))
            DO J=1,I_REPEAT
            IMX=IMX+1
            if(abs(VALUES(I)-rvind)/rvind.lt.eps) then
               ipixels(IMX)=65535
            else
               IF(NINT(VALUES(I)).GT.MAX) MAX=NINT(VALUES(I))
               IVAL=NINT(VALUES(I))
               ipixels(IMX)=ival
            end if
            END DO
      ELSEIF(KTDEXP(I).EQ.030001) THEN
            IMX=IMX+1
            if(abs(VALUES(I)-rvind)/rvind.lt.eps) then
               ipixels(IMX)=255
            else 
               IF(NINT(VALUES(I)).GT.MAX) MAX=NINT(VALUES(I))
               IVAL=NINT(VALUES(I))
               ipixels(IMX)=ival
            end if
      ELSEIF(KTDEXP(I).EQ.030002) THEN
            IMX=IMX+1
            if(abs(VALUES(I)-rvind)/rvind.lt.eps) then
               ipixels(IMX)=255
            else
               IF(NINT(VALUES(I)).GT.MAX) MAX=NINT(VALUES(I))
               IVAL=NINT(VALUES(I))
               ipixels(IMX)=ival
            end if
      ELSEIF(KTDEXP(I).EQ.030004) THEN
            IMX=IMX+1
            if(abs(VALUES(I)-rvind)/rvind.lt.eps) then
               ipixels(IMX)=65355
            else
               IF(NINT(VALUES(I)).GT.MAX) MAX=NINT(VALUES(I))
               IVAL=NINT(VALUES(I))
               ipixels(IMX)=ival
            end if
      END IF
      END DO
C
      KSIZE_IMG_BYTES=IMX*I_N_BYTE
      KPIXEL_SIZE=I_N_BYTE
      KVALS_IMG=IMX
C
      CALL SBYTES(IMAGE,IPIXELS,0,8,0,IMX)
      
C     print*,'max pixel value=',max
C     print*,'number of pixels=',KVALS_IMG
C     print*,'image size bytes=',KSIZE_IMG_BYTES
C
      GO TO 900
C     -----------------------------------------------------------------
C
 810  CONTINUE
C
      if (lfirst) then
         WRITE(*,'(1H ,A)') 'OPEN ERROR ON INPUT FILE'
         lfirst=.false.
      end if
      GO TO 900
C
 800  CONTINUE
C
      IF(IRET.EQ.-1) THEN
         PRINT*,'NUMBER OF RECORDS PROCESSED ',N
         PRINT*,'NUMBER OF OBSERVATIONS      ',IOBS
      ELSE
         PRINT*,' BUFR : ERROR= ',IERR
      END IF
C
 900  CONTINUE
C
C
C     ------------------------------------------------------------------
      RETURN
      END
