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

      SUBROUTINE MBUFR_MARS_FILTER(KSIZE,KREQUEST,KBUFL,
     1                             KBUFF,FOUND,KERR)
C
C**** *MBUFR_MARS_FILTER*
C
C
C     PURPOSE.
C     --------
C         FILTER BUFR MESSAGES FROM kbuff array
C         ACCORDING TO REQUEST and returning requested bufr message 
C         in kbuff array if repacking is not needed or kbufr otherwise.
C
C
C**   INTERFACE.
C     ----------
C
C          NONE.
C
C     METHOD.
C     -------
C
C
C     EXTERNALS.
C     ----------
C
C         CALL MBUSEL
C         CALL MBUFREX
C         CALL MBUFREN
C         CALL MBUPRS0
C         CALL MBUPRS1
C         CALL MBUPRS2
C         CALL MBUPRS3
C         CALL MBUPRT
C         CALL MBUUKEY
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       12/09/2002
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
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2=4096,JSEC3=   4,
     1          JSEC4=2,JELEM=160000,JSUBS=400,JCVAL=150 ,JBUFL=512000,
#ifdef JBPW_64
     2          JBPW =  64,JTAB =3000,JCTAB=3000,JCTST=3000,JCTEXT=6000,
#else
     2          JBPW =  32,JTAB =3000,JCTAB=3000,JCTST=3000,JCTEXT=6000,
#endif
     3          JWORK=4096000,JKEY=46, JTMAX=10,JTCLAS=64,JTEL=255)
C
      PARAMETER (KELEM=200)
      PARAMETER (KVALS=200000)
C 
      DIMENSION KBUFF(JBUFL)
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY(JKEY),KEY1(JKEY),KEY2(JKEY)
      REAL*8 VALUES(KVALS)
      REAL*8 RQV(KELEM)
      REAL*8 RVIND
      REAL*8 EPS
      DIMENSION KTDLST(KELEM),KTDEXP(KELEM)
      DIMENSION KDATA(200)
      DIMENSION KREQ(2),KRQ(KELEM)
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*9  CIDENTS
      LOGICAL      FOUND
      DIMENSION KREQUEST(KSIZE)
C                                                                       
C
C     ------------------------------------------------------------------
C*          1. GET FILE NAMES.
C              ---------------
 100  CONTINUE
C
      RVIND=1.7D38
      NBYTES=JBPW/8
      EPS=10.D-7
C
      
      KBUFL1=KBUFL/NBYTES+1
C
C
C     -----------------------------------------------------------------
C*          2.1 EXPAND BUFR MESSAGE SECTION 0,1,2
C               ---------------------------------
 210  CONTINUE
C
      IERR=0
      CALL BUS0123( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KSEC3,IERR)
      IF(KERR.NE.0) RETURN
C
      IF(KSEC3(3).EQ.1.OR.KSEC3(3).GT.1.AND.
     1   KSEC3(4).EQ.192.OR.KSEC3(4).EQ.64) THEN
C       Expand first 9 elements
         CALL BUSTOP(9)
      END IF

C            2.1.1  UNPACK KEY
C
C     CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
C
C              PRINT KEY
C
C      CALL BUPRS2(KSUP ,KEY)
C
C----------------------------------------------------------------
C
C            2.3 MULTI-SUBSET MESSAGE
C
 230  CONTINUE
C
      IF(KSUP(6).GT.1)  THEN
         KEL=KVALS/KSUP(6)
         IF(KEL.GT.KELEM) KEL=KELEM
      ELSE
         KEL=KELEM
      END IF
C
      KREQ(1)=1
      KREQ(2)=2
      KRQ (1)=002048
      KRQ (2)=002019
      RQV (1)=RVIND
      RQV (2)=RVIND
      KRQL=2

      KERR=0
      CALL BUSRQ(KREQ,KRQL,KRQ,RQV,KERR)
      IF(KERR.NE.0) RETURN
C
      CALL BUFREX(KBUFL1,KBUFF,KSUP,KSEC0 ,KSEC1,KSEC2 ,KSEC3 ,KSEC4,
     1            KEL,CNAMES,CUNITS,KVALS,VALUES,CVALS,KERR)
C
      IF(KERR.NE.0) RETURN
C
C*          2.4 PRINT EXPANDED BUFR MESSAGE.
C               ----------------------------
 240  CONTINUE
C
C      CALL BUPRS0(KSEC0)
C      CALL BUPRS1(KSEC1)
C      CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
C      CALL BUPRS2(KSUP ,KEY)
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) RETURN
C
C      CALL BUPRS3(KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,KEL,CNAMES)
C      IST=1
C      IEND=KSUP(6)
C      CALL BUPRT(ICODE,IST,IEND,KEL,CNAMES,CUNITS,CVALS,
C     1           KVALS,VALUES,KSUP,KSEC1,IERR)
C
C
C
      i_002048=0
      i_002019=0
      DO I=1,KTDEXL
        IF(KTDEXP(i).EQ.002048) i_002048=i
        IF(KTDEXP(i).EQ.002019) i_002019=i
      END DO
C
      if(i_002048.ne.0) then
         do i=1,ksize
         if(krequest(i).eq.605) then
            krequest(i)=0
         elseif(krequest(i).eq.623) then
            krequest(i)=1
         elseif(krequest(i).eq.627) then
            krequest(i)=2
         elseif(krequest(i).eq.570) then
            krequest(i)=3
         elseif(krequest(i).eq.574) then
            krequest(i)=4
         elseif(krequest(i).eq.590) then
            krequest(i)=5
         elseif(krequest(i).eq.905) then
            krequest(i)=6
         elseif(krequest(i).eq.312) then
            krequest(i)=7
         elseif(krequest(i).eq.313) then
            krequest(i)=8
         elseif(krequest(i).eq.203) then
            krequest(i)=11
         end if
         end do
      end if
C
      FOUND=.FALSE.
C     
      if(i_002048.ne.0) then
         do i=1,ksize
           do j=1,ksec3(3)
             ij=i_002048+(j-1)*kel
             if(abs(values(ij)-rvind)/rvind.gt.eps) then
             if(nint(values(ij)).eq.krequest(i)) FOUND =.TRUE.
             end if
           end do 
         end do
      elseif(i_002019.ne.0) then
         do i=1,ksize
           do j=1,ksec3(3)
             ij=i_002019+(j-1)*kel
             if(abs(values(ij)-rvind)/rvind.gt.eps) then
             if(nint(values(ij)).eq.krequest(i)) FOUND =.TRUE.
             end if
           end do
         end do
      else
         FOUND =.FALSE.
      end if

C     -----------------------------------------------------------------
      KERR=0
C
      RETURN
C
C      
      END
