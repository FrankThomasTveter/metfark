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

      SUBROUTINE BUGTLL(KELEM,KTDEXL,KTDEXP,KSEC1,KSEC3 ,
     1                    KVALS, VALUES,  KERR)
C
C**** *BUGTLL*
C
C
C     PURPOSE.
C     --------
C           Set section 1 info for Bufr Edition 4
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
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       06/11/2004.
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
C
      DIMENSION KSEC1(*),KSEC3(*)
      DIMENSION KTDEXP(*)
#ifndef R_4
      REAL*8 VALUES(*)
      REAL*8 RYEAR(10000)
      REAL*8 RMONTH(10000)
      REAL*8 RDAY(10000)
      REAL*8 RHOUR(10000)
      REAL*8 RMINUTE(10000)
      REAL*8 RSECOND(10000)
      REAL*8 RMINLAT, RMINLON, RMAXLAT, RMAXLON
#else
      REAL   VALUES(*)
      REAL   RYEAR(10000)
      REAL   RMONTH(10000)
      REAL   RDAY(10000)
      REAL   RHOUR(10000)
      REAL   RMINUTE(10000)
      REAL   RSECOND(10000)
      REAL   RMINLAT, RMINLON, RMAXLAT, RMAXLON
#endif
      DIMENSION IDATE(10000)
      DIMENSION ITIME(10000)
C
C     -----------------------------------------------------------------
C
C                1. FIND TIME,LAT,LONG
C
      IPYEAR=0
      IPMONTH=0
      IPDAY=0
      IPHOUR=0
      IPMINUTE=0
      IPSECOND=0
      IPLAT=0
      IPLON=0
C
      DO I=1,KTDEXL
        IF(KTDEXP(I).EQ.004001) THEN
           IF(IPYEAR.EQ.0) IPYEAR=I
        END IF
        IF(KTDEXP(I).EQ.004002) THEN
           IF(IPMONTH.EQ.0) IPMONTH=I
        END IF
        IF(KTDEXP(I).EQ.004003) THEN
           IF(IPDAY.EQ.0) IPDAY=I
        END IF
        IF(KTDEXP(I).EQ.004004) THEN
           IF(IPHOUR.EQ.0) IPHOUR=I
        END IF
        IF(KTDEXP(I).EQ.004005) THEN
           IF(IPMINUTE.EQ.0) IPMINUTE=I
        END IF
        IF(KTDEXP(I).EQ.004006) THEN
           IF(IPSECOND.EQ.0) IPSECOND=I
        END IF
        IF(KTDEXP(I).EQ.005001) THEN
           IF(IPLAT.EQ.0) IPLAT=I
        END IF
        IF(KTDEXP(I).EQ.006001) THEN
           IF(IPLON.EQ.0) IPLON=I
        END IF
        IF(KTDEXP(I).EQ.005002) THEN
           IF(IPLAT.EQ.0) IPLAT=I
        END IF
        IF(KTDEXP(I).EQ.006002) THEN
           IF(IPLON.EQ.0) IPLON=I
        END IF
      END DO
C
       CALL FMMH(IPLAT,IPLON,KELEM,VALUES,KSEC3,RMINLAT,RMINLON,
     1           RMAXLAT,RMAXLON)
C
C      IF(RMINLON.LT.0) RMINLON=360.0-ABS(RMINLON)
C      IF(RMAXLON.lt.0) RMAXLON=360.0-ABS(RMAXLON)
C
       KSEC1(31)=NINT(RMINLAT*100000)
       KSEC1(32)=NINT(RMINLON*100000)
       KSEC1(33)=NINT(RMAXLAT*100000)
       KSEC1(34)=NINT(RMAXLON*100000)
C
       J=0
       IF(IPYEAR.NE.0) THEN
          DO I=1,KSEC3(3)
             IYE=IPYEAR+(I-1)*KELEM
             J=J+1
             RYEAR(J)=VALUES(IYE)
          END DO 
       ELSE
          DO I=1,KSEC3(3)
          RYEAR(I)=0.
          END DO
       END IF
       J=0
       IF(IPMONTH.NE.0) THEN
          DO I=1,KSEC3(3)
             IMO=IPMONTH+(I-1)*KELEM
             J=J+1
             RMONTH(J)=VALUES(IMO)
          END DO
       ELSE
          DO I=1,KSEC3(3)
          RMONTH(I)=0.
          END DO
       END IF
       J=0
       IF(IPDAY.NE.0) THEN
          DO I=1,KSEC3(3)
             IDA=IPDAY+(I-1)*KELEM
             J=J+1
             RDAY(J)=VALUES(IDA)
          END DO
       ELSE
          DO I=1,KSEC3(3)
          RDAY(I)=0.
          END DO
       END IF
       J=0
       IF(IPHOUR.NE.0) THEN
          DO I=1,KSEC3(3)
             IHO=IPHOUR+(I-1)*KELEM
             J=J+1
             RHOUR(J)=VALUES(IHO)
          END DO
       ELSE
          DO I=1,KSEC3(3)
          RHOUR(I)=0.
          END DO
       END IF
       J=0
       IF(IPMINUTE.NE.0) THEN
          DO I=1,KSEC3(3)
             IMI=IPMINUTE+(I-1)*KELEM
             J=J+1
             RMINUTE(J)=VALUES(IMI)
          END DO
       ELSE
          DO I=1,KSEC3(3)
          RMINUTE(I)=0.
          END DO
       END IF
       J=0
       IF(IPSECOND.NE.0) THEN
          DO I=1,KSEC3(3)
             ISE=IPSECOND+(I-1)*KELEM
             J=J+1
             RSECOND(J)=VALUES(ISE)
          END DO
       ELSE
          DO I=1,KSEC3(3)
          RSECOND(I)=0.
          END DO
       END IF
C
       
       DO I=1,KSEC3(3)
       IDATE(I)=NINT(RYEAR(I))*10000+NINT(RMONTH(I))*100+NINT(RDAY(I))
       ITIME(I)=NINT(RDAY(I))*1000000+NINT(RHOUR(I))*10000
     1          +NINT(RMINUTE(I))*100 +NINT(RSECOND(I))
       END DO
C
       IDMAX=IDATE(1)
       ITMAX=ITIME(1)
       IDMIN=IDATE(1)
       ITMIN=ITIME(1)
       IMAX=1
       IMIN=1
C
       DO I=1,KSEC3(3)
          IF(IDATE(I).GT.IDMAX) THEN
             IDMAX=IDATE(I)
             ITMAX=ITIME(I)
             IMAX=I
          ELSEIF(IDATE(I).EQ.IDMAX) THEN
             IF(ITIME(I).GT.ITMAX) THEN
                ITMAX=ITIME(I)
                IMAX=I
             END IF
          END IF
       END DO
C
       DO I=1,KSEC3(3)
          IF(IDATE(I).LT.IDMIN) THEN
             IDMIN=IDATE(I)
             ITMIN=ITIME(I)
             IMIN=I
          ELSEIF(IDATE(I).EQ.IDMIN) THEN
             IF(ITIME(I).LT.ITMIN) THEN
                ITMIN=ITIME(I)
                IMIN=I
             END IF
          END IF
       END DO
C
       KSEC1(19)=NINT(RYEAR(IMIN))
       KSEC1(20)=NINT(RMONTH(IMIN))
       KSEC1(21)=NINT(RDAY(IMIN))
       KSEC1(22)=NINT(RHOUR(IMIN))
       KSEC1(23)=NINT(RMINUTE(IMIN))
       KSEC1(24)=NINT(RSECOND(IMIN))
C
       KSEC1(25)=NINT(RYEAR(IMAX))
       KSEC1(26)=NINT(RMONTH(IMAX))
       KSEC1(27)=NINT(RDAY(IMAX))
       KSEC1(28)=NINT(RHOUR(IMAX))
       KSEC1(29)=NINT(RMINUTE(IMAX))
       KSEC1(30)=NINT(RSECOND(IMAX))
C

      RETURN
      END
