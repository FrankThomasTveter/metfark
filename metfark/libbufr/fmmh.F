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

      SUBROUTINE FMMH(KLA,KLO,KELEM,VALUES,KSEC3,RMINLAT,RMINLON,
     1                RMAXLAT,RMAXLON)
C
C**** *FMM*
C
C
C     PURPOSE.
C     --------
C         FIND MINIMUM AND MAXIMUM LATITUDE AND LOGITUDE.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *FMMH(KLA,KLO,VALUES,KSEC3,RMINLAT,RMINLON,
C                      RMAXLAT,RMAXLON)*
C
C     METHOD.
C     -------
C
C          NONE.
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
C          M. DRAGOSAVAC    *ECMWF*       15/04/1991.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
C
      DIMENSION KSEC3(*)
#ifndef R_4
      REAL*8 VALUES(*)
      REAL*8 RNLON
      REAL*8 RNLAT
      REAL*8 RMINLAT
      REAL*8 RMINLON
      REAL*8 RMAXLAT
      REAL*8 RMAXLON
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL VALUES(*)
      REAL RNLON
      REAL RNLAT
      REAL RMINLAT
      REAL RMINLON
      REAL RMAXLAT
      REAL RMAXLON
      REAL RVIND
      REAL EPS

#endif
C     -----------------------------------------------------------------
C*          1.   FIND MIN/MAX LATITUDES/LONGITUDES.
C                ----------------------------------
 100  CONTINUE
C
      RVIND=1.7D38
      EPS=10.D-7
C
      ISUB=KSEC3(3)
C
C
      DO 101 I=1,ISUB
      KLAI=KELEM*(I-1)+KLA
      IF(ABS(VALUES( KLAI)-RVIND)/RVIND.GT.EPS)
     1   THEN
            RMINLAT=VALUES( KLAI)
            GO TO 105
         END IF
 101  CONTINUE
C
 105  CONTINUE
C
      DO 110 I=1,ISUB
C
      KLAI=KELEM*(I-1)+KLA
      IF(ABS(VALUES( KLAI)-RVIND)/RVIND.LT.EPS) GO TO 110
      RNLAT=VALUES( KLAI)
      IF(RNLAT.LT.RMINLAT) RMINLAT=RNLAT
C
 110  CONTINUE
C
      DO 111 I=1,ISUB
      KLOI=KELEM*(I-1)+KLO
      IF(ABS(VALUES( KLOI)-RVIND)/RVIND.GT.EPS) THEN
         RMINLON=VALUES( KLOI)+180.
         GO TO 115
      END IF
 111  CONTINUE
C
 115  CONTINUE
C
      DO 120 I=1,ISUB
C
      KLOI=KELEM*(I-1)+KLO
      IF(ABS(VALUES( KLOI)-RVIND)/RVIND.LT.EPS) GO TO 120
      RNLON=VALUES( KLOI)+180.
      IF(RNLON.LT.RMINLON) RMINLON=RNLON
C
 120  CONTINUE
C
      DO 121 I=1,ISUB
      KLAI=KELEM*(I-1)+KLA
      IF(ABS(VALUES(KLAI)-RVIND)/RVIND.GT.EPS) THEN
         RMAXLAT=VALUES( KLAI)
         GO TO 125
      END IF
 121  CONTINUE
C
 125  CONTINUE
C
      DO 130 I=1,ISUB
C
      KLAI=KELEM*(I-1)+KLA
      IF(ABS(VALUES( KLAI)-RVIND)/RVIND.LT.EPS) GO TO 130
      RNLAT=VALUES( KLAI)
      IF(RNLAT.GT.RMAXLAT) RMAXLAT=RNLAT
C
 130  CONTINUE
C
C
      DO 131 I=1,ISUB
      KLOI=KELEM*(I-1)+KLO
      IF(ABS(VALUES( KLOI)-RVIND)/RVIND.GT.EPS) THEN
         RMAXLON=VALUES( KLOI)+180.
         GO TO 135
      END IF
 131  CONTINUE
C
 135  CONTINUE
C
      DO 140 I=1,ISUB
C
      KLOI=KELEM*(I-1)+KLO
      IF(ABS(VALUES( KLOI)-RVIND)/RVIND.LT.EPS) GO TO 140
      RNLON=VALUES( KLOI)+180.
      IF(RNLON.GT.RMAXLON) RMAXLON=RNLON
C
 140  CONTINUE
C
      RMINLON=RMINLON-180.

      RMAXLON=RMAXLON-180.
C
C
C*          2. CHECK IF ALL OBSERVATION FITS IN THE BOX.
C              -----------------------------------------
 200  CONTINUE
C
      DO 201 I=1,ISUB
C
      KLAI=KELEM*(I-1)+KLA
      KLOI=KELEM*(I-1)+KLO
      IF((VALUES( KLAI).LT.RMINLAT.AND.VALUES( KLAI).GT.RMAXLAT).OR.
     1   (VALUES( KLOI).LT.RMINLON.AND.VALUES( KLOI).GT.RMAXLON) )
     2   GO TO 210
 201  CONTINUE
C
C     ALL DATA IN THE BOX
C
      RETURN
C
C*          2.1 FIND MAX NEGATIVE LONGITUDE AND MIN POSITIVE LONGITUDE.
C               -------------------------------------------------------
 210  CONTINUE
C
      DO 211 I=1,ISUB
      KLOI=KELEM*(I-1)+KLO
      IF(ABS(VALUES( KLOI)-RVIND)/RVIND.GT.EPS) THEN
         IF(VALUES( KLOI).LT.0) THEN
            RMAXLON=VALUES( KLOI)
            GO TO 215
         END IF
      END IF
 211  CONTINUE
C
C     NEGATIVE LONGITUDES NOT FOUND
C
      PRINT*,'FMM:NEGATIVE LONGITUDES NOT FOUND'
      RETURN
C
 215  CONTINUE
C
      DO 220 I=1,ISUB
C
      KLOI=KELEM*(I-1)+KLO
      IF(ABS(VALUES( KLOI)-RVIND)/RVIND.LT.EPS) GO TO 220
      IF(VALUES( KLOI).LT.0) THEN
         RNLON=VALUES( KLOI)

         IF(RNLON.GT.RMAXLON) RMAXLON=RNLON
      END IF
C
 220  CONTINUE
C
C*          2.3 FIND MIN POSITIVE LONGITUDE.
C
 230  CONTINUE
      DO 231 I=1,ISUB
      KLOI=KELEM*(I-1)+KLO
      IF(ABS(VALUES( KLOI)-RVIND)/RVIND.GT.EPS) THEN
         IF(VALUES( KLOI).GE.0) THEN
            RMINLON=VALUES( KLOI)
            GO TO 235
         END IF
      END IF
 231  CONTINUE
C
C     POSITIVE LONGITUDES NOT FOUND
C
      PRINT*,'FMM:POSITIVE LONGITUDES NOT FOUND'
      RETURN
C
 235  CONTINUE
C
      DO 232 I=1,ISUB
C
      KLOI=KELEM*(I-1)+KLO
      IF(ABS(VALUES( KLOI)-RVIND)/RVIND.LT.EPS) GO TO 232
      IF(VALUES( KLOI).GE.0) THEN
         RNLON=VALUES( KLOI)
         IF(RNLON.LT.RMINLON) RMINLON=RNLON
      END IF
C
 232  CONTINUE

      RETURN
      END
