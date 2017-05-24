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

      SUBROUTINE BUIVAR(KERR)
C
C**** *BUIVAR*
C
C
C     PURPOSE.
C     --------
C         INITIALIZE CONSTANTS AND VARIABLES.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BUIVAR(KERR)*
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
C     GETENV  - GETS VALUE OF AN ENVIRONMENT VARIABLE.
C     EMOSNUM - GIVES CURRENT EMOSLIB VERSION NUMBER.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/03/92.
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
#     include "bcmatb.F"
#     include "bcmbef.F"
#     include "bcmwork.F"
#     include "bcmwt.F"
#     include "bcmroot.F"
#     include "bcmtabload.F"
#     include "bcmtabloadc.F"
C
      CHARACTER*256 CROOT, HDEFAULT,CPRINT
      CHARACTER*9   STD_OUT,YKNTN
      CHARACTER*24 CTABLE_LIST,CCTABBU
      CHARACTER*64 CCTABBEN
      LOGICAL OPRINT
C
C
      INTEGER IEMOSNM
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif

#ifdef TABLE_PATH
      DATA HDEFAULT / TABLE_PATH /
#else
      DATA HDEFAULT / '' /
#endif

C
C     EXTERNAL FUNCTIONS
C
      INTEGER EMOSNUM
      EXTERNAL EMOSNUM
C
C     ------------------------------------------------------------------
C*          1.   INITIALIZE VARIABLES AND CONSTANTS.
C                -----------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
      NJA= 0
      M  =0
      MM =0
      N  =0
      JCV=0
      NBPW=JBPW
      NWPT=0
      NBPT=0
      NWPTB=0
      NBPTB=0
      NVIND=2147483647
      RVIND=1.7D38
      EPS=10.D-7
      NBENP=0
      NLTVNP=0
      NWWP=0
      NXXP=0
      NYYP=0
      NZZP=0
      NTC=0
      NTT=0
      NDWINC=0
      NSCAM=0
      NSCAM07=0
      NDWINC07=0
      N07=0
      NAFDW=0
      NFD=0
      NWT=0
      ODREPF=.FALSE.
      N221=0
      N40=0
      MREL=0
      NFCM=0
      MBMP=0
      MBMPL=0
      OMARKER=.FALSE.
      CROOT=' '
      NREP=0
      OREP=.false.
      N08=0
      NSSP=0
C
C     STANDARD OUTPUT UNIT NUMBER
C
      STD_OUT=' '
      YKNTN=' '
      CALL GETENV('STD_OUT',YKNTN)
      INKNTN=INDEX(YKNTN,' ')
      IF(INKNTN.EQ.1) THEN
        KNTN=6
      ELSE
        READ(YKNTN,'(I3.3)') KNTN
      END IF
C
      CROOT=' '
      CALL GETENV('BUFR_TABLES',CROOT)
      ILNG=INDEX(CROOT,' ')
      IF(ILNG.LE.1) THEN
        ILNG=INDEX(HDEFAULT,' ') - 1
        IF(ILNG.GT.0) THEN
	  CROOT = HDEFAULT(1:ILNG)//'/bufrtables/'
        ELSE
          CROOT='/home/ma/emos/tables/bufr/text/'
C
C         ADD VERSION NUMBER
C
          ILNG=INDEX(CROOT,' ')
          IEMOSNM = EMOSNUM(1)
          WRITE(CROOT(ILNG:ILNG+5),'(I6.6)') IEMOSNM
          CROOT(ILNG+6:) = '/'
        ENDIF
      ENDIF
C
      CPRINT=' '
      OPRINT=.TRUE.
      CALL GETENV('PRINT_TABLE_NAMES',CPRINT)
      ILN=INDEX(CPRINT,' ')
      IF(ILN.LE.1) THEN
         OPRINT=.TRUE.
      ELSE
         IF(CPRINT(1:ILN-1).EQ.'FALSE'.OR.
     1      CPRINT(1:ILN-1).EQ.'false') OPRINT=.FALSE.
      END IF

      IF(OPRINT) THEN
C
      WRITE(KNTN,'(1H ,A)') '                  ECMWF '
      WRITE(KNTN,'(1H )')
      WRITE(KNTN,'(1H ,A,A)') '     BUFR ENCODING SOFTWARE',
     1' VERSION -  7.2 '
      WRITE(KNTN,'(1H ,A)') '           1 April  2007. '
      WRITE(KNTN,'(1H )')
      WRITE(KNTN,'(1H )')
      WRITE(KNTN,'(1H )')
      WRITE(KNTN,'(1H ,A)') 'Your path for bufr tables is :'
      WRITE(KNTN,'(1H ,A)')  CROOT(1:ILNG+6)
      END IF
C
      DO 101 I=1,JBPW-2
      NMASK(I)=2**I-1
  101 CONTINUE
      IF(JBPW.EQ.32) THEN
         NMASK(31)=2147483647
      END IF
C
      RETURN
      END
