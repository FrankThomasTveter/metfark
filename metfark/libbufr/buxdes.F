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

      SUBROUTINE BUXDES(K,KSEC1,KTDLEN,KTDLST,KDLEN,KDATA,KELEM,
     2                  KTDEXL,KTDEXP,CNAMES,CUNITS,KERR)
C
C**** *BUXDES*
C
C
C     PURPOSE.
C     --------
C          EXPAND DATA DESCRIPTORS TO SHOW USER TEMPLATE HE
C          DESCRIBED BY ARAY KTDLST AND KDATA.
C
C
C**   INTERFACE.
C     ----------
C
C               *CALL BUXDES(K,KSEC1,KTDLEN,KTDLST,KDLEN,KDATA,KELEM,
C                            KTDEXL,KTDEXP,CNAMES,CUNITS,KERR)
C
C        INPUT :
C
C               *K*       -  AN INTEGER, PRINTING SWITCH 0 - NO PRINT
C                                                        1 - PRINT
C               *KSEC1*   -  INTEGER ARRAY OF AT LEAST 40 WORDS CONTAINING
C                            SECTION 1 INFORMATION
C               *KTDLEN*  -  INTEGER NUMBER OF DATA DESCRIPTORS IN SECTION 3
C               *KTDLST*  -  INTEGER ARRAY OF AT LEAST KTDLEN WORDS
C                            CONTAINING DATA DESCRIPTORS FOR BUFR SECTION 3
C               *KDLEN*   -  INTEGER (DIMENSION OF KDATA ARRAY)
C               *KDATA*   -  INTEGER ARRAY CONTAINING DATA NEEDED FOR DATA
C                            DESCRIPTOR EXPANSION (DELAYED REPLICATION FACTORS)
C                            WHICH APPEAR IN THE VALUES ARRAY
C
C               *KELEM*   -  INTEGER NUMBER OF ELEMENTS IN BUFR TEMPLATE.
C
C        OUTPUT:
C               *KTDEXL*  - AN INTEGER CONTAINING NUMBER OF EXPANDED ELEMENTS
C               *KTDEXP*  - AN INTEGER ARRAY CONTAINING EXPANDED LIST OF DESCRIPTORS
C               *CNAMES*  - CHARACTER*64 ARRAY OF KELEM CONTAINING ELEMENT NAMES
C               *CUNITS*  - CHARACTER*24 ARRAY OF KELEM CONTAINING ELEMENT UNITS
C               *KERR*    -  RETURN ERROR CODE
C
C
C     METHOD.
C     -------
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
C          M. DRAGOSAVAC    *ECMWF*       15/06/93.
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
#     include "bcmdefc.F"
#     include "bcmbef.F"
#     include "bcmwt.F"
C
      DIMENSION  KSEC1(JSEC1),KSEC3(JSEC3)
#ifndef R_4
      REAL*8 VALUES(JCVAL)
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL  VALUES(JCVAL)
      REAL  RVIND
      REAL  EPS
#endif
      DIMENSION  KTDLST(KTDLEN)
      DIMENSION  KDATA(KDLEN)
      DIMENSION  KTDEXP(KELEM)
C
      CHARACTER*4   CECMWF,CUSER
      CHARACTER*64  CNAMES(KELEM)
      CHARACTER*24  CUNITS(KELEM)

C
C     ------------------------------------------------------------------
C
C*          1.   SET CONSTANTS.
C                --------------
 100  CONTINUE
C
      KERR=0
      IF(CECMWF.NE.'ECMF') THEN
         CALL BUIVAR(KERR)
         KPT   = 0
         CECMWF='ECMF'
      END IF
C
C     -----------------------------------------------------------------
C*          2.   LOAD BUFR TABLES.
C                -----------------
 200  CONTINUE
C
      CALL BUETAB(KSEC1,KERR)
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C          6.  EXPAND DATA DESCRIPTORS.
C              ------------------------
 600  CONTINUE
C

      CALL BUEDD(KPT,KTDLEN,KTDLST,KDLEN,KDATA,KSEC3,
     1           KVALS,VALUES,KELEM,CNAMES,CUNITS,KERR)
C
      IF(KERR.GT.0) RETURN
C
      KTDEXL =M
      DO 301 I=1,KTDEXL
      KTDEXP(I)=NWTR(I)
 301  CONTINUE
C
      IF(K.EQ.1) THEN
C
         WRITE(KNTN,'(1H ,//)')
         WRITE(KNTN,'(1H ,A)')    '       DATA DESCRIPTORS (UNEXPANDED)'
C
         WRITE(KNTN,'(1H )')
         DO 110 I=1,KTDLEN
          WRITE(KNTN,'(1H ,I4,2X,I6.6)') I,KTDLST(I)
 110     CONTINUE
C
         WRITE(KNTN,'(1H ,/)')
         WRITE(KNTN,'(1H ,A)')    '       DATA DESCRIPTORS (EXPANDED)'
         WRITE(KNTN,'(1H )')
         WRITE(KNTN,'(1H ,A,A)')'               ELEMENT NAME',
     1   '                                                      UNIT'
         WRITE(KNTN,'(1H )')
C
         DO 120 I=1,KTDEXL
          WRITE(KNTN,'(1H ,I5,2X,I6.6,2X,A,2X,A)') I,KTDEXP(I),
     1                      CNAMES(I)(1:64),CUNITS(I)(1:24)
 120     CONTINUE
      END IF
C
C
C     -----------------------------------------------------------------
      RETURN
C
      END
