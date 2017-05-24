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

      SUBROUTINE BUENS4(KSEC3,KSEC4,KELEM,KVALS,VALUES,CVALS,
     1                  KBUFL,KBUFF,KERR)
C
C**** *BUENS4*
C
C
C     PURPOSE.
C     --------
C          PACK PRELIMINARY ITEMS AND DATA OF SECTION 4 OF BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUENS4(KSEC3,KSEC4,KELEM,KVALS,VALUES,CVALS,
C                         KBUFL,KBUFF,KERR)*
C
C        INPUT :
C               *KSEC3*   -  ARRAY CONTAINING SECTION 3 INFORMATION
C                            KSEC3( 1)-- LENGTH OF SECTION 3 (BYTES)
C                            KSEC3( 2)-- RESERVED
C                            KSEC3( 3)-- NUMBER OF SUBSETS
C                            KSEC3( 4)-- FLAG (DATA TYPE,DATA COMPRESSION)
C               *KSEC4*   -  ARRAY CONTAINING SECTION 4 INFORMATION
C                            KSEC4( 1)-- LENGTH OF SECTION 4 (BYTES)
C                            KSEC4( 2)-- RESERVED
C               *KELEM*   -  NUMBER OF ELEMENTS IN BUFR TEMPLATE
C               *KVALS*   -  DIMENSION OF VALUES ARRAY
C               *VALUES*  -  REAL ARRAY (EXPANDED DATA VALUES)
C
C        OUTPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
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
C          BUPCK          -  PACK BIT PATHERN
C          BUPKS          -  PACK BIT PATHERN IN REPEATED WAY,
C                            POINTER ADJUSTMENT
C          BUOCTN         -  SET LENGTH OF SECTION
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
C          M. DRAGOSAVAC    *ECMWF*       17/01/91.
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
#     include "bcmwt.F"
#     include "bcmwtc.F"
#     include "bcmbef.F"
#     include "bcprq.F"
#     include "bcmoff.F"
C
      CHARACTER*64 CWTEN
      CHARACTER*24 CWTU
C
      DIMENSION KBUFF(KBUFL)
C
      DIMENSION KSEC3(JSEC3),KSEC4(JSEC4)
#ifndef R_4
      REAL*8 VALUES(KVALS)
      REAL*8 RVIND
      REAL*8 EPS
      REAL*8 VAL,VAL8S
      REAL*8 VCHECK
      REAL*8 RVALS(JWORK)
#else
      REAL   VALUES(KVALS)
      REAL   RVIND
      REAL   EPS
      REAL   VAL
      REAL   VCHECK
      REAL   RVALS(JWORK)
#endif
C
      REAL*4 RVALS4(JWORK)
      REAL*4 VAL4,VAL4S
      INTEGER*4 I4
      REAL*8 R8
      REAL*4 R4
      LOGICAL OBIG, OEQUAL
      LOGICAL BIG_ENDIAN
C
      DIMENSION IVALS(JWORK),INC(JELEM),ILOCVAL(JELEM)
      DIMENSION IIVALS(JELEM)
C
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*80 YVAL
C
      DIMENSION IMASK(8),IMAXV(32)
      DATA IMASK/1,2,4,8,16,32,64,128/
C     DATA ILOCVAL/JELEM*0/
C
      DATA IMAXV/1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,
     1  16383,32767,65535,131071,262143,524287,1048575,2097151,
     2  4194305,8388607,16777215,33554431,671108863,134217727,
     3  268435455,536870911,1073741823,2147483647,2147483647/
C
      SAVE IWPTB,IBPTB
C     ------------------------------------------------------------------
C*          1.  PACK PRELIMINARY ITEMS OF SECTION 4.
C               ------------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
      R4=3.4028235e+38
      R8=1.7976931348623157D+308
C
C*          1.1  KEEP POINTERS TO THE BEGINING OF THE SECTION.
C                ---------------------------------------------
 110  CONTINUE
C
      IF(OMULTI) THEN
        IF(NSUBSET.EQ.1) THEN
           IWPTB = NWPT
           IBPTB = NBPT 
        END IF
      ELSE
         IWPTB = NWPT
         IBPTB = NBPT
      END IF

c     IF(OMULTI.AND.NSUBSET.EQ.1) THEN
      IF(NSUBSET.EQ.1) THEN
C
C*          1.2  PACK LENGTH OF SECTION 4.
C                -------------------------
 120  CONTINUE
C
         CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,24,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*) 'ERROR PACKING LENGTH OF SECTION 4.'
            RETURN
         END IF
C
C*          1.4  PACK RESERVED BYTE.
C                -------------------
 140     CONTINUE
C
         CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            WRITE(KNTN,*)'ERROR PACKING RESERVED BYTE IN SECTION 4.'
            RETURN
         END IF
C
      END IF
C     -----------------------------------------------------------------
C*          2. PACK DATA.
C              ----------
 200  CONTINUE
C
C      IKK=KELEM*KSEC3(3)
C      IF(IKK.GT.JWORK) THEN
C         KERR=17
C         CALL BUERR(KERR)
C         WRITE(KNTN,*)'CHECK VALUES OF KELEM AND NUMBER OF SUBSETS KSEC3(3).'
C         RETURN
C      END IF
C      IF(IKK.GT.KVALS) THEN
C         KERR=14
C         CALL BUERR(KERR)
C         WRITE(KNTN,*)'KVALS MUST BE GREATER THAN KELEM*KSEC3(3).'
C         RETURN
C      END IF
C
C*          2.1  CHECK IF DATA HAS TO BE COMRESSED.
C                ----------------------------------
 210  CONTINUE
C
      IB=0
      IF(IAND(KSEC3(4),IMASK(7)).NE.0) IB=1
C
C
C     ------------------------------------------------------------------
C
C*          3.  UNCOMPRESSED DATA.
C               ------------------
 300  CONTINUE
C
      IF(IB.EQ.0) THEN
C
         N=KSEC3(3)
         IF(OMULTI) N=1
         
         DO 301 I=1,N
C
         IF(OMULTI) THEN
            IM1K=(NSUBSET-1)*KELEM
         ELSE
            IM1K=(I-1)*KELEM
         END IF
C
         DO 302 J=1,M
C
         
C        IF(NWTR(J).EQ.31011.OR.NWTR(J).EQ.31012) THEN
C           IRP=VALUES(J+IM1K)             
C           DO III=J+2,J+IRP
C            NWTDW(iii)=0
C           END DO
C        END IF
C
         IF(NWTDW(J).EQ.0) GO TO 302
C
         
         IREF  =NWTRV(J)
         ISCALE=NWTS (J)
         IBDW  =NWTDW(J)
C
         JI=J+IM1K
C
         VAL=VALUES(JI)
         IF(NWTEN(J).EQ.-999) THEN
            IBYTES=NWTDW(J)/8
            IF(ABS(VAL-RVIND)/RVIND.LE.EPS) THEN
               IF(IBYTES.EQ.4) VAL=R4
               IF(IBYTES.EQ.8) VAL=R8
            END IF
            IF(IBYTES.EQ.4) THEN
               VAL4=VAL
               OBIG=BIG_ENDIAN()
               IF(.NOT.OBIG) THEN
                  CALL SWAP_BYTES4(VAL4,VAL4S)
                  VAL4=VAL4S
               END IF
               CALL BUGBYTESR4(VAL4,ILOCVAL,0,8,0,IBYTES)
            ELSE
               OBIG=BIG_ENDIAN()
               IF(.NOT.OBIG) THEN
                  CALL SWAP_BYTES8(VAL,VAL8S)
                  VAL=VAL8S
               END IF
               CALL BUGBYTESR8(VAL,ILOCVAL,0,8,0,IBYTES)
            END IF
            ISKIP=0
            CALL BUPKS(NBPW,KBUFF(NWPT),ILOCVAL,NWPT,NBPT,8,
     1                ISKIP,IBYTES,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)'BUENS4 :'
               WRITE(KNTN,*)'ERROR PACKING REAL IEEE'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 302
         END IF
C
         IF(NWTEN(J).EQ.836970) THEN
            IF(VAL.LT.0) THEN
               IPACK=ABS(NINT(VAL))
               IBDW1=1
               CALL BUPCK(NBPW,KBUFF(NWPT),1,NWPT,NBPT,IBDW1,KERR)
               IBDWM1=IBDW-1
               CALL BUPCK(NBPW,KBUFF(NWPT),IPACK,NWPT,NBPT,IBDWM1,KERR)
            ELSE
               IPACK=NINT(VAL)
               CALL BUPCK(NBPW,KBUFF(NWPT),IPACK,NWPT,NBPT,IBDW,KERR)
            END IF
            GO TO 302
         END IF
C
         IF(NWTEN(J).EQ.658367) THEN
            IF(ABS(VAL-RVIND)/RVIND.LE.EPS) THEN
              YVAL=' '
              NCHAR=NWTDW(J)
              DO 3031 II=1,NCHAR/8
C             IPACK=ICHAR(YVAL(II:II))
              IPACK=255
              IF(IPACK.GT.IMAXV(8)) IPACK=IMAXV(8)
              CALL BUPCK(NBPW,KBUFF(NWPT),IPACK,NWPT,NBPT,8,KERR)
              IF(KERR.GT.0) THEN
                 WRITE(KNTN,*)'ERROR PACKING ',JI,' VALUE FOR ',J,
     1                        ' ELEMENT.'
                 RETURN
              END IF
 3031         CONTINUE
              GO TO 302

            ELSE
              IST=NINT(VAL)/1000
              YVAL=CVALS(IST)
              NCHAR=NWTDW(J)/8
              DO 303 II=1,NCHAR
              IPACK=ICHAR(YVAL(II:II))
              IF(IPACK.GT.IMAXV(8)) IPACK=IMAXV(8)
              CALL BUPCK(NBPW,KBUFF(NWPT),IPACK,NWPT,NBPT,8,KERR)
              IF(KERR.GT.0) THEN
                 WRITE(KNTN,*)'ERROR PACKING ',JI,' VALUE FOR ',J,
     1                        ' ELEMENT.'
                 RETURN
              END IF
 303          CONTINUE
              GO TO 302
            END IF
         END IF
C        
         IF(ABS(VAL-RVIND)/RVIND.LE.EPS) THEN
            CALL BUPCK(NBPW,KBUFF(NWPT),NMASK(IBDW),NWPT,NBPT,IBDW,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)'ERROR PACKING ',JI,' VALUE FOR ',J,
     1                      ' ELEMENT.'
               RETURN
            END IF
         ELSE
C
C        CHECK VAL VALUE FOR POSSIBLE OVERFLOW
C

            IF(NOFL.EQ.1) THEN
               IF(ABS(VAL).LT.EPS) VAL = 0.0
               IF(VAL.GE.0) THEN
                  ICHECK=IMAXV(IBDW)
                  ICHECK=ICHECK+IREF
                  VCHECK=ICHECK*10.**(-ISCALE)
C
                  IF(VAL .GT. VCHECK) THEN
                     WRITE(KNTN,*) 'VALUE=',VAL,'TOO BIG FOR ',
     1                      J,' ELEMENT ',I,' SUBSET.'
                     VAL=VCHECK+1.
                  END IF
               ELSE
                  VCHECK=IREF/10.**ISCALE
                  IF(VAL.LT.VCHECK) THEN
                WRITE(KNTN,*)'VALUE=',VAL,'TOO BIG NEGATIVE FOR ',
     1                      J,' ELEMENT ',I,' SUBSET.'
                     VAL=IREF/10.**ISCALE-1.
                  END IF
               END IF
            END IF

            IF(ISCALE.LT.0) THEN
               ISCALE=IABS(ISCALE)
               IPACK=NINT(VAL/10.**ISCALE) - IREF
            ELSE
               IPACK=NINT(VAL*10.**ISCALE) - IREF
            END IF
C
C           CHECK IF VALUE TO BE PACKED NEGATIVE
C
            IF(IPACK.LT.0) THEN
               KERR=-33
               WRITE(KNTN,*)'BUENS4:'
               WRITE(KNTN,*)'VALUE ',IPACK,' IS NEGATIVE'
               WRITE(KNTN,*)'PROBABLY REFERENCE VALUE TOO BIG.'
               WRITE(KNTN,*)J,' ELEMENT = ',NWTR(J),
     1               ' REFERENCE = ',IREF,
     1               ' SCALE = ',ISCALE,
     1               ' BITS PER VALUE = ',NBPW,
     1               ' VALUE = ',VAL
               IPACK=0
               WRITE(KNTN,*)'ELEMENT VALUE PACKED AS',IPACK
            END IF
C
C*          CHECK IF VALUE TO BE PACKED TOO BIG.
C
            IF(IPACK.GT.IMAXV(IBDW)) THEN
C
               IF(NPMISS.EQ.0) THEN
                KERR=-28
                WRITE(KNTN,*)'VALUE ',IPACK,' TOO BIG.'
                WRITE(KNTN,*)'VALUE FOR ',J,' ELEMENT AND ',I,
     1                       ' SUBSET'
                WRITE(KNTN,*)'PACKED AS MISSING VALUE FOR',
     1                         ' DATA WIDTH -1.'
                IPACK=IMAXV(IBDW)-1
               ELSE
C
C                 ALL ELEMENTS IN CLASS 1 TO 9 MUST BE CORRECT
C
                  IF(NWTR(J).GE.31000.AND.NWTR(J).LE.31012) THEN
                     WRITE(KNTN,*)'VALUE ',IPACK,' TOO BIG.'
                     WRITE(KNTN,*)'VALUE FOR ',J,' ELEMENT AND ',I,
     1                           ' SUBSET'
                     KERR=28
                     CALL BUERR(KERR)
                     RETURN
                  END IF
C
                  KERR=-28
                  WRITE(KNTN,*)'VALUE ',IPACK,' TOO BIG.'
                  WRITE(KNTN,*)'VALUE FOR ',J,' ELEMENT AND ',I,
     1                         ' SUBSET'
                  WRITE(KNTN,*)'PACKED AS MISSING VALUE FOR',
     1                         '  DATA WIDTH.'
                  IPACK=IMAXV(IBDW)
               END IF
            END IF
            CALL BUPCK(NBPW,KBUFF(NWPT),IPACK,NWPT,NBPT,IBDW,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)'ERROR PACKING ',JI,' VALUE FOR ',J,
     1         ' ELEMENT.'
               WRITE(KNTN,*)'VALUE ',IPACK,' DATA WIDTH ',IBDW,
     1         ' BITS'
               RETURN
            END IF
         END IF
C
 302     CONTINUE
 301     CONTINUE
C
         NWP=NWPT
         NBP=NBPT
      END IF
C
C*          4.  COMPRESS DATA.
C               --------------
 400  CONTINUE
C
      IF(IB.EQ.1) THEN
         DO 402 J=1,M
C
         IF(NWTDW(J).EQ.0) GO TO 402
C
         IREF  =NWTRV(J)
         ISCALE=NWTS (J)
         IBDW  =NWTDW(J)
C
         DO 401 I=1,KSEC3(3)
C
         JI=J+(I-1)*KELEM
C
         IF(NWTEN(J).EQ.658367) THEN
            IVALS(JI)=VALUES(JI)
            GO TO 401
         END IF
C
         IF(NWTEN(J).EQ.-999) THEN
            IF(NWTDW(J).EQ.32) THEN
               IF(ABS(VALUES(JI)-RVIND)/RVIND.LT.EPS) THEN
                  RVALS4(JI)=R4
               ELSE
                  RVALS4(JI)=VALUES(JI)
               END IF
            ELSE
              IF(ABS(VALUES(JI)-RVIND)/RVIND.LT.EPS) THEN 
                 RVALS(JI)=R8
               ELSE
                 RVALS(JI)=VALUES(JI)
               END IF
            END IF
            GO TO 401
         END IF
C
         IF(NWTEN(J).EQ.836970) THEN
            ISG_REF=0
            IF(VALUES(JI).LT.0) ISG_REF=1
            IVALS(JI)=IABS(NINT(VALUES(JI)))
            GO TO 401
         END IF
C
         IF(ABS(VALUES(JI)-RVIND)/RVIND.LE.EPS) THEN
            IVALS(JI)=NMASK(IBDW)
         ELSE
            IF(NOFL.EQ.1) THEN
               IF(VALUES(JI).GE.0) THEN
                  ICHECK=IMAXV(IBDW)
                  ICHECK=ICHECK+IREF
                  VCHECK=ICHECK*10.**(-ISCALE)
C
                  IF(VALUES(JI) .GT. VCHECK) THEN
                     WRITE(KNTN,*) 'VALUE=',VALUES(JI),'TOO BIG FOR ',
     1                      J,' ELEMENT AND ',I,' SUBSET.'
                     VALUES(JI)=VCHECK+1.
                  END IF
               ELSE
                  VCHECK=IREF/10.**ISCALE
                  IF(VALUES(JI).LT.VCHECK) THEN 
                     WRITE(KNTN,*) 'VALUE=',VALUES(JI),
     1                      'TOO BIG NEGATIVE FOR ',
     1                      J,' ELEMENT AND ',I,' SUBSET.'
                     VALUES(JI)=IREF/10.**ISCALE-1.
                  END IF
               END IF
            END IF
C
            IF(ISCALE.LT.0) THEN
               ISCAL=IABS(ISCALE)
               IPACK=NINT(VALUES(JI)/10.**ISCAL ) - IREF
            ELSE
               IPACK=NINT(VALUES(JI)*10.**ISCALE) - IREF
            END IF
C
            IF(IPACK.LT.0) THEN
               WRITE(KNTN,*)'BUENS4 :'
               KERR=-33
               WRITE(KNTN,*)'VALUE ',IPACK,' IS NEGATIVE'
               WRITE(KNTN,*)'PROBABLY REFERENCE VALUE TOO BIG.'
               WRITE(KNTN,*)J,'ELEMENT = ',NWTR(J)
               WRITE(KNTN,*) ' REFERENCE = ',IREF
               WRITE(KNTN,*)' SCALE = ',ISCALE
               WRITE(KNTN,*) ' BITS PER VALUE = ',NBPW
               WRITE(KNTN,*) ' VALUE = ',VALUES(JI)
               IPACK=0
               WRITE(KNTN,*)'ELEMENT PACKED AS',IPACK
            END IF
C
C           REPLACE IPACK VALUE WITH MISSING VALUE FOR IBDW -1 
C           IF GREATER THEN MAXIMUM ALLOWED.
C
            IF(IPACK.GT.IMAXV(IBDW)) THEN
C
               IF(NPMISS.EQ.0) THEN
                  KERR=-28
                  WRITE(KNTN,*)'VALUE ',IPACK,' TOO BIG.'
                  WRITE(KNTN,*)'VALUE FOR ',J,' ELEMENT AND ',I,
     1            ' SUBSET'
                  WRITE(KNTN,*)'PACKED AS MISSING VALUE FOR DATA ',
     1                         'WIDTH -1.'
                  IPACK=IMAXV(IBDW)-1
               ELSE
C
C                 ALL ELEMENTS IN CLASS 1 TO 9 MUST BE CORRECT
C
                  IF(NWTR(J).GE.31000.AND.NWTR(J).LE.31012) THEN
                     KERR=28
                     CALL BUERR(KERR)
                     WRITE(KNTN,*)'VALUE ',IPACK,' TOO BIG.'
                     WRITE(KNTN,*)'VALUE FOR ',J,' ELEMENT AND ',I,
     1               ' SUBSET'
                     RETURN
                  END IF
C
                  KERR=-28
                  WRITE(KNTN,*)'VALUE ',IPACK,' TOO BIG.'
                  WRITE(KNTN,*)'VALUE FOR ',J,' ELEMENT AND ',I,
     1            ' SUBSET'
                  WRITE(KNTN,*)'PACKED AS MISSING VALUE.'
                  IPACK=IMAXV(IBDW)
               END IF
            END IF
            IVALS(JI)=IPACK
C
         END IF
C
 401     CONTINUE
 402     CONTINUE
C
C*          4.1  CHECK IF ALL VALUES ARE MISSING.
C
 410  CONTINUE
C
         DO 411 I=1,M
C
         IF(NWTDW(I).EQ.0) GO TO 411
         IBDW  =NWTDW(I)
C
         OMIS=.TRUE.
         IF(NWTEN(I).EQ.658367) THEN
            OMIS=.FALSE.
         ELSEIF(NWTEN(I).EQ.-999) THEN
            OMIS=.FALSE.
         ELSE
            DO 412 J=1,KSEC3(3)
            IJ=I+(J-1)*KELEM
            IF(IVALS(IJ).NE.NMASK(IBDW)) THEN
               OMIS=.FALSE.
            END IF
 412        CONTINUE
         END IF
C
         IF(.NOT.OMIS) THEN
C
            IF(NWTEN(I).NE.658367.AND.NWTEN(I).NE.-999) THEN
C
C              FIND MINIMUM VALUE FOR ELEMENT
C
               MIN=IVALS(I)
               DO 413 J=1,KSEC3(3)
               IJ=I+(J-1)*KELEM
               IF(IVALS(IJ).LT.MIN) MIN=IVALS(IJ)
 413           CONTINUE
C
C              FIND INCREMENTS
C
               DO 414 J=1,KSEC3(3)
               IJ=I+(J-1)*KELEM
               INC(J)=IVALS(IJ)-MIN
               IF(IVALS(IJ).EQ.NMASK(IBDW)) INC(J)=NVIND
 414           CONTINUE
C
C              FIND NUMBER OF BITS NEEDED FOR MAX VALUE OF INCREMENT
C
               MAX=0
               DO 415 J=1,KSEC3(3)
               IF(INC(J).NE.NVIND.AND.INC(J).GT.MAX) MAX=INC(J)
 415           CONTINUE
C
C              CHECK IF ALL INCREMENTS ARE ZERO
C
               INC0=0
               DO 419 J=1,KSEC3(3)
               IF(INC(J).NE.0) INC0=1
 419           CONTINUE
C
C              FIND NUMBER OF BITS NEEDED
C
               IF(INC0.NE.0) THEN
                  MAX=MAX+1
                  DO 416 J=1,32
                  IR=MAX/2
                  IF(IR.EQ.0) GO TO 417
                  MAX=IR
 416              CONTINUE
C
               END IF
C
 417           CONTINUE
C
               INCBIT=0
               IF(INC0.NE.0) INCBIT=J
C
C              REPLACE MISSING VALUES FOR INCREMENT BY ALL BITS SET TO 1.
C
               DO 418 J=1,KSEC3(3)
               IF(INC(J).EQ.NVIND) INC(J)=NMASK(INCBIT)
 418           CONTINUE
            END IF
         END IF
C
C*          4.2  PACK DATA IN COMPRESSED FORM.
C                -----------------------------
 420  CONTINUE
C
         IF(NWTEN(I).EQ.658367) THEN
C
            OSTRING=.TRUE.
            JI1=I
            IST1=IVALS(JI1)/1000
            ICS=NWTDW(I)/8
C           ICS=NINT(VALUES(JI1))-IST1*1000
            NCSMAX=ICS
            DO  IX=ICS,1,-1
            IF(CVALS(IST1)(IX:IX).NE.' ') THEN
               NCS=IX
               GO TO 522
            END IF
            END DO

 522        NCSMAX=NCS
            DO J=2,KSEC3(3)
            JI=I+(J-1)*KELEM
            IST=VALUES(JI)/1000
            IF(CVALS(IST1).NE.CVALS(IST)) OSTRING=.FALSE.
C           FIND MAX SIZE OF STRINGS
            ICS=NWTDW(I)/8
C           ICS=NINT(VALUES(JI))-IST*1000
            DO  IX=ICS,1,-1
            IF(CVALS(IST)(IX:IX).NE.' ') THEN
               NCS=IX
               GO TO 521
            END IF
            END DO
 521        CONTINUE
            IF(NCS.GT.NCSMAX) NCSMAX=NCS 
            END DO

C
            IF(OSTRING) THEN
C
              INCHAR=NWTDW(I)/8
              ISKIP=0
              DO II=1,INCHAR
              IPACK=ICHAR(CVALS(IST1)(II:II))
              CALL BUPCK(NBPW,KBUFF(NWPT),IPACK,NWPT,NBPT,8,KERR) 
              IF(KERR.GT.0) THEN
                 WRITE(KNTN,*)'BUENS4 :'
                 CALL BUERR(KERR)
                 RETURN
              END IF
              END DO
              IPACK=0
              CALL BUPCK(NBPW,KBUFF(NWPT),IPACK,NWPT,NBPT,6,KERR)
              IF(KERR.GT.0) THEN
                 WRITE(KNTN,*)'BUENS4 :'
                 CALL BUERR(KERR)
                 RETURN
              END IF
            ELSE
C
C           PACK LOCAL REFERENCE VALUE FOR ELEMENT
C
            INCHAR=NWTDW(I)/8
            ISKIP=0
            CALL BUPKS(NBPW,KBUFF(NWPT),ILOCVAL,NWPT,NBPT,8,
     1                ISKIP,INCHAR,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)'BUENS4 :'
               WRITE(KNTN,*)'ERROR PACKING LOCAL REFERENCE VALUE'
               CALL BUERR(KERR)
               RETURN
            END IF
C
C           PACK NUMBER OF BITS FOR INCREMENTS/NUMBER OF CHARACTERS
C
C           CALL BUPCK(NBPW,KBUFF(NWPT),INCHAR,NWPT,NBPT,6,KERR)
            CALL BUPCK(NBPW,KBUFF(NWPT),NCSMAX,NWPT,NBPT,6,KERR)
            IF(KERR.GT.0) THEN
              WRITE(KNTN,*)'BUENS4 :'
              WRITE(KNTN,*)'ERROR PACKING NUMBER OF BITS FOR INCREMENTS'
              CALL BUERR(KERR)
              RETURN
            END IF
C
C           PACK INCREMENTS
C
            DO 421 J=1,KSEC3(3)
C
            JI=I+(J-1)*KELEM
C
            IST=IVALS(JI)/1000
            YVAL=CVALS(IST)
C
            DO 423 II=1,NCSMAX
            IPACK=ICHAR(YVAL(II:II))
            CALL BUPCK(NBPW,KBUFF(NWPT),IPACK,NWPT,NBPT,8,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)'BUENS4 :'
               CALL BUERR(KERR)
               RETURN
            END IF
 423        CONTINUE
C
 421        CONTINUE
C
            END IF
         ELSEIF(NWTEN(I).EQ.-999) THEN
C
            OEQUAL=.TRUE.
            DO J=1,KSEC3(3)-1
            JI=I+(J-1)*KELEM
            JII=I+J*KELEM
            IF(NWTDW(I).EQ.32) THEN
               IF(ABS(RVALS4(JI)-RVALS4(JII))/RVALS4(JII).GT.EPS) THEN
                  OEQUAL=.FALSE.
                  GO TO 4444
               END IF

            ELSE
               IF(ABS(RVALS(JI)-RVALS(JII))/RVALS(JII).GT.EPS) THEN
                  OEQUAL=.FALSE.
                  GO TO 4444
               END IF
            END IF
            END DO
C
C           PACK LOCAL REFERENCE VALUE FOR ELEMENT
C
4444        INCHAR=NWTDW(I)/8
            ISKIP=0

            IF(.NOT.OEQUAL) THEN
C
              CALL BUPKS(NBPW,KBUFF(NWPT),ILOCVAL,NWPT,NBPT,8,
     1                  ISKIP,INCHAR,KERR)
              IF(KERR.GT.0) THEN
                 WRITE(KNTN,*)'BUENS4 :'
                 WRITE(KNTN,*)'ERROR PACKING LOCAL REFERENCE VALUE'
                 CALL BUERR(KERR)
                 RETURN
              END IF
C
C             PACK NUMBER OF BITS FOR INCREMENTS/NUMBER OF CHARACTERS
C
              CALL BUPCK(NBPW,KBUFF(NWPT),INCHAR,NWPT,NBPT,6,KERR)
              IF(KERR.GT.0) THEN
                 WRITE(KNTN,*)'BUENS4 :'
                 WRITE(KNTN,*)'ERROR PACKING NUMBER OF BITS
     1                         FOR INCREMENTS'
                 CALL BUERR(KERR)
                 RETURN
              END IF
C
C             PACK INCREMENTS
C
              DO J=1,KSEC3(3)
C
              JI=I+(J-1)*KELEM
C
              IF(NWTDW(I).EQ.32) THEN
                 VAL4=RVALS4(JI)
                 CALL BUGBYTESR4(VAL4,IIVALS,0,8,0,INCHAR)
              ELSE
                 VAL=RVALS(JI)
                 CALL BUGBYTESR8(VAL,IIVALS,0,8,0,INCHAR)
              END IF
C
              DO IZ=1,INCHAR
               IPACK=IIVALS(IZ)
               CALL BUPCK(NBPW,KBUFF(NWPT),IPACK,NWPT,NBPT,8,KERR)
              END DO
C
              END DO

            ELSE

              IBYTES=NWTDW(I)/8
              IF(IBYTES.EQ.4) THEN
                 VAL4=RVALS4(I)
                 OBIG=BIG_ENDIAN()
                 IF(.NOT.OBIG) THEN
                    CALL SWAP_BYTES4(VAL4,VAL4S)
                    VAL4=VAL4S
                 END IF
                 CALL BUGBYTESR4(VAL4,ILOCVAL,0,8,0,IBYTES)
              ELSE
                 VAL=RVALS(I)
                 OBIG=BIG_ENDIAN()
                 IF(.NOT.OBIG) THEN
                    CALL SWAP_BYTES8(VAL,VAL8S)
                    VAL=VAL8S
                 END IF
                 CALL BUGBYTESR8(VAL,ILOCVAL,0,8,0,IBYTES)
              END IF
              ISKIP=0
              CALL BUPKS(NBPW,KBUFF(NWPT),ILOCVAL,NWPT,NBPT,8,
     1                  ISKIP,IBYTES,KERR)
              IF(KERR.GT.0) THEN
                 WRITE(KNTN,*)'BUENS4 :'
                 WRITE(KNTN,*)'ERROR PACKING REAL IEEE'
                 CALL BUERR(KERR)
                 RETURN
              END IF
C
              CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,6,KERR)
              IF(KERR.GT.0) THEN
                 WRITE(KNTN,*)'BUENS4 :'
                 WRITE(KNTN,*)'ERROR PACKING NUMBER OF BITS FOR'
                 WRITE(KNTN,*)'INCREMENTS FOR ',I,' ELEMENT.'
                 RETURN
              END IF

            END IF
C
 4211       CONTINUE
C
         ELSE
             IF(OMIS) THEN
C
C               PACK LOCAL REFERENCE VALUE FOR ELEMENT SET TO MISSING VALUE.
C
                CALL BUPCK(NBPW,KBUFF(NWPT),NMASK(IBDW),NWPT,NBPT,
     1                     IBDW,KERR)
                IF(KERR.GT.0) THEN
                   WRITE(KNTN,*)'BUENS4 :'
                   WRITE(KNTN,*)'ERROR PACKING LOCAL REFERENCE VALUE '
                   WRITE(KNTN,*)I,' ELEMENT.'
                   RETURN
                END IF
C
C               PACK NUMBER OF BITS FOR INCREMENTS (SET TO ZERO)
C
                CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,6,KERR)
                IF(KERR.GT.0) THEN
                   WRITE(KNTN,*)'BUENS4 :'
                   WRITE(KNTN,*)'ERROR PACKING NUMBER OF BITS FOR'
                   WRITE(KNTN,*)'INCREMENTS FOR ',I,' ELEMENT.'
                   RETURN
                END IF
C
             ELSE
C
                IF(INCBIT.EQ.0) THEN
C
C
C                  PACK LOCAL REFERENCE VALUE FOR ELEMENT
C
                 IF(NWTEN(I).EQ.836970) THEN
                  IF(ISG_REF.EQ.1) THEN
                    CALL BUPCK(NBPW,KBUFF(NWPT),1,NWPT,NBPT,1,KERR)
                    CALL BUPCK(NBPW,KBUFF(NWPT),MIN,NWPT,NBPT,IBDW-1,
     1                         KERR)
                  ELSE
                    CALL BUPCK(NBPW,KBUFF(NWPT),MIN,NWPT,NBPT,IBDW,
     1                         KERR)
                    IF(KERR.GT.0) THEN
                      WRITE(KNTN,*)'BUENS4 :'
                      WRITE(KNTN,*)'ERROR PACKING LOCAL REFERENCE VALUE'
                      WRITE(KNTN,*)'FOR ',I,' ELEMENT IN ',IBDW,' BITS.'
                      RETURN
                    END IF
                  END IF
                 ELSE
                   CALL BUPCK(NBPW,KBUFF(NWPT),MIN,NWPT,NBPT,IBDW,KERR)
                   IF(KERR.GT.0) THEN
                      WRITE(KNTN,*)'BUENS4 :'
                      WRITE(KNTN,*)'ERROR PACKING LOCAL REFERENCE VALUE'
                      WRITE(KNTN,*)'FOR ',I,' ELEMENT IN ',IBDW,' BITS.'
                      RETURN
                   END IF
                 ENDIF
C
C                  PACK NUMBER OF BITS FOR INCREMENTS
C
                   CALL BUPCK(NBPW,KBUFF(NWPT),INCBIT,NWPT,NBPT,6,KERR)
                   IF(KERR.GT.0) THEN
                      WRITE(KNTN,*)'BUENS4 :'
                      WRITE(KNTN,*)'ERROR PACKING NUMBER OF BITS FOR'
                      WRITE(KNTN,*)'INCREMENTS FOR ',I,' ELEMENT.'
                      RETURN
                   END IF
C
                ELSE
C
C                  PACK LOCAL REFERENCE VALUE FOR ELEMENT
C
                   IF(NWTEN(I).EQ.836970) THEN
                      IF(ISG_REF.EQ.1) THEN
                         CALL BUPCK(NBPW,KBUFF(NWPT),1,NWPT,NBPT,1,KERR)
                         CALL BUPCK(NBPW,KBUFF(NWPT),MIN,NWPT,NBPT,
     1                              IBDW-1,KERR)
                      ELSE
                         CALL BUPCK(NBPW,KBUFF(NWPT),MIN,NWPT,NBPT,IBDW,
     1                              KERR)
                         IF(KERR.GT.0) THEN
                            WRITE(KNTN,*)'BUENS4 :'
                      WRITE(KNTN,*)'ERROR PACKING LOCAL REFERENCE VALUE'
                      WRITE(KNTN,*)'FOR ',I,' ELEMENT IN ',IBDW,' BITS.'
                            RETURN
                         END IF
                      END IF
                   ELSE
                     CALL BUPCK(NBPW,KBUFF(NWPT),MIN,NWPT,NBPT,IBDW,
     1                          KERR)
                      IF(KERR.GT.0) THEN
                         WRITE(KNTN,*)'BUENS4 :'
                      WRITE(KNTN,*)'ERROR PACKING LOCAL REFERENCE VALUE'
                      WRITE(KNTN,*)'FOR ',I,' ELEMENT IN ',IBDW,' BITS.'
                         RETURN
                      END IF
                   ENDIF

C
C                  PACK NUMBER OF BITS FOR INCREMENTS
C
                   CALL BUPCK(NBPW,KBUFF(NWPT),INCBIT,NWPT,NBPT,6,KERR)
                   IF(KERR.GT.0) THEN
                      WRITE(KNTN,*)'BUENS4 :'
                      WRITE(KNTN,*)'ERROR PACKING NUMBER OF BITS FOR'
                      WRITE(KNTN,*)  'INCREMENTS FOR ',I,' ELEMENT.'
                      RETURN
                   END IF
C
C                  PACK INCREMENTS
C
                   CALL BUPKS(NBPW,KBUFF(NWPT),INC,NWPT,NBPT,
     1                        INCBIT,0,KSEC3(3),KERR)
                   IF(KERR.GT.0) THEN
                      WRITE(KNTN,*)  'BUENS4 :'
                      WRITE(KNTN,*)  'ERROR PACKING INCREMENTS FOR',I,
     1                ' ELEMENT'
                      RETURN
                   END IF
C
                END IF
             END IF
          END IF
C
 411     CONTINUE
C
      END IF
C
C*          5.  SET UP LENGTH OF THE SECTION 4.
C               --------------------------------
 500  CONTINUE
C
      IF(OMULTI) THEN
         IF(NSUBSET.EQ.KSEC3(3)) THEN
            CALL BUOCTN(IWPTB,IBPTB,KBUFL,KBUFF,KERR)
            IF(KERR.GT.0) THEN
               CALL BUERR(KERR)
               RETURN
            END IF
         END IF
      ELSE
         CALL BUOCTN(IWPTB,IBPTB,KBUFL,KBUFF,KERR)
         IF(KERR.GT.0) THEN
            CALL BUERR(KERR)
            RETURN
         END IF
      END IF
C
C     ------------------------------------------------------------------
      RETURN
      END
