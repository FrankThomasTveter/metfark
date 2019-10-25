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

      SUBROUTINE BUEXS4(KBUFL,KBUFF,KSUP,KSEC3,KSEC4,KELEM,CNAMES,
     1                  CUNITS,KVALS,VALUES,CVALS,KERR)
C
C**** *BUEXS4*
C
C
C     PURPOSE.
C     --------
C          EXPAND PRELIMINARY ITEMS AND DATA OF SECTION 4 OF BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS4(KBUFL,KBUFF,KSUP,KSEC3,KSEC4,KELEM,CNAMES,
C                         CUNITS,KVALS,VALUES,CVALS,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KELEM*   -  DIMENSION OF CNAMES, CUNITS ARRAY
C               *KVALS*   -  DIMENSION OF VALUES ARRAY
C        OUTPUT:
C               *KSUP*    -  ARRAY CONTAINING SUPLEMENTARY INFORMATION
C                         -  KSUP( 1) -- IDIM1, DIMENSION OF KSEC1
C                         -  KSUP( 2) -- IDIM2, DIMENSION OF KSEC2
C                         -  KSUP( 3) -- IDIM3, DIMENSION OF KSEC3
C                         -  KSUP( 4) -- IDIM4, DIMENSION OF KSEC4
C                         -  KSUP( 5) -- M (NUMBER OF ELEMENTS IN VALUES ARRAY,
C                                           FIRST INDEX)
C                         -  KSUP( 6) -- N (NUMBER OF SUBSETS,SECOND INDEX OF
C                                           VALUES ARRAY)
C                         -  KSUP( 7) -- JVC (NUMBER OF ELEMENTS IN CVAL ARRAY)
C                         -  KSUP( 8) -- TOTAL BUFR MESSAGE LENGTH IN BYTES
C                         -  KSUP( 9) -- IDIM0, DIMENSION OF KSEC0
C               *KSEC3*   -  ARRAY CONTAINING SECTION 3 INFORMATION
C                            KSEC3( 1)-- LENGTH OF SECTION 3 (BYTES)
C                            KSEC3( 2)-- RESERVED
C                            KSEC3( 3)-- NUMBER OF SUBSETS
C                            KSEC3( 4)-- FLAG (DATA TYPE,DATA COMPRESSION)
C               *KSEC4*   -  ARRAY CONTAINING SECTION 4 INFORMATION
C                            KSEC4( 1)-- LENGTH OF SECTION 4 (BYTES)
C                            KSEC4( 2)-- RESERVED
C               *CNAMES*  -  CHARACTER ARRAY CONTAINING ELEMENT NAMES
C               *CUNITS*  -  CHARACTER ARRAY CONTAINIG UNITS
C               *VALUES*  -  REAL ARRAY (EXPANDED DATA VALUES)
C               *CVALS*   -  CHARACTER ARRAY CONTAINING TEXT
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
C          BUNPCK          -  UNPACK BIT PATTERN
C          BUNPKS          -  UNPACK BIT PATTERN IN REPEATED WAY,
C                             POINTER ADJUSTMENT
C          BUUNPS          -  UNPACK BIT PATTERN IN REPEATED WAY,
C                             NO POINTER ADJUSTMENT
C          BUUNP           -  UNPACK BIT PATTERN, NO POINTER ADJUSTMENT
C          BUNEXS          -  SET WORD AND BIT POINTERS AT THE BEGINING OF
C                             NEXT SECTION
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
#     include "bcomreq.F"
#     include "bcomunit.F"
#     include "bcomwork.F"
#     include "bcomwt.F"
#     include "bcomp.F"
#     include "bcombef.F"
#     include "bcomrq.F"
#     include "bcomoff.F"
C
      CHARACTER CTEXT*64
      CHARACTER CNAMR*64,CUNIR*24
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION ICH(255),ILIST(JWORK),IVALUES(JWORK) 
      DIMENSION IILIST(8),IR0LIST(8)
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      REAL*4 VVV4,VVV4S
      REAL*8 VVV8S,VVV8,VAL8S
      REAL*4 R4, RHUGE4
      REAL*8 R8, RHUGE8
      LOGICAL OBIG, BIG_ENDIAN
C
#ifndef R_4
      REAL*8 VALUES(KVALS),RQVAL
      REAL*8 RVIND
      REAL*8 EPS
      REAL*8 VVV
#else
      REAL   VALUES(KVALS),RQVAL
      REAL   RVIND
      REAL   EPS
      REAL   VVV
#endif
      LOGICAL CHECK_S4,OS4
      DIMENSION KSUP(JSUP),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION IMASK(8),IRO(80)
      DATA IMASK/1,2,4,8,16,32,64,128/

C
C     ------------------------------------------------------------------
C*          1.  EXPAND PRELIMINARY ITEMS FROM SECTION4.
C               ---------------------------------------
 100  CONTINUE
C
C                JCV - POINTER TO CVALS ARRAY
C                JWT - POINTER TO WORKING TABLE ARRAY
C                JNS - POINTER TO VALUES ARRAY FOR SUB-SETS.
C
      IF(KERR.GT.0) RETURN
C
      R4=3.4028235e+38
      R8=1.7976931348623157D+308
C
      IF(OMULTI) THEN
         IF(NSUBSET.EQ.1) JCV = 0
      ELSE
        JCV = 0
      END IF
      OREPF =.FALSE.
C
      IF(KELEM*N.GT.KVALS) THEN
         KERR=14
         CALL BUERR(KERR)
         WRITE(KNTN,*)  ' BUEXS4: NUMBER OF ELEMENTS ',M
         WRITE(KNTN,*)  ' BUEXS4: NUMBER OF SUBSETS  ',N
         MN=M*N
         WRITE(KNTN,*)  ' BUEXS4: SUGGESTED VALUE FOR KVALS ',MN
         WRITE(KNTN,*)  ' BUEXS4: SUGGESTED VALUE FOR KELEM ',M
         RETURN
      END IF
C
C*          1.1  SET THE POINTERS NWPT AND NBPT
C                ------------------------------
C                TO THE BEGINING OF THE SECTION.
C                -------------------------------
 110  CONTINUE
C
      NWPT = NWPTB
      NBPT = NBPTB
      NWPTB4 = NWPTB
      NBPTB4 = NBPTB
C
C*          1.2  UNPACK LENGTH OF SECTION 4.
C                ---------------------------
 120  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC4(1),NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC4(1).'
         RETURN
      END IF
C
      IF(OS4.AND.NREQ(1).EQ.0) THEN
        CALL CHECK_S4S(KELEM,KSEC3,KSEC4,KERR)
        IF(KERR.NE.0) THEN
           return
        END IF
      END IF
C
C
C*          1.3  SET THE POINTERS NWPTB AND NBPTB.
C                ---------------------------------
C                TO BEGINING OF THE NEXT SECTION.
C                --------------------------------
 130  CONTINUE
C
      CALL BUNEXS(KSEC4(1))
C
C
C*          1.4  EXPAND RESERVED BYTE.
C                ---------------------
 140  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC4(2),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC4(2).'
         RETURN
      END IF
C
C     -----------------------------------------------------------------
C*          2. EXPAND DATA.
C              ------------
 200  CONTINUE
C
C
C*          2.1  CHECK IF DATA ARE COMRESSED.
C                ----------------------------
 210  CONTINUE
C
      IB=0
      IF(IAND(KSEC3(4),IMASK(7)).NE.0) IB=1
C
      IF(IB.EQ.0) THEN
C
C     ------------------------------------------------------------------
C
C*          3.  UNCOMPRESSED DATA.
C               ------------------
 300  CONTINUE
C
         IF(OMULTI) N=1
C
         DO 301 JNS=1,N
C
         ITEMPREP=0

         IF(OMULTI) THEN
            JNSK=(NSUBSET-1)*KELEM
            JNSK1=(JNS-1)*KELEM
         ELSE
            JNSK=(JNS-1)*KELEM
         END IF
C
         DO 302 JWT=1,M
C
         IF(OMULTI) THEN
            JWTJNS1=JWT+JNSK1
            JWTJNS=JWT+JNSK
         ELSE
            JWTJNS=JWT+JNSK
         END IF
C
c        IF(ITEMPREP.GT.0) THEN
c           IF(NWTR(JWT-1).NE.031011.AND.NWTR(JWT-1)
c    1         .NE.031012) THEN
c              VALUES(JWTJNS)=VALUES(JWTJNS-1)
c              ITEMPREP=ITEMPREP-1
c              GO TO 302
c           ELSE
c              ITEMPREP=ITEMPREP-1
c           END IF
c        END IF

         IF(INWTDW(JWT).EQ.0) THEN
            VALUES(JWTJNS)=0.0
            GO TO 302
         END IF
C
         IF(OMULTI) THEN
            NWPT=INWORDP(JWTJNS1)
            NBPT=INBITP (JWTJNS1)
         ELSE
            NWPT=INWORDP(JWTJNS)
            NBPT=INBITP (JWTJNS)
         END IF
C
c        IF(INWTR(JWT).EQ.31011.OR.INWTR(JWT).EQ.31012) OREPF=.TRUE.
C
C
C     ------------------------------------------------------------------
C*          3.1 CHARACTER DATA / IEEE ?
C               -----------------------
 310  CONTINUE
C
         IF(INWTEN(JWT).EQ.-999) THEN
            IBYTES=INWTDW(JWT)/8
            CALL BUNPKS(NBPW,KBUFF,ILIST,NWPT,NBPT,
     1                  8,0,IBYTES,KERR)
            IF(IBYTES.EQ.4) THEN
               OBIG=BIG_ENDIAN()
               CALL BUSBYTESR4(VVV4,ILIST,0,8,0,IBYTES)
               IF(.NOT.OBIG) THEN
                  CALL SWAP_BYTES4(VVV4,VVV4S)
                  VVV4=VVV4S
               END IF
               IF(ABS(VVV4-R4)/R4.LT.EPS) THEN
                  VALUES(JWTJNS)=RVIND
               ELSE
                  VALUES(JWTJNS)=VVV4
               END IF
            ELSE
               OBIG=BIG_ENDIAN()
               CALL BUSBYTESR8(VVV8,ILIST,0,8,0,IBYTES)
               IF(.NOT.OBIG) THEN
                  CALL SWAP_BYTES8(VVV8,VVV8S)
                  VVV8=VVV8S
               END IF
               IF(ABS(VVV8-R8)/R8.LT.EPS) THEN
                  VALUES(JWTJNS)=RVIND
               ELSE
                  VALUES(JWTJNS)=VVV8
               END IF
            END IF
            GO TO 302
         END IF
C
         IF(INWTEN(JWT).EQ.658367) THEN
C
C*          3.2  OBTAIN CHARACTER DATA FROM DATA SECTION.
C                ----------------------------------------
 320  CONTINUE
C
            IY=INWTDW(JWT)/8
C
            CALL  BUUNPS(NBPW,KBUFF,ICH,NWPT,NBPT,8,0,IY,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)  'ERROR UNPACKING CHARACTER DATA.'
               RETURN
            END IF
C
C*          3.3  MOVE CHARACTER DATA TO "CVALS".
C                -------------------------------
 330  CONTINUE
C
            IYLINE=IY/80
            IYOFF =IY-IYLINE*80
            JCVINC=IYLINE
C
            JCV = JCV + 1
            JCVW= JCV
C
            IF(IYLINE.EQ.0) THEN
               CVALS(JCVW)=' '
               DO 331 J=1,IY
               CVALS(JCVW)(J:J)=CHAR(ICH(J))
 331           CONTINUE
            ELSE
               CVALS(JCVW)=' '
               DO 332 J=1,IYLINE
               DO 333 JJ=1,80
               CVALS(JCVW)(JJ:JJ)= CHAR(ICH(JJ))
 333           CONTINUE
C
               JCVW=JCVW+1
C
 332           CONTINUE
C
               CVALS(JCVW)=' '
               DO 334 J=1,IYOFF
               CVALS(JCVW)(J:J)= CHAR(ICH(J))
 334           CONTINUE
C
            END IF
C
C*          3.4  COMPUTE POINTER VALUES TO BE STORED IN "VALUES".
C                ------------------------------------------------
 340  CONTINUE
C
            VALUES(JWTJNS) = JCV*1000 + IY
            JCV            = JCV + JCVINC
C
            GO TO 302
C
         END IF
C
C*          3.5  OBTAIN VALUE FROM DATA SECTION.
C                -------------------------------
 350  CONTINUE
C
         IF(NWTEN(JWT).EQ.836970) THEN
            ISGN=0
            IWPT=NWPT
            IBPT=NBPT
C
            CALL GBYTE(KBUFF(IWPT),ISGN,IBPT,1)
C
            IBPT=IBPT+1
            IWORD= IBPT/NBPW
            IBPT= IBPT - IWORD*NBPW
            IWPT= IWPT + IWORD
C
            CALL GBYTE(KBUFF(IWPT),IVAL,IBPT,INWTDW(JWT)-1)
            VALUES(JWTJNS)= DFLOAT(IVAL)
            IF(ISGN.EQ.1) VALUES(JWTJNS)=-VALUES(JWTJNS)
            GO TO 302
         ELSE
            CALL GBYTE(KBUFF(NWPT),IVAL,NBPT,INWTDW(JWT))
         END IF
C
C
C     -----------------------------------------------------------------
C*          3.6  UPDATE THE ARRAY "VALUES".
C                --------------------------
 360  CONTINUE
C
C
C        CHECK IF DATA IS MISSING
C
         IF(IVAL.EQ.NMASK(INWTDW(JWT))) THEN
            IF(INWTDW(JWT).NE.1) THEN
               IF(INWTR(JWT).NE.999999.AND.
     1            INWTR(JWT).NE.030001.AND.INWTR(JWT).NE.030002.AND.
     2            INWTR(JWT).NE.030004) THEN
                  VALUES(JWTJNS)=RVIND
               ELSE
                  VALUES(JWTJNS)=ival
               END IF
            ELSE
               VALUES(JWTJNS)=1.0
            END IF
         ELSE
            IVAL=IVAL+INWTRV(JWT)
            IF(INWTS(JWT).GT.0) THEN
               VALUES(JWTJNS)= DFLOAT(IVAL)/10.**INWTS(JWT)
            ELSE
               IIWTS=IABS(INWTS(JWT))
               VALUES(JWTJNS)= DFLOAT(IVAL)*10.**IIWTS
            END IF
         END IF
C
c        IF(INWTR(JWT).EQ.31011.OR.INWTR(JWT).EQ.31012) THEN
c           ITEMPREP=VALUES(JWTJNS)
c        END IF

 302     CONTINUE
C
         KSUP(7)=JCV
C
 301     CONTINUE
C
         IF(OMULTI) THEN
            IBIT=NBITP(NSIZEG(NSUBSET)) + NWTDW(NSIZEG(NSUBSET))
            IWORD=IBIT/NBPW
            NBP=IBIT-IWORD*NBPW
            NWP=NWORDP(NSIZEG(NSUBSET))+IWORD
         END IF
      ELSE 
C     ------------------------------------------------------------------
C
C*          4.  COMPRESSED DATA.
C               ----------------
 400  CONTINUE
C
C*          4.1 OBTAIN N VALUES BY EXPANSION.
C               -----------------------------
 410  CONTINUE
C
         DO 411 JWT=1,M
C
         NWPT=INWORDP(JWT)
         NBPT=INBITP (JWT)
C
         IF(INWTDW(JWT).EQ.0) THEN
            DO 412 J=1,N
            JWTJ=JWT+(J-1)*KELEM
            VALUES(JWTJ)=0.0
 412        CONTINUE
C
            GO TO 411
         END IF
C
C        CHECK IF CHARACTER DATA
C
         IF(INWTEN(JWT).EQ.658367) THEN
            IICH=INWTDW(JWT)/8
            CALL BUNPKS(NBPW,KBUFF,IRO,NWPT,NBPT,8,0,IICH,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)  'ERROR UNPACKING REFERENCE VALUES FOR'
               WRITE(KNTN,*)  JWT,' ELEMENT, OF ',J,' SUBSET.'
               RETURN
            END IF
            CALL BUNPCK(NBPW,KBUFF,IDWINC,NWPT,NBPT,6,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)  'ERROR UNPACKING NUMBER OF BITS FOR ',
     1                        'INCREMENTS'
               WRITE(KNTN,*)  'FOR ',JWT,' ELEMENT, OF ',J,' SUBSET'
               RETURN
            END IF
C
            ITOTAL=IDWINC*KSEC3(3)
            IF(ITOTAL.GT.JWORK) THEN
               KERR=17
               CALL BUERR(KERR)
               RETURN
            END IF 
C
            DO 413 J=1,ITOTAL
            ILIST(J)=0
 413        CONTINUE
C
            CALL BUNPKS(NBPW,KBUFF,ILIST,NWPT,NBPT,
     1                  8,0,ITOTAL,KERR)
            IF(KERR.GT.0) RETURN
C
C           MOVE CHARACTER DATA TO "CVALS" ARRAY.
C
            IY=IDWINC
            IF(IY.EQ.0) THEN
               IY=IICH
               IZ=0
               IF(IICH*KSEC3(3).GT.JWORK) THEN
                  KERR=17
                  CALL BUERR(KERR)
                  RETURN
               END IF
               DO IXX=1,KSEC3(3)
               DO IZZ=1,IICH
               IZ=IZ+1
               ILIST(IZ)=IRO(IZZ)
               END DO
               END DO
            END IF
C
            IYLINE=IY/80
            IYOFF=IY-IYLINE*80
C
            JCVINC=IYLINE
            JJC=0
C
            DO 414 I=1,KSEC3(3)
C
            JCV=JCV+1
            JCVW=JCV
C
            IF(IYLINE.EQ.0) THEN
               CVALS(JCVW)=' '
               DO 415 J=1,IY
               JJC=JJC+1
               CVALS(JCVW)(J:J)=CHAR(ILIST(JJC))
 415           CONTINUE
C
            ELSE
C
               CVALS(JCVW)=' '
               DO 416 J=1,IYLINE
               DO 417 JJJ=1,80
               JJC=JJC+1
               CVALS(JCVW)(JJJ:JJJ)=CHAR(ILIST(JJC))
 417           CONTINUE
C
               JCVW=JCVW+1
 416           CONTINUE
C
               CVALS(JCVW)=' '
               DO 418 J=1,IYOFF
               JJC=JJC+1
               CVALS(JCVW)(J:J)=CHAR(ILIST(JJC))
 418           CONTINUE
C
            END IF
C
C           COMPUTE POINTERS TO VALUES ARRAY
C
            JWTI=JWT+(I-1)*KELEM
C           VALUES(JWTI)=JCV*1000+IY
            VALUES(JWTI)=JCV*1000+INWTDW(JWT)/8
            JCV         =JCV+JCVINC
C
 414        CONTINUE
C
            GO TO 411
C
         ELSEIF(INWTEN(JWT).EQ.-999) THEN
            IICH=INWTDW(JWT)/8
            CALL BUNPKS(NBPW,KBUFF,IR0LIST,NWPT,NBPT,8,0,IICH,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)  'ERROR UNPACKING REFERENCE VALUES FOR',
     1                         JWT,' ELEMENT, OF ',J,' SUBSET.'
               RETURN
            END IF
            CALL BUNPCK(NBPW,KBUFF,IDWINC,NWPT,NBPT,6,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)  'ERROR UNPACKING NUMBER OF BITS FOR ',
     1                        'INCREMENTS'
               WRITE(KNTN,*)  'FOR ',JWT,' ELEMENT, OF ',J,' SUBSET'
               RETURN
            END IF
C
            IF(IDWINC.NE.0) THEN
               ITOTAL=IDWINC*KSEC3(3)
               DO J=1,ITOTAL
                 ILIST(J)=0
               END DO
C
               CALL BUNPKS(NBPW,KBUFF,ILIST,NWPT,NBPT,
     1                     8,0,ITOTAL,KERR)
               IF(KERR.GT.0) RETURN
C
C
C              COMPUTE POINTERS TO VALUES ARRAY
C
               JJC=0
               DO I=1,KSEC3(3)
                JWTI=JWT+(I-1)*KELEM
C
                DO J=1,IICH
                 JJC=JJC+1
                 IILIST(J)=ILIST(JJC)
                END DO

                IF(IICH.EQ.4) THEN
                  CALL BUSBYTESR4(RHUGE4,IILIST,0,8,0,IICH)
                   OBIG=BIG_ENDIAN()
                   IF(.NOT.OBIG) THEN
                      CALL SWAP_BYTES4(RHUGE4,VAL4S)
                      RHUGE4=VAL4S
                   END IF
                  VALUES(JWTI)=RHUGE4
                  IF(ABS(VALUES(JWTI)-R4)/R4.LT.EPS) THEN
                     VALUES(JWTI)=RVIND               
                  END IF
                ELSE
                  CALL BUSBYTESR8(RHUGE8,IILIST,0,8,0,IICH)
                  OBIG=BIG_ENDIAN()
                   IF(.NOT.OBIG) THEN
                      CALL SWAP_BYTES8(RHUGE8,VAL8S)
                      RHUGE8=VAL8S
                   END IF
                  VALUES(JWTI)=RHUGE8
                  IF(ABS(VALUES(JWTI)-R8)/R8.LT.EPS) THEN
                     VALUES(JWTI)=RVIND               
                  END IF
                END IF
C
               END DO
            ELSE
                IF(IICH.EQ.4) THEN
                   CALL BUSBYTESR4(RHUGE4,IR0LIST,0,8,0,IICH)
                   OBIG=BIG_ENDIAN()
                   IF(.NOT.OBIG) THEN
                      CALL SWAP_BYTES4(RHUGE4,VAL4S)
                      RHUGE4=VAL4S
                   END IF
                   DO I=1,KSEC3(3)
                    JWTI=JWT+(I-1)*KELEM
                    IF(ABS(RHUGE4-R4)/R4.LT.EPS) THEN
                       VALUES(JWTI)=RVIND
                    ELSE 
                       VALUES(JWTI)=RHUGE4
                    END IF
                   END DO
                ELSE          
                   CALL BUSBYTESR8(RHUGE8,IR0LIST,0,8,0,IICH)
                   OBIG=BIG_ENDIAN()
                   IF(.NOT.OBIG) THEN
                      CALL SWAP_BYTES8(RHUGE8,VAL8S)
                      RHUGE8=VAL8S
                   END IF
                   DO I=1,KSEC3(3)
                    JWTI=JWT+(I-1)*KELEM
                    IF(ABS(RHUGE8-R8)/R8.LT.EPS) THEN
                       VALUES(JWTI)=RVIND
                    ELSE
                       VALUES(JWTI)=RHUGE8
                    END IF
                   END DO
                END IF 
            END IF
C
            GO TO 411
         ELSE
            IF(INWTEN(JWT).EQ.836970) THEN
               CALL BUNPCK(NBPW,KBUFF,ISG_REF,NWPT,NBPT,1,KERR)
               CALL BUNPCK(NBPW,KBUFF,IR0,NWPT,NBPT,INWTDW(JWT)-1,KERR)
               IF(KERR.GT.0) THEN
                  WRITE(KNTN,*)  'ERROR UNPACKING REFERENCE VALUES FOR'
                  WRITE(KNTN,*)  JWT,' ELEMENT.'
                  RETURN
               END IF
            ELSE
               CALL BUNPCK(NBPW,KBUFF,IR0,NWPT,NBPT,INWTDW(JWT),KERR)
               IF(KERR.GT.0) THEN
                  WRITE(KNTN,*)  'ERROR UNPACKING REFERENCE VALUES FOR'
                  WRITE(KNTN,*)  JWT,' ELEMENT.'
                  RETURN
               END IF
            END IF
            CALL BUNPCK(NBPW,KBUFF,IDWINC,NWPT,NBPT,6,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)  'ERROR UNPACKING NUMBER OF BITS FOR'
               WRITE(KNTN,*)  'INCREMENTS, FOR ',JWT,' ELEMENT.'
               RETURN
            END IF
            IF(IDWINC.GT.JBPW) THEN
               KERR=15
               WRITE(KNTN,*)  ' BUEXS4 :'
               CALL BUERR(KERR)
               PRINT*,'JWT=',JWT,'REF=',INWTR(JWT)
               RETURN
            END IF
         END IF
C
         DO 423 J=1,N
         ILIST(J)=0
 423     CONTINUE
C
         IF(IDWINC.NE.0) THEN
            CALL BUNPKS(NBPW,KBUFF,ILIST,NWPT,NBPT,
     1                  IDWINC,0,N,KERR)
            IF(KERR.GT.0) THEN
               WRITE(KNTN,*)  'ERROR UNPACKING INCREMENTS ',
     1                        'FOR ',JWT,' ELEMENT.'
               RETURN
            END IF
         END IF
C
         IF(INWTEN(JWT).NE.658367) THEN
            IF(IR0.EQ.NMASK(INWTDW(JWT))) THEN
               DO 425 J=1,N
               IVALUES(J)=NMASK(INWTDW(JWT))
 425           CONTINUE
            ELSE
               IWTPR0=INWTRV(JWT)+IR0
C
               IF(IDWINC.EQ.0) THEN
                  DO 426 J=1,N
                  IVALUES(J)=IWTPR0
 426              CONTINUE
C
               ELSE
                  DO 424 J=1,N
                  IF(ILIST(J).EQ.NMASK(IDWINC)) THEN
                     IVALUES(J)=NMASK(INWTDW(JWT))
                  ELSE
                     IVALUES(J)= IWTPR0 + ILIST(J)
                  END IF
 424              CONTINUE
C
               END IF
            END IF
         END IF
C
         DO 427 J=1,N
C
         JWTJ=JWT+(J-1)*KELEM
C
         IF(INWTDW(JWT).NE.1) THEN 
            VALUES(JWTJ)=RVIND
         ELSE
            VALUES(JWTJ)=1.0
         END IF
            
         IF(IVALUES(J).NE.NMASK(INWTDW(JWT))) THEN
            IF(INWTS(JWT).GT.0) THEN
               VALUES(JWTJ)= DFLOAT(IVALUES(J))/10.**INWTS(JWT)
            ELSE
               IIWTS=IABS(INWTS(JWT))
               VALUES(JWTJ)= DFLOAT(IVALUES(J))*10.**IIWTS
            END IF
         END IF
C
         IF(INWTEN(JWT).EQ.836970.AND.ISG_REF.EQ.1) THEN
            VALUES(JWTJ)= - VALUES(JWTJ)
         END IF
 427     CONTINUE
C
 411     CONTINUE
C
         KSUP(7)=JCV
C
      END IF
C
C     ------------------------------------------------------------------
C*             5.  REPEAT ENTRIES IN CNAMES,CUNITS AND VALUES IF NEEDED.
C                  -----------------------------------------------------
 500  CONTINUE
C
      IF(.NOT.OREPF) GO TO 600
      IST=1
C
C*             5.1 SEARCH CNAMES FOR DELAYED REPETITION FACTOR.
C                  --------------------------------------------
 510  CONTINUE
C
C      DO 511 J=IST,M
C      IF(INWTR(J).EQ.31001.OR.INWTR(J).EQ.31002
C     1   .OR.INWTR(J).EQ.31000) GO TO 520
C 511  CONTINUE
C
C      GO TO 600
C
C*             5.2 GET REPETITION FACTOR FROM ARRAY "VALUES".
C                  -------------------------------------------
 520  CONTINUE
C
C      IREPF=VALUES(J)
C      CNAMR=CNAMES(J+1)
C      CUNIR=CUNITS(J+1)
C      VALUR=VALUES(J+1)
C
C*             5.3 PUSH DOWN ENTRIES IN CNAMES,CUNITS,VALUES.
C                  ------------------------------------------
 530  CONTINUE
C
C      IREPM2=IREPF-2
C
C      DO 531 JA=M,J+2,-1
C
C      CNAMES(JA+IREPM2)=CNAMES(JA)
C      CUNITS(JA+IREPM2)=CUNITS(JA)
C
C      DO 932 JB=1,N
C
C      JAJB=JA+(JB-1)*KELEM
C
C      VALUES(JAJB+IREPM2)=VALUES(JAJB)
C 932  CONTINUE
C
C 531  CONTINUE
C
C*             5.4 REPETITION.
C                  -----------
 540  CONTINUE
C
C      DO 541 JA=1,IREPF
C      CNAMES(J+JA)=CNAMR
C      CUNITS(J+JA)=CUNIR
C
C      DO 542 JB=1,N
C
C      JAJB=JA+(JB-1)*KELEM
C
C      VALUES(J+JAJB)=VALUR
C 542  CONTINUE
C
C 541  CONTINUE
C
C*             5.5 UPDATE M AND POINTER TO CONTINUE SEARCH.
C                  ----------------------------------------
C 550  CONTINUE
C
C      M=M+IREPF-2
C      IST=J+IREPF
C
C      GO TO 510
C
C     -----------------------------------------------------------------
C*            6.  SET SUPPLEMENTARY INFORMATION.
C                 ------------------------------
C
 600  CONTINUE
C
      KSUP(4)= 2
C
C     ------------------------------------------------------------------
      RETURN
      END
