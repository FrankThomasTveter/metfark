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

      SUBROUTINE BURQC(KBUFL,KBUFF,KELEM,CNAMES,CUNITS,KSUP,KSEC3,KERR)
C
C**** *BURQC*
C
C
C     PURPOSE.
C     --------
C
C          CREATE PARAMETERS NEEDED FOR PARTIAL EXPANSION OF
C     BUFR MESSAGE WITH COMPRESSED DATA ACCORDING TO REQUESTED
C     INPUT LISTS.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BURQC(KBUFL,KBUFF,KELEM,CNAMES,CUNITS,KSUP,KSEC3,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KELEM*   -  DIMENSION OF CNAMES, CUNITS ARRAY
C               *CNAMES*  -  CHARACTER ARRAY CONTAINING ELEMENT NAMES
C               *CUNITS*  -  CHARACTER ARRAY CONTAINIG UNITS
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
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C      -------
C
C          WORD AND BIT POINTERS ARE CALCULATED FOR EVERY ELEMENT
C     IN THE EXPANDED LIST OF ELEMENTS. IF PARTIAL EXPANSION REQUESTED,
C     INDECES TO REQUIRED ELEMENTS ARE DETERMINED, AS WELL AS CORRESPONDING
C     QUALITY CONTROL, STATISTICS ETC. INFORMATION.
C
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          - UNPACKS BIT PATTERN
C          BUNPKS         - UNPACKS BIT PATTERN IN REPEATED WAY
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
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
#     include "bcomwork.F"
#     include "bcombef.F"
#     include "bcomwt.F"
#     include "bcomp.F"
#     include "bcomwtc.F"
#     include "bcomrq.F"
#     include "bcomreq.F"
C
      CHARACTER*64 CWTEN
      CHARACTER*24 CWTU
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
C
      DIMENSION KSUP(JSUP),KSEC3(JSEC3)
      DIMENSION KBUFF(KBUFL)
C
      DIMENSION IRQEI(JELEM),IQCI(JELEM),IQCDPI(JELEM)
      DIMENSION IBVAL(JELEM),IBV(JELEM)
      DIMENSION IC7(JELEM),IC8(JELEM),IC7R(JELEM),IC8R(JELEM)
      DIMENSION NQP(JELEM),ITYPE(100)
C
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
      REAL*8 RQVAL
      REAL*8 VAL
#else
      REAL   RVIND
      REAL   EPS
      REAL   RQVAL
      REAL   VAL
#endif
      DIMENSION IMASK(8)
C
      SAVE IBV,IBVAL
C
      DATA IMASK /1,2,4,8,16,32,64,128/
C
C
C     ------------------------------------------------------------------
C
C*          1.  DEFINE WORD/BIT POINTERS TO EVERY ELEMENT.
C               ------------------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
      NFCM=1
      NFUCM=0
C
C     CHECK REQUEST VALIDITY
C
      IF(NREQ(1).EQ.0.AND.NREQ(2).NE.0) THEN
         KERR=38
         CALL BUERR(KERR)
         RETURN
      END IF
C
      MREL=0
C
      IBP32= 32 + NBPTB
C
      IB1=0
      IF(IAND(NREQ(2),IMASK(1)).NE.0) IB1=1
      IB2=0
      IF(IAND(NREQ(2),IMASK(2)).NE.0) IB2=1
      IB3=0
      IF(IAND(NREQ(2),IMASK(3)).NE.0) IB3=1
      IB4=0
      IF(IAND(NREQ(2),IMASK(4)).NE.0) IB4=1
      IB5=0
      IF(IAND(NREQ(2),IMASK(5)).NE.0) IB5=1
      IB6=0
      IF(IAND(NREQ(2),IMASK(6)).NE.0) IB6=1
C
C*          1.1 COMPRESSED DATA.
C               ----------------
 110  CONTINUE
C
      IBIT=IBP32
      IWORD=IBIT/NBPW
C
      NWORDP(1)=NWPTB+IWORD
      NBITP (1)=IBIT-IWORD*NBPW
C
      DO 111 I=2,M+1
      IWRD=NWORDP(I-1)
      IBTP=NBITP (I-1)
C
      IF(NWTDW(I-1).EQ.0) THEN
         NBITP(I) =NBITP(I-1)
         NWORDP(I)=NWORDP(I-1)
         GO TO 111
      END IF
C
      IBTP=IBTP+NWTDW(I-1)
      IF(IBTP.GE.NBPW) THEN
         IW=IBTP/NBPW
         IBTP=IBTP-IW*NBPW
         IWRD=IWRD+IW
      END IF
C
      CALL BUNPCK(NBPW,KBUFF,IDWINC,IWRD,IBTP,6,KERR)
      IF(KERR.GT.0) RETURN
      IF(IDWINC.GT.JBPW) THEN
            KERR=15
            WRITE(KNTN,*)  'BURQC :'
            CALL BUERR(KERR)
            RETURN
      END IF
      IF(IWRD.GT.KBUFL) THEN
            KERR=26
            WRITE(KNTN,*)  'BURQC :'
            CALL BUERR(KERR)
            RETURN
      END IF
C
      IF(CWTU(I-1).EQ.'CCITTIA5') THEN
         NWTIWS(I-1)=NWTDW(I-1)+6+N*IDWINC*8
      ELSEIF(NWTEN(I-1).EQ.-999) THEN
         NWTIWS(I-1)=NWTDW(I-1)+6+N*IDWINC*8
      ELSE
         NWTIWS(I-1)=NWTDW(I-1)+6+N*IDWINC
      END IF
C
      IBIT = NBITP(I-1) + NWTIWS(I-1)
      IWORD= IBIT/NBPW
C
      NBITP (I)= IBIT - IWORD*NBPW
      NWORDP(I)= NWORDP(I-1) + IWORD
C
      IF(NWORDP(I).GT.KBUFL) THEN
            KERR=26
            WRITE(KNTN,*)  'BURQC :'
            CALL BUERR(KERR)
            RETURN
      END IF
 111  CONTINUE
C
C     -----------------------------------------------------------------
C*          2. CREATE POINTERS FOR REQUESTED ELEMENTS.
C              ---------------------------------------
 200  CONTINUE
C
C
C*          2.1 CHECK IF SUBSET OF ELEMENTS REQUESTED.
C
 210  CONTINUE
C
      IF(NREQ(1).EQ.0) THEN
         DO 212 I=1,M
         INWTEN(I)=NWTEN(I)
         INWTR (I)=NWTR (I)
         INWTS (I)=NWTS (I)
         INWTDW(I)=NWTDW(I)
         INWTRV(I)=NWTRV(I)
         CNAMES(I)=CWTEN(I)
         CUNITS(I)=CWTU(I)
         INWORDP(I)=NWORDP(I)
         INBITP(I)=NBITP(I)
 212     CONTINUE
         RETURN
      END IF
C
      IF(NREQ(1).EQ.1) THEN
         DO 213 I=1,M
         IF(NWTR(I).GT.200000.AND.
     1      NWTR(I).NE.999999) THEN
            MREL=I-1
            GO TO 2133
         END IF
 213     CONTINUE
         MREL=M
C
 2133    CONTINUE
         J=0
         IF(NRQL.EQ.0) THEN
            DO 2131 I=1,MREL
            J=J+1
            INBITP(J)=NBITP(I)
            INWORDP(J)=NWORDP(I)
            INWTEN(J)=NWTEN(I)
            INWTR(J) =NWTR(I)
            INWTS(J) =NWTS(I)
            INWTDW(J)=NWTDW(I)
            INWTRV(J)=NWTRV(I)
            CNAMES(J)=CWTEN(I)
            CUNITS(J)=CWTU(I)
 2131       CONTINUE
C
            M=J
            RETURN
         ELSE
            M=MREL
            GO TO 220
         END IF
      END IF
C
      IF(NREQ(1).EQ.2) THEN
         DO 214 I=1,M
         IF(NWTR(I).EQ.235000) THEN
            M=I-1
            GO TO 2141
         END IF
 214     CONTINUE
C
 2141    CONTINUE
C
         J=0
         IF(NREQ(2).EQ.0) THEN
            DO 2142 I=1,M
            J=J+1
            INBITP(J)=NBITP(I)
            INWORDP(J)=NWORDP(I)
            INWTEN(J)=NWTEN(I)
            INWTR(J)=NWTR(I)
            INWTS(J)=NWTS(I)
            INWTDW(J)=NWTDW(I)
            INWTRV(J)=NWTRV(I)
            CNAMES(J)=CWTEN(I)
            CUNITS(J)=CWTU(I)
 2142       CONTINUE
            RETURN
         ELSE
            GO TO 220
         END IF
      END IF
C
      IF(NREQ(1).EQ.3) THEN
         DO 215 I=1,M
         IF(NWTR(I).EQ.235000) THEN
C
C           COPY CLASS 1 - 8 ELEMENTS
C
            DO 2151 III=1,M
            IF(NWTR(III).GT.8000) THEN
               J=0
               IF(NREQ(2).EQ.0) THEN
                  DO 2161 II=1,III-1
                  J=J+1
                  INBITP(J)=NBITP(II)
                  INWORDP(J)=NWORDP(II)
                  INWTR(J)=NWTR(II)
                  INWTS(J)=NWTS(II)
                  INWTRV(J)=NWTRV(II)
                  INWTDW(J)=NWTDW(II)
                  INWTEN(J)=NWTEN(II)
                  CNAMES(J)=CWTEN(II)
                  CUNITS(J)=CWTU(II)
 2161             CONTINUE
                  GO TO 2160
              ELSE
                  DO 21611 II=1,III-1
                  J=J+1
                  NBITP(J)=NBITP(II)
                  NWORDP(J)=NWORDP(II)
                  NWTR(J)=NWTR(II)
                  NWTS(J)=NWTS(II)
                  NWTRV(J)=NWTRV(II)
                  NWTDW(J)=NWTDW(II)
                  NWTEN(J)=NWTEN(II)
                  CWTEN(J)=CWTEN(II)
                  CWTU(J)=CWTU(II)
21611             CONTINUE
                  GO TO 2160

              END IF
            END IF
 2151       CONTINUE
C
 2160       CONTINUE
C
            IF(NREQ(2).EQ.0) THEN
               DO 216 II=I+1,M
               J=J+1
               INBITP(J)=NBITP(II)
               INWORDP(J)=NWORDP(II)
               INWTR(J)=NWTR(II)
               INWTS(J)=NWTS(II)
               INWTRV(J)=NWTRV(II)
               INWTDW(J)=NWTDW(II)
               INWTEN(J)=NWTEN(II)
               CNAMES(J)=CWTEN(II)
               CUNITS(J)=CWTU(II)
 216           CONTINUE
               M=J
               RETURN
            ELSE
               DO 2169 II=I+1,M
               J=J+1
               NBITP(J)=NBITP(II)
               NWORDP(J)=NWORDP(II)
               NWTR(J)=NWTR(II)
               NWTS(J)=NWTS(II)
               NWTRV(J)=NWTRV(II)
               NWTDW(J)=NWTDW(II)
               NWTEN(J)=NWTEN(II)
               CWTEN(J)=CWTEN(II)
               CWTU(J)=CWTU(II) 
 2169          CONTINUE
               M=J
               GO TO 220
            END IF
         END IF
C
 215     CONTINUE
C
         KERR=39
         CALL BUERR(KERR)
         RETURN
      ELSE
         KERR=40
         CALL BUERR(KERR)
         RETURN
      END IF
C
C
C*          2.2 CREATE INDECES TO CLASS 7/8
C               ---------------------------
 220  CONTINUE
C
      DO 225 I=1,M
      IF(NWTR(I).GT.200000.AND.
     1   NWTR(I).NE.999999) THEN
         MREL=I-1
         GO TO 226
      END IF
 225  CONTINUE
C
      MREL=M
C
 226  CONTINUE
C
      J=0
      JJ=0
      DO 221 I=1,MREL
      ICLASS=NWTR(I)/1000
      IF(ICLASS.EQ.7) THEN
         J=J+1
         IC7(J)=I
      END IF
C
      IF(ICLASS.EQ.8) THEN
         JJ=JJ+1
         IC8(JJ)=I
      END IF
 221  CONTINUE
C
      IC7L=J
      IC8L=JJ
      J=0
      JJ=0
      DO 223 I=1,NRQL
      ICLASS=NRQ(I)/1000
      IF(ICLASS.EQ.7.AND.(ABS(RQVAL(I)-RVIND)/RVIND.GT.EPS)) THEN
         J=J+1
         IC7R(J)=I
      END IF
C
      IF(ICLASS.EQ.8.AND.(ABS(RQVAL(I)-RVIND)/RVIND.GT.EPS)) THEN
         JJ=JJ+1
         IC8R(JJ)=I
      END IF
 223  CONTINUE
C
      IC7RL=J
      IC8RL=JJ
C
C*          2.3 CREATE INDECES TO THE REQUESTED ELEMENTS.
C               -----------------------------------------
 230  CONTINUE
C
      IF(IC7RL.EQ.0.AND.IC8RL.EQ.0) THEN
         IL=MREL
         IRL=NRQL
      END IF
      IF(IC7RL.NE.0.AND.IC8RL.EQ.0) THEN
         IL=IC7(1)
         IRL=IC7R(1)-1
      END IF
      IF(IC8RL.NE.0.AND.IC7RL.EQ.0) THEN
         IL=IC8(1)
         IRL=IC8R(1)-1
      END IF
      IF(IC7RL.NE.0.AND.IC8RL.NE.0) THEN
         IL=IC7(1)
         IF(IC7(1).GT.IC8(1)) IL=IC8(1)
         IRL=IC7R(1)
         IF(IC7R(1).GT.IC8R(1)) IRL=IC8R(1)
         IRL=IRL-1
      END IF
C
      DO 299 I=1,MREL
      IRQEI(I)=0
 299  CONTINUE
C
      ORQEI=.FALSE.
      DO 231 I=1,IL
      IF(IB2.EQ.0.OR.NRQL.EQ.0) THEN
         ORQEI=.TRUE.
         IRQEI(I)=I
      ELSE
         DO 232 K=1,IRL
         IF(NWTR(I).EQ.NRQ(K)) THEN
            ORQEI=.TRUE.
            IRQEI(I)=I
         END IF
 232     CONTINUE
      END IF
 231  CONTINUE
C
      IF(IC7RL.NE.0) THEN
C
C        CLASS 7 SIGNIFYING
C
         IST=1
         DO 233 IJ=1,IC7RL
         III=IC7R(IJ)
         DO 234 I=IST,IC7L
         II=IC7(I)
         IBITP=NBITP(II)
         IWORD=NWORDP(II)

         CALL BUNPCK(NBPW,KBUFF,IVAL,IWORD,IBITP,NWTDW(II),KERR)
         IF(KERR.NE.0) THEN
            CALL BUERR(KERR)
            RETURN
         END IF
         CALL BUNPCK(NBPW,KBUFF,IDWINC,IWORD,IBITP,6,KERR)
         IF(KERR.NE.0) THEN
            CALL BUERR(KERR)
            RETURN
         END IF
         CALL BUNPCK(NBPW,KBUFF,INC,IWORD,IBITP,IDWINC,KERR)
         IF(INC.EQ.NMASK(IDWINC)) IVAL=NMASK(NWTDW(II))
C
         IF(IVAL.NE.NMASK(NWTDW(II))) THEN
            ISCALE=NWTS(II)
            IREF  =NWTRV(II)
            IVAL  =IVAL+IREF+INC
            VAL   =RVIND
            IF(IVAL.NE.NMASK(NWTDW(II))) THEN
               IF(ISCALE.GT.0) THEN
                  VAL=IVAL/10.**ISCALE
               ELSE
                  IISCALE=IABS(ISCALE)
                  VAL=IVAL*10.**IISCALE
               END IF
            END IF
         ELSE
            VAL=RVIND
         END IF
C
         INEXT=IC7(I+1)-1
         IF(IC7(I+1).LE.0) INEXT=MREL
         IF(ABS(RQVAL(III)-VAL).LT.EPS.OR.
     1      ABS(RQVAL(III)-RVIND)/RVIND.LT.EPS) THEN
            DO 236 KI=II,INEXT
            IF(IB2.EQ.0) THEN
               ORQEI=.TRUE.
               IRQEI(KI)=KI
            ELSE
               IE=IC7R(IJ+1)-1
               IF(IJ.EQ.IC7RL) IE=NRQL
               IF(IC8RL.NE.0) THEN
                  DO 238 JA=1,IC8RL
                  IF(IC8R(JA).GT.III.AND.IC8R(JA).LT.IE) THEN
                     IE=IC8R(JA)-2
                     GO TO 298
                  END IF
 238              CONTINUE
               END IF
 298           CONTINUE
               DO 237 IK=III,IE
               IF(NWTR(KI).EQ.NRQ(IK)) THEN
                  ORQEI=.TRUE.
                  IRQEI(KI)=KI
               END IF
 237           CONTINUE
            END IF
 236        CONTINUE
C
            GO TO 233
         END IF
 234     CONTINUE
 233     CONTINUE
C
      END IF
C
      IF(IC8RL.NE.0) THEN
C
C        CLASS 8 SIGNIFYING
C
         IST=1
         DO 241 IJ=1,IC8RL
         III=IC8R(IJ)
         DO 242 I=IST,IC8L
         II=IC8(I)
         IBITP=NBITP(II)
         IWORD=NWORDP(II)
         CALL GBYTE(KBUFF(IWORD),IVAL,IBITP,NWTDW(II))
         ISCALE=NWTS(II)
         IREF  =NWTRV(II)
         IVAL  =IVAL+IREF
         VAL   =RVIND
         IF(IVAL.NE.NMASK(NWTDW(II))) THEN
            IF(ISCALE.GT.0) THEN
               VAL=IVAL/10.**ISCALE
            ELSE
               IISCALE=IABS(ISCALE)
               VAL=IVAL*10.**IISCALE
            END IF
         END IF
C
         INEXT=IC8(I+1)-2
         IF(IC8(I+1).EQ.0) INEXT=MREL
C
         OT=.FALSE.
         IF(ABS(VAL-RVIND)/RVIND.LT.EPS) THEN
            IVAL=NINT(VAL)
            IRQV=NINT(RQVAL(III))
C
            IF(NWTR(II).EQ.008001) THEN
               OT=.TRUE.
               IF(IAND(IVAL,IRQV).NE.IRQV) OT=.FALSE.
            ELSE
               OT=.TRUE.
               IF(ABS(VAL-RQVAL(III)).GT.EPS) OT=.FALSE.
            END IF
         END IF
C
         IF(OT) THEN
            ICL=NWTR(II-1)/1000
            IF(ICL.EQ.7) IRQEI(II-1)=II-1
            DO 244 KI=II,INEXT
            IF(IB2.EQ.0) THEN
               ORQEI=.TRUE.
               IRQEI(KI)=KI
            ELSE
               IE=IC8R(IJ+1)-1
               IF(IJ.EQ.IC8RL) IE=NRQL
               IF(IC7RL.NE.0) THEN
                  DO 248 JA=1,IC7RL
                  IF(IC7R(JA).GT.III.AND.IC7R(JA).LT.IE) THEN
                     IE=IC7R(JA)-1
                     GO TO 297
                  END IF
 248              CONTINUE
               END IF
 297           CONTINUE
               DO 245 IK=III,IE
               IF(NWTR(KI).EQ.NRQ(IK)) THEN
                 ORQEI=.TRUE.
                 IRQEI(KI)=KI
               END IF
 245           CONTINUE
            END IF
 244        CONTINUE
C
         END IF
 242     CONTINUE
 241     CONTINUE
C
      END IF
C
      IF(.NOT.ORQEI) THEN
         KERR=45
         CALL BUERR(KERR)
         M=0
         RETURN
      END IF
C
C
C
C*          3. FIND POINTERS TO QUALITY CONTROL,START OF DATA
C               ----------------------------------------------
C               PRESENT INDICATORS AND %CONFIDENCE.
C               -----------------------------------
C
 300  CONTINUE
C
      I=0
      IF(IB3.NE.0) THEN
         I=I+1
         ITYPE(I)=222000
      END IF
C
      IF(IB4.NE.0) THEN
         I=I+1
         ITYPE(I)=224000
      END IF
C
      IF(IB5.NE.0) THEN
         I=I+1
         ITYPE(I)=225000
      END IF
C
      IF(IB6.NE.0) THEN
         I=I+1
         ITYPE(I)=223000
      END IF
C
      KEND=I
      IF(KEND.EQ.0) THEN
         J=0
         DO 4011 I=1,MREL
         IF(IRQEI(I).NE.0) THEN
            II=IRQEI(I)
            J=J+1
            INWORDP(J)=NWORDP(II)
            INBITP(J)=NBITP(II)
            INWTR(J)=NWTR(II)
            INWTS(J)=NWTS(II)
            INWTRV(J)=NWTRV(II)
            INWTDW(J)=NWTDW(II)
            INWTEN(J)=NWTEN(II)
            CNAMES(J)=CWTEN(II)
            CUNITS(J)=CWTU(II)
         END IF
 4011    CONTINUE
         GO TO 900
      END IF
C
      OLIST=.FALSE.
      O236=.FALSE.
      O237=.FALSE.
C
      DO 500 KT=1,KEND
         JQPR=0
         JQUA=0
         JQCA=0
         JQC =0
         JQCC=0
         JQCS=0
         KM=MREL
C
C
C        FIND POINTERS TO OPERATORS
C
         KZ=0
         DO 3031 I=KM,M
         IF(NWTR(I).EQ.ITYPE(KT)) THEN
            KZ=KZ+1
            NQP(KZ)=I
         END IF
 3031    CONTINUE
C
         IF(KZ.EQ.0) THEN
            WRITE(KNTN,*) ITYPE(KT),' NOT PRESENT IN THIS MESSAGE.'
            GO TO 500
         ELSE
            KZ=KZ+1
            NQP(KZ)=M
         END IF
C
         DO 3033 KQ=1,KZ-1
C
         JQUA=NQP(KQ)
C
         IF(NWTR(JQUA+1).EQ.236000) THEN
            JQ236=JQUA+1
            O236=.TRUE.
         END IF
C
         IF(NWTR(JQUA+1).EQ.237000) THEN
            JQ237=JQUA+1
            O237=.TRUE.
         ELSE
            O237=.FALSE.
         END IF
C
C
C        DATA PRESENT INDICATOR
C
         IF(.NOT.O236.AND.O237) THEN
C
C           FIND LAST DEFINED BIT MAP
C
            DO 420 I=JQUA,MREL,-1
            IF(NWTR(I).EQ.236000) GO TO 421
 420        CONTINUE
C
            KERR=41
            CALL BUERR(KERR)
            RETURN
C
 421        CONTINUE
C
            O236=.TRUE.
            O237=.FALSE.
            JQ236=I
            JQ237=0
            IK=I
            IDPRF=0
            DO 3015 I=IK,M
            IF(NWTR(I).EQ.31031.OR.NWTR(I).EQ.31192) THEN
               IDPRF=IDPRF+1
               IF(IDPRF.EQ.1) JQPR=I
            ELSE
               IF(IDPRF.NE.0) GO TO 30111
            END IF
 3015       CONTINUE
C
 3016       CONTINUE
         END IF
C
C        DATA PRESENT INDICATOR
C
         IF(.NOT.O237) THEN
            IDPRF=0
            DO 301 I=JQUA,NQP(KQ+1)
            IF(NWTR(I).EQ.31031.OR.NWTR(I).EQ.31192) THEN
               IDPRF=IDPRF+1
               IF(IDPRF.EQ.1) JQPR=I
            ELSE
               IF(IDPRF.NE.0) GO TO 30111
            END IF
 301        CONTINUE
         END IF
C
30111   CONTINUE
C
C        GENERATING CENTRE
C
         DO 309 I=JQUA,NQP(KQ+1)
         IF(NWTR(I).EQ.1031) THEN
            JQCC=I
            GO TO 302
         END IF
 309     CONTINUE
C
 302     CONTINUE
C
C        GENERATING APPLICATION
C
         DO 307 I=JQUA,NQP(KQ+1)
         IF(NWTR(I).EQ.1032.OR.NWTR(I).EQ.1201.OR.
     1      NWTR(I).EQ.63191) THEN
            JQCA=I
            GO TO 3071
         END IF
 307     CONTINUE
C
 3071    CONTINUE
C
C        PERCENTAGE CONFIDENCE
C
         IF(ITYPE(KT).EQ.222000) THEN
            DO 305 I=JQUA,NQP(KQ+1)
            IF(NWTR(I)/1000.EQ.33.OR.NWTR(I).EQ.63192) THEN
               JQC=I
               GO TO 306
            END IF
 305        CONTINUE
         ELSE
C           SIGNIFICANCE
C
            DO 308 I=JQUA,NQP(KQ+1)
            IF(NWTR(I)/1000.EQ.8) THEN
               JQCS=I
               GO TO 3088
            END IF
 308        CONTINUE
C
 3088       CONTINUE
C
            DO 3051 I=JQUA,NQP(KQ+1)
            IF(NWTR(I).EQ.223255.OR.NWTR(I).EQ.224255.OR.
     1         NWTR(I).EQ.225255) THEN
               JQC=I
               GO TO 306
            END IF
 3051       CONTINUE
         END IF
C
C
 306     CONTINUE
C
C*          3.1  SET INDICES TO GET Q/C.
C                GET BIT MAP FROM DATA SECTION.
C
 310  CONTINUE
C
         IF(JQUA.EQ.0) THEN
            WRITE(KNTN,'(1H )')
        WRITE(KNTN,'(1H ,A)') 'Q/C NOT PRESENT IN THIS BUFR MESSAGE.'
            WRITE(KNTN,'(1H )')
         ELSE
C
            IF(.NOT.O237) THEN
C
               KAK=0
               IQPR=JQPR-1
               DO 311 K=1,IDPRF
               IQPR=IQPR+1
               KAK=KAK+1
               IWPT=NWORDP(IQPR)
               IBPT=NBITP (IQPR)
               CALL GBYTE(KBUFF(IWPT),IBVAL(KAK),IBPT,1)
 311           CONTINUE
            END IF
C
            IDIF=MREL-IDPRF
C
            JS=0
            IF(IDIF.NE.0) THEN
               DO 313 I=1,IDIF
               JS=JS+1
               IQCDPI(JS)=0
 313           CONTINUE
            END IF
C
            JQPRM1=JQPR-1
            DO 314 I=1,IDPRF
            JS=JS+1
            IQCDPI(JS)=I+JQPRM1
 314        CONTINUE
C
            JS=0
            IF(IDIF.NE.0) THEN
               DO 315 I=1,IDIF
               JS=JS+1
               IQCI(JS)=0
 315           CONTINUE
            END IF
C
            K=JQC-1
            DO 317 I=1,IDPRF
            OK=.TRUE.
            JS=JS+1
            IQCI(JS)=0
            IF(IBVAL(I).EQ.0) THEN
               IF(OK) K=K+1
               IQCI(JS)=K
               OK=.FALSE.
            END IF
 317        CONTINUE
C
         END IF
C
C
C*          4.  CREATE REQUESTED ELEMENT LIST.
C               ------------------------------
 400  CONTINUE
C
      IF(.NOT.OLIST) THEN
         IF(NREQ(2).EQ.0) NRQL=0
         J=0
         DO 401 I=1,MREL
         IF(IRQEI(I).NE.0) THEN
            II=IRQEI(I)
            J=J+1
            INBITP(J)=NBITP(II)
            INWORDP(J)=NWORDP(II)
            INWTR(J)  =NWTR(II)
            INWTS(J)  =NWTS(II)
            INWTRV(J) =NWTRV(II)
            INWTDW(J) =NWTDW(II)
            INWTEN(J) =NWTEN(II)
            CNAMES(J)=CWTEN(II)
            CUNITS(J)=CWTU(II)
            OLIST=.TRUE.
         END IF
 401     CONTINUE
      END IF
C
      IF(JQUA.NE.0) THEN
C
C*          4.1 ADD DATA PRESENT INDICATOR AND Q/C.
C               -----------------------------------
 410     CONTINUE
C
C                  ADD OPERATOR 222000
         J=J+1
         INWORDP(J)=NWORDP(JQUA)
         INBITP (J)=NBITP (JQUA)
         INWTR(J)  =NWTR(JQUA)
         INWTS(J)  =NWTS(JQUA)
         INWTRV(J) =NWTRV(JQUA)
         INWTDW(J) =NWTDW(JQUA)
         INWTEN(J) =NWTEN(JQUA)
         CNAMES(J)=CWTEN(JQUA)
         CUNITS(J)=CWTU(JQUA)
C
         IF(O236.AND..NOT.O237) THEN
            J=J+1
            INWORDP(J)=NWORDP(JQ236)
            INBITP (J)=NBITP (JQ236)
            INWTR(J)  =NWTR(JQ236)
            INWTS(J)  =NWTS(JQ236)
            INWTRV(J) =NWTRV(JQ236)
            INWTDW(J) =NWTDW(JQ236)
            INWTEN(J) =NWTEN(JQ236)
            CNAMES(J)=CWTEN(JQ236)
         END IF
C
         IF(O237) THEN
            J=J+1
            INWORDP(J)=NWORDP(JQ237)
            INBITP (J)=NBITP (JQ237)
            INWTR (J) =NWTR (JQ237)
            INWTS (J) =NWTS (JQ237)
            INWTRV(J) =NWTRV(JQ237)
            INWTDW(J) =NWTDW(JQ237)
            INWTEN(J) =NWTEN(JQ237)
            CNAMES(J)=CWTEN(JQ237)
            CUNITS(J)=CWTU (JQ237)
         END IF
C
         IF(.NOT.O237) THEN 
            DO 412 I=1,MREL
            IF(IRQEI(I).NE.0) THEN
               IF(IQCDPI(I).NE.0) THEN
                  J=J+1
                  INWORDP(J)=NWORDP(IQCDPI(I))
                  INBITP (J)=NBITP (IQCDPI(I))
                  INWTR(J)  =NWTR(IQCDPI(I))
                  INWTS(J)  =NWTS(IQCDPI(I))
                  INWTRV(J) =NWTRV(IQCDPI(I))
                  INWTDW(J) =NWTDW(IQCDPI(I))
                  INWTEN(J) =NWTEN(IQCDPI(I))
                  CNAMES(J)=CWTEN(IQCDPI(I))
                  CUNITS(J)=CWTU(IQCDPI(I))
               END IF
            END IF
 412        CONTINUE
         END IF
C
C
C        GENERATING CENTRE
C
         IF(JQCC.NE.0) THEN
            J=J+1
            INWORDP(J)=NWORDP(JQCC)
            INBITP (J)=NBITP (JQCC)
            INWTR(J)  =NWTR(JQCC)
            INWTS(J)  =NWTS(JQCC)
            INWTRV(J) =NWTRV(JQCC)
            INWTDW(J) =NWTDW(JQCC)
            INWTEN(J) =NWTEN(JQCC)
            CNAMES(J)=CWTEN(JQCC)
            CUNITS(J)=CWTU(JQCC)
         END IF
C
C        Q/C APPLICATION
C
         IF(JQCA.NE.0) THEN
            J=J+1
            INWORDP(J)=NWORDP(JQCA)
            INBITP (J)=NBITP (JQCA)
            INWTR(J)  =NWTR(JQCA)
            INWTS(J)  =NWTS(JQCA)
            INWTRV(J) =NWTRV(JQCA)
            INWTDW(J) =NWTDW(JQCA)
            INWTEN(J) =NWTEN(JQCA)
            CNAMES(J)=CWTEN(JQCA)
            CUNITS(J)=CWTU(JQCA)
         END IF
C
C        SIGNIFICANCE
C
         IF(JQCS.NE.0) THEN
            J=J+1
            INWORDP(J)=NWORDP(JQCS)
            INBITP (J)=NBITP (JQCS)
            INWTR(J)  =NWTR(JQCS)
            INWTS(J)  =NWTS(JQCS)
            INWTRV(J) =NWTRV(JQCS)
            INWTDW(J) =NWTDW(JQCS)
            INWTEN(J) =NWTEN(JQCS)
            CNAMES(J)=CWTEN(JQCS)
            CUNITS(J)=CWTU(JQCS)
         END IF
C
C
C        CLASS 33 ELEMENTS
C
         DO 415 I=1,MREL
         IF(IRQEI(I).NE.0) THEN
            OK=.TRUE.
            IF(IQCI(I).NE.0) THEN
               IF(OK) J=J+1
               OK=.FALSE.
               INWORDP(J)=NWORDP(IQCI(I))
               INBITP (J)=NBITP (IQCI(I))
               INWTR(J)  =NWTR  (IQCI(I))
               INWTS(J)  =NWTS  (IQCI(I))
               INWTRV(J) =NWTRV (IQCI(I))
               INWTDW(J) =NWTDW (IQCI(I))
               INWTEN(J) =NWTEN (IQCI(I))
               CNAMES(J)=CWTEN (IQCI(I))
               CUNITS(J)=CWTU  (IQCI(I))
            END IF
         END IF
 415     CONTINUE
      END IF
 3033 CONTINUE
C
C
C*          5. MAKE ONE TO ONE CORRESPONDENCE BETWEEN ELEMENTS AND
C              Q/C,STATISTICS,DIFFERENCE STATISTICS OR SUBSTITUTED VALUES
C              LEAVING BEHIND DATA PRESENT INDICATORS.
 500  CONTINUE
C
C
C*          9.  UPDATE TOTAL NUMBER OF ELEMENTS.
C               --------------------------------
 900  CONTINUE
C
      M=J
      NFCM=1
      NFUCM=0
C
      RETURN
C
      END
