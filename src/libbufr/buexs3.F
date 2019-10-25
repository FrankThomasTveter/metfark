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

      SUBROUTINE BUEXS3(KBUFL,KBUFF,KSUP,KSEC3,KELEM,CNAMES,CUNITS,KERR)
C
C**** *BUEXS3*
C
C
C     PURPOSE.
C     --------
C
C          EXPAND SECTION 3 OF BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS3( KBUFL,KBUFF,KSUP,KSEC3,KELEM,CNAMES,CUNITS,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C               *KELEM*   -  DIMENSION OF CNAMES, CUNITS ARRAY
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
C               *CNAMES*  -  CHARACTER ARRAY CONTAINING ELEMENT NAMES
C               *CUNITS*  -  CHARACTER ARRAY CONTAINIG UNITS
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C      -------
C
C          EXPANDS LIST OF DATA DESCRIPTORS PACKED IN SECTION 3
C     OF BUFR MESSAGE. WORKING TABLES FOR FURTHER DATA DECODING ARE SET,
C     LIST OF PACKED BUFR DATA DESCRIPTORS AND LIST OF BUFR DATA DESCRIPTORS
C     EXPANDED ACCORDING TO TABLE D REFERENCE ARE RETURNED RESPECTIVELY.
C
C
C
C     EXTERNALS.
C     ----------
C
C          BUNEXS        - SET WORD AND BIT POINTERS AT THE BEGINING OF
C                          NEXT SECTION
C          BUNPCK        - UNPACKS BIT PATTERN
C          BUSRP         - SOLVES REPLICATION PROBLEM
C          BUSTDR        - SOLVES TABLE D REFERENCE
C          BUPRCO        - PROCESS OPERATOR
C          BUUPWT        - UPDATES WORKING TABLE
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
#     include "bcomel.F"
#     include "bcprq.F"
#     include "bcomoff.F"
#     include "bcomcom.F"
C
      CHARACTER*64 CWTEN
      CHARACTER*24 CWTU
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
C
      DIMENSION ISTACK(JELEM),IISTACK(JELEM)
      DIMENSION KBUFF(KBUFL)
      DIMENSION IMASK(8)
C
#ifndef R_4
      REAL*8 RQVAL
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RQVAL
      REAL   RVIND
      REAL   EPS
#endif
C
      DIMENSION KSUP(JSUP),KSEC3(JSEC3)
C
      DATA IMASK/1,2,4,8,16,32,64,128/
C
      SAVE NOLD,KELEMOLD
C     ------------------------------------------------------------------
C
C*          1.   EXPAND PRELIMINARY ITEMS OF SECTION 3.
C                --------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C
C*          1.1   SET THE POINTERS NWPT AND NBPT TO THE
C                 -------------------------------------
C                 BEGINING OF THE SECTION 3.
C                 --------------------------
 110  CONTINUE
C
      NWPT = NWPTB
      NBPT = NBPTB
C
C*          1.2   UNPACK LENGTH OF SECTION 3.
C                 ---------------------------
 120  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(1),NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC3(1).'
         RETURN
      END IF
C
C*          1.2.1  SET THE POINTERS NWPTB AND NBPTB TO
C                  -----------------------------------
C                  THE BEGINNING OF THE NEXT SECTION.
C                  ----------------------------------
      CALL BUNEXS(KSEC3(1))
C
C*          1.3    UNPACK ZERO BYTE AND PUT IT IN KSEC3(2).
C                  ----------------------------------------
 130  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(2),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC3(2).'
         RETURN
      END IF
C
C*          1.4    UNPACK NUMBER OF DATA SUB-SETS.
C                  -------------------------------
 140  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(3),NWPT,NBPT,16,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC3(3).'
         RETURN
      END IF
      IF(KSEC3(3).LE.0) THEN
         KERR=32
         WRITE(KNTN,*)  ' BUEXS3 :'
         CALL BUERR(KERR)
         RETURN
      END IF
C
      N = KSEC3(3)
C
C
C*          1.5    UNPACK INTEGER VALUE OF THE OCTET
C                  ---------------------------------
C                  CONTAINING FLAG BITS.
C                  --------------------
 150  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(4),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC3(4).'
         RETURN
      END IF
C
      ICOMP=KSEC3(4)
C     -----------------------------------------------------------------
C
C*          2.   EXPAND DATA DESCRIPTORS.
C                ------------------------
 200  CONTINUE
C
C
C*          2.1  CALCULATE EXPECTED NUMBER OF DATA DESCRIPTORS.
C                ----------------------------------------------
C                AND INITIALIZE NUMBER OF DATA VALUES PER SUB-SET.
C                -------------------------------------------------
 210  CONTINUE
C
      J      = 0
      NWT    = 0
      JMAX   = ( KSEC3(1) - 7)/2
      JMAXNEW=JMAX
C
      IF(JMAX.GT.JELEM) THEN
         WRITE(KNTN,*)  'NUMBER OF ELEMENTS IN SECTION3 TOO BIG.'
         WRITE(KNTN,*)  'PROGRAM CAN NOT HANDLE',JMAX
         WRITE(KNTN,*)  'DATA DESCRIPTORS IN SECTION3.'
         WRITE(KNTN,*)  'MAXIMUM NUMBER OF ELEMENTS IS ',JELEM
         KERR=200
         RETURN
      END IF
C
C*          2.2  UNPACK AND PUT DATA DESCRIPTORS IN STACK.
C                -----------------------------------------
 220  CONTINUE
C
      DO 221 JJ=1,JMAX
C
      CALL BUNPCK(NBPW,KBUFF,IF,NWPT,NBPT,2,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING F PART OF DESCRIPTOR.'
         RETURN
      END IF
      CALL BUNPCK(NBPW,KBUFF,IX,NWPT,NBPT,6,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING X PART OF DESCRIPTOR.'
         RETURN
      END IF
      CALL BUNPCK(NBPW,KBUFF,IY,NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING Y PART OF DESCRIPTOR.'
         RETURN
      END IF
C
      ISTACK(JJ)=IF*100000+IX*1000+IY
      IISTACK(JJ)=ISTACK(JJ)
C
 221  CONTINUE
C
C*          2.2.1 CHECK IF IT IS SAME DATA DESCRIPTOR DESCRIPTION.
C                 ------------------------------------------------
C                 TO MAKE MORE EFFICIENT DATA DESCRIPTOR DESCRIPTION
C                 EXPANSION, IN CASE THAT DELAYED REPLICATION FACTOR
C                 IS NOT PRESENT AND DATA DESCRIPTORS ARE THE SAME,
C                 PREVIOUS WORKING TABLE SHOULD BE USED. IT IS POSIBLE
C                 AT THIS PLACE IN THE FUTURE TO MAKE MORE SOPHISTICATED
C                 CONTROL.
C
C
      DO 222 JC=1,JMAX
C
      IF(ISTACK(JC).NE.NSTACK(JC)) THEN
C
         ODREPF=.FALSE.
C
C        SWAP CONTENT OF THE STACKS.
C
         DO 223 JJC=1,JMAX
         NSTACK(JJC)=ISTACK(JJC)
 223     CONTINUE
C
         NTDLEN = JMAX
         M=0
         M0=1
         NOLD=N
         KELEMOLD=KELEM
         NFCM=0
         NFUCM=0
         MREL=0
         OMARKER=.FALSE.
         MBMP=0
         MBMPL=0
C
         GO TO 230
C
      END IF
C
 222  CONTINUE
C
C*    IF MARKER OPERATOR PRESENT EXPAND DESCRIPTORS AGAIN
C
      IF(OMARKER) THEN
         M=0
         M0=1
         NOLD=N
         KELEMOLD=KELEM
         NFCM=0
         NFUCM=0
         MREL=0
         OMARKER=.FALSE.
         NTDLEN=JMAX
         MBMP=0
         MBMPL=0
         GO TO 230
      END IF
C
C*    CHECK IF THE SAME NUMBER OF DESCRIPTORS
C     AS IN A PREVIOUS MESSAGE
C
      IF(JMAX.NE.NTDLEN) THEN
         M=0
         M0=1
         NOLD=N
         KELEMOLD=KELEM
         NFCM=0
         NFUCM=0
         MREL=0
         OMARKER=.FALSE.
         NTDLEN=JMAX
         MBMP=0
         MBMPL=0
         GO TO 230
      END IF
C
C*    RETURN IF DELAYED REPLICATION FACTOR IS NOT PRESENT.
C
      IF(NPRUS.EQ.1) GO TO 229
C
      OB=.FALSE.
      IF(IAND(KSEC3(4),IMASK(7)).NE.0) OB=.TRUE.
C
C     CHECK FOR DELAYED REPLICATION FACTOR
C
      IF(ODREPF) GO TO 229
C
C     CHECK FOR COMPRESSION
C
      IF(OB) THEN
C
C        DATA COMPRESSED =/ PREVIOUS  --> RECALCULATE POINTERS
C
         GO TO 229
      END IF
C
 229  CONTINUE
C
      M=0
      M0=1
      NOLD=N
      KELEMOLD=KELEM
      NFCM=0
      NFUCM=0
      MREL=0
      OMARKER=.FALSE.
      NTDLEN=JMAX
      MBMP=0
      MBMPL=0
C
C     ------------------------------------------------------------------
C*          2.3  GET NEXT DESCRIPTOR FROM THE STACK.
C                -----------------------------------
 230  CONTINUE
C
      J   = J + 1
      IF(J.GT.JMAX) GO TO 270
C
      IDD = ISTACK(J)
      IF(IDD.EQ.0)  GO TO 230
C
      IF = IDD/100000
C
      IF(NWT.GT.NSTOP) GO TO 270
C     ------------------------------------------------------------------
C*          2.4  CHECK IF IT IS REPLICATION DESCRIPTOR.
C                --------------------------------------
 240  CONTINUE
C
      IF( IF.EQ.0) THEN
C
C*          2.6  ELEMENT DESCRIPTOR, SO UPDATE WORKING TABLE.
C                --------------------------------------------
 260     CONTINUE
C
            CALL BUUPWT(IDD,KELEM,KERR)
            IF(KERR.GT.0) RETURN
C       
      ELSEIF( IF.EQ.1) THEN
C
C*          2.4.1     SOLVE REPLICATION PROBLEM.
C                     --------------------------
C
C
         CALL BUSRP(KBUFL,KBUFF,KSEC3,J,JMAX,IDD,ISTACK,KELEM,KERR)
         IF(KERR.GT.0) RETURN
C
      ELSEIF( IF.EQ.2) THEN
C
C*          2.5.3 PROCESS OPERATOR.
C                 -----------------
            CALL BUPRCO(KBUFL,KBUFF,J,IDD,ISTACK,KELEM,KERR)
            IF(KERR.GT.0) RETURN
C
      ELSEIF( IF.EQ.3) THEN
C
C*          2.5.2 REPLACE BY LIST OF DESCRIPTORS FROM TABLE *D.
C                 ---------------------------------------------
            CALL BUSTDR(J,JMAX,IDD,ISTACK,KERR)
            IF(KERR.GT.0) THEN
               DO 252 IQ=1,JELEM
               NSTACK(IQ)=0.
 252           CONTINUE
               RETURN
            END IF
      ELSE
         KERR=37
         CALL BUERR(KERR)
         RETURN
      END IF
C
      GO TO 230
C
C     ------------------------------------------------------------------
C*          2.7 RESOLVE MARKER OPERATOR.
C               ------------------------
 270  CONTINUE
C
      IF(OMARKER) THEN
         CALL BUPMRK(KBUFL,KBUFF,KSEC3,KELEM,KERR)
         IF(KERR.GT.0) RETURN
      END IF
C
C*          2.8 CHECK IF IT IS CORRESPONDING DATA.
C               ----------------------------------
 280  CONTINUE
C
C     CHECK FOR WORKING SPACE.
C
      IF(JWORK/N.LT.KELEM) THEN
         KERR=17
         WRITE(KNTN,*)  'BUEXS3:'
         CALL BUERR(KERR)
         MN=KELEM*N
         WRITE(KNTN,*)  ' SUGGESTED VALUE FOR JWORK ',MN
         WRITE(KNTN,*)  ' CHECK IF TOO BIG KELEM USED.'
         RETURN
      END IF
C
      IF(IAND(KSEC3(4),IMASK(7)).NE.0) THEN
C
C        COMPRESSED DATA
C
         CALL BURQC(KBUFL,KBUFF,KELEM,CNAMES,CUNITS,KSUP ,KSEC3,KERR)
         IF(KERR.GT.0) RETURN
      ELSE
C
C        UNCOMPRESSED DATA
C
         CALL BURQUC(KBUFL,KBUFF,KELEM,CNAMES,CUNITS,KSUP ,KSEC3,KERR)
         IF(KERR.GT.0) RETURN
C
      END IF
C
C     ------------------------------------------------------------------
C
C*          3. COLLECT  SUPPLEMENTARY ITEMS.
C              -----------------------------
 300  CONTINUE
C
      NTDEXL =M
      DO 301 I=1,NTDEXL
      NTDEXP(I)=INWTR(I)
 301  CONTINUE
C
      NTDLEN=JMAXNEW
      DO 302 I=1,NTDLEN
      NTDLST (I)=IISTACK(I)
 302  CONTINUE
C
      DO 303 I=1,NTDEXL
      IJ=I+(NSUBSET-1)*KELEM
      NWTRG(IJ)=INWTR(I)
      NWTDWG(IJ)=INWTDW(I)
 303  CONTINUE
C
      NSIZE(NSUBSET)  =M
C
      KSUP(3)= 4
      KSUP(5)= NSIZE(1)
      KSUP(6)= KSEC3(3)
C
      N07=0
      N08=0
      N40=0
      NDWINC=0
      NSCAM=0
      NSCAM07=0
      NDWINC07=0
      NFD=0
    
      do i=1,100
      NAFDWA(i)=0
      end do
      RETURN
      END
