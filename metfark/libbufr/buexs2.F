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

      SUBROUTINE BUEXS2( KBUFL,KBUFF,KSUP,KSEC1,KSEC2,KERR )
C
C**** *BUEXS2*
C
C
C     PURPOSE.
C     --------
C          EXPANDS SECTION 2 OF BUFR MESSAGE. EXPANDED DATA ARE
C     STORED IN THE ARRAY KSEC2.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS2(KBUFL,KBUFF,KSUP,KSEC1,KSEC2,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
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
C               *KSEC1*   -  ARRAY CONTAINING SECTION 1 INFORMATION
C                            KSEC1( 1)-- LENGTH OF SECTION 1 (BYTES)
C                            KSEC1( 2)-- BUFR EDITION NUMBER
C                            KSEC1( 3)-- ORIGINATING CENTRE
C                            KSEC1( 4)-- UPDATE SEQUENCE NUMBER
C                            KSEC1( 5)-- FLAG (PRESENCE OF SECTION 2)
C                            KSEC1( 6)-- BUFR MESSAGE TYPE
C                            KSEC1( 7)-- BUFR MESSAGE SUBTYPE
C                            KSEC1( 8)-- VERSION NUMBER OF LOCAL TABLE USED
C                            KSEC1( 9)-- YEAR
C                            KSEC1(10)-- MONTH
C                            KSEC1(11)-- DAY
C                            KSEC1(12)-- HOUR
C                            KSEC1(13)-- MINUTE
C                            KSEC1(14)-- BUFR MASTER TABLE
C                            KSEC1(15)-- VERSION NUMBER OF MASTER TABLE USED
C                            KSEC1(16) - KSEC1(JSEC1) -- LOCAL ADP CENTRE
C                                        INFORMATION(PACKED FORM)
C
C                            FOR BUFR EDITION 3 ONWARD
C
C                            KSEC1(16)-- ORIGINATING SUB-CENTRE
C                            KSEC1(17)-- NOT USED
C                            KSEC1(18) TO KSEC1(JSEC1) - LOCAL ADP CENTRE
C                                        INFORMATION(PACKED FORM)
C
C                            FOR BUFR EDITION 4 ONWARD
C
C                            KSEC1(16)-- ORIGINATING SUB-CENTRE
C                            KSEC1(17)-- INTERNATIONAL SUB-CATEGORY (SEE COMMON TABLE C-13
C                            KSEC1(18)-- SECOND
C                            KSEC1(19)-- YEAR
C                            KSEC1(20)-- MONTH
C                            KSEC1(21)-- DAY
C                            KSEC1(22)-- HOUR
C                            KSEC1(23)-- MINUTE
C                            KSEC1(24)-- SECOND
C                            KSEC1(25)-- YEAR
C                            KSEC1(26)-- MONTH
C                            KSEC1(27)-- DAY
C                            KSEC1(28)-- HOUR
C                            KSEC1(29)-- MINUTE
C                            KSEC1(30)-- SECOND
C                            KSEC1(31)-- MOST SOUTHERN LATITUDE  (-90 to 90)
C                            KSEC1(32)-- MOST WESTERN LONGITUDE  ( 0-360)
C                            KSEC1(33)-- MOST NORTHEN LATITUDE   (-90 to 90)
C                            KSEC1(34)-- MOST EASTERN LONGITUDE  ( 0-360)
C                            KSEC1(35) TO KSEC1(JSEC1) - LOCAL ADP CENTRE INFORMATION(PACKED FORM)
C
C               *KSEC2*   -  ARRAY CONTAINING SECTION 2 INFORMATION
C                            KSEC2( 1)-- LENGTH OF SECTION 2 (BYTES)
C                            KSEC2( 2) TO KSEC2(JSEC2) LOCAL ADP CENTRE
C                                         INFORMATION(PACKED FORM)
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C     -------
C
C          THE LENGTH OF SECTION 2 AND POINTERS TO THE BEGINING
C     OF SECTION 3 ARE SET. RDB KEY IS THEN UNPACKED AND ALL INFORMATION
C     STORED INTO ARRAY KSEC2.
C
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          - UNPACK BIT PATTERN
C          BUNEXS         - SET WORD AND BIT POINTERS AT THE BEGINING
C                           OF NEXT BUFR SECTION.
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
#     include "bcomwork.F"
#     include "bcprq.F"
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSUP(JSUP),KSEC1(JSEC1),KSEC2(JSEC2)
      DIMENSION ISEC2(JSEC2),IISEC2(JSEC2)
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
#else
      REAL   RVIND
      REAL   EPS
#endif
C
C     ------------------------------------------------------------------
C*          1.  EXPAND SECTION 2.
C               -----------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      DO 101 I=1,JSEC2
      KSEC2(I)=0
 101  CONTINUE
C
      IF( KSEC1(5) .NE. 0 .AND. KSEC1(5) .NE. 128) THEN
          KERR=50
          CALL BUERR(KERR)
          RETURN
      END IF
C
      IF( KSEC1(5).EQ.0) THEN
          KSUP(2) = 1
          KSEC2(1)= 0
          RETURN
      END IF
C
C*          1.1 SET THE POINTERS NWPT AND NBPT AT THE BEGINING OF SECTION.
C              -----------------------------------------------------------
 110  CONTINUE
C
      NWPT = NWPTB
      NBPT = NBPTB
C
C*          1.2  UNPACK LENGTH OF SECTION 2.
C                ---------------------------
      CALL BUNPCK(NBPW,KBUFF,KSEC2(1),NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING KSEC2(1).'
         RETURN
      END IF
C
C*          1.2.1  SET POINTERS NWPTB AND NBPTB TO THE
C                  -----------------------------------
C                  BEGINING OF THE NEXT SECTION.
C                  -----------------------------
      CALL BUNEXS(KSEC2(1))
C
      CALL BUNPCK(NBPW,KBUFF,IDUMMY,NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*)  'ERROR UNPACKING DUMMY OCTET IN SECTION 2.'
         RETURN
      END IF
C
C*          1.3  UNPACK LOCAL ADP CENTRE INFORMATION.
C                ------------------------------------
C
      IOFF=KSEC2(1)-4
C
      IF(IOFF.GT.JSEC2) THEN
         KERR=5
         CALL BUERR(KERR)
         RETURN
      END IF
C
      CALL BUNPKS(NBPW,KBUFF,ISEC2(1),NWPT,NBPT,
     1            8,0,IOFF,KERR)
      IF(KERR.GT.0) THEN
      WRITE(KNTN,*)  'ERROR UNPACKING LOCAL ADP CENTRE INFORMATION'
         WRITE(KNTN,*)  'IN SECTION2.'
         RETURN
      END IF
C
      IF(KSEC1(3).EQ.98) THEN
         IF(NOKEY.EQ.0) THEN
            IF(ISEC2(2).NE.KSEC1(7)) THEN
C
C              BYTES IN THE KEY IN REVERSED ORDER.
C
               J=0
               DO 131 I=1,JSEC2,4
               IISEC2(I)=ISEC2(I+3)
               IISEC2(I+1)=ISEC2(I+2)
               IISEC2(I+2)=ISEC2(I+1)
               IISEC2(I+3)=ISEC2(I)
 131           CONTINUE
               DO 132 I=1,JSEC2
               ISEC2(I)=IISEC2(I)
 132           CONTINUE
            END IF
         END IF
      END IF
C
C                PACK LOCAL ADP CENTRE INFORMATION INTO KSE2(2) ONWARD.
C
      IW=2
      IBT=0
      CALL BUPKS(NBPW,KSEC2(IW),ISEC2,IW,IBT,8,0,IOFF,KERR)
      IF(KERR.GT.0) THEN
         WRITE(KNTN,*) 'ERROR PACKING LOCAL ADP CENTRE',
     1                 ' INFORMATION INTO'
         WRITE(KNTN,*)  'KSEC2(2) ONWARD.'
         RETURN
      END IF
C
C                SET LENGHT OF KSEC2
C
      KSUP(2)=IW
C     ------------------------------------------------------------------
 200  CONTINUE
C
      RETURN
      END
