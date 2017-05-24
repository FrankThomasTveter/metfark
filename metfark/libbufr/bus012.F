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

      SUBROUTINE BUS012( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KERR)
C
C**** *BUS012*
C
C
C     PURPOSE.
C     --------
C          EXPANDS SECTION 0,1 AND 2 OF BUFR MESSAGE.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUS012( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KERR)*
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
C               *KSEC0*   -  ARRAY CONTAINING SECTION 0 INFORMATION
C                            KSEC0( 1)-- LENGTH OF SECTION 0 (BYTES)
C                            KSEC0( 2)-- TOTAL LENGTH OF BUFR MESSAGE (BYTES)
C                            KSEC0( 3)-- BUFR EDITION NUMBER
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
C                            KSEC2( 2) TO KSEC2(47) RDB KEY
C               *KERR*    -  RETURNED ERROR CODE
C
C     METHOD.
C     --------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUEXS0          - UNPACK SECTION 0
C          BUEXS1          - UNPACK SECTION 1
C          BUEXS2          - UNPACK SECTION 2
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/07/91.
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
C
#     include "parameter.F"
#     include "bcomunit.F"
#     include "bcomwork.F"
#     include "bcomwt.F"
#     include "bcomdefc.F"
#     include "bcomreq.F"
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSUP(*),KSEC0(*),KSEC1(*)
      DIMENSION KSEC2(*)
      DIMENSION KSEC3(JSEC3)
#ifndef R_4
      REAL*8 RVIND
      REAL*8 EPS
      REAL*8 RQVAL
#else
      REAL   RVIND
      REAL   EPS
      REAL   RQVAL
#endif
C
      CHARACTER*4 CECMWF,CUSER
C
C     ------------------------------------------------------------------
C*          1.   SET CONSTANTS.
C                --------------
 100  CONTINUE
C
      KERR=0
C
      IF(CECMWF.NE.'ECMF') THEN
         CALL BUEVAR(KERR)
         CECMWF='ECMF'
      END IF
C
      IF(CUSER.NE.'USER') THEN
         NREQ(1)=0
         NREQ(2)=0
         NRQL=0
      END IF
C     -----------------------------------------------------------------
C
C*          2.  EXPAND SECTION 0.
C               -----------------
 200  CONTINUE
C
      CALL BUEXS0( KBUFL,KBUFF,KSUP,KSEC0,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C
C*          3.   EXPAND SECTION 1.
C                ------------------
 300  CONTINUE
C
      CALL BUEXS1( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C
C*          4.  EXPAND SECTION 2.
C               -----------------
 400  CONTINUE
C
      CALL BUEXS2( KBUFL,KBUFF,KSUP,KSEC1,KSEC2,KERR )
      IF(KERR.GT.0) RETURN
C
C*          5.  EXPAND SECTION 3 (PRELIMINARY ITEMS).
C               -------------------------------------
 500  CONTINUE
C
      CALL BUEXS3P(KBUFL,KBUFF,KSUP,KSEC3,KERR)
C
      RETURN
      END
