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

      SUBROUTINE BUSRQ(KREQ,KRQL,KRQ,RQV,KERR)
C
C**** *BUSRQ*
C
C
C     PURPOSE.
C     --------
C          SET LIST OF BUFR TABLE B ELEMENT REFERENCES
C     FOR PARTIAL EXPANSION.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUSRQ(KREQ,KRQL,KRQ,RQV,KERR)*
C
C        INPUT :
C               *KREQ*    -  FLAG
C                            BIT NUMBER     MEANING
C                                1        - 0 NO BIT MAP DELIVERED TO USER
C                                           1    BIT MAP DELIVERED TO USER
C                                2        - 0 NO PARTIAL EXPANSION
C                                           1    PARTIAL EXPANSION
C                                3        - 0 NO Q/C REQUIRED
C                                         - 1    Q/C REQUIRED
C                                4        - 0 NO STATISTICS REQUIRED
C                                         - 1    STATISTICS
C                                5        - 0 NO DIFFRENCE STATISTICS
C                                         - 1    DIFFERENCE STATISTICS
C                                6        - 0 NO SUBSTITUTED VALUES
C                                         - 1    SUBSTITUTED VALUES
C               *KRQL*    -  NUMBER OF REQUESTED ELEMENTS
C               *KRQ*     -  LIST OF REQUESTED TABLE B REFERENCE
C               *RQV*     -  LIST OF VALUES SIGNIFYING REQUESTED ELEMENT
C                            (SAY PRESSURE  AT 50000 PA)
C        OUTPUT:
C               *KERR*    -  RETURNED ERROR CODE
C
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
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
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
#     include "bcomreq.F"
#     include "bcomdefc.F"
C
      CHARACTER*4 CUSER,CECMWF
      DIMENSION KRQ(*),KREQ(*)
#ifndef R_4
      REAL*8 RQV(*)
      REAL*8 RQVAL
#else
      REAL   RQV(*)
      REAL   RQVAL
#endif
C
C     ------------------------------------------------------------------
C*          1. MOVE REQUESTED ELEMENTS INTO COMMON BLOCK /COMREQ/.
C              ---------------------------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
      CUSER='USER'
C
      NRQL=KRQL
      NREQ(1)=KREQ(1)
      NREQ(2)=KREQ(2)
C
      DO 101 I=1,KRQL
      NRQ(I)=KRQ(I)
      RQVAL(I)=RQV(I)
 101  CONTINUE
C
      RETURN
      END
