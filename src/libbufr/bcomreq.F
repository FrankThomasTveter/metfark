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

C
      COMMON /BCOMREQ/ RQVAL(JELEM),NREQ(2),NRQ(JELEM),NRQL
C
C             *NREQ*    -  FLAG
C                          BIT NUMBER     MEANING
C
C                              1        - 0 NO BIT MAP DELIVERED TO USER
C                                         1    BIT MAP DELIVERED TO USER
C                              2        - 0 NO PARTIAL EXPANSION
C                                         1    PARTIAL EXPANSION
C                              3        - 0 NO Q/C REQUIRED
C                                       - 1    Q/C REQUIRED
C                              4        - 0 NO STATISTICS REQUIRED
C                                       - 1    STATISTICS
C                              5        - 0 NO DIFFRENCE STATISTICS
C                                       - 1    DIFFERENCE STATISTICS
C                              6        - 0 NO SUBSTITUTED VALUES
C                                       - 1    SUBSTITUTED VALUES
C             *NRQL*    -  NUMBER OF REQUESTED ELEMENTS
C             *NRQ*     -  LIST OF REQUESTED TABLE B REFERENCE
C             *RQVAL*   -  LIST OF VALUES SIGNIFYING REQUESTED ELEMENT
C                          (SAY PRESSURE  AT 50000 PA)
C

