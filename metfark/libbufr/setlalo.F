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

      SUBROUTINE SETLALO(K,KLA,KLO,KERR)
C
C     RETURN INDICES FOR LATITUDE AND LONGITUDE
C
      KERR=0
C
      IF(K.EQ.1) THEN
         KLA=9
         KLO=10
         RETURN
      ELSEIF(K.EQ.2) THEN
         KLA=9
         KLO=10
         RETURN
      ELSEIF(K.EQ.3) THEN
         KLA=9
         KLO=10
         RETURN
      ELSEIF(K.EQ.4) THEN
         KLA=9
         KLO=10
         RETURN
      ELSEIF(K.EQ.7) THEN
         KLA=9
         KLO=10
         RETURN
      ELSEIF(K.EQ.10) THEN
         KLA=12
         KLO=13
         RETURN
      ELSEIF(K.EQ.9) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.11) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.13) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.19) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.21) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.22) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.23) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.51) THEN
         KLA=8
         KLO=9
         RETURN
      ELSEIF(K.EQ.53) THEN
         KLA=8
         KLO=9
         RETURN
      ELSEIF(K.EQ.54) THEN
         KLA=22
         KLO=23
         RETURN
      ELSEIF(K.EQ.55) THEN
         KLA=22
         KLO=23
         RETURN
       ELSEIF(K.EQ.61) THEN
         KLA=9
         KLO=10
         RETURN
       ELSEIF(K.EQ.62) THEN
         KLA=9
         KLO=10
         RETURN
       ELSEIF(K.EQ.63) THEN
         KLA=9
         KLO=10
         RETURN
       ELSEIF(K.EQ.71) THEN
         KLA=9
         KLO=10
         RETURN
       ELSEIF(K.EQ.72) THEN
         KLA=9
         KLO=10
         RETURN
       ELSEIF(K.EQ.73) THEN
         KLA=9
         KLO=10
         RETURN
       ELSEIF(K.EQ.82) THEN
         KLA=9
         KLO=10
         RETURN
       ELSEIF(K.EQ.83) THEN
         KLA=9
         KLO=10
         RETURN
       ELSEIF(K.EQ.84) THEN
         KLA=9
         KLO=10
         RETURN
       ELSEIF(K.EQ.85) THEN
         KLA=9
         KLO=10
         RETURN
       ELSEIF(K.EQ.86) THEN
         KLA=9
         KLO=10
         RETURN
       ELSEIF(K.EQ.87) THEN
         KLA=12
         KLO=13
         RETURN
       ELSEIF(K.EQ.88) THEN
         KLA=9
         KLO=10
         RETURN
      ELSEIF(K.EQ.89) THEN
         KLA=12
         KLO=13
         RETURN
      ELSEIF(K.EQ.91) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.92) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.95) THEN
         KLA=3 
         KLO=4
         RETURN
      ELSEIF(K.EQ.96) THEN
         KLA=9
         KLO=10
         RETURN
      ELSEIF(K.EQ.97) THEN
         KLA=9
         KLO=10
         RETURN
      ELSEIF(K.EQ.101) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.102) THEN
         KLA=11
         KLO=12
         RETURN
      ELSEIF(K.EQ.103) THEN
         KLA=11
         KLO=12
         RETURN
      ELSEIF(K.EQ.104) THEN
         KLA=15
         KLO=16
         RETURN
      ELSEIF(K.EQ.106) THEN
         KLA=11
         KLO=12
         RETURN
      ELSEIF(K.EQ.121) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.122) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.123) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.126) THEN
         KLA=9
         KLO=10
         RETURN
      ELSEIF(K.EQ.127) THEN
         KLA=11
         KLO=12
         RETURN
      ELSEIF(K.EQ.129) THEN
         KLA=11
         KLO=12
         RETURN
      ELSEIF(K.EQ.130) THEN
         KLA=11
         KLO=12
         RETURN
      ELSEIF(K.EQ.131) THEN
         KLA=10
         KLO=11
         RETURN
      ELSEIF(K.EQ.132) THEN
         KLA=7
         KLO=8
         RETURN
      ELSEIF(K.EQ.133) THEN
         KLA=7
         KLO=8
         RETURN
      ELSEIF(K.EQ.136) THEN
         KLA=14
         KLO=15
         RETURN
      ELSEIF(K.EQ.137) THEN
         KLA=16
         KLO=17
         RETURN
      ELSEIF(K.EQ.142) THEN
         KLA=8
         KLO=9
         RETURN
      ELSEIF(K.EQ.144) THEN
         KLA=8
         KLO=9
         RETURN
      ELSEIF(K.EQ.145) THEN
         KLA=15
         KLO=16
         RETURN
      ELSEIF(K.EQ.164) THEN
         KLA=6
         KLO=7
         RETURN
      ELSEIF(K.EQ.255) THEN
         KLA=8
         KLO=9
         RETURN
      ELSE
         PRINT*,'POINTERS FOR ',K,' SYBTYPE NOT DEFINED.'
         KERR=1
         KLA=0
         KLO=0
         RETURN
      END IF
C
      RETURN
      END
