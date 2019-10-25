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

      subroutine swap_bytes4(r1,r2)
c     ---------------------------------
      implicit none

      real*4 r1, r2
      real*4 r14
      real*4 r24
      integer*4 kint14,kint24

      equivalence(r14,kint14)
      equivalence(r24,kint24)



c Reverse order of bytes

      r14=r1

      call mvbits(kint14, 24, 8, kint24, 0)
      call mvbits(kint14, 16, 8, kint24, 8)
      call mvbits(kint14,  8, 8, kint24,16)
      call mvbits(kint14,  0, 8, kint24,24)
      r2=r24

      return
      end
