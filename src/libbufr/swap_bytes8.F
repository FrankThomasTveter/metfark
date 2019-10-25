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

      subroutine swap_bytes8(r1,r2)
c     ---------------------------------
      implicit none

      real*8 r1, r2
      real*8 r18
      real*8 r28
      integer*8 kint18,kint28

      equivalence(r18,kint18)
      equivalence(r28,kint28)



c Reverse order of bytes

      r18=r1

      call mvbits(kint18, 56, 8, kint28, 0)
      call mvbits(kint18, 48, 8, kint28, 8)
      call mvbits(kint18, 40, 8, kint28,16)
      call mvbits(kint18, 32, 8, kint28,24)
      call mvbits(kint18, 24, 8, kint28,32)
      call mvbits(kint18, 16, 8, kint28,40)
      call mvbits(kint18,  8, 8, kint28,48)
      call mvbits(kint18,  0, 8, kint28,56)


      r2=r28

      return
      end
