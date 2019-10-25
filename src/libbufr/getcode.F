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

      SUBROUTINE GETCODE(KREF,KCODE,YCODE,KERR)
C
C**** *GETCODE*
C
C
C     PURPOSE.
C     --------
C
C          The main purpose of the subroutine is to get text 
C          associated with code number 
C
C**   INTERFACE.
C     ----------
C          NONE.
C
C
C
C
C     *METHOD.
C      -------
C          NONE.
C
C
C
C     EXTERNALS.
C     ----------
C          NONE.
C
C
C
C
C     REFERENCE.
C     ----------
C
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC       *ECMWF*       JANUARY 2008.
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
#     include "parameter.F"
#     include "bcomwork.F"
#     include "bcomtab.F"
#     include "bcomtabc.F"
#     include "bcomct.F"
#     include "bcomctc.F"
#     include "bcomroot.F"
C#     include "bcomtabload.F"
C#     include "bcomtabloadc.F"


C
      CHARACTER*64 CTEXT
      CHARACTER*64 CTABBEN
      CHARACTER*24 CTABBU
      CHARACTER*256 YCODE
C
C
C     ------------------------------------------------------------------
C*          1.   SET INITIAL CONSTANTS AND POINTERS
C                ----------------------------------
 100  CONTINUE
C
      kerr=0
C
      ycode=' '
      do i=1,JCTAB
      if(nref(i).eq.kref) go to 200
      end do
      ycode(1:)='CODE ENTRY NOT KNOWN'
      return      
c
 200  continue
      ipt=nstart(i)
      do i=ipt,ipt+nlen(i)-1
       if(ncodnum(i).eq.KCODE) then
         iptc=nstartc(i)
         ii=1
         iptc1=nlenc(i)+iptc-1
         do j=iptc,iptc1
          do k=64,1,-1
          if(ctext(j)(k:k).ne.' ') then
           iz=k
           go to 201
          end if
          end do
 201      ycode(ii:ii+iz-1)=ctext(j)(1:iz)
          ii=ii+iz
         end do
         go to 300
       end if
      end do
      ycode(1:)='CODE ENTRY NOT KNOWN'
C
 300  continue
      return
      END
