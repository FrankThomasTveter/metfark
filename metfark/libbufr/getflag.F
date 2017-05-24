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

      SUBROUTINE GETFLAG(KREF,KCODE,KFLAG,YFLAG,KERR)
C
C**** *GETFLAG*
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
      CHARACTER*64  CTEXT
      CHARACTER*64  CTABBEN
      CHARACTER*24  CTABBU
      CHARACTER*64  YFLAG(32)
C
C
C     ------------------------------------------------------------------
C*          1.   SET INITIAL CONSTANTS AND POINTERS
C                ----------------------------------
 100  CONTINUE
C
      J=0
      do i=1,32
      YFLAG(i)=' '
      end do
C
      do i=1,JCTAB
      if(nref(i).eq.kref) go to 200
      end do
      print*,'Could not find ',kref
      ierr=2
      return      
c
 200  continue
C
      ictab=i
C
C     Find data width for kref
C
      do i=1,JTAB
      if(NTABBTR(I).eq.kref) go to 201
      end do

      print*,'Could not find ',kref
      ierr=2
      return
 201  continue
c
      ibtab=i
c
      iwidth=NTABBDW(ibtab)
c
      if(KCODE.eq.NMASK(iwidth)) then
         KFLAG=1
         YFLAG(1)='MISSING VALUE'         
         return
      else
         ipt=nstart(ictab)
         iptc=nstartc(ipt)
         KFLAG=0
         do i=ipt,ipt+nlen(ictab)-1
            ibitnumber=ncodnum(i)
c           test bit 
            ipos=iwidth-ibitnumber
            if(btest(KCODE,ipos)) then
               ic=iptc+ibitnumber-1
               KFLAG=KFLAG+1
               YFLAG(KFLAG)=ctext(ic)
            end if
         end do
      end if

      return
      END
