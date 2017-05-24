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
      SUBROUTINE CHECK_S4S(KELEM,KSEC3,KSEC4,KERR)
C
C**** *CHECK_S4S*
C
C
C     PURPOSE.
C     --------
C          CHECK SECTION 4 SIZE
C
C
C**   INTERFACE.
C     ----------
C
C
C     METHOD.
C     -------
C
C           NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       JUNE 2009
C
C
C     MODIFICATIONS.
C     --------------

      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)

#     include "parameter.F"
#     include "bcomoff.F"
#     include "bcomwt.F"

      DIMENSION KSEC3(*),KSEC4(*)
      DIMENSION IMASK(8)

    
      DATA IMASK/1,2,4,8,16,32,64,128/

c        Check if data are compressed
         IB=0
         IF(IAND(KSEC3(4),IMASK(7)).NE.0) IB=1
         IF(IB.EQ.0) THEN
C         Uncompressed
            IF(OMULTI) then
              isum=0
              IF(NSUBSET.EQ.KSEC3(3)) THEN
              DO J=1,KSEC3(3)
              DO I=1,NSIZEG(J)
              IJ=I+(J-1)*KELEM 
              isum=isum+NWTDWG(IJ)
              END DO
              END DO
              ibytes=isum/8
              ibit=isum-ibytes*8
              if(ibit.ne.0) then
C                padding to byte
                 ibytes=ibytes+1
                 if(mod(ibytes,2).ne.0) then
                    ibytes=ibytes+1
                 end if
                 ibytes=ibytes+4
              else
                if(mod(ibytes,2).ne.0) then
                    ibytes=ibytes+1
                 end if
                 ibytes=ibytes+4
              end if
              if((ksec4(1)-ibytes).gt.1) then
                  print*,'Error in section 4 size'
                  print*,'ksec4(1)=',ksec4(1)
                  print*,'Actual size=',ibytes
                  KERR=56
                  return
              end if
              END IF
            ELSE
              isum=0
              do I=1,NSIZE(1)
              isum=isum+NWTDW(I)
              END DO
              isum=isum*ksec3(3)
              ibytes=isum/8
              ibit=isum-ibytes*8
              if(ibit.ne.0) then
C                padding to byte
                 ibytes=ibytes+1
                 if(mod(ibytes,2).ne.0) then
                    ibytes=ibytes+1
                 end if
                 ibytes=ibytes+4
              else
                if(mod(ibytes,2).ne.0) then
                    ibytes=ibytes+1
                 end if
                 ibytes=ibytes+4
              end if
              if((ksec4(1)-ibytes).gt.1) then
               KERR=56
               WRITE(KNTN,*)  'ERROR IN BU_CHECK_S4S: '
               CALL BUERR(KERR)
              WRITE(KNTN,*)  'KSEC4(1)=',ksec4(1),' ACTUAL SIZE=',ibytes
               return
              end if
            END IF
         ELSE
C           Compressed
            isum=0
            do I=1,NSIZE(1)
            isum=isum+NWTIWS(I)
            END DO
            ibytes=isum/8
            ibit=isum-ibytes*8
            if(ibit.ne.0) then
C              padding to byte
                ibytes=ibytes+1
                if(mod(ibytes,2).ne.0) then
                    ibytes=ibytes+1
                 end if
               ibytes=ibytes+4
            else
               if(mod(ibytes,2).ne.0) then
                  ibytes=ibytes+1
                end if
               ibytes=ibytes+4
            end if
            if((ksec4(1)-ibytes).gt.1) then
               KERR=56
               WRITE(KNTN,*)  'ERROR IN BU_CHECK_S4S: '
               CALL BUERR(KERR)
              WRITE(KNTN,*)  'KSEC4(1)=',KSEC4(1),' ACTUAL SIZE=',ibytes
               return
            end if
         END IF
      END
