C     Library: ncll $Id: mncll.F,v 1.5 2010-05-13 10:26:26 franktt Exp $
C     
      SUBROUTINE MNCLL(UNITI,IRC)
C     
C     ***********************************************************************
C     +                                                                     *
C     +  UNITI = UNIT NUMBER FOR INPUT FILE                                 *
C     +  IRC=ERROR RETURN CODE (0=OK)                                       *
C     +                                                                     *
C     +                                                                     *
C     VERSION                      : 01/01/00                               *
C     +                                                                     *
C     WRITTEN/MODIFIED BY:                                                  *
C     --------------------------------------------------------------------- *
C     |    NAME      |   DATE   |                 REASON                  | *
C     --------------------------------------------------------------------- *
C     | F. TVETER    | 01/01/00 | NEW                                     | *
C     |              |          |                                         | *
C     --------------------------------------------------------------------- *
C     ***********************************************************************
C     
      IMPLICIT NONE
      SAVE
C     
C     INTERFACE VARIABLES
C     
      INTEGER  UNITI
      INTEGER IRC
C     
      integer nrpred, maxpred
      REAL, allocatable ::   PRED(:)
C     
      CHARACTER*12 MYNAME
      DATA MYNAME /'MNCLL'/
C     
      INTEGER  IKODE,OKODE
      LOGICAL  OK,LFLDAT(100),BDEB,ACTIVE,FIRSTW,found
      DATA BDEB   /.FALSE./
      DATA ACTIVE /.FALSE./
      DATA FIRSTW /.TRUE./
C     
      INTEGER  KODE,lena,lenb,lend,lenh,lenv,lenvi,lens,lenm,length
      integer leno,clen,leng,plen
      external length
      real sindeg, cosdeg, asindeg, acosdeg
      external sindeg,cosdeg,asindeg, acosdeg
      real wg(4)
C     
      INTEGER  LINE
      INTEGER  NRHDR
      PARAMETER (NRHDR=100)
      CHARACTER*250 HDR250(NRHDR),DAT250, INF250,buff250,buffx250
      CHARACTER*250 FILENM,NUKEHEAD
      EXTERNAL NUKEHEAD
      INTEGER  INTOUT
      LOGICAL  ENDOFF
C     
      integer locX(4), locY(4)
      integer dpX(2), dpY(2)    ! distance between points
      integer drX(2), drY(2)    ! dimension limit
      integer ix,jx,iy,jy
      real bc,rc,tc,lc,xfc
      integer jbc,jrc,jtc,jlc
      real brc,rtc,tlc,tbc,lbc,lrc
      integer jbrc,jrtc,jtlc,jlbc
      logical changed,inside
C     
      integer :: ignore=0
      integer :: request=1
      integer :: latlon=2
      integer :: project=4
      integer :: dimlatlon=5
      integer :: unchanged=6
C     
      type inventory
      integer :: ncid
      character*250 :: in250
      integer :: leni
      integer :: nrdim
      integer :: nrvar
      integer :: nrgatt
      integer :: unlimdimid
      integer :: latid
      integer :: lonid
      integer :: projid
      integer :: loc
      character*250, pointer :: dim250(:)
      logical, pointer       :: use(:)
      integer, pointer       :: lim(:)
      integer, pointer       :: pos(:)
      integer, pointer       :: gdim(:)
      integer, pointer       :: lend(:)
      character*250, pointer :: var250(:)
      integer, pointer :: lenv(:)
      type(variable), pointer :: var(:)  => null()
      integer :: nrlatlon
      integer, pointer :: indlatlon(:)
      integer, pointer :: limlatlon(:)
      integer, pointer :: poslatlon(:)
      integer :: loclatlon
      integer :: len
      integer :: nrInterpolation
      type(attribute), pointer :: attributes(:)
      end type inventory

      type variable
      character*250 :: var250
      integer       :: type
      integer       :: nrdim
      integer       :: nratt
      real          :: scale
      integer       :: len
      integer       :: loc
      logical       :: status(10)
      logical       :: use
      integer       :: gvar
      integer :: nrlatlon, nrrest
      integer, pointer :: indlatlon(:), indrest(:), ind(:)
      integer, pointer :: limlatlon(:), limrest(:), lim(:)
      integer, pointer :: poslatlon(:), posrest(:), pos(:)
      character*250, pointer :: att250(:)
      character*1,pointer::  fc(:)
      integer*1,  pointer::  f1(:)
      integer*2,  pointer::  f2(:)
      integer*4,  pointer::  f4(:)
      real*4,     pointer::  fr(:)
      real*8,     pointer::  fd(:)
      integer  :: missatt;
      integer  :: projatt;
      integer :: loclatlon
      type(attribute), pointer :: attributes(:)
      end type variable

      type attribute
      character*250  :: att250
      integer :: type
      integer :: len
      character*1, pointer ::  ac(:)
      integer*1,  pointer::  a1(:)
      integer*2,  pointer::  a2(:)
      integer*4,  pointer::  a4(:)
      real*4,     pointer::  ar(:)
      real*8,     pointer::  ad(:)
      end type attribute
C     
      type(inventory), pointer :: f(:)
C     
      character*250 out250, val250, att250
      integer passes, errors
      integer nrdim,ii,jj,kk,ll,ss,pp,nrwgt,len,varid,dd
      logical binterpolate

C--------------------------------------------direction should be a vector of length 2...

      integer dir,indx
      real dist, newdist, griddist, p, r, x, maxdist
      integer maxss,nrss,ssv,ssp
      parameter(maxss=2,ssv=1,ssp=2) ! projection/variable file index
      integer loc,loclatlon,nr,type,cnt
      integer nrvar,maxvar
      parameter (maxvar=100)
      character*250 var250(maxvar)

      integer, allocatable :: start(:),pos(:),lim(:),ind(:),loclim(:)
      integer, allocatable :: locmap(:,:)
      real, allocatable :: wgmap(:,:)
      integer ncid
      character*250 match250
      integer nrgdim
      integer, allocatable :: gdim(:)
      integer, allocatable :: glim(:)
      logical, allocatable :: gunl(:)
      character*250, allocatable :: gdim250(:)
      character*250 grid250
      integer lengrid
C     
#     include "netcdf.inc"
C     
      INTEGER ret,CHUNKSIZEHINT
      logical proceed,idone,odone,bdone,bok
      real dlat1,dlon1,dlat2,dlon2,a,b,axb,cost,maxcost
      integer costii, costjj, buff
      real, allocatable :: lat(:),lon(:)

      character*1,allocatable::  fc(:)
      integer*1,  allocatable::  f1(:)
      integer*2,  allocatable::  f2(:)
      integer*4,  allocatable::  f4(:)
      real*4,     allocatable::  fr(:)
      real*8,     allocatable::  fd(:)
C     
      integer nrgvar,nrgvardim
      character*250, allocatable :: gvar250(:)
      integer, allocatable   ::  gvardim(:),gvarid(:)
      integer gvar
C     
      character*1::  fillc
      integer*1  ::  fill1
      integer*2  ::  fill2
      integer*4  ::  fill4
      real*4     ::  fillr
      real*8     ::  filld
C     
      IRC=0
C     
C     DEBUG SYSTEM
C     
C     IF (.NOT.ACTIVE) CALL DEBUG(MYNAME,BDEB,ACTIVE)
C     bdeb=.true.
C     bdeb=.FALSE.
C     
      errors=0
      passes=0
      maxdist=0.0D0
C     
      fillc=char(nf_fill_char)
      fill1=nf_fill_int1
      fill2=nf_fill_int2
      fill4=nf_fill_int
      fillr=nf_fill_real
      filld=nf_fill_double
C     
      allocate(f(maxss),stat=irc)
      if (irc.ne.0) then
         write(*,*)myname,'Unable to allocate F.',irc
         return
      end if
C     initialise file-structures
      nrss=0
      do ii=1,maxss
         f(ii)%nrdim=0
         f(ii)%nrvar=0
         f(ii)%nrgatt=0
         f(ii)%latid=0
         f(ii)%lonid=0
         f(ii)%projid=0
      end do
C     
      BDEB = .FALSE.
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.',IRC
C     
      DO II=1,NRHDR
         HDR250(II) = ''
         LFLDAT(II)=.FALSE.
      ENDDO

      HDR250(1)   = 'NCLL V1.0 [0]VFLR'
      HDR250(10)  = 'VARIABLE FILE (NETCDF)   [1]VFLR &'
      HDR250(15)  = 'VARIABLES                [*]VFLR'
      HDR250(30)  = 'PROJECTION FILE (NETCDF) [1]VFLR &'
      HDR250(50)  = 'OUTPUT FILE (NETCDF)     [1]VFLR &'
C     
C     NF_FILL_CHAR, NF_FILL_BYTE, NF_FILL_SHORT, NF_FILL_INT, NF_FILL_FLOAT, and NF_FILL_DOUBLE
C     
      WRITE(*,*) MYNAME,'----------------------------------------'
C     
C     READ DATA FROM INPUT FILE..............................
C     
      KODE=-1
      CALL NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,ENDOFF,IRC)
      IF (IRC.NE.0) THEN
         WRITE(*,*) MYNAME,'-1 Error return from NUKEM.',IRC
         RETURN
      END IF
      LINE=INTOUT
C     
      KODE=0
      nrvar=0
      lend=1
      DO WHILE (.NOT.ENDOFF)
C     
C     READ NEXT DATA LINE
C     
         CALL NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,ENDOFF,IRC)
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,' 0 Error return from NUKEM.',IRC
            RETURN
         END IF
         LINE=INTOUT
         lend=length(dat250,250,lend)
c     WRITE(*,*) MYNAME,'line=',line
C     
         IF (BDEB) WRITE(*,*) MYNAME,'Debug: Read header:',LINE
C     
C     CHECK WHAT LINE WE JUST READ
C     
         IF (LINE.EQ.1) THEN
         ELSEIF (LINE.EQ.10) THEN ! variable file
            nrss=max(nrss,ssv)
            f(ssv)%in250=dat250
            LFLDAT(LINE) = .TRUE.
         ELSEIF (LINE.EQ.15) THEN ! variables
            nrvar=min(nrvar+1,maxvar)
            var250(nrvar)=dat250
            LFLDAT(LINE) = .TRUE.
         ELSEIF (LINE.EQ.30) THEN ! projection file
            nrss=max(nrss,ssp)
            f(ssp)%in250=dat250
            LFLDAT(LINE) = .TRUE.
         ELSEIF (LINE.EQ.50) THEN ! output file
            out250=dat250
            LFLDAT(LINE) = .TRUE.
         ELSE IF (LINE.NE.0) THEN ! LINE.EQ.0 IMPLIES SOMETIMES EOF
            WRITE(*,*) MYNAME,
     &           'System error, line not implemented:',LINE
            IRC=999
            RETURN
         ENDIF
      ENDDO
C     
      KODE=1
      CALL NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,ENDOFF,IRC)
      IF (IRC.NE.0) THEN
         WRITE(*,*) MYNAME,'Error return from NUKEM.',IRC
         RETURN
      END IF
C     
      GOTO 202
 201  CONTINUE
      LEND=LENGTH(DAT250,250,1)
      CALL CHOP0(HDR250(LINE),250)
      LENH=LENGTH(HDR250(LINE),250,1)
      WRITE(*,*) MYNAME,'unable to read: '//
     &     DAT250(1:LEND)//
     &     ' (HDR='//HDR250(LINE)(1:LENH)//')'
      IRC=522
      RETURN
 202  CONTINUE
C     
      WRITE(*,*) MYNAME,'----------------------------------------'
C     
      grid250=""
      lengrid=0
      

C--------------------------------------------if projection file is missing, we create a rotated polar projection...


C     
C     should provide a list of variables that should be processed... 
C     ...(all if no list is available)
C     
      write(*,*)myname,'Making file inventories.'
C     =============================================
C     make dimension/variable file inventory
C     =============================================
C     
      do ss=1,nrss
         f(ss)%leni=length(f(ss)%in250,250,10)
         if (ss.eq.ssv) then
            write(*,*)myname,'Scanning Variable file:  ',
     &           f(ss)%in250(1:f(ss)%leni)
         else if (ss.eq.ssp) then
            write(*,*)myname,'Scanning Projection file:',
     &           f(ss)%in250(1:f(ss)%leni)
         else
            write(*,*)myname,'Unknown file type:',ss
            irc=999
            return
         end if
c     cinfile=trim(f(ss)%in250)
         proceed=.true.
C     -> open NETCDF file
         if (proceed) then
            chunksizehint= 1024*1024*1024
C     chunksizehint= 1024*1024*1024
            RET = NF__OPEN(f(ss)%in250(1:F(SS)%LENI),nf_nowrite,
     &           chunksizehint,f(ss)%ncid)
C     RET = NF_OPEN(IN250(1:F(SS)%LENI),nf_nowrite,NCID)
            if (ret .ne. NF_NOERR) then
               write(buff250,"(A)") 
     &              " Unable to open: "//f(ss)%in250(1:f(ss)%leni)
               call chop0(buff250,250)
               lenb=length(buff250,250,10)
               write(*,'(X,A10,X,A)') myname,
     &              buff250(1:lenb)
               errors=errors+1
               irc=780
               return
            else
               write(buff250,"(A)") 
     &              "  Opening: "//f(ss)%in250(1:f(ss)%leni)
               call chop0(buff250,250)
               lenb=length(buff250,250,10)
C     write(*,'(X,A10,X,A)') myname,
C     &              buff250(1:lenb)
               passes=passes+1
            endif
         end if
C     -> get number of dimension, variables, attributes, unlimited dimension id
         RET = NF_INQ(f(ss)%ncid, f(ss)%nrdim, f(ss)%nrvar, 
     &        f(ss)%nrgatt, f(ss)%unlimdimid)
         if (ret .ne. NF_NOERR) then
            write(*,*) myname,"ERROR from NF_INQ:",
     $           nf_strerror(ret),f(ss)%ncid
            irc=790
            return
         end if
C     allocate variables in file
         allocate(
     &        f(ss)%dim250(f(ss)%nrdim),
     &        f(ss)%lend(f(ss)%nrdim),
     &        f(ss)%use(f(ss)%nrdim),
     &        f(ss)%lim(f(ss)%nrdim),
     &        f(ss)%pos(f(ss)%nrdim),
     &        f(ss)%gdim(f(ss)%nrdim),
     &        f(ss)%var250(f(ss)%nrvar),
     &        f(ss)%lenv(f(ss)%nrvar),
     &        f(ss)%indlatlon(f(ss)%nrdim),
     &        f(ss)%limlatlon(f(ss)%nrdim),
     &        f(ss)%attributes(f(ss)%nrgatt),
     &        stat=irc)
         if (irc.ne.0) then
            write(*,*) myname,'Unable to allocate F-vars.',irc
            return
         end if
C     
         do ii=1,f(ss)%nrdim
            f(ss)%gdim(ii)=0    ! globale dimension id
            f(ss)%use(ii)=.false.
         end do
C     
C     -> store dimension names
         do dd=1,f(ss)%nrdim
            RET = NF_INQ_DIM(f(ss)%NCID, DD, 
     &           f(ss)%dim250(dd), 
     &           f(ss)%lim(dd))
            if (ret .ne. NF_NOERR) then
               write(*,*) myname,"ERROR from NF_INQ_DIM:",
     $              nf_strerror(ret)
               irc=801
               return
            end if
            call chop0(f(ss)%dim250(dd),250)
            f(ss)%lend(dd)=length(f(ss)%dim250(dd),250,10)
            write(buff250,'(X,A20,5X,X,I5)') 
     &           f(ss)%dim250(dd)(1:f(ss)%lend(dd)),
     &           f(ss)%lim(dd)
            call chop0(buff250,250)
            lenb=length(buff250,250,10)
C     write(*,'(10X,A)') buff250(1:lenb)
         end do

C     -> store global attributes
         do ii=1,f(ss)%nrgatt
            RET=NF_INQ_ATTNAME(f(ss)%NCID,NF_GLOBAL,ii,
     &           f(ss)%attributes(ii)%att250)
            if (ret .ne. NF_NOERR) cycle
            RET=NF_INQ_ATT(f(ss)%NCID,NF_GLOBAL,
     &           f(ss)%attributes(ii)%att250,
     $           f(ss)%attributes(ii)%type,
     &           f(ss)%attributes(ii)%len)
            if (ret.ne.NF_NOERR) cycle
            if (f(ss)%attributes(ii)%type
     &           .eq.nf_char)then
               allocate(f(ss)%attributes(ii)%ac(
     &              f(ss)%attributes(ii)%len),stat=irc)
               ret = nf_get_att_text (f(ss)%ncid, nf_global, 
     &              f(ss)%attributes(ii)%att250,
     &              f(ss)%attributes(ii)%ac)
               if (ret.ne.NF_NOERR) cycle
            else if (f(ss)%attributes(ii)%type
     &              .eq.nf_int1)then
               allocate(f(ss)%attributes(ii)%a1(
     &              f(ss)%attributes(ii)%len),stat=irc)
               ret = nf_get_att_int1 (f(ss)%ncid, nf_global, 
     &              f(ss)%attributes(ii)%att250,
     &              f(ss)%attributes(ii)%a1)
               if (ret.ne.NF_NOERR) cycle
            elseif (f(ss)%attributes(ii)%type
     &              .eq.nf_int2)then
               allocate(f(ss)%attributes(ii)%a2(
     &              f(ss)%attributes(ii)%len),stat=irc)
               ret = nf_get_att_int2 (f(ss)%ncid, nf_global, 
     &              f(ss)%attributes(ii)%att250,
     &              f(ss)%attributes(ii)%a2)
               if (ret.ne.NF_NOERR) cycle
            elseif (f(ss)%attributes(ii)%type
     &              .eq.nf_int)then
               allocate(f(ss)%attributes(ii)%a4(
     &              f(ss)%attributes(ii)%len),stat=irc)
               ret = nf_get_att_int (f(ss)%ncid, nf_global, 
     &              f(ss)%attributes(ii)%att250,
     &              f(ss)%attributes(ii)%a4)
               if (ret.ne.NF_NOERR) cycle
            elseif (f(ss)%attributes(ii)%type
     &              .eq.nf_real)then
               allocate(f(ss)%attributes(ii)%ar(
     &              f(ss)%attributes(ii)%len),stat=irc)
               ret = nf_get_att_real (f(ss)%ncid, nf_global, 
     &              f(ss)%attributes(ii)%att250,
     &              f(ss)%attributes(ii)%ar)
               if (ret.ne.NF_NOERR) cycle
            elseif (f(ss)%attributes(ii)%type
     &              .eq.nf_double)then
               allocate(f(ss)%attributes(ii)%ad(
     &              f(ss)%attributes(ii)%len),stat=irc)
               ret = nf_get_att_double (f(ss)%ncid, nf_global, 
     &              f(ss)%attributes(ii)%att250,
     &              f(ss)%attributes(ii)%ad)
               if (ret.ne.NF_NOERR) cycle
            else
            end if 
            call chop0(f(ss)%attributes(ii)%att250,250)
            lena=length(f(ss)%attributes(ii)%att250,
     &           250,10)
            if (f(ss)%attributes(ii)%att250(1:lena)
     &           .eq."history") then
            end if
         end do

C     allocate variables in file
         allocate(f(ss)%var(f(ss)%nrvar),stat=irc)
         if (irc.ne.0) then
            write(*,*) myname,'Unable to allocate F-VAR.',irc
            return
         end if
C     initialise variables
         do ii=1,f(ss)%nrvar
            f(ss)%var(ii)%nrdim=0
            f(ss)%var(ii)%nratt=0
            f(ss)%var(ii)%scale=1.0
            f(ss)%var(ii)%nrlatlon=0
            f(ss)%var(ii)%nrrest=0
            f(ss)%var(ii)%missatt=0
            f(ss)%var(ii)%projatt=0
            do jj=1,10
               f(ss)%var(ii)%status(jj)=.false.
            end do
         end do
C     -> store variable names
         do varid=1,f(ss)%nrvar
            ret = NF_INQ_VARNDIMS (f(ss)%ncid, VARID, 
     &           f(ss)%var(varid)%nrdim);
            if (ret .ne. NF_NOERR) then
               write(*,*) myname,"ERROR from NF_INQ_VARNDIMS:",
     $              nf_strerror(ret)
               irc=802
               return
            end if
            allocate(f(ss)%var(varid)%ind(f(ss)%var(varid)%nrdim),
     &           f(ss)%var(varid)%lim(f(ss)%var(varid)%nrdim),
     &           f(ss)%var(varid)%pos(f(ss)%var(varid)%nrdim),
     &           f(ss)%var(varid)%indlatlon(f(ss)%var(varid)%nrdim),
     &           f(ss)%var(varid)%limlatlon(f(ss)%var(varid)%nrdim),
     &           f(ss)%var(varid)%poslatlon(f(ss)%var(varid)%nrdim),
     &           f(ss)%var(varid)%indrest(f(ss)%var(varid)%nrdim),
     &           f(ss)%var(varid)%limrest(f(ss)%var(varid)%nrdim),
     &           f(ss)%var(varid)%posrest(f(ss)%var(varid)%nrdim),
     &           stat=irc)
            if (irc.ne.0) then
               write(*,*) myname,'Unable to allocate F-VAR-vars.',irc
               return
            end if
            RET = NF_INQ_VAR(f(ss)%ncid, VARID, 
     &           f(ss)%var(varid)%var250, 
     &           f(ss)%var(varid)%type,
     &           f(ss)%var(varid)%nrdim,
     &           f(ss)%var(varid)%ind,
     &           f(ss)%var(varid)%nratt)
            if (ret .ne. NF_NOERR) then
               write(*,*) myname,"ERROR from NF_INQ_VAR:",
     $              nf_strerror(ret)
               irc=802
               return
            end if
            f(ss)%var250(varid)=f(ss)%var(varid)%var250
            do ii=1,f(ss)%var(varid)%nrdim
               f(ss)%var(varid)%lim(ii)=f(ss)%lim(
     &              f(ss)%var(varid)%ind(ii))
            end do
            call chop0(f(ss)%var(varid)%var250,250)

C     write(*,'(X,A,A20,100(X,A10))')
C     &           '   Found: ',f(ss)%var250(varid)(1:20),
C     &           (f(ss)%dim250(f(ss)%var(varid)%ind(ii))(1:10),
C     &           ii=1,f(ss)%var(varid)%nrdim)

            proceed=.true.
            if (lfldat(15)) then
               proceed=.false.
               do ii=1,nrvar
                  lenv=length(var250(ii),250,10)
                  lenvi=length(f(ssv)%var(varid)%var250,250,10)
                  if (var250(ii)(1:lenv).eq.
     *                 f(ssv)%var(varid)%var250(1:lenvi)) then
                     proceed=.true.
                  end if
               end do
            end if
            if (proceed)then
               f(ss)%var(varid)%status(request)=.true. ! field is requested
            end if
            f(ss)%var(varid)%scale=1.0D0
            allocate(f(ss)%var(varid)%attributes(
     &           f(ss)%var(varid)%nratt),stat=irc)
            if (irc.ne.0) then
               write(*,*) myname,'Unable to allocate F-atts.',irc
               return
            end if
            do ii=1,f(ss)%var(varid)%nratt
               RET=NF_INQ_ATTNAME(f(ss)%NCID,VARID,ii,
     &              f(ss)%var(varid)%attributes(ii)%att250)
               if (ret .ne. NF_NOERR) cycle
               RET=NF_INQ_ATT(f(ss)%NCID,VARID,
     &              f(ss)%var(varid)%attributes(ii)%att250,
     $              f(ss)%var(varid)%attributes(ii)%type,
     &              f(ss)%var(varid)%attributes(ii)%len)
               if (ret.ne.NF_NOERR) cycle
               if (f(ss)%var(varid)%attributes(ii)%type
     &              .eq.nf_char)then
                  clen=f(ss)%var(varid)%attributes(ii)%len
                  allocate(f(ss)%var(varid)%attributes(ii)%
     &                 ac(f(ss)%var(varid)%attributes(ii)%len),
     &                 stat=irc)
                  ret = nf_get_att_text (f(ss)%ncid, varid, 
     &                 f(ss)%var(varid)%attributes(ii)%att250,
     &                 f(ss)%var(varid)%attributes(ii)%ac)
               else if (f(ss)%var(varid)%attributes(ii)%type
     &                 .eq.nf_int1)then
                  allocate(f(ss)%var(varid)%attributes(ii)%a1(
     &                 f(ss)%var(varid)%attributes(ii)%len),stat=irc)
                  ret = nf_get_att_int1 (f(ss)%ncid, varid, 
     &                 f(ss)%var(varid)%attributes(ii)%att250,
     &                 f(ss)%var(varid)%attributes(ii)%a1)
               else if (f(ss)%var(varid)%attributes(ii)%type
     &                 .eq.nf_int2)then
                  allocate(f(ss)%var(varid)%attributes(ii)%a2(
     &                 f(ss)%var(varid)%attributes(ii)%len),stat=irc)
                  ret = nf_get_att_int2 (f(ss)%ncid, varid, 
     &                 f(ss)%var(varid)%attributes(ii)%att250,
     &                 f(ss)%var(varid)%attributes(ii)%a2)
               else if (f(ss)%var(varid)%attributes(ii)%type
     &                 .eq.nf_int)then
                  allocate(f(ss)%var(varid)%attributes(ii)%a4(
     &                 f(ss)%var(varid)%attributes(ii)%len),stat=irc)
                  ret = nf_get_att_int (f(ss)%ncid, varid, 
     &                 f(ss)%var(varid)%attributes(ii)%att250,
     &                 f(ss)%var(varid)%attributes(ii)%a4)
               else if (f(ss)%var(varid)%attributes(ii)%type
     &                 .eq.nf_real)then
                  allocate(f(ss)%var(varid)%attributes(ii)%ar(
     &                 f(ss)%var(varid)%attributes(ii)%len),stat=irc)
                  ret = nf_get_att_real (f(ss)%ncid, varid, 
     &                 f(ss)%var(varid)%attributes(ii)%att250,
     &                 f(ss)%var(varid)%attributes(ii)%ar)
               elseif (f(ss)%var(varid)%attributes(ii)%type
     &                 .eq.nf_double)then
                  allocate(f(ss)%var(varid)%attributes(ii)%ad(
     &                 f(ss)%var(varid)%attributes(ii)%len),stat=irc)
                  ret = nf_get_att_double (f(ss)%ncid, varid, 
     &                 f(ss)%var(varid)%attributes(ii)%att250,
     &                 f(ss)%var(varid)%attributes(ii)%ad)
               else
               end if 
               if (irc.ne.0) then
                  write(*,*) myname,'Unable to allocate F-VAR-atts',irc
                  return
               end if
               if (ret.ne.NF_NOERR) cycle
               call chop0(f(ss)%var(varid)%attributes(ii)%att250,250)
               lena=length(f(ss)%var(varid)%attributes(ii)%att250,
     &              250,10)
C     check if we have scale-factor
               if (f(ss)%var(varid)%attributes(ii)%att250(1:lena)
     &              .eq."scale_factor") then
                  if (f(ss)%var(varid)%attributes(ii)%type
     &                 .eq.nf_real)then
                     f(ss)%var(varid)%scale=
     &                    f(ss)%var(varid)%attributes(ii)%ar(1)
                  elseif (f(ss)%var(varid)%attributes(ii)%type
     &                    .eq.nf_double)then
                     f(ss)%var(varid)%scale=
     &                    f(ss)%var(varid)%attributes(ii)%ad(1)
                  end if
C     check if we have standard name...(i.e. "latitude" or "longitude")
               else if (f(ss)%var(varid)%attributes(ii)%att250(1:lena)
     &                 .eq."standard_name") then
                  if (f(ss)%var(varid)%attributes(ii)%type
     &                 .eq.nf_char)then
                     val250=""
                     do jj=1,min(250,f(ss)%var(varid)
     &                    %attributes(ii)%len)
                        val250(jj:jj)=f(ss)%var(varid)
     &                       %attributes(ii)%ac(jj)
                     end do
                     call chop0(val250,250)
                     lenv=length(val250,250,10)
C     write(*,*)myname,"Name:", val250(1:lenv)
                     if (lenv.eq.0) then
                        write(*,*)myname,'Empty standard_name found.'
                        irc=802
                        return
                     else if (val250(1:lenv).eq."latitude") then
                        f(ss)%var(varid)%status(latlon)=.true. ! latlon field
                        f(ss)%latid=varid
                     else if (val250(1:lenv).eq."longitude") then
                        f(ss)%var(varid)%status(latlon)=.true. ! latlon field
                        f(ss)%lonid=varid
                     end if
                  end if
               else if (f(ss)%var(varid)%attributes(ii)%att250(1:lena)
     &                 .eq."grid_mapping_name") then
                  f(ss)%projid=varid ! projection variable
                  f(ss)%var(varid)%status(project)=.true. ! this is a grid definition
                  if (ss.eq.ssp) then
                     grid250=""
                     do jj=1,min(250,f(ss)%var(varid)
     &                    %attributes(ii)%len)
                        grid250(JJ:JJ)=f(ss)%var(varid)
     &                       %attributes(ii)%ac(jj)
                     end do
                     call chop0(grid250,250)
                     lengrid=length(grid250,250,10)
                     write(*,*) myname,'Mapping to:',grid250(1:lengrid)
                  end if
C     check if we have missing values
               else if (f(ss)%var(varid)%attributes(ii)%att250(1:lena)
     &                 .eq."grid_mapping") then
                  f(ss)%var(varid)%projatt=ii ! projection variable
C     check if we have missing values
               else if (f(ss)%var(varid)%attributes(ii)%att250(1:lena)
     &                 .eq."missing_value") then
                  f(ss)%var(varid)%missatt=ii
               end if
            end do
            match250=getVar250(
     &           f(ss)%nrdim,
     &           f(ss)%dim250,
     &           f(ss)%var(varid)%var250,
     &           f(ss)%var(varid)%nrdim,
     &           f(ss)%var(varid)%ind)
            lenm=length(match250,250,10)
            buff250=match250
            call chop0(buff250,250)
            lenb=length(buff250,250,10)
C     write(*,'(10X,A10,5X,A)')
C     &           gettype(f(ss)%var(varid)%type),
C     &           buff250(1:lenb)
         end do
      end do
C     
C     =============================================
C     allocate and read lat/lon variables into memory (size may differ)
C     =============================================
      write(*,*)myname,'Reading lat-lon fields into memory.'
C     
      do ss=1,nrss
         if (f(ss)%latid .ne. 0 .and. f(ss)%lonid .ne.0) then
            do jj=1,2
               if (jj.eq.1) then
                  varid=f(ss)%latid
               else
                  varid=f(ss)%lonid
               end if
C     find length of lat/lon-grid
               clen=1
               do ii=1,f(ss)%var(varid)%nrdim
                  clen=clen*f(ss)%lim(f(ss)%var(varid)%ind(ii))
               end do
               f(ss)%var(varid)%len=clen
C     allocate and read from file
               allocate( f(ss)%var(varid)%fd(clen), 
     &              start( f(ss)%var(varid)%nrdim),
     &              stat=irc)
               if (irc.ne.0) then
                  write(*,*) myname,'Unable to allocate FD-LP.',irc,
     &                 f(ss)%var(varid)%nrdim
                  return
               end if
               do ii=1,f(ss)%var(varid)%nrdim
                  start(ii)=1
               end do
               if (f(ss)%var(varid)%type.eq.nf_int2) then
                  allocate( f(ss)%var(varid)%f2(clen), 
     &                 stat=irc)
                  if (irc.ne.0) then
                     write(*,*) myname,'Unable to allocate F2.',irc
                     return
                  end if
                  ret = nf_get_vara_int2(f(ss)%ncid,varid,
     &                 start,f(ss)%var(varid)%lim,
     &                 f(ss)%var(varid)%f2)
                  if (ret .ne. NF_NOERR) then
                     write(*,*) myname,"ERROR from NF_GET_VARA_INT2:",
     $                    nf_strerror(ret)
                     irc=812
                     return
                  end if
C     handle missing values gracefully...
                  fill2=nf_fill_int2
                  filld=nf_fill_double
                  if (f(ss)%var(varid)%missatt.gt.0) then
                     ii=f(ss)%var(varid)%missatt
                     if (f(ss)%var(varid)%attributes(ii)
     &                    %type.eq.nf_int2) then
                        fill2=f(ss)%var(varid)%attributes(ii)%a2(1)
                     end if
                  end if
                  do ii=1,clen
                     if (f(ss)%var(varid)%f2(ii).eq.fill2) then
                        f(ss)%var(varid)%fd(ii)=filld
                     else
                        f(ss)%var(varid)%fd(ii)=f(ss)%var(varid)%scale*
     &                       f(ss)%var(varid)%f2(ii)
                     end if
                  end do
                  deallocate(f(ss)%var(varid)%f2, 
     &                 stat=irc)
               else if (f(ss)%var(varid)%type.eq.nf_int) then
                  allocate( f(ss)%var(varid)%f4(clen), 
     &                 stat=irc)
                  if (irc.ne.0) then
                     write(*,*) myname,'Unable to allocate F4.',irc
                     return
                  end if
                  ret = nf_get_vara_int(f(ss)%ncid,varid,
     &                 start,f(ss)%var(varid)%lim,
     &                 f(ss)%var(varid)%f4)
                  if (ret .ne. NF_NOERR) then
                     write(*,*) myname,"ERROR from NF_GET_VARA_INT:",
     $                    nf_strerror(ret)
                     irc=812
                     return
                  end if
C     handle missing values gracefully...
                  fill4=nf_fill_int
                  filld=nf_fill_double
                  if (f(ss)%var(varid)%missatt.gt.0) then
                     ii=f(ss)%var(varid)%missatt
                     if (f(ss)%var(varid)%attributes(ii)
     &                    %type.eq.nf_int) then
                        fill4=f(ss)%var(varid)%attributes(ii)%a4(1)
                     end if
                  end if
                  do ii=1,clen
                     if (f(ss)%var(varid)%f4(ii).eq.fill4) then
                        f(ss)%var(varid)%fd(ii)=filld
                     else
                        f(ss)%var(varid)%fd(ii)=f(ss)%var(varid)%scale*
     &                       f(ss)%var(varid)%f4(ii)
                     end if
                  end do
                  deallocate(f(ss)%var(varid)%f2, 
     &                 stat=irc)
               else if (f(ss)%var(varid)%type.eq.nf_real) then
                  allocate( f(ss)%var(varid)%fr(clen), 
     &                 stat=irc)
                  if (irc.ne.0) then
                     write(*,*) myname,'Unable to allocate FR.',irc
                     return
                  end if
                  ret = nf_get_vara_real(f(ss)%ncid,varid,
     &                 start,f(ss)%var(varid)%lim,
     &                 f(ss)%var(varid)%fr)
                  if (ret .ne. NF_NOERR) then
                     write(*,*) myname,"ERROR from NF_GET_VARA_REAL:",
     $                    nf_strerror(ret)
                     irc=812
                     return
                  end if
C     handle missing values gracefully...
                  fillr=nf_fill_real
                  filld=nf_fill_double
                  if (f(ss)%var(varid)%missatt.gt.0) then
                     ii=f(ss)%var(varid)%missatt
                     if (f(ss)%var(varid)%attributes(ii)
     &                    %type.eq.nf_real) then
                        fillr=f(ss)%var(varid)%attributes(ii)%ar(1)
                     end if
                  end if
                  do ii=1,clen
                     if (f(ss)%var(varid)%fr(ii).eq.fillr) then
                        f(ss)%var(varid)%fd(ii)=filld
                     else
                        f(ss)%var(varid)%fd(ii)=f(ss)%var(varid)%scale*
     &                       f(ss)%var(varid)%fr(ii)
                     end if
                  end do
                  f(ss)%var(varid)%scale=1.0D0
                  deallocate(f(ss)%var(varid)%fr, 
     &                 stat=irc)
               else if (f(ss)%var(varid)%type.eq.nf_double) then
                  ret = nf_get_vara_double(f(ss)%ncid,varid,
     &                 start,f(ss)%var(varid)%lim,
     &                 f(ss)%var(varid)%fd)
                  if (ret .ne. NF_NOERR) then
                     write(*,*) myname,"ERROR from NF_GET_VARA_DOUBLE:",
     $                    nf_strerror(ret)
                     irc=812
                     return
                  end if
               else
                  write(*,*)myname,'LATLON values are not DECIMAL!',
     &                 f(ss)%var(varid)%type
                  irc=845
                  return
               end if
               deallocate(start,stat=irc)
            end do
         else
            if (ss.eq.ssv) then
               write(*,*) 'No lat/lon in Variable file.'
            else if (ss.eq.ssp) then
               write(*,*) 'No lat/lon in Projection file.'
            end if
            irc=945
            return
         end if
      end do
C     
C     =============================================
C     make common latlon dimension index
C     =============================================
C     
      do ss=1,nrss
         allocate(f(ss)%indlatlon(f(ss)%nrdim),
     &        f(ss)%limlatlon(f(ss)%nrdim),
     &        stat=irc)
         if (irc.ne.0) then
            write(*,*) myname,'Unable to allocate F-indlim.',irc
            return
         end if
         f(ss)%nrlatlon=0
         f(ss)%indlatlon=-1
         do jj=1,2
            if (jj.eq.1) then
               varid=f(ss)%latid
            else
               varid=f(ss)%lonid
            end if
            do kk=1,f(ss)%var(varid)%nrdim
               found =.false.
               do ll=1,f(ss)%nrlatlon
                  if (f(ss)%var(varid)%ind(kk).eq.
     &                 f(ss)%indlatlon(ll)) then
                     indx=ll
                     found=.true.
                  end if
               end do
               if (.not.found) then
                  f(ss)%nrlatlon=f(ss)%nrlatlon+1
                  f(ss)%indlatlon(f(ss)%nrlatlon)=
     &                 f(ss)%var(varid)%ind(kk) ! kk
                  f(ss)%limlatlon(f(ss)%nrlatlon)=
     &                 f(ss)%lim(f(ss)%var(varid)%ind(kk)) ! kk
               else
               end if
               if (ss.eq.ssp) then
                  f(ss)%use(f(ss)%var(varid)%ind(kk))=.true.
               end if
            end do
         end do
      end do
C     
C     identify latlon dimensions in other variables
C     
      do ss=1,nrss
         do varid=1,f(ss)%nrvar
            f(ss)%var(varid)%nrlatlon=0
            f(ss)%var(varid)%nrrest=0
            do ii=1,f(ss)%var(varid)%nrdim
               found=.false.
               do ll=1,f(ss)%nrlatlon
                  if (f(ss)%var(varid)%ind(ii).eq.
     &                 f(ss)%indlatlon(ll)) then
                     indx=ll 
                     found=.true.
                  end if
               end do
               if (found) then
                  f(ss)%var(varid)%nrlatlon=
     &                 f(ss)%var(varid)%nrlatlon+1
                  f(ss)%var(varid)%indlatlon(f(ss)
     &                 %var(varid)%nrlatlon)=
     &                 f(ss)%var(varid)%ind(ii)
                  f(ss)%var(varid)%limlatlon(f(ss)
     &                 %var(varid)%nrlatlon)=
     &                 f(ss)%lim(f(ss)%var(varid)%ind(ii))
               else
                  f(ss)%var(varid)%nrrest=
     &                 f(ss)%var(varid)%nrrest+1
                  f(ss)%var(varid)%indrest(f(ss)
     &                 %var(varid)%nrrest)=
     &                 f(ss)%var(varid)%ind(ii)
                  f(ss)%var(varid)%limrest(f(ss)
     &                 %var(varid)%nrrest)=
     &                 f(ss)%lim(f(ss)%var(varid)%ind(ii))
               end if
            end do
            f(ss)%var(varid)%status(dimlatlon)=
     &           (f(ss)%var(varid)%nrlatlon.eq.f(ss)%nrlatlon) ! are all latlon dimensions present?
            f(ss)%var(varid)%use=
     &           (ss.eq.ssv.and.f(ss)%var(varid)%status(request).and.
     &           f(ss)%var(varid)%status(dimlatlon)) .or. 
     &           (ss.eq.ssp.and.f(ss)%var(varid)%status(latlon)).or.
     &           (ss.eq.ssp.and.f(ss)%var(varid)%status(project))
            if (f(ss)%var(varid)%use) then
               lenv=length(f(ss)%var(varid)%var250,250,10)
C     write(*,*)myname,'Using:',
C     &              f(ss)%var(varid)%status(request),
C     &              f(ss)%var(varid)%status(dimlatlon),
C     &              f(ss)%var(varid)%status(latlon),
C     &              f(ss)%var(varid)%status(project),
C     &              f(ss)%var(varid)%var250(1:lenv)
            end if
            if (ss.eq.ssp.and.(f(ss)%var(varid)%status(project).or.
     &           f(ss)%var(varid)%status(latlon))) then
               f(ss)%var(varid)%status(unchanged)=.true.
               do kk=1,f(ss)%var(varid)%nrdim ! use all dimensions
                  f(ss)%use(f(ss)%var(varid)%ind(kk))=.true.
               end do
            else if (ss.eq.ssv.and. ! use non-latlon dimensions
     &              f(ss)%var(varid)%status(request).and.
     &              f(ss)%var(varid)%status(dimlatlon)) then
               do kk=1,f(ss)%var(varid)%nrrest
                  f(ss)%use(f(ss)%var(varid)%indrest(kk))=.true.
               end do
            end if
         end do
      end do
C
C     make index for dimensions used in lat-lon interpolation
C
      allocate(lat(0:f(ssv)%nrlatlon), lon(0:f(ssv)%nrlatlon),
     &     stat=irc)
      if (irc.ne.0) then
         write(*,*)myname,'Unable to allocate lat/lon:',
     &        f(ssv)%nrlatlon
         return
      end if
C-----------------------------------------check if lat-lon dimensions are interpolatable
      if (f(ssv)%nrlatlon .ge. 2) then
         do ii=1,f(ssv)%nrdim
            f(ssv)%pos(ii)=1
         end do
C     C     longitude
         call pushPos(f(ssv)%nrdim,
     &        f(ssv)%pos,
     &        f(ssv)%var(f(ssv)%lonid)%nrlatlon,
     &        f(ssv)%var(f(ssv)%lonid)%indlatlon,
     &        f(ssv)%var(f(ssv)%lonid)%poslatlon)
         call pushLoc(
     &        f(ssv)%var(f(ssv)%lonid)%nrlatlon,
     &        f(ssv)%var(f(ssv)%lonid)%limlatlon,
     &        f(ssv)%var(f(ssv)%lonid)%poslatlon,
     &        f(ssv)%var(f(ssv)%lonid)%loc)
         lon(0)=f(ssv)%var(f(ssv)%lonid)%fd(
     &        f(ssv)%var(f(ssv)%lonid)%loc)
C     latitude
         call pushPos(f(ssv)%nrdim,
     &        f(ssv)%pos,
     &        f(ssv)%var(f(ssv)%latid)%nrlatlon,
     &        f(ssv)%var(f(ssv)%latid)%indlatlon,
     &        f(ssv)%var(f(ssv)%latid)%poslatlon)
         call pushLoc(
     &        f(ssv)%var(f(ssv)%latid)%nrlatlon,
     &        f(ssv)%var(f(ssv)%latid)%limlatlon,
     &        f(ssv)%var(f(ssv)%latid)%poslatlon,
     &        f(ssv)%var(f(ssv)%latid)%loc)
         lat(0)=f(ssv)%var(f(ssv)%latid)%fd(
     &        f(ssv)%var(f(ssv)%latid)%loc)
         
         do ii=1,f(ssv)%nrlatlon
C     longitude
            f(ssv)%pos(f(ssv)%indlatlon(ii))=
     &           f(ssv)%pos(f(ssv)%indlatlon(ii))+1
            call pushPos(f(ssv)%nrdim,
     &           f(ssv)%pos,
     &           f(ssv)%var(f(ssv)%lonid)%nrlatlon,
     &           f(ssv)%var(f(ssv)%lonid)%indlatlon,
     &           f(ssv)%var(f(ssv)%lonid)%poslatlon)
            call pushLoc(
     &           f(ssv)%var(f(ssv)%lonid)%nrlatlon,
     &           f(ssv)%var(f(ssv)%lonid)%limlatlon,
     &           f(ssv)%var(f(ssv)%lonid)%poslatlon,
     &           f(ssv)%var(f(ssv)%lonid)%loc)
            lon(ii)=f(ssv)%var(f(ssv)%lonid)%fd(
     &           f(ssv)%var(f(ssv)%lonid)%loc)
C     latitude
            call pushPos(f(ssv)%nrdim,
     &           f(ssv)%pos,
     &           f(ssv)%var(f(ssv)%latid)%nrlatlon,
     &           f(ssv)%var(f(ssv)%latid)%indlatlon,
     &           f(ssv)%var(f(ssv)%latid)%poslatlon)
            call pushLoc(
     &           f(ssv)%var(f(ssv)%latid)%nrlatlon,
     &           f(ssv)%var(f(ssv)%latid)%limlatlon,
     &           f(ssv)%var(f(ssv)%latid)%poslatlon,
     &           f(ssv)%var(f(ssv)%latid)%loc)
            lat(ii)=f(ssv)%var(f(ssv)%latid)%fd(
     &           f(ssv)%var(f(ssv)%latid)%loc)
            f(ssv)%pos(f(ssv)%indlatlon(ii))=
     &           f(ssv)%pos(f(ssv)%indlatlon(ii))-1
         end do
         maxcost=-1.0D0
         do ii=1,f(ssv)%nrlatlon-1
            dlat1 = 90.0D0-acosdeg(
     &           cosdeg(90.0D0-lat(ii))*cosdeg(lat(0)) + 
     &           sindeg(90.0D0-lat(ii))*sindeg(lat(0)) * 
     &           cosdeg(180.0D0-(lon(ii)-lon(0))))
            dlon1=asindeg(sindeg(180.0D0-(lon(ii)-lon(0)))*
     &           sindeg(90-lat(ii))/sindeg(90.0D0-dlat1))
            do jj=ii+1,f(ssv)%nrlatlon
               dlat2 = 90.0D0-acosdeg(
     &              cosdeg(90.0D0-lat(jj))*cosdeg(lat(0)) + 
     &              sindeg(90.0D0-lat(jj))*sindeg(lat(0)) * 
     &              cosdeg(180.0D0-(lon(jj)-lon(0))))
               dlon2=asindeg(sindeg(180.0D0-(lon(jj)-lon(0)))*
     &              sindeg(90-lat(jj))/sindeg(90.0D0-dlat2))
               a=dsqrt(dlat1*dlat1+dlon1*dlon1)
               b=dsqrt(dlat2*dlat2+dlon2*dlon2)
               axb=dlon1*dlat2-dlat1*dlon2
C     expect more than 1 meter resolution.... (~ 1.0D-5 degrees)
               cost=1.0D0
C     cost=cost*2*(|a|*|b|)/(|a|**2+|b|**2) ! equal length
               cost=cost*2.0D0*(a*b)/max(1.0D-10,a*a + b*b)
C     cost=cost*(|axb|/(|a|*|b|)            ! right angles
               cost=cost*(axb)/max(1.0D-10,a*b)
               if (cost.gt.maxcost) then
                  maxcost=cost
                  costii=ii
                  costjj=jj
               end if
            end do
         end do
         if (maxcost.gt.0.5D0) then
C     rearrange index so that they are last....
            if (f(ssv)%indlatlon(costii).ne.
     &           f(ssv)%indlatlon(f(ssv)%nrlatlon-1)) then
               buff=f(ssv)%indlatlon(f(ssv)%nrlatlon-1)
               f(ssv)%indlatlon(f(ssv)%nrlatlon-1)=
     &              f(ssv)%indlatlon(costii)
               f(ssv)%indlatlon(costii)=buff
               if (costjj.eq.f(ssv)%nrlatlon-1) then
                  costjj=costii
               end if
            end if
            if (f(ssv)%indlatlon(costjj).ne.
     &           f(ssv)%indlatlon(f(ssv)%nrlatlon)) then
               buff=f(ssv)%indlatlon(f(ssv)%nrlatlon)
               f(ssv)%indlatlon(f(ssv)%nrlatlon)=
     &              f(ssv)%indlatlon(costjj)
               f(ssv)%indlatlon(costjj)=buff
            end if
            write(*,*)myname,'LATLON:DIMENSIONS:',
     &           f(ssv)%indlatlon,f(ssv)%nrlatlon
            write(*,*)myname,'LATLON:LIMITS:',
     &           f(ssv)%lim
            f(ssv)%nrInterpolation=2
         else
            f(ssv)%nrInterpolation=0
         end if
      else
         f(ssv)%nrInterpolation=0
      end if
      deallocate(lat,lon)
C
C     allocate position vectors...
C
      do ss=1,nrss
         allocate(f(ss)%poslatlon(f(ss)%nrdim),stat=irc)
         if (irc.ne.0) then
            write(*,*) myname,'Unable to allocate F-POSLL.',irc
            return
         end if
C     find length of latlon-grid
         clen=1
         do ii=1,f(ss)%nrlatlon
            clen=clen*f(ss)%limlatlon(ii)
         end do
         f(ss)%len=clen
      end do
      
C----------------------------------------------------------the lat lon projection grid can be maximum 2D...
C     ...while the variable grid may have extra variables.... (i.e. different latlon for each height level etc)...
C     
      allocate(locmap(4,f(ssp)%len),wgmap(4,f(ssp)%len),stat=irc)
      if (irc.ne.0) then
         write(*,*)myname,'Unable to allocate MAP.',irc
         return
      end if

C     
      write(*,*)myname,'Making lat-lon map.'
C     
C     =============================================
C     loop over all projection dimensions, and make lat-lon map...
C     ...map new(II,JJ)old(KK...) to old(II,JJ,KK....) lower left corner...
C     =============================================
C     
      do ss=1,nrss
         do ii=1,f(ss)%nrlatlon
            f(ss)%poslatlon(ii)=1 ! initialise loop vector
         end do
      end do
C     
      odone=.false.
      do while (.not.odone)     ! loop over projection
         
         call pullPos(f(ssp)%nrdim,
     &        f(ssp)%pos,
     &        f(ssp)%nrlatlon,
     &        f(ssp)%indlatlon,
     &        f(ssp)%poslatlon)

         write(*,*)myname,'SSP-Pos:',f(ssp)%pos

         call pushPos(f(ssp)%nrdim,
     &        f(ssp)%pos,
     &        f(ssp)%var(f(ssp)%latid)%nrlatlon,
     &        f(ssp)%var(f(ssp)%latid)%indlatlon,
     &        f(ssp)%var(f(ssp)%latid)%poslatlon)
         call pushPos(f(ssp)%nrdim,
     &        f(ssp)%pos,
     &        f(ssp)%var(f(ssp)%lonid)%nrlatlon,
     &        f(ssp)%var(f(ssp)%lonid)%indlatlon,
     &        f(ssp)%var(f(ssp)%lonid)%poslatlon)
!     make latitude
         call pushLoc(
     &        f(ssp)%var(f(ssp)%latid)%nrlatlon,
     &        f(ssp)%var(f(ssp)%latid)%limlatlon,
     &        f(ssp)%var(f(ssp)%latid)%poslatlon,
     &        f(ssp)%var(f(ssp)%latid)%loc)
!     make longitude
         call pushLoc(
     &        f(ssp)%var(f(ssp)%lonid)%nrlatlon,
     &        f(ssp)%var(f(ssp)%lonid)%limlatlon,
     &        f(ssp)%var(f(ssp)%lonid)%poslatlon,
     &        f(ssp)%var(f(ssp)%lonid)%loc)
C         
         bok=(f(ssp)%var(f(ssp)%lonid)%fd(
     &        f(ssp)%var(f(ssp)%lonid)%loc).ne.nf_fill_double .and.            
     &        f(ssp)%var(f(ssp)%latid)%fd(
     &        f(ssp)%var(f(ssp)%latid)%loc).ne.nf_fill_double)

         if (.not.bok) then
            call pushLoc(
     &           f(ssp)%nrlatlon,
     &           f(ssp)%limlatlon,
     &           f(ssp)%poslatlon,
     &           f(ssp)%loc)
            if (f(ssp)%loc .gt. f(ssp)%len) then
               write(*,*)myname,'Invalid ssp-loc'
               do ii=1,f(ssp)%nrlatlon
                  write(*,*) f(ssp)%poslatlon(ii),
     &                 f(ssp)%limlatlon(ii)
               end do
            end if
            do pp=1,4
               locmap(pp,f(ssp)%loc)=0
               wgmap(pp,f(ssp)%loc)=0.0D0
            end do
C     increment projection loop position
            odone=increment(1,f(ssp)%nrlatlon,
     &           f(ssp)%limlatlon,f(ssp)%poslatlon,
     &           irc)
            if (irc.ne.0) then
               write(*,*)myname,
     &              'Error return from INCREMENT.',irc
               return
            end if
            cycle
         end if

C-------------------------------------------loop over outer grid
         
         idone=.false.
         do while (.not.idone)  ! loop over outer grid...
C     
C     copy global position vectors to local and get location
C     
            call pullPos(f(ssv)%nrdim,
     &           f(ssv)%pos,
     &           f(ssv)%nrlatlon,
     &           f(ssv)%indlatlon,
     &           f(ssv)%poslatlon)

            write(*,*)myname,'     SSV-Pos:',f(ssv)%pos

            call pushPos(f(ssv)%nrdim,
     &           f(ssv)%pos,
     &           f(ssv)%var(f(ssv)%latid)%nrlatlon,
     &           f(ssv)%var(f(ssv)%latid)%indlatlon,
     &           f(ssv)%var(f(ssv)%latid)%poslatlon)
            call pushPos(f(ssv)%nrdim,
     &           f(ssv)%pos,
     &           f(ssv)%var(f(ssv)%lonid)%nrlatlon,
     &           f(ssv)%var(f(ssv)%lonid)%indlatlon,
     &           f(ssv)%var(f(ssv)%lonid)%poslatlon)
!     make latitude
            call pushLoc(
     &           f(ssv)%var(f(ssv)%latid)%nrlatlon,
     &           f(ssv)%var(f(ssv)%latid)%limlatlon,
     &           f(ssv)%var(f(ssv)%latid)%poslatlon,
     &           f(ssv)%var(f(ssv)%latid)%loc)
!     make longitude
            call pushLoc(
     &           f(ssv)%var(f(ssv)%lonid)%nrlatlon,
     &           f(ssv)%var(f(ssv)%lonid)%limlatlon,
     &           f(ssv)%var(f(ssv)%lonid)%poslatlon,
     &           f(ssv)%var(f(ssv)%lonid)%loc)
C     
C     search for grid-cell, and find interpolation weights...
!     
            dpX(1)=1.0D0
            dpX(2)=1.0D0
            do kk=1,f(ssv)%var(f(ssv)%lonid)%nrlatlon-2
               dpX(1)=dpX(1)*f(ssv)%var(f(ssv)%lonid)%limlatlon(kk)
               dpX(2)=dpX(2)*f(ssv)%var(f(ssv)%lonid)%limlatlon(kk)
            end do
            locX(1)=f(ssv)%var(f(ssv)%lonid)%loc
            locX(2)=locX(1)+dpX(1)
            locX(3)=locX(1)+dpX(2)
            locX(4)=locX(1)+dpX(1)+dpX(2)
            kk=f(ssv)%var(f(ssv)%lonid)%nrlatlon-1
            dpX(2)=dpX(2)*f(ssv)%var(f(ssv)%lonid)%limlatlon(kk)
            kk=f(ssv)%var(f(ssv)%lonid)%nrlatlon-1
            drX(1)=f(ssv)%var(f(ssv)%lonid)%limlatlon(kk)
            kk=f(ssv)%var(f(ssv)%lonid)%nrlatlon
            drX(2)=f(ssv)%var(f(ssv)%lonid)%limlatlon(kk)
!     
            dpY(1)=1.0D0
            dpY(2)=1.0D0
            do kk=1,f(ssv)%var(f(ssv)%latid)%nrlatlon-2
               dpY(1)=dpY(1)*f(ssv)%var(f(ssv)%latid)%limlatlon(kk)
               dpY(2)=dpY(2)*f(ssv)%var(f(ssv)%latid)%limlatlon(kk)
            end do
            locY(1)=f(ssv)%var(f(ssv)%latid)%loc
            locY(2)=locY(1)+dpY(1)
            locY(3)=locY(1)+dpY(2)
            locY(4)=locY(1)+dpY(1)+dpY(2)
            kk=f(ssv)%var(f(ssv)%latid)%nrlatlon-1
            dpY(2)=dpY(2)*f(ssv)%var(f(ssv)%latid)%limlatlon(kk)
            kk=f(ssv)%var(f(ssv)%latid)%nrlatlon-1
            drY(1)=f(ssv)%var(f(ssv)%latid)%limlatlon(kk)
            kk=f(ssv)%var(f(ssv)%latid)%nrlatlon
            drY(2)=f(ssv)%var(f(ssv)%latid)%limlatlon(kk)
!     
!     search for match where locition is within gridcell
!     
!     write(*,*)"XYSEARCH New:",xf,yf
!     
            bdone=.false.
            bdeb=.false.
            do while (.not. bdone)
!
!     check that all grid points are valid...
!
               bok= ( f(ssv)%var(f(ssv)%lonid)%fd(locX(1))
     &              .ne.nf_fill_double .and.  
     &              f(ssv)%var(f(ssv)%latid)%fd(locY(1))
     &              .ne.nf_fill_double .and.
     &              f(ssv)%var(f(ssv)%lonid)%fd(locX(2))
     &              .ne.nf_fill_double .and.
     &              f(ssv)%var(f(ssv)%latid)%fd(locY(2))
     &              .ne.nf_fill_double .and.
     &              f(ssv)%var(f(ssv)%lonid)%fd(locX(4))
     &              .ne.nf_fill_double .and.
     &              f(ssv)%var(f(ssv)%latid)%fd(locY(4))
     &              .ne.nf_fill_double .and.
     &              f(ssv)%var(f(ssv)%lonid)%fd(locX(3))
     &              .ne.nf_fill_double .and.
     &              f(ssv)%var(f(ssv)%latid)%fd(locY(3))
     &              .ne.nf_fill_double)

               do while (.not.bok .and. .not.bdone)
C     increment outer loop position
                  if (.not.increment(f(ssv)%nrlatlon-
     &                 f(ssv)%nrInterpolation,
     &                 f(ssv)%nrlatlon,
     &                 f(ssv)%limlatlon,
     &                 f(ssv)%poslatlon,
     &                 irc)) then
                     call pullPos(f(ssv)%nrdim,
     &                    f(ssv)%pos,
     &                    f(ssv)%nrlatlon,
     &                    f(ssv)%indlatlon,
     &                    f(ssv)%poslatlon)
                     
                     write(*,*)myname,'     SSV-Pos:',f(ssv)%pos
                     
                     call pushPos(f(ssv)%nrdim,
     &                    f(ssv)%pos,
     &                    f(ssv)%var(f(ssv)%latid)%nrlatlon,
     &                    f(ssv)%var(f(ssv)%latid)%indlatlon,
     &                    f(ssv)%var(f(ssv)%latid)%poslatlon)
                     call pushPos(f(ssv)%nrdim,
     &                    f(ssv)%pos,
     &                    f(ssv)%var(f(ssv)%lonid)%nrlatlon,
     &                    f(ssv)%var(f(ssv)%lonid)%indlatlon,
     &                    f(ssv)%var(f(ssv)%lonid)%poslatlon)
!     make latitude
                     call pushLoc(
     &                    f(ssv)%var(f(ssv)%latid)%nrlatlon,
     &                    f(ssv)%var(f(ssv)%latid)%limlatlon,
     &                    f(ssv)%var(f(ssv)%latid)%poslatlon,
     &                    f(ssv)%var(f(ssv)%latid)%loc)
!     make longitude
                     call pushLoc(
     &                    f(ssv)%var(f(ssv)%lonid)%nrlatlon,
     &                    f(ssv)%var(f(ssv)%lonid)%limlatlon,
     &                    f(ssv)%var(f(ssv)%lonid)%poslatlon,
     &                    f(ssv)%var(f(ssv)%lonid)%loc)
C     
C     search for grid-cell, and find interpolation weights...
!     
                     dpX(1)=1.0D0
                     dpX(2)=1.0D0
                     do kk=1,f(ssv)%var(f(ssv)%lonid)%nrlatlon-2
                        dpX(1)=dpX(1)*f(ssv)%var(f(ssv)%lonid)%
     &                       limlatlon(kk)
                        dpX(2)=dpX(2)*f(ssv)%var(f(ssv)%lonid)%
     &                       limlatlon(kk)
                     end do
                     locX(1)=f(ssv)%var(f(ssv)%lonid)%loc
                     locX(2)=locX(1)+dpX(1)
                     locX(3)=locX(1)+dpX(2)
                     locX(4)=locX(1)+dpX(1)+dpX(2)
                     kk=f(ssv)%var(f(ssv)%lonid)%nrlatlon-1
                     dpX(2)=dpX(2)*f(ssv)%var(f(ssv)%lonid)%
     &                    limlatlon(kk)
                     kk=f(ssv)%var(f(ssv)%lonid)%nrlatlon-1
                     drX(1)=f(ssv)%var(f(ssv)%lonid)%limlatlon(kk)
                     kk=f(ssv)%var(f(ssv)%lonid)%nrlatlon
                     drX(2)=f(ssv)%var(f(ssv)%lonid)%limlatlon(kk)
!     
                     dpY(1)=1.0D0
                     dpY(2)=1.0D0
                     do kk=1,f(ssv)%var(f(ssv)%latid)%nrlatlon-2
                        dpY(1)=dpY(1)*f(ssv)%var(f(ssv)%latid)%
     &                       limlatlon(kk)
                        dpY(2)=dpY(2)*f(ssv)%var(f(ssv)%latid)%
     &                       limlatlon(kk)
                     end do
                     locY(1)=f(ssv)%var(f(ssv)%latid)%loc
                     locY(2)=locY(1)+dpY(1)
                     locY(3)=locY(1)+dpY(2)
                     locY(4)=locY(1)+dpY(1)+dpY(2)
                     kk=f(ssv)%var(f(ssv)%latid)%nrlatlon-1
                     dpY(2)=dpY(2)*f(ssv)%var(f(ssv)%latid)%
     &                    limlatlon(kk)
                     kk=f(ssv)%var(f(ssv)%latid)%nrlatlon-1
                     drY(1)=f(ssv)%var(f(ssv)%latid)%limlatlon(kk)
                     kk=f(ssv)%var(f(ssv)%latid)%nrlatlon
                     drY(2)=f(ssv)%var(f(ssv)%latid)%limlatlon(kk)
                     bok= ( f(ssv)%var(f(ssv)%lonid)%fd(locX(1))
     &                    .ne.nf_fill_double .and.  
     &                    f(ssv)%var(f(ssv)%latid)%fd(locY(1))
     &                    .ne.nf_fill_double .and.
     &                    f(ssv)%var(f(ssv)%lonid)%fd(locX(2))
     &                    .ne.nf_fill_double .and.
     &                    f(ssv)%var(f(ssv)%latid)%fd(locY(2))
     &                    .ne.nf_fill_double .and.
     &                    f(ssv)%var(f(ssv)%lonid)%fd(locX(4))
     &                    .ne.nf_fill_double .and.
     &                    f(ssv)%var(f(ssv)%latid)%fd(locY(4))
     &                    .ne.nf_fill_double .and.
     &                    f(ssv)%var(f(ssv)%lonid)%fd(locX(3))
     &                    .ne.nf_fill_double .and.
     &                    f(ssv)%var(f(ssv)%latid)%fd(locY(3))
     &                    .ne.nf_fill_double)
                  else
                     bdone=.true.
                  end if
               end do   
!     
!     walk around grid cell border and calculate cross product
!     
               if (bok.and..not.bdone) then
                  bc=xycross(
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(1)),       
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(1)),
     &                 f(ssp)%var(f(ssp)%lonid)%fd(
     &                 f(ssp)%var(f(ssp)%lonid)%loc),             
     &                 f(ssp)%var(f(ssp)%latid)%fd(
     &                 f(ssp)%var(f(ssp)%latid)%loc),
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(2)),
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(2))) ! bottom
                  rc=xycross(
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(2)),       
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(2)),
     &                 f(ssp)%var(f(ssp)%lonid)%fd(
     &                 f(ssp)%var(f(ssp)%lonid)%loc),                    
     &                 f(ssp)%var(f(ssp)%latid)%fd(
     &                 f(ssp)%var(f(ssp)%latid)%loc),
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(4)),
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(4))) ! right
                  tc=xycross(
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(4)), 
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(4)),
     &                 f(ssp)%var(f(ssp)%lonid)%fd(
     &                 f(ssp)%var(f(ssp)%lonid)%loc),                    
     &                 f(ssp)%var(f(ssp)%latid)%fd(
     &                 f(ssp)%var(f(ssp)%latid)%loc),
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(3)),        
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(3))) ! top
                  lc=xycross(
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(3)),   
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(3)),
     &                 f(ssp)%var(f(ssp)%lonid)%fd(
     &                 f(ssp)%var(f(ssp)%lonid)%loc),                
     &                 f(ssp)%var(f(ssp)%latid)%fd(
     &                 f(ssp)%var(f(ssp)%latid)%loc),
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(1)),          
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(1))) ! left
                  
                  jbc=nint(sign(1.0D0,bc))
                  jrc=nint(sign(1.0D0,rc))
                  jtc=nint(sign(1.0D0,tc))
                  jlc=nint(sign(1.0D0,lc))
                  
                  brc=xycross(
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(1)),              
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(1)),
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(4)),
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(4)),
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(2)),       
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(2))) ! bottom-right
                  rtc=xycross(
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(2)),       
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(2)),
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(3)),       
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(3)),
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(4)),
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(4))) ! right-top
                  tlc=xycross(
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(4)),
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(4)),
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(1)),              
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(1)),
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(3)),       
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(3))) ! top-left
                  lbc=xycross(
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(3)),    
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(3)),
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(2)),    
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(2)),
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(1)),           
     &                 f(ssv)%var(f(ssv)%latid)%fd(locY(1))) ! left-bottom
                  
                  jbrc=nint(sign(1.0D0,brc))
                  jrtc=nint(sign(1.0D0,rtc))
                  jtlc=nint(sign(1.0D0,tlc))
                  jlbc=nint(sign(1.0D0,lbc))
                  
                  if (.true. .or. bdeb) then
                     write(*,'("XYSEARCH ",2(I3,X),'//
     &                    '2(3X,"(",4(X,F13.5),")")'
     &                    //',2(3X,"(",4(X,I2),")"))') 
     &                    ix,jx, bc,rc,tc,lc, brc,rtc,tlc,lbc,
     &                    jbc,jrc,jtc,jlc, jbrc,jrtc,jtlc,jlbc
                     write(*,'(20X,5(" (",F10.5,",'//
     &                    '",F10.5,")"))') 
     &                    f(ssp)%var(f(ssp)%lonid)%fd(
     &                    f(ssp)%var(f(ssp)%lonid)%loc),                     
     &                    f(ssp)%var(f(ssp)%latid)%fd(
     &                    f(ssp)%var(f(ssp)%latid)%loc),
     &                    f(ssv)%var(f(ssv)%lonid)%fd(locX(1)),               
     &                    f(ssv)%var(f(ssv)%latid)%fd(locY(1)),
     &                    f(ssv)%var(f(ssv)%lonid)%fd(locX(2)),        
     &                    f(ssv)%var(f(ssv)%latid)%fd(locY(2)),
     &                    f(ssv)%var(f(ssv)%lonid)%fd(locX(4)), 
     &                    f(ssv)%var(f(ssv)%latid)%fd(locY(4)),
     &                    f(ssv)%var(f(ssv)%lonid)%fd(locX(3)),        
     &                    f(ssv)%var(f(ssv)%latid)%fd(locY(3))



                     f(ssv)%var(f(ssv)%lonid)%loc=locX(1)
                     f(ssv)%var(f(ssv)%latid)%loc=locY(1)
                     call pullLoc(
     &                    f(ssv)%var(f(ssv)%lonid)%nrlatlon,
     &                    f(ssv)%var(f(ssv)%lonid)%limlatlon,
     &                    f(ssv)%var(f(ssv)%lonid)%poslatlon,
     &                    f(ssv)%var(f(ssv)%lonid)%loc)
                     call pullLoc(
     &                    f(ssv)%var(f(ssv)%latid)%nrlatlon,
     &                    f(ssv)%var(f(ssv)%latid)%limlatlon,
     &                    f(ssv)%var(f(ssv)%latid)%poslatlon,
     &                    f(ssv)%var(f(ssv)%latid)%loc)
C     
                     call pullPos(
     &                    f(ssv)%nrdim,
     &                    f(ssv)%pos,
     &                    f(ssv)%var(f(ssv)%lonid)%nrlatlon,
     &                    f(ssv)%var(f(ssv)%lonid)%indlatlon,
     &                    f(ssv)%var(f(ssv)%lonid)%poslatlon)
                     call pullPos(
     &                    f(ssv)%nrdim,
     &                    f(ssv)%pos,
     &                    f(ssv)%var(f(ssv)%latid)%nrlatlon,
     &                    f(ssv)%var(f(ssv)%latid)%indlatlon,
     &                    f(ssv)%var(f(ssv)%latid)%poslatlon)
C     
                     do ss=1,2
                        call pushLoc(
     &                       f(ss)%nrlatlon,
     &                       f(ss)%limlatlon,
     &                       f(ss)%poslatlon,
     &                       f(ss)%loc)
                     end do

                     write(*,'(X,A,A,4(X,I8),5X,10(X,I8))')
     &                    myname,'     SSV-Pos:',
     &                    ix,jx,locX(1),locY(1),
     &                    f(ssv)%var(f(ssv)%latid)%poslatlon,
     &                    f(ssv)%var(f(ssv)%latid)%limlatlon

               if (bok.and.(
     &              f(ssv)%var(f(ssv)%lonid)%fd(locX(1))
     &              .gt.400.0D0.or.
     &              f(ssv)%var(f(ssv)%lonid)%fd(locX(1))
     &              .lt.-400.0D0)) then
                  write(*,*) myname,'System error:',
     &                 f(ssv)%var(f(ssv)%lonid)%fd(locX(1)),
     &                 nf_fill_double
               end if




                  end if
!     
!     if cross product has same sign as axis-sign => inside, else outside
!     (make sure we do not cross border while searching...)
!     
                  ix=mod(int(locX(1)-1)/dpX(1),drx(1))+1
                  jx=mod(int(locX(2)-1)/dpX(2),drx(2))+1
                  iy=mod(int(locY(1)-1)/dpY(1),dry(1))+1
                  jy=mod(int(locY(2)-1)/dpY(2),dry(2))+1
                  inside=.true.
                  changed=.false.
                  if (jbc.ne.jbrc) then ! decrease j
                     if (jx.gt.1.and.jy.gt.1) then
!     check that we move to a valid grid point....
                        if ( f(ssv)%var(f(ssv)%lonid)%fd(locX(1)-dpX(2))
     &                       .ne.nf_fill_double .and.  
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(1)-dpY(2))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%lonid)%fd(locX(2)-dpX(2))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(2)-dpY(2))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%lonid)%fd(locX(4)-dpX(2))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(4)-dpY(2))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%lonid)%fd(locX(3)-dpX(2))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(3)-dpY(2))
     &                       .ne.nf_fill_double) then
                           locX(1)=locX(1)-dpX(2)
                           locY(1)=locY(1)-dpY(2)
                           changed=.true.
                        end if
                     end if
                     inside=.false.
                  else if (jtc.ne.jtlc) then ! increase j
                     if (jx.lt.drx(2)-1.and.jy.lt.dry(2)-1) then
                        if ( f(ssv)%var(f(ssv)%lonid)%fd(locX(1)+dpX(2))
     &                       .ne.nf_fill_double .and.  
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(1)+dpY(2))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%lonid)%fd(locX(2)+dpX(2))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(2)+dpY(2))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%lonid)%fd(locX(4)+dpX(2))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(4)+dpY(2))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%lonid)%fd(locX(3)+dpX(2))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(3)+dpY(2))
     &                       .ne.nf_fill_double) then
                           locX(1)=locX(1)+dpX(2)
                           locY(1)=locY(1)+dpY(2)
                           changed=.true.
                        end if
                     end if
                     inside=.false.
                  end if
                  if (jlc.ne.jlbc) then ! decrease i
                     if (ix.gt.1.and.iy.gt.1) then
                        if ( f(ssv)%var(f(ssv)%lonid)%fd(locX(1)-dpX(1))
     &                       .ne.nf_fill_double .and.  
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(1)-dpY(1))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%lonid)%fd(locX(2)-dpX(1))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(2)-dpY(1))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%lonid)%fd(locX(4)-dpX(1))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(4)-dpY(1))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%lonid)%fd(locX(3)-dpX(1))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(3)-dpY(1))
     &                       .ne.nf_fill_double) then
                           locX(1)=locX(1)-dpX(1)
                           locY(1)=locY(1)-dpY(1)
                           changed=.true.
                        end if
                     end if
                     inside=.false.
                  else if (jrc.ne.jrtc) then ! increase i
                     if (ix.lt.drx(1)-1.and.iy.lt.dry(1)-1) then
                        if ( f(ssv)%var(f(ssv)%lonid)%fd(locX(1)+dpX(1))
     &                       .ne.nf_fill_double .and.  
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(1)+dpY(1))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%lonid)%fd(locX(2)+dpX(1))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(2)+dpY(1))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%lonid)%fd(locX(4)+dpX(1))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(4)+dpY(1))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%lonid)%fd(locX(3)+dpX(1))
     &                       .ne.nf_fill_double .and.
     &                       f(ssv)%var(f(ssv)%latid)%fd(locY(3)+dpY(1))
     &                       .ne.nf_fill_double) then
                           locX(1)=locX(1)+dpX(1)
                           locY(1)=locY(1)+dpY(1)
                           changed=.true.
                        end if
                     end if
                     inside=.false.
                  end if
                  locX(2)=locX(1)+dpX(1)
                  locX(3)=locX(1)+dpX(2)
                  locX(4)=locX(1)+dpX(1)+dpX(2)
                  locY(2)=locY(1)+dpY(1)
                  locY(3)=locY(1)+dpY(2)
                  locY(4)=locY(1)+dpY(1)+dpY(2)
                  if (inside) then ! we are inside the cell
                     bok=.true.
                     bdone=.true.
                     tbc=tc+bc
                     lrc=lc+rc
                     xfc=(tbc*lrc)
                     wg(1)=rc*tc/xfc
                     wg(2)=lc*tc/xfc
                     wg(4)=lc*bc/xfc
                     wg(3)=rc*bc/xfc
                     if (bdeb) write(*,*)"XYSEARCH",ix,jx,"Done:",wg
                  else if (.not.changed) then ! not inside + no valid step
                     if (bdeb) then
                        write(*,'(X,A,2(I3,X),6(F10.1,X))')
     &                       'No joy:',iX,jX
                     end if
                     bdone=.true.
                     bok=.false.
                  end if
               end if
            end do              ! bdone
               
               
            if (bok) then
               do pp=1,4
                  f(ssv)%var(f(ssv)%lonid)%loc=locX(pp)
                  f(ssv)%var(f(ssv)%latid)%loc=locY(pp)
                  call pullLoc(
     &                 f(ssv)%var(f(ssv)%lonid)%nrlatlon,
     &                 f(ssv)%var(f(ssv)%lonid)%limlatlon,
     &                 f(ssv)%var(f(ssv)%lonid)%poslatlon,
     &                 f(ssv)%var(f(ssv)%lonid)%loc)
                  call pullLoc(
     &                 f(ssv)%var(f(ssv)%latid)%nrlatlon,
     &                 f(ssv)%var(f(ssv)%latid)%limlatlon,
     &                 f(ssv)%var(f(ssv)%latid)%poslatlon,
     &                 f(ssv)%var(f(ssv)%latid)%loc)
C     
                  call pullPos(
     &                 f(ssv)%nrdim,
     &                 f(ssv)%pos,
     &                 f(ssv)%var(f(ssv)%lonid)%nrlatlon,
     &                 f(ssv)%var(f(ssv)%lonid)%indlatlon,
     &                 f(ssv)%var(f(ssv)%lonid)%poslatlon)
                  call pullPos(
     &                 f(ssv)%nrdim,
     &                 f(ssv)%pos,
     &                 f(ssv)%var(f(ssv)%latid)%nrlatlon,
     &                 f(ssv)%var(f(ssv)%latid)%indlatlon,
     &                 f(ssv)%var(f(ssv)%latid)%poslatlon)
C     
                  do ss=1,2
                     call pushLoc(
     &                    f(ss)%nrlatlon,
     &                    f(ss)%limlatlon,
     &                    f(ss)%poslatlon,
     &                    f(ss)%loc)
                  end do
C     
                  if (f(ssp)%loc .gt. f(ssp)%len) then
                     write(*,*)myname,'Invalid ssp-loc'
                     do ii=1,f(ssp)%nrlatlon
                        write(*,*) f(ssp)%poslatlon(ii),
     &                       f(ssp)%limlatlon(ii)
                     end do
                  end if
                  
                  if (f(ssv)%loc .gt. f(ssv)%len) then
                     write(*,*)myname,'Invalid ssv-loc'
                     do ii=1,f(ssv)%nrlatlon
                        write(*,*) f(ssv)%poslatlon(ii),
     &                       f(ssv)%limlatlon(ii)
                     end do
                  end if
C     
                  locmap(pp,f(ssp)%loc)=f(ssv)%loc
                  wgmap(pp,f(ssp)%loc)=wg(pp)
               end do
            end if
            
C     
C     increment outer loop position
            idone=increment(1,f(ssv)%nrlatlon-f(ssv)%nrInterpolation,
     &           f(ssv)%limlatlon,f(ssv)%poslatlon,
     &           irc)
            if (irc.ne.0) then
               write(*,*)myname,
     &              'Error return from INCREMENT.',irc
               return
            end if
         end do                 ! idone
         
C     increment projection loop position
         odone=increment(1,f(ssp)%nrlatlon,
     &        f(ssp)%limlatlon,f(ssp)%poslatlon,
     &        irc)
         if (irc.ne.0) then
            write(*,*)myname,
     &           'Error return from INCREMENT.',irc
            return
         end if
      end do                    ! odone
C     
C     clean up memory
C     
      do ss=1,nrss
         do jj=1,2
            if (jj.eq.1) then
               varid=f(ss)%latid
            else
               varid=f(ss)%lonid
            end if
            if (associated(f(ss)%var(varid)%fc))
     &           deallocate( f(ss)%var(varid)%fr,stat=irc)
            if (associated(f(ss)%var(varid)%f1)) 
     &           deallocate( f(ss)%var(varid)%fr,stat=irc)
            if (associated(f(ss)%var(varid)%f2)) 
     &           deallocate( f(ss)%var(varid)%fr,stat=irc)
            if (associated(f(ss)%var(varid)%f4)) 
     &           deallocate( f(ss)%var(varid)%fr,stat=irc)
            if (associated(f(ss)%var(varid)%fr)) 
     &           deallocate( f(ss)%var(varid)%fr,stat=irc)
            if (associated(f(ss)%var(varid)%fd)) 
     &           deallocate( f(ss)%var(varid)%fr,stat=irc)
         end do
      end do
C     
      write(*,*)myname,'Making output file.'
C     
C     create output file, enter define mode
C     
      leno=length(out250,250,10)
      write(*,*)myname,'Creating:',out250(1:leno)
      ret=NF_CREATE(out250(1:leno),or(NF_NOCLOBBER,NF_SHARE),ncid) ! create netCDF dataset: enter define mode
      if (ret .ne. NF_NOERR) then
         write(*,*) myname,"ERROR from NF_CREATE:",
     $        nf_strerror(ret),ncid
         irc=790
         return
      end if
C     
      write(*,*)myname,'Allocating.'
C     
C     make global dimension list (dimensions have already been marked)
C     
      allocate(
     &     gdim250(f(ssv)%nrdim+f(ssp)%nrdim),
     &     gdim(f(ssv)%nrdim+f(ssp)%nrdim),
     &     glim(f(ssv)%nrdim+f(ssp)%nrdim),
     &     gunl(f(ssv)%nrdim+f(ssp)%nrdim),
     &     stat=irc)
      if (irc.ne.0) then
         write(*,*) myname,'Unable to allocate GDIM.',irc
         return
      end if
      nrgdim=0
      do ss=1,nrss
         do jj=1,f(ss)%nrdim
            if (f(ss)%use(jj)) then
               nrgdim=nrgdim+1
               f(ss)%gdim(jj)=nrgdim
               gdim250(nrgdim)=f(ss)%dim250(jj)
               call chop0(gdim250(nrgdim),250)
               glim(nrgdim)=f(ss)%lim(jj)
               gunl(nrgdim)=(f(ss)%unlimdimid.eq.jj)
               found=.true.
               do while (found)
                  found=.false.
                  do kk=1,nrgdim-1
                     if (gdim250(kk).eq.gdim250(nrgdim)) then
                        found=.true.
                     end if
                  end do
                  if (found) then ! change name
                     leng=length(gdim250(nrgdim),250,10)
                     gdim250(nrgdim)=gdim250(nrgdim)(1:leng)//'+'
                     call chop0(gdim250(nrgdim),250)
                  end if
               end do
            end if
         end do
      end do
C     define dimensions in output file
      do ii=1,nrgdim
         leng=length(gdim250(ii),250,10)
         if (gunl(ii)) then
            ret=NF_DEF_DIM(NCID,gdim250(ii)(1:leng),
     &           nf_unlimited,gdim(ii))
         else
            ret=NF_DEF_DIM(NCID,gdim250(ii)(1:leng),
     &           glim(ii),gdim(ii))
         end if
         if (ret .ne. NF_NOERR) then
            write(*,*) myname,"ERROR from NF_DEF_DIM:",
     $           nf_strerror(ret),ncid
            irc=790
            return
         end if
C     write(*,'(X,A,A,A20,100(X,I5))')
C     &        myname,'   Defined: ',gdim250(ii)(1:leng),
C     &        gdim(ii),glim(ii)
      end do
C     
C     make variable list
C     
      allocate(
     &     gvar250(f(ssv)%nrvar+f(ssp)%nrvar),
     &     gvarid(f(ssv)%nrvar+f(ssp)%nrvar),
     &     gvardim(f(ssv)%nrdim+f(ssp)%nrdim),
     &     stat=irc)
      if (irc.ne.0) then
         write(*,*) myname,'Unable to allocate GVAR.',irc
         return
      end if
      nrgvar=0
      do ss=1,nrss
         do varid=1,f(ss)%nrvar
            if (f(ss)%var(varid)%use) then
               nrgvar=nrgvar+1
               f(ss)%var(varid)%gvar=nrgvar
               gvar250(nrgvar)=f(ss)%var(varid)%var250
               call chop0(gvar250(nrgvar),250)
               found=.true.
               do while (found)
                  found=.false.
                  do kk=1,nrgvar-1
                     if (gvar250(kk).eq.gvar250(nrgvar)) then
                        found=.true.
                     end if
                  end do
                  if (found) then ! change name
                     leng=length(gvar250(nrgvar),250,10)
                     gvar250(nrgvar)=gvar250(nrgvar)(1:leng)//'+'
                     call chop0(gvar250(nrgvar),250)
                  end if
               end do
               if (f(ss)%var(varid)%status(request).and.
     &              f(ss)%var(varid)%status(dimlatlon))then ! use part dimensions
                  nrgvardim=0
                  do jj=1,f(ssp)%nrlatlon
                     nrgvardim=nrgvardim+1
                     gvardim(nrgvardim) = 
     &                    f(ssp)%gdim(f(ssp)%indlatlon(jj))
                  end do
                  do jj=1,f(ss)%var(varid)%nrrest
                     nrgvardim=nrgvardim+1
                     gvardim(nrgvardim)= 
     &                    f(ss)%gdim(f(ss)%var(varid)%indrest(jj))
                  end do
               else if (f(ss)%var(varid)%status(latlon).or.
     &                 f(ss)%var(varid)%status(project).or.
     &                 f(ss)%var(varid)%status(request)) then
                  nrgvardim=0
                  do jj=1,f(ss)%var(varid)%nrdim
                     nrgvardim=nrgvardim+1
                     gvardim(nrgvardim)=
     &                    f(ss)%gdim(f(ss)%var(varid)%ind(jj))
                  end do
               end if
               leng=length(gvar250(nrgvar),250,10)
               ret=NF_DEF_VAR(NCID,gvar250(nrgvar),
     &              f(ss)%var(varid)%type,
     &              nrgvardim,gvardim,gvarid(nrgvar))
               if (ret .ne. NF_NOERR) then
                  write(*,*) myname,"ERROR from NF_DEF_VAR:",
     $                 nf_strerror(ret),ncid
                  irc=790
                  return
               end if

C     write(*,'(X,A,A20,100(X,A10))')
C     &              '   Defined: ',gvar250(nrgvar)(1:20),
C     &              gvarid(nrgvar),(gdim250(gvardim(ii))(1:10),
C     &              ii=1,nrgvardim)


C     change value of "grid_mapping" attribute to ssp-variable name having attribute "grid_mapping_name"
               ii=f(ss)%var(varid)%projatt
               if (ii.ne.0.and.lengrid.ne.0) then
                  if (f(ss)%var(varid)%attributes(ii)%type
     &                 .eq.nf_char) then
                     leng=length(grid250,250,10)
                     if (associated(f(ss)%var(varid)%attributes(ii)
     &                    %ac)) deallocate(f(ss)%var(varid)
     &                    %attributes(ii)%ac,stat=irc)
                     f(ss)%var(varid)%attributes(ii)%len=leng
                     allocate(f(ss)%var(varid)%attributes(ii)
     &                    %ac(leng),stat=irc)
                     do jj=1,leng
                        f(ss)%var(varid)%attributes(ii)%ac(jj)=
     &                       grid250(jj:jj)
                     end do
                  end if
               end if
               do ii=1,f(ss)%var(varid)%nratt
                  lena=length(f(ss)%var(varid)%attributes(ii)%att250,
     &                 250,10)
                  if (f(ss)%var(varid)%attributes(ii)%type
     &                 .eq.nf_char) then
                     ret=NF_PUT_ATT_TEXT  (ncid, gvarid(nrgvar),
     &                    f(ss)%var(varid)%attributes(ii)
     &                    %att250(1:lena),
     &                    f(ss)%var(varid)%attributes(ii)%LEN,
     &                    f(ss)%var(varid)%attributes(ii)%ac) 
                  else if (f(ss)%var(varid)%attributes(ii)%type
     &                    .eq.nf_int1) then
                     ret=NF_PUT_ATT_INT1  (ncid, gvarid(nrgvar),
     &                    f(ss)%var(varid)%attributes(ii)
     &                    %att250(1:lena),
     &                    f(ss)%var(varid)%attributes(ii)%type,
     &                    f(ss)%var(varid)%attributes(ii)%LEN, 
     &                    f(ss)%var(varid)%attributes(ii)%a1)
                  else if (f(ss)%var(varid)%attributes(ii)%type
     &                    .eq.nf_int2) then
                     ret=NF_PUT_ATT_INT2  (ncid, gvarid(nrgvar),
     &                    f(ss)%var(varid)%attributes(ii)
     &                    %att250(1:lena),
     &                    f(ss)%var(varid)%attributes(ii)%type,
     &                    f(ss)%var(varid)%attributes(ii)%LEN, 
     &                    f(ss)%var(varid)%attributes(ii)%a2)
                  else if (f(ss)%var(varid)%attributes(ii)%type
     &                    .eq.nf_int) then
                     ret=NF_PUT_ATT_INT  (ncid, gvarid(nrgvar),
     &                    f(ss)%var(varid)%attributes(ii)
     &                    %att250(1:lena),
     &                    f(ss)%var(varid)%attributes(ii)%type,
     &                    f(ss)%var(varid)%attributes(ii)%LEN, 
     &                    f(ss)%var(varid)%attributes(ii)%a4)
                  else if (f(ss)%var(varid)%attributes(ii)%type
     &                    .eq.nf_real) then
                     ret=NF_PUT_ATT_REAL  (ncid, gvarid(nrgvar),
     &                    f(ss)%var(varid)%attributes(ii)
     &                    %att250(1:lena),
     &                    f(ss)%var(varid)%attributes(ii)%type,
     &                    f(ss)%var(varid)%attributes(ii)%LEN, 
     &                    f(ss)%var(varid)%attributes(ii)%ar)
                  else if (f(ss)%var(varid)%attributes(ii)%type
     &                    .eq.nf_double) then
                     ret=NF_PUT_ATT_DOUBLE  (ncid, gvarid(nrgvar),
     &                    f(ss)%var(varid)%attributes(ii)
     &                    %att250(1:lena),
     &                    f(ss)%var(varid)%attributes(ii)%type,
     &                    f(ss)%var(varid)%attributes(ii)%LEN, 
     &                    f(ss)%var(varid)%attributes(ii)%ad)
                  end if
                  if (ret .ne. NF_NOERR) then
                     write(*,*) myname,"ERROR from NF_PUT_ATT_*:",
     $                    nf_strerror(ret),ncid
                     irc=790
                     return
                  end if
               end do
            end if
         end do
      end do
C     
C     write global attributes to file
C     
      write(*,*)myname,'Writing global attributes.'
      ss=ssv
      do ii=1,f(ss)%nrgatt
         att250=f(ss)%attributes(ii)%att250
         lena=length(att250,250,10)
         do jj=1,lena
            if (
     &           att250(jj:min(lena,jj+9)).eq."projection".or.
     &           att250(jj:min(lena,jj+4)).eq."north".or.
     &           att250(jj:min(lena,jj+3)).eq."west".or.
     &           att250(jj:min(lena,jj+4)).eq."south".or.
     &           att250(jj:min(lena,jj+3)).eq."east".or.
     &           att250(jj:min(lena,jj+7)).eq."latitude".or.
     &           att250(jj:min(lena,jj+8)).eq."longitude"
     &           ) then
               cycle
            end if
         end do
         if (f(ss)%attributes(ii)%type
     &        .eq.nf_char) then
            ret=NF_PUT_ATT_TEXT  (NCID, NF_GLOBAL,
     &           f(ss)%attributes(ii)
     &           %att250(1:lena),
     &           f(ss)%attributes(ii)%LEN, 
     &           f(ss)%attributes(ii)%ac)
         else if (f(ss)%attributes(ii)%type
     &           .eq.nf_int1) then
            ret=NF_PUT_ATT_INT1  (NCID, NF_GLOBAL,
     &           f(ss)%attributes(ii)
     &           %att250(1:lena),
     &           f(ss)%attributes(ii)%type,
     &           f(ss)%attributes(ii)%LEN, 
     &           f(ss)%attributes(ii)%a1)
         else if (f(ss)%attributes(ii)%type
     &           .eq.nf_int2) then
            ret=NF_PUT_ATT_INT2  (NCID, NF_GLOBAL,
     &           f(ss)%attributes(ii)
     &           %att250(1:lena),
     &           f(ss)%attributes(ii)%type,
     &           f(ss)%attributes(ii)%LEN, 
     &           f(ss)%attributes(ii)%a2)
         else if (f(ss)%attributes(ii)%type
     &           .eq.nf_int) then
            ret=NF_PUT_ATT_INT  (NCID, NF_GLOBAL,
     &           f(ss)%attributes(ii)
     &           %att250(1:lena),
     &           f(ss)%attributes(ii)%type,
     &           f(ss)%attributes(ii)%LEN, 
     &           f(ss)%attributes(ii)%a4)
         else if (f(ss)%attributes(ii)%type
     &           .eq.nf_real) then
            ret=NF_PUT_ATT_REAL  (NCID, NF_GLOBAL,
     &           f(ss)%attributes(ii)
     &           %att250(1:lena),
     &           f(ss)%attributes(ii)%type,
     &           f(ss)%attributes(ii)%LEN, 
     &           f(ss)%attributes(ii)%ar)
         else if (f(ss)%attributes(ii)%type
     &           .eq.nf_DOUBLE) then
            ret=NF_PUT_ATT_DOUBLE  (NCID, NF_GLOBAL,
     &           f(ss)%attributes(ii)
     &           %att250(1:lena),
     &           f(ss)%attributes(ii)%type,
     &           f(ss)%attributes(ii)%LEN, 
     &           f(ss)%attributes(ii)%ad)
         end if
         if (ret .ne. NF_NOERR) then
            write(*,*) myname,"ERROR-2 from NF_PUT_ATT_*:",
     $           nf_strerror(ret),ncid
            irc=790
            return
         end if
      end do

      ret=NF_ENDDEF(ncid)       ! end definitions: leave define mode
      if (ret .ne. NF_NOERR) then
         write(*,*) myname,"ERROR from NF_ENDDEF:",
     $        nf_strerror(ret),ncid
         irc=790
         return
      end if
C     
C     we want to write out the ssp-fields: latid, lonid, projid, without any processing...
C     
      write(*,*)myname,'Writing NAV fields.'

C     and all global ssv-attributes except those containing "projection", "latitude" etc.
C     and the ssp-attributes containing "projection", "north", "latitude" etc...

C     
      nrdim=f(ssv)%nrdim-f(ssv)%nrlatlon+f(ssp)%nrlatlon ! global
C     
C     allocate new global position
C     
      allocate(pos(nrdim),lim(nrdim),stat=irc)
      if (irc.ne.0) then
         write(*,*) myname,'Unable to allocate POS+LIM.',irc
         return
      end if
C     
C     loop over ssv-variable fields i variable file, synthesize output and write to file
C     
      write(*,*)myname,'Making variables.'
      do ss=1,nrss
         do varid=1,f(ss)%nrvar
C     
            if (.not.(f(ss)%var(varid)%use)) cycle
C     
            gvar=gvarid(f(ss)%var(varid)%gvar)
C     
C     ************************************************************
C     make limits
C     ************************************************************
C     
C     make ssp-latlon-lim
            call pushPos(
     &           f(ssp)%nrdim,
     &           f(ssp)%lim,
     &           f(ssp)%nrlatlon,
     &           f(ssp)%indlatlon,
     &           f(ssp)%limlatlon)
C     
C     make ssv-var-lim
            call pushPos(
     &           f(ss)%nrdim,
     &           f(ss)%lim,
     &           f(ss)%var(varid)%nrdim,
     &           f(ss)%var(varid)%ind,
     &           f(ss)%var(varid)%lim)
C     
C     make var-latlon-lim
            call pushPos(
     &           f(ss)%nrdim,
     &           f(ss)%lim,
     &           f(ss)%var(varid)%nrlatlon,
     &           f(ss)%var(varid)%indlatlon,
     &           f(ss)%var(varid)%limlatlon)
C     
            if (f(ss)%var(varid)%status(unchanged)) then
               call pushPos(
     &              f(ss)%nrdim,
     &              f(ss)%lim,
     &              f(ss)%var(varid)%nrdim,
     &              f(ss)%var(varid)%ind,
     &              lim)
C     
               nrdim=f(ss)%var(varid)%nrdim ! local
            else
C     
C     make new-lim
               call pushPos(
     &              f(ssp)%nrdim,
     &              f(ssp)%lim,
     &              f(ssp)%nrlatlon,
     &              f(ssp)%indlatlon,
     &              lim)
               call pushPos(
     &              f(ss)%nrdim,
     &              f(ss)%lim,
     &              f(ss)%var(varid)%nrrest,
     &              f(ss)%var(varid)%indrest,
     &              lim(f(ssp)%nrlatlon+1))
C     
               nrdim=f(ssp)%nrlatlon+f(ssv)%var(varid)%nrrest ! local
            end if
C     
C     allocate variable projection and read variable field into memory
C     
            plen=f(ssp)%nrdim+f(ssV)%nrdim
            allocate(start(plen),stat=irc)
            if (irc.ne.0) then
               write(*,*) myname,'Unable to allocate LOC.',irc,
     &              f(ssv)%var(varid)%nrdim
               return
            end if
            do ii=1,plen
               start(ii)=1
            end do
C     
C     reading variable
            lenvi=length(f(ss)%var(varid)%var250,250,10)
            write(*,*)myname,'Reading field: ',
     &           f(ss)%var(varid)%var250(1:lenvi)

C     find length of var-field
            type=f(ss)%var(varid)%type
            clen=1
            do ii=1,f(ss)%var(varid)%nrdim
               clen=clen*f(ss)%var(varid)%lim(ii)
            end do
            f(ss)%var(varid)%len=clen
            if (type.eq.nf_char) then
               allocate( f(ss)%var(varid)%fc(clen), 
     &              stat=irc)
               if (irc.ne.0) then
                  write(*,*) myname,'Unable to allocate fc.',irc
                  return
               end if
               ret = nf_get_vara_text(f(ss)%ncid,varid,
     &              start,f(ss)%var(varid)%lim,
     &              f(ss)%var(varid)%fc)
               if (ret .ne. NF_NOERR) then
                  write(*,*) myname,"ERROR from NF_GET_VARA_TEXT:",
     $                 nf_strerror(ret)
                  irc=812
                  return
               end if
            else if (type.eq.nf_int1) then
               allocate( f(ss)%var(varid)%f1(clen), 
     &              stat=irc)
               if (irc.ne.0) then
                  write(*,*) myname,'Unable to allocate f1.',irc
                  return
               end if
               ret = nf_get_vara_int1(f(ss)%ncid,varid,
     &              start,f(ss)%var(varid)%lim,
     &              f(ss)%var(varid)%f1)
               if (ret .ne. NF_NOERR) then
                  write(*,*) myname,"ERROR from NF_GET_VARA_INT1:",
     $                 nf_strerror(ret)
                  irc=812
                  return
               end if
            else if (type.eq.nf_int2) then
               allocate( f(ss)%var(varid)%f2(clen), 
     &              stat=irc)
               if (irc.ne.0) then
                  write(*,*) myname,'Unable to allocate f2.',irc
                  return
               end if
               ret = nf_get_vara_int2(f(ss)%ncid,varid,
     &              start,f(ss)%var(varid)%lim,
     &              f(ss)%var(varid)%f2)
               if (ret .ne. NF_NOERR) then
                  write(*,*) myname,"ERROR from NF_GET_VARA_INT2:",
     $                 nf_strerror(ret)
                  irc=812
                  return
               end if
            else if (type.eq.nf_int) then
               allocate( f(ss)%var(varid)%f4(clen), 
     &              stat=irc)
               if (irc.ne.0) then
                  write(*,*) myname,'Unable to allocate f4.',irc
                  return
               end if
               ret = nf_get_vara_int(f(ss)%ncid,varid,
     &              start,f(ss)%var(varid)%lim,
     &              f(ss)%var(varid)%f4)
               if (ret .ne. NF_NOERR) then
                  write(*,*) myname,"ERROR from NF_GET_VARA_INT:",
     $                 nf_strerror(ret)
                  irc=812
                  return
               end if
            else if (type.eq.nf_real) then
               allocate( f(ss)%var(varid)%fr(clen), 
     &              stat=irc)
               if (irc.ne.0) then
                  write(*,*) myname,'Unable to allocate fr.',irc
                  return
               end if
               ret = nf_get_vara_real(f(ss)%ncid,varid,
     &              start,f(ss)%var(varid)%lim,
     &              f(ss)%var(varid)%fr)
               if (ret .ne. NF_NOERR) then
                  write(*,*) myname,"ERROR from NF_GET_VARA_REAL:",
     $                 nf_strerror(ret)
                  irc=812
                  return
               end if
            else if (type.eq.nf_double) then
               allocate( f(ss)%var(varid)%fd(clen), 
     &              stat=irc)
               if (irc.ne.0) then
                  write(*,*) myname,'Unable to allocate fd.',irc
                  return
               end if
               ret = nf_get_vara_double(f(ss)%ncid,varid,
     &              start,f(ss)%var(varid)%lim,
     &              f(ss)%var(varid)%fd)
               if (ret .ne. NF_NOERR) then
                  write(*,*) myname,"ERROR from NF_GET_VARA_DOUBLE:",
     $                 nf_strerror(ret)
                  irc=812
                  return
               end if
            else 
            end if
            if (f(ss)%var(varid)%status(unchanged)) then
C     write to file
               lenvi=length(f(ss)%var(varid)%var250,250,10)
               write(*,*)myname,'Writing field: ',
     &              f(ss)%var(varid)%var250(1:lenvi)
C     
               if (type.eq.nf_char) then
                  ret= NF_PUT_VARA_TEXT(NCID,gvar,
     &                 start, f(ss)%var(varid)%lim,f(ss)%var(varid)%fc)
               else if (type.eq.nf_int1) then
                  ret= NF_PUT_VARA_INT1(NCID,gvar,
     &                 start, f(ss)%var(varid)%lim,f(ss)%var(varid)%f1)
               else if (type.eq.nf_int2) then
                  ret= NF_PUT_VARA_INT2(NCID,gvar,
     &                 start, f(ss)%var(varid)%lim,f(ss)%var(varid)%f2)
               else if (type.eq.nf_int) then
                  ret= NF_PUT_VARA_INT(NCID,gvar,
     &                 start, f(ss)%var(varid)%lim,f(ss)%var(varid)%f4)
               else if (type.eq.nf_real) then
                  ret= NF_PUT_VARA_REAL(NCID,gvar,
     &                 start,f(ss)%var(varid)%lim,f(ss)%var(varid)%fr)
               else if (type.eq.nf_double) then
                  ret= NF_PUT_VARA_DOUBLE(NCID,gvar,
     &                 start, f(ss)%var(varid)%lim,f(ss)%var(varid)%fd)
               else
                  write(*,*)myname,'Unknown variable type:',
     &                 f(ssv)%var(varid)%type
                  irc=919
                  return
               end if
               if (ret .ne. NF_NOERR) then
                  write(*,*) myname,"ERROR from NF_PUT_VARA_*:",
     $                 nf_strerror(ret)
                  irc=812
                  return
               end if
            else                ! remap and write to file
C     
C     find new length and allocate
               len=1
               do ii=1,nrdim
                  len=len*lim(ii)
               end do
               if (type.eq.nf_char) then
                  allocate( fc(len),stat=irc)
                  do ii=1,len
                     fc(ii)=fillc
                  end do
               elseif (type.eq.nf_int1) then
                  allocate( f1(len),stat=irc)
                  do ii=1,len
                     f1(ii)=fill1
                  end do
               elseif (type.eq.nf_int2) then
                  allocate( f2(len),stat=irc)
                  do ii=1,len
                     f2(ii)=fill2
                  end do
               elseif (type.eq.nf_int) then
                  allocate( f4(len),stat=irc)
                  do ii=1,len
                     f4(ii)=fill4
                  end do
               elseif (type.eq.nf_real) then
                  allocate( fr(len),stat=irc)
                  do ii=1,len
                     fr(ii)=fillr
                  end do
               elseif (type.eq.nf_double) then
                  allocate( fd(len),stat=irc)
                  do ii=1,len
                     fd(ii)=filld
                  end do
               else 
               end if
               if (irc.ne.0) then
                  write(*,*) myname,'Unable to allocate f*.',irc,len
                  return
               end if
C     
C     initialise ssp-latlon-pos
C     
               do ii=1,f(ssp)%nrlatlon
                  f(ssp)%poslatlon(ii)=1
               end do

C     reading variable
               lenvi=length(f(ss)%var(varid)%var250,250,10)
               write(*,*)myname,'Remapping:     ',
     &              f(ss)%var(varid)%var250(1:lenvi)
C     
C     ignore variables without all latlon dimensions
C     
               odone=.false.
C     
C     loop over ssp-latlon-pos
C     
               do while (.not.odone) ! indlatlon loop over ssp pos
C     
C     get ssp-pos from ssp-latlon-pos
C     
                  call pullPos(
     &                 f(ssp)%nrdim,
     &                 f(ssp)%pos,
     &                 f(ssp)%nrlatlon,
     &                 f(ssp)%indlatlon,
     &                 f(ssp)%poslatlon)
C     
C     get ssp-latlon-loc from ssp-latlon-pos
C     
                  call pushLoc(
     &                 f(ssp)%nrlatlon,
     &                 f(ssp)%limlatlon,
     &                 f(ssp)%poslatlon,
     &                 f(ssp)%loclatlon)
C     
C     ************************************************************
C     get ssv-latlon-loc
C     ************************************************************
C     
                  do pp=1,4
                     loclatlon=locmap(pp,f(ssp)%loclatlon)
                     wg(pp)=wgmap(pp,f(ssp)%loclatlon)
                     if (loclatlon.gt.0) then
                        f(ssv)%loclatlon=loclatlon ! latlon location in variable...
C     
C     ************************************************************
C     make ssv-latlon-pos
C     ************************************************************
C     
C     get ssv-latlon-pos from ssv-latlon-loc
C     
                        call pullLoc(
     &                       f(ssv)%nrlatlon,
     &                       f(ssv)%limlatlon,
     &                       f(ssv)%poslatlon,
     &                       f(ssv)%loclatlon)
C     
C     write(*,*)myname,'SSV-latlon-pos:'
C     do ii=1,f(ssv)%nrlatlon
C     write(*,*) 
C     &                 f(ssv)%poslatlon(ii),
C     &                 f(ssv)%limlatlon(ii)
C     end do
C     
C     get ssv-pos from ssv-latlon-pos
C     
                        call pullPos(
     &                       f(ssv)%nrdim,
     &                       f(ssv)%pos,
     &                       f(ssv)%nrlatlon,
     &                       f(ssv)%indlatlon,
     &                       f(ssv)%poslatlon)
                        
                        
C     write(*,*)myname,'SSV-pos:', f(ssv)%loclatlon
C     do ii=1,f(ssv)%nrdim
C     write(*,*) 
C     &                 f(ssv)%pos(ii),
C     &                 f(ssv)%lim(ii)
C     end do
C     
C     get ssv-var-latlon-pos from ssv-pos
C     
                        call pushPos(
     &                       f(ssv)%nrdim,
     &                       f(ssv)%pos,
     &                       f(ssv)%var(varid)%nrlatlon,
     &                       f(ssv)%var(varid)%indlatlon,
     &                       f(ssv)%var(varid)%poslatlon)
C     
C     ************************************************************
C     initialise rest of ssv-pos
C     ************************************************************
C     
                        do ii=1,f(ssv)%var(varid)%nrrest
                           f(ssv)%var(varid)%posrest(ii)=1
                        end do
C     
C     initialise the rest of ssv-pos
C     
                        call pullPos(
     &                       f(ssv)%nrdim,
     &                       f(ssv)%pos,
     &                       f(ssv)%var(varid)%nrrest,
     &                       f(ssv)%var(varid)%indrest,
     &                       f(ssv)%var(varid)%posrest)
C     
                        idone=.false.
C     
C     loop over rest of ssv-rest-pos
C     
                        do while (.not.idone) ! indrest loop over ssv pos
C     
C     get ssv-pos from ssv-rest-pos
C     
                           call pullPos(
     &                          f(ssv)%nrdim,
     &                          f(ssv)%pos,
     &                          f(ssv)%var(varid)%nrrest,
     &                          f(ssv)%var(varid)%indrest,
     &                          f(ssv)%var(varid)%posrest)
C     
C     make new-pos and new-loc
C     
                           call pushPos(
     &                          f(ssp)%nrdim,
     &                          f(ssp)%pos,
     &                       f(ssp)%nrlatlon,
     &                          f(ssp)%indlatlon,
     &                          pos)
                           call pushPos(
     &                          f(ssv)%nrdim,
     &                          f(ssv)%pos,
     &                          f(ssv)%var(varid)%nrrest,
     &                          f(ssv)%var(varid)%indrest,
     &                          pos(f(ssp)%nrlatlon+1))
                           call pushLoc(
     &                          nrdim,
     &                          lim,
     &                          pos,
     &                          loc)
C     
C     get ssv-var-pos and ssv-var-loc
C     
                           call pushPos(
     &                          f(ssv)%nrdim,
     &                          f(ssv)%pos,
     &                          f(ssv)%var(varid)%nrdim,
     &                          f(ssv)%var(varid)%ind,
     &                          f(ssv)%var(varid)%pos)
                           call pushLoc(
     &                          f(ssv)%var(varid)%nrdim,
     &                          f(ssv)%var(varid)%lim,
     &                          f(ssv)%var(varid)%pos,
     &                          f(ssv)%var(varid)%loc)
C     
                           if (f(ssv)%var(varid)%loc .lt. 1.or.
     &                          f(ssv)%var(varid)%loc .gt. 
     &                          f(ssv)%var(varid)%len) then
                              write(*,*)myname,'Invalid ssv-loc'
                              do ii=1,f(ssv)%var(varid)%nrdim
                                 write(*,*) 
     &                                f(ssv)%var(varid)%pos(ii),
     &                                f(ssv)%var(varid)%lim(ii)
                              end do
                              write(*,*)myname,'Global:'
                              do ii=1,f(ssv)%nrdim
                                 write(*,*) 
     &                                f(ssv)%pos(ii),
     &                                f(ssv)%lim(ii)
                              end do
                           end if
C     
C     save field in global array
C     
                           if (pp.eq.1) then
                              if (type.eq.nf_char) then
!     can not interpolate character strings..                                 
                                 fc(loc)=f(ssv)%var(varid)%fc(
     &                                f(ssv)%var(varid)%loc)
                              else if (type.eq.nf_int1) then
                                 f1(loc)=f(ssv)%var(varid)%f1(
     &                                f(ssv)%var(varid)%loc)*wg(pp)
                              else if (type.eq.nf_int2) then
                                 f2(loc)=f(ssv)%var(varid)%f2(
     &                                f(ssv)%var(varid)%loc)*wg(pp)
                              else if (type.eq.nf_int) then
                                 f4(loc)=f(ssv)%var(varid)%f4(
     &                                f(ssv)%var(varid)%loc)*wg(pp)
                              else if (type.eq.nf_real) then
                                 fr(loc)=f(ssv)%var(varid)%fr(
     &                                f(ssv)%var(varid)%loc)*wg(pp)
                              else if (type.eq.nf_double) then
                                 fd(loc)=f(ssv)%var(varid)%fd(
     &                                f(ssv)%var(varid)%loc)*wg(pp)
                              end if
                           else 
                              if (type.eq.nf_char) then
!     can not interpolate character strings..                                 
                              else if (type.eq.nf_int1) then
                                 f1(loc)=f1(loc)+f(ssv)%var(varid)%f1(
     &                                f(ssv)%var(varid)%loc)*wg(pp)
                              else if (type.eq.nf_int2) then
                                 f2(loc)=f2(loc)+f(ssv)%var(varid)%f2(
     &                                f(ssv)%var(varid)%loc)*wg(pp)
                              else if (type.eq.nf_int) then
                                 f4(loc)=f4(loc)+f(ssv)%var(varid)%f4(
     &                                f(ssv)%var(varid)%loc)*wg(pp)
                              else if (type.eq.nf_real) then
                                 fr(loc)=fr(loc)+f(ssv)%var(varid)%fr(
     &                                f(ssv)%var(varid)%loc)*wg(pp)
                              else if (type.eq.nf_double) then
                                 fd(loc)=fd(loc)+f(ssv)%var(varid)%fd(
     &                                f(ssv)%var(varid)%loc)*wg(pp)
                              end if
                           end if
C     
C     increment ssv-rest-pos
C     
                           idone=increment(1,f(ssv)%var(varid)%nrrest,
     &                          f(ssv)%var(varid)%limrest,
     &                          f(ssv)%var(varid)%posrest,
     &                          irc)
                           if (irc.ne.0) then
                              write(*,*)myname,
     &                             'Error return from INCREMENT.',irc
                              return
                           end if
C     write(*,*) myname,'Incrementing SSV:',idone,
C     &                 (f(ssp)%var(varid)%posrest(ii),ii=1,
C     &                 f(ssv)%var(varid)%nrrest)
                        end do  ! idone
                     end if
C     
C     increment ssp-latlon-position
C     
                     odone=increment(1,f(ssp)%nrlatlon,
     &                    f(ssp)%limlatlon,
     &                    f(ssp)%poslatlon,
     &                    irc)
                     if (irc.ne.0) then
                        write(*,*)myname,
     &                       'Error return from INCREMENT.',irc
                        return
                     end if
                     
C     write(*,*) myname,'Incrementing SSP:',odone
C     do ii=1,f(ssp)%nrlatlon
C     write(*,*) ii,f(ssp)%poslatlon(ii)
C     end do
                     
                  end do        ! pp (corners of interpolation grid)
               end do           ! odone
C     
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C     write to file
               lenvi=length(f(ss)%var(varid)%var250,250,10)
               write(*,*)myname,'Writing field: ',
     &              f(ss)%var(varid)%var250(1:lenvi)
C     
               if (type.eq.nf_char) then
                  ret= NF_PUT_VARA_TEXT(NCID,gvar,
     &                 start, lim,fc)
               else if (type.eq.nf_int1) then
                  ret= NF_PUT_VARA_INT1(NCID,gvar,
     &                 start, lim,f1)
               else if (type.eq.nf_int2) then
                  ret= NF_PUT_VARA_INT2(NCID,gvar,
     &                 start, lim,f2)
               else if (type.eq.nf_int) then
                  ret= NF_PUT_VARA_INT(NCID,gvar,
     &                 start, lim,f4)
               else if (type.eq.nf_real) then
                  ret= NF_PUT_VARA_REAL(NCID,gvar,
     &                 start, lim,fr)
               else if (type.eq.nf_double) then
                  ret= NF_PUT_VARA_DOUBLE(NCID,gvar,
     &                 start, lim,fd)
               else
                  write(*,*)myname,'Unknown variable type:',
     &                 f(ssv)%var(varid)%type
                  irc=919
                  return
               end if
               if (ret .ne. NF_NOERR) then
                  write(*,*) myname,"ERROR from NF_PUT_VARA_*:",
     $                 nf_strerror(ret)
                  irc=812
                  return
               end if
            end if
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

            deallocate(start,loclim,stat=irc)

         end do                 ! varid
      end do

      write(*,*)myname,'Closing file.'
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C     save close etc... etc...
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      ret=NF_CLOSE(ncid)        ! end definitions: leave define mode
      if (ret .ne. NF_NOERR) then
         write(*,*) myname,"ERROR from NF_CLOSE:",
     $        nf_strerror(ret),ncid
         irc=790
         return
      end if

      if (allocated(gvar250)) deallocate(gvar250,stat=irc)
      if (allocated(gvardim)) deallocate(gvardim,stat=irc)
      if (allocated(gdim250)) deallocate(gdim250,stat=irc)
C     if (allocated()) deallocate(,stat=irc)
C     if (allocated()) deallocate(,stat=irc)
C     if (allocated()) deallocate(,stat=irc)
C     if (allocated()) deallocate(,stat=irc)
C     if (allocated()) deallocate(,stat=irc)
C     if (allocated()) deallocate(,stat=irc)
C     if (allocated()) deallocate(,stat=irc)
C     if (allocated()) deallocate(,stat=irc)
C     if (allocated()) deallocate(,stat=irc)
C     if (allocated()) deallocate(,stat=irc)
C     if (allocated()) deallocate(,stat=irc)
C     if (allocated()) deallocate(,stat=irc)

      return
      contains

C     create one pos-vector from an other pos-vector
      subroutine pushPos(nrdim,pos, ! copy pos to posx
     &     nrdimx,indx,posx)
      implicit none
      integer nrdim             ! number of dimension
      integer pos(nrdim)        ! position
      integer nrdimx            ! number of mapped dimension
      integer indx(nrdimx)      ! dimension index
      integer posx(nrdimx)      ! mapped position
      integer ii
      do ii=1,nrdimx
         if (indx(ii).ne.0) then
            posx(ii)=pos(indx(ii))
         end if
      end do
      return
      end subroutine pushPos
      subroutine pullPos(nrdim,pos, ! copy posx to pos
     &     nrdimx,indx,posx)
      implicit none
      integer nrdim             ! number of dimension
      integer pos(nrdim)         ! position
      integer nrdimx            ! number of mapped dimension
      integer indx(nrdimx)      ! dimension index
      integer posx(nrdimx)      ! mapped position
      integer ii
      do ii=1,nrdimx
         if (indx(ii).ne.0) then
            pos(indx(ii))=posx(ii)
         end if
      end do
      return
      end subroutine pullPos

C     create pos-vector from loc-scalar
      subroutine pullLoc(nr,lim,pos,loc)
      implicit none
      integer loc
      integer nr
      integer lim(nr)
      integer pos(nr)
      if (nr.gt.0) then
         r=loc
         do ii=1,nr
C         do ii=nr,1,-1
            if (lim(ii).gt.0.and.r.ge.0) then
               x=int((r-1)/lim(ii))
               p=x*lim(ii)
               pos(ii)=r-p
               r=x+1
            else
               pos(ii)=0
            end if
         end do
      end if
      return
      end subroutine pullLoc

C     get loc-scalar from pos-vector
      subroutine  pushLoc(nr,lim,pos,loc)
      implicit none
      integer loc
      integer nr
      integer lim(nr)
      integer pos(nr)
      if (nr.gt.0) then
         loc=0.0D0

        do kk=nr,1,-1
C         do kk=1,nr
            loc=loc*lim(kk) + (pos(kk)-1)
         end do
         loc=loc+1
      else
         loc=1
      end if
      return
      end subroutine pushLoc


C     increment count
      logical function increment(st,nr,lim,pos,irc)
      implicit none
      integer st
      integer nr
      integer lim(nr)
      integer pos(nr)
      integer irc
      logical bdone
      integer jj
      increment=.false.
      bdone=.false.
      jj=st-1
      do while (.not.bdone)
         jj=jj+1
         if (jj.le.nr) then
            pos(jj)=pos(jj)+1
            if (pos(jj).gt.lim(jj)) then
               pos(jj)=1
            else
               bdone=.true.
            end if
         else
            bdone=.true.
            increment=.true.
         end if
      end do
      return
      end function increment

      character*250 function getVar250(
     &     nrdim,dim250,var250,varnrdim,vardim)
C     Return readable variable with dimensions...
      integer nrdim             ! number of available dimensions
      character*250 dim250(nrdim) ! dimension names
      character*250 var250      ! list of variables
      integer varnrdim
      integer vardim(varnrdim)
C
      character*250 match250
      integer ii,lenm,length
      external length
C     
      match250=var250
      call chop0(match250,250)
      lenm=length(match250,250,10)
      match250=match250(1:lenm)//'('
      lenm=lenm+1
      do ii=1,varnrdim
         match250=match250(1:lenm)//
     &        dim250(vardim(ii))
         call chop0(match250,250)
         lenm=length(match250,250,10)
         if (ii.ne.varnrdim) then
            match250=match250(1:lenm)//','
            lenm=lenm+1
         end if
      end do
      match250=match250(1:lenm)//')'
      call chop0(match250,250)
      lenm=length(match250,250,10)
      getVar250=match250
      return
      end function getVar250
C
      character*6 function gettype(type)
      integer type
      if (type.eq.nf_char) then
         gettype="char  "
      elseif (type.eq.nf_int1) then
         gettype="int1  "
      elseif (type.eq.nf_int2) then
         gettype="int2  "
      elseif (type.eq.nf_int) then
         gettype="int   "
      elseif (type.eq.nf_real) then
         gettype="real  "
      elseif (type.eq.nf_double) then
         gettype="double"
      else
         gettype="any   "
      end if
      return
      end function gettype

C
C     get distance along great circle in degrees
      real function getDist(LATA,LONA,LATB,LONB)
      IMPLICIT NONE
      SAVE
      REAL   LONA,LATA,LONB,LATB
      REAL   CDIFF
      real sindeg, cosdeg,acosdeg
      external sindeg,cosdeg,acosdeg
      CHARACTER*8 MYNAME 
      DATA MYNAME /'DIST'/
      CDIFF=SINDEG(LATA)*SINDEG(LATB)+
     &     COSDEG(LATA)*COSDEG(LATB)*COSDEG(LONA-LONB)
      CDIFF=MAX(-1.0D0,MIN(1.0D0,CDIFF)) ! handle truncation errors
      getDist=ABS(ACOSDEG(CDIFF))
      RETURN
      END function getDist
      real function xycross(x1,y1,x2,y2,x3,y3)
      real :: x1,y1,x2,y2,x3,y3
      real :: dx1,dy1,dx2,dy2
      dx1=x3-x1
      dy1=y3-y1
      dx2=x2-x1
      dy2=y2-y1
      xycross = dx1 * dy2 - dx2 * dy1
      end function xycross

      subroutine rotpol(zlon,zlat,zangu,plon,plat)
! finds rotated pole for spherical projection
      implicit none
      real :: zlon   ! longitude of position (degrees)
      real :: zlat   ! latitude of position (degrees)
      real :: zangu  ! angle of rotation (degrees)
      real :: plon   ! longitude of pole (degrees)
      real :: plat   ! latitude of pole (degrees)
!
      real :: alpha, beta, dlon, sa, ca, sz, cz,sbeta,sgamma
      alpha=zangu
!
!     find angle new equator makes with old equator (beta = 90 - declination of pole)
!
      sa=sindeg(alpha)
      ca=cosdeg(alpha)
      sz=sindeg(zlat)
      cz=cosdeg(zlat)
      beta=acosdeg(ca*cz)
      sbeta=sindeg(beta)
      plat=90.0D0-beta
!
!     find angle between meridian and new equator intersects old equator (reciprocal of pole longitude)
!     sin(zlat)=sin(dlon)/sin(alpha)
!     dlon=asin(sin(zlat)*sin(alpha))
      sgamma=cz*sa/sbeta
      dlon=acosdeg(-ca*cosdeg(asindeg(sgamma)))
      if (zangu.lt.0.0D0) then
         plon=zlon+dlon
      else
         plon=zlon-dlon
      end if
      write(*,*) 'ROTPOL',dlon,beta,sbeta,plon,plat,zlon,zlat
!
      return
      end subroutine rotpol
 


      end subroutine mncll
