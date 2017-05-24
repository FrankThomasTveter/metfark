#__file: 'obs_clearFileStack.F90' 0100664    **DO NOT DELETE**
subroutine obs_clearfilestack(sid, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "clearFileStack"
  !write(*,*) myname, 'Entering.',irc,sid,varname
  call observation_stackclear(sid,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_stackclear.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_clearfilestack
#__file: 'obs_clearTargets.F90' 0100664    **DO NOT DELETE**
subroutine obs_clearTargetStack(sid, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "obs_clearTarget"
  !write(*,*) myname, 'Entering.',irc
  call observation_clearTargetStack(sid,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_clearTarget.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_clearTargetStack
#__file: 'obs_closeSession.F90' 0100664    **DO NOT DELETE**
subroutine obs_closesession(sid, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "obs_closesession"
  !write(*,*) myname,'Entering.',irc
  call observation_closesession(sid,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_closeSession.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.'
  return
end subroutine obs_closeSession
#__file: 'observations.F90' 0100664    **DO NOT DELETE**
module observations
  IMPLICIT NONE
  !
  ! Global constants
  !
  CHARACTER(LEN=50)               :: blank50 = ''
  character*1 :: sep = "|"
  logical                         :: bdeb=.false.
  !
  ! bufrdc PARAMETERS
  integer,parameter :: JSUP   =       9
  integer,parameter :: JSEC0  =       3
  integer,parameter :: JSEC1  =      40
  integer,parameter :: JSEC2  =    4096
  integer,parameter :: JSEC3  =       4
  integer,parameter :: JSEC4  =       2
  integer,parameter :: JELEM  =  320000
  integer,parameter :: JBUFL  =  512000
#ifdef JBPW_64
  integer,parameter :: JBPW   =      64
#else
  integer,parameter :: JBPW   =      32
#endif
  integer,parameter :: JTAB   =    3000
  integer,parameter :: JCTAB  =    3000
  integer,parameter :: JCTST  =    9000
  integer,parameter :: JCTEXT =    9000
  integer,parameter :: JKEY   =      46 
  integer,parameter :: JTMAX  =      10
  integer,parameter :: JTCLAS =      64
  integer,parameter :: JTEL   =     255
  integer,parameter :: KELEM  =   400000
  integer,parameter :: KVALS  = 4096000
  !
  integer :: nsubset=0           ! number of subsets in file
  integer :: isubset=1           ! current subset
  integer :: nitem = 0           ! number of items in a subset
  integer :: irep=0
  integer :: NBYTPW
  integer :: kbufl
  integer :: ktdlen
  integer :: KEL
  integer :: ktdexl
  integer :: KBUFF(JBUFL)
  integer :: KSUP(JSUP)
  integer :: KSEC0(JSEC0)
  integer :: KSEC1(JSEC1)
  integer :: KSEC2(JSEC2)
  integer :: KSEC3(JSEC3)
  integer :: KSEC4(JSEC4)
  integer :: KEY(JKEY)
  REAL*8 :: VALUES(KVALS)
  integer :: KTDLST(JELEM)
  integer :: KTDEXP(JELEM)
  CHARACTER*64 ::CNAMES(KELEM)
  CHARACTER*24 ::CUNITS(KELEM)
  CHARACTER*80 ::CVALS(KELEM)
  REAL*8 :: RVIND=1.7D38
  integer :: nvind=2147483647
  integer*8 :: unit
  logical :: fopen=.false. ! is file open
  !
  ! category used for counting observations categories in file
  !
  type :: subCategory
     integer :: subcategory = 0
     integer :: cnt = 0
     integer :: ktdexl = 0
     integer, allocatable :: ktdexp(:)
     character*64, allocatable :: cnames(:)
     character*24, allocatable :: cunits(:)
     type(subCategory), pointer :: prev
     type(subCategory), pointer :: next
  end type subCategory
  !
  type :: mainCategory
     integer :: category = 0
     integer :: cnt = 0
     type(subCategory) :: firstSubCategory
     type(subCategory) :: lastSubCategory
     integer :: nsub=0
     type(mainCategory), pointer :: prev
     type(mainCategory), pointer :: next
  end type mainCategory
  !
  ! BUFR FILE STACK
  !
  type :: file
     character*250  :: fn250 = ""          ! file name
     integer        :: lenf=0              ! length of file name string
     character*250  :: tablepath = ""      ! table path
     logical        :: ltset = .false.     ! is sort tstart available
     real           :: tstart              ! sorting tstart (first observation index)
     real           :: tend                ! last observation index
     integer        :: nmessage=0          ! number of BUFR messages in file
     integer        :: nsubset=0           ! total number of subsets in file
     integer        :: nitem = 0           ! total number of items in file
     type(mainCategory) :: firstCategory
     type(mainCategory) :: lastCategory
     integer :: ncat=0
     integer :: nsub=0
     type(file), pointer :: prev => null() ! linked list
     type(file), pointer :: next => null() ! linked list
  end type file
  !
  type :: filePointer
     type(file), pointer :: pointer => null()
  end type filePointer
  !
  ! Target item
  !   
  type :: target
     character*80 :: trg80      ! target name
     character*250 :: pos250      ! target name
     character*80 :: descr80      ! target name
     character*250 :: info250      ! target name
     character*80 :: min80      ! target name
     character*80 :: max80      ! target name
     integer :: seq
     integer :: descr
     logical :: lval(3)
     real    :: minval,maxval
     type(target), pointer :: prev => null()   ! linked list
     type(target), pointer :: next => null()   ! linked list
  end type target
  !
  type :: targetPointer
     type(target), pointer :: pointer => null()
  end type targetPointer
  !
  ! report
  !
  type :: reportItem
     character*250 :: desc250 ! description
     type(reportItem), pointer :: prev => null()   ! linked list
     type(reportItem), pointer :: next => null()   ! linked list
  end type reportItem
  !
  type :: report
     type(reportItem), pointer :: firstItem => null()    ! linked list start
     type(reportItem), pointer :: lastItem => null()     ! linked list end
     type(reportItem), pointer :: currentItem => null()  ! current pointer
     integer :: nitem = 0                           ! number of items
     type(report), pointer :: prev => null()   ! linked list
     type(report), pointer :: next => null()   ! linked list
  end type report
  !
  ! BUFR arrays:::  WARNING: WILL PROBABLY USE ALMOST ALL OF YOUR COMPUTER MEMORY!!!!
  !
  ! SESSION VARIABLES
  !
  type :: session
     integer                         :: bid
     CHARACTER(LEN=250)              :: fn250
     CHARACTER(LEN=250)              :: tablepath=""
     !
     ! STACK
     !
     type(file), pointer :: firstFile => null()   ! linked list start
     type(file), pointer :: lastFile => null()    ! linked list end
     type(file), pointer :: currentFile => null()
     type(file), pointer :: nextFile => null()
     type(filePointer), pointer   :: fileStack(:) => null() ! array of the stack elements
     real, allocatable            :: fileStackSort(:)
     integer, allocatable         :: fileStackInd(:)
     integer :: nFileIndexes = 0              ! total number of files on the stack
     integer :: nFileSortIndexes = 0          ! number of file indexes on the stack
     integer :: newnFileSortIndexes = 0       ! new number of file indexes on the stack
     integer :: currentFileSortIndex = 0      ! current stack index element
     integer :: currentFileIndex = 0          ! current stack element
     logical :: stackReady =.false.           ! are sorted data ready for use?
     !
     ! data selection
     !
     logical        :: ltset = .false.   ! is sort tstart available
     real           :: tstart              ! sorting tstart (first observation index)
     real           :: tend                ! last observation index
     !
     ! TARGET
     !
     type(target), pointer        :: firstTarget => null()   ! linked list start
     type(target), pointer        :: lastTarget => null()    ! linked list end
     integer :: ttarget=0                               ! number of targets
     integer :: mvar=0                                  ! max number of variables in a target
     integer :: category
     integer :: subCategory
     !
     logical :: ignmis=.false.
     logical :: ignuni=.false.
     logical :: ignden=.false.
     logical :: ignder=.false.
     logical :: ignval=.false.
     logical :: ignsec=.false.
     logical :: ignarr=.false.
     !
     ! REPORTS
     !
     type(report), pointer :: firstReport => null()    ! linked list start
     type(report), pointer :: lastReport => null()     ! linked list end
     type(report), pointer :: currentReport => null()  ! current pointer
     type(report), pointer :: nextReport => null()  ! current pointer
     integer :: nsubset=0               ! number of reports
     logical :: reportsReady=.false.    ! are reported data ready for use
     !
     type(session), pointer :: prev => null()         ! linked list
     type(session), pointer :: next => null()         ! linked list
  end type session

  type :: table
     integer :: code
     integer :: maxnn = 0
     integer, allocatable :: subcodes(:)
     character*250, allocatable :: values(:)
     integer :: nn = 0
     integer, allocatable :: index(:)
  end type table
  !
  type :: codeTable
     integer :: maxnn = 0
     integer, allocatable :: codes(:)
     type(table), allocatable :: tables(:)
     integer :: nn = 0
     integer, allocatable :: index(:)
  end type codeTable
  !
  character*250 :: c250 = ""
  type(codeTable), target :: ctable
  logical :: ctableInit=.false.
  !
  integer :: maxid=0 ! session counter
  type(session), pointer :: firstSession => null()   ! linked list start
  type(session), pointer :: lastSession => null()    ! linked list end
  !
CONTAINS
  !
  !###############################################################################
  ! SESSION ROUTINES
  !###############################################################################
  !
  subroutine observation_opensession(bid,crc250,irc)
    integer :: bid
    character*250 :: crc250
    integer :: irc
    type(session),pointer :: newSession !  new session
    character*22 :: myname = "observation_opensession"
    write(*,*) "*********************************************************************************************"
    write(*,*) "***This software is ONLY permitted used by staff at the Norwegian Meteorological Institute***"
    write(*,*) "*********************************************************************************************"
    !write(*,*)myname,'Entering.'
    if (.not.associated(firstSession)) then
       allocate(firstSession, lastSession,stat=irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250,"Unable to allocate 'firstSession/lastSession'.")
          call observation_errorappend(crc250,"\n")
          return
       end if
       firstSession%next => lastSession
       lastSession%prev => firstSession
       !
    end if
    allocate(newSession,stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Unable to allocate 'new session'.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    maxid=maxid+1
    newSession%bid=maxid
    newSession%prev => lastSession%prev
    newSession%next => lastSession
    newSession%prev%next => newSession
    newSession%next%prev => newSession
    bid = newSession%bid
    ! stack
    allocate(newSession%firstFile,newSession%lastFile, stat=irc) ! 
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Unable to allocate &
            & 'newSession%firstFile/newSession%lastFile'.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    newSession%firstFile%next => newSession%lastFile
    newSession%lastFile%prev => newSession%firstFile
    ! reports
    allocate(newSession%firstReport,newSession%lastReport,stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Unable to allocate first/lastReport.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    newSession%firstReport%next => newSession%lastReport
    newSession%lastReport%prev => newSession%firstReport
    newSession%nsubset = 0
    ! targets
    call observation_targetinit(newSession,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Error return from observation_targetInit.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    !
    !write(*,*)myname,' Done.'
    return
  end subroutine observation_opensession

  subroutine observation_getSession(css,bid,crc250,irc)
    type(session), pointer :: css !  current session
    integer :: bid
    character*250 :: crc250
    integer :: irc
    character*22 :: myname = "observation_getSession"
    if(bdeb)write(*,*)myname,' Entering.',irc,bid
    css => firstSession%next
    do while ( .not.associated(css,target=lastSession))
       if (css%bid .eq. bid) then
          if(bdeb)write(*,*)myname,' Done.',irc,bid
          return
       end if
       css=>css%next
    end do
    nullify(css)
    irc=342
    call observation_errorappend(crc250,myname)
    call observation_errorappend(crc250,"Invalid session id:")
    call observation_errorappendi(crc250,bid)
    call observation_errorappend(crc250,"\n")
    if(bdeb)write(*,*)myname,'Error.',irc,bid
    return
  end subroutine observation_getSession

  subroutine observation_closeSession(bid,crc250,irc)
    integer :: bid
    character*250 :: crc250
    integer :: irc
    type(session), pointer :: css !  current session
    integer :: ii
    character*22 :: myname = "observation_closeSession"
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if (associated(css)) then
       ! remove reportdata
       call observation_clearReports(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_clearReports.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       deallocate(css%firstReport,css%lastReport,stat=irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Unable to deallocate first/lastreport.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       ! remove file-stack
       if (associated(css%filestack)) then
          do ii=1,size(css%filestack)
             if (associated(css%filestack(ii)%pointer)) then
                nullify(css%filestack(ii)%pointer)
             end if
          end do
          deallocate(css%filestack)
       end if
       if (allocated(css%filestacksort)) deallocate(css%filestacksort)
       if (allocated(css%filestackind)) deallocate(css%filestackind)
       call observation_removeFiles(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from removeFiles.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       deallocate(css%firstFile,css%lastFile)
       ! clear target 
       call observation_removeTarget(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_removeTarget.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       ! 
       css%prev%next => css%next
       css%next%prev => css%prev
       deallocate(css)
    end if
  end subroutine observation_closeSession
  !
  ! make cache file
  !
  subroutine observation_makeCache(bid,path250,crc250,irc)
    integer :: bid
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(file), pointer :: currentFile !  current file
    type(mainCategory), pointer :: currentCat !  current file
    type(subCategory), pointer :: currentSub !  current file
    type(session), pointer :: css !  current session
    integer, external :: length,ftunit
    integer :: lenp,unitr
    character*250 :: buff250, str250
    character*22 :: myname = "observation_makeCache"
    if(bdeb)write(*,*) myname,' Entering.',irc,bid
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    call chop0(path250,250)
    lenp=length(path250,250,20)
    if(bdeb)write(*,*)myname,' Path.',bid,path250(1:lenp)
    ! open file
    unitr=ftunit(irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," no free unit number for:"//path250(1:lenp))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    lenp=length(path250,250,10)
    open ( unit=unitr, status="unknown", form="formatted", &
         &        access="sequential", &
         &        iostat=irc, file=path250(1:lenp) )
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," unable to open:"//path250(1:lenp))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! write number of files: css%nFileIndexes
    if(bdeb)write(*,*) myname,' Files:',css%nFileIndexes
    write(unitr,'(I0)',iostat=irc) css%nFileIndexes
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," unable to write to:"//path250(1:lenp))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! loop over file stack
    currentFile=>css%firstFile%next
    do while (.not.associated(currentFile,target=css%lastFile))
          write(unitr,'(L1,2(X,F27.10),4(X,I0),X,A)',iostat=irc) currentFile%ltset,&
               & currentFile%tstart,currentFile%tend,&
               & currentFile%nmessage,currentFile%ncat,currentFile%nsub,&
               & currentFile%lenf,currentFile%fn250(1:currentFile%lenf)
       ! write category summary
       currentCat=>currentFile%firstCategory%next
       do while (.not.associated(currentCat,target=currentFile%lastCategory)) 
          write(unitr,'(3(X,I0))',iostat=irc) currentCat%category,&
               & currentCat%cnt,currentCat%nsub
          currentSub=> currentCat%firstSubCategory%next
          do while (.not.associated(currentSub,target=currentCat%lastSubCategory)) 
             write(unitr,'(2(X,I0))',iostat=irc) currentSub%subcategory,currentSub%cnt
             currentSub=>currentSub%next
          end do
          currentCat=>currentCat%next
       end do
       currentFile=>currentFile%next
    end do
    ! close file
    close(unitr,iostat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," unable to close:"//path250(1:lenp))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if(bdeb)write(*,*)myname,' Done.',irc,bid
  end subroutine observation_makeCache
  !
  ! load cache file
  !
  subroutine observation_loadCache(bid,path250,crc250,irc)
    integer :: bid
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(file),pointer :: newFile
    type(mainCategory), pointer :: newCat !  current file
    type(subCategory), pointer :: newSub !  current file
    type(session), pointer :: css !  current session
    integer, external :: length
    integer :: lenp,lenf,lenb,ii,jj,kk,opos,pos,unitr
    character*250 :: buff250
    character*22 :: myname = "observation_loadCache"
    if(bdeb)write(*,*) myname,' Entering.',irc,bid
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    call chop0(path250,250)
    lenp=length(path250,250,20)
    if(bdeb)write(*,*)myname,' Path.',bid,path250(1:lenp)
    ! clear existing cache
    css%stackReady=.false.
    call observation_removeFiles(css,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from removeFiles.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! open cache file
    open ( unit=unitr, status="old", form="formatted", &
         &        access="sequential", &
         &        iostat=irc, file=path250(1:lenp) )
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," unable to open:"//path250(1:lenp))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! write number of files: css%nFileIndexes
    read(unitr,'(A)',iostat=irc) buff250
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," unable to read:"//path250(1:lenp))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    read(buff250,*,iostat=irc) css%nFileIndexes
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," unable to interpret:"//path250(1:lenp))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! loop through cache file
    do ii=1,css%nFileIndexes
       allocate(newFile,stat=irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Unable to allocate new File item.")
          call observation_errorappend(crc250,"\n")
          return
       end if
       newFile%firstCategory%next => newFile%lastCategory
       newFile%lastCategory%prev => newFile%firstCategory
       css%stackReady=.false.
       newFile%prev => css%lastFile%prev
       newFile%next => css%lastFile
       newFile%prev%next => newFile
       newFile%next%prev => newFile
       css%currentFile=>newFile
       !
       read(unitr,'(A)',iostat=irc) buff250
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," unable to interpret(2):"//path250(1:lenp))
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       call chop0(buff250,250)
       pos=0
       opos=pos
       call findDelimiter(buff250," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%ltset
       opos=pos
       call findDelimiter(buff250," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%tstart
       opos=pos
       call findDelimiter(buff250," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%tend
       opos=pos
       call findDelimiter(buff250," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%nmessage
       opos=pos
       call findDelimiter(buff250," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%ncat
       opos=pos
       call findDelimiter(buff250," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%nsub
       opos=pos
       call findDelimiter(buff250," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%lenf
       opos=pos
       pos=length(buff250,250,10)
       newFile%fn250=buff250(opos+1:pos)
       call chop0(newFile%fn250,250)
       lenf=length(newFile%fn250,250,10)
       !
       do jj=1,newFile%ncat
          allocate(newCat,stat=irc)
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Unable to allocate new Cat item.")
             call observation_errorappend(crc250,"\n")
             return
          end if
          newCat%next => newFile%lastCategory
          newCat%prev => newFile%lastCategory%prev
          newFile%lastCategory%prev%next => newCat
          newFile%lastCategory%prev => newCat
          newCat%lastSubCategory%prev => newCat%firstSubCategory
          newCat%firstSubCategory%next => newCat%lastSubCategory
          read(unitr,'(A)',iostat=irc) buff250
          call chop0(buff250,250)
          pos=0
          opos=pos
          call findDelimiter(buff250," ",pos)
          read(buff250(opos+1:pos-1),*,iostat=irc)newCat%category
          opos=pos
          call findDelimiter(buff250," ",pos)
          read(buff250(opos+1:pos-1),*,iostat=irc)newCat%cnt
          opos=pos
          pos=length(buff250,250,10)
          read(buff250(opos+1:pos),*,iostat=irc)newCat%nsub
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," unable to get nsub from:"//buff250(opos+1:pos-1))
             call observation_errorappendi(crc250,irc)
             call observation_errorappend(crc250,"\n")
             return
          end if
          do kk=1,newCat%nsub
             allocate(newSub,stat=irc)
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Unable to allocate new Subcat item.")
                call observation_errorappend(crc250,"\n")
                return
             end if
             newSub%next => newCat%lastSubCategory
             newSub%prev => newCat%lastSubCategory%prev
             newCat%lastSubCategory%prev%next => newSub
             newCat%lastSubCategory%prev => newSub
             read(unitr,'(A)',iostat=irc) buff250
             call chop0(buff250,250)
             pos=0
             opos=pos
             call findDelimiter(buff250," ",pos)
             read(buff250(opos+1:pos-1),*,iostat=irc)newSub%subcategory
             opos=pos
             pos=length(buff250,250,10)
             read(buff250(opos+1:pos),*,iostat=irc)newSub%cnt
          end do
       end do

    end do
    ! close file
    close(unitr,iostat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," unable to close:"//path250(1:lenp))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if(bdeb)write(*,*)myname,' Done.',irc,bid
  end subroutine observation_loadCache
  !
  !###############################################################################
  !STACK ROUTINES
  !###############################################################################
  !
  ! clear the BUFR STACK
  !
  subroutine observation_stackclear(bid,crc250,irc)
    integer :: bid
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    integer :: lens
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_stackclear"
    if(bdeb)write(*,*)myname,' Entering.'
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! mark as prepared
    css%stackReady=.false.
    !
    if(bdeb)write(*,*)myname,' Removing files.'
    call observation_removeFiles(css,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from observation_removeFiles.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if (css%nFileIndexes .ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," System error A:")
       call observation_errorappendi(crc250,css%nFileIndexes)
       call observation_errorappend(crc250,"\n")
       irc=940
       return
    end if
    if(bdeb)write(*,*)myname,' Done.'
  end subroutine observation_stackclear
  !
  ! set the bufr table path
  !
  subroutine observation_setTablePath(bid,path250,crc250,irc)
    integer :: bid
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(file), pointer :: currentFile => null()
    type(file), pointer :: stackNext => null()
    integer, external :: length
    integer :: lens
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_setTablePath"
    if(bdeb)write(*,*)myname,' Entering.'
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    css%tablepath=path250
    call chop0(css%tablepath,250)
    if(bdeb)write(*,*)myname,' Done.'
  end subroutine observation_setTablePath
  !
  ! set table c file name
  !
  subroutine observation_setTableC(bid,path250,crc250,irc)
    integer :: bid
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(file), pointer :: currentFile => null()
    type(file), pointer :: stackNext => null()
    integer, external :: length
    integer :: lens
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_setTableC"
    ctableInit=.false.
    c250=path250
    call chop0(c250,250)
    if(bdeb)write(*,*)myname,' Done.'
  end subroutine observation_setTableC
  !
  !
  !
  subroutine observation_setType(bid,category,subCategory,crc250,irc)
    integer :: bid
    integer :: category
    integer :: subCategory
    character*250 :: crc250
    integer :: irc
    type(file), pointer :: currentFile => null()
    type(file), pointer :: stackNext => null()
    integer, external :: length
    integer :: lens
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_setType"
    if(bdeb)write(*,*)myname,' Entering.'
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    css%category=category
    css%subCategory=subCategory
    if(bdeb)write(*,*)myname,' Done.'
  end subroutine observation_setType
  !
  ! remove item from bufr stack
  !
  subroutine observation_removeFiles (css,crc250,irc)
    type(session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    integer :: irc2
    type(file), pointer :: currentFile,nextFile
    character*22 :: myname = "observation_removeFiles "
    currentFile => css%firstFile%next
    do while (.not.associated(currentFile,target=css%lastFile))
       nextFile => currentFile%next
       if (associated(currentFile)) then
          if(bdeb)write(*,*)myname,' Removing categories.'
          call observation_clearCat(currentFile)
          if(bdeb)write(*,*)myname,' Updating inventory.'
          css%nFileIndexes = css%nFileIndexes - 1
          css%stackReady=.false.
          currentFile%next%prev => currentFile%prev
          currentFile%prev%next => currentFile%next
          nullify(currentFile%prev)
          nullify(currentFile%next)
          deallocate(currentFile) ! arrays are deallocated automatically
       end if
       currentFile => nextFile
    end do
  end subroutine observation_removeFiles
  !
  ! Add bufr-file to the BUFR STACK
  !
  subroutine observation_stackpush(bid,path250,crc250,irc)
    integer :: bid
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(file),pointer :: newFile
    logical  :: bok =.false.
    INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:) :: start, vsize, atypes
    character(len=80), allocatable, dimension(:) :: dimnames
    integer :: nvalues, tsize, ndims
    integer :: ii,jj,kk,tt
    CHARACTER(LEN=80)               :: varname
    integer :: irc2
    integer, external :: length
    integer :: lenc,leni,lenv,lens,lenp,lend
    logical :: bbok
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_stackpush"
    if(bdeb)write(*,*) myname,' Entering.',irc,bid
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    call chop0(path250,250)
    lenp=length(path250,250,20)
    if(bdeb)write(*,*)myname,' Starting.',bid,path250(1:lenp)
    ! create new stack-item
    bok=.true.
    allocate(newFile,stat=irc)
    if (irc.ne.0) then
       bok=.false.
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Unable to allocate new stack item.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    newFile%firstCategory%next => newFile%lastCategory
    newFile%lastCategory%prev => newFile%firstCategory
    if(bdeb)write(*,*)myname,'Care.',bid
    ! push onto stack
    if (bok) then
       css%nFileIndexes=css%nFileIndexes + 1
       css%stackReady=.false.
       newFile%prev => css%lastFile%prev
       newFile%next => css%lastFile
       newFile%prev%next => newFile
       newFile%next%prev => newFile
       css%currentFile=>newFile
    end if
    if(bdeb)write(*,*)myname,'Dare.',bid
    ! open file
    if (bok) then
       ! set file name...
       newFile%fn250=path250
       call chop0(newFile%fn250,250)
       newFile%lenf=length(newFile%fn250,250,20)
       newFile%tablepath=css%tablepath
       ! open file
       if(bdeb)write(*,*)myname,'Flare.',bid,bok
       call observation_scanFile(css,newFile,bok,crc250,irc)
       if(bdeb)write(*,*)myname,'Share.',bid,bok
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_scanFileSortIndexes.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    if (.not.bok) then
       if (associated(newFile)) then
          call observation_stackpop(bid,path250,crc250,irc)
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Error return from observation_stackpop.")
             call observation_errorappendi(crc250,irc)
             call observation_errorappend(crc250,"\n")
             return
          end if
       end if
    end if
    if(bdeb)write(*,*)myname,' Done.',irc,bid
  end subroutine observation_stackpush

  !
  ! Remove last bufr-file on the BUFR STACK
  !
  subroutine observation_stackpop(bid,path250,crc250,irc)
    integer :: bid
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(file), pointer :: currentFile => null()
    type(file), pointer :: prevFile => null()
    type(session), pointer :: css !  current session
    integer :: irc2
    character*22 :: myname = "observation_stackpop"
    logical :: bdone
    integer, external :: length
    integer :: lenp
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    call chop0(path250,250)
    lenp=length(path250,250,10)
    currentFile => css%lastFile%prev
    bdone=associated(currentFile,target=css%firstFile)
    do while (.not. bdone) 
       prevFile=>currentFile%prev
       if (currentFile%fn250(1:currentFile%lenf).eq.path250(1:lenp).or.lenp.eq.0) then
          css%nFileIndexes=css%nFileIndexes - 1
          css%stackReady=.false.
          currentFile%next%prev => currentFile%prev
          currentFile%prev%next => currentFile%next
          nullify(currentFile%prev)
          nullify(currentFile%next)
          deallocate(currentFile) ! arrays are deallocated automatically
          css%currentFile => prevFile
          bdone=(lenp.eq.0)
       end if
       currentFile=>prevFile
       bdone=(bdone.or.associated(currentFile,target=css%firstFile))
    end do
  end subroutine observation_stackpop

  !
  ! Peek at last bufr-file put onto the BUFR STACK
  !
  subroutine observation_stackpeeklen(bid,maxrep,crc250,irc)
    integer :: bid
    integer :: maxrep
    character*250 :: crc250
    integer :: irc
    type(file), pointer :: currentFile => null()
    integer :: ii,jj
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_stackpeeklen"
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    currentFile => css%lastFile%prev
    ! report file-name
    maxrep=1
    if (.not.associated(currentFile,target=css%firstFile)) then
       call observation_getFileReportLen(currentFile,maxrep,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_getFileReportLen.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    if(bdeb)write(*,*)myname,' Done.',maxrep
  end subroutine observation_stackpeeklen
  !
  subroutine observation_stackpeek(bid,maxrep,nrep,rep250,crc250,irc)
    integer :: bid
    integer :: maxrep
    integer :: nrep
    character*250 :: rep250(maxrep)
    character*250 :: crc250
    integer :: irc
    character*50 :: s1, s2, s3
    integer, external :: length
    integer :: len1,len2,len3,lenm,lenv,lena,lenr,lend,lens
    type(file), pointer :: currentFile => null()
    integer :: ii,jj
    character*80 :: varname
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_stackpeek"
    if(bdeb)write(*,*)myname,' Entering.'
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    currentFile => css%lastFile%prev
    ! report file-name
    nrep=0
    if (.not.associated(currentFile,target=css%firstFile)) then
       call observation_getFileReport(currentFile,maxrep,nrep,rep250,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_getFileReport.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    if(bdeb)write(*,*)myname,' Done.',maxrep,nrep
  end subroutine observation_stackpeek

  subroutine observation_getFileReportLen(currentFile,maxrep,crc250,irc)
    type(file), pointer :: currentFile
    integer :: maxrep
    character*250 :: crc250
    integer :: irc
    type(mainCategory), pointer :: cat
    type(subCategory), pointer :: sub
    character*22 :: myname = "observation_getFileReportLen"
    maxrep=2
    if (currentFile%ltset) then
       maxrep=maxrep+2
    end if
    cat => currentFile%firstCategory%next
    do while (.not.associated(cat,target=currentFile%lastCategory))
       maxrep=maxrep+1
       sub => cat%firstSubCategory%next
       do while (.not.associated(sub,target=cat%lastSubCategory))
          maxrep=maxrep+3
          maxrep=maxrep+3*sub%ktdexl
          sub => sub%next
       end do
       cat => cat%next
    end do
  end subroutine observation_getFileReportLen

  subroutine observation_getFileReport(currentFile,maxrep,nrep,rep250,crc250,irc)
    type(file), pointer :: currentFile
    integer :: maxrep
    integer :: nrep
    character*250 :: rep250(maxrep)
    character*250 :: crc250
    integer :: irc
    character*50 :: s1, s2, s3
    integer, external :: length
    integer :: len1,len2,len3,lenm,lenv,lena,lenr,lend,lens
    integer :: ii,jj
    character*80 :: varname
    type(mainCategory), pointer :: cat
    type(subCategory), pointer :: sub
    logical :: first
    character*22 :: myname = "observation_getFileReport"
    ! file name
    call chop0(currentFile%fn250,250)
    lenm=length(currentFile%fn250,250,20)
    nrep=min(maxrep,nrep+1)               ! file name  +1
    rep250(nrep)="file"//sep//"name"//sep//currentFile%fn250(1:lenm)
    write(s1,'(I12)') currentFile%nsubset; call chop0(s1,50); len1=length(s1,50,10)
    nrep=min(maxrep,nrep+1)               ! number of observations  +1
    rep250(nrep)="file"//sep//"message count"//sep//s1(1:len1)
    ! sorting variable is available
    if (currentFile%ltset) then
       write(s2,'(A)') observation_gettime(currentFile%tstart); call chop0(s2,50); len2=length(s2,50,10)
       write(s3,'(A)') observation_gettime(currentFile%tend); call chop0(s3,50); len3=length(s3,50,10)
       nrep=min(maxrep,nrep+1)          ! start time      +1
       rep250(nrep)="file"//sep//"time"//sep//"start"//sep//s2(1:len2)
       nrep=min(maxrep,nrep+1)          ! end time      +1
       rep250(nrep)="file"//sep//"time"//sep//"stop"//sep//s3(1:len3)
    end if
    cat => currentFile%firstCategory%next
    do while (.not.associated(cat,target=currentFile%lastCategory))
       nrep=min(maxrep,nrep+1)          ! cat      +1
       write(rep250(nrep),'("file",A,"type",A,I0,A,"message count",A,I0)') sep,sep,&
            & cat%category, sep,sep, cat%cnt
       first=.true.
       sub => cat%firstSubCategory%next
       do while (.not.associated(sub,target=cat%lastSubCategory))
          call observation_getType(cat%category,sub%subcategory,s2,s3,crc250,irc)
          call chop0(s2,50); len2=length(s2,50,10);call chop0(s3,50); len3=length(s3,50,10)
          if (len3.ne.0) then
             nrep=min(nrep+1,maxrep)
             write(rep250(nrep),'("file",A,"type",A,I0,A,"subtype",A,I0,A,"description",A,A)') sep,sep,&
                  & cat%category, sep,sep, sub%subcategory, sep,sep, s3(1:len3)
          end if
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'("file",A,"type",A,I0,A,"subtype",A,I0,A,"message count",A,I0)') sep,sep,&
               & cat%category, sep,sep, sub%subcategory, sep,sep, sub%cnt
          if (first.and.len2.ne.0) then
             first=.false.
             nrep=min(maxrep,nrep+1)          ! desc      +1
             write(rep250(nrep),'("file",A,"type",A,I0,A,"description",A,A)') sep,sep,&
                  & cat%category, sep,sep, s2(1:len2)
          end if

          write(*,*)myname,"Sequence:",sub%ktdexl

          do ii=1,sub%ktdexl
             nrep=min(nrep+1,maxrep)
             write(rep250(nrep),'("file",A,"type",A,I0,A,"subtype",A,I0,A,"seqno",A,I0,A,I0)') sep,sep,&
                  & cat%category, sep,sep, sub%subcategory, sep,sep, ii,sep,sub%ktdexp(ii)
             nrep=min(nrep+1,maxrep)
             write(rep250(nrep),'("file",A,"type",A,I0,A,"subtype",A,I0,A,"name",A,I0,A,A)') sep,sep,&
                  & cat%category, sep,sep, sub%subcategory, sep,sep, ii,sep,sub%cnames(ii)
             nrep=min(nrep+1,maxrep)
             write(rep250(nrep),'("file",A,"type",A,I0,A,"subtype",A,I0,A,"unit",A,I0,A,A)') sep,sep,&
                  & cat%category, sep,sep, sub%subcategory, sep,sep, ii,sep,sub%cunits(ii)
          end do
          sub => sub%next
       end do
       cat => cat%next
    end do

    !do ii=1,nrep
    !   call chop0(rep250(II),250)
    !   lenr=length(rep250(ii),250,100)
    !   write(*,*) myname,'REP:',ii,maxrep,rep250(ii)(1:lenr)
    !end do

    return
  end subroutine observation_getFileReport

  !
  !###############################################################################
  !ROUTINES FOR MAINTAINING TARGET STACK
  !###############################################################################
  !
  subroutine observation_targetinit(css,crc250,irc)
    type(session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname = "observation_targetinit"
    css%reportsReady=.false. ! we must redo report generation
    ! initialise chain
    if (.not.associated(css%firstTarget)) then
       allocate(css%firstTarget,css%lastTarget, stat=irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250,"Unable to allocate 'firstTarget/lastTarget'.")
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%firstTarget%next => css%lastTarget
       css%lastTarget%prev => css%firstTarget
       css%ttarget=0
    end if
  end subroutine observation_targetinit
  !
  ! clear the target stack
  !
  subroutine observation_clearTargetStack(sid,crc250,irc)
    integer :: sid
    character*250 :: crc250
    integer :: irc
    character*22 :: myname = "observation_clearTargetStack"
    integer :: ii, lens
    integer, external :: length
    type(session), pointer :: css !  current session
    if(bdeb)write(*,*)myname,' Entering.'
    call observation_getSession(css,sid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    css%reportsReady=.false. ! old reports are discarded...
    call observation_targetinit(css,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from targetinit.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! delete any existing Target-entries
    call observation_removeTarget(css,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from targetrmitem.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if (css%ttarget .ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," System error B:")
       call observation_errorappendi(crc250,css%ttarget)
       call observation_errorappend(crc250,"\n")
       irc=940
       return
    end if
  end subroutine observation_clearTargetStack
  !
  ! set the observation time span
  !
  !  subroutine observation_pushTarget(sid,category,subcategory,nvar,var80,crc250,irc)
  subroutine observation_pushtarget(bid,trg80,pos250,descr80,info250,&
       & min80,max80,crc250,irc)
    integer :: bid
    character*80 :: trg80      ! target name
    character*250 :: pos250      ! target name
    character*80 :: descr80      ! target name
    character*250 :: info250      ! target name
    character*80 :: min80      ! target name
    character*80 :: max80      ! target name
    character*250 :: crc250
    integer :: irc
    type(target),pointer :: newTarget
    integer :: ii,yy,mm,dd,hh,mi
    real:: sec
    integer :: lenc,lenp,lend,lens,lene
    integer, external :: length
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_pushTarget"
    integer :: pos,opos
    if(bdeb)write(*,*)myname,' Entering.'
    call observation_getSession(css,sid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    css%reportsReady=.false. ! we must redo report generation
    ! initialise Target stack
    call observation_targetinit(css,crc250,irc)
    if (irc.ne.0) then 
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from targetinit.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! create new pos-item
    allocate(newTarget,stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Unable to allocate new Target.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    newTarget%trg80=trg80
    newTarget%pos250=pos250
    newTarget%descr80=descr80
    newTarget%info250=info250
    newTarget%min80=min80
    newTarget%max80=max80
    call chop0(newTarget%pos250,250)
    call chop0(newTarget%descr80,80)
    call chop0(newTarget%info250,250)
    call chop0(newTarget%min80,80)
    call chop0(newTarget%max80,80)
    lenp=length(newTarget%pos250,250,10)
    lend=length(newTarget%descr80,80,10)
    lens=length(newTarget%min80,80,10)
    lene=length(newTarget%max80,80,10)
    read(newTarget%pos250(1:lenp),*,iostat=irc) newTarget%seq
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Unable to read seq"//newTarget%pos250(1:lenp))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    read(newTarget%descr80,*,iostat=irc) newTarget%descr
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Unable to read min"//newTarget%descr80(1:lend))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    read(newTarget%min80,*,iostat=irc) newTarget%minval
    newTarget%lval(1)=(irc.eq.0); irc=0
    read(newTarget%max80,*,iostat=irc) newTarget%maxval
    newTarget%lval(2)=(irc.eq.0); irc=0
    newTarget%lval(3)=(newTarget%lval(1).and.&
         & newTarget%lval(2).and.&
         & newTarget%minval.gt.newTarget%maxval) 
    ! push onto stack
    css%ttarget=css%ttarget + 1
    newTarget%prev => css%lastTarget%prev
    newTarget%next => css%lastTarget
    newTarget%prev%next => newTarget
    newTarget%next%prev => newTarget
    if(bdeb)write(*,*)myname,' Done.'
    return
  end subroutine observation_pushTarget
  !
  subroutine findDelimiter(var80,del,pos)
    character*80 :: var80
    character*1 :: del
    integer :: pos
    logical :: bdone
    !write(*,*)'findDelimiter entering:',pos,'"',del,'" ',var80
    pos=min(80,pos+1)
    bdone=(pos.eq.80)
    do while (.not.bdone)
       !write(*,*)'findDelimiter:',pos,' "', &
       !     & var80(pos:pos),'" ',ichar(var80(pos:pos))
       if (var80(pos:pos).eq.del.or.var80(pos:pos).eq.char(0)) then
          bdone=.true.
       else
          pos=min(80,pos+1)
          bdone=(pos.eq.80)
       end if
    end do
    !write(*,*)'findDelimiter pos:',pos
  end subroutine findDelimiter
  !
  ! remove item from MODEL POS
  !
  subroutine observation_removeTarget (css,crc250,irc)
    type(session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    type(target), pointer :: currentTarget => null()
    type(target), pointer :: nextTarget => null()
    character*22 :: myname = "observation_removeTarget "
    currentTarget => css%firstTarget%next
    do while (.not.associated(currentTarget,target=css%lastTarget))
       nextTarget => currentTarget%next
       if (associated(currentTarget)) then
          css%ttarget = css%ttarget - 1
          currentTarget%next%prev => currentTarget%prev
          currentTarget%prev%next => currentTarget%next
          nullify(currentTarget%prev)
          nullify(currentTarget%next)
          nullify(currentTarget)
       end if
       currentTarget => nextTarget
    end do
  end subroutine observation_removeTarget
  !
  ! check if current reports are valid
  !
  subroutine observation_checkTarget(css,bok,crc250,irc)
    type(session), pointer :: css
    logical:: bok
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname = "observation_checkTarget"
    type(target), pointer :: currentTarget
    integer :: seq, ii
    if (css%ttarget .eq. 0) then ! accept all reports if there is no target...
       bok=.true.
       return
    end if
    if (css%category .eq. ksec1(6) .and. &
         & css%subcategory .eq. ksec1(7)) then
       bok=.true.
       currentTarget => css%firstTarget%next
       TARGETS : do while (.not.associated(currentTarget , target = css%lastTarget))
          !write(*,*) myname,'Checking:',currentTarget%category,ksec1(6),currentTarget%subcategory,ksec1(7)
          seq=currentTarget%seq
          if (seq.gt.ktdexl) bok=.false.
          if (bok) bok=(ktdexp(seq).eq.currentTarget%descr)
          if (currentTarget%lval(1).or.currentTarget%lval(2)) then
             if (currentTarget%lval(3)) then ! invert check
                if (bok) bok=(values(seq).ge.currentTarget%minval.or.values(seq).le.currentTarget%maxval)
             else
                if (bok.and.currentTarget%lval(1)) bok=(values(seq).ge.currentTarget%minval)
                if (bok.and.currentTarget%lval(1)) bok=(values(seq).le.currentTarget%maxval)
             end if
          end if
          currentTarget => currentTarget%next
       end do TARGETS
    end if
    return
  end subroutine observation_checkTarget
  !
  !###############################################################################
  !ROUTINES FOR HANDLING STACK
  !###############################################################################
  ! Forecasts from one analysis is reportsReady at a time... (=the same parameters)
  ! Used in this way
  ! 1)  observation_sortFiles: make index of all analysis in stack
  ! 2a) observation_stackfirst: put pointer to before first analysis
  ! 3a) observation_getNextFile: report to prev analysis and get forecast times
  ! 2b) observation_stacklast: put pointer to after last analysis
  ! 3b) observation_getPrevFile: report to next analysis and get forecast times
  !
  ! Reset indexes for looping over analysis
  !
  subroutine observation_sortFiles(bid,crc250,irc)
    integer :: bid
    character*250 :: crc250
    integer :: irc
    type(file), pointer :: currentFile => null()
    integer :: ii
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_sortFiles"
    !
    ! make array of files
    if(bdeb)write(*,*)myname,' Entering.'
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from observation_getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    call observation_sortStack(css,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from observation_sortStack.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    css%stackReady = .true.
    return
  end subroutine observation_sortFiles
  !
  ! Prepare to process next analysis
  !
  subroutine observation_getPrevFilelen(bid,maxrep,s25,e25,crc250,irc)
    integer :: bid
    integer :: maxrep        ! max number of reports
    character*25 :: s25,e25
    character*250 :: crc250
    integer :: irc
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_getPrevFilelen"
    logical :: bdone
    if(bdeb)write(*,*)myname,' Entering.'
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! write(*,*)myname,'Here.',css%currentFileSortIndex,css%newnFileSortIndexes,css%stackReady
    if (.not.css%stackReady) then
       if(bdeb)write(*,*)myname,'Calling makeana.'
       call observation_sortStack(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_sortStack.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       if(bdeb)write(*,*)myname,'Calling observation_stackLast.'
       call observation_stacklast(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_stacklast.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%stackReady = .true.
    end if
    call observation_setSortLimits(css,s25,e25)
    !if(bdeb)
    ! write(*,*)myname,'There.',css%currentFileSortIndex,css%newnFileSortIndexes
    isubset=1
    nsubset=0
    maxrep=1
    bdone=.false.
    css%currentFileSortIndex=css%currentFileSortIndex-1                ! count down...
    if (css%currentFileSortIndex.le.0) then
       bdone=.true.
    else
       css%currentFileIndex=css%fileStackInd(css%currentFileSortIndex)
       css%currentFile => css%fileStack(css%currentFileIndex)%pointer
    end if
    SEARCH : do while (.not.bdone)
       if (css%currentFile%ltset .and. css%ltset) then
          if ((css%currentFile%tstart.le.css%tend .and.css%currentFile%tstart.ge.css%tstart) .or.  &
               & (css%currentFile%tend.le.css%tend .and.css%currentFile%tend.ge.css%tstart) .or. &
               & (css%currentFile%tstart.le.css%tstart .and.css%currentFile%tend.ge.css%tstart) .or. &
               & (css%currentFile%tstart.le.css%tend .and.css%currentFile%tend.ge.css%tend)) then ! overlap
             bdone=.true.
          else ! next
             css%currentFileSortIndex=css%currentFileSortIndex-1                ! count down...
             if (css%currentFileSortIndex.le.0) then
                bdone=.true.
             else
                css%currentFileIndex=css%fileStackInd(css%currentFileSortIndex)
                css%currentFile => css%fileStack(css%currentFileIndex)%pointer
             end if
          end if
       else ! no search necessary
          bdone=.true.
       end if
    end do SEARCH
    if (css%currentFileSortIndex.gt.0.and.css%currentFileSortIndex.le.css%newnFileSortIndexes) then
       call observation_getFileReportLen(css%currentFile,maxrep,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_getFileReportLen.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    if(bdeb)write(*,*)myname,' Done.',maxrep
  end subroutine observation_getPrevFilelen
  !
  subroutine observation_getPrevFile(bid,maxrep,nrep,rep250,s25,e25,crc250,irc)
    integer :: bid
    integer :: maxrep        ! max number of reports
    integer :: nrep          ! number of reports
    character*250 :: rep250(maxrep)  ! bufr report
    character*25 :: s25,e25
    character*250 :: crc250
    integer :: irc
    character*50 :: s1, s2, s3
    integer :: len1,len2,len3,lenr,lens,jj
    integer, external :: length
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_getPrevFile"
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if (.not.css%stackReady) then
       call observation_sortStack(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_sortStack.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       call observation_stacklast(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_stacklast.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%stackReady = .true.
    end if
    nrep=0
    if (css%currentFileSortIndex.gt.0.and.css%currentFileSortIndex.le.css%newnFileSortIndexes) then
       if(bdeb)write(*,*)myname,'More data.',css%currentFileIndex
       call observation_getFileReport(css%currentFile,maxrep,nrep,rep250,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_getFileReport.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    !write(*,*)myname,' Done.',nrep
  end subroutine observation_getPrevFile

  !
  ! Prepare to process next analysis
  !
  subroutine observation_getNextFileLen(bid,maxrep,s25,e25,crc250,irc)
    integer :: bid
    integer :: maxrep        ! max number of reports
    character*25 :: s25,e25
    character*250 :: crc250
    integer :: irc
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_getNextFilelen"
    logical :: bdone
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if (.not.css%stackReady) then
       call observation_sortStack(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_sortStack.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       call observation_stackfirst(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_stacklast.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%stackReady = .true.
    end if
    call observation_setSortLimits(css,s25,e25)
    isubset=1
    nsubset=0
    maxrep=1
    bdone=.false.
    css%currentFileSortIndex=css%currentFileSortIndex+1                ! count down...
    if (css%currentFileSortIndex.gt.css%newnFileSortIndexes) then
       bdone=.true.
    else
       css%currentFileIndex=css%fileStackInd(css%currentFileSortIndex)
       css%currentFile => css%fileStack(css%currentFileIndex)%pointer
    end if
    SEARCH : do while (.not.bdone)
       if (css%currentFile%ltset .and. css%ltset) then
          if ((css%currentFile%tstart.le.css%tend .and.css%currentFile%tstart.ge.css%tstart) .or.  &
               & (css%currentFile%tend.le.css%tend .and.css%currentFile%tend.ge.css%tstart) .or. &
               & (css%currentFile%tstart.le.css%tstart .and.css%currentFile%tend.ge.css%tstart) .or. &
               & (css%currentFile%tstart.le.css%tend .and.css%currentFile%tend.ge.css%tend)) then ! overlap
             bdone=.true.
          else ! next
             css%currentFileSortIndex=css%currentFileSortIndex+1                ! count down...
             if (css%currentFileSortIndex.gt.css%newnFileSortIndexes) then
                bdone=.true.
             else
                css%currentFileIndex=css%fileStackInd(css%currentFileSortIndex)
                css%currentFile => css%fileStack(css%currentFileIndex)%pointer
             end if
          end if
       else ! no search necessary
          bdone=.true.
       end if
    end do SEARCH
    if (css%currentFileSortIndex.gt.0.and.css%currentFileSortIndex.le.css%newnFileSortIndexes) then
       call observation_getFileReportLen(css%currentFile,maxrep,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_getFileReportLen.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
  end subroutine observation_getNextFileLen
  !
  subroutine observation_getNextFile(bid,maxrep,nrep,rep250,s25,e25,crc250,irc)
    integer :: bid
    integer :: maxrep        ! max number of rep       if(bdeb)write(*,*)myname,'More data.',css%currentFileSortIndex,nsortorts
    integer :: nrep          ! number of reports
    character*250 :: rep250(maxrep)  ! bufr report
    character*25 :: s25,e25
    character*250 :: crc250
    integer :: irc
    character*50 :: s1, s2, s3
    integer :: len1,len2,len3,lenr,lens,jj
    integer, external :: length
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_getNextFile"
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if (.not.css%stackReady) then
       call observation_sortStack(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_sortStack.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       call observation_stackfirst(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_stackfirst.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%stackReady = .true.
    end if
    nrep=0 ! no more data
    if (css%currentFileSortIndex.gt.0.and.css%currentFileSortIndex.le.css%newnFileSortIndexes) then
       call observation_getFileReport(css%currentFile,maxrep,nrep,rep250,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_getFileReport.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
  end subroutine observation_getNextFile
  !
  ! set the observation time span
  !
  subroutine observation_setIndex(bid,s25,e25,crc250,irc)
    integer :: bid
    character*25 :: s25,e25
    character*250 :: crc250
    integer :: irc
    type(file), pointer :: currentFile => null()
    type(file), pointer :: stackNext => null()
    integer, external :: length
    integer :: lens
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_setIndex"
    if(bdeb)write(*,*)myname,' Entering.'
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    !!!
    if(bdeb)write(*,*)myname,' Done.'
  end subroutine observation_setIndex
  !
  ! set the observation time span
  !
  subroutine observation_setIndexSpan(bid,s25,e25,crc250,irc)
    integer :: bid
    character*25 :: s25,e25
    character*250 :: crc250
    integer :: irc
    type(file), pointer :: currentFile => null()
    type(file), pointer :: stackNext => null()
    integer, external :: length
    integer :: lens
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_setIndexSpan"
    if(bdeb)write(*,*)myname,' Entering.'
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    call observation_setSortLimits(css,s25,e25)
    if(bdeb)write(*,*)myname,' Done.'
  end subroutine observation_setIndexSpan
  !
  ! ignore labels
  !
  subroutine observation_ignorelabel(bid,lab250,crc250,irc)
    integer :: bid
    character*250 :: lab250
    character*250 :: crc250
    integer :: irc
    type(file), pointer :: currentFile => null()
    type(file), pointer :: stackNext => null()
    integer, external :: length
    integer :: lenl
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_ignoreLabel"
    if(bdeb)write(*,*)myname,' Entering.'
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    call chop0(lab250,250)
    lenl=length(lab250,250,10)
    if (lenl.eq.0) then
       css%ignmis=.false.
       css%ignuni=.false.
       css%ignden=.false.
       css%ignder=.false.
       css%ignval=.false.
       css%ignsec=.false.
    else if (lab250(1:lenl).eq."missing") then
       css%ignmis=.true.
    else if (lab250(1:lenl).eq."unit") then
       css%ignuni=.true.
    else if (lab250(1:lenl).eq."description") then
       css%ignden=.true.
    else if (lab250(1:lenl).eq."descriptor") then
       css%ignder=.true.
    else if (lab250(1:lenl).eq."value") then
       css%ignval=.true.
    else if (lab250(1:lenl).eq."section") then
       css%ignsec=.true.
    else if (lab250(1:lenl).eq."array") then
       css%ignarr=.true.
    end if
    if(bdeb)write(*,*)myname,' Done.'
  end subroutine observation_ignorelabel
  !
  ! private subroutine for setting start/end-search-dates
  !
  subroutine observation_setSortLimits(css,s25,e25)
    type(session), pointer :: css !  current session
    character*25 :: s25,e25
    integer :: lens, lene
    integer, external :: length
    integer :: yy,mm,dd,hh,mi
    real :: sec, j2000
    integer :: irc2
    character*22 :: myname="observation_setSortLimits"
    call chop0(s25,25)
    lens=length(s25,25,10)
    call chop0(e25,25)
    lene=length(e25,25,10)
    yy=0
    mm=0
    dd=0
    hh=0
    mi=0
    sec=0.0D0
    if (lens.ne.0.and.lene.ne.0) then
       css%ltset=.true.
       if (css%ltset) then
          read(s25,'(I4,4(X,I2))',iostat=irc2)yy,mm,dd,hh,mi
          if (irc2.eq.0) then
             css%ltset=.true.
          else
             read(s25,'(I4,2(X,I2))',iostat=irc2)yy,mm,dd
             if (irc2.eq.0) then
                css%ltset=.true.
                hh=0
                mi=0
             else
                write(*,*)myname,'Unable to interpret start date:',s25
                css%ltset=.false.
             end if
          end if
       end if
       if (css%ltset) then
          call jd2000(j2000,yy,mm,dd,hh,mi,sec)
          css%tstart=j2000
       end if
       if (css%ltset) then
          read(e25,'(I4,4(X,I2))',iostat=irc2)yy,mm,dd,hh,mi
          if (irc2.eq.0) then
             css%ltset=.true.
          else
             read(e25,'(I4,2(X,I2))',iostat=irc2)yy,mm,dd
             if (irc2.eq.0) then
                css%ltset=.true.
                hh=0
                mi=0
             else
                write(*,*)myname,'Unable to interpret end date:',e25
                css%ltset=.false.
             end if
          end if
       end if
       if (css%ltset) then
          mi=mi+1
          call jd2000(j2000,yy,mm,dd,hh,mi,sec)
          css%tend=j2000
       end if
    else
       css%ltset=.false.
    end if
  end subroutine observation_setSortLimits
  !
  ! private subroutine for sorting the stack
  !
  subroutine observation_sortStack(css,crc250,irc)
    type(session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(file), pointer :: currentFile => null()
    integer :: ii
    character*22 :: myname = "observation_sortStack"
    !
    ! make array of files
    if(bdeb)write(*,*)myname,' Entering.'
    if (associated(css%fileStack)) deallocate(css%fileStack)
    if (allocated(css%fileStackSort)) deallocate(css%fileStackSort)
    if (allocated(css%fileStackInd)) deallocate(css%fileStackInd)
    allocate(css%fileStack(css%nFileIndexes),css%fileStackSort(css%nFileIndexes),&
         &css%fileStackInd(css%nFileIndexes),stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from allocate (")
       call observation_errorappendi(crc250,css%nFileIndexes)
       call observation_errorappend(crc250,")")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if(bdeb)write(*,*)myname,' Here.'
    currentFile => css%firstFile%next
    ii=0
    do while (.not.associated(currentFile, target=css%lastFile))
       ii=ii+1
       if (ii.le.css%nFileIndexes) then
          css%fileStack(ii)%pointer => currentFile
          css%fileStackInd(ii)=ii
          css%fileStackSort(ii)=css%fileStack(ii)%pointer%tstart
       end if
       currentFile => currentFile%next
    end do
    if(bdeb)write(*,*)myname,' There.'
    if (ii.ne.css%nFileIndexes) then
       irc=944
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," System error C:")
       call observation_errorappendi(crc250,css%nFileIndexes)
       call observation_errorappend(crc250,"!=")
       call observation_errorappendi(crc250,ii)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! make sorted index (chronologically)
    css%nFileSortIndexes=css%nFileIndexes
    css%newnFileSortIndexes=css%nFileIndexes
    call observation_heapsort1r(css%nFileIndexes,css%fileStackSort,1.0D-5, &
         & css%newnFileSortIndexes,css%nFileSortIndexes,css%fileStackInd,.false.)
    ! set index range
    if(bdeb)write(*,*)myname,' Where.'
    call observation_stacklast(css,crc250,irc)   ! start with latest analysis
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from observation_setLast.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if(bdeb)write(*,*)myname,' Done.',css%newnFileSortIndexes
    return
  end subroutine observation_sortStack
  !
  subroutine observation_stackfirst(css,crc250,irc)
    type(session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname = "observation_stackfirst"
    css%currentFileSortIndex=0
    css%currentFileIndex=0
  end subroutine observation_stackfirst
  !
  subroutine observation_stacklast(css,crc250,irc)
    type(session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname = "observation_stacklast"
    if(bdeb)write(*,*)myname,' Entering.',associated(css)
    css%currentFileSortIndex=css%newnFileSortIndexes+1
    css%currentFileIndex=0
    if(bdeb)write(*,*)myname,' Done.'
  end subroutine observation_stacklast
  !
  !###############################################################################
  ! ROUTINES FOR REPORTING OBSERVATIONS TO USER
  !###############################################################################
  !
  ! put next BUFR-message in memory and get report length
  !
  subroutine observation_sliceCurrentFileLen(bid,maxrep,maxarr,bok,crc250,irc)
    integer :: bid
    integer :: maxrep        ! max number of report elements
    integer :: maxarr        ! max number of value elements
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    integer :: nrtarget,ii,lent
    integer, external :: length
    logical :: bok, bdone
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_sliceCurrentFileLen"
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! get next observation from file
    if (.not.css%stackReady) then
       call observation_sortStack(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_sortStack.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       call observation_stackfirst(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_stacklast.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%currentFileSortIndex=1
       css%stackReady = .true.
    end if
    bok=.false.
    if (isubset > nsubset) then
       call observation_readMessage(css,bok,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_readMessage.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    if (bok) then
       call observation_getReportLen(css,maxrep,bok,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_getReportLen.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       call observation_getArrayLen(css,maxarr,bok,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_getArrayLen.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    else
       maxrep=1
       maxarr=1
    end if
    ! write(*,*)myname,' Done.',bok
  end subroutine observation_sliceCurrentFileLen
  !
  ! retrieve next bufr-array from memory
  !
  subroutine observation_sliceCurrentFile(bid,maxrep,nrep,rep250,maxarr,narr,arr,bok,crc250,irc)
    integer :: bid
    integer :: maxrep        ! max number of arrays
    integer :: nrep          ! number of arrays
    character*250 :: rep250(maxrep)  ! bufr array
    integer :: maxarr        ! max number of arrays
    integer :: narr          ! number of arrays
    real :: arr(maxarr)  ! bufr array
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    type(reportItem), pointer :: old ! last arrayItem
    integer :: lenr, lenb, lenc
    logical :: bdone
    integer, external :: length
    type(reportItem), pointer :: current => null()
    type(session), pointer :: css !  current session
    character*22 :: myname = "observation_sliceCurrentFile"
    !write(*,*)myname,' Entering.',bok
    call observation_getSession(css,bid,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from getSession.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if (bok) then
       call observation_getReport(css,maxrep,nrep,rep250,bok,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_getReport.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       call observation_getArray(css,maxarr,narr,arr,bok,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_getArray.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       isubset=isubset+1
    end if
    !write(*,*)myname,' Done.',bok,narr
    return
  end subroutine observation_sliceCurrentFile


  ! !
  ! ! put next BUFR-message in memory and get report length
  ! !
  ! subroutine observation_sliceCurrentFileLen(bid,maxrep,bok,crc250,irc)
  !   integer :: bid
  !   integer :: maxrep        ! max number of reports
  !   character*250 :: crc250  ! error message string
  !   integer :: irc           ! error return code (0=ok)
  !   integer :: nrtarget,ii,lent
  !   integer, external :: length
  !   logical :: bok, bdone
  !   type(session), pointer :: css !  current session
  !   character*22 :: myname = "observation_sliceCurrentFileLen"
  !   call observation_getSession(css,bid,crc250,irc)
  !   if (irc.ne.0) then
  !      call observation_errorappend(crc250,myname)
  !      call observation_errorappend(crc250," Error return from getSession.")
  !      call observation_errorappendi(crc250,irc)
  !      call observation_errorappend(crc250,"\n")
  !      return
  !   end if
  !   ! get next observation from file
  !   if (.not.css%stackReady) then
  !      call observation_sortStack(css,crc250,irc)
  !      if (irc.ne.0) then
  !         call observation_errorappend(crc250,myname)
  !         call observation_errorappend(crc250," Error return from observation_sortStack.")
  !         call observation_errorappendi(crc250,irc)
  !         call observation_errorappend(crc250,"\n")
  !         return
  !      end if
  !      call observation_stackfirst(css,crc250,irc)
  !      if (irc.ne.0) then
  !         call observation_errorappend(crc250,myname)
  !         call observation_errorappend(crc250," Error return from observation_stacklast.")
  !         call observation_errorappendi(crc250,irc)
  !         call observation_errorappend(crc250,"\n")
  !         return
  !      end if
  !      css%currentFileSortIndex=1
  !      css%stackReady = .true.
  !   end if
  !   bok=.false.
  !   if (isubset > nsubset) then
  !      call observation_readMessage(css,bok,crc250,irc)
  !      if (irc.ne.0) then
  !         call observation_errorappend(crc250,myname)
  !         call observation_errorappend(crc250," Error return from observation_readMessage.")
  !         call observation_errorappendi(crc250,irc)
  !         call observation_errorappend(crc250,"\n")
  !         return
  !      end if
  !   end if
  !   if (bok) then
  !      call observation_getReportLen(css,maxrep,bok,crc250,irc)
  !      if (irc.ne.0) then
  !         call observation_errorappend(crc250,myname)
  !         call observation_errorappend(crc250," Error return from observation_getReportLen.")
  !         call observation_errorappendi(crc250,irc)
  !         call observation_errorappend(crc250,"\n")
  !         return
  !      end if
  !   else
  !      maxrep=1
  !   end if
  ! end subroutine observation_sliceCurrentFileLen
  ! !
  ! ! retrieve next bufr-report from memory
  ! !
  ! subroutine observation_sliceCurrentFile(bid,maxrep,nrep,rep250,bok,crc250,irc)
  !   integer :: bid
  !   integer :: maxrep        ! max number of reports
  !   integer :: nrep          ! number of reports
  !   character*250 :: rep250(maxrep)  ! bufr report
  !   logical :: bok           ! was get successful?
  !   character*250 :: crc250  ! error message string
  !   integer :: irc           ! error return code (0=ok)
  !   type(reportItem), pointer :: old ! last reportItem
  !   integer :: lenr, lenb, lenc
  !   logical :: bdone
  !   integer, external :: length
  !   type(reportItem), pointer :: current => null()
  !   type(session), pointer :: css !  current session
  !   character*22 :: myname = "observation_sliceCurrentFile"
  !   !write(*,*)myname,' Entering.',bok
  !   call observation_getSession(css,bid,crc250,irc)
  !   if (irc.ne.0) then
  !      call observation_errorappend(crc250,myname)
  !      call observation_errorappend(crc250," Error return from getSession.")
  !      call observation_errorappendi(crc250,irc)
  !      call observation_errorappend(crc250,"\n")
  !      return
  !   end if
  !   if (bok) then
  !      call observation_getReport(css,maxrep,nrep,rep250,bok,crc250,irc)
  !      if (irc.ne.0) then
  !         call observation_errorappend(crc250,myname)
  !         call observation_errorappend(crc250," Error return from observation_getReport.")
  !         call observation_errorappendi(crc250,irc)
  !         call observation_errorappend(crc250,"\n")
  !         return
  !      end if
  !      isubset=isubset+1
  !   end if
  !   !write(*,*)myname,' Done.',bok,nrep
  !   return
  ! end subroutine observation_sliceCurrentFile


  !
  !###############################################################################
  ! REPORT PROCESSING
  !###############################################################################
  !
  ! remove report generation-item
  !
  subroutine observation_clearReports(css,crc250,irc)
    type(session), pointer :: css !  current session
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    type(reportitem), pointer :: item, nextItem
    css%currentReport => css%firstReport%next
    do while (.not.associated(css%currentReport,target=css%lastReport))
       if (associated(css%currentReport%firstItem)) then
          item=>css%currentReport%firstItem%next
          do while (.not.associated(item,target=css%currentReport%lastItem))
             nextItem=>item%next;
             deallocate(item)
             item=>nextItem;
          end do
       end if
       css%firstReport%next => css%currentReport%next
       css%currentReport%next%prev => css%firstReport
       deallocate(css%currentReport)
       css%currentReport=>css%firstReport%next
    end do
    css%nsubset=0
    nullify(css%currentReport)
  end subroutine observation_clearReports
  !
  subroutine observation_createReport(css,crc250,irc)
    type(session), pointer :: css !  current session
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    type(report), pointer :: newReport
    character*22 :: myname = "observation_createReport"
    allocate(newReport,stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Unable to allocate 'new report'.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    allocate(newReport%firstItem,newReport%lastitem,stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Unable to allocate 'first/lastitem'.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    newReport%firstitem%next => newReport%lastitem
    newReport%lastitem%prev => newReport%firstitem
    newReport%prev => css%lastReport%prev
    newReport%next => css%lastReport
    css%lastReport%prev%next => newReport
    css%lastReport%prev => newReport
    css%currentReport => newReport
    css%nsubset = css%nsubset + 1
  end subroutine observation_createReport
  !
  subroutine observation_addReportItem(css,buff250,crc250,irc)
    type(session), pointer :: css !  current session
    character*250 :: buff250
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    type(reportItem), pointer :: newItem
    character*22 :: myname = "observation_addReportItem"
    !write(*,*)myname,' Entering.',irc,buff250(1:10)
    allocate(newItem,stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"observation_addReportItem")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    newItem%desc250=buff250
    newItem%prev => css%currentReport%lastItem%prev
    newItem%next => css%currentReport%lastItem
    css%currentReport%lastItem%prev%next => newItem
    css%currentReport%lastItem%prev => newItem
    css%currentReport%nitem = css%currentReport%nitem + 1
    !write(*,*)myname,' Done.',irc
  end subroutine observation_addReportItem
  !
  ! Read next report into memory and return report-length
  !
  subroutine observation_readMessage(css,bok,crc250,irc)
    type(session), pointer :: css !  current session
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname = "observation_readMessage"
    !
    ! check if file is open, if not open it
    !
    bok=.true.
    if (.not.fopen) then
       call observation_openFile(css,bok,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_openFile.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       if (.not.bok) return
    end if
    !
    ! read next observation, make reports
    !
    call observation_readFile(css,bok,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from observation_readObservation.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    !
    ! if last observation, close file
    !
    if (.not.bok) then
       call observation_closeFile(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_closeFile.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    return
  end subroutine observation_readMessage
  !
  ! make raw-array
  !
  subroutine observation_getArrayLen(css,maxarr,bok,crc250,irc)
    type(session), pointer :: css
    integer :: maxarr        ! max number of array elements
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    if (isubset > nsubset) then
       bok=.false.
       maxarr=1
    else if (css%ignarr) then
       bok=.true.
       maxarr=1
    else
       bok=.true.
       maxarr = KTDEXL
    end if
    return
  end subroutine observation_getArrayLen
  subroutine observation_getArray(css,maxarr,narr,arr,bok,crc250,irc)
    type(session), pointer :: css
    integer :: maxarr        ! max number of array elements
    integer :: narr          ! number of array elements
    real :: arr(maxarr)      ! bufr array
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    integer, external :: length
    integer :: lenb,len0,len1,len2,len3
    character*50 :: s0,s1,s2,s3
    integer :: ii,ipos
    real :: val
    narr=0
    if (isubset > nsubset) then
       bok=.false.
    else if (css%ignarr) then
       bok=.true.
    else
       bok=.true.
       DO II=1,KTDEXL
          IPOS=II+(isubset-1)*KEL
          val=values(ipos)
          if (val.eq.RVIND) then
             narr=narr+1
             arr(narr)=val
          else if (.not.css%ignval) then
             narr=narr+1
             arr(narr)=val
          end if
       end do
    end if
    return
  end subroutine observation_getArray
  !
  ! Make report item
  !
  subroutine observation_getReportLen(css,maxrep,bok,crc250,irc)
    type(session), pointer :: css
    integer :: maxrep        ! max number of reports
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    !
    integer, external :: length
    integer :: lenb,len0,len1,len2,len3
    character*50 :: s0,s1,s2,s3
    integer :: ii,jj,ipos,i,j
    real :: rlon1,rlat1,rlon2,rlat2
    integer :: iktype,idd,id
    CHARACTER*9 :: CIDENT
    character*22 :: myname = "observation_getReportLen"
    maxrep=0
    if (isubset > nsubset) then
       bok=.false.
    else
       maxrep=maxrep+4
       !
       if (.not. css%ignsec) then
          maxrep=maxrep+16
          ! section 2
          !
          IF(KSUP(2).LE.1) THEN
             !WRITE(*,*)  'Prtkey : RDB key not defined in section 2.'
          else
             maxrep=maxrep+1
             IKTYPE=0
             IF(KEY(2).EQ.2) IKTYPE=2
             IF(KEY(2).EQ.3) IKTYPE=2
             IF(KEY(2).EQ.12)IKTYPE=2
             IF(KEY(2).EQ.08)IKTYPE=2
             IF(IKTYPE.EQ.0.AND.KSUP(6).GT.1) IKTYPE=2
             IF(IKTYPE.EQ.2) THEN
                IF(KEY(2).EQ.2.OR.KEY(2).EQ.3 &
                     & .OR.KEY(2).EQ.12) THEN
                   maxrep=maxrep+32
                ELSE
                   maxrep=maxrep+29
                END IF
             end if
          end if
          !
          ! section 3
          !
          maxrep=maxrep+4
       end if
       if (.not.css%ignder) maxrep=maxrep+KTDLEN
       if (.not.css%ignden) maxrep=maxrep+KTDEXL
       if (.not.css%ignuni) maxrep=maxrep+KTDEXL
       if (.not.css%ignval) maxrep=maxrep+2*KTDEXL
       maxrep=maxrep+css%mvar
    end if
    !write(*,*)myname,' Done.',irc
  end subroutine observation_getReportLen
  subroutine observation_getReport(css,maxrep,nrep,rep250,bok,crc250,irc)
    type(session), pointer :: css
    integer :: maxrep        ! max number of reports
    integer :: nrep          ! number of reports
    character*250 :: rep250(maxrep)  ! bufr report
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    !
    character*250 :: buff250
    integer, external :: length
    integer :: lenb,len0,len1,len2,len3
    character*50 :: s0,s1,s2,s3
    integer :: ii,jj,ipos,i,j
    real :: rlon1,rlat1,rlon2,rlat2
    integer :: iktype,idd,id
    CHARACTER*9 :: CIDENT
    type(target), pointer :: currentTarget => null()
    logical :: bbok           ! was get successful?
    integer :: seq
    real :: val
    character*22 :: myname = "observation_getReport"
    nrep=0
    if (isubset > nsubset) then
       bok=.false.
    else
       !write(*,*)myname,'Report A:',isubset
       write(s0,'(A,I0)') 'subset'//sep,isubset;call chop0(s0,50);
       len0=length(s0,50,10) ! subset/report identification
       if (.not. css%ignsec) then
          nrep=min(nrep+1,maxrep)
          WRITE(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '0'//sep//'Length of section 0 (bytes)'//sep,KSEC0(1)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '0'//sep//'Total length of Bufr message (bytes)'//sep,KSEC0(2)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '0'//sep//'Bufr Edition number'//sep,KSEC0(3)," "
          !
          ! section 1
          !
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Length of section 1 (bytes)'//sep,KSEC1( 1)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Bufr Edition number'//sep,KSEC1( 2)," "
          nrep=min(nrep+1,maxrep)
          if(ksec1(2).ge.3) then
             write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
                  & '1'//sep//'Originating sub-centre'//sep,KSEC1(16)," "
          end if
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Originating centre'//sep,KSEC1( 3)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Update sequence number'//sep,KSEC1( 4)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Flag (presence of section 2)'//sep,KSEC1( 5)," "
       end if
       call observation_getType(ksec1(6),ksec1(7),s2,s3,crc250,irc) 
       call chop0(s2,50); len2=length(s2,50,10)
       call chop0(s3,50); len3=length(s3,50,10)
       nrep=min(nrep+1,maxrep)
       write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
            & '1'//sep//'Bufr message type'//sep,KSEC1( 6)," "
       if (len2.ne.0) then
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,A,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Bufr message type description'//sep,S2(1:len2)," "
       end if
       nrep=min(nrep+1,maxrep)
       write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
            & '1'//sep//'Bufr message subtype'//SEP,KSEC1( 7)," "
       if (len3.ne.0) then
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,A,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Bufr message subtype description'//SEP,s3(1:len3)," "
       end if
       if (.not. css%ignsec) then
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Version number of local table'//SEP,KSEC1( 8)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Year'//SEP,KSEC1( 9)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Month'//SEP,KSEC1(10)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Day'//SEP,KSEC1(11)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Hour'//SEP,KSEC1(12)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Minute'//SEP,KSEC1(13)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Version number of Master table'//SEP,KSEC1(15)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '1'//sep//'Bufr Master table'//SEP,KSEC1(14)," "
          !
          ! section 2
          !
          IF(KSUP(2).LE.1) THEN
             !WRITE(*,*)  'Prtkey : RDB key not defined in section 2.'
          else
             nrep=min(nrep+1,maxrep)
             write(rep250(nrep),'(A,I9)')s0(1:len0)//sep//'section'//sep//&
                  & '2'//sep//'Length of section 2'//sep, KEY(1)
             IKTYPE=0
             IF(KEY(2).EQ.2) IKTYPE=2
             IF(KEY(2).EQ.3) IKTYPE=2
             IF(KEY(2).EQ.12)IKTYPE=2
             IF(KEY(2).EQ.08)IKTYPE=2
             IF(IKTYPE.EQ.0.AND.KSUP(6).GT.1) IKTYPE=2
             IF(IKTYPE.EQ.2) THEN
                IF(KEY(2).EQ.2.OR.KEY(2).EQ.3 &
                     & .OR.KEY(2).EQ.12) THEN
                   RLAT1=(KEY(11)-9000000)/100000.
                   RLON1=(KEY(10)-18000000)/100000.
                   RLAT2=(KEY(13)-9000000)/100000.
                   RLON2=(KEY(12)-18000000)/100000.
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'RDB data type'//sep, KEY(2)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'RDB data subtype'//sep, KEY(3)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Year'//sep, KEY(4)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Month'//sep, KEY(5)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Day'//sep, KEY(6)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Hour'//sep, KEY(7)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Minute'//sep, KEY(8)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Second'//sep, KEY(9)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,F0.2,A)')s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Latitude  1'//sep, RLAT1," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,F0.2,A)')s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Longitude 1'//sep, RLON1," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,F0.2,A)')s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Latitude  2'//sep, RLAT2," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,F0.2,A)')s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Longitude 2'//sep, RLON2," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Number of observations'//sep, KEY(14)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Identifier'//sep, KEY(15)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Total Bufr message length'//sep, KEY(25)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Day    (RDB insertion'//sep, KEY(26)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Hour   (RDB insertion'//sep, KEY(27)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Minute( (RDB insertion'//sep, KEY(28)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Second (RDB insertion'//sep, KEY(29)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Day    (MDB arrival'//sep, KEY(30)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Hour   (MDB arrival'//sep, KEY(31)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Minute (MDB arrival'//sep, KEY(32)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Second (MDB arrival'//sep, KEY(33)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Correction number'//sep, KEY(34)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Part of message'//sep, KEY(35)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Correction number'//sep, KEY(37)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Part of message'//sep, KEY(38)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Correction number'//sep, KEY(40)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Part of message'//sep, KEY(41)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Correction number'//sep, KEY(43)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Part of message'//sep, KEY(44)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Quality control % conf'//sep, KEY(46)," "
                ELSE
                   RLAT1=(KEY(11)-9000000)/100000.
                   RLON1=(KEY(10)-18000000)/100000.
                   IDD=0
                   CIDENT=' '
                   DO ID=16,24
                      IDD=IDD+1
                      CIDENT(IDD:IDD)=CHAR(KEY(ID))
                   end do
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'RDB data type'//sep, KEY(2)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'RDB data subtype'//sep, KEY(3)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Year'//sep, KEY(4)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Month'//sep, KEY(5)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Day'//sep, KEY(6)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Hour'//sep, KEY(7)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Minute'//sep, KEY(8)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Second'//sep, KEY(9)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,F0.2,A)')s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Latitude  1', RLAT1," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,F0.2,A)')s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Longitude 1', RLON1," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,A)')s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Identifier'//sep, CIDENT," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Total Bufr message length'//sep, KEY(25)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Day    (RDB insertion'//sep, KEY(26)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Hour   (RDB insertion'//sep, KEY(27)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Minute (RDB insertion'//sep, KEY(28)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Second (RDB insertion'//sep, KEY(29)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Day    (MDB arrival'//sep, KEY(30)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Hour   (MDB arrival'//sep, KEY(31)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Minute (MDB arrival'//sep, KEY(32)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Second (MDB arrival'//sep, KEY(33)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Correction number'//sep, KEY(34)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Part of message'//sep, KEY(35)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Correction number'//sep, KEY(37)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Part of message'//sep, KEY(38)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Correction number'//sep, KEY(40)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Part of message'//sep, KEY(41)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Correction number'//sep, KEY(43)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Part of message'//sep, KEY(44)," "
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,I0,A)') s0(1:len0)//sep//'section'//sep//&
                        & '2'//sep//'Quality control % conf'//sep, KEY(46)," "
                END IF
             end if
          end if
          !
          ! section 3
          !
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '3'//sep//'Length of section 3 (bytes)'//sep,KSEC3(1)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '3'//sep//'Reserved'//sep,KSEC3(2)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '3'//sep//'Number of data subsets'//sep,KSEC3(3)," "
          nrep=min(nrep+1,maxrep)
          write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
               & '3'//sep//'Flag (data type/data compression)'//sep,KSEC3(4)," "
          !
       end if
       !write(*,*)myname,'Report D:',KTDLEN
       if (.not.css%ignder) then
          DO I=1,KTDLEN
             write(s1,'(I0)') i; call chop0(s1,50); len1=length(s1,50,10)
             nrep=min(nrep+1,maxrep)
             write(rep250(nrep),'(A,I0,A)')s0(1:len0)//sep//'section'//sep//&
                  & '3'//sep//'unexpanded'//sep//s1(1:len1)//sep//'descriptor'//sep,KTDLST(I)," "
          end do
       end if
       !write(*,*)myname,'Report D:',KTDEXL
       !
       if (.not.css%ignuni.or..not.css%ignden.or..not.css%ignval) then
          DO II=1,KTDEXL
             IPOS=II+(isubset-1)*KEL
             val=values(ipos)
             if ((val.eq.rvind.and..not.css%ignmis).or.val.ne.rvind) then
                write(s1,'(I0)') ii; call chop0(s1,50); len1=length(s1,50,10)   ! element identification
                if (css%ignder) then
                   write(buff250,'(A)')s0(1:len0)//sep//'sequence'//sep//s1(1:len1)
                else
                   write(buff250,'(A,I0,A)')s0(1:len0)//sep//'sequence'//sep//s1(1:len1)//sep//&
                        & 'descriptor'//sep,KTDEXP(II)," "
                end if
                call chop0(buff250,250);lenb=length(buff250,250,20)
                if (.not.css%ignden) then
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,A,A)')buff250(1:lenb)//sep//&
                        & 'description'//sep,CNAMES(ii)," "
                end if
                if (.not.css%ignuni) then
                   nrep=min(nrep+1,maxrep)
                   write(rep250(nrep),'(A,A,A)')buff250(1:lenb)//sep//&
                        & 'unit'//sep,CUNITS(ii)," "
                end if
                if (.not.css%ignval) then
                   if (val.eq.RVIND) then
                      s2="MISSING";call chop0(s2,50); len2=length(s2,50,10) 
                      nrep=min(nrep+1,maxrep)
                      write(rep250(nrep),'(A,A,A)')buff250(1:lenb)//sep//&
                           & 'value'//sep,s2(1:len2)," "
                   else
                      write(s2,'(F0.14)') val; call chop0(s2,50); len2=length(s2,50,10)   ! element identification
                      if (len2.gt.1) then
                         OUTER: do JJ=1,len2
                            if (s2(JJ:JJ).eq.".") then
                               INNER: do while (len2.gt.JJ.and.(s2(len2:len2).eq."0".or.s2(len2:len2).eq."."))
                                  len2=len2-1
                               end do INNER
                               exit OUTER
                            end if
                         end do OUTER
                         if (len2.eq.1.and.s2(1:1).eq.".") then
                            s2="0"
                         else if (s2(len2:len2).eq.".") then
                            len2=len2-1
                         end if
                      end if
                      nrep=min(nrep+1,maxrep)
                      write(rep250(nrep),'(A,A,A)')buff250(1:lenb)//sep//&
                           & 'value'//sep,s2(1:len2)," "
                      s2=cvals(ii);call chop0(s2,50);len2=length(s2,50,10)
                      if (len2.gt.0)then
                         nrep=min(nrep+1,maxrep)
                         write(rep250(nrep),'(A,A,A)')buff250(1:lenb)//sep//&
                              & 'code'//sep,S2(1:len2)," "
                      end if
                   end if
                end if
             end if
          end do
       end if
       !
       ! add target values
       !
       if (css%ttarget .gt. 0) then
          if (css%category .eq. ksec1(6) .and. &
               & css%subcategory .eq. ksec1(7)) then
             currentTarget => css%firstTarget%next
             TARGETS : do while (.not.associated(currentTarget , target = css%lastTarget))
                seq=currentTarget%seq
                bbok=(seq.le.ktdexl)
                if (bbok) bbok=(ktdexp(seq).eq.currentTarget%descr)
                if (currentTarget%lval(1).or.currentTarget%lval(2)) then
                   if (currentTarget%lval(3)) then
                      if (bbok) bbok=(values(seq).ge.currentTarget%minval.or.values(seq).le.currentTarget%maxval)
                   else
                      if (bbok.and.currentTarget%lval(1)) bbok=(values(seq).ge.currentTarget%minval)
                      if (bbok.and.currentTarget%lval(1)) bbok=(values(seq).le.currentTarget%maxval)
                   end if
                end if
                if (bbok) then
                   s1=currentTarget%trg80(1:50) ; call chop0(s1,50); len1=length(s1,50,10)   ! element identification
                   write(s2,*) values(seq); call chop0(s1,50); len1=length(s1,50,10)
                   nrep=min(nrep+1,maxrep)
                   WRITE(rep250(nrep),'(A,A)')s0(1:len0)//sep//'target'//sep//&
                        & s1(1:len1)//sep//s2(1:len2)//" "
                end if
                currentTarget => currentTarget%next
             end do TARGETS
          end if
       end if
    end if
    !write(*,*)myname,' Done.',irc
  end subroutine observation_getReport
  !
  !###############################################################################
  ! ROUTINES FOR EXTRACTING BASIC DATA FROM ECMWF BUFR FILES -> FILE OBJECT
  !###############################################################################
  !
  subroutine observation_scanFile(css,newFile,bok,crc250,irc)
    type(session), pointer :: css !  current sessio
    type(file), pointer :: newFile !  current session
    logical :: bok
    character*250 :: crc250
    integer :: irc
    integer :: yy,mm,dd,hh,mi
    real :: sec,j2000
    character*22 :: myname = "observation_scanFile"
    integer :: cnt
    logical :: bbok
    !write(*,*)myname,' Entering.',irc
    !
    ! open file
    !
    call observation_openFile(css,bok,crc250,irc)
    !write(*,*)myname,'Open:',css%currentFile%fn250(1:css%currentFile%lenf),irc,bok
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from observation_openFile.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    !
    call observation_clearCat(css%currentFile)
    !
    ! loop over file
    !
    cnt=0
    if (bok) then
       bok=.false. ! must read at least one observation
       bbok=.true.
       do while (bbok)
          call observation_readFile(css,bbok,crc250,irc)
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Error return from observation_readFile.")
             call observation_errorappendi(crc250,irc)
             call observation_errorappend(crc250,"\n")
             return
          end if
          if (bbok) then
             cnt=cnt+1
             bok=.true.
             yy=KSEC1( 9)
             mm=KSEC1(10)
             dd=KSEC1(11)
             hh=KSEC1(12)
             mi=KSEC1(13)
             sec=0.0D0
             if(bdeb)write(*,'(X,A,A,A,5(A,I0))') myname,"File:",&
                  & css%currentFile%fn250(1:css%currentFile%lenf),":",&
                  & yy,"/",mm,"/",dd," ",hh,":",mi
             call jd2000(j2000,yy,mm,dd,hh,mi,sec)
             !read file and get start/end indexs...
             if (newFile%ltset) then
                newFile%tstart=min(newFile%tstart,j2000)
                newFile%tend=max(newFile%tend,j2000)
             else
                newFile%ltset=.true.
                newFile%tstart=j2000
                newFile%tend=j2000
             end if
             ! store category:
             call observation_storeCat(css%currentFile,KSEC1( 6),KSEC1( 7))
          end if
          isubset=nsubset+1 ! mark data "read"
       end do
       !write(*,*)myname,'Closing file.'
       !
       ! close file
       !
       call observation_closeFile(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_closeFile.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       if (newFile%nsubset.eq.0) then
          write(*,*)myname,'No subsets found.'
       else
          !write(*,*)myname,'Subsets:',newFile%nsubset
       end if
    end if
    !write(*,*)myname,' Done.',irc,bok,cnt
    return
  end subroutine observation_scanFile
  !
  !###############################################################################
  ! ECMWF LIBEMOS ROUTINES FOR READING BUFR FILES
  !###############################################################################
  !
  ! open file
  !
  subroutine observation_openFile(css,bok,crc250,irc)
    type(session), pointer :: css
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    integer :: lenf
    integer,external :: length
    character*250 :: fn250
    character*3 :: mode
    character*22 :: myname = "observation_openFile"
    !
    !     MISSING VALUE INDICATOR
    ! 
    NBYTPW=JBPW/8
    RVIND=1.7D38
    NVIND=2147483647
    css%currentfile%NSUBSET=0
    css%currentFile%NMESSAGE=0
    write(*,*)myname,'Opening file: ',css%currentfile%fn250(1:css%currentfile%lenf)
    CALL PBOPEN(UNIT,css%currentfile%fn250(1:css%currentfile%lenf),'R',irc)
    !write(*,*)myname,'Opened file: ',css%currentfile%fn250(1:css%currentfile%lenf)
    if (irc.ne.0) then
       write(*,*)myname,'Unable to open file.',irc
       call observation_errorappend(crc250,myname)
       IF(irc.EQ.-1) call observation_errorappend(crc250,'OPEN FAILED.')
       IF(irc.EQ.-2) call observation_errorappend(crc250,'INVALID FILE NAME.')
       IF(irc.EQ.-3) call observation_errorappend(crc250,'INVALID OPEN MODE SPECIFIED.')
       call observation_errorappend(crc250,"observation_openFile"//css%currentfile%fn250(1:css%currentfile%lenf));
       call observation_errorappend(crc250,"\n")
       bok=.false.
       return
    end if
    fopen=.true.
    return
  end subroutine observation_openFile
  !
  ! read next BUFR message
  !
  subroutine observation_readFile(css,bok,crc250,irc)
    type(session), pointer :: css
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    integer :: yy,mm,dd,hh,mi
    real :: sec, j2000
    integer :: ii, jj
    character*22 :: myname = "observation_readFile"
    !
    irc=0
    bok=.true.
    msg: do
       KBUFL=0
       CALL PBBUFR(UNIT,KBUFF,JBUFL,KBUFL,irc)
       IF (IRC.EQ.-1) THEN ! EOF
          if(bdeb)PRINT*,'NUMBER OF SUBSETS     ',css%currentFile%NSUBSET
          if(bdeb)PRINT*,'NUMBER OF MESSAGES    ',css%currentFile%NMESSAGE
          IRC=0
          bok=.false.
          return
       end if
       IF (IRC.NE.0) THEN
          write(*,*)myname,'Unable to read file.',irc
          call observation_errorappend(crc250,myname)
          IF(irc.EQ.-2) call observation_errorappend(crc250,'FILE HANDLING PROBLEM.' )
          IF(irc.EQ.-3) call observation_errorappend(crc250,'ARRAY TOO SMALL FOR PRODUCT.')
          call observation_errorappend(crc250,"observation_readFile"//css%currentFile%fn250(1:css%currentFile%lenf));
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%currentfile%nmessage=css%currentfile%nmessage+1
       !     PRINT*,'----------------------------------',N,' ',KBUFL
       KBUFL=KBUFL/NBYTPW+1
       !
       CALL BUS0123( KBUFL,KBUFF,KSUP,KSEC0,&
            & KSEC1,KSEC2,KSEC3,irc)
       !*KSUP*    -  ARRAY CONTAINING SUPLEMENTARY INFORMATION
       !-  KSUP( 1) -- IDIM1, DIMENSION OF KSEC1
       !-  KSUP( 2) -- IDIM2, DIMENSION OF KSEC2
       !-  KSUP( 3) -- IDIM3, DIMENSION OF KSEC3
       !-  KSUP( 4) -- IDIM4, DIMENSION OF KSEC4
       !-  KSUP( 5) -- M (NUMBER OF ELEMENTS IN VALUES ARRAY,FIRST INDEX)
       !-  KSUP( 6) -- N (NUMBER OF SUBSETS,SECOND INDEX OF VALUES ARRAY)
       !-  KSUP( 7) -- JVC (NUMBER OF ELEMENTS IN CVAL ARRAY)
       !-  KSUP( 8) -- TOTAL BUFR MESSAGE LENGTH IN BYTES
       !-  KSUP( 9) -- IDIM0, DIMENSION OF KSEC0
       ! KELEM < KSUP(5)
       ! KVALS < KSUP(6)*KSUP(5)
       ! check that we have enough space in arrays...
       IF(IRC.NE.0) THEN 
          write(*,*)myname,'Unable to decode message header.',irc
          if(bdeb)write(*,*)'ERROR IN BUS012: CORRUPTED BUFR MESSAGE.',css%currentfile%nmessage,&
               & ' in file:',css%currentFile%fn250(1:css%currentFile%lenf)
          IRC=0
          cycle msg
       END IF
       !
       nsubset=KSEC3(3)
       KEL=KVALS/nsubset
       IF(KEL.GT.KELEM) KEL=KELEM
       !WRITE(*,*)myname,'KSUP:',ksup(5),ksup(6),KEL, nsubset
       !
       !write(*,*)myname,'E:'
       CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,&
            & KSEC1,KSEC2 ,KSEC3 ,&
            & KSEC4,KEL,CNAMES,CUNITS,&
            & KVALS,VALUES,CVALS,irc)
       !write(*,*)myname,'F:'
       IF(IRC.NE.0) THEN
          write(*,*)myname,'Unable to decode message data.',irc
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250,"Unable to decode:"//css%currentFile%fn250(1:css%currentFile%lenf));
          call observation_errorappend(crc250,"\n")
          RETURN
       END IF
       css%currentfile%NSUBSET=css%currentfile%NSUBSET+nsubset
       ISUBSET=1 ! start with first subset
       !write(*,*)myname,'G:'
       CALL BUSEL2(ISUBSET,KEL,KTDLEN,&
            & KTDLST,KTDEXL,KTDEXP, &
            & CNAMES,CUNITS,irc)

       !write(*,*)myname,'H:',ISUBSET
       if (irc.ne.0) then
          write(*,*)myname,'Unable to decode message description.',irc
          if(bdeb)write(*,*)'ERROR IN BUSEL2: CORRUPTED BUFR MESSAGE.',css%currentfile%nmessage,&
               & ' in file:',css%currentFile%fn250(1:css%currentFile%lenf)
          irc=0
          cycle msg
       end if
       ! CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,IRC)
       ! if (irc.ne.0) then
       !    write(*,*)myname,'Unable to decode message keys.',irc
       !    if(bdeb)write(*,*)'ERROR IN BUUKEY: CORRUPTED BUFR MESSAGE.',css%currentfile%nmessage,&
       !         & ' in file:',css%currentFile%fn250(1:css%currentFile%lenf)
       !    irc=0
       !    cycle msg
       ! end if
       nitem=KTDEXL
       !write(*,*)myname,'I:'
       !
       ! check against target
       !
       call observation_checkTarget(css,bok,crc250,irc)
       !
       ! check against times
       !
       if (bok .and. css%ltset) then
          ! get message time
          yy=KSEC1( 9)
          mm=KSEC1(10)
          dd=KSEC1(11)
          hh=KSEC1(12)
          mi=KSEC1(13)
          sec=0.0D0
          call jd2000(j2000,yy,mm,dd,hh,mi,sec)
          bok= ((j2000.le.css%tend .and.j2000.ge.css%tstart))
          if(bdeb)write(*,'(X,A,A,A,5(A,I0),X,L1)') myname,"File:",&
               & css%currentFile%fn250(1:css%currentFile%lenf),":",&
               & yy,"/",mm,"/",dd," ",hh,":",mi,bok
          !write(*,*) myname,' Time:',bok,css%ltset,j2000,css%tstart,css%tend
       end if
       if (.not. bok) then
          irc=0
          cycle msg
       else
          exit msg
       end if
    end do msg
    !write(*,*)myname,'X:'
  end subroutine observation_readFile
  !
  ! close file
  !
  subroutine observation_closeFile(css,crc250,irc)
    type(session), pointer :: css
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname = "observation_closeFile"
    !
    CALL PBCLOSE(unit,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"observation_closeFile")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    unit=0
    fopen=.false.
    !
  end subroutine observation_closeFile


  !######################################################
  subroutine observation_clearCat(currentFile)
    type(file), pointer :: currentFile
    type(mainCategory),pointer :: currentCat, nextCat
    type(subCategory),pointer :: currentSub, nextSub
    !write(*,*) 'observation_clearCat Entering.'
    if (associated(currentFile%firstCategory%next)) then
       currentCat=> currentFile%firstCategory%next
       do while (.not.associated(currentCat,target=currentFile%lastCategory)) 
          nextCat=>currentCat%next
          currentSub=> currentCat%firstSubCategory%next
          do while (.not.associated(currentSub,target=currentCat%lastSubCategory)) 
             nextSub=>currentSub%next
             if (allocated(currentSub%ktdexp)) deallocate(currentSub%ktdexp)
             if (allocated(currentSub%cnames)) deallocate(currentSub%cnames)
             if (allocated(currentSub%cunits)) deallocate(currentSub%cunits)
             deallocate(currentSub)
             currentSub=>nextSub
          end do
          deallocate(currentCat)
          currentCat=>nextCat
       end do
       currentFile%ncat=0
       currentFile%nsub=0
    end if
    !write(*,*) 'observation_clearCat Done.'
  end subroutine observation_clearCat
  !
  subroutine observation_storeCat(currentFile,cat,subcat)
    type(file), pointer :: currentFile
    integer :: cat
    integer :: subcat
    type(mainCategory),pointer :: currentCat
    type(subCategory),pointer :: currentSub
    integer ii,irc
    character*22 :: myname = "observation_storeCat"
    currentCat=> currentFile%firstCategory%next
    do while (.not.associated(currentCat,target=currentFile%lastCategory)) 
       if (currentCat%category .eq. cat) then
          currentCat%cnt=currentCat%cnt+1
          currentSub=> currentCat%firstSubCategory%next
          do while (.not.associated(currentSub,target=currentCat%lastSubCategory)) 
             if (currentSub%subCategory .eq. subcat) then
                currentSub%cnt=currentSub%cnt+1
                return
             else
                currentSub=>currentSub%next
             end if
          end do
          ! found cat but not subcat
          currentFile%nsub=currentFile%nsub+1
          currentCat%nsub=currentCat%nsub+1
          allocate(currentSub)
          currentSub%subcategory=subcat
          currentSub%next => currentCat%lastSubCategory
          currentSub%prev => currentCat%lastSubCategory%prev
          currentCat%lastSubCategory%prev%next => currentSub
          currentCat%lastSubCategory%prev => currentSub
          currentSub%cnt=currentSub%cnt+1
          ! store data sequence
          currentSub%ktdexl=KTDEXL
          allocate(currentSub%ktdexp(currentSub%ktdexl),currentSub%cnames(currentSub%ktdexl),&
               & currentSub%cunits(currentSub%ktdexl),stat=irc)
          if (irc.ne.0) then
             currentSub%ktdexl=0
             !call observation_errorappend(crc250,myname)
             !call observation_errorappend(crc250,"Unable to allocate 'sub-sequence'.")
             !call observation_errorappend(crc250,"\n")
             !return
          end if

          write(*,*)myname,"SEQUENCE:",currentSub%ktdexl

          do ii=1,currentSub%ktdexl
             currentSub%ktdexp(ii)=ktdexp(ii)
             currentSub%cnames(ii)=cnames(ii)
             currentSub%cunits(ii)=cunits(ii)
          end do
          return
       else
          currentCat=>currentCat%next
       end if
    end do
    ! did not find cat nor subcat
    currentFile%ncat=currentFile%ncat+1
    allocate(currentCat)
    currentCat%category=cat
    currentCat%next => currentFile%lastCategory
    currentCat%prev => currentFile%lastCategory%prev
    currentFile%lastCategory%prev%next => currentCat
    currentFile%lastCategory%prev => currentCat
    currentCat%lastSubCategory%prev => currentCat%firstSubCategory
    currentCat%firstSubCategory%next => currentCat%lastSubCategory
    currentCat%cnt=currentCat%cnt+1
    !
    currentFile%nsub=currentFile%nsub+1
    currentCat%nsub=currentCat%nsub+1
    allocate(currentSub)
    currentSub%subcategory=subcat
    currentSub%next => currentCat%lastSubCategory
    currentSub%prev => currentCat%lastSubCategory%prev
    currentCat%lastSubCategory%prev%next => currentSub
    currentCat%lastSubCategory%prev => currentSub
    currentSub%cnt=currentSub%cnt+1
    return
  end subroutine observation_storeCat
  !
  ! returns time as character-string
  !
  character*21 function observation_gettime(j2000) 
    implicit none
    real :: j2000
    integer :: yy,mm,dd,hh,mi
    real :: sec
    character*4 :: csec
    integer, external :: length
    integer :: lenp,lenc
    character*22 :: myname = "observation_gettime"
    call dj2000(j2000,yy,mm,dd,hh,mi,sec)
    write(csec,'(F4.1)') sec
    call chop0(csec,4)
    lenc=length(csec,4,4)
    if (sec.lt.10.0D0)  csec="0"//csec(1:lenc)
    write(observation_gettime,'(I4.4,"/",I2.2,"/",I2.2," ",I2.2,":",I2.2,":",A4)') yy,mm,dd,hh,mi,csec
!!!!! write(*,*) myname,j2000,observation_gettime
  end function observation_gettime

  real function  observation_getj2000(time50,crc250,irc) 
    implicit none
    character*50 :: time50
    character*250 :: crc250
    integer :: irc
    real :: j2000
    integer, external :: length
    integer :: lenp,lent
    character*22 :: myname = "observation_getj2000"
    integer :: yy,mm,dd,hh,mi
    real :: sec
    ! first try to read as formatted time
    read(time50,'(I4,X,I2,X,I2,X,I2,X,I2,X,F4.1)',iostat=irc)yy,mm,dd,hh,mi,sec
    if (irc.eq.0) then
       call jd2000(j2000,yy,mm,dd,hh,mi,sec)
    else
       read(time50,*,iostat=irc)j2000
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Unable to determine time from:")
          call observation_errorappend(crc250,time50)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    observation_getj2000=j2000
    !lent=length(time50,50,10)
    if(bdeb)write(*,*)myname,' Time:',time50(1:lent)," Found:",j2000
  end function observation_getj2000

  !
  ! B U F R   C O D E T A B L E   R O U T I N E S
  !
  subroutine observation_getType(icode,isubcode,typ50,sub50,crc250,irc) 
    implicit none
    integer :: icode
    integer :: isubcode
    character*50 :: typ50, sub50
    character*250 :: crc250
    integer :: irc
    integer :: left,right,bingo
    type(table), pointer :: ptable
    character*22 :: myname="observation_getType"
    if (icode.eq.0) then ! surface
       typ50="Surface"
       if (isubcode.eq.1) then
          sub50="synop land"
       else if (isubcode.eq.2) then
          sub50="synop land record 2"
       else if (isubcode.eq.3) then
          sub50="synop land (auto)"
       else if (isubcode.eq.4) then
          sub50="synop land (auto) 2"
       else if (isubcode.eq.7) then
          sub50="soil temperatures"
       else if (isubcode.eq.108) then
          sub50="climat synop"
       else if (isubcode.eq.116) then
          sub50="soil temperatures USA"
       else if (isubcode.eq.117) then
          sub50="soil temperatures"
       else if (isubcode.eq.140) then
          sub50="metar"
       else
          sub50=""
       end if
    else if (icode.eq.1) then ! surface
       typ50="Surface"
       if (isubcode.eq.9) then
          sub50="synop ship abbreviated "
       else if (isubcode.eq.11) then
          sub50="synop ship"
       else if (isubcode.eq.12) then
          sub50="synop ship record 2"
       else if (isubcode.eq.13) then
          sub50="synop ship (auto)"
       else if (isubcode.eq.14) then
          sub50="synop ship (auto) 2"
       else if (isubcode.eq.19) then
          sub50="synop ship reduced"
       else if (isubcode.eq.21) then
          sub50="surface buoy"
       else if (isubcode.eq.22) then
          sub50="surface bathy"
       else if (isubcode.eq.23) then
          sub50="surface tesac"
       else if (isubcode.eq.27) then
          sub50="buoy argos"
       else
          sub50=""
       end if
    else if (icode.eq.3) then ! Tovs
       typ50="Tovs"
       if (isubcode.eq.51) then
          sub50="80 km"
       else if (isubcode.eq.53) then
          sub50="rtovs"
       else if (isubcode.eq.54) then
          sub50="tovs1b"
       else if (isubcode.eq.55) then
          sub50="atovs"
       else if (isubcode.eq.56) then
          sub50="atovs product"
       else if (isubcode.eq.57) then
          sub50="airs"
       else if (isubcode.eq.58) then
          sub50="avhrr"
       else if (isubcode.eq.61) then
          sub50="500 km low level"
       else if (isubcode.eq.62) then
          sub50="500 km water"
       else if (isubcode.eq.63) then
          sub50="500 km high level"
       else if (isubcode.eq.71) then
          sub50="250 km low level"
       else if (isubcode.eq.72) then
          sub50="250 km water"
       else if (isubcode.eq.73) then
          sub50="250 km high level"
       else if (isubcode.eq.129) then
          sub50="trmm bt"
       else if (isubcode.eq.130) then
          sub50="trmra rain"
       else if (isubcode.eq.206) then
          sub50="gomos"
       else
          sub50=""
       end if
    else if (icode.eq.5) then ! Geostationay
       typ50="Geostationary"
       if (isubcode.eq.82) then
          sub50="satob section 2"
       else if (isubcode.eq.83) then
          sub50="satob section3"
       else if (isubcode.eq.84) then
          sub50="satob section 4"
       else if (isubcode.eq.85) then
          sub50="satob secrion 5"
       else if (isubcode.eq.86) then
          sub50="HR wind"
       else if (isubcode.eq.87) then
          sub50="HR wind + qc"
       else if (isubcode.eq.88) then
          sub50="radiances (geostat)"
       else if (isubcode.eq.89) then
          sub50="radiences (geostat)"
       else if (isubcode.eq.212) then
          sub50="meris"
       else
          sub50=""
       end if
    else if (icode.eq.2) then ! Wind profile, temperature, profile
       if (isubcode.eq.91) then
          sub50="pilot"
          typ50="Wind profile"
       else if (isubcode.eq.92) then
          sub50="pilot ship"
          typ50="Wind profile"
       else if (isubcode.eq.95) then
          sub50="wind profiler (usa)"
          typ50="Wind profile"
       else if (isubcode.eq.96) then
          sub50="wind profiler (Europe)"
          typ50="Wind profile"
       else if (isubcode.eq.97) then
          sub50="wind profiler (Europe)"
          typ50="Wind profile"
       else if (isubcode.eq.101) then
          sub50="temp"
          typ50="Temperature"
       else if (isubcode.eq.102) then
          sub50=" temp ship"
          typ50="profile"
       else if (isubcode.eq.103) then
          sub50=" temp drop"
          typ50="profile"
       else if (isubcode.eq.104) then
          sub50=" rocob"
          typ50="profile"
       else if (isubcode.eq.105) then
          sub50=" rocob ship"
          typ50="profile"
       else if (isubcode.eq.106) then
          sub50=" temp mobile"
          typ50="profile"
       else if (isubcode.eq.107) then
          sub50=" climat temp"
          typ50="profile"
       else
          sub50=""
          typ50="profile"
       end if
    else if (icode.eq.31) then ! Oceanographic 
       typ50="Oceanographic"
       if (isubcode.eq.131) then
          sub50=" dribu/buoy"
       else if (isubcode.eq.132) then
          sub50=" bathy"
       else if (isubcode.eq.133) then
          sub50=" tesac"
       else
          sub50=""
       end if
    else if (icode.eq.4) then ! Single level 
       typ50="Single level"
       if (isubcode.eq.141) then
          sub50=" codar"
       else if (isubcode.eq.142) then
          sub50=" airep"
       else if (isubcode.eq.143) then
          sub50=" colba"
       else if (isubcode.eq.144) then
          sub50=" amdar"
       else if (isubcode.eq.145) then
          sub50=" acar"
       else
          sub50=""
       end if
    else if (icode.eq.12) then ! Satellite 
       typ50="Satellite"
       if (isubcode.eq.121) then
          sub50=" uwa"
       else if (isubcode.eq.122) then
          sub50=" uwi"
       else if (isubcode.eq.123) then
          sub50=" ura"
       else if (isubcode.eq.124) then
          sub50=" uat"
       else if (isubcode.eq.126) then
          sub50=" ssmi (mapped)"
       else if (isubcode.eq.127) then
          sub50=" ssbt (ssmi brightness T)"
       else if (isubcode.eq.136) then
          sub50=" nwi (nsqat)"
       else if (isubcode.eq.138) then
          sub50=" qwi (qscat )"
       else if (isubcode.eq.210) then
          sub50=" asar"
       else if (isubcode.eq.213) then
          sub50=" ra-2"
       else if (isubcode.eq.220) then
          sub50=" aatsr"
       else if (isubcode.eq.214) then
          sub50=" jason"
       else
          sub50=""
       end if
    else if (icode.eq.7) then ! Tropical 
       typ50="Tropical"
       if (isubcode.eq.31) then
          sub50=" tropical storm"
       else
          sub50=""
       end if
    else if (icode.eq.8) then ! Chemical 
       typ50="Chemical"
       if (isubcode.eq.157) then
          sub50=" Surface ozone"
       else if (isubcode.eq.158) then
          sub50=" Surface ozone average"
       else if (isubcode.eq.159) then
          sub50=" Vertical sounding ozone"
       else if (isubcode.eq.170) then
          sub50=" O3 layers"
       else
          sub50=""
       end if
    else if (icode.eq.253) then ! Bogus 
       typ50="Bogus"
       if (isubcode.eq.164) then
          sub50=" paob"
       else
          sub50=""
       end if
    else
       typ50=""
       sub50=""
   end if
   ! write(*,*)myname,'Debug:',icode,isubcode,typ50,sub50
 end subroutine observation_getType

  character*250 function observation_getCodeValue(icode,isubcode,crc250,irc) 
    implicit none
    integer :: icode
    integer :: isubcode
    character*250 :: crc250
    integer :: irc
    integer :: left,right,bingo
    type(table), pointer :: ptable
    character*22 :: myname="observation_getCodeValue"
    if (.not.ctableInit) then
       !
       call observation_initCodeTable(irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250,"Error return from observation_initCodeTable.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       ctableInit=.true.
    end if
    call observation_heapsearch1i(ctable%maxnn,ctable%codes, &
         & ctable%nn,ctable%index,icode,left,right)
    if (left.eq.right) then
       bingo=left
       call observation_heapsearch1i(ctable%tables(bingo)%maxnn,ctable%tables(bingo)%subcodes, &
            & ctable%tables(bingo)%nn,ctable%tables(bingo)%index,isubcode,left,right)
       if (left.eq.right) then
          observation_getCodeValue=ctable%tables(bingo)%values(left)
       else
          observation_getCodeValue=""
       end if
    else
       observation_getCodeValue=""
    end if
    return
  end function observation_getCodeValue
  !
  subroutine observation_initCodeTable(irc) 
    integer :: irc
    integer :: iunit
    integer, external :: ftunit
    character*250 :: val250
    integer :: code,scnt,subcode,lcnt,cnt,ii
    integer :: lent,lenv,pos,line
    integer, external :: length
    logical :: bdone
    character*22 :: myname ="observation_initCodeTable"
    call chop0(c250,250)
    lent=length(c250,250,10)
    if (lent.ne.0) then
       do ii=1,2
          cnt=0
          unit=ftunit(irc)
          ! open file
          write(*,*) myname,'Opening: ',c250(1:lent)
          open(unit=iunit,file=c250(1:lent),iostat=irc,status="old",&
               & FORM='FORMATTED',ACCESS='SEQUENTIAL')
          if (irc.ne.0) then
             write(*,*) myname,'Unable to open: ',c250(1:lent)
             return
          end if
          line=0
          read(iunit,'(I6,X,I4,X,I8,X,I2,X,A)',iostat=irc) code, scnt, subcode, lcnt, val250
          line=line+1
          if (irc.ne.0) then
             write(*,*) myname,'Unable to read: ',c250(1:lent)
             return
          end if
          bdone=irc.ne.0
          do while (.not. bdone) 
             call getRest(val250,iunit,lcnt-1)
             line=line+lcnt-1
             cnt=cnt+1
             if (ii.eq.2) then
                ctable%codes(cnt) = code
                allocate(ctable%tables(cnt),stat=irc)
                ctable%index(cnt) = cnt
                ctable%tables(cnt)%code=code
                ctable%tables(cnt)%maxnn=scnt;
                ctable%tables(cnt)%nn=scnt;
                allocate(ctable%tables(cnt)%subcodes(scnt),ctable%tables(cnt)%values(scnt),ctable%tables(cnt)%index(scnt),stat=irc);
                ctable%tables(cnt)%subcodes(1)=subcode;
                ctable%tables(cnt)%values(1)=val250;
                ctable%tables(cnt)%index(1)=1;
             end if
             do pos = 2,scnt
                read(iunit,'(12X,I8,X,I2,X,A)',iostat=irc) subcode, lcnt, val250
                line=line+1
             if (irc.eq.0) then
                   call getRest(val250,iunit,lcnt-1)
                   line=line+lcnt-1
                   call chop0(val250,250)
                   lenv=length(val250,250,10)
                   if (lenv>80) then
                      val250=val250(1:75)//"..."
                   end if
                   if (ii.eq.2) then
                      ctable%tables(cnt)%subcodes(pos)=subcode
                      ctable%tables(cnt)%values(pos)=val250
                      ctable%tables(cnt)%index(pos)=pos
                   end if
                else
                   write(*,*)myname,'Error reading line:',line
                end if
             end do
             read(iunit,'(I6,X,I4,X,I8,X,I2,X,A)',iostat=irc) code, scnt, subcode, lcnt, val250
             bdone=(irc.ne.0)
             line=line+1
             irc=0
          end do
          close(iunit,iostat=irc)
          if (ii.eq.1) then
             ctable%maxnn=cnt;
             ctable%nn=cnt;
             allocate(ctable%codes(cnt),ctable%tables(cnt),ctable%index(cnt),stat=irc);
          end if
       end do
    end if
  end subroutine observation_initCodeTable

  subroutine getRest(val250,iunit,lcnt)
    character*250 :: val250
    integer :: iunit
    integer :: lcnt
    integer :: irc
    character*250 :: buff250
    integer :: lenv
    integer, external :: length
    lenv=10
    call chop0(val250,250)
    lenv=length(val250,250,lenv)
    do while (lcnt.gt.0)
       read(iunit,'(22X,A)',iostat=irc) buff250
       val250=val250(1:lenv)//buff250
       call chop0(val250,250)
       lenv=length(val250,250,lenv)
       lcnt = lcnt-1;
    end do
  end subroutine getRest
  !
  ! E R R O R    R O U T I N E S
  !
  subroutine observation_errorappend(crc250,string)
    implicit none
    character*250 :: crc250
    character*(*) :: string
    character*250 :: buff250
    integer :: lenc, lenb
    integer, external :: length
    character*22 :: myname ="observation_errorappend"
    call chop0(crc250,250)
    lenc=length(crc250,250,10)
    lenb=len(trim(string))
    buff250=string(1:lenb)
    if (lenc.eq.0) then
       crc250=buff250(1:lenb)
    else
       crc250=crc250(1:lenc)//""//buff250(1:min(250-lenc-1,lenb))
    end if
  end subroutine observation_errorappend
  subroutine observation_errorappendi(crc250,inum)
    implicit none
    character*250 :: crc250
    integer :: inum
    character*250 :: buff250
    integer :: lenc, lenb
    integer, external :: length
    character*22 :: myname ="observation_errorappendi"
    call chop0(crc250,250)
    lenc=length(crc250,250,10)
    write(buff250,'(I12)')inum
    call chop0(buff250,250)
    lenb=length(buff250,250,1)
    if (lenc.eq.0) then
       crc250=buff250(1:lenb)
    else
       crc250=crc250(1:lenc)//""//buff250(1:min(250-lenc-1,lenb))
    end if
  end subroutine observation_errorappendi
  !
  ! S O R T I N G   R O U T I N E S
  !
  subroutine observation_heapsearch1r(maxnn,key,eps,nn,ind,tkey,left,right)
    !
    implicit none
    !
    integer :: maxnn
    real :: key(maxnn)
    real :: eps ! tolerance
    integer :: nn
    integer :: ind(nn)
    real :: tkey
    integer :: left
    integer :: right
    !
    real :: mid
    integer :: mfl,mcl,kfl,kcl,mch
    logical bdone
    !
    if (nn.eq.0) then
       left=-1                ! first element regardless of value
       return
    end if
    !
    left = 1
    right = nn
    do
       mid=float(left+right)/2.0D0
       mfl=floor(mid)
       mcl=ceiling(mid)
       kfl=observation_cmpr(tkey,key(ind(mfl)),eps)
       kcl=observation_cmpr(tkey,key(ind(mcl)),eps)
       !write(*,'(X,A,X,I3,F0.2,5(X,I3),3(X,F0.2),2(X,I0))')'observation_heapsearch:',left,mid,right,mfl,mcl,kfl,kcl,&
       !& tkey,key(ind(mfl)),key(ind(mcl)),ind(mfl),ind(mcl)
       if (kfl.eq.0) then        ! target is at ceiling => exit
          left=mfl
          right=mfl
          exit
       else if (kcl.eq.0) then   ! target is at floor => exit
          left=mcl
          right=mcl
          exit
       else if (kfl.gt.0) then   ! target is lower than floor
          IF (left.eq.right) then
             right=mfl-1
             exit ! out of bounds -> exit
          else
             right=mfl
          end if
       else if (kcl.lt.0) then   ! target is higher than ceiling
          if (left.eq.right) then
             left=mcl+1
             exit ! out of bounds -> exit
          else
             left=mcl
          end if
       else                      ! target is between floor and ceiling => exit
          left=mfl
          right=mcl
          exit
       end if
    end do
    IF (left > right) return
    !find first match...
    bdone=(left<2)
    do while (.not.bdone)
       mch=observation_cmpr(tkey, key(ind(left-1)),eps)
       if (mch == 0) then ! equal or target is below
          left=left-1
          bdone=(left<2)
       else
          bdone=.true.
       end if
    end do
    !find last match
    bdone=(right>nn-1)
    do while (.not.bdone)
       mch=observation_cmpr(tkey, key(ind(right+1)),eps)
       if (mch == 0) then ! equal or target is above
          right=right+1
          bdone=(right>nn-1)
       else
          bdone=.true.
       end if
    end do
    !
  end subroutine observation_heapsearch1r
  !
  subroutine observation_heapsearch1i(maxnn,key,nn,ind,tkey,left,right)
    !
    implicit none
    !
    integer :: maxnn
    integer, allocatable :: key(:)
    integer :: nn
    integer,allocatable :: ind(:)
    integer :: tkey
    integer :: left
    integer :: right
    !
    real :: mid
    integer :: mfl,mcl,kfl,kcl,mch
    logical bdone
    !
    if (nn.eq.0) then
       left=-1                ! first element regardless of value
       return
    end if
    !
    left = 1
    right = nn
    do
       mid=float(left+right)/2.0D0
       mfl=floor(mid)
       mcl=ceiling(mid)
       kfl=observation_cmpi(tkey,key(ind(mfl)))
       kcl=observation_cmpi(tkey,key(ind(mcl)))
       !write(*,'(X,A,X,I3,F0.2,5(X,I3),3(X,F0.2),2(X,I0))')'observation_heapsearch:',left,mid,right,mfl,mcl,kfl,kcl,&
       !& tkey,key(ind(mfl)),key(ind(mcl)),ind(mfl),ind(mcl)
       if (kfl.eq.0) then        ! target is at ceiling => exit
          left=mfl
          right=mfl
          exit
       else if (kcl.eq.0) then   ! target is at floor => exit
          left=mcl
          right=mcl
          exit
       else if (kfl.gt.0) then   ! target is lower than floor
          IF (left.eq.right) then
             right=mfl-1
             exit ! out of bounds -> exit
          else
             right=mfl
          end if
       else if (kcl.lt.0) then   ! target is higher than ceiling
          if (left.eq.right) then
             left=mcl+1
             exit ! out of bounds -> exit
          else
             left=mcl
          end if
       else                      ! target is between floor and ceiling => exit
          left=mfl
          right=mcl
          exit
       end if
    end do
    IF (left > right) return
    !find first match...
    bdone=(left<2)
    do while (.not.bdone)
       mch=observation_cmpi(tkey, key(ind(left-1)))
       if (mch == 0) then ! equal or target is below
          left=left-1
          bdone=(left<2)
       else
          bdone=.true.
       end if
    end do
    !find last match
    bdone=(right>nn-1)
    do while (.not.bdone)
       mch=observation_cmpi(tkey, key(ind(right+1)))
       if (mch == 0) then ! equal or target is above
          right=right+1
          bdone=(right>nn-1)
       else
          bdone=.true.
       end if
    end do
    !
  end subroutine observation_heapsearch1i
  !
  subroutine observation_heapsort1r(mm,key1,eps,newnn,nn,ind,uniq)
    !
    !! Generate sorted index for key1 
    !
    implicit none

    integer :: mm                ! Number of elements
    real :: key1(mm)             ! key
    real :: eps                  ! key tolerance (when are they equal)
    integer :: newnn             ! new number of keys
    integer :: nn                ! Number of elements
    integer :: ind(nn)           ! Resulting sorted index
    logical uniq               ! Ignore duplicate records
    !
    integer :: ii,dmp

    if (nn.eq.0) then
       newnn=0
       return
    end if
    !
    do ii = nn/2, 1, -1
       call observation_pushdownr(ii, nn, mm,key1,eps,newnn,nn,ind)
    end do
    do ii = nn, 2, -1
       call observation_swap(ind(1), ind(ii))
       call observation_pushdownr(1, ii-1, mm,key1,eps,newnn,nn,ind)
    end do
    !
    if (uniq) then
       dmp=0
       newnn=1
       do ii=2,nn
          if (observation_cmpr(key1(ind(ii-1)),key1(ind(ii)),eps) /= 0) then
             ! Keep ind(ii)
             newnn = newnn+1
             ind(newnn) = ind(ii)
          else
             dmp=dmp+1
          end if
       end do
       if(bdeb)write(*,*)"OBSERVATION_HEAPSORT dumped elements:",dmp
    else
       newnn=nn
    end if
    !
    !
  end subroutine observation_heapsort1r
  !
  subroutine observation_heapsort1i(mm,key1,newnn,nn,ind,uniq)
    !
    !! Generate sorted index for key1 
    !
    implicit none

    integer :: mm                ! Number of elements
    integer :: key1(mm)             ! key
    integer :: newnn             ! new number of keys
    integer :: nn                ! Number of elements
    integer :: ind(nn)           ! Resulting sorted index
    logical uniq               ! Ignore duplicate records
    !
    integer :: ii,dmp

    if (nn.eq.0) then
       newnn=0
       return
    end if
    !
    do ii = nn/2, 1, -1
       call observation_pushdowni(ii, nn, mm,key1,newnn,nn,ind)
    end do
    do ii = nn, 2, -1
       call observation_swap(ind(1), ind(ii))
       call observation_pushdowni(1, ii-1, mm,key1,newnn,nn,ind)
    end do
    !
    if (uniq) then
       dmp=0
       newnn=1
       do ii=2,nn
          if (observation_cmpi(key1(ind(ii-1)),key1(ind(ii))) /= 0) then
             ! Keep ind(ii)
             newnn = newnn+1
             ind(newnn) = ind(ii)
          else
             dmp=dmp+1
          end if
       end do
       if(bdeb)write(*,*)"OBSERVATION_HEAPSORT dumped elements:",dmp
    else
       newnn=nn
    end if
    !
    !
  end subroutine observation_heapsort1i
  !
  subroutine observation_pushdownr(first, last,mm,key1,eps,newnn,nn,ind)
    !
    integer :: first
    integer :: last
    integer :: mm                ! Number of elements
    real :: key1(mm)             ! key
    real :: eps                  ! key tolerance (when are they equal)
    integer :: newnn             ! new number of keys
    integer :: nn                ! Number of elements
    integer :: ind(nn)           ! Resulting sorted index
    !
    integer :: r
    !
    r = first
    !
    MAINLOOP: do while (r <= last/2)
       if (last == 2*r) then
          if (observation_cmpr(key1(ind(r)),key1(ind( 2*r)),eps) > 0) then
             call observation_swap(ind(r), ind(2*r))
          end if
          exit MAINLOOP
       else
          if (observation_cmpr(key1(ind(r)),key1(ind(2*r)),eps) > 0 .and. &
               & observation_cmpr(key1(ind(2*r)),key1(ind(2*r+1)),eps) <= 0) then
             call observation_swap(ind(r), ind(2*r))
             r = 2*r
          else if (observation_cmpr(key1(ind(r)),key1(ind(2*r+1)),eps)>0 .and. &
               & observation_cmpr(key1(ind(2*r+1)),key1(ind(2*r)),eps)<0) then
             call observation_swap(ind(r), ind(2*r+1))
             r = 2*r+1
          else
             exit MAINLOOP
          end if
       end if
    end do MAINLOOP
    !
  end subroutine observation_pushdownr
  !
  subroutine observation_pushdowni(first, last,mm,key1,newnn,nn,ind)
    !
    integer :: first
    integer :: last
    integer :: mm                ! Number of elements
    integer :: key1(mm)          ! key
    integer :: newnn             ! new number of keys
    integer :: nn                ! Number of elements
    integer :: ind(nn)           ! Resulting sorted index
    !
    integer :: r
    !
    r = first
    !
    MAINLOOP: do while (r <= last/2)
       if (last == 2*r) then
          if (observation_cmpi(key1(ind(r)),key1(ind( 2*r))) > 0) then
             call observation_swap(ind(r), ind(2*r))
          end if
          exit MAINLOOP
       else
          if (observation_cmpi(key1(ind(r)),key1(ind(2*r))) > 0 .and. &
               & observation_cmpi(key1(ind(2*r)),key1(ind(2*r+1))) <= 0) then
             call observation_swap(ind(r), ind(2*r))
             r = 2*r
          else if (observation_cmpi(key1(ind(r)),key1(ind(2*r+1)))>0 .and. &
               & observation_cmpi(key1(ind(2*r+1)),key1(ind(2*r)))<0) then
             call observation_swap(ind(r), ind(2*r+1))
             r = 2*r+1
          else
             exit MAINLOOP
          end if
       end if
    end do MAINLOOP
    !
  end subroutine observation_pushdowni
  !
  !
  integer function observation_cmpr(a,b,eps)
    real :: a
    real :: b
    real :: eps
    if (abs(a-b) < eps) then
       observation_cmpr = 0
    else if (a < b) then
       observation_cmpr = 1
    else
       observation_cmpr = -1
    end if
  end function observation_cmpr
  !
  integer function observation_cmpi(a,b)
    integer :: a
    integer :: b
    if (a == b) then
       observation_cmpi = 0
    else if (a < b) then
       observation_cmpi = 1
    else
       observation_cmpi = -1
    end if
  end function observation_cmpi
  !
  !
  subroutine observation_swap(k1, k2)
    !
    implicit none
    !
    integer :: k1
    integer :: k2
    !
    integer :: tmp
    !
    tmp = k1
    k1 = k2
    k2 = tmp
    !
  end subroutine observation_swap

  character*250 function observation_pretty(varname,ndims,dimnames,start,vsize)
    character*80 :: varname
    integer :: ndims
    character*80 :: dimnames(ndims)
    integer :: start(ndims)
    integer :: vsize(ndims)
    integer, external :: length
    integer :: lenv, lend, lenb, lenx
    character*250 :: xuff250, yuff250,buff250
    integer :: ii
    character*22 :: myname = "observation_pretty"
    buff250=""
    lenb=0
    do ii=1,ndims
       lend=length(dimnames(ii),80,10)
       if(bdeb)write(*,*) "observation_pretty  dimnames:",dimnames(ii)(1:lend),start(ii),vsize(ii)
       if (vsize(ii).gt.1) then
          write(yuff250,*)vsize(ii);call chop0(yuff250,250);lenx=length(yuff250,250,2)
          write(xuff250,'(I8,"+",A)')start(ii),yuff250(1:lenx);call chop0(xuff250,250);lenx=length(xuff250,250,2)
       else
          write(xuff250,'(I8)')start(ii);call chop0(xuff250,250);lenx=length(xuff250,250,2)
       end if
       xuff250=dimnames(ii)(1:lend)//"["//xuff250(1:lenx)//"]";call chop0(xuff250,250);lenx=length(xuff250,250,2)
       if(bdeb)write(*,*)'Observation_Pretty here:',xuff250(1:lenx)
       if (lenb.eq.0) then
          buff250=xuff250(1:lenx)
       else
          buff250=buff250(1:lenb)//","//xuff250(1:lenx)
       end if
       call chop0(buff250,250)
       lenb=length(buff250,250,10)
    end do
    lenb=length(buff250,250,10)
    if(bdeb)write(*,*)'Observation_Pretty there:',buff250(1:lenb)
    lenv=length(varname,80,10)
    xuff250=varname(1:lenv)//"("//buff250(1:lenb)//")";
    call chop0(xuff250,250);
    observation_pretty=xuff250
  end function observation_pretty

end module observations
#__file: 'obs_getNextFile.F90' 0100664    **DO NOT DELETE**
subroutine obs_getnextfile(sid,s25,e25,maxrep,nrep,rep250,crc250,irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  integer :: nrep
  character*250 :: rep250(maxrep)
  character*25 :: s25,e25
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "getNextfile"
  !write(*,*) myname,' Entering.',irc
  call observation_getNextFile(sid,maxrep,nrep,rep250,s25,e25,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_stacknext.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_getnextfile
#__file: 'obs_getNextFileLen.F90' 0100664    **DO NOT DELETE**
subroutine obs_getnextfilelen(sid,s25,e25,maxrep,crc250,irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  character*25 :: s25,e25
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "getNextfileLen"
  call observation_getNextFileLen(sid,maxrep,s25,e25,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_stacknextlen.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  return
end subroutine obs_getnextfilelen
#__file: 'obs_getPrevFile.F90' 0100664    **DO NOT DELETE**
subroutine obs_getprevfile(sid,s25,e25,maxrep,nrep,rep250,crc250,irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  integer :: nrep
  character*250 :: rep250(maxrep)
  character*25 :: s25,e25
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "getprevfile"
  !write(*,*) myname,' Entering.',irc
  call observation_getPrevFile(sid,maxrep,nrep,rep250,s25,e25,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_stackprev.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.',nrep
  return
end subroutine obs_getprevfile
#__file: 'obs_getPrevFileLen.F90' 0100664    **DO NOT DELETE**
subroutine obs_getprevfilelen(sid,s25,e25,maxrep,crc250,irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  character*25 :: s25,e25
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "getPrevfileLen"
  !write(*,*) myname,' Entering.',irc
  call observation_getPrevFilelen(sid,maxrep,s25,e25,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_stackprevlen.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.',maxrep
  return
end subroutine obs_getprevfilelen
#__file: 'obs_getTableCValue.F90' 0100664    **DO NOT DELETE**
subroutine obs_getTableCValue(sid, code, subcode, val250, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: code
  integer :: subcode
  character*250 :: val250      ! bufr table c path
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "obs_getTableCValue"
  !write(*,*) myname, 'Entering.',irc,sid
  val250=observation_getCodeValue(code,subcode,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_getCodeValue.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_getTableCValue
#__file: 'obs_ignoreLabel.F90' 0100664    **DO NOT DELETE**
subroutine obs_ignoreLabel(sid, lab250, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: lab250      ! bufr table path
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "obs_ignoreLabel"
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_ignorelabel(sid,lab250,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_ignoreLabel.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_ignoreLabel
#__file: 'obs_loadCache.F90' 0100664    **DO NOT DELETE**
subroutine obs_loadCache(sid, path, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: path
  character*250 :: crc250
  integer :: irc
  character*250 :: buff250
  integer :: lenc
  character*25 :: myname = "obs_loadCache"
  !write(*,*) myname,'Entering.',irc,sid,path
  call observation_loadcache(sid,path,crc250,irc)
  if (irc.ne.0) then
     !write(*,*) 'pushFile Error.'
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_loadcache.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.',irc,sid
  return
end subroutine obs_loadCache
#__file: 'obs_makeCache.F90' 0100664    **DO NOT DELETE**
subroutine obs_makeCache(sid, path, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: path
  character*250 :: crc250
  integer :: irc
  character*250 :: buff250
  integer :: lenc
  character*25 :: myname = "obs_makeCache"
  !write(*,*) myname,'Entering.',irc,sid,path
  call observation_makecache(sid,path,crc250,irc)
  if (irc.ne.0) then
     !write(*,*) 'pushFile Error.'
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_makecache.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.',irc,sid
  return
end subroutine obs_makeCache
#__file: 'obs_openSession.F90' 0100664    **DO NOT DELETE**
subroutine obs_opensession(sid, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "obs_opensession"
  !write(*,*) myname,'Entering.',irc
  call observation_opensession(sid,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_openSession.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.',sid
  return
end subroutine obs_opensession
#__file: 'obs_peekFile.F90' 0100664    **DO NOT DELETE**
subroutine obs_peekfile(sid,maxrep, nrep, rep250, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  integer :: nrep
  character*250 :: rep250(maxrep)
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "peekFile"
  call observation_stackpeek(sid,maxrep,nrep,rep250,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_stackpeek.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  return
end subroutine obs_peekfile
#__file: 'obs_peekFileLen.F90' 0100664    **DO NOT DELETE**
subroutine obs_peekfilelen(sid,maxrep, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "peekFileLen"
  call observation_stackpeeklen(sid,maxrep,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_stackpeeklen.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  return
end subroutine obs_peekfilelen
#__file: 'obs_popFile.F90' 0100664    **DO NOT DELETE**
subroutine obs_popfile(sid,path250,crc250,irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: path250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "popFile"
  !write(*,*) myname,'Entering.',irc
  call observation_stackpop(sid,path250,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_stackpop.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.'
  return
end subroutine obs_popfile
#__file: 'obs_pushFile.F90' 0100664    **DO NOT DELETE**
subroutine obs_pushfile(sid, path, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: path
  character*250 :: crc250
  integer :: irc
  character*250 :: buff250
  integer :: lenc
  character*25 :: myname = "obs_pushFile"
  !write(*,*) myname,'Entering.',irc,sid,path
  call observation_stackpush(sid,path,crc250,irc)
  if (irc.ne.0) then
     !write(*,*) 'pushFile Error.'
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_stackpush.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.',irc,sid
  return
end subroutine obs_pushfile
#__file: 'obs_pushtarget.F90' 0100664    **DO NOT DELETE**
subroutine obs_pushtarget(sid,trg80,pos250,descr80,info250,&
     & min80,max80,crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*80 :: trg80      ! target name
  character*250 :: pos250      ! target name
  character*80 :: descr80      ! target name
  character*250 :: info250      ! target name
  character*80 :: min80      ! target name
  character*80 :: max80      ! target name
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "pushtarget"
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_pushtarget(sid,trg80,pos250,descr80,info250,&
       & min80,max80,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_pushtarget.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_pushtarget
#__file: 'obs_setIndex.F90' 0100664    **DO NOT DELETE**
subroutine obs_setIndex(sid, trg80, exp250, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*80 :: trg80      ! target name
  character*250 :: exp250    ! expression
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setIndex"
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_setIndex(sid,trg80,exp250,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_setIndex.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_setIndex
#__file: 'obs_setIndexSpan.F90' 0100664    **DO NOT DELETE**
subroutine obs_setIndexSpan(sid, s25,e25, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*25 :: s25,e25      ! start/end index
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "clearFileStack"
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_setIndexSpan(sid,s25,e25,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_setIndexSpan.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_setIndexSpan
#__file: 'obs_setTableC.F90' 0100664    **DO NOT DELETE**
subroutine obs_setTableC(sid, path250, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: path250      ! bufr table c path
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "obs_setTableC"
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_settablec(sid,path250,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_settablec.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_setTableC
#__file: 'obs_setTablePath.F90' 0100664    **DO NOT DELETE**
subroutine obs_setTablePath(sid, path250, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: path250      ! bufr table path
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "obs_setTablePath"
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_settablepath(sid,path250,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_stackclear.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_setTablePath
#__file: 'obs_setTimeSpan.F90' 0100664    **DO NOT DELETE**
subroutine obs_setTimeSpan(sid, s25,e25, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*25 :: s25,e25      ! start/end time
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "clearFileStack"
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_setTimeSpan(sid,s25,e25,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_setTimeSpan.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_setTimeSpan
#__file: 'obs_setType.F90' 0100664    **DO NOT DELETE**
subroutine obs_setType(sid, category, subCategory, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: category
  integer :: subCategory
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setType"
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_setType(sid,category,subCategory,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_setType.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_setType
#__file: 'obs_sliceCurrentFile.F90' 0100664    **DO NOT DELETE**
subroutine obs_sliceCurrentFile(sid,maxrep, nrep, rep250, maxarr, narr, arr, ibok, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  integer :: nrep
  character*250 :: rep250(maxrep)
  integer :: maxarr
  integer :: narr
  real :: arr(maxarr)
  integer :: ibok
  character*250 :: crc250
  integer :: irc
  !
  character*25 :: myname = "obs_sliceCurrentFile"
  logical :: bok
  !
  !write(*,*) myname,' Entering.',maxarr,narr,ibok,irc
  bok=.true.
  call observation_slicecurrentfile(sid,maxrep,nrep,rep250,maxarr,narr,arr,bok,crc250,irc)
  if (irc.ne.0) then
     ibok=0 ! abort and process error message
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_slicecurrentfile.")
     call observation_errorappendi(crc250,irc)
     ! write(*,*)myname,'Aborting with irc:',irc,ibok
     return
  else
     if (bok) then
        ibok=1 ! results are ok
     else
        ibok=0 ! ignore the results and continue...
     end if
  end if
  !write(*,*) myname,' Done.',narr
  return
end subroutine obs_sliceCurrentFile
#__file: 'obs_sliceCurrentFileLen.F90' 0100664    **DO NOT DELETE**
subroutine obs_sliceCurrentFileLen(sid,maxrep,maxarr,ibok,crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  integer :: maxarr
  integer :: ibok
  character*250 :: crc250
  integer :: irc
  !
  character*25 :: myname = "obs_sliceCurrentFileLen"
  logical bok
  !
  bok=.true.;ibok=1;
  !write(*,*) myname,' Entering.',ibok
  call observation_slicecurrentfilelen(sid,maxrep,maxarr,bok,crc250,irc)
  if (irc.ne.0) then
     ibok=0 ! abort and process error message
     maxrep=1
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_slicecurrentfilelen.")
     call observation_errorappendi(crc250,irc)
     ! write(*,*)myname,'Aborting with irc:',irc,ibok
     return
  else
     if (bok) then
        ibok=1 ! results are ok
     else
        ibok=0 ! ignore the results and continue...
        maxrep=1
        maxarr=1
     end if
  end if
  !write(*,*) myname,' Done.',ibok
  return
end subroutine obs_sliceCurrentFileLen
