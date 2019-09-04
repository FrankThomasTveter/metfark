!
!"*********************************************************************************************"
!"***This software is ONLY permitted used by staff at the Norwegian Meteorological Institute***"
!"*********************************************************************************************"
!
module observations
  use parse
  IMPLICIT NONE
  !
  ! Global constants
  !
  CHARACTER(LEN=50)               :: blank50 = ''
  character*1 :: sep = "|"
  logical                         :: obs_bdeb=.false.
  !
  ! bufrdc PARAMETERS
  integer,parameter :: JSUP   =       9
  integer,parameter :: JSEC0  =       3
  integer,parameter :: JSEC1  =      80
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
  integer :: isubset=0           ! current subset
  integer :: nitem = 0           ! number of items in a subset
  integer :: irep=0
  integer :: NBYTPW
  integer :: kbufl
  integer :: ktdlen
  integer :: KEL
  integer :: ktdexl
  integer,parameter :: ktdexl_max=250
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
  integer :: KTDEXP(0:JELEM)
  CHARACTER*64 ::CNAMES(KELEM)
  CHARACTER*24 ::CUNITS(KELEM)
  CHARACTER*80 ::CVALS(KELEM)
  REAL*8 :: RVIND=1.7D38
  integer :: nvind=2147483647
  integer*8 :: unit
  logical :: fopen=.false. ! is file open
  logical :: bread=.false.
  !
  ! category used for counting observations categories in file
  !
  type :: obs_subCategory
     integer :: subcategory = 0
     integer :: cnt = 0
     integer :: ktdexl = 0
     integer, allocatable :: ktdexp(:)
     character*64, allocatable :: cnames(:)
     character*24, allocatable :: cunits(:)
     real*8, allocatable :: values(:)
     type(obs_subCategory), pointer :: prev
     type(obs_subCategory), pointer :: next
  end type obs_subCategory
  !
  type :: obs_mainCategory
     integer :: category = 0
     integer :: cnt = 0
     type(obs_subCategory) :: firstSubCategory
     type(obs_subCategory) :: lastSubCategory
     integer :: nsub=0
     type(obs_mainCategory), pointer :: prev
     type(obs_mainCategory), pointer :: next
  end type obs_mainCategory
  !
  !
  type :: obs_message
     integer :: nobs=0 ! number of observations
     integer :: vobs=0 ! number of valid observations
     integer :: iobs=0  ! current observation
     logical :: check=.false. ! has message check been performed yet...
     !
     integer :: ctrg = 0 ! number of allocated targets
     integer :: cobs = 0 ! number of allocated observations
     real, allocatable :: trg_val(:,:)   ! target values
     logical, allocatable :: trg_vok(:,:)   ! target values ok?
     logical, allocatable :: trg_set(:)  ! did observation pass message check?
     real, allocatable :: trg_res(:)     ! filter values
  end type obs_message
  !
  ! BUFR FILE STACK
  !
  type :: obs_file
     character*250  :: fn250 = ""          ! file name
     integer        :: lenf=0              ! length of file name string
     character*250  :: tablepath = ""      ! table path
     real           :: ind_start=0.0D0     ! lowest index value in file
     real           :: ind_stop=0.0D0      ! highest index value in file
     logical        :: ind_lim = .false.   ! are ind_start/ind_stop available
     real           :: time_start=0.0D0    ! lowest index value in file
     real           :: time_stop=0.0D0     ! highest index value in file
     logical        :: time_lim = .false.  ! are ind_start/ind_stop available
     integer        :: nmessage=0          ! number of BUFR messages in file
     integer        :: nsubset=0           ! total number of subsets in file
     integer        :: nitem = 0           ! total number of items in file
     type(obs_mainCategory) :: firstCategory
     type(obs_mainCategory) :: lastCategory
     integer :: ncat=0
     integer :: nsub=0
     integer :: mok(10),mrm(10),ook(10),orm(10),lenh(10)
     character*80 :: hint80(10)
     type(obs_file), pointer :: prev => null() ! linked list
     type(obs_file), pointer :: next => null() ! linked list
  end type obs_file
  !
  type :: obs_filePointer
     type(obs_file), pointer :: ptr => null()
  end type obs_filePointer
  !
  ! Target item
  !   
  type :: obs_target
     character*80 :: trg80      ! target name
     character*250 :: pos250    ! position/sequence number
     integer :: type            ! empty, constant, variable, expression
     integer :: ind             ! index to internal variable (0=none)
     character*80 :: descr80    ! descriptor
     character*250 :: info250   ! information
     character*80 :: min80      ! min value
     character*80 :: max80      ! max value
     type(obs_target), pointer :: prev => null()   ! linked list
     type(obs_target), pointer :: next => null()   ! linked list
  end type obs_target
  !
  type :: obs_targetPointer
     type(obs_target), pointer :: ptr => null()
  end type obs_targetPointer
  !
  ! Location item
  !   
  type :: obs_location
     integer :: locid
     integer :: iloc
     integer :: ntrg ! number of targets
     real, allocatable :: trg_val(:) ! target value
     logical, allocatable :: trg_vok(:) ! target value ok
     integer :: search = 0     ! is grid search successful, 0=ok?
     logical :: bok = .true.
     type(obs_location), pointer :: prev => null()   ! linked list
     type(obs_location), pointer :: next => null()   ! linked list
  end type obs_location
  !
  type :: obs_locPointer
     type(obs_location), pointer :: ptr => null()
  end type obs_locPointer
  !
  ! report
  !
  type :: obs_reportItem
     character*250 :: desc250 ! description
     type(obs_reportItem), pointer :: prev => null()   ! linked list
     type(obs_reportItem), pointer :: next => null()   ! linked list
  end type obs_reportItem
  !
  type :: obs_report
     type(obs_reportItem), pointer :: firstItem => null()    ! linked list start
     type(obs_reportItem), pointer :: lastItem => null()     ! linked list end
     type(obs_reportItem), pointer :: currentItem => null()  ! current pointer
     integer :: nitem = 0                           ! number of items
     type(obs_report), pointer :: prev => null()   ! linked list
     type(obs_report), pointer :: next => null()   ! linked list
  end type obs_report
  !
  ! BUFR arrays:::  WARNING: WILL PROBABLY USE ALMOST ALL OF YOUR COMPUTER MEMORY!!!!
  !
  ! SESSION VARIABLES
  !
  type :: obs_session
     integer                         :: sid
     CHARACTER(LEN=250)              :: fn250
     CHARACTER(LEN=250)              :: tablepath=""
     !
     ! STACK
     !
     type(obs_file), pointer :: firstFile => null()   ! linked list start
     type(obs_file), pointer :: lastFile => null()    ! linked list end
     type(obs_file), pointer :: currentFile => null()
     type(obs_file), pointer :: nextFile => null()
     type(obs_filePointer), pointer   :: fileStack(:) => null() ! array of the stack elements
     real, allocatable            :: fileStackSort(:,:)
     integer, allocatable         :: fileStackInd(:,:)
     integer :: nFileIndexes = 0              ! total number of files on the stack
     integer :: nFileSortIndexes = 0          ! number of file indexes on the stack
     integer :: newnFileSortIndexes(2)        ! new number of file indexes on the stack
     integer :: currentFileSortIndex = 0      ! current stack index element
     integer :: currentFileIndex = 0          ! current stack element
     logical :: stackReady =.false.           ! are sorted data ready for use?
     integer :: leftFileSortIndex=0           ! ref fileStackSort(*,2) - maxvalues
     integer :: rightFileSortIndex=0          ! ref fileStackSort(*,1) - minvalues
     logical :: sortLimitsOk  = .false.
     integer :: fopened  = 0                 ! number of overlapping observation files
     integer :: fok(10), frm(10)
     !
     ! data selection
     !
     real                            :: ind_minval=0.0D0      ! lowest index
     real                            :: ind_maxval=0.0D0      ! highest index
     logical                         :: ind_lval(2) = .false. ! are ind_minval/ind_maxval available
     !
     character*80                    :: ind_trg80=""          ! index target name
     integer                         :: ind_lent              ! length of target name
     character*250                   :: ind_exp250=""         ! index target expression
     integer                         :: ind_lene              ! length of target expression
     real                            :: ind_val=0.0D0         ! index value (=trg_val(ntrg))
     logical                         :: ind_eset = .false.    ! is index expression set
     type(parse_session), pointer    :: ind_pe => null()      ! index expression
     logical                         :: ind_tset = .false.    ! is index tranformation set
     type(parse_session), pointer    :: ind_pt => null()      ! index transformation
     !
     ! TARGET
     !
     type(obs_target), pointer        :: firsttarget => null()   ! linked list start
     type(obs_target), pointer        :: lasttarget => null()    ! linked list end
     type(obs_target), pointer        :: ctarget => null()    ! linked list end
     integer :: ntarget=0                  ! number of targets
     integer :: ntrg=0                     ! number of targets including index_target
     integer :: category                   ! filter category/bufrType
     integer :: subCategory                ! filter subcategory/subType
     !
     character*80, allocatable       :: trg80(:)        ! target name
     integer, allocatable            :: trg_lent(:)     ! target name length
     character*250, allocatable      :: trg_pos250(:)   ! target position
     integer, allocatable            :: trg_lenp(:)     ! target position length
     !  position can be empty,constant,variable or expression
     integer, allocatable            :: trg_type(:)     ! type of sequence
     integer, allocatable            :: trg_seq(:)      ! constant
     integer, allocatable            :: trg_ind(:)      ! index to internal variable
     type(parse_pointer), pointer    :: trg_psp(:) => null() ! expression parser
     integer, allocatable            :: trg_descr(:)    ! descriptor
     logical, allocatable            :: trg_lval(:,:)   ! above/below/between limits?
     real, allocatable               :: trg_minval(:)
     real, allocatable               :: trg_maxval(:)
     integer, allocatable            :: trg_ook(:)
     integer, allocatable            :: trg_orm(:)
     logical,allocatable             :: trg_req(:)      ! is target required?
     REAL,allocatable                :: trg_val(:)      ! target variable values
     logical,allocatable             :: trg_vok(:)      ! is target valid?
     type(obs_targetPointer), pointer:: trg_ptr(:)=> null()
     logical                         :: trg_set=.false. ! is target list set?
     ! message -> location ratios     
     integer :: omin,omax,osum,ocnt
     integer :: lmin,lmax,lsum,lcnt
     logical :: keepstat=.false.
     !
     ! index to duplication targets (targets that cause observation duplication)
     !
     integer :: ndup=0                    ! number of duplication targets
     integer, allocatable :: dup_ind(:)   ! duplication index
     integer, allocatable :: dup_inc(:)   ! duplication index
     !
     ! dynamic position/sequence variables
     integer                         :: ndyn = 0        ! number of dynamic variables
     CHARACTER (LEN=80), allocatable :: dyn_var(:)      ! dynamic variable name
     integer,            allocatable :: dyn_lenv(:)     ! dynamic variable name length
     !     REAL(rn),           allocatable :: dyn_val(:)      ! dynamic variable values
     REAL,               allocatable :: dyn_val(:)      ! dynamic variable values
     integer                         :: dyn_pos=0       ! dynamic search position
     integer                         :: dyn_max=0       ! dynamic max count
     integer                         :: dyn_cnt=0       ! dynamic count position
     logical                         :: dyn_set=.false. ! is dynamic list set?
     logical                         :: dyn_bok=.false. ! is duplicate observation valid
     !
     ! internal variables...
     integer                         :: nint = 0        ! number of internal variables
     CHARACTER (LEN=80), allocatable :: int_var(:)      ! internal variable name
     integer,            allocatable :: int_lenv(:)     ! internal variable name length
     !REAL(rn),           allocatable :: int_val(:)      ! internal variable values
     REAL,               allocatable :: int_val(:)      ! internal variable values
     !
     ! locations
     type(obs_location), pointer :: firstLoc => null()   ! linked list start
     type(obs_location), pointer :: lastLoc => null()    ! linked list end
     integer :: nloc=0                                   ! number of items in location-chain
     integer :: locoffset = 0                            ! offset between locid and position in locdata
     type(obs_locPointer), allocatable :: locData(:)     !  data locations
     logical :: locReady = .false.
     !
     character*80, allocatable :: var(:)
     real, allocatable :: val(:)
     integer,dimension(8) :: values
     !
     ! report label flags
     !
     logical :: ignmis=.false.
     logical :: ignuni=.false.
     logical :: ignden=.false.
     logical :: ignder=.false.
     logical :: ignval=.false.
     logical :: ignsec=.false.
     logical :: ignarr=.false.
     logical :: igncat=.false.
     !
     type(parse_session), pointer :: psf => null()
     logical ::  flt_set=.false.
     character*250 :: flt250
     integer :: lenf=0
     type(obs_message), pointer :: msg => null()
     !
     ! REPORTS
     !
     type(obs_report), pointer :: firstReport => null()    ! linked list start
     type(obs_report), pointer :: lastReport => null()     ! linked list end
     type(obs_report), pointer :: currentReport => null()  ! current pointer
     type(obs_report), pointer :: nextReport => null()  ! current pointer
     integer :: nsubset=0               ! number of reports
     logical :: reportsReady=.false.    ! are reported data ready for use
     !
     ! output identification...
     !integer :: fid = 0 ! file id (index number)
     !integer :: mid = 0 ! message id in file
     !integer :: oid = 0 ! observation id in file
     !integer :: lid = 0 ! location id in file
     !
     type(obs_session), pointer :: prev => null()         ! linked list
     type(obs_session), pointer :: next => null()         ! linked list
  end type obs_session

  type :: obs_table
     integer :: code
     integer :: maxnn = 0
     integer, allocatable :: subcodes(:)
     character*250, allocatable :: values(:)
     integer :: nn = 0
     integer, allocatable :: index(:)
  end type obs_table
  !
  type :: obs_codeTable
     integer :: maxnn = 0
     integer, allocatable :: codes(:)
     type(obs_table), allocatable :: tables(:)
     integer :: nn = 0
     integer, allocatable :: index(:)
  end type obs_codeTable
  !
  character*250 :: c250 = ""
  type(obs_codeTable), target :: ctable
  logical :: ctableInit=.false.
  !
  integer :: maxid=0 ! session counter
  type(obs_session), pointer :: firstSession => null()   ! linked list start
  type(obs_session), pointer :: lastSession => null()    ! linked list end
  !
CONTAINS
  !
  !###############################################################################
  ! SESSION ROUTINES
  !###############################################################################
  !
  subroutine observation_opensession(sid,css,crc250,irc)
    integer :: sid
    character*250 :: crc250
    integer :: irc
    type(obs_session),pointer :: css !  new session
    character*22 :: myname="observation_opensession"
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
    nullify(css)
    allocate(css,stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Unable to allocate 'new session'.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    !
    call date_and_time(VALUES=css%values) ! get current date
    if (allocated(css%var)) deallocate (css%var)
    if (allocated(css%val)) deallocate (css%val)
    allocate(css%var(2),css%val(2),stat=irc)
    css%var(1)="now"
    css%var(2)="midnight"
    css%val(1)=parse_f1970(&
         & real(css%values(1)),real(css%values(2)),&
         & real(css%values(3)),real(css%values(5)),&
         & real(css%values(6)),real(css%values(7)))
    css%val(2)=parse_f1970(&
         & real(css%values(1)),real(css%values(2)),&
         & real(css%values(3)),0.0D0,&
         & 0.0D0,0.0D0) ! midnight
    !
    maxid=maxid+1
    css%sid=maxid
    css%prev => lastSession%prev
    css%next => lastSession
    css%prev%next => css
    css%next%prev => css
    sid = css%sid
    ! stack
    allocate(css%firstFile,css%lastFile, stat=irc) ! 
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Unable to allocate &
            & 'css%firstFile/css%lastFile'.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    css%firstFile%next => css%lastFile
    css%lastFile%prev => css%firstFile
    ! internal variables
    css%nint=5
    if (allocated(css%int_var)) deallocate(css%int_var)
    if (allocated(css%int_lenv)) deallocate(css%int_lenv)
    if (allocated(css%int_val)) deallocate(css%int_val)
    allocate(css%int_var(css%nint),css%int_lenv(css%nint),&
         & css%int_val(css%nint),stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Unable to allocate internal variables.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    !
    css%int_var(1)="mid" ! model file id
    css%int_var(2)="oid" ! observation file id
    css%int_var(3)="bid" ! message id 
    css%int_var(4)="sid" ! obs id
    css%int_var(5)="lid" ! location id
    !
    css%int_val(1)=0 ! model file id
    css%int_val(2)=0 ! observation file id
    css%int_val(3)=0 ! message id (for each file)
    css%int_val(4)=0 ! obs id (for each message)
    css%int_val(5)=0 ! location id (for each message)
    ! reports
    allocate(css%firstReport,css%lastReport,stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Unable to allocate first/lastReport.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    css%firstReport%next => css%lastReport
    css%lastReport%prev => css%firstReport
    css%nsubset = 0
    ! targets
    call observation_targetinit(css,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from observation_targetInit.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    !
    allocate(css%msg)
    !
    !write(*,*)myname,' Done.'
    return
  end subroutine observation_opensession

  subroutine observation_getSession(css,sid,crc250,irc)
    type(obs_session), pointer :: css !  current session
    integer :: sid
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_getSession"
    !if(obs_bdeb)write(*,*)myname,' Entering.',irc,sid
    if (.not.associated(firstSession)) then
       irc=911
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"No session is opened!")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    css => firstSession%next
    do while ( .not.associated(css,target=lastSession))
       if (css%sid .eq. sid) then
          !if (obs_bdeb) write(*,*)myname,'Exiting with sid:',sid,irc
          return
       end if
       css=>css%next
    end do
    nullify(css)
    irc=342
    call observation_errorappend(crc250,myname)
    call observation_errorappend(crc250,"Invalid session id:")
    call observation_errorappendi(crc250,sid)
    call observation_errorappend(crc250,"\n")
    if(obs_bdeb)write(*,*)myname,'Error.',irc,sid
    return
  end subroutine observation_getSession

  subroutine observation_closeSession(css,crc250,irc)
    character*250 :: crc250
    integer :: irc
    type(obs_session), pointer :: css !  current session
    integer :: ii
    character*22 :: myname="observation_closeSession"
    if(obs_bdeb)write(*,*)myname,'Entering.',irc
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       !
       if (allocated(css%var)) deallocate (css%var)
       if (allocated(css%val)) deallocate (css%val)
       if (allocated(css%dup_ind)) deallocate(css%dup_ind)
       if (allocated(css%dup_inc)) deallocate(css%dup_inc)
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
             if (associated(css%filestack(ii)%ptr)) then
                nullify(css%filestack(ii)%ptr)
             end if
          end do
          deallocate(css%filestack)
       end if
       !
       do ii=1,css%ntrg
          call parse_close(css%trg_psp(ii)%ptr,crc250,irc)
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Error return from parse_close.")
             return
          end if
       end do
          
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
       ! internal variables
       css%nint=0
       if (allocated(css%int_var)) deallocate(css%int_var)
       if (allocated(css%int_lenv)) deallocate(css%int_lenv)
       if (allocated(css%int_val)) deallocate(css%int_val)
       ! deallocate observation filter...
       if (css%lenf.ne.0) then
          call parse_close(css%psf,crc250,irc)
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Error return from parse_close.")
             return
          end if
       end if
       !
       call observation_clearTargetList(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from cleartargetlist.")
          return
       end if
       deallocate(css%msg)
       ! 
       css%prev%next => css%next
       css%next%prev => css%prev
       deallocate(css)
    else
       irc=599
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Attempt to close none-existent session.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    if(obs_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine observation_closeSession
  !
  ! make cache file
  !
  subroutine observation_makeCache(css,path250,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(obs_file), pointer :: currentFile !  current file
    type(obs_mainCategory), pointer :: currentCat !  current file
    type(obs_subCategory), pointer :: currentSub !  current file
    integer, external :: length,ftunit
    integer :: lenp,unitr
    character*250 :: buff250, str250
    character*22 :: myname="observation_makeCache"
    integer :: ii, leno,cnt
    character*250 :: old250
    !if(obs_bdeb)write(*,*) myname,' *** Entering.',irc
    leno=0
    call chop0(path250,250)
    lenp=length(path250,250,20)
    if(obs_bdeb)write(*,*)myname," *** Entering '"//path250(1:lenp)//"'",irc
    ! open file
    unitr=ftunit(irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," no free unit number for "//path250(1:lenp))
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
       call observation_errorappend(crc250," unable to open '"//path250(1:lenp)//"'")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    !
    call observation_makeTargetList(css,crc250,irc) 
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from makeTargetList.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    !
    call observation_sortStack(css,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from sortStack.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    !
    ! write number of files: css%nFileIndexes
    if(obs_bdeb)write(*,*) myname,' Files:',css%nFileIndexes
    leno=0
    cnt=0
    do ii=1,css%newnFileSortIndexes(1)
       currentFile=>css%fileStack(css%fileStackInd(ii,1))%ptr
       if (old250(1:leno).ne.currentFile%fn250(1:currentFile%lenf)) then
          cnt=cnt+1
       end if
       old250=currentFile%fn250
       leno=currentFile%lenf
    end do
    write(unitr,'(I0,8(X,I0))',iostat=irc) cnt,css%values
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," unable to write to "//path250(1:lenp))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    !
    write(*,'(X,A,A,I0,A,I4.4,6(A,I2.2))')myname," Index '"//path250(1:lenp)//&
         & "' contains ",cnt," files. Modified:", &
         & css%values(1),"-",css%values(2),"-",css%values(3),"T",&
         & css%values(5),":",css%values(6),":",css%values(7)
    ! loop over file stack
    leno=0
    do ii=1,css%newnFileSortIndexes(1)
       currentFile=>css%fileStack(css%fileStackInd(ii,1))%ptr
       if (old250(1:leno).ne.currentFile%fn250(1:currentFile%lenf)) then
          write(unitr,'(L1,2(X,F0.10),4(X,I0),X,A)',iostat=irc) &
               & currentFile%ind_lim,&
               & currentFile%ind_start,currentFile%ind_stop,&
               & currentFile%nmessage,currentFile%ncat,currentFile%nsub,&
               & currentFile%lenf,currentFile%fn250(1:currentFile%lenf)
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," unable to write to "//path250(1:lenp))
             call observation_errorappendi(crc250,irc)
             call observation_errorappend(crc250,"\n")
             return
          end if
          !
          if (currentFile%ind_lim) then
             write(*,'(2X,A," <",F0.1,",",F0.1,">")') currentFile%fn250(1:currentFile%lenf),&
                  & currentFile%ind_start,currentFile%ind_stop
          else
             write(*,'(2X,A," <*,*>")') currentFile%fn250(1:currentFile%lenf)
          end if
          ! write category summary
          currentCat=>currentFile%firstCategory%next
          do while (.not.associated(currentCat,target=currentFile%lastCategory)) 
             write(unitr,'(3(X,I0))',iostat=irc) currentCat%category,&
                  & currentCat%cnt,currentCat%nsub
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," unable to write to "//path250(1:lenp))
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
             currentSub=> currentCat%firstSubCategory%next
             do while (.not.associated(currentSub,target=currentCat%lastSubCategory)) 
                write(unitr,'(2(X,I0))',iostat=irc) currentSub%subcategory,currentSub%cnt
                if (irc.ne.0) then
                   call observation_errorappend(crc250,myname)
                   call observation_errorappend(crc250," unable to write to "//path250(1:lenp))
                   call observation_errorappendi(crc250,irc)
                   call observation_errorappend(crc250,"\n")
                   return
                end if
                currentSub=>currentSub%next
             end do
             currentCat=>currentCat%next
          end do
       end if
       old250=currentFile%fn250
       leno=currentFile%lenf
    end do
    ! close file
    close(unitr,iostat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," unable to close "//path250(1:lenp))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if(obs_bdeb)write(*,*)myname,' *** Done.',irc
  end subroutine observation_makeCache
  !
  ! load cache file
  !
  subroutine observation_loadCache(css,path250,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(obs_file),pointer :: newFile
    type(obs_mainCategory), pointer :: newCat !  current file
    type(obs_subCategory), pointer :: newSub !  current file
    integer, external :: length
    integer :: lenp,lenf,lenb,ii,jj,kk,opos,pos,unitr
    character*250 :: buff250
    character*22 :: myname="observation_loadCache"
    character*250 diff250
    integer :: values(8),yy,mm,dd,hh,mi,lend
    real :: sec, f2000,s2000
    !if(obs_bdeb)write(*,*) myname,' *** Entering.',irc
    call chop0(path250,250)
    lenp=length(path250,250,20)
    if(obs_bdeb)write(*,*)myname," *** Entering '"//path250(1:lenp)//"'",irc
    if (lenp.eq.0) then
       irc=377
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Empty cache-file.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
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
    if(obs_bdeb)write(*,*)myname," Opening obscache: '"//path250(1:lenp)//"'"
    open ( unit=unitr, status="old", form="formatted", &
         &        access="sequential", &
         &        iostat=irc, file=path250(1:lenp) )
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," unable to open '"//path250(1:lenp)//"'")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! read number of files: css%nFileIndexes
    read(unitr,'(A)',iostat=irc) buff250
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," unable to read: '"//path250(1:lenp)//"'")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    call chop0(buff250,250)
    lenb=length(buff250,250,10)
    read(buff250(1:lenb),*,iostat=irc) css%nFileIndexes,values
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Corrupt index file "//path250(1:lenp))
       call observation_errorappend(crc250,buff250(1:lenb))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if(obs_bdeb)write(*,*)myname," Indexed files:",css%nFileIndexes
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
       do jj=1,10
          newFile%ook(jj)=0
          newFile%orm(jj)=0
       end do
       !
       read(unitr,'(A)',iostat=irc) buff250
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," unable to read (2) "//path250(1:lenp))
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       call chop0(buff250,250)
       pos=0
       opos=pos
       call findDelimiter(buff250," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%ind_lim
       opos=pos
       call findDelimiter(buff250," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%ind_start
       opos=pos
       call findDelimiter(buff250," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%ind_stop
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
       if (obs_bdeb) then
          if (newFile%ind_lim) then
             write(*,'(X,A,X,A,I0,X,"<",F0.1,",",F0.1,">")') myname,&
                  & "loaded:'"//newFile%fn250(1:newFile%lenf)//"'",ii,newFile%ind_start,newfile%ind_stop
          else
              write(*,'(X,A,X,A,I0,X,"<*,*>")') myname,&
                  & "loaded:'"//newFile%fn250(1:newFile%lenf)//"'",ii
         end if
       end if
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
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," unable to read "//path250(1:lenp))
             call observation_errorappendi(crc250,irc)
             call observation_errorappend(crc250,"\n")
             return
          end if
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
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," unable to read "//path250(1:lenp))
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
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
       call observation_errorappend(crc250," unable to close "//path250(1:lenp))
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    yy=css%values(1)
    mm=css%values(2)
    dd=css%values(3)
    hh=css%values(5)
    mi=css%values(6)
    sec=css%values(7)
    call jd2000(s2000,yy,mm,dd,hh,mi,sec)
    yy=values(1)
    mm=values(2)
    dd=values(3)
    hh=values(5)
    mi=values(6)
    sec=values(7)
    call jd2000(f2000,yy,mm,dd,hh,mi,sec)
    diff250=observation_diff(s2000-f2000)
    lend=length(diff250,250,10)
    write(*,'(X,A,A,I0,A,I4.4,6(A,I2.2),A)')myname," Index '"//path250(1:lenp)//&
         & "' contains ",css%nFileIndexes," files. Modified:", &
         & values(1),"-",values(2),"-",values(3),"T",&
         & values(5),":",values(6),":",values(7),". Age:"//diff250(1:lend)
    if(obs_bdeb)write(*,*)myname,' *** Done.',irc
  end subroutine observation_loadCache
  !
  !###############################################################################
  !STACK ROUTINES
  !###############################################################################
  !
  ! clear the BUFR STACK
  !
  subroutine observation_stackclear(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    integer :: lens
    character*22 :: myname="observation_stackclear"
    if(obs_bdeb)write(*,*)myname,' Entering.'
    ! mark as prepared
    css%stackReady=.false.
    !
    if(obs_bdeb)write(*,*)myname,' Removing files.'
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
    if(obs_bdeb)write(*,*)myname,' Done.'
  end subroutine observation_stackclear
  !
  ! get the bufr table path
  !
  subroutine observation_getTablePath(css,path250,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    path250=css%tablepath
    return
  end subroutine observation_getTablePath
  !
  ! set the bufr table path
  !
  subroutine observation_setTablePath(css,path250,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(obs_file), pointer :: currentFile => null()
    type(obs_file), pointer :: stackNext => null()
    integer, external :: length
    integer :: lenb
    character*250 :: buff250
    character*22 :: myname="observation_setTablePath"
    buff250=path250
    call chop0(buff250,250)
    lenb=length(buff250,250,10)
    if(obs_bdeb)write(*,*)myname,' Entering.'//buff250(1:lenb)
    if (lenb.gt.0.and.lenb.lt.250) then
       if (buff250(lenb:lenb).ne."/") then
          buff250=buff250(1:lenb)//"/"
          call chop0(buff250,250)
          lenb=length(buff250,250,10)
       end if
    end if
    css%tablepath=buff250
    call chop0(css%tablepath,250)
    write(*,*)myname," BUFR table path= '"//buff250(1:lenb)//"'"
    !if(obs_bdeb)write(*,*)myname,' Done. "'//buff250(1:lenb)//'"'
  end subroutine observation_setTablePath
  !
  ! set table c file name
  !
  subroutine observation_setTableC(css,path250,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(obs_file), pointer :: currentFile => null()
    type(obs_file), pointer :: stackNext => null()
    integer, external :: length
    integer :: lens
    character*22 :: myname="observation_setTableC"
    ctableInit=.false.
    c250=path250
    call chop0(c250,250)
    if(obs_bdeb)write(*,*)myname,' Done.'
  end subroutine observation_setTableC
  !
  !
  !
  subroutine observation_setBufrType(css,category,subCategory,crc250,irc)
    type(obs_session), pointer :: css !  current session
    integer :: category
    integer :: subCategory
    character*250 :: crc250
    integer :: irc
    type(obs_file), pointer :: currentFile => null()
    type(obs_file), pointer :: stackNext => null()
    integer, external :: length
    integer :: lens
    character*22 :: myname="observation_setBufrType"
    if(obs_bdeb)write(*,*)myname,' Entering.'
    css%category=category
    css%subCategory=subCategory
    if(obs_bdeb)write(*,*)myname,' Done.'
  end subroutine observation_setBufrType
  !
  !
  subroutine observation_getBufrType(css,category,subCategory,crc250,irc)
    type(obs_session), pointer :: css !  current session
    integer :: category
    integer :: subCategory
    character*250 :: crc250
    integer :: irc
    type(obs_file), pointer :: currentFile => null()
    type(obs_file), pointer :: stackNext => null()
    integer, external :: length
    integer :: lens
    character*22 :: myname="observation_getBufrType"
    if(obs_bdeb)write(*,*)myname,' Entering.'
    category=css%category
    subCategory=css%subCategory
    if(obs_bdeb)write(*,*)myname,' Done.'
  end subroutine observation_getBufrType
  !
  ! remove item from bufr stack
  !
  subroutine observation_removeFiles (css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    type(obs_file), pointer :: currentFile,nextFile
    character*22 :: myname="observation_removeFiles "
    currentFile => css%firstFile%next
    do while (.not.associated(currentFile,target=css%lastFile))
       nextFile => currentFile%next
       if (associated(currentFile)) then
          !if(obs_bdeb)write(*,*)myname,' Removing categories.'
          call observation_clearCat(currentFile)
          !if(obs_bdeb)write(*,*)myname,' Updating inventory.'
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
  subroutine observation_stackpush(css,path250,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(obs_file),pointer :: newFile
    logical  :: bok =.false.
    INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:) :: start, vsize, atypes
    character(len=80), allocatable, dimension(:) :: dimnames
    integer :: nvalues, tsize, ndims
    integer :: ii,jj,kk,tt
    CHARACTER(LEN=80)               :: varname
    integer, external :: length
    integer :: lenc,leni,lenv,lens,lenp,lend
    logical :: bbok
    character*22 :: myname="observation_stackpush"
    if(obs_bdeb)write(*,*) myname,' Entering.',irc
    call chop0(path250,250)
    lenp=length(path250,250,20)
    write(*,*)myname," Pushing '"//path250(1:lenp)//"'"
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
    ! push onto stack
    if (bok) then
       css%nFileIndexes=css%nFileIndexes + 1
       css%stackReady=.false.
       newFile%prev => css%lastFile%prev
       newFile%next => css%lastFile
       newFile%prev%next => newFile
       newFile%next%prev => newFile
       ! set file name...
       newFile%fn250=path250
       call chop0(newFile%fn250,250)
       newFile%lenf=length(newFile%fn250,250,20)
       newFile%tablepath=css%tablepath
       !
       do jj=1,10
          newFile%ook(jj)=0
          newFile%orm(jj)=0
       end do
       !
       if (obs_bdeb) call observation_printStack(css,crc250,irc)
       css%currentFile=>newFile
    end if
    ! open file
    if (bok) then
       ! open file
       call observation_scanFile(css,bok,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_scanFile.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    ! if (.not.bok) then
    !    if (associated(css%currentFile)) then
    !       call observation_stackpop(css,path250,crc250,irc)
    !       if (irc.ne.0) then
    !          call observation_errorappend(crc250,myname)
    !          call observation_errorappend(crc250," Error return from observation_stackpop.")
    !          call observation_errorappendi(crc250,irc)
    !          call observation_errorappend(crc250,"\n")
    !          return
    !       end if
    !    end if
    ! end if
    if (obs_bdeb) call observation_printStack(css,crc250,irc)
    if(obs_bdeb)write(*,*)myname,' Done.',irc
  end subroutine observation_stackpush

  !
  ! Remove last bufr-file on the BUFR STACK
  !
  subroutine observation_stackpop(css,path250,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(obs_file), pointer :: currentFile => null()
    type(obs_file), pointer :: prevFile => null()
    character*22 :: myname="observation_stackpop"
    logical :: bdone
    integer, external :: length
    integer :: lenp
    if(obs_bdeb)write(*,*)myname,' Entering.',irc
    call chop0(path250,250)
    lenp=length(path250,250,10)
    currentFile => css%lastFile%prev
    bdone=associated(currentFile,target=css%firstFile)
    do while (.not. bdone) 
       prevFile=>currentFile%prev
       if (currentFile%fn250(1:currentFile%lenf).eq.path250(1:lenp).or.lenp.eq.0) then
          if(obs_bdeb)write(*,*)myname,' *** Popping:',currentFile%fn250(1:currentFile%lenf)
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
    if(obs_bdeb)write(*,*)myname,' Done.',irc
    return
  end subroutine observation_stackpop

  !
  ! Peek at last bufr-file put onto the BUFR STACK
  !
  subroutine observation_stackpeeklen(css,maxrep,crc250,irc)
    type(obs_session), pointer :: css !  current session
    integer :: maxrep
    character*250 :: crc250
    integer :: irc
    type(obs_file), pointer :: currentFile => null()
    integer :: ii,jj
    character*22 :: myname="observation_stackpeeklen"
    maxrep=1
    currentFile => css%lastFile%prev
    ! report file-name
    if (.not.associated(currentFile,target=css%firstFile)) then
       call observation_getFileReportLen(css,currentFile,maxrep,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_getFileReportLen.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    !if(obs_bdeb)write(*,*)myname,' Done.',associated(css%lastFile%prev,target=css%firstFile)
  end subroutine observation_stackpeeklen
  !
  subroutine observation_stackpeek(css,maxrep,nrep,rep250,crc250,irc)
    type(obs_session), pointer :: css !  current session
    integer :: maxrep
    integer :: nrep
    character*250 :: rep250(maxrep)
    character*250 :: crc250
    integer :: irc
    character*50 :: s1, s2, s3
    integer, external :: length
    integer :: len1,len2,len3,lenm,lenv,lena,lenr,lend,lens
    type(obs_file), pointer :: currentFile => null()
    integer :: ii,jj
    character*80 :: varname
    character*22 :: myname="observation_stackpeek"
    if(obs_bdeb)write(*,*)myname,' Entering.'
    currentFile => css%lastFile%prev
    ! report file-name
    nrep=0
    if (.not.associated(currentFile,target=css%firstFile)) then
       call observation_getFileReport(css,currentFile,maxrep,nrep,rep250,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_getFileReport.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    if(obs_bdeb)write(*,*)myname,' Done.',maxrep,nrep
  end subroutine observation_stackpeek

  subroutine observation_getFileReportLen(css,currentFile,maxrep,crc250,irc)
    type(obs_session), pointer :: css !  current session
    type(obs_file), pointer :: currentFile
    integer :: maxrep
    character*250 :: crc250
    integer :: irc
    type(obs_mainCategory), pointer :: cat
    type(obs_subCategory), pointer :: sub
    character*22 :: myname="observation_getFileReportLen"
    maxrep=2
    if (currentFile%time_lim) then
       maxrep=maxrep+2
    end if
    if (currentFile%ind_lim) then
       maxrep=maxrep+2
    end if
    cat => currentFile%firstCategory%next
    do while (.not.associated(cat,target=currentFile%lastCategory))
       maxrep=maxrep+1
       sub => cat%firstSubCategory%next
       do while (.not.associated(sub,target=cat%lastSubCategory))
          maxrep=maxrep+3
          maxrep=maxrep+4*sub%ktdexl
          sub => sub%next
       end do
       cat => cat%next
    end do
    !write(*,*)myname,'Maxrep:',maxrep
    return
  end subroutine observation_getFileReportLen

  subroutine observation_getFileReport(css,currentFile,maxrep,nrep,rep250,crc250,irc)
    type(obs_session), pointer :: css !  current session
    type(obs_file), pointer :: currentFile
    integer :: maxrep
    integer :: nrep
    character*250 :: rep250(maxrep)
    character*250 :: crc250
    integer :: irc
    character*50 :: s1, s2, s3, s4
    integer, external :: length
    integer :: len1,len2,len3,len4,lenm,lenv,lena,lenr,lend,lens
    integer :: ii,jj
    character*80 :: varname
    type(obs_mainCategory), pointer :: cat
    type(obs_subCategory), pointer :: sub
    logical :: first
    character*22 :: myname="observation_getFileReport"
    ! file name
    call chop0(currentFile%fn250,250)
    lenm=length(currentFile%fn250,250,20)
    nrep=min(maxrep,nrep+1)               ! file name  +1
    rep250(nrep)="file"//sep//"name"//sep//currentFile%fn250(1:lenm)
    write(s1,'(I12)') currentFile%nsubset; call chop0(s1,50); len1=length(s1,50,10)
    nrep=min(maxrep,nrep+1)               ! number of observations  +1
    rep250(nrep)="file"//sep//"message count"//sep//s1(1:len1)
    ! sorting variable is available
    if (currentFile%time_lim) then
       write(s2,*) currentFile%time_start; call chop0(s2,50); len2=length(s2,50,10)
       write(s3,*) currentFile%time_stop; call chop0(s3,50); len3=length(s3,50,10)
       nrep=min(maxrep,nrep+1)          ! start time      +1
       rep250(nrep)="file"//sep//"time"//sep//"start"//sep//s2(1:len2)
       nrep=min(maxrep,nrep+1)          ! end time      +1
       rep250(nrep)="file"//sep//"time"//sep//"stop"//sep//s3(1:len3)
    end if
    if (currentFile%ind_lim) then
       write(s2,*) currentFile%ind_start; call chop0(s2,50); len2=length(s2,50,10)
       write(s3,*) currentFile%ind_stop; call chop0(s3,50); len3=length(s3,50,10)
       nrep=min(maxrep,nrep+1)          ! start time      +1
       rep250(nrep)="file"//sep//"index"//sep//"start"//sep//s2(1:len2)
       nrep=min(maxrep,nrep+1)          ! end time      +1
       rep250(nrep)="file"//sep//"index"//sep//"stop"//sep//s3(1:len3)
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
          do ii=1,min(sub%ktdexl,ktdexl_max)
             nrep=min(nrep+1,maxrep)
             write(rep250(nrep),'("file",A,"type",A,I0,A,"subtype",A,I0,A,"seqno",A,I0,A,I0)') sep,sep,&
                  & cat%category, sep,sep, sub%subcategory, sep,sep, ii,sep,sub%ktdexp(ii)
             nrep=min(nrep+1,maxrep)
             write(rep250(nrep),'("file",A,"type",A,I0,A,"subtype",A,I0,A,"name",A,I0,A,A)') sep,sep,&
                  & cat%category, sep,sep, sub%subcategory, sep,sep, ii,sep,sub%cnames(ii)
             nrep=min(nrep+1,maxrep)
             write(rep250(nrep),'("file",A,"type",A,I0,A,"subtype",A,I0,A,"unit",A,I0,A,A)') sep,sep,&
                  & cat%category, sep,sep, sub%subcategory, sep,sep, ii,sep,sub%cunits(ii)
             nrep=min(nrep+1,maxrep)
             if (sub%values(ii).eq.rvind) then
                s4="NA"
                len4=2
             else
                call observation_wash(sub%values(ii),s4,len4)
             end if
             write(rep250(nrep),'("file",A,"type",A,I0,A,"subtype",A,I0,A,"val1",A,I0,A,A)') sep,sep,&
                  & cat%category, sep,sep, sub%subcategory, sep,sep, ii,sep,s4(1:len4)
          end do
          sub => sub%next
       end do
       cat => cat%next
    end do

    if (obs_bdeb) then
       do ii=1,nrep
          call chop0(rep250(II),250)
          lenr=length(rep250(ii),250,100)
          write(*,*) myname,' REP:',ii,maxrep,rep250(ii)(1:lenr)
       end do
    end if

    return
  end subroutine observation_getFileReport

  !
  !###############################################################################
  !ROUTINES FOR MAINTAINING TARGET STACK
  !###############################################################################
  !
  subroutine observation_targetinit(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_targetinit"
    css%reportsReady=.false. ! we must redo report generation
    ! initialise chain
    if (.not.associated(css%firsttarget)) then
       allocate(css%firsttarget,css%lasttarget, stat=irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250,"Unable to allocate 'firsttarget/lasttarget'.")
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%firsttarget%next => css%lasttarget
       css%lasttarget%prev => css%firsttarget
       css%ntarget=0
       css%ndyn=0
       css%trg_set=.false.
       css%dyn_set=.false.
    end if
  end subroutine observation_targetinit
  !
  ! clear the target stack
  !
  subroutine observation_clearTargetStack(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_clearTargetStack"
    integer :: ii, lens
    integer, external :: length
    if(obs_bdeb)write(*,*)myname,' Entering.'
    css%reportsReady=.false. ! old reports are discarded...
    ! delete any existing Target-entries
    call observation_removeTarget(css,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from targetrmitem.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if (css%ntarget .ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," System error B:")
       call observation_errorappendi(crc250,css%ntarget)
       call observation_errorappend(crc250,"\n")
       irc=940
       return
    end if
  end subroutine observation_clearTargetStack
  !
  ! add item to target list
  !
  subroutine observation_pushtarget(css,trg,pos,descr,info,&
       & min,max,crc250,irc)
    implicit none
    type(obs_session), pointer :: css !  current session
    character(len=*) :: trg      ! target name
    character(len=*) :: pos      ! target position
    character(len=*) :: descr    ! target description
    character(len=*) :: info     ! target info
    character(len=*) :: min      ! target min value
    character(len=*) :: max      ! target max value
    character*250 :: crc250
    integer :: irc
    type(obs_target),pointer :: newTarget
    integer :: ii,yy,mm,dd,hh,mi
    real:: sec
    integer :: lenc,lenp,lend,lens,lene
    integer, external :: length
    character*22 :: myname="observation_pushTarget"
    if(obs_bdeb)write(*,*)myname,' Adding: "'//trg(1:len_trim(trg))//'" "'//pos(1:len_trim(pos))//'"'
    if (len(trg).eq.0) then
       irc=983
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Invalid target name:")
       call observation_errorappend(crc250,trg(1:len(trg))//" "//pos(1:len(pos)))
       call observation_errorappend(crc250,"\n")
       return
    end if
    if (len(pos).eq.0.and.len(descr).eq.0.and.(len(min).eq.0.or.len(max).eq.0)) then
       irc=984
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Invalid target position:")
       call observation_errorappend(crc250,trg(1:len(trg))//" "//pos(1:len(pos)))
       call observation_errorappend(crc250,"\n")
       return
    end if
    css%reportsReady=.false. ! we must redo report generation
    ! create new pos-item
    allocate(newTarget,stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Unable to allocate new Target.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    newTarget%trg80=trg
    newTarget%pos250=pos
    newTarget%descr80=descr
    newTarget%info250=info
    newTarget%min80=min
    newTarget%max80=max
    newTarget%type=parse_type(pos,css%int_var,crc250,irc) ! get position type...
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from parse_type.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! push onto stack
    css%ntarget=css%ntarget + 1
    newTarget%prev => css%lasttarget%prev
    newTarget%next => css%lasttarget
    newTarget%prev%next => newTarget
    newTarget%next%prev => newTarget
    css%trg_set=.false.
    css%dyn_set=.false.
    if (newTarget%type.eq.parse_variable) then
       css%ndyn=css%ndyn+1
       newTarget%ind=css%ndyn
    else
       newTarget%ind=0
    end if
    nullify(newTarget)
    !if(obs_bdeb)write(*,*)myname,' Done.',trg
    return
  end subroutine observation_pushTarget
  !
  ! loop over target entries
  !
  logical function observation_looptarget(css,trg80,pos250,descr80,&
       & info250,min80,max80,crc250,irc)
    implicit none
    type(obs_session), pointer :: css !  current session
    character*80  :: trg80      ! target name
    character*250 :: pos250    ! position/sequence number
    character*80  :: descr80    ! descriptor
    character*250 :: info250   ! information
    character*80  :: min80      ! min value
    character*80  :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_looptarget"
    observation_looptarget=.false. ! only true if all is ok...
    if (.not.associated(css%ctarget)) then
       css%ctarget =>  css%firstTarget%next 
    else
       css%ctarget =>  css%ctarget%next
    end if
    if (associated(css%ctarget,css%lastTarget)) then
       nullify(css%ctarget)
       observation_looptarget=.false.
    else
       trg80=css%ctarget%trg80
       pos250=css%ctarget%pos250
       descr80=css%ctarget%descr80
       info250=css%ctarget%info250
       min80=css%ctarget%min80
       max80=css%ctarget%max80
       observation_looptarget=.true.
    end if
    return
  end function observation_looptarget
  !
  ! get number of targets
  !
  integer function observation_targetCount(css,crc250,irc)
    type(obs_session), pointer :: css   ! session structure
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_targetCount "
    observation_targetCount=css%ntarget
    return
  end function observation_targetCount
  !
  integer function observation_trgCount(css,crc250,irc)
    type(obs_session), pointer :: css   ! session structure
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_trgCount "
    observation_trgCount=css%ntrg
    return
  end function observation_trgCount
  !
  ! get number of locations
  !
  integer function observation_locationCount(css,crc250,irc)
    type(obs_session), pointer :: css   ! session structure
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_locationCount "
    observation_locationCount=css%nloc
    return
  end function observation_locationCount
  !
  ! make target list
  !
  subroutine observation_getTrg80(css,var80,offset,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*80, allocatable :: var80(:)
    integer :: offset
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="observation_getTrg80"
    integer ii
    do ii=1,css%ntrg
       var80(ii+offset)=css%trg80(ii)
    end do
    return
  end subroutine observation_getTrg80
  !
  ! get output values
  !
  subroutine observation_getVal(css,iloc,val,offset,crc250,irc)
    type(obs_session), pointer :: css !  current session
    integer :: iloc
    real, allocatable :: val(:)
    integer :: offset
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="observation_getVal"
    integer ii
    do ii=1,css%ntrg
       val(ii+offset)=css%locdata(iloc)%ptr%trg_val(ii)
    end do
    return
  end subroutine observation_getVal
  !
  ! check if session has index expression defined
  !
  logical function observation_hasValidIndex(css,crc250,irc)
    type(obs_session), pointer :: css   ! session structure
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_hasValidIndex "
    observation_hasValidIndex=css%ind_eset
  end function observation_hasValidIndex
  !
  ! print stack
  !
  subroutine observation_printStack(css,crc250,irc) 
    type(obs_session), pointer :: css   ! session structure
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_printStack "
    type(obs_target), pointer :: currenttarget => null()
    integer :: lent,lenp,lend,lens,lene,ii
    integer, external :: length
    ii=0
    if ( .not. css%trg_set ) then
       if(obs_bdeb)write(*,*)myname,' Targets:',css%ntrg
       currenttarget => css%firsttarget%next
       do while (.not.associated(currenttarget,target=css%lasttarget))
          ii=ii+1
          call chop0(currenttarget%trg80,80)
          call chop0(currenttarget%pos250,250)
          call chop0(currenttarget%descr80,80)
          call chop0(currenttarget%info250,250)
          call chop0(currenttarget%min80,80)
          call chop0(currenttarget%max80,80)
          lent=length(currenttarget%trg80,80,10)
          lenp=length(currenttarget%pos250,250,10)
          lend=length(currenttarget%descr80,80,10)
          lens=length(currenttarget%min80,80,10)
          lene=length(currenttarget%max80,80,10)
          write(*,*) myname,' Stack:',ii,' pos="'//currenttarget%pos250(1:lenp)//'" target="'//currenttarget%trg80(1:lent)//'"'
          currenttarget => currenttarget%next
       end do
    end if
    write(*,*) myname,' Stack entries:',ii
    return
  end subroutine observation_printStack

  !
  ! make target list from target chain
  !
  subroutine observation_clearTargetList(css,crc250,irc)
    type(obs_session), pointer :: css   ! session structure
    character*250 :: crc250
    integer :: irc
    if (allocated(css%trg80)) deallocate(css%trg80)
    if (allocated(css%trg_lent)) deallocate(css%trg_lent)
    if (allocated(css%trg_pos250)) deallocate(css%trg_pos250)
    if (allocated(css%trg_lenp)) deallocate(css%trg_lenp)
    if (allocated(css%trg_type)) deallocate(css%trg_type)
    if (allocated(css%trg_seq)) deallocate(css%trg_seq)
    if (allocated(css%trg_ind)) deallocate(css%trg_ind)
    if (associated(css%trg_psp)) deallocate(css%trg_psp)
    if (allocated(css%trg_descr)) deallocate(css%trg_descr)
    if (allocated(css%trg_lval)) deallocate(css%trg_lval)
    if (allocated(css%trg_minval)) deallocate(css%trg_minval)
    if (allocated(css%trg_maxval)) deallocate(css%trg_maxval)
    if (allocated(css%trg_ook)) deallocate(css%trg_ook)
    if (allocated(css%trg_orm)) deallocate(css%trg_orm)
    if (allocated(css%trg_req)) deallocate(css%trg_req)
    if (allocated(css%trg_val)) deallocate(css%trg_val)
    if (allocated(css%trg_vok)) deallocate(css%trg_vok)
    if (associated(css%trg_ptr)) deallocate(css%trg_ptr)
    css%trg_set=.false.
    ! remove any message data...
    if (associated(css%msg)) then
       if (allocated(css%msg%trg_val)) deallocate(css%msg%trg_val)
       if (allocated(css%msg%trg_vok)) deallocate(css%msg%trg_vok)
       if (allocated(css%msg%trg_set)) deallocate(css%msg%trg_set)
       if (allocated(css%msg%trg_res)) deallocate(css%msg%trg_res)
       deallocate(css%msg)
       allocate(css%msg)
    end if
    return
  end subroutine observation_clearTargetList
  
  subroutine observation_makeTargetList(css,crc250,irc) 
    type(obs_session), pointer :: css   ! session structure
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_makeTargetList "
    type(obs_target), pointer :: currenttarget => null()
    integer :: lent,lenp,lend,ii
    integer, external :: length
    if(obs_bdeb)write(*,*)myname,' Entering.',irc,css%ntarget,css%trg_set,css%ind_eset
    if ( .not. css%trg_set ) then
       if (css%ind_eset) then
          css%ntrg=css%ntarget+1
       else
          css%ntrg=css%ntarget
       end if
       !
       ! set target arrays
       !
       if(obs_bdeb)write(*,*)myname,' Targets:',css%ntrg
       if (allocated(css%trg80)) deallocate(css%trg80)
       if (allocated(css%trg_lent)) deallocate(css%trg_lent)
       if (allocated(css%trg_pos250)) deallocate(css%trg_pos250)
       if (allocated(css%trg_lenp)) deallocate(css%trg_lenp)
       if (allocated(css%trg_type)) deallocate(css%trg_type)
       if (allocated(css%trg_seq)) deallocate(css%trg_seq)
       if (allocated(css%trg_ind)) deallocate(css%trg_ind)
       if (associated(css%trg_psp)) deallocate(css%trg_psp)
       if (allocated(css%trg_descr)) deallocate(css%trg_descr)
       if (allocated(css%trg_lval)) deallocate(css%trg_lval)
       if (allocated(css%trg_minval)) deallocate(css%trg_minval)
       if (allocated(css%trg_maxval)) deallocate(css%trg_maxval)
       if (allocated(css%trg_ook)) deallocate(css%trg_ook)
       if (allocated(css%trg_orm)) deallocate(css%trg_orm)
       if (allocated(css%trg_req)) deallocate(css%trg_req)
       if (allocated(css%trg_val)) deallocate(css%trg_val)
       if (allocated(css%trg_vok)) deallocate(css%trg_vok)
       if (associated(css%trg_ptr)) deallocate(css%trg_ptr)
       allocate(css%trg80(css%ntrg),css%trg_lent(css%ntrg),&
            & css%trg_pos250(css%ntrg),css%trg_lenp(css%ntrg),&
            & css%trg_type(css%ntrg),css%trg_seq(css%ntrg), &
            & css%trg_ind(css%ntrg),css%trg_psp(css%ntrg), &
            & css%trg_descr(css%ntrg),css%trg_lval(2,css%ntrg),&
            & css%trg_minval(css%ntrg),css%trg_maxval(css%ntrg),&
            & css%trg_ook(0:css%ntrg),css%trg_orm(0:css%ntrg),&
            & css%trg_req(css%ntrg),css%trg_val(css%ntrg), &
            & css%trg_vok(css%ntrg),css%trg_ptr(css%ntrg),stat=irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250,"Unable to allocate 'targetList'.")
          call observation_errorappend(crc250,"\n")
          return
       end if
       !
       ! set dynamic variable arrays...
       !
       if (allocated(css%dyn_var)) deallocate(css%dyn_var)
       if (allocated(css%dyn_lenv)) deallocate(css%dyn_lenv)
       if (allocated(css%dyn_val)) deallocate(css%dyn_val)
       allocate(css%dyn_var(css%ndyn),css%dyn_lenv(css%ndyn),css%dyn_val(css%ndyn),stat=irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250,"Unable to allocate 'intlist'.")
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%dyn_pos=0 ! floating position
       css%dyn_cnt=0 ! floating position
       !
       ii=0 ! target
       css%trg_ook(ii)=0
       css%trg_orm(ii)=0
       currenttarget => css%firsttarget%next
       do while (.not.associated(currenttarget,target=css%lasttarget))
          ii=ii+1
          css%trg_ptr(ii)%ptr=>currenttarget
          call chop0(currenttarget%trg80,80)
          call chop0(currenttarget%pos250,250)
          call chop0(currenttarget%descr80,80)
          call chop0(currenttarget%info250,250)
          lent=length(currenttarget%trg80,80,10)
          lenp=length(currenttarget%pos250,250,10)
          lend=length(currenttarget%descr80,80,10)
          css%trg80(ii)=currenttarget%trg80(1:lent)
          css%trg_lent(ii)=lent
          css%trg_ook(ii)=0
          css%trg_orm(ii)=0
          css%trg_req(ii)=.false.           ! is target required?
          css%trg_type(ii)=currenttarget%type
          if (lend.eq.0.and.lenp.eq.0) then ! this is a delayed variable
             css%trg_type(ii)=parse_delay ! delay processing
             css%trg_descr(ii)=0
             if(obs_bdeb)write(*,*)myname,' Duplicator candidate at:',ii
          else if (css%trg_type(ii).ne.parse_internal) then ! no descriptor
             read(currenttarget%descr80(1:lend),*,iostat=irc) css%trg_descr(ii)
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Unable to read descr from '"//currenttarget%descr80(1:lend)//"'")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
          end if
          css%trg_pos250(ii)=currenttarget%pos250
          css%trg_lenp(ii)=lenp
          select case (css%trg_type(ii)) ! process position (empty,const,var,expr=
          case (parse_delay) ! delayed processing
          case (parse_empty)
             css%trg_seq(ii)=0
             css%trg_ind(ii)=0
          case (parse_constant)
             css%trg_ind(ii)=0
             read(currenttarget%pos250(1:lenp),*,iostat=irc) css%trg_seq(ii)
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Unable to read seq from '"&
                     & //currenttarget%pos250(1:lenp)//"'")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
          case (parse_internal); ! no descriptor allowed
             if (lend.ne.0) then
                irc=244
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Interal variable with descriptor: '"&
                     & //currenttarget%pos250(1:lenp)//"'")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
             css%trg_seq(ii)=0
             css%trg_ind(ii)=0
             call parse_open(css%trg_psp(ii)%ptr,crc250,irc)
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Error return from parse_open.")
                return
             end if
          case (parse_variable);
             css%trg_seq(ii)=0
             css%trg_ind(ii)=currenttarget%ind
             css%dyn_var(currenttarget%ind)=currenttarget%pos250(1:min(80,lenp))
             css%dyn_lenv(currenttarget%ind)=lenp
             css%dyn_val(currenttarget%ind)=0
          case (parse_expression);
             css%trg_seq(ii)=0
             css%trg_ind(ii)=0
             call parse_open(css%trg_psp(ii)%ptr,crc250,irc)
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Error return from parse_open.")
                return
             end if
          end select
          currenttarget => currenttarget%next
       end do
       if (css%ind_eset) then
          ii=ii+1
          nullify(css%trg_ptr(ii)%ptr)
          css%trg80(ii)=css%ind_trg80(1:css%ind_lent)
          css%trg_lent(ii)=css%ind_lent
          css%trg_lval(1,ii)=css%ind_lval(1)
          css%trg_lval(2,ii)=css%ind_lval(2)
          css%trg_minval(ii)=css%ind_minval
          css%trg_maxval(ii)=css%ind_maxval
          css%trg_req(ii)=(css%ind_lval(1).or.css%ind_lval(2)) ! is target required?
       end if
       css%trg_set=.true.
       css%dyn_set=.true.
    end if
    if(obs_bdeb)write(*,*)myname,' Done.',irc,css%ntarget,css%ntrg,css%ind_eset
    return
  end subroutine observation_makeTargetList
  !
  ! set target limits
  !
  subroutine observation_setTargetLimits(css,var,val,crc250,irc)
    implicit none
    type(obs_session), pointer :: css !  observation session
    character*80, allocatable :: var(:)
    real, allocatable :: val(:)
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    integer :: lens,lene
    integer, external :: length
    integer :: ii,jj
    type(obs_target), pointer :: currenttarget
    type(parse_session),pointer :: plim => null()  ! parse_session pointer must be se
    character*22 :: myname="observation_setTargetLimits "
    integer :: ind(css%ntrg),inc(0:css%ntrg)
    call parse_open(plim,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from parse_open.")
       return
    end if
    css%dyn_max=1
    jj=0
    inc(0)=1
    do ii=1,css%ntrg
       currenttarget => css%trg_ptr(ii)%ptr
       if (associated(currenttarget)) then
          call chop0(currenttarget%min80,80)
          call chop0(currenttarget%max80,80)
          lens=length(currenttarget%min80,80,10)
          lene=length(currenttarget%max80,80,10)
          if (lens.ne.0) then
             call parse_parsef(plim,currenttarget%min80(1:lens),var,crc250,irc)
             if (irc.ne.0) then
                if(obs_bdeb)then
                   write(*,*)myname," Compiling target limit: '"//&
                        & currenttarget%max80(1:lens)//"'",ii
                   do jj=1,size(var)
                      write(*,'(A,A,I0,A)')myname,"     var(",jj,") = '"//&
                           & trim(var(jj))//"'"
                   end do
                end if
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Error return from parsef.")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
             css%trg_req(ii)=.true. ! target is required
             if (obs_bdeb)write(*,*)myname,' Local:',val
             css%trg_minval(ii)=parse_evalf(plim,val,crc250,irc)
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Error return from evalf.")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
             css%trg_lval(1,ii)=.true.
          else
             css%trg_lval(1,ii)=.false.
          end if
          if (lene.ne.0) then
             call parse_parsef(plim,currenttarget%max80(1:lene),var,crc250,irc)
             if (irc.ne.0) then
                if(obs_bdeb)then
                   write(*,*)myname," Compiling target limit: '"//&
                        & currenttarget%max80(1:lene)//"'",ii
                   do jj=1,size(var)
                      write(*,'(A,A,I0,A)')myname,"     var(",jj,") = '"//&
                           & trim(var(jj))//"'"
                   end do
                end if
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Error return from parsef.")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
             css%trg_req(ii)=.true. ! target is required
             if (obs_bdeb)write(*,*)myname,' Local:',val
             css%trg_maxval(ii)=parse_evalf(plim,val,crc250,irc)
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Error return from evalf.")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
             css%trg_lval(2,ii)=.true.
          else
             css%trg_lval(2,ii)=.false.
          end if
          if (css%trg_lval(1,ii).and.css%trg_lval(2,ii)) then ! max min available
             if (css%trg_type(ii).eq.parse_delay) then ! no descriptor or position...
                if(obs_bdeb)write(*,*)myname,' Found duplicator at:',ii
                jj=jj+1
                ind(jj)=ii
                inc(jj)=abs(nint(css%trg_maxval(ii))-nint(css%trg_minval(ii))+1)*inc(jj-1)
                css%dyn_max=css%dyn_max*inc(jj)
             end if
          end if
       end if
    end do
    call parse_close(plim,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from parse_close.")
       return
    end if
    if (allocated(css%dup_ind)) deallocate(css%dup_ind)
    if (allocated(css%dup_inc)) deallocate(css%dup_inc)
    css%ndup=jj
    allocate(css%dup_ind(css%ndup),css%dup_inc(0:css%ndup),stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Unable to allocate 'duplicates'.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    css%dup_inc(0)=1
    do jj=1,css%ndup
       css%dup_ind(jj)=ind(jj)
       css%dup_inc(jj)=inc(jj)
    end do
    return
  end subroutine observation_setTargetLimits

  !
  ! clear target stack
  !
  subroutine observation_removeTarget (css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    type(obs_target), pointer :: currenttarget => null()
    type(obs_target), pointer :: nexntarget => null()
    character*22 :: myname="observation_removeTarget "
    if(obs_bdeb)write(*,*)myname,' Entering.'
    currenttarget => css%firsttarget%next
    do while (.not.associated(currenttarget,target=css%lasttarget))
       nexntarget => currenttarget%next
       if (associated(currenttarget)) then
          css%ntarget=css%ntarget - 1
          if (currenttarget%type.eq.parse_variable)then
             css%ndyn=css%ndyn - 1
          end if
          currenttarget%next%prev => currenttarget%prev
          currenttarget%prev%next => currenttarget%next
          nullify(currenttarget%prev)
          nullify(currenttarget%next)
          nullify(currenttarget)
       end if
       currenttarget => nexntarget
    end do
    if(obs_bdeb)write(*,*)myname,' Deallocating.'
    if (allocated(css%trg80)) deallocate(css%trg80)
    if (allocated(css%trg_lent)) deallocate(css%trg_lent)
    if (allocated(css%trg_pos250)) deallocate(css%trg_pos250)
    if (allocated(css%trg_lenp)) deallocate(css%trg_lenp)
    if (allocated(css%trg_type)) deallocate(css%trg_type)
    if (allocated(css%trg_seq)) deallocate(css%trg_seq)
    if (allocated(css%trg_ind)) deallocate(css%trg_ind)
    if(obs_bdeb)write(*,*)myname,' Deallocating trg_psp.'
    if (associated(css%trg_psp)) deallocate(css%trg_psp)
    if (allocated(css%trg_descr)) deallocate(css%trg_descr)
    if (allocated(css%trg_lval)) deallocate(css%trg_lval)
    if (allocated(css%trg_minval)) deallocate(css%trg_minval)
    if (allocated(css%trg_maxval)) deallocate(css%trg_maxval)
    if (allocated(css%trg_ook)) deallocate(css%trg_ook)
    if (allocated(css%trg_orm)) deallocate(css%trg_orm)
    if (allocated(css%trg_req)) deallocate(css%trg_req)
    if (allocated(css%trg_val)) deallocate(css%trg_val)
    if (allocated(css%trg_vok)) deallocate(css%trg_vok)
    if(obs_bdeb)write(*,*)myname,' Deallocating trg_ptr.'
    if (associated(css%trg_ptr)) deallocate(css%trg_ptr)
    if (allocated(css%dyn_var)) deallocate(css%dyn_var)
    if (allocated(css%dyn_lenv)) deallocate(css%dyn_lenv)
    if (allocated(css%dyn_val)) deallocate(css%dyn_val)
    css%ntrg = 0
    css%trg_set=.false.
    if(obs_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine observation_removeTarget
  !
  ! check if current reports are valid
  !
  subroutine observation_checkTarget(css,bok,crc250,irc)
    type(obs_session), pointer :: css
    logical:: bok
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="observation_checkTarget"
    integer :: seq, ii
    if(obs_bdeb)write(*,*)myname,' Entering.'
    if (css%ntarget .eq. 0) then ! accept all reports if there is no target...
       bok=.true.
       return
    end if
    if (css%category .eq. ksec1(6) .and. &
         & css%subcategory .eq. ksec1(7)) then
       bok=.true.
       if(obs_bdeb)write(*,*)myname,' Found BUFR:',ksec1(6),ksec1(7)
       TRG: do ii=1,css%ntrg
          if (bok.and.css%trg_lval(1,ii).or.css%trg_lval(2,ii)) then
             if (bok.and.css%trg_lval(1,ii)) bok=(css%trg_val(ii).ge.css%trg_minval(ii))
             if (bok.and.css%trg_lval(2,ii)) bok=(css%trg_val(ii).le.css%trg_maxval(ii))
          end if
          if (bok) then
             css%trg_ook(ii)=css%trg_ook(ii)+1
          else
             css%trg_orm(ii)=css%trg_orm(ii)+1
          end if
          if(obs_bdeb.and..not.bok)write(*,*)myname,' Failed check:',ii,css%trg_val(ii)
          if (.not.bok) exit TRG
       end do TRG
       if (bok) then
          css%trg_ook(0)=css%trg_ook(0)+1
       else
          css%trg_orm(0)=css%trg_orm(0)+1
       end if
    else
       bok=.false.
    end if
    if(obs_bdeb)write(*,*)myname,' Done.',ksec1(6),ksec1(7),bok
    return
  end subroutine observation_checkTarget
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
  !###############################################################################
  !ROUTINES FOR HANDLING STACK
  !###############################################################################
  ! Forecasts from one analysis is reportsReady at a time... (=the same parameters)
  ! Used in this way
  ! 1) observation_setFileStackLimits: make index limits of files in stack
  ! 2) observation_loopFileStack: loop over files until false return...
  !
  ! Reset indexes for looping over analysis
  !
  subroutine observation_sortFiles(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(obs_file), pointer :: currentFile => null()
    integer :: ii
    character*22 :: myname="observation_sortFiles"
    !
    ! make array of files
    if(obs_bdeb)write(*,*)myname,' Entering.'
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
  subroutine observation_setModelFileId(css,mid)
    type(obs_session), pointer :: css !  current session
    integer :: mid
    css%int_val(1)=mid
    return
  end subroutine observation_setModelFileId
  !
  ! get next observation file within limits...
  !
  logical function observation_loopFileStack(css,obs_lval,obs_minval,obs_maxval,crc250,irc)
    type(obs_session), pointer :: css !  current session
    logical :: obs_lval(2)
    real :: obs_minval
    real :: obs_maxval
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_loopFileStack"
    logical :: bdone,found
    if (obs_bdeb)write(*,*)myname,' Entering:',css%sortLimitsOk,css%currentFileSortIndex,&
         & css%leftFileSortIndex,css%rightFileSortIndex
    found=.false.
    bdone=(.not. css%sortLimitsOk)
    if (bdone) then ! index is not sorted..
       css%frm(1)=css%frm(1)+css%nFileIndexes ! no sort index
    else if (css%currentFileSortIndex.lt.css%leftFileSortIndex) then ! first
       css%fok(1)=css%fok(1)+css%nFileIndexes ! we have a sort index
       css%frm(2)=css%frm(2)+(css%leftFileSortIndex-1&
            & +css%nFileIndexes-css%rightFileSortIndex) ! outside index search
    end if
    do while (.not.bdone)
       css%currentFileSortIndex=max(css%currentFileSortIndex+1,css%leftFileSortIndex)
       if (css%currentFileSortIndex.gt.css%rightFileSortIndex) then
          css%currentFileSortIndex=0
          css%currentFileIndex=0
          nullify(css%currentFile)
          bdone=.true.
       else
          css%currentFileIndex=css%fileStackInd(css%currentFileSortIndex,2)
          css%currentFile => css%fileStack(css%currentFileIndex)%ptr
          ! check if inside limits
          if (.not.((obs_lval(1).and.obs_minval.gt.css%currentFile%ind_stop) .or.&
               & (obs_lval(2).and.obs_maxval.lt.css%currentFile%ind_start)).and. &
               & .not.((css%ind_lval(1).and.css%ind_minval.gt.css%currentFile%ind_stop) .or.&
               & (css%ind_lval(2).and.css%ind_maxval.lt.css%currentFile%ind_start)) &
               &) then 
             found=.true.
             bdone=.true.
             if (obs_bdeb) write(*,*)myname,' Found:',&
                  & css%currentFileSortIndex,css%leftFileSortIndex,css%rightFileSortIndex
             css%fok(3)=css%fok(3)+1 ! within index target range
          else
             css%frm(3)=css%frm(3)+1 ! outside index target range
          end if
          css%fok(2)=css%fok(2)+1 ! within index search
       end if
    end do
    css%int_val(2)=css%currentFileIndex ! observation file id
    css%int_val(3)=0 ! message id
    css%int_val(4)=0 ! obs id
    css%int_val(5)=0 ! location id
    observation_loopFileStack=found
    if (obs_bdeb)write(*,*)myname,' Done:',found,css%currentFileSortIndex,&
         & associated(css%currentFile)
    return
  end function observation_loopFileStack
  !
  ! sort the file stack
  !
  subroutine observation_sortStack(css,crc250,irc)
    use sort
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(obs_file), pointer :: currentFile => null()
    integer :: ii,jj,kk
    character*22 :: myname="observation_sortStack"
    real :: buff
    logical :: luff
    !
    ! make array of files
    if(obs_bdeb)write(*,*)myname,' Entering.'
    if (associated(css%fileStack)) deallocate(css%fileStack)
    if (allocated(css%fileStackSort)) deallocate(css%fileStackSort)
    if (allocated(css%fileStackInd)) deallocate(css%fileStackInd)
    if (obs_bdeb) write(*,*)myname,'Allocating sort stack:',css%nFileIndexes
    allocate(css%fileStack(css%nFileIndexes),css%fileStackSort(css%nFileIndexes,2),&
         &css%fileStackInd(css%nFileIndexes,2),stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from allocate (")
       call observation_errorappendi(crc250,css%nFileIndexes)
       call observation_errorappend(crc250,")")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if (obs_bdeb) write(*,*)myname,'Making sort stack list:',css%nFileIndexes
    currentFile => css%firstFile%next
    ii=0
    kk=0
    do while (.not.associated(currentFile, target=css%lastFile))
       ii=ii+1
       kk=kk+1
       if (ii.le.css%nFileIndexes) then
          css%fileStack(ii)%ptr => currentFile
          if (currentFile%ind_lim) then ! requested observations present?
             css%fileStackInd(ii,1)=ii
             css%fileStackInd(ii,2)=ii
             buff=css%trg_val(css%ntrg)
             luff=css%trg_vok(css%ntrg)
             css%trg_val(css%ntrg)=currentFile%ind_start
             css%trg_vok(css%ntrg)=.true.
             !if (obs_bdeb) then
             !   write(*,*)myname,' Eval:',css%ntrg,css%trg_val(css%ntrg),&
             !        & "'"//css%ind_pt%funcStr100(1:css%ind_pt%lenf)//"'"
             !   do jj=1,css%ntrg
             !      write(*,'(X,A,A,I0,A,F0.10)')myname,'  Trg(',jj,')=',css%trg_val(jj)
             !   end do
             !end if
             if (css%ind_tset) then
                !if (obs_bdeb)write(*,*)myname,' Sort targets:',css%trg_val
                css%fileStackSort(ii,1)=parse_evalf(css%ind_pt,css%trg_val,crc250,irc)
                if (irc.ne.0) then
                   call observation_errorappend(crc250,myname)
                   call observation_errorappend(crc250," Error return from evalf.")
                   call observation_errorappendi(crc250,irc)
                   call observation_errorappend(crc250,"\n")
                   return
                end if
             else
                css%fileStackSort(ii,1)=css%trg_val(css%ntrg)
             end if
             css%trg_val(css%ntrg)=currentFile%ind_stop
             css%trg_vok(css%ntrg)=.true.
             if (css%ind_tset) then
                !if (obs_bdeb)write(*,*)myname,' Sort targets:',css%trg_val
                css%fileStackSort(ii,2)=parse_evalf(css%ind_pt,css%trg_val,crc250,irc)
                if (irc.ne.0) then
                   call observation_errorappend(crc250,myname)
                   call observation_errorappend(crc250," Error return from evalf.")
                   call observation_errorappendi(crc250,irc)
                   call observation_errorappend(crc250,"\n")
                   return
                end if
             else
                css%fileStackSort(ii,2)=css%trg_val(css%ntrg)
             end if
             !if (obs_bdeb)write(*,*)myname,'Eval:',ii,css%fileStackSort(ii,1),css%fileStackSort(ii,2),&
              !    & currentFile%ind_start,currentFile%ind_stop
             css%trg_val(css%ntrg)=buff
             css%trg_vok(css%ntrg)=luff
          else
             if (obs_bdeb)then
                write(*,*)myname,"Missing index limits in '"//&
                     & currentFile%fn250(1:currentFile%lenf)//"', ignoring file."
                write(*,*)myname,'Indexes ',currentFile%ind_lim,&
                     & currentFile%ind_start,currentFile%ind_stop
             end if
             ii=ii-1 ! ignore file...
          end if
       end if
       currentFile => currentFile%next
    end do
    if (kk.ne.css%nFileIndexes) then
       irc=944
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," System error:")
       call observation_errorappendi(crc250,css%nFileIndexes)
       call observation_errorappend(crc250,"!=")
       call observation_errorappendi(crc250,kk)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if (obs_bdeb) write(*,*)myname,'Sorting:',css%nFileIndexes
    ! make sorted index (chronologically)
    css%nFileIndexes=ii
    css%nFileSortIndexes=css%nFileIndexes
    css%newnFileSortIndexes(1)=css%nFileIndexes
    css%newnFileSortIndexes(2)=css%nFileIndexes
    if (.false..and.obs_bdeb)then
       do ii=1,css%nFileSortIndexes
          write(*,'(X,A,X,A,X,I0,2(X,I0,X,F15.1))') myname,"Before: ",ii,&
               & css%fileStackInd(ii,1),css%fileStackSort(ii,1),css%fileStackInd(ii,2),css%fileStackSort(ii,2)
       end do
    end if
    call sort_heapsort1r(css%nFileIndexes,css%fileStackSort(1,1),1.0D-5, &
         & css%newnFileSortIndexes(1),css%nFileSortIndexes,css%fileStackInd(1,1),.false.)
    call sort_heapsort1r(css%nFileIndexes,css%fileStackSort(1,2),1.0D-5, &
         & css%newnFileSortIndexes(2),css%nFileSortIndexes,css%fileStackInd(1,2),.false.)
    if (.false..and.obs_bdeb)then
       write(*,*)myname,'Indexes:',css%nFileSortIndexes,css%newnFileSortIndexes
       do ii=1,css%nFileSortIndexes
          write(*,'(X,A,X,A,X,I0,2(X,I0,X,F15.1))') myname,"After: ",ii,&
               & css%fileStackInd(ii,1),css%fileStackSort(ii,1),css%fileStackInd(ii,2),css%fileStackSort(ii,2)
       end do
    end if
    ! set index range
    if(obs_bdeb)write(*,*)myname,' Done.',css%newnFileSortIndexes
    return
  end subroutine observation_sortStack
  !
  subroutine observation_stackfirst(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_stackfirst"
    css%currentFileSortIndex=0
    css%currentFileIndex=0
    css%int_val(2)=css%currentFileIndex ! observation file id
    css%int_val(3)=0 ! message id
    css%int_val(4)=0 ! obs id
    css%int_val(5)=0 ! location id
  end subroutine observation_stackfirst
  !
  subroutine observation_findStackLimits(css,ind_lval,ind_minval,ind_maxval,crc250,irc)
    use sort
    type(obs_session), pointer :: css !  current session
    logical :: ind_lval(2)
    real :: ind_minval
    real :: ind_maxval
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_findStackLimits"
    integer :: leftmin,rightmin,leftmax,rightmax
    logical :: obs_lval(2)
    real :: obs_minval
    real :: obs_maxval
    ! leftFileSortIndex refers to fileStackSort(*,2)
    ! rightFileSortIndex refers to fileStackSort(*,2)
    ! check 
    obs_lval(1)=ind_lval(1)
    obs_minval=ind_minval
    if (obs_lval(1).and.css%ind_lval(1)) then
       obs_minval=max(obs_minval,css%ind_minval)
    else if (css%ind_lval(1)) then
       obs_lval(1)=.true.
       obs_minval=css%ind_minval
    end if
    obs_lval(2)=ind_lval(2)
    obs_maxval=ind_maxval
    if (obs_lval(2).and.css%ind_lval(2)) then
       obs_maxval=min(obs_maxval,css%ind_maxval)
    else if (css%ind_lval(2)) then
       obs_lval(2)=.true.
       obs_maxval=css%ind_maxval
    end if
    if (obs_lval(1)) then
       call sort_heapsearch1r(css%nFileIndexes,css%fileStackSort(1,2),1.0D-5, &
            & css%nFileSortIndexes,css%fileStackInd(1,2),obs_minval,leftmin,rightmin)
       rightmin=max(leftmin,rightmin) ! ignore before first entry
       if (obs_bdeb) write(*,*)myname,' Minval:',obs_minval,&
            & css%fileStackSort(1,2),css%fileStackInd(1,2),&
            & leftmin,rightmin
    else
       leftmin=1
       rightmin=1
    end if
    if (obs_lval(2)) then
       call sort_heapsearch1r(css%nFileIndexes,css%fileStackSort(1,1),1.0D-5, &
            & css%nFileSortIndexes,css%fileStackInd(1,1),obs_maxval,leftmax,rightmax)
       leftmax=min(rightmax,leftmax) ! ignore after last entry
       if (obs_bdeb) write(*,*)myname,' Maxval:',obs_maxval,&
            & css%fileStackSort(1,1),css%fileStackInd(1,1),&
            & leftmax,rightmax
    else
       leftmax=css%nFileSortIndexes
       rightmax=css%nFileSortIndexes
    end if
    css%sortLimitsOk= (leftmin.le.css%nFileSortIndexes.and.rightmax.ge.1) ! check for overlap...
    if (css%sortLimitsOk) then
       css%leftFileSortIndex=min(leftmin,rightmin)
       css%rightFileSortIndex=max(leftmax,rightmax)
    else
       css%leftFileSortIndex=0
       css%rightFileSortIndex=0
    end if
    css%currentFileIndex=0
    css%int_val(2)=css%currentFileIndex ! observation file id
    css%int_val(3)=0 ! message id
    css%int_val(4)=0 ! obs id
    css%int_val(5)=0 ! location id
    if (obs_bdeb)write(*,*)myname,' Done.', css%sortLimitsOk,&
         & css%leftFileSortIndex, css%rightFileSortIndex,css%nFileIndexes
    return
  end subroutine observation_findStackLimits
  !
  ! set index transformation
  !
  subroutine observation_setTransformation(css,pit,crc250,irc)
    type(obs_session), pointer :: css !  current session
    type(parse_session), pointer :: pit
    character*250 :: crc250
    integer :: irc
    character*30 :: myname="observation_setTransformation"
    if (obs_bdeb)write(*,*)myname," Transformation:'"//pit%funcStr100(1:pit%lenf)//"'", &
         & css%ind_eset,associated(css%ind_pt),css%ind_lval(1),css%ind_lval(2)
    css%ind_pt => pit
    css%ind_tset=associated(css%ind_pt)
    if (css%ind_eset .and. css%ind_tset .and. css%ind_lval(1)) then
       css%trg_val(css%ntrg)=css%ind_minval
       css%trg_vok(css%ntrg)=.true.
       if (obs_bdeb)write(*,*)myname,' Targets:',css%trg_val
       css%ind_minval=parse_evalf(css%ind_pt,css%trg_val,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from evalf.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    if (css%ind_eset .and. css%ind_tset .and. css%ind_lval(2)) then
       css%trg_val(css%ntrg)=css%ind_maxval
       css%trg_vok(css%ntrg)=.true.
       if (obs_bdeb)write(*,*)myname,' Targets:',css%trg_val
       css%ind_maxval=parse_evalf(css%ind_pt,css%trg_val,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from evalf.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    return
  end subroutine observation_setTransformation
  !
  ! set the observation time span
  !
  subroutine observation_getIndex(css,trg80,exp250,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*80 :: trg80
    character*250 :: exp250
    character*250 :: crc250
    integer :: irc
    trg80=css%ind_trg80
    exp250=css%ind_exp250
    return
  end subroutine observation_getIndex
  !
  subroutine observation_setIndex(css,trg80,exp250,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*80 :: trg80
    character*250 :: exp250
    character*250 :: crc250
    integer :: irc
    type(obs_file), pointer :: currentFile => null()
    type(obs_file), pointer :: stackNext => null()
    integer, external :: length
    character*22 :: myname="observation_setIndex"
    if(obs_bdeb)write(*,*)myname,' Entering.'
    css%ind_trg80=trg80
    css%ind_exp250=exp250
    call chop0(css%ind_trg80,80)
    call chop0(css%ind_exp250,250)
    css%ind_lent=length(css%ind_trg80,80,10)
    css%ind_lene=length(css%ind_exp250,250,10)
    if(obs_bdeb)write(*,*)myname,' Index:'//css%ind_trg80(1:css%ind_lent)
    css%ind_eset=(css%ind_lene.gt.0)
    if(obs_bdeb)write(*,*)myname,' Done.',css%ind_eset
    !write(*,*)myname,'Setting index:',css%ind_trg80(1:css%ind_lent)
  end subroutine observation_setIndex
  !
  ! set the observation time span
  !
  subroutine observation_getIndexLimits(css,s25,e25,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*25 :: s25,e25
    character*250 :: crc250
    integer :: irc
    integer :: irc2
    write(s25,*,iostat=irc2) css%ind_minval
    write(e25,*,iostat=irc2) css%ind_maxval
    return
  end subroutine observation_getIndexLimits
  !
  subroutine observation_setIndexLimits(css,s25,e25,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*25 :: s25,e25
    character*250 :: crc250
    integer :: irc
    type(obs_file), pointer :: currentFile => null()
    type(obs_file), pointer :: stackNext => null()
    integer :: lens, lene
    integer, external :: length
    type(parse_session),pointer :: plim => null()  ! parse_session pointer must be se
    character*80, allocatable :: var(:)
    real, allocatable :: val(:)
    integer :: ii,jj
    character*22 :: myname="observation_setIndexLimits"
    if(obs_bdeb)write(*,*)myname,' Entering.'
    call chop0(s25,25)
    lens=length(s25,25,10)
    call chop0(e25,25)
    lene=length(e25,25,10)
    if(obs_bdeb)write(*,*)myname,' Limits:'//s25(1:lens)//" -> "//e25(1:lene)
    call parse_open(plim,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from parse_open.")
       return
    end if
    if (lens.ne.0) then
       call parse_parsef(plim,s25(1:lens),var,crc250,irc)
       if (irc.ne.0) then
          if(obs_bdeb)then
             write(*,*)myname," Compiling target limit: '"//&
                  & s25(1:lens)//"'",ii
             do jj=1,size(var)
                write(*,'(A,A,I0,A)')myname,"     var(",jj,") = '"//&
                     & trim(var(jj))//"'"
             end do
          end if
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from parsef.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%ind_minval=parse_evalf(plim,val,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from evalf.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%ind_lval(1)=.true.
    else
       css%ind_lval(1)=.false.
    end if
    if (lene.ne.0) then
       call parse_parsef(plim,e25(1:lene),var,crc250,irc)
       if (irc.ne.0) then
          if(obs_bdeb)then
             write(*,*)myname," Compiling target limit: '"//&
                  & e25(1:lene)//"'",ii
             do jj=1,size(var)
                write(*,'(A,A,I0,A)')myname,"     var(",jj,") = '"//&
                     & trim(var(jj))//"'"
             end do
          end if
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from parsef.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%ind_maxval=parse_evalf(plim,val,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from evalf.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%ind_lval(2)=.true.
    else
       css%ind_lval(2)=.false.
    end if
    call parse_close(plim,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from parse_close.")
       return
    end if
    if(obs_bdeb)write(*,*)myname,' Done.',css%ind_lval,css%ind_minval,css%ind_maxval
    return
  end subroutine observation_setIndexLimits
  !
  ! ignore labels
  !
  subroutine observation_ignorelabel(css,lab250,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: lab250
    character*250 :: crc250
    integer :: irc
    type(obs_file), pointer :: currentFile => null()
    type(obs_file), pointer :: stackNext => null()
    integer, external :: length
    integer :: lenl
    character*22 :: myname="observation_ignoreLabel"
    if(obs_bdeb)write(*,*)myname,' Entering.'
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
    if(obs_bdeb)write(*,*)myname,' Done.'
  end subroutine observation_ignorelabel
  !
  ! set stack file limits
  !
  subroutine observation_setFileStackLimits(css,ind_lval,ind_minval,ind_maxval,crc250,irc)
    type(obs_session), pointer :: css !  current session
    logical :: ind_lval(2)
    real :: ind_minval,ind_maxval
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_setFileStackLimits"
    ! make sure file stack is sorted
    if (.not.css%stackReady) then
       call observation_sortStack(css,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_sortStack.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%stackReady = .true.
    end if
    ! find index start/stop...
    call observation_findStackLimits(css,ind_lval,ind_minval,ind_maxval,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from observation_makeStackLimits.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    css%currentFileSortIndex=0 ! reset index
    return
  end subroutine observation_setFileStackLimits
  !
  ! set index limits directly
  !
  subroutine observation_setIndexLimitsRaw(css,ind_lval,ind_minval,ind_maxval)
    type(obs_session), pointer :: css !  current session
    logical :: ind_lval(2)
    real :: ind_minval,ind_maxval
    character*22 :: myname="observation_setIndexLimitsRaw"
    ! set limits
    css%ind_lval(1)=ind_lval(1)
    css%ind_lval(2)=ind_lval(2)
    css%ind_minval=ind_minval
    css%ind_maxval=ind_maxval
    return
  end subroutine observation_setIndexLimitsRaw
  !
  !
  subroutine observation_getIndexLimitsRaw(css,ind_lval,ind_minval,ind_maxval)
    type(obs_session), pointer :: css !  current session
    logical :: ind_lval(2)
    real :: ind_minval,ind_maxval
    character*22 :: myname="observation_getIndexLimitsRaw"
    ind_lval(1)=css%ind_lval(1)
    ind_lval(2)=css%ind_lval(2)
    ind_minval=css%ind_minval
    ind_maxval=css%ind_maxval
    return
  end subroutine observation_getIndexLimitsRaw
  !
  !###############################################################################
  ! LOCATION ROUTINES
  !###############################################################################
  ! initialise the MODEL location
  !
  subroutine observation_locinit(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="observation_locinit"
    ! initialise chain
    if (.not.associated(css%firstLoc)) then
       allocate(css%firstLoc,css%lastLoc, stat=irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250,"Unable to allocate 'firstLoc/lastLoc'.")
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%firstLoc%next => css%lastLoc
       css%lastLoc%prev => css%firstLoc
       css%nloc=0
       css%locReady=.false.
    end if
  end subroutine observation_locinit
  !
  ! clear the location stack
  !
  subroutine observation_clearLocStack(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(obs_location), pointer :: currentLoc => null()
    type(obs_location), pointer :: locNext => null()
    character*25 :: myname="observation_clearLocStack"
    integer :: ii, lens
    integer, external :: length
    if(obs_bdeb)write(*,*)myname,' Entering.'
    call observation_locinit(css,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from locinit.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! delete any existing location-entries
    currentLoc => css%firstLoc%next
    do while (.not.associated(currentLoc,target=css%lastLoc))
       locNext => currentLoc%next
       call observation_deleteLoc(css,currentLoc,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from locrmitem.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       currentLoc => locNext
    end do
    if (css%nloc .ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," System error:")
       call observation_errorappendi(crc250,css%nloc)
       call observation_errorappend(crc250,"\n")
       irc=940
       return
    end if
    if(obs_bdeb)write(*,*)myname,' Done.'
  end subroutine observation_clearLocStack
  !
  ! delete loc
  !
  subroutine observation_deleteLoc (css,loc, crc250,irc)
    type(obs_session), pointer :: css !  current session
    type(obs_location), pointer :: loc
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    character*25 :: myname="observation_deleteLoc"
    if (associated(loc)) then
       css%nloc = css%nloc - 1
       loc%next%prev => loc%prev
       loc%prev%next => loc%next
       if (allocated(loc%trg_val)) deallocate(loc%trg_val)
       if (allocated(loc%trg_vok)) deallocate(loc%trg_vok)
       deallocate(loc)
    end if
    return
  end subroutine observation_deleteLoc
  !
  ! Add a "location", specified by target variables...
  !
  subroutine observation_locpushTarget(css,locid,bok,crc250,irc)
    type(obs_session), pointer :: css !  current session
    integer :: locid
    logical :: bok
    character*250 :: crc250
    integer :: irc
    type(obs_location),pointer :: newLoc
    character*80 :: var80
    real(KIND=8), allocatable :: values(:)
    integer :: ii,yy,mm,dd,hh,mi
    real:: sec
    integer :: lenc
    integer, external :: length
    character*25 :: myname="observation_locpushTarget"
    if(obs_bdeb)write(*,*)myname,' Entering.'
    ! initialise location stack
    if (css%nloc.eq.0) then
       css%locoffset=locid-1
    end if
    !
    call observation_locinit(css,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from locinit.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    ! create new location-item
    allocate(newLoc,stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Unable to allocate new location.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    newLoc%ntrg=css%ntrg
    allocate(newLoc%trg_val(newLoc%ntrg),newLoc%trg_vok(newLoc%ntrg),stat=irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Unable to allocate newLoc%var.")
       call observation_errorappendi(crc250,newLoc%ntrg)
       call observation_errorappend(crc250,"\n")
       return
    end if
    newLoc%locid=locid
    do ii=1,newLoc%ntrg
       newLoc%trg_val(ii)=css%trg_val(ii)
       newLoc%trg_vok(ii)=css%trg_vok(ii)
    end do
    newLoc%bok=bok
    ! push onto stack
    css%nloc=css%nloc + 1
    newLoc%prev => css%lastLoc%prev
    newLoc%next => css%lastLoc
    newLoc%prev%next => newLoc
    newLoc%next%prev => newLoc
    css%locReady=.false.
    if (css%nloc+css%locoffset .ne. locid) then
       irc=346
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Non-sequential locid:")
       call observation_errorappendi(crc250,locid)
       call observation_errorappend(crc250,"<>")
       call observation_errorappendi(crc250,css%nloc+css%locoffset)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if(obs_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine observation_locpushTarget
  !
  subroutine observation_makeLocList(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(obs_location), pointer :: currentLoc => null()
    character*25 :: myname="observation_makeLocList"
    integer :: ii
    if(obs_bdeb)write(*,*)myname,"ind_pe:",associated(css%ind_pe),css%sid
    if (associated(css%firstLoc).and..not.css%locready.and.css%nloc.gt.0) then
       if (allocated(css%locdata)) deallocate(css%locdata)
       allocate(css%locdata(css%nloc),stat=irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Unable to allocate locid,lat,lon.")
          call observation_errorappendi(crc250,css%nloc)
          call observation_errorappend(crc250," , ")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       ii=0
       currentLoc => css%firstLoc%next
       do while (.not.associated(currentLoc,target=css%lastLoc))
          ii=ii+1
          css%locdata(ii)%ptr=>currentLoc
          css%locdata(ii)%ptr%iloc=ii
          currentLoc => currentLoc%next
       end do
       css%locReady=.true.
    end if
    return
  end subroutine observation_makeLocList
  !
  !###############################################################################
  ! ROUTINES FOR REPORTING OBSERVATIONS TO USER
  !###############################################################################
  !
  ! put next BUFR-message in memory
  !
  subroutine observation_getNextLoc(css,bok,crc250,irc)
    type(obs_session), pointer :: css !  current session
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    integer :: cnt
    character*22 :: myname="observation_getNextLoc"
    if(obs_bdeb)write(*,*)myname,' Entering.',bok
    if(obs_bdeb)write(*,*)myname,"ind_pe:",associated(css%ind_pe),css%sid
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
          call observation_errorappend(crc250," Error return from observation_stackfirst.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       css%currentFileSortIndex=1
       css%stackReady = .true.
    end if
    if(obs_bdeb)write(*,*)myname,' OOK.',css%currentFile%ook
    cnt=0
    ! loop until we have valid message or EOF
    LOOP : do
       do while (css%msg%vobs .eq. 0 .and. bok) ! bok=.false. -> end of file
          ! read next message (can contain several observations)
          call observation_getMsgObs(css,bok,crc250,irc)
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Error return from getMsgObs.")
             return
          end if
          css%msg%check=.false. ! redo message check
       end do
       if (.not.css%msg%check .and. css%msg%nobs .ne. 0 .and. bok) then
          ! apply the observation filter on the whole message (possibly several observations)
          call observation_checkMsgObs(css,crc250,irc)
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Error return from checkMsgObs.")
             return
          end if
          css%msg%check=.true. ! message check completed
       end if
       if (css%msg%vobs .ne. 0 .and. bok) then
          ! return next valid observation from the message...
          if (observation_popMsgObs(css,crc250,irc)) then
             exit LOOP ! got a valid observation
          end if
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Error return from popObs.")
             return
          end if
       else if (.not. bok) then ! end of file
          exit LOOP
       end if
    end do LOOP
    if(obs_bdeb)write(*,*)myname,' OOK.',css%currentFile%ook
    if(obs_bdeb)write(*,*)myname,' Done.',bok,cnt,isubset,nsubset
    return
  end subroutine observation_getNextLoc
  !
  ! put valid observations from message into the location chain
  !
  subroutine observation_getMsgObs(css,bok,crc250,irc)
    type(obs_session), pointer :: css !  current session
    logical :: bok           ! successful get (not EOF)
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="observation_getMsgObs"
    integer :: cnt, mcnt, ii, jj
    integer :: tcnt=0
    logical :: bbok
    mcnt=0
    MSG: do
       ! increment isubset
       if(obs_bdeb)write(*,'(X,A,X,A,3(I0,A))')myname,' Incrementing isubset ',&
            &isubset,' -> ',isubset+1,' (',nsubset,')'
       isubset=isubset+1
       if (isubset > nsubset) then ! read next message into memory
          if(obs_bdeb)write(*,*)myname,' Reading message.',bok
          call observation_autoMessage(css,bok,crc250,irc) ! always returns isubset=1, nsubset=ksec3(3)
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Error return from observation_autoMessage.")
             call observation_errorappendi(crc250,irc)
             call observation_errorappend(crc250,"\n")
             return
          end if
          if (nsubset.gt.0.and.bok) call observation_updateObsCnt(css,nsubset)
          css%msg%nobs=0 ! number of obs
          css%msg%vobs=0 ! remaining valid obs
          css%msg%iobs=0 ! current obs
          if(obs_bdeb)write(*,*)myname,' Read message, EOF:',bok,isubset,nsubset
          if (.not.bok) exit MSG
       else
          bok=.true.
       end if
       if(obs_bdeb)write(*,*)myname,' Checking obs.',bok
       if (bok) then
          cnt=0
          OBS : DO ! loop over message observations
             css%int_val(4)=css%int_val(4)+1 ! obs id
             if (observation_checkObs(css,bbok,crc250,irc)) then
                cnt=cnt+1
                mcnt=mcnt+1
                tcnt=tcnt+1
                css%msg%nobs=css%msg%nobs+1
                if(obs_bdeb)write(*,*)myname,' Found obs.',bbok,cnt,css%msg%nobs,css%msg%cobs
                ! store location in message-location chain and loop...
                ! check if we must allocate more memory
                css%msg => observation_allocateMsg(css,css%msg,crc250,irc)
                if (irc.ne.0) then
                   call observation_errorappend(crc250,myname)
                   call observation_errorappend(crc250," Error return from observation_checkMsg.")
                   call observation_errorappendi(crc250,irc)
                   call observation_errorappend(crc250,"\n")
                   return
                end if
                if(obs_bdeb)write(*,*)myname,' Setting msg obs.',css%msg%nobs,&
                     & allocated(css%msg%trg_val),css%msg%ctrg,css%msg%cobs
                do ii=1,css%msg%ctrg
                   css%msg%trg_val(ii,css%msg%nobs)=css%trg_val(ii)
                   css%msg%trg_vok(ii,css%msg%nobs)=css%trg_vok(ii)
                   if (obs_bdeb) then
                      if (css%trg_val(ii).gt.1.0D10) then
                         write(*,*)myname,' xxxxxxxxxxxxxx INVALID obs:',&
                              & css%msg%nobs,ii,&
                              & "'"//css%trg80(ii)(1:css%trg_lent(ii))//"'", &
                              & css%trg_val(ii),css%trg_vok(ii),bbok
                      end if
                   end if
                end do
                css%msg%trg_set(css%msg%nobs)=bbok ! is obs valid?
                if (bbok) then
                   css%msg%vobs=css%msg%vobs+1
                end if
                if(obs_bdeb)write(*,*)myname,' CheckObs Read message.',bbok
             else ! no more locations in message
                exit OBS
             end if
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Error return from observation_checkObs.")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
          end do OBS
          if (cnt.gt.0) call observation_updateLocCnt(css,cnt)
       end if
       if (css%msg%nobs.ne.0.or..not.bok) exit MSG ! valid obs in matrix
    end do MSG
    if(obs_bdeb)write(*,*)myname,' OOK.',css%currentFile%ook
    ! if (bok) write(*,*)myname,' Added obs:',mcnt,tcnt,isubset,nsubset
    if(obs_bdeb)write(*,*)myname,' Done.',bok,mcnt,isubset,nsubset
    return
  end subroutine observation_getMsgObs
  ! check that message has enough memory
  function observation_allocateMsg(css,msg,crc250,irc)
    type(obs_message), pointer :: observation_allocateMsg
    type(obs_session), pointer :: css !  current session
    type(obs_message), pointer :: msg
    logical :: bok           ! successful get (not EOF)
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="observation_allocateMsg"
    type(obs_message), pointer :: newmsg => null()
    integer :: cnt, ii, jj
    logical :: bbok
    if(obs_bdeb)write(*,*)myname,"ind_pe:",associated(css%ind_pe),css%sid
    if (css%msg%nobs.gt.css%msg%cobs) then ! allocate more memory
       allocate(newmsg,stat=irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Unable to allocate newmsg:")
          call observation_errorappendi(crc250,newmsg%nobs)
          call observation_errorappendi(crc250,newmsg%cobs)
          call observation_errorappend(crc250,"\n")
          return
       end if
       newmsg%nobs=css%msg%nobs
       newmsg%vobs=css%msg%vobs
       newmsg%cobs=2*(css%msg%nobs)
       newmsg%ctrg=max(css%msg%ctrg,css%ntrg)
       allocate(newmsg%trg_val(newmsg%ctrg,newmsg%cobs),&
            & newmsg%trg_vok(newmsg%ctrg,newmsg%cobs),&
            & newmsg%trg_set(newmsg%cobs),newmsg%trg_res(newmsg%cobs),&
            & stat=irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Unable to allocate msg:")
          call observation_errorappendi(crc250,newmsg%ctrg)
          call observation_errorappendi(crc250,newmsg%cobs)
          call observation_errorappend(crc250,"\n")
          return
       end if
       if (allocated(css%msg%trg_val)&
            & .and.allocated(css%msg%trg_vok)&
            & .and.allocated(css%msg%trg_set)&
            & .and.allocated(css%msg%trg_res)) then
          do jj=1,css%msg%nobs-1
             do ii=1,css%msg%ctrg
                newmsg%trg_val(ii,jj)=css%msg%trg_val(ii,jj)
                newmsg%trg_vok(ii,jj)=css%msg%trg_vok(ii,jj)
             end do
             newmsg%trg_set(jj)=css%msg%trg_set(jj)
             newmsg%trg_res(jj)=0.0D0
          end do
          if (allocated(css%msg%trg_val)) deallocate(css%msg%trg_val)
          if (allocated(css%msg%trg_vok)) deallocate(css%msg%trg_vok)
          if (allocated(css%msg%trg_set)) deallocate(css%msg%trg_set)
          if (allocated(css%msg%trg_res)) deallocate(css%msg%trg_res)
       else if (obs_bdeb) then
          write(*,*)myname,' Not allocated: msg%trg_* ',&
               & css%msg%nobs,css%msg%cobs
       end if
       if (associated(css%msg)) deallocate(css%msg,stat=irc)
       observation_allocateMsg => newmsg
    else
       observation_allocateMsg => css%msg
    end if
    return
  end function observation_allocateMsg
  !
  ! check observations in message simulaneosul using observation filter
  !
  subroutine observation_checkMsgObs(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="observation_checkMsgObs"
    integer :: ii
    logical :: bok
    ! loop over observations
    if (css%flt_set) then ! check if we have an observation filter...
       ! should probably reset the parser here if msg-functions are implemented...
       if (obs_bdeb)then
          write(*,*)myname,' Evaluating filter:',associated(css%psf),&
               & css%msg%ctrg,css%msg%cobs,&
               & size(css%msg%trg_val),size(css%msg%trg_set),size(css%msg%trg_res),&
               & allocated(css%msg%trg_val),allocated(css%msg%trg_set),&
               & allocated(css%msg%trg_res)
       end if
       call parse_evala(css%psf,css%msg%ctrg,css%msg%cobs,css%msg%nobs, &
            & css%msg%trg_val,css%msg%trg_set,css%msg%trg_res,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from evala.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       if (obs_bdeb)write(*,*)myname,' Looping over msg:',css%msg%nobs
       do ii=1,css%msg%nobs
          if (css%msg%trg_set(ii)) then
             ! evaluate filter
             bok=(nint(css%msg%trg_res(ii)).ne.0) ! NB bok is local, reject obs using trg_set->.false.
             if (obs_bdeb)write(*,*)myname,'Returned:',css%msg%trg_res(ii),bok
             if (bok) then
                css%currentFile%ook(5)=css%currentFile%ook(5)+1
             else
                css%currentFile%orm(5)=css%currentFile%orm(5)+1 ! search failed
                css%msg%vobs=css%msg%vobs-1
                css%msg%trg_set(ii)=.false. ! reject observation
             end if
          end if
       end do
    end if
    return
  end subroutine observation_checkMsgObs
  !
  ! pop next valid message obs to the session trg_val
  !
  logical function observation_popMsgObs(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="observation_popMsgObs"
    logical :: bok
    integer :: ii
    bok=(css%msg%vobs.gt.0)
    if (bok) then
       css%msg%iobs=css%msg%iobs+1
       do while (css%msg%iobs.le.css%msg%nobs)
          css%int_val(5)=css%int_val(5)+1 ! location id
          if (css%msg%trg_set(css%msg%iobs)) then ! passed msg-check?
             css%msg%vobs=css%msg%vobs-1
             do ii=1,css%msg%ctrg
                css%trg_val(ii)=css%msg%trg_val(ii,css%msg%iobs)
                css%trg_vok(ii)=css%msg%trg_vok(ii,css%msg%iobs)
                if (obs_bdeb) then                
                   if (css%trg_val(ii).gt.1.0D10) then
                      write(*,*)myname,' yyyyyyyyyyyyyyyy Invalid obs:',&
                           & css%msg%iobs,ii,css%trg_val(ii),css%msg%trg_set(css%msg%iobs)
                   end if
                end if
             end do
             ! recalculate internal variables
             do ii=1,css%ntarget
                select case (css%trg_type(ii))
                case (parse_internal)
                   if (obs_bdeb)write(*,*)myname,' Internals:',css%int_val
                   css%trg_val(ii)=parse_evalf(css%trg_psp(ii)%ptr,css%int_val,crc250,irc)
                   if (irc.ne.0) then
                      call observation_errorappend(crc250,myname)
                      call observation_errorappend(crc250," Error return from evalf.")
                      call observation_errorappendi(crc250,irc)
                      call observation_errorappend(crc250,"\n")
                      return
                   end if
                   css%trg_vok(ii)=.true.
                   if (obs_bdeb)write(*,*)myname,' Internal:',&
                        & ii,css%trg_val(ii)
                end select
             end do
             observation_popMsgObs=.true.
             return
          else
             css%msg%iobs=css%msg%iobs+1
          end if
       end do
       if (css%msg%vobs.ne.0) then
          if (obs_bdeb) then
             write(*,*) myname,'Obs count=',css%msg%nobs,css%msg%vobs,css%msg%iobs
             do ii=1,css%msg%nobs
                write(*,*)myname,'Obs:',ii,css%msg%trg_set(ii)
             end do
          end if
          irc=844
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," System error.")
          call observation_errorappendi(crc250,css%msg%vobs)
          call observation_errorappend(crc250,"\n")
          return
       end if
    end if
    observation_popMsgObs=.false.
    return
  end function observation_popMsgObs
  !
  subroutine observation_setfilter(css,flt,crc250,irc)
    implicit none
    type(obs_session), pointer :: css !  current session
    character*(*) :: flt
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    character*22 :: myname="observation_setfilter"
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       css%flt250=trim(flt)
       call chop0(css%flt250,250)
       css%lenf=length(css%flt250,250,10)
       if(obs_bdeb)write(*,*)myname,"Filter:'"//css%flt250(1:css%lenf)//"'",irc
    end if
    return
  end subroutine observation_setfilter
  !
  subroutine observation_compileFilter(css,crc250,irc)
    implicit none
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    integer :: jj
    character*22 :: myname="observation_ompileFilter"
    if (css%lenf.ne.0) then
       call parse_open(css%psf,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from parse_open.")
          return
       end if
       if(obs_bdeb)write(*,*)myname,"Parsing filter: '"//css%flt250(1:css%lenf)//"'",size(css%trg80)
       call parse_parsef(css%psf,css%flt250(1:css%lenf),css%trg80,crc250,irc)
       if (irc.ne.0) then
          if(obs_bdeb)then
             write(*,*)myname,"Unable to parse:'"//css%flt250(1:css%lenf)//"'"
             write(*,*)myname,' nvar:',css%ntrg
             do jj=1,css%ntrg
                write(*,*) myname,' var:',jj,css%trg80(jj)(1:css%trg_lent(jj))
             end do
          end if
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from parsef.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       call parse_used(css%psf,css%trg_req)
       css%flt_set=.true.
    end if
    if(obs_bdeb)write(*,*)myname,"Done."
    return
  end subroutine observation_compileFilter
  !
  subroutine observation_getfilter(css,flt,crc250,irc)
    implicit none
    type(obs_session), pointer :: css !  current session
    character*(*) :: flt
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_getfilter"
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       flt=css%flt250(1:css%lenf)
       if(obs_bdeb)write(*,*)myname,"Filter:'"//css%flt250(1:css%lenf)//"'",irc
    end if
    return
  end subroutine observation_getfilter
  !
  !###############################################################################
  ! REPORT PROCESSING
  !###############################################################################
  !
  ! remove report generation-item
  !
  subroutine observation_clearReports(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    type(obs_reportitem), pointer :: item, nextItem
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
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    type(obs_report), pointer :: newReport
    character*22 :: myname="observation_createReport"
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
    type(obs_session), pointer :: css !  current session
    character*250 :: buff250
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    type(obs_reportItem), pointer :: newItem
    character*22 :: myname="observation_addReportItem"
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
  ! Read next message into memory, open/close file automatically
  !
  subroutine observation_autoMessage(css,bok,crc250,irc)
    type(obs_session), pointer :: css !  current session
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="observation_autoMessage"
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
    call observation_readMessage(css,bok,crc250,irc)
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
  end subroutine observation_autoMessage
  !
  ! make raw-array
  !
  subroutine observation_getArrayLen(css,maxarr,bok,crc250,irc)
    type(obs_session), pointer :: css
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
    type(obs_session), pointer :: css
    integer :: maxarr        ! max number of array elements
    integer :: narr          ! number of array elements
    real*8 :: arr(maxarr)      ! bufr array
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    integer, external :: length
    integer :: lenb,len0,len1,len2,len3
    character*50 :: s0,s1,s2,s3
    integer :: ii,ipos
    narr=0
    if (isubset > nsubset) then
       bok=.false.
    else if (css%ignarr) then
       bok=.true.
    else
       bok=.true.
       DO II=1,KTDEXL
          IPOS=II+(isubset-1)*KEL
          if (values(ipos).eq.RVIND) then
             narr=narr+1
             arr(narr)=values(ipos)
          else if (.not.css%ignval) then
             narr=narr+1
             arr(narr)=values(ipos)
          end if
       end do
    end if
    return
  end subroutine observation_getArray
  !
  ! Make report item
  !
  subroutine observation_getReportLen(css,maxrep,bok,crc250,irc)
    type(obs_session), pointer :: css
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
    character*22 :: myname="observation_getReportLen"
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
    end if
    !write(*,*)myname,' Done.',irc
  end subroutine observation_getReportLen
  subroutine observation_getReport(css,maxrep,nrep,rep250,bok,crc250,irc)
    type(obs_session), pointer :: css
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
    type(obs_target), pointer :: currenttarget => null()
    logical :: bbok           ! was get successful?
    integer :: seq
    real :: val
    character*22 :: myname="observation_getReport"
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
             if ((values(ipos).eq.rvind.and..not.css%ignmis).or.values(ipos).ne.rvind) then
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
                   if (values(ipos).eq.RVIND) then
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
       if (css%ntarget .gt. 0) then
          if (css%category .eq. ksec1(6) .and. &
               & css%subcategory .eq. ksec1(7)) then
             do ii=1,css%ntarget
                seq=css%trg_seq(ii)
                bbok=(seq.le.ktdexl)
                if (bbok) bbok=(ktdexp(seq).eq.css%trg_descr(ii))
                if (css%trg_lval(1,ii).or.css%trg_lval(2,ii)) then
                   if (bbok.and.css%trg_lval(1,ii)) bbok=(values(seq).ge.css%trg_minval(ii))
                   if (bbok.and.css%trg_lval(1,ii)) bbok=(values(seq).le.css%trg_maxval(ii))
                end if
                if (bbok) then
                   s1=css%trg80(ii)(1:50) ; call chop0(s1,50); len1=length(s1,50,10)   ! element identification
                   write(s2,*) values(seq); call chop0(s1,50); len1=length(s1,50,10)
                   nrep=min(nrep+1,maxrep)
                   WRITE(rep250(nrep),'(A,A)')s0(1:len0)//sep//'target'//sep//&
                        & s1(1:len1)//sep//s2(1:len2)//" "
                end if
             end do
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
  subroutine observation_scanFile(css,bok,crc250,irc)
    type(obs_session), pointer :: css !  current session
    logical :: bok
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="observation_scanFile"
    integer :: cnt
    logical :: bbok
    if(obs_bdeb)write(*,*)myname,' Entering.',irc
    !
    ! open file
    !
    call observation_openFile(css,bok,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from observation_openFile.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    !
    ! loop over file
    !
    if (bok) then
       if(obs_bdeb)write(*,*)myname,' Calling readfile.'
       bok=.false. ! must read at least one observation
       bbok=.true.
       do while (bbok)
          call observation_readMessage(css,bbok,crc250,irc)
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Error return from observation_readMessage.")
             call observation_errorappendi(crc250,irc)
             call observation_errorappend(crc250,"\n")
             return
          end if
          if (bbok) then
             cnt=0
             do while (observation_checkObs(css,bok,crc250,irc))
                cnt=cnt+1
             end do
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Error return from observation_checkObs.")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
          end if
          if(obs_bdeb)write(*,'(X,A,X,A,3(I0,A))')myname,' Setting isubset ',&
               &isubset,' -> ',nsubset+1,' (',nsubset,')'
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
       if (css%currentFile%nsubset.eq.0) then
          if(obs_bdeb)write(*,*)myname,'No subsets found.'
       else
          !write(*,*)myname,'Subsets:',css%currentFile%nsubset
       end if
    end if
    if(obs_bdeb)write(*,*)myname,' Done.',irc
    return
  end subroutine observation_scanFile
  !
  ! check if obs is ok, store statistics. 
  ! Returns .false. if no more obs in message.
  ! bok is .true. if observation is ok
  !
  logical function observation_checkObs(css,bok,crc250,irc)
    type(obs_session), pointer :: css
    logical :: bok           ! is everything ok?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="observation_checkObs"
    integer :: yy,mm,dd,hh,mi,cnt
    real :: sec, j2000
    if (observation_eval(css,bok,crc250,irc)) then
       css%currentfile%ook(1)=css%currentfile%ook(1)+1
       observation_checkObs=.true. ! we have a valid observation in memory
       ! store max/min values and counts
       if (bok) then
          css%currentfile%ook(2)=css%currentfile%ook(2)+1 ! successful evaluation
          if (css%ind_eset) then
             if (css%currentFile%ind_lim) then
                css%currentFile%ind_start=min(css%currentFile%ind_start,css%ind_val)
                css%currentFile%ind_stop=max(css%currentFile%ind_stop,css%ind_val)
             else
                css%currentFile%ind_lim=.true.
                css%currentFile%ind_start=css%ind_val
                css%currentFile%ind_stop=css%ind_val
             end if
          else if (obs_bdeb) then
             write(*,*)myname,'Index not set, no index limits available.'
          end if
          yy=KSEC1( 9)
          mm=KSEC1(10)
          dd=KSEC1(11)
          hh=KSEC1(12)
          mi=KSEC1(13)
          if (yy <= 99) then
             if (yy < 78) then
                yy = yy + 2000
             else
                yy = yy + 1900
             end if
          end if
          sec=0.0D0
          if(obs_bdeb)write(*,'(X,A,X,A,I4.4,4(A,I2.2),A)') myname,"Time: ",&
               & yy,"/",mm,"/",dd," ",hh,":",mi,"  => '"//&
               & css%currentFile%fn250(1:css%currentFile%lenf)//"'"
          call jd2000(j2000,yy,mm,dd,hh,mi,sec)
          !read file and get start/end indexs...
          if (css%currentFile%time_lim) then
             css%currentFile%time_start=min(css%currentFile%time_start,j2000)
             css%currentFile%time_stop=max(css%currentFile%time_stop,j2000)
          else
             css%currentFile%time_lim=.true.
             css%currentFile%time_start=j2000
             css%currentFile%time_stop=j2000
          end if
          !if(obs_bdeb)write(*,*)myname,"MaxMin:",css%ind_eset,css%ind_val,css%currentFile%ind_start,css%currentFile%ind_stop
          ! write(*,*) myname,"Value:",css%ind_val,css%currentFile%ind_start,css%currentFile%ind_stop
       else
          css%currentfile%orm(2)=css%currentfile%orm(2)+1 ! evaluation failed
       end if
       if(obs_bdeb)write(*,*)myname,' Index limits:',css%ind_eset,css%ind_val,&
            & css%currentFile%ind_lim,css%currentFile%ind_start,css%currentFile%ind_stop,bok
       !
       ! check against index limits
       !
       if (bok) then
          if (css%ind_eset) then
             if (css%ind_lval(1) .and. css%ind_lval(2)) then ! between
                bok= (css%ind_val.ge.css%ind_minval .and.css%ind_val.le.css%ind_maxval)
             else if (css%ind_lval(1)) then ! above
                bok= (css%ind_val.ge.css%ind_minval)
             else if (css%ind_lval(2)) then ! below
                bok= (css%ind_val.le.css%ind_maxval)
             end if
          end if
          if (bok) then
             css%currentfile%ook(3)=css%currentfile%ook(3)+1 ! inside index limits
          else
             if (obs_bdeb)write(*,*)myname,' Outside index limits:',&
                  & css%ind_lval,css%ind_val,css%ind_minval,css%ind_maxval
             css%currentfile%orm(3)=css%currentfile%orm(3)+1 ! out of index limits
          end if
       end if
       !
       ! check against targets
       !
       if (bok) then
          if(obs_bdeb)write(*,*)myname,' Checking target.'
          call observation_checkTarget(css,bok,crc250,irc)
          IF(IRC.NE.0) THEN
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Error return from checkTarget.");
             call observation_errorappend(crc250,"\n")
             observation_checkObs=.false. ! exit loop
             RETURN
          END IF
          if (bok) then
             css%currentfile%ook(4)=css%currentfile%ook(4)+1 ! target check ok
          else
             css%currentfile%orm(4)=css%currentfile%orm(4)+1 ! target check failed
          end if
       else
       end if
    else
       bok=.false. ! "observation" is no good..
       observation_checkObs=.false. ! exit loop
    end if
    if(obs_bdeb)write(*,*)myname,'Done:',irc,observation_checkObs
    IF(IRC.NE.0) THEN
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from eval.");
       call observation_errorappend(crc250,"\n")
       observation_checkObs=.false. ! exit loop
       RETURN
    END IF
    return
  end function observation_checkObs
  !
  !###############################################################################
  ! ECMWF LIBEMOS ROUTINES FOR READING BUFR FILES
  !###############################################################################
  !
  ! open file
  !
  subroutine observation_openFile(css,bok,crc250,irc)
    type(obs_session), pointer :: css
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    integer :: lenf
    integer,external :: length
    character*250 :: fn250
    character*3 :: mode
    character*22 :: myname="observation_openFile"
    integer :: ii
    !
    !     MISSING VALUE INDICATOR
    ! 
    NBYTPW=JBPW/8
    NVIND=2147483647
    css%currentfile%NSUBSET=0
    css%currentFile%NMESSAGE=0
    if(obs_bdeb)write(*,*)myname,'Opening obsfile: ',css%currentfile%fn250(1:css%currentfile%lenf)
    CALL PBOPEN(UNIT,css%currentfile%fn250(1:css%currentfile%lenf),'R',irc)
    !if(obs_bdeb)write(*,*)myname,'Opened file: ',css%currentfile%fn250(1:css%currentfile%lenf)
    if (irc.ne.0) then
       if(obs_bdeb)write(*,*)myname,'Unable to open file.',irc
       call observation_errorappend(crc250,myname)
       IF(irc.EQ.-1) call observation_errorappend(crc250,'OPEN FAILED.')
       IF(irc.EQ.-2) call observation_errorappend(crc250,'INVALID FILE NAME.')
       IF(irc.EQ.-3) call observation_errorappend(crc250,'INVALID OPEN MODE SPECIFIED.')
       call observation_errorappend(crc250,"observation_openFile"//css%currentfile%fn250(1:css%currentfile%lenf));
       call observation_errorappend(crc250,"\n")
       bok=.false.
       return
    end if
    call observation_compile(css,crc250,irc)
    IF (IRC.NE.0) THEN
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Error compiling index expressions.");
       call observation_errorappend(crc250,"\n")
       return
    end if
    if (.not. css%keepstat) then
       do ii=1,10
          css%currentfile%mok(ii)=0
          css%currentfile%mrm(ii)=0
          css%currentfile%ook(ii)=0
          css%currentfile%orm(ii)=0
          css%currentfile%hint80(ii)=""
       end do
       do ii=1,css%ntrg
          css%trg_ook(ii)=0
          css%trg_orm(ii)=0
       end do
    end if
    css%int_val(3)=0 ! message id
    css%int_val(4)=0 ! obs id
    css%int_val(5)=0 ! location id
    call observation_clearCat(css%currentFile)
    fopen=.true.
    css%fopened=css%fopened+1
    return
  end subroutine observation_openFile
  !
  ! read next BUFR message
  !
  subroutine observation_readMessage(css,bok,crc250,irc)
    type(obs_session), pointer :: css
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    integer :: ii, jj
    character*25 :: myname="observation_readMessage"
    if(obs_bdeb)write(*,*)myname,' Entering.'
    css%dyn_pos=0               ! reset dynamic position search index
    css%dyn_cnt=0               ! reset dynamic position search index
    irc=0
    msg: do
       bok=.true.
       KBUFL=0
       CALL PBBUFR(UNIT,KBUFF,JBUFL,KBUFL,irc)
       IF (IRC.EQ.-1) THEN ! EOF
          if(obs_bdeb)PRINT*,'NUMBER OF SUBSETS     ',css%currentFile%NSUBSET
          if(obs_bdeb)PRINT*,'NUMBER OF MESSAGES    ',css%currentFile%NMESSAGE
          IRC=0
          bok=.false.
          return
       end if
       IF (IRC.NE.0) THEN
          if(obs_bdeb)write(*,*)myname,'Unable to read file.',irc
          call observation_errorappend(crc250,myname)
          IF(irc.EQ.-2) call observation_errorappend(crc250,'FILE HANDLING PROBLEM.' )
          IF(irc.EQ.-3) call observation_errorappend(crc250,'ARRAY TOO SMALL FOR PRODUCT.')
          call observation_errorappend(crc250,"observation_readMessage"//css%currentFile%fn250(1:css%currentFile%lenf));
          call observation_errorappend(crc250,"\n")
          return
       end if
       bread=.true. ! new message in memory
       css%currentfile%nmessage=css%currentfile%nmessage+1
       css%int_val(3)=css%int_val(3)+1 ! message id
       css%int_val(4)=0 ! location id
       css%int_val(5)=0 ! location id
       !     PRINT*,'----------------------------------',N,' ',KBUFL
       KBUFL=KBUFL/NBYTPW+1
       !
       css%currentfile%mok(1)=css%currentfile%mok(1)+1
       ! if(obs_bdeb)WRITE(*,*)myname,'Calling BUS0123...',kbufl,nbytpw,jbufl,jbpw

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
          if (css%currentfile%mrm(2).eq.1) write(*,*)myname,'Unable to decode a message header in:',&
               & css%currentFile%fn250(1:css%currentFile%lenf),irc
          if(obs_bdeb)write(*,*)'ERROR IN BUS012: CORRUPTED BUFR MESSAGE.',css%currentfile%nmessage,&
               & ' in file:',css%currentFile%fn250(1:css%currentFile%lenf)
          IRC=0
          css%currentfile%mrm(2)=css%currentfile%mrm(2)+1 ! unable to decode header
          cycle msg
       else
          css%currentfile%mok(2)=css%currentfile%mok(2)+1
       END IF
       !
       if(obs_bdeb)write(*,'(X,A,X,A,3(I0,A))')myname,' Setting nsubset ',&
            &nsubset,' -> ',ksec3(3),' (',isubset,')'
       nsubset=KSEC3(3)
       KEL=KVALS/nsubset
       IF(KEL.GT.KELEM) KEL=KELEM
       if(obs_bdeb)WRITE(*,*)myname,'KSUP:',ksup(5),ksup(6),KEL, nsubset
       !
       !write(*,*)myname,'Before BUFREX.'
       CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,&
            & KSEC1,KSEC2 ,KSEC3 ,&
            & KSEC4,KEL,CNAMES,CUNITS,&
            & KVALS,VALUES,CVALS,irc)
       !write(*,*)myname,'After BUFREX.'
       if(obs_bdeb)write(*,*)myname,'Done BUFREX'
       IF(IRC.NE.0) THEN
          css%currentfile%mrm(3)=css%currentfile%mrm(3)+1 ! unable to decode body
          if (css%currentfile%mrm(3).eq.1) then
             write(*,*)myname,'Unable to decode a message body in:',&
                  & css%currentFile%fn250(1:css%currentFile%lenf),irc
          end if
          !call observation_errorappend(crc250,myname)
          !call observation_errorappend(crc250,"Unable to decode "//css%currentFile%fn250(1:css%currentFile%lenf));
          !call observation_errorappend(crc250,"\n")
          IRC=0
          cycle msg
       else
          css%currentfile%mok(3)=css%currentfile%mok(3)+1 ! able to decode body
       END IF
       css%currentfile%NSUBSET=css%currentfile%NSUBSET+nsubset
       if(obs_bdeb)write(*,'(X,A,X,A,3(I0,A))')myname,' Setting isubset ',&
            &isubset,' -> ',1,' (',nsubset,')'
       ISUBSET=1 ! start with first subset
       !write(*,*)myname,'G:'
       CALL BUSEL2(ISUBSET,KEL,KTDLEN,&
            & KTDLST,KTDEXL,KTDEXP(1), &
            & CNAMES,CUNITS,irc)

       !write(*,*)myname,'H:',ISUBSET
       if (irc.ne.0) then
          css%currentfile%mrm(4)=css%currentfile%mrm(4)+1 ! unable to decode description
          !write(*,*)myname,'Unable to decode message description.',irc
          if(obs_bdeb)write(*,*)'ERROR IN BUSEL2: CORRUPTED BUFR MESSAGE.',css%currentfile%nmessage,&
               & ' in file:',css%currentFile%fn250(1:css%currentFile%lenf)
          irc=0
          cycle msg
       else
          css%currentfile%mok(4)=css%currentfile%mok(4)+1 ! able to decode description
       end if
       ! store category:
       if(obs_bdeb)write(*,*)myname,'Found BUFR cat:',KSEC1( 6),KSEC1( 7)
       if (.not.css%igncat) call observation_storeCat(css%currentFile,KSEC1( 6),KSEC1( 7))
       ! CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,IRC)
       ! if (irc.ne.0) then
       !    write(*,*)myname,'Unable to decode message keys.',irc
       !    if(obs_bdeb)write(*,*)'ERROR IN BUUKEY: CORRUPTED BUFR MESSAGE.',css%currentfile%nmessage,&
       !         & ' in file:',css%currentFile%fn250(1:css%currentFile%lenf)
       !    irc=0
       !    cycle msg
       ! end if
       nitem=KTDEXL
       !
       ! check descr
       !
       if(obs_bdeb)write(*,*)myname,'Checking obs.'
       call observation_checkDescr(css,bok,crc250,irc)
       IF(IRC.NE.0) THEN
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from eval.");
          call observation_errorappend(crc250,"\n")
          RETURN
       END IF
       if(obs_bdeb)write(*,'(X,A,A,4(X,I0),X,L1)')myname,' Checked obs.',&
            & KSEC1( 6),KSEC1( 7),css%category,css%subcategory,bok
       if (.not. bok) then
          irc=0
          cycle msg
       else
          exit msg
       end if
    end do msg
    if(obs_bdeb)write(*,*)myname,' Done.',css%currentFile%ook
    return
  end subroutine observation_readMessage
  !
  ! close file
  !
  subroutine observation_closeFile(css,crc250,irc)
    type(obs_session), pointer :: css
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="observation_closeFile"
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
    isubset=1
    nsubset=0
    call observation_terminate(css,crc250,irc)
    IF (IRC.NE.0) THEN
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Error compiling index expressions.");
       call observation_errorappend(crc250,"\n")
       return
    end if
    fopen=.false.
    !
  end subroutine observation_closeFile


  !######################################################
  subroutine observation_ignoreCat(css)
    type(obs_session), pointer :: css
    css%igncat=.true.
    !write(*,*) 'observation_clearCat Done.'
  end subroutine observation_ignoreCat
  !
  subroutine observation_clearCat(currentFile)
    type(obs_file), pointer :: currentFile
    type(obs_mainCategory),pointer :: currentCat, nextCat
    type(obs_subCategory),pointer :: currentSub, nextSub
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
             if (allocated(currentSub%values)) deallocate(currentSub%values)
             deallocate(currentSub)
             currentSub=>nextSub
          end do
          deallocate(currentCat)
          currentCat=>nextCat
       end do
       currentFile%ncat=0
       currentFile%nsub=0
    end if
    currentFile%firstCategory%next => currentFile%lastCategory
    currentFile%lastCategory%prev => currentFile%firstCategory
    !write(*,*) 'observation_clearCat Done.'
  end subroutine observation_clearCat
  !
  subroutine observation_storeCat(currentFile,cat,subcat)
    type(obs_file), pointer :: currentFile
    integer :: cat
    integer :: subcat
    type(obs_mainCategory),pointer :: currentCat
    type(obs_subCategory),pointer :: currentSub
    integer ii,irc
    character*22 :: myname="observation_storeCat"
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
               & currentSub%cunits(currentSub%ktdexl),currentSub%values(currentSub%ktdexl),stat=irc)
          if (irc.ne.0) then
             currentSub%ktdexl=0
             write(*,*) myname,"Unable to allocate 'sub-sequence'."
             !call observation_errorappend(crc250,myname)
             !call observation_errorappend(crc250,"Unable to allocate 'sub-sequence'.")
             !call observation_errorappend(crc250,"\n")
             !return
          end if
          do ii=1,currentSub%ktdexl
             currentSub%ktdexp(ii)=ktdexp(ii)
             currentSub%cnames(ii)=cnames(ii)
             currentSub%cunits(ii)=cunits(ii)
             currentSub%values(ii)=values(ii)
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
    ! store data sequence
    currentSub%ktdexl=KTDEXL
    allocate(currentSub%ktdexp(currentSub%ktdexl),currentSub%cnames(currentSub%ktdexl),&
         & currentSub%cunits(currentSub%ktdexl),currentSub%values(currentSub%ktdexl),stat=irc)
    if (irc.ne.0) then
       currentSub%ktdexl=0
       write(*,*) myname,"Unable to allocate 'sub-sequence'."
       !call observation_errorappend(crc250,myname)
       !call observation_errorappend(crc250,"Unable to allocate 'sub-sequence'.")
       !call observation_errorappend(crc250,"\n")
       !return
    end if
    do ii=1,currentSub%ktdexl
       currentSub%ktdexp(ii)=ktdexp(ii)
       currentSub%cnames(ii)=cnames(ii)
       currentSub%cunits(ii)=cunits(ii)
       currentSub%values(ii)=values(ii)
    end do
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
    character*22 :: myname="observation_gettime"
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
    character*22 :: myname="observation_getj2000"
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
    if(obs_bdeb)write(*,*)myname,' Time:',time50(1:lent)," Found:",j2000
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
    type(obs_table), pointer :: ptable
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
    use sort
    implicit none
    integer :: icode
    integer :: isubcode
    character*250 :: crc250
    integer :: irc
    integer :: left,right,bingo
    type(obs_table), pointer :: ptable
    character*22 :: myname="observation_getCodeValue"
    if (.not.ctableInit) then
       !
       call observation_initCodeTable(irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from observation_initCodeTable.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       ctableInit=.true.
    end if
    call sort_heapsearch1i(ctable%maxnn,ctable%codes, &
         & ctable%nn,ctable%index,icode,left,right)
    if (left.eq.right) then
       bingo=left
       call sort_heapsearch1i(ctable%tables(bingo)%maxnn,ctable%tables(bingo)%subcodes, &
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
    character*22 :: myname="observation_initCodeTable"
    call chop0(c250,250)
    lent=length(c250,250,10)
    if (lent.ne.0) then
       do ii=1,2
          cnt=0
          unit=ftunit(irc)
          ! open file
          open(unit=iunit,file=c250(1:lent),iostat=irc,status="old",&
               & FORM='FORMATTED',ACCESS='SEQUENTIAL')
          if (irc.ne.0) then
             if(obs_bdeb)write(*,*) myname,"Unable to open '"//c250(1:lent)//"'"
             return
          end if
          line=0
          read(iunit,'(I6,X,I4,X,I8,X,I2,X,A)',iostat=irc) code, scnt, subcode, lcnt, val250
          if (irc.ne.0) then
             return
          end if
          line=line+1
          if (irc.ne.0) then
             if(obs_bdeb)write(*,*) myname,'Unable to read: ',c250(1:lent)
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
                if (irc.ne.0) then
                   return
                end if
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
                else if (obs_bdeb) then
                   write(*,*)myname,'Error reading line:',line
                end if
             end do
             read(iunit,'(I6,X,I4,X,I8,X,I2,X,A)',iostat=irc) code, scnt, subcode, lcnt, val250
             if (irc.ne.0) then
                return
             end if
             bdone=(irc.ne.0)
             line=line+1
             irc=0
          end do
          close(iunit,iostat=irc)
          if (irc.ne.0) then
             return
          end if
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
  ! keep track of locations per message
  !
  subroutine observation_updateObsCnt(css,cnt)
    type(obs_session), pointer :: css
    integer :: cnt
    if (css%ocnt.eq.0.or.cnt.lt.css%omin) css%omin=cnt
    if (css%ocnt.eq.0.or.cnt.gt.css%omax) css%omax=cnt
    css%osum=css%osum+cnt
    css%ocnt=css%ocnt+1
    return
  end subroutine observation_updateObsCnt
  !
  subroutine observation_updateLocCnt(css,cnt)
    type(obs_session), pointer :: css
    integer :: cnt
    if (css%lcnt.eq.0.or.cnt.lt.css%lmin) css%lmin=cnt
    if (css%lcnt.eq.0.or.cnt.gt.css%lmax) css%lmax=cnt
    css%lsum=css%lsum+cnt
    css%lcnt=css%lcnt+1
    return
  end subroutine observation_updateLocCnt
  !
  subroutine observation_resetStat(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    integer :: ii
    type(obs_file), pointer :: cfile
    character*22 :: myname="observation_resetStat"
    if (obs_bdeb) write(*,*)myname,'Entering.',irc
    css%fopened = 0
    do ii=1,10
       css%fok(ii)=0
       css%frm(ii)=0
    end do
    css%keepstat=.true.
    if (associated(css%firstFile)) then
       cfile => css%firstFile%next
       do while (.not.associated(cfile,target=css%lastFile))
          ! update statistics
          do ii=1,10
             cfile%ook(ii)=0
             cfile%orm(ii)=0
             cfile%mok(ii)=0
             cfile%mrm(ii)=0
          end do
          cfile  => cfile%next
       end do
    end if
    do ii=0,css%ntrg
       css%trg_orm(ii)=0
       css%trg_ook(ii)=0
    end do
    css%ocnt=0
    css%osum=0
    css%lcnt=0
    css%lsum=0
    if (obs_bdeb) write(*,*)myname,'Done.',irc
    return
  end subroutine observation_resetStat
 !
  subroutine observation_printStat(css,crc250,irc)
    type(obs_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    integer :: ii
    integer :: ook(10),orm(10),mok(10),mrm(10)
    real :: pst(10),pp
    type(obs_file), pointer :: cfile
    character*22 :: myname="observation_printStat"
    do ii=1,10
       ook(ii)=0
       orm(ii)=0
       mok(ii)=0
       mrm(ii)=0
    end do
    !
    ! file statistics
    do ii=1,10
       pst(ii)=dfloat(css%frm(ii))/max(1.0d0,dfloat(css%frm(ii)+css%fok(ii)))*100
    end do
    WRITE(*,*)
    WRITE(*,998) MYNAME,                     'Possible obs file matches: ', css%fok(1)+css%frm(1)
    IF (CSS%FRM(1).NE.0) WRITE(*,999) MYNAME,'No overlap:                ', -CSS%FRM(1),PST(1)
    IF (CSS%FRM(2).NE.0) WRITE(*,999) MYNAME,'Out of range for sure:     ', -CSS%FRM(2),PST(2)
    IF (CSS%FRM(3).NE.0) WRITE(*,999) MYNAME,'Out of range fine check:   ', -CSS%FRM(3),PST(3)
    WRITE(*,997) MYNAME,     '--------------------------------------------------'
    pp=dfloat(css%fok(3))/max(1.0d0,dfloat(css%frm(1)+css%fok(1)))*100
    WRITE(*,999) MYNAME,                     'Obs file matches:          ', css%fok(3),pp
    !
    if (css%fopened.eq.0) then
       WRITE(*,*)
       write(*,*)myname,'No obs files were opened. How strange!'
    else
       write(*,'(X,A,X,A,I0)')myname,'Obs files opened:',css%fopened
    end if
    ! accumulate file statistics...
    if (associated(css%firstFile)) then
       cfile => css%firstFile%next
       do while (.not.associated(cfile,target=css%lastFile))
          ! update statistics
          do ii=1,10
             ook(ii)=ook(ii)+cfile%ook(ii)
             orm(ii)=orm(ii)+cfile%orm(ii)
             mok(ii)=mok(ii)+cfile%mok(ii)
             mrm(ii)=mrm(ii)+cfile%mrm(ii)
          end do
          cfile  => cfile%next
       end do
    end if
    ! message statistics
    do ii=1,10
       pst(ii)=dfloat(mrm(ii))/max(1.0d0,dfloat(mrm(ii)+mok(ii)))*100
    end do
    WRITE(*,*)
    WRITE(*,998) MYNAME,                 'BUFR messages:               ', mok(1)+mrm(1)
    IF (MRM(2).NE.0) WRITE(*,999) MYNAME,'Unable to decode header:     ', -mRM(2),PST(2)
    IF (MRM(3).NE.0) WRITE(*,999) MYNAME,'Unable to decode body:       ', -mRM(3),PST(3)
    IF (MRM(4).NE.0) WRITE(*,999) MYNAME,'Unable to decode description:', -mRM(4),PST(4)
    IF (MRM(5).NE.0) WRITE(*,999) MYNAME,'Other BUFR/sub-type:         ', -mRM(5),PST(5)
    IF (MRM(6).NE.0) WRITE(*,999) MYNAME,'DESCR out of range:          ', -mRM(6),PST(6)
    IF (MRM(7).NE.0) WRITE(*,999) MYNAME,'DESCR mismatch:              ', -mRM(7),PST(7)
    WRITE(*,997) MYNAME,     '----------------------------------------------------'
    pp=dfloat(mok(7))/max(1.0d0,dfloat(mrm(1)+mok(1)))*100
    WRITE(*,999) MYNAME,                 'Accepted BUFR messages:      ', mok(7),pp
    ! MESSAGE -> OBS -> Location
    if (css%lcnt.ne.0) then
       write(*,*)
       write(*,'(X,A12,X,A,I0,A,I0,A,F0.2,A,I0)') myname,&
            & "Obs per message, min:",css%omin," max:",css%omax," avg:",&
            real(css%osum)/max(1.0D0,real(css%ocnt))," valid:",css%ocnt
       write(*,'(X,A12,X,A,I0,A,I0,A,F0.2,A,I0)') myname,&
            & "Loc per obs,     min:",css%lmin," max:",css%lmax," avg:",&
            real(css%lsum)/max(1.0D0,real(css%lcnt))," valid:",css%lcnt
    end if
    ! location statistics
    do ii=1,10
       pst(ii)=dfloat(orm(ii))/max(1.0d0,dfloat(orm(ii)+ook(ii)))*100
    end do
    WRITE(*,*)
    WRITE(*,998) MYNAME,                 'Locations:                   ', ook(1)+orm(1)
    IF (ORM(2).NE.0) WRITE(*,999) MYNAME,'Evaluation error:            ', -orm(2),PST(2)
    IF (ORM(3).NE.0) WRITE(*,999) MYNAME,'Index filter:                ', -orm(3),PST(3)
    do ii=1,css%ntrg
       pp=dfloat(css%trg_orm(ii))/max(1.0d0,dfloat(css%trg_orm(0)+css%trg_ook(0)))*100
       IF (css%trg_orm(ii).NE.0) WRITE(*,996) MYNAME,"'"//css%trg80(ii)(1:css%trg_lent(ii))//"' filter:", &
            & -css%trg_orm(ii),PP
    end do
    IF (ORM(5).NE.0) WRITE(*,999) MYNAME,'Obs filter:                  ', -orm(5),PST(5)
    WRITE(*,997) MYNAME,     '----------------------------------------------------'
    pp=dfloat(ook(5))/max(1.0d0,dfloat(orm(1)+ook(1)))*100
    WRITE(*,999) MYNAME,                 'Accepted locations:          ', ook(5),pp
    !
999 FORMAT(X,A12,X,A,I13,' (',F6.2,'%)')
998 FORMAT(X,A12,X,A,I13)
997 FORMAT(X,A12,X,A)
996 FORMAT(X,A12,X,A30,I13,' (',F6.2,'%)')
    
    return
  end subroutine observation_printStat
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
    character*22 :: myname="observation_errorappend"
    call chop0(crc250,250)
    lenc=length(crc250,250,10)
    lenb=len(trim(string))
    buff250=string(1:lenb)
    if (lenc.eq.0) then
       crc250=buff250(1:lenb)
    else
       crc250=crc250(1:lenc)//""//buff250(1:min(250-lenc-1,lenb))
    end if
    call chop0(crc250,250)
  end subroutine observation_errorappend
  subroutine observation_errorappendi(crc250,inum)
    implicit none
    character*250 :: crc250
    integer :: inum
    character*250 :: buff250
    integer :: lenc, lenb
    integer, external :: length
    character*22 :: myname="observation_errorappendi"
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
    call chop0(crc250,250)
  end subroutine observation_errorappendi

  character*250 function observation_diff(dt)
    real :: dt
    character*250 :: buff250
    character*25 :: siff25
    integer, external :: length
    integer :: lenb,lens
    integer :: dd
    integer :: hh
    integer :: mi
    integer :: ss
    lenb=0
    call chop0(buff250,250)
    ss=nint(dt*86400.0D0)
    if (ss.gt.86400) then
       if (lenb.gt.0) then
          buff250=buff250(1:lenb)//" "
          lenb=lenb+1
       end if
       dd=int(ss/86400)
       ss=ss-dd*86400
       write(siff25,*)dd
       call chop0(siff25,25)
       lens=length(siff25,25,5)
       buff250=buff250(1:lenb)//siff25(1:lens)//"d"
       call chop0(buff250,250)
       lenb=length(buff250,250,lenb)
    end if
    if (ss.gt.3600) then
       if (lenb.gt.0) then
          buff250=buff250(1:lenb)//" "
          lenb=lenb+1
       end if
       hh=int(ss/3600)
       ss=ss-hh*3600
       write(siff25,*)hh
       call chop0(siff25,25)
       lens=length(siff25,25,5)
       buff250=buff250(1:lenb)//siff25(1:lens)//"h"
       call chop0(buff250,250)
       lenb=length(buff250,250,lenb)
    end if
    if (ss.gt.60) then
       if (lenb.gt.0) then
          buff250=buff250(1:lenb)//" "
          lenb=lenb+1
       end if
       mi=int(ss/60)
       ss=ss-mi*60
       write(siff25,*)mi
       call chop0(siff25,25)
       lens=length(siff25,25,5)
       buff250=buff250(1:lenb)//siff25(1:lens)//"m"
       call chop0(buff250,250)
       lenb=length(buff250,250,lenb)
    end if
    if (ss.gt.0) then
       if (lenb.gt.0) then
          buff250=buff250(1:lenb)//" "
          lenb=lenb+1
       end if
       write(siff25,*)ss
       call chop0(siff25,25)
       lens=length(siff25,25,5)
       buff250=buff250(1:lenb)//siff25(1:lens)//"s"
       call chop0(buff250,250)
       lenb=length(buff250,250,lenb)
    end if
    observation_diff=buff250
    return
  end function observation_diff

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
    character*22 :: myname="observation_pretty"
    buff250=""
    lenb=0
    do ii=1,ndims
       lend=length(dimnames(ii),80,10)
       if(obs_bdeb)write(*,*) "observation_pretty  dimnames:",dimnames(ii)(1:lend),start(ii),vsize(ii)
       if (vsize(ii).gt.1) then
          write(yuff250,*)vsize(ii);call chop0(yuff250,250);lenx=length(yuff250,250,2)
          write(xuff250,'(I8,"+",A)')start(ii),yuff250(1:lenx);call chop0(xuff250,250);lenx=length(xuff250,250,2)
       else
          write(xuff250,'(I8)')start(ii);call chop0(xuff250,250);lenx=length(xuff250,250,2)
       end if
       xuff250=dimnames(ii)(1:lend)//"["//xuff250(1:lenx)//"]";call chop0(xuff250,250);lenx=length(xuff250,250,2)
       if(obs_bdeb)write(*,*)myname,'XUFF:',xuff250(1:lenx)
       if (lenb.eq.0) then
          buff250=xuff250(1:lenx)
       else
          buff250=buff250(1:lenb)//","//xuff250(1:lenx)
       end if
       call chop0(buff250,250)
       lenb=length(buff250,250,10)
    end do
    lenb=length(buff250,250,10)
    if(obs_bdeb)write(*,*)myname,'Buff:',buff250(1:lenb)
    lenv=length(varname,80,10)
    xuff250=varname(1:lenv)//"("//buff250(1:lenb)//")";
    call chop0(xuff250,250);
    observation_pretty=xuff250
  end function observation_pretty
  !
  ! parse and compile index expression
  !
  subroutine observation_compile(css,crc250,irc)
    type(obs_session), pointer :: css   ! session structure
    character*250 :: crc250         ! error message string
    integer :: irc                  ! error return code (0=ok)
    character*22 :: myname="observation_compile"
    integer :: ii,jj
    type(obs_target), pointer :: currenttarget => null()
    if(obs_bdeb)write(*,*)myname,' Entering.',obs_bdeb
    call observation_makeTargetList(css,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," Error return from maketargetlist.")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    if(obs_bdeb)write(*,*)myname,' Parsing.',css%ind_eset
    if (css%ind_eset) then
       call parse_open(css%ind_pe,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from parse_open.")
          return
       end if
       call parse_parsef(css%ind_pe,css%ind_exp250(1:css%ind_lene),css%trg80,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from parsef.")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       call parse_used(css%ind_pe,css%trg_req)
       if(obs_bdeb)write(*,*)myname,' Parsed.',css%ind_exp250(1:css%ind_lene),associated(css%ind_pe)
    end if
    if (css%dyn_set) then
       ! parse position expressions
       do ii=1,css%ntarget
          select case (css%trg_type(ii)) ! process position
          case (parse_internal)
             call parse_parsef(css%trg_psp(ii)%ptr,&
                  & css%trg_pos250(ii)(1:css%trg_lenp(ii)),&
                  & css%int_var,crc250,irc)
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Error return from parsef.")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
          case (parse_expression)
             if(obs_bdeb)then
                write(*,*)myname," Compiling pos: '"//&
                     & css%trg_pos250(ii)(1:css%trg_lenp(ii))//"'",ii
                do jj=1,size(css%dyn_var)
                   write(*,'(A,A,I0,A)')myname,"     dyn_var(",jj,") = '"//&
                        & css%dyn_var(jj)(1:css%dyn_lenv(jj))//"'"
                end do
             end if
             if (obs_bdeb)write(*,*)myname,' Dynamic var:',ii,&
                  & (" "//css%dyn_var(jj)(1:css%dyn_lenv(jj)),jj=1,size(css%dyn_var))
             call parse_parsef(css%trg_psp(ii)%ptr,&
                  & css%trg_pos250(ii)(1:css%trg_lenp(ii)),&
                  & css%dyn_var,crc250,irc)
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Error return from parsef.")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
          end select
       end do
    end if
    if(obs_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine observation_compile
  !
  ! check if descr match observation targets, and evaluate expression
  ! ...load next target into memory...
  !
  logical function observation_eval(css,bok,crc250,irc)
    type(obs_session), pointer :: css
    logical :: bok           ! is everything ok?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="observation_eval"
    integer :: ii,dyn_pos,ipos
    logical :: bbok
    if(obs_bdeb) write(*,*)myname,' Entering.',css%int_val
    if(obs_bdeb)write(*,*)myname,"ind_pe:",associated(css%ind_pe),css%sid
    !if(obs_bdeb) write(*,*)myname,'Position.',ntarget,css%dyn_pos,css%dyn_cnt,css%dyn_max
    !
    ! get next position
    !
    bbok=(css%dyn_pos.le.ktdexl)
    observation_eval=bbok
    if (.not.bbok) return
    bok=.true.
    if (css%ntarget== 0) then ! no targets to evaluate
       return
    else
       if (css%dyn_cnt.eq.0.or.css%dyn_cnt.ge.css%dyn_max) then ! read next observation
          if (obs_bdeb)write(*,*)myname,' Assigning:',css%ntarget,ktdexl
          css%dyn_cnt=1
          do ii=1,css%ntarget
             select case (css%trg_type(ii))
             case (parse_delay) ! delayed processing
             case (parse_empty)
                if (observation_getPos(css,css%trg_descr(ii))) then ! find next descr
                   css%trg_seq(ii)=css%dyn_pos
                   ipos=css%trg_seq(ii)+(isubset-1)*KEL
                   css%trg_val(ii)=values(ipos)
                   css%trg_vok(ii)=(values(ipos).ne.rvind)
                   if (.not.css%trg_vok(ii).and.css%trg_req(ii)) then
                      write(css%currentfile%hint80(2),'(A," (",I0,"), value=undefined")')&
                           & css%trg80(ii)(1:css%trg_lent(ii)),ii,dyn_pos
                      bok=.false. ! missing target value
                   end if
                   if (obs_bdeb)write(*,*)myname,' Found:',ii,css%trg_seq(ii),css%trg_descr(ii)
                else
                   bbok=.false.
                end if
             case (parse_constant)
                ipos=css%trg_seq(ii)+(isubset-1)*KEL
                css%trg_val(ii)=values(ipos)
                css%trg_vok(ii)=(values(ipos).ne.rvind)
                if (.not.css%trg_vok(ii).and.css%trg_req(ii)) then
                   write(css%currentfile%hint80(2),'(A," (",I0,"), value undefined at:",I0)')&
                        & css%trg80(ii)(1:css%trg_lent(ii)),ii,css%trg_seq(ii)
                   bok=.false. ! missing target value
                end if
             case (parse_internal)
                if (obs_bdeb)write(*,*)myname,' Internals:',css%int_val
                css%trg_val(ii)=parse_evalf(css%trg_psp(ii)%ptr,css%int_val,crc250,irc)
                if (irc.ne.0) then
                   call observation_errorappend(crc250,myname)
                   call observation_errorappend(crc250," Error return from evalf.")
                   call observation_errorappendi(crc250,irc)
                   call observation_errorappend(crc250,"\n")
                   return
                end if
                css%trg_vok(ii)=.true.
                if (obs_bdeb)write(*,*)myname,' Internal:',ii,css%trg_val(ii)
             case (parse_variable)
                if (observation_getPos(css,css%trg_descr(ii))) then ! find next descr
                   css%trg_seq(ii)=css%dyn_pos
                   ipos=css%trg_seq(ii)+(isubset-1)*KEL
                   css%trg_val(ii)=values(ipos)
                   css%trg_vok(ii)=(values(ipos).ne.rvind)
                   css%dyn_val(css%trg_ind(ii))=css%dyn_pos
                   if (.not.css%trg_vok(ii).and.css%trg_req(ii)) then
                      write(css%currentfile%hint80(2),'(A," (",I0,"), value undefined at:",I0)')&
                           & css%trg80(ii)(1:css%trg_lent(ii)),ii,css%trg_seq(ii)
                      bok=.false. ! missing target value
                   end if
                   if (obs_bdeb)write(*,*)myname,' Found:',css%trg_seq(ii),css%trg_descr(ii)
                else
                   bbok=.false.
                end if
             case (parse_expression)
                dyn_pos=nint(parse_evalf(css%trg_psp(ii)%ptr,css%dyn_val,crc250,irc))
                if (irc.ne.0) then
                   call observation_errorappend(crc250,myname)
                   call observation_errorappend(crc250," Error return from evalf.")
                   call observation_errorappendi(crc250,irc)
                   call observation_errorappend(crc250,"\n")
                   return
                end if
                if (obs_bdeb)write(*,*)myname,' Dynamic pos:',ii,dyn_pos,nint(css%dyn_val)
                if (dyn_pos.ge.1.and.dyn_pos.le.ktdexl) then ! out of bounds...
                   css%trg_seq(ii)=dyn_pos
                   if (css%trg_descr(ii).ne.ktdexp(dyn_pos)) then
                      write(css%currentfile%hint80(2),&
                           & '(A," (",I0,"), DESCR(",I0,")=",I0," expected ",I0)')&
                           & css%trg80(ii)(1:css%trg_lent(ii)),ii,dyn_pos,&
                           & ktdexp(dyn_pos),css%trg_descr(ii)
                      bok=.false.
                   else
                      ipos=css%trg_seq(ii)+(isubset-1)*KEL
                      css%trg_val(ii)=values(ipos)
                      css%trg_vok(ii)=(values(ipos).ne.rvind)
                      if (.not.css%trg_vok(ii).and.css%trg_req(ii)) then
                         write(css%currentfile%hint80(2),'(A," (",I0,"), value undefined at:",I0)')&
                              & css%trg80(ii)(1:css%trg_lent(ii)),ii,css%trg_seq(ii)

                         bok=.false. ! missing target value
                      end if
                   end if
                else
                   write(css%currentfile%hint80(2),'(A," (",I0,"), out of bounds at:",I0)')&
                        & css%trg80(ii)(1:css%trg_lent(ii)),ii,dyn_pos
                   bok=.false. ! reject observation...
                end if
             end select
             if (obs_bdeb) then
                if (css%trg_val(ii).gt.1.0D10) then
                   write(*,*)myname,' zzzzzzzzzzzzzz Invalid value:',&
                        & ii,css%trg_vok(ii),css%trg_val(ii),(css%trg_val(ii).eq.rvind),bok
                end if
             end if
          end do
          if (bok.and.css%ind_eset) then
             if(obs_bdeb)write(*,*)myname,"ind_pe:",associated(css%ind_pe),css%sid
             if(obs_bdeb)write(*,*)myname,"Calling parse_evals.",&
                  & associated(css%ind_pe),allocated(css%trg_val),css%dyn_pos
             call parse_evals(css%ind_pe,css%trg_val,css%trg_vok,&
                  & css%trg_val(css%ntrg),css%trg_vok(css%ntrg),crc250,irc)
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Error return from evals.")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
          end if
          if(obs_bdeb)write(*,*)myname,"Here ind_pe:",associated(css%ind_pe),css%sid
          if (bok.and.css%ind_tset) then
             call parse_evals(css%ind_pt,css%trg_val,css%trg_vok,&
                  & css%trg_val(css%ntrg),css%trg_vok(css%ntrg),crc250,irc)
             if (irc.ne.0) then
                call observation_errorappend(crc250,myname)
                call observation_errorappend(crc250," Error return from evals.")
                call observation_errorappendi(crc250,irc)
                call observation_errorappend(crc250,"\n")
                return
             end if
          end if
          if (bok) then
             css%ind_val=css%trg_val(css%ntrg)
          end if
          ! extract duplicate indexes...
          call observation_pullTargets(css,crc250,irc)
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Error return from pullTargets.")
             call observation_errorappend(crc250,"\n")
             return
          end if
          if (css%dyn_pos.eq.0) then ! no search, i.e. 1 iteration only
             css%dyn_pos=ktdexl+1 ! force stop next time...
          end if
          css%dyn_bok=bok
       else ! make duplicate of existing obs
          css%dyn_cnt=css%dyn_cnt+1
          ! extract duplicate indexes...
          call observation_pullTargets(css,crc250,irc)
          if (irc.ne.0) then
             call observation_errorappend(crc250,myname)
             call observation_errorappend(crc250," Error return from pullTargets.")
             call observation_errorappend(crc250,"\n")
             return
          end if
          bok=css%dyn_bok
       end if
    end if
    observation_eval=bbok
    if(obs_bdeb)write(*,*)myname,"Done ind_pe:",associated(css%ind_pe),css%sid
    return
  end function observation_eval
  !
  ! calculate the duplicate target locations from count
  !
  subroutine observation_pullTargets(css,crc250,irc)
    type(obs_session), pointer :: css
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="observation_pulltargets"
    integer :: ii,jj,kk
    jj=css%dyn_cnt
    do ii=css%ndup,1,-1
       css%trg_val(css%dup_ind(ii))=int(jj/css%dup_inc(ii-1)) + &
            & nint(css%trg_minval(css%dup_ind(ii)))-1
       if (obs_bdeb) write(*,'(X,A,X,A,I0,A,I0,3X,I0)')myname,' Target(',css%dup_ind(ii),') = ',&
            & nint(css%trg_val(css%dup_ind(ii))),ii
       jj=mod(jj,css%dup_inc(ii-1))
    end do
    return
  end subroutine observation_pullTargets
  !
  ! search for next descriptor
  !
  logical function observation_getPos(css,descr)
    type(obs_session), pointer :: css
    integer :: descr
    SEARCH : do
       css%dyn_pos=css%dyn_pos+1
       if (css%dyn_pos.gt.ktdexl) then
          observation_getPos=.false.
          exit SEARCH
       else if (descr.eq.KTDEXP(css%dyn_pos)) then
          observation_getPos=.true.
          exit SEARCH
       end if
    end do SEARCH
    return
  end function observation_getPos
  !
  ! check if message descriptors match target descriptors
  !
  subroutine observation_checkDescr(css,bok,crc250,irc)
    type(obs_session), pointer :: css
    logical :: bok           ! is everything ok?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="observation_checkDescr"
    integer :: ii,jj
    logical bok1,bok2
    if(obs_bdeb) write(*,*)myname,'Entering.',css%ntarget,css%category,ksec1(6),css%subcategory,ksec1(7)
    bok=.true.
    if (css%category .eq. ksec1(6) .and.css%subcategory .eq. ksec1(7)) then
       !if(obs_bdeb) write(*,*)myname,'Here.'
       css%currentfile%mok(5)=css%currentfile%mok(5)+1 ! other BUFRtype/subtype
       if (css%ntarget== 0) then ! no targets to evaluate
          return
       else
          bok1=.true.
          bok2=.true.
          do ii=1,css%ntarget
             ! if(obs_bdeb) write(*,*)myname,'There.',ii
             select case (css%trg_type(ii))
             case (parse_empty)
             case (parse_constant)
                if (bok) then
                   if (css%trg_seq(ii).lt.1.or.css%trg_seq(ii).gt.ktdexl) then
                      if(obs_bdeb)write(*,*)myname,'Failed limit:',ii,ktdexl,css%trg_seq(ii)
                      bok1=.false.
                      bok=.false.
                   end if
                end if
                if (bok) then
                   if (ktdexp(css%trg_seq(ii)).ne.css%trg_descr(ii)) then
                      if(obs_bdeb)then
                         do jj=1,css%ntarget
                            if (ktdexp(css%trg_seq(jj)).ne.css%trg_descr(jj)) then
                               write(*,'(X,I3,X,A20,I8,A,I0,A)') &
                                    & jj,css%trg80(jj)(1:css%trg_lent(jj))//'=',css%trg_descr(jj),&
                                    & ' (',ktdexp(css%trg_seq(jj)),') !!'
                            else 
                               write(*,'(X,I3,X,A20,I8,A,I0,A)') &
                                    & jj,css%trg80(jj)(1:css%trg_lent(jj))//'=',css%trg_descr(jj),&
                                    & ' (',ktdexp(css%trg_seq(jj)),')'
                            end if
                         end do
                         write(*,'(X,A,3(2X,A,I0))')&
                              & myname,'Failed sanity:',ii,&
                              & css%trg80(ii)(1:css%trg_lent(ii))//'=',css%trg_descr(ii),&
                              & ' found=',ktdexp(css%trg_seq(ii))
                      end if
                      
                      bok2=.false.
                      css%currentfile%hint80(7)=css%trg80(ii)(1:25)
                      bok=.false.
                      ! ! bah... abort anyway...
                      ! irc=945
                      ! call observation_errorappend(crc250,myname)
                      ! call observation_errorappendi(crc250,ii)
                      ! call observation_errorappend(crc250,"Failed sanity check for target "// &
                      !      & css%trg80(ii)(1:css%trg_lent(ii))//",")
                      ! call observation_errorappendi(crc250,css%trg_descr(ii))
                      ! call observation_errorappend(crc250,"!=")
                      ! call observation_errorappendi(crc250,ktdexp(css%trg_seq(ii)))
                      ! RETURN
                   end if
                end if
             case (parse_internal)
             case (parse_variable)
             case (parse_expression)
             end select
          end do
          if (bok1) then
             css%currentfile%mok(6)=css%currentfile%mok(6)+1 ! limits are ok
             if (bok2) then
                css%currentfile%mok(7)=css%currentfile%mok(7)+1 ! descriptors match
             else
                !write(*,*)myname,'***FAILED SANITY:',ii,ktdexp(css%trg_seq(ii)),css%trg_descr(ii)
                css%currentfile%mrm(7)=css%currentfile%mrm(7)+1 ! descriptors do not match
             end if
          else
             css%currentfile%mrm(6)=css%currentfile%mrm(6)+1 ! limits not ok
          end if
       end if
    else
       css%currentfile%mrm(5)=css%currentfile%mrm(5)+1 ! other BUFRtype/subtype
       bok=.false.
    end if
    if(obs_bdeb) write(*,*)myname,'OK.',css%currentfile%mok
    if(obs_bdeb) write(*,*)myname,'RM.',css%currentfile%mrm
    if(obs_bdeb) write(*,*)myname,'Done.',bok
    return
  end subroutine observation_checkDescr
  !
  ! terminate evaluation and parsing session
  !
  subroutine observation_terminate(css,crc250,irc)
    type(obs_session), pointer :: css
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="observation_terminate"
    if (css%ind_eset) then
       if(obs_bdeb)write(*,*)myname,'Closing ind_pe:',associated(css%ind_pe),css%sid
       call parse_close(css%ind_pe,crc250,irc)
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," Error return from parse_close.")
          return
       end if
    end if
    return
  end subroutine observation_terminate
  !
  subroutine observation_filestartxml(css,ounit,crc250,irc)
    type(obs_session), pointer :: css !  current session
    integer :: ounit
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="observation_fileStartXml"
    if (associated(css%currentFile)) then
       write(ounit,'(2X,A)',iostat=irc)"<observationFile file='"//css%currentFile%fn250(1:css%currentFile%lenf)//"'>"
    else
       write(ounit,'(2X,A)',iostat=irc)"<observationFile>"
    end if
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250," unable to write to file ")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
       return
    end if
    return
  end subroutine observation_filestartxml
  !
  subroutine observation_writexml(css,ounit,locid,crc250,irc)
    type(obs_session), pointer :: css !  current session
    integer :: ounit
    integer :: locid
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="observation_writexml"
    character*250 :: buff250
    character*50 :: s1, s2, s3, s4
    integer :: ii,jj
    integer :: len1,len2,len3,len4,lenb=0
    integer, external :: length
    integer :: iktype,id,idd,seq,ipos
    logical :: bbok
    character*9 :: cident
    integer :: cnt
    logical :: bok
    real :: rlat1,rlat2,rlon1,rlon2,rlat,rlon
    if (isubset .le. nsubset.and.bread) then ! isubset .le. nsubset (always true)
       bread=.false. ! wait for next message...
       ! IF (observation_eval(css,bok,crc250,irc)) THEN
       !   ! valid obs-target in memory
       ! END IF
       ! IF(IRC.NE.0) THEN
       !    call observation_errorappend(crc250,myname)
       !    call observation_errorappend(crc250," Error return from eval.");
       !    call observation_errorappend(crc250,"\n")
       !    RETURN
       ! END IF
       write(s1,'(I0)') locid
       call chop0(s1,50)
       len1=length(s1,50,10)
       write(ounit,'(3X,A,I0,A)',iostat=irc)"<message id='"//s1(1:len1)//"' subset='",isubset,"'>"
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," unable to write to file ")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       !
       ! add target values
       !
       do ii=1,css%ntrg
          s1=css%trg80(ii)(1:50) ; call chop0(s1,50); len1=length(s1,50,10)   ! element identification
          IF (len1.ne.0) then
             s1=" name='"//s1(1:len1)//"'"
             len1=len1+8
          end if
          call observation_wash(css%trg_val(ii),s2,len2)
          if (len2.ne.0) then
             s2=" value='"//s2(1:len2)//"'"
             len2=len2+9
          end if
          if (css%trg_lval(1,ii)) then
             call observation_wash(css%trg_minval(ii),s3,len3)
             if (len3.ne.0) then
                s3=" min='"//s3(1:len3)//"'"
                len3=len3+7
             end if
          else
             len3=0
          end if
          if (css%trg_lval(2,ii)) then
             call observation_wash(css%trg_maxval(ii),s4,len4)
             if (len4.ne.0) then
                s4=" max='"//s4(1:len4)//"'"
                len4=len4+7
             end if
          else
             len4=0
          end if
          write(buff250,'(A)')"<target "//s1(1:len1)//s2(1:len2)//s3(1:len3)//s4(1:len4)//"/>"
          call wo(ounit,4,buff250)
       end do
       !
       ! write BUFR sequence
       !
       if (obs_bdeb) then
          !
          ! section 0
          write(buff250,'(A,I0,A)')"<sec0 pos='1' info='Length of section 0 (bytes)' val='",ksec0(1),"'/>"
          call wo(ounit,4,buff250)
          write(buff250,'(A,I0,A)')"<sec0 pos='2' info='Total length of Bufr message (bytes)' val='",ksec0(2),"'/>";
          call wo(ounit,4,buff250)
          write(buff250,'(A,I0,A)')"<sec0 pos='3' info='Bufr Edition number' val='",ksec0(3),"'/>"
          call wo(ounit,4,buff250)
          !
          ! section 1
          write(buff250,'(A,I0,A)')"<sec1 pos='1' info='Length of section 1 (bytes)' val='",ksec1(1),"'/>"
          call wo(ounit,4,buff250)
          write(buff250,'(A,I0,A)')"<sec1 pos='2' info='Bufr Edition number' val='",ksec1(2),"'/>"
          call wo(ounit,4,buff250)
          if(ksec1(2).ge.3) then
             write(buff250,'(A,I0,A)')"<sec1 pos='16' info='Originating sub-centre' val='",ksec1(16),"'/>"
             call wo(ounit,4,buff250)
          end if
          write(buff250,'(A,I0,A)')"<sec1 pos='3' info='Originating centre' val='",ksec1(3),"'/>"
          call wo(ounit,4,buff250)
          write(buff250,'(A,I0,A)')"<sec1 pos='4' info='Update sequence number' val='",ksec1(4),"'/>"
          call wo(ounit,4,buff250)
          write(buff250,'(A,I0,A)')"<sec1 pos='5' info='Flag (presence of section 2)' val='",ksec1(5),"'/>"
          call wo(ounit,4,buff250)
       end if
       call observation_getType(ksec1(6),ksec1(7),s1,s2,crc250,irc) 
       call chop0(s1,50); len1=length(s1,50,10)
       call chop0(s2,50); len2=length(s2,50,10)
       !
       if (len1.eq.0) then
          write(buff250,'(A,I0,A)')"<sec1 pos='6' info='Bufr message type' val='",ksec1(6),"'/>"
       else
          write(buff250,'(A,I0,A)')"<sec1 pos='6' info='Bufr message type' val='",ksec1(6),"' type='"//s1(1:len1)//"'/>"
       end if
       call wo(ounit,4,buff250)
       if (len2.eq.0) then
          write(buff250,'(A,I0,A)')"<sec1 pos='7' info='Bufr message subtype' val='",ksec1(7),"'/>"
       else
          write(buff250,'(A,I0,A)')"<sec1 pos='7' info='Bufr message subtype' val='",ksec1(7),"' type='"//s2(1:len2)//"'/>"
       end if
       call wo(ounit,4,buff250)
       if (obs_bdeb) then
          write(buff250,'(A,I0,A)')"<sec1 pos='8' info='Version number of local table' val='",ksec1(8),"'/>"
          call wo(ounit,4,buff250)
          write(buff250,'(A,I0,A)')"<sec1 pos='9' info='Year' val='",ksec1(9),"'/>"
          call wo(ounit,4,buff250)
          write(buff250,'(A,I0,A)')"<sec1 pos='10' info='Month' val='",ksec1(10),"'/>"
          call wo(ounit,4,buff250)
          write(buff250,'(A,I0,A)')"<sec1 pos='11' info='Day' val='",ksec1(11),"'/>"
          call wo(ounit,4,buff250)
          write(buff250,'(A,I0,A)')"<sec1 pos='12' info='Hour' val='",ksec1(12),"'/>"
          call wo(ounit,4,buff250)
          write(buff250,'(A,I0,A)')"<sec1 pos='13' info='Minute' val='",ksec1(13),"'/>"
          call wo(ounit,4,buff250)
          write(buff250,'(A,I0,A)')"<sec1 pos='15' info='Version number of Master table' val='",ksec1(15),"'/>"
          call wo(ounit,4,buff250)
          write(buff250,'(A,I0,A)')"<sec1 pos='14' info='Bufr Master table' val='",ksec1(14),"'/>"
          call wo(ounit,4,buff250)
          !
          ! section 2
          IF(KSUP(2).LE.1) THEN
             write(buff250,'(A)')"<sec2 info='RDB key not defined in section 2'/>"
             call wo(ounit,4,buff250)
          else
             write(buff250,'(A,I0,A)')"<sec3 pos='1' info='Length of section 2' val='",key(1),"'/>"
             call wo(ounit,4,buff250)
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
                   write(s1,*) rlat1; call chop0(s1,50); len1=length(s1,50,10)
                   write(s2,*) rlon1; call chop0(s2,50); len2=length(s2,50,10)
                   write(s3,*) rlat2; call chop0(s3,50); len3=length(s3,50,10)
                   write(s4,*) rlon2; call chop0(s4,50); len4=length(s4,50,10)
                   write(buff250,'(A,I0,A)')"<sec2 pos='2' info='RDB data type' val='",key(2),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='3' info=''RDB data subtype' val='",key(3),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='4' info='Year' val='",key(4),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='5' info='Month' val='",key(5),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='6' info='Day' val='",key(6),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='7' info='Hour' val='",key(7),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='8' info='Minute' val='",key(8),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='9' info='Second' val='",key(9),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A)')"<sec2 pos='10' info='Longitude 1' val='"//s2(1:len2)//"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A)')"<sec2 pos='11' info='Latitude  1' val='"//S1(1:len1)//"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A)')"<sec2 pos='12' info='Longitude 2' val='"//S4(1:len4)//"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A)')"<sec2 pos='13' info='Latitude  2' val='"//S3(1:len3)//"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='14' info='Number of observations' val='",key(14),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='15' info='Identifier' val='",key(15),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='25' info='Total Bufr message length' val='",key(25),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='26' info='Day    (RDB insertion' val='",key(26),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='27' info='Hour   (RDB insertion' val='",key(27),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='28' info='Minute( (RDB insertion' val='",key(28),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='29' info='Second (RDB insertion' val='",key(29),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='30' info='Day    (MDB arrival' val='",key(30),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='31' info='Hour   (MDB arrival' val='",key(31),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='32' info='Minute (MDB arrival' val='",key(32),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='33' info='Second (MDB arrival' val='",key(33),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='34' info='Correction number' val='",key(34),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='35' info='Part of message' val='",key(35),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='37' info='Correction number' val='",key(37),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='38' info='Part of message' val='",key(38),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='40' info='Correction number' val='",key(40),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='41' info='Part of message' val='",key(41),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='43' info='Correction number' val='",key(43),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='44' info='Part of message' val='",key(44),"'/>"
                   call wo(ounit,4,buff250)
                   write(buff250,'(A,I0,A)')"<sec2 pos='46' info='Quality control % conf' val='",key(46),"'/>"
                   call wo(ounit,4,buff250)
                ELSE
                   RLAT1=(KEY(11)-9000000)/100000.
                   RLON1=(KEY(10)-18000000)/100000.
                   write(s1,*) rlat1; call chop0(s1,50); len1=length(s1,50,10)
                   write(s2,*) rlon1; call chop0(s2,50); len2=length(s2,50,10)
                   IDD=0
                   CIDENT=' '
                   DO ID=16,24
                      IDD=IDD+1
                      CIDENT(IDD:IDD)=CHAR(KEY(ID))
                   end do
                   write(buff250,'(A,I0,A)')"<sec2 pos='2' info='RDB data type' val='", KEY(2),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='3' info='RDB data subtype' val='", KEY(3),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='4' info='Year' val='", KEY(4),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='5' info='Month' val='", KEY(5),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='6' info='Day' val='", KEY(6),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='7' info='Hour' val='", KEY(7),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='8' info='Minute' val='", KEY(8),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='9' info='Second' val='", KEY(9),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A)')"<sec2 pos='11' info='Latitude  1'"//s1(1:len1)//"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A)')"<sec2 pos='10' info='Longitude 1'"//s2(1:len2)//"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 info='Identifier' val='", CIDENT,"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='25' info='Total Bufr message length' val='", KEY(25),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='26' info='Day    (RDB insertion' val='", KEY(26),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='27' info='Hour   (RDB insertion' val='", KEY(27),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='28' info='Minute (RDB insertion' val='", KEY(28),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='29' info='Second (RDB insertion' val='", KEY(29),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='30' info='Day    (MDB arrival' val='", KEY(30),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='31' info='Hour   (MDB arrival' val='", KEY(31),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='32' info='Minute (MDB arrival' val='", KEY(32),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='33' info='Second (MDB arrival' val='", KEY(33),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='34' info='Correction number' val='", KEY(34),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='35' info='Part of message' val='", KEY(35),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='37' info='Correction number' val='", KEY(37),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='38' info='Part of message' val='", KEY(38),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='40' info='Correction number' val='", KEY(40),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='41' info='Part of message' val='", KEY(41),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='43' info='Correction number' val='", KEY(43),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='44' info='Part of message' val='", KEY(44),"'/>"
                   call wo(ounit,4,buff250);
                   write(buff250,'(A,I0,A)')"<sec2 pos='46' info='Quality control % conf'", KEY(46),"'/>"
                   call wo(ounit,4,buff250);
                END IF
             end if
          end if
          !
          ! section 3
          !
          write(buff250,'(A,I0,A)')"<sec3 pos='1' info='Length of section 3 (bytes)'", ksec3(1),"'/>"
          call wo(ounit,4,buff250);
          write(buff250,'(A,I0,A)')"<sec3 pos='2' info='Reserved'", ksec3(2),"'/>"
          call wo(ounit,4,buff250);
          write(buff250,'(A,I0,A)')"<sec3 pos='3' info='Number of data subsets'", ksec3(3),"'/>"
          call wo(ounit,4,buff250);
          write(buff250,'(A,I0,A)')"<sec3 pos='4' info='Flag (data type/data compression)'", ksec3(4),"'/>"
          call wo(ounit,4,buff250);
       end if
       !write(*,*)myname,'Report D:',KTDLEN
       if (obs_bdeb) then
          write(buff250,'(A,I0,A)')"<sec3 type='unexpanded' count='",KTDLEN,"'>"
          call wo(ounit,4,buff250);
          DO II=1,KTDLEN
             write(buff250,'(A,I0,A,I0,A)')"<unexpanded pos='",ii,"' descr='",KTDLST(II),"'/>"
             call wo(ounit,4,buff250);
          end do
          write(buff250,'(A)')"</sec3>"
          call wo(ounit,4,buff250);
       end if
       !write(*,*)myname,'Report D:',KTDEXL
       !
       if (.not.css%ignuni.or..not.css%ignden.or..not.css%ignval) then
          write(buff250,'(A,I0,A)')"<sec3 type='expanded' count='",KTDEXL,"'>"
          call wo(ounit,4,buff250);
          cnt=0
          EXPANDED: DO II=1,KTDEXL
             IPOS=II+(isubset-1)*KEL
             if ((values(ipos).eq.rvind.and..not.css%ignmis).or.values(ipos).ne.rvind) then
                write(buff250,'(A,I0,A)') "<expanded pos='",ii,"'"
                call chop0(buff250,250)
                lenb=length(buff250,250,20)
                if (.not. css%ignder) then
                   write(s1,'(A,I0,A)')"descr='",KTDEXP(II),"'";call chop0(s1,50); len1=length(s1,50,10) 
                   buff250=buff250(1:lenb)//" "//s1(1:len1)
                   lenb=lenb+1+len1
                end if
                if (.not.css%ignval) then
                   if (ii.gt.1.and.ktdexp(ii).eq.ktdexp(max(1,ii-1)).and.&
                        & values(ipos).eq.values(max(1,ipos-1))) then
                      ! do nothing
                   else if (values(ipos).eq.RVIND) then ! missing
                      s1="MISSING";call chop0(s1,50); len2=length(s1,50,10) 
                      buff250=buff250(1:lenb)//" value='"//s1(1:len2)//"'"
                      call chop0(buff250,250)
                      lenb=length(buff250,250,20)
                      if (.not.css%ignden) then
                         buff250=buff250(1:lenb)//" name='"//cnames(ii)//"'"
                         call chop0(buff250,250)
                         lenb=length(buff250,250,20)
                      end if
                   else
                      call observation_wash(values(ipos),s1,len1)
                      buff250=buff250(1:lenb)//" value='"//s1(1:len1)//"'"
                      call chop0(buff250,250)
                      lenb=length(buff250,250,20)
                      if (.not.css%ignden) then
                         buff250=buff250(1:lenb)//" name='"//cnames(ii)//"'"
                         call chop0(buff250,250)
                         lenb=length(buff250,250,20)
                      end if
                      if (.not.css%ignuni) then
                         buff250=buff250(1:lenb)//" unit='"//cunits(ii)//"'"
                         call chop0(buff250,250)
                         lenb=length(buff250,250,20)
                      end if
                      s1=cvals(ii);call chop0(s1,50);len1=length(s1,50,10)
                      if (len1.gt.0)then
                         buff250=buff250(1:lenb)//" code='"//s1(1:len1)//"'"
                         call chop0(buff250,250)
                         lenb=length(buff250,250,20)
                      end if
                      buff250=buff250(1:lenb)//"/>"
                      call wo(ounit,4,buff250);
                      cnt=cnt+1
                      !if (cnt.gt.5000) exit EXPANDED
                   end if
                end if
             end if
          end do EXPANDED
          write(buff250,'(A)')"</sec3>"
          call wo(ounit,4,buff250);
       end if
       write(ounit,'(3X,A)')"</message>"
    else if (isubset .gt. nsubset) then
       if(obs_bdeb)write(*,'(X,A,X,A,2(I0,A))')myname,' Invalid isubset ',&
            &isubset,' (',nsubset,')'
       irc=845
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"This should never happen...")
       call observation_errorappendi(crc250,irc)
       call observation_errorappend(crc250,"\n")
    end if
    return
  end subroutine observation_writexml
  !
  subroutine wo(ounit,ind,buff250)
    integer :: ounit
    integer :: ind
    character*250 :: buff250
    integer :: lenb
    integer, external :: length
    character*20 :: blank20="                    "
    call chop0(buff250,250)
    lenb=length(buff250,250,10)
    write(ounit,'(A)')blank20(1:max(0,min(20,ind)))//buff250(1:lenb)
    return
  end subroutine wo
  !
  subroutine observation_filestopxml(css,ounit,crc250,irc)
    type(obs_session), pointer :: css !  current session
    integer :: ounit
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="observation_fileStopXml"
    integer :: ii
    integer :: len1,len2,len3,len4
    integer, external :: length
    character*50 :: s1,s2,s3,s4
    type(obs_mainCategory), pointer :: currentCat !  current file
    type(obs_subCategory), pointer :: currentSub !  current file
    ! write summary
    if (associated(css%currentFile)) then
       write(ounit,'(3X,A)',iostat=irc)"<summary>"
       if (irc.ne.0) then
          call observation_errorappend(crc250,myname)
          call observation_errorappend(crc250," unable to write to file ")
          call observation_errorappendi(crc250,irc)
          call observation_errorappend(crc250,"\n")
          return
       end if
       if (css%currentFile%mok(1).eq.css%currentFile%mok(7)) then
          write(ounit,'(4X,A,I0,A,I0,A)')"<messages found='",css%currentFile%mok(1),&
               & "' accepted='",css%currentFile%mok(7),"'/>"
       else
          do ii=1,10
             call chop0(css%currentFile%hint80(ii),80)
             css%currentFile%lenh(ii)=length(css%currentFile%hint80(ii),80,1)
          end do
          write(ounit,'(4X,A,4(I0,A))')"<messages found='",css%currentFile%mok(1),&
               & "' accepted='",css%currentFile%mok(7),&
               & "' type='",css%category,"' subtype='",css%subcategory,"'>"
          if (css%currentFile%mrm(2).ne.0) write(ounit,'(5X,A,I0,A)')"<check removed='",&
               & css%currentFile%mrm(2),&
               & "' reason='unable to decode header.'/>"
          if (css%currentFile%mrm(3).ne.0) write(ounit,'(5X,A,I0,A)')"<check removed='",&
               & css%currentFile%mrm(3),&
               & "' reason='unable to decode body.'/>"
          if (css%currentFile%mrm(4).ne.0) write(ounit,'(5X,A,I0,A)')"<check removed='",&
               & css%currentFile%mrm(4),&
               & "' reason='unable to decode description.'/>"
          if (css%currentFile%mrm(5).ne.0) write(ounit,'(5X,A,I0,A)')"<check removed='",&
               & css%currentFile%mrm(5),&
               & "' reason='Other BUFR/sub-type.'/>"
          if (css%currentFile%mrm(6).ne.0) write(ounit,'(5X,A,I0,A)')"<check removed='",&
               & css%currentFile%mrm(6),&
               & "' reason='DESCR out of range.'/>"
          if (css%currentFile%lenh(7).eq.0) then     
             if (css%currentFile%mrm(7).ne.0) write(ounit,'(5X,A,I0,A)')"<check removed='",&
                  & css%currentFile%mrm(7),&
                  & "' reason='DESCR mismatch.'"
          else
             if (css%currentFile%mrm(7).ne.0) write(ounit,'(5X,A,I0,A)')"<check removed='",&
                  & css%currentFile%mrm(7),&
                  & "' reason='DESCR mismatch.' hint='"//&
                  & css%currentFile%hint80(7)(1:css%currentFile%lenh(7))//"'/>"
          end if
          ! make list of the messages found
          currentCat=>css%currentFile%firstCategory%next
          do while (.not.associated(currentCat,target=css%currentFile%lastCategory)) 
             call observation_getType(currentCat%category,0,s1,s2,crc250,irc) 
             call chop0(s1,50); len1=length(s1,50,10)
             call chop0(s2,50); len2=length(s2,50,10)
             if (len1.ne.0) then
                write(ounit,'(5X,A,3(I0,A))',iostat=irc) "<msg type='",currentCat%category,&
                     & "' cnt='",currentCat%cnt,"' nsub='",currentCat%nsub,"' info='"//s1(1:len1)//"'>"
             else
                write(ounit,'(5X,A,3(I0,A))',iostat=irc) "<msg type='",currentCat%category,&
                     & "' cnt='",currentCat%cnt,"' nsub='",currentCat%nsub,"'>"
             end if
             currentSub=> currentCat%firstSubCategory%next
             do while (.not.associated(currentSub,target=currentCat%lastSubCategory)) 
                call observation_getType(currentCat%category,currentSub%subcategory,s1,s2,crc250,irc) 
                call chop0(s1,50); len1=length(s1,50,10)
                call chop0(s2,50); len2=length(s2,50,10)
                if (len2.ne.0) then
                   write(ounit,'(7X,A,2(I0,A))',iostat=irc) "<sub type='",&
                        & currentSub%subcategory,"' cnt='",currentSub%cnt,"' info='"//s2(1:len2)//"'/>"
                else
                   write(ounit,'(7X,A,2(I0,A))',iostat=irc) "<sub type='",&
                        & currentSub%subcategory,"' cnt='",currentSub%cnt,"'/>"
                end if
                currentSub=>currentSub%next
             end do
             write(ounit,'(5X,A)',iostat=irc) "</msg>"
             currentCat=>currentCat%next
          end do
          !
          write(ounit,'(4X,A)')"</messages>"
       end if
       if (css%currentFile%ook(1).eq.css%currentFile%ook(5)) then
          write(ounit,'(4X,A,I0,A,I0,A)')"<locations found='",css%currentFile%ook(1),&
               & "' accepted='",css%currentFile%ook(5),"'/>"
       else
          do ii=1,10
             call chop0(css%currentFile%hint80(ii),80)
             css%currentFile%lenh(ii)=length(css%currentFile%hint80(ii),80,1)
          end do
          write(ounit,'(4X,A,I0,A,I0,A)')"<locations found='",css%currentFile%ook(1),&
               & "' accepted='",css%currentFile%ook(5),"'>"
          if (css%currentFile%orm(2).ne.0) write(ounit,'(5X,A,I0,A)')"<check removed='",&
               & css%currentFile%orm(2),&
               & "' reason='evaluation error.' hint='"//&
               & css%currentFile%hint80(2)(1:css%currentFile%lenh(2))//"'/>"
          if (css%currentFile%orm(3).ne.0) write(ounit,'(5X,A,I0,A)')"<check removed='",&
               & css%currentFile%orm(3),&
               & "' reason='outside index limits.'/>"
          if (css%currentFile%orm(4).ne.0) then
             write(ounit,'(5X,A,I0,A)')"<check removed='",css%currentFile%orm(4),&
                  & "' reason='outside target limits.'>"
             do ii=1,css%ntrg
                if (css%trg_orm(ii).ne.0) then
                   s1=css%trg80(ii)(1:50) ; call chop0(s1,50); &
                        & len1=length(s1,50,10)   ! element identification
                   IF (len1.ne.0) then
                      s1=" name='"//s1(1:len1)//"'"
                      len1=len1+8
                   end if
                   if (css%trg_lval(1,ii)) then
                      call observation_wash(css%trg_minval(ii),s3,len3)
                      if (len3.ne.0) then
                         s3=" min='"//s3(1:len3)//"'"
                         len3=len3+7
                      end if
                   else
                      len3=0
                   end if
                   if (css%trg_lval(2,ii)) then
                      call observation_wash(css%trg_maxval(ii),s4,len4)
                      if (len4.ne.0) then
                         s4=" max='"//s4(1:len4)//"'"
                         len4=len4+7
                      end if
                   else
                      len4=0
                   end if
                   write(ounit,'(6X,A,I0,A)')"<target removed='",css%trg_orm(ii),&
                        & "'"//s1(1:len1)//s3(1:len3)//s4(1:len4)//"/>"
                end if
             end do
             write(ounit,'(5X,A)')"</check>"
          end if
          if (css%currentFile%orm(5).ne.0) write(ounit,'(5X,A,I0,A)')"<check removed='",&
               & css%currentFile%orm(5),&
               & "' reason='rejected by obs filter.'/>"
          write(ounit,'(4X,A)')"</locations>"
       end if
       write(ounit,'(3X,A)')"</summary>"
    end if
    write(ounit,'(2X,A)')"</observationFile>"
    return
  end subroutine observation_filestopxml
  !
  ! convert real to pretty string
  !
  subroutine observation_wash(val,s2,len2)
    real :: val
    character*50 :: s2
    integer :: len2
    integer, external :: length
    integer :: jj
    write(s2,'(F0.10)') val; call chop0(s2,50); len2=length(s2,50,10) ! ignore last digit...
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
    return
  end subroutine observation_wash
  !
end module observations
