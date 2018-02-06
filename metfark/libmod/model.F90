!
!"*********************************************************************************************"
!"***This software is ONLY permitted used by staff at the Norwegian Meteorological Institute***"
!"*********************************************************************************************"
! 1) define files/make cache
! 2) define variable targets
!    ...and indexes etc.
! 3) define slice variables/clear locations
!    4) push locations
! 5) loop over files
!    6) get output array(nloc,ntrg).
!
module model
  use parse
  IMPLICIT NONE
# include "netcdf.inc"
  !
  ! Global constants
  !
  character*1 :: sep = "|"
  logical     :: mod_bdeb=.false.
  !
  ! target variables
  !
  type :: mod_target
     character*80 :: n80         ! name
     integer :: lenn             ! length of name
     character*80 :: v80         ! variable
     character*80 :: min80         ! lower limit
     character*80 :: max80         ! upper limit
     logical :: lslice = .false. ! is target a slice variable
     type(mod_target), pointer :: prev => null()   ! linked list
     type(mod_target), pointer :: next => null()   ! linked list
  end type mod_target
  !
  ! Location item
  !   
  type :: mod_location
     integer :: locid
     integer :: iloc
     integer :: csli=0                  ! number of slice variables
     real, allocatable :: sli_val(:)    ! value of slice variable
     integer :: ctrg=0                  ! number of target variables
     real, allocatable    :: trg_val(:) ! value of target variable
     logical, allocatable :: trg_vok(:) ! value of target variable
     logical, allocatable :: trg_set(:)! is target variable set?
     integer :: cobs=0                  ! number of obs variables
     real, allocatable    :: obs_val(:) ! value of obs variable
     logical, allocatable :: obs_vok(:) ! is value set?
     integer :: ndim                    ! number of dimensions
     integer, allocatable :: pos(:)     ! current position (unexpanded)
     real, allocatable    :: rpos(:)    ! current position in real (unexpanded)
     integer, allocatable :: lstart(:)  ! start position (unexpanded)
     integer, allocatable :: lstop(:)   ! stop position
     real, allocatable    :: intpf(:)   ! interpolation factor
     integer :: search = 0              ! is grid search successful, 0=ok?
     logical :: bok = .true.
     type(mod_location), pointer :: prev => null()   ! linked list
     type(mod_location), pointer :: next => null()   ! linked list
  end type mod_location
  !
  type :: mod_locPointer
     type(mod_location), pointer :: ptr => null()
  end type mod_locPointer
  !
  ! a batch job contains variables with common dimensions
  type mod_batch
     integer :: ndim
     integer, allocatable      :: ind(:)         ! dimension index in file
     integer, allocatable      :: inc(:)         ! current search increment
     integer, allocatable      :: dim2slc(:)     ! slice index
     real, allocatable         :: trgdim(:)      ! slice target
     character*80, allocatable :: dim80(:)       ! dimension name
     integer :: nvar
     integer, allocatable      :: var(:)         ! variable index in file
     real, allocatable         :: val(:)         ! current value...
     logical, allocatable      :: proc(:)        ! processed?
     integer, allocatable      :: var2slc(:)     ! index to slice
     real, allocatable         :: trgvar(:)      ! slice target value
     character*80, allocatable :: var80(:)       ! variable name
     type(mod_batch), pointer  :: prev => null() ! linked list
     type(mod_batch), pointer  :: next => null() ! linked list
  end type mod_batch
  !
  type mod_plan
     type(mod_batch), pointer  :: first => null() ! linked list start
     type(mod_batch), pointer  :: last => null()  ! linked list end
     integer :: ndim
     logical, allocatable      :: innerDim(:)! is inner dimension
     integer :: nvar
     real, allocatable         :: trgvar(:)  !  variable target values
     integer, allocatable      :: slc2var(:) ! index from slice array to file var
     integer, allocatable      :: slc2dim(:) ! index from slice array to file dim
     logical, allocatable      :: proc(:)    ! is variable processed
  end type mod_plan
  !
  type mod_attribute
     character*80  :: att80
     integer :: lena
     integer :: type
     integer :: len
     character*1, allocatable ::  ac(:)
     integer*1,  allocatable::  a1(:)
     integer*2,  allocatable::  a2(:)
     integer*4,  allocatable::  a4(:)
     real*4,     allocatable::  ar(:)
     real*8,     allocatable::  ad(:)
  end type mod_attribute
  !
  type :: mod_attPointer
     type(mod_attribute), pointer :: ptr => null()
  end type mod_attPointer
  !
  type :: mod_variable
     character*80 :: var80
     integer :: lenv
     integer :: type
     ! dimensions
     integer :: ndim = 0                    ! number of variable dimensions
     integer(KIND=4), allocatable :: ind(:) ! points to file-dimensions
     integer(KIND=4), allocatable :: istart(:)
     integer(KIND=4), allocatable :: icount(:)
     integer :: natt=0
     integer :: itrg=0    ! index to target...
     type(mod_attPointer), pointer :: att(:) => null()
     real :: scale = 1.0D0
     ! missing value
     logical :: mmrange = .false. ! are limits set?
     logical :: mmset   = .false. ! were there any valid values?
     real :: minval = 0.0D0
     real :: maxval = 0.0D0
     integer :: misstype = 0 ! default
     character*1 ::mc
     integer*1::   m1
     integer*2::   m2
     integer*4::   m4
     real*4::      mr
     real*8::      md
     integer*8    :: len = 0
     character*1,allocatable::  fc(:)
     integer*1,  allocatable::  f1(:)
     integer*2,  allocatable::  f2(:)
     integer*4,  allocatable::  f4(:)
     real*4,     allocatable::  fr(:)
     real*8,     allocatable::  fd(:)
  end type mod_variable
  !
  type :: mod_varPointer
     type(mod_variable), pointer :: ptr => null()
  end type mod_varPointer
  !
  ! MODEL FILE STACK
  !
  type :: mod_file
     character*250                   :: fn250 = "" ! file name
     integer                         :: lenf
     integer                         :: ncid     ! netcdf id
     CHARACTER(len=10)               :: cfiletype = "netcdf"
     character*250 :: mod250=""                  ! model id
     ! index variable
     real :: trg                                 ! target value of index variable
     integer :: tsort                            ! total number of sort values 
     real :: ind_start                           ! sort starting value (index variable)
     real :: ind_stop                            ! sort stop value (index variable)
     logical :: ind_lim = .false.                ! are ind_start/ind_stop available?
     ! sorting the index variable
     integer :: nsort=0                          ! number of sort values
     real, allocatable :: sort(:)                ! values array
     integer,allocatable :: indsort(:)           ! index of sorted values
     character*250, allocatable :: desc250(:)    ! description of position
     type(mod_file), pointer :: prev => null()   ! linked list
     type(mod_file), pointer :: next => null()   ! linked list
     ! dimensions (names, length)
     integer :: ndim = 0                         ! number of dimensions
     character*80, allocatable :: dim80(:)       ! dimension names
     integer, allocatable :: lend(:)             ! dimension name length
     integer, allocatable :: dim_trg(:)          ! index to target (0=no target)
     integer, allocatable :: istart(:)           ! dimension start
     integer, allocatable :: istop(:)            ! dimension stop
     character*80, allocatable :: dim_var(:)     ! dimension name
     real, allocatable :: dim_val(:)             ! dimension values
     ! variables
     integer :: nvar                             ! number of variables
     character*80, allocatable :: var80(:)       ! variable name
     integer, allocatable :: lenv(:)             ! variables name length
     type(mod_varPointer), pointer :: var(:)  => null() ! variable pointer
     integer :: ngatt          ! number of global attributes
     integer :: unlimdimid     ! index to the dimension that is unlimited...
     integer :: ook(10),orm(10)
  end type mod_file
  !
  type :: mod_filePointer
     type(mod_file), pointer :: ptr => null()
  end type mod_filePointer
  !
  ! SESSION VARIABLES
  !
  type :: mod_session
     integer                         :: sid
     CHARACTER(LEN=250)              :: fn250
     CHARACTER(len=10)               :: cfiletype = "netcdf"
     !
     type(mod_file), pointer :: firstFile => null()   ! linked list start
     type(mod_file), pointer :: lastFile => null()    ! linked list end
     type(mod_file), pointer :: currentFile => null()
     type(mod_file), pointer :: nextFile => null()
     type(mod_filePointer), pointer   :: fileStack(:) => null() ! array of the stack elements
     real, allocatable            :: fileStackSort(:,:)
     integer, allocatable         :: fileStackInd(:,:)
     integer :: nFileIndexes = 0              ! total number of files on the stack
     integer :: nFileSortIndexes = 0          ! number of file indexes on the stack
     integer :: newnFileSortIndexes(2)        ! new number of file indexes on the stack
     integer :: currentFileSortIndex = 0      ! current stack index element
     integer :: currentFileIndex = 0          ! current stack element
     logical :: stackReady =.false.           ! are sorted data ready for use?
     integer :: leftFileSortIndex = 0         ! ref fileStackSort(*,2) - maxvalues
     integer :: rightFileSortIndex = 0        ! ref fileStackSort(*,1) - minvalues
     logical :: sortLimitsOk  = .false.       ! is there overlap between current file and min/max limits
     character*80, allocatable :: sys_var(:)
     real, allocatable         :: sys_val(:)
     integer,dimension(8)      :: values    
     !
     integer :: tsort = 0              ! total number of "index values" on the stack
     integer :: msort                  ! maximum number of "index variables"
     !
     ! index variable
     character(LEN=80)         :: ind_trg   ! index name
     integer :: ind_lent=0                  ! length of sorting variable
     character(LEN=80)         :: ind_var   ! index sorting variable
     integer :: ind_lenv=0                  ! length of sorting variable
     logical :: ind_set = .false.
     real    :: ind_minval=0.0D0
     real    :: ind_maxval=0.0D0
     logical :: ind_lval(2) = .false.
     !
     ! locations
     type(mod_location), pointer :: firstLoc => null()   ! linked list start
     type(mod_location), pointer :: lastLoc => null()    ! linked list end
     integer :: nloc=0                                   ! number of items in location-chain
     integer :: locoffset = 0                            ! offset between locid and position in locdata
     type(mod_locPointer), allocatable :: locData(:)     !  data locations
     logical :: locReady = .false.
     !
     ! slice variables
     integer :: csli = 0                      ! number of slice variables allocated
     character*80, allocatable :: sli_v80(:)  ! slice variable
     integer, allocatable      :: sli_lenv(:) ! length of slice variable
     integer, allocatable      :: sli_2trg(:) ! index from slice to target
     !
     ! targets
     type(mod_target), pointer :: firstTrg => null()   ! linked list start of target-chain
     type(mod_target), pointer :: lastTrg => null()    ! linked list end of target-chain
     type(mod_target), pointer :: currentTrg => null() ! current target loop
     integer :: ntrg=0                                 ! number of items in target-chain
     integer :: ctrg = 0                               ! number of targets allocated in array
     character*80, pointer :: trg80(:) => null()       ! list of target names
     integer, pointer      :: trg_lent(:) => null()    ! list of target name length
     character*80, pointer :: trg_v80(:) => null()     ! list of variable names
     integer, pointer      :: trg_lenv(:) => null()    ! list of target name length
     integer, pointer      :: trg_dim(:) => null()     ! index to dimension
     integer, pointer      :: trg_var(:) => null()     ! index to variable
     character*80, pointer :: trg_min80(:) => null()     ! list of lower limits
     character*80, pointer :: trg_max80(:) => null()     ! list of upper limits
     real, pointer         :: trg_minval(:) => null()  ! list of lower values
     real, pointer         :: trg_maxval(:) => null()  ! list of upper values
     logical, pointer      :: trg_sliceset(:) => null()! is target a slice variable?
     logical, pointer      :: trg_valset(:) => null()  ! is target value set by match?
     logical, pointer      :: trg_minset(:) => null()  ! list of is lower set?
     logical, pointer      :: trg_maxset(:) => null()  ! list of is upper set?
     logical, pointer      :: trg_req(:) => null()     ! is variable required?
     logical, pointer      :: trg_vok(:) => null()     ! is variable required?
     real, pointer         :: trg_val(:) => null()     ! list of values
     integer, pointer      :: trg_ook(:) => null()
     integer, pointer      :: trg_orm(:) => null()
     logical :: trg_set=.false.                     ! is target list set?
     !
     ! observation targets...
     integer :: cobs = 0                            ! number of observation values
     character*80, pointer :: obs_var(:)  => null() ! list of obs target names
     integer, pointer      :: obs_lenv(:) => null() ! list of obs target name length
     logical, pointer      :: obs_req(:)  => null() ! list of required variables
     logical, pointer      :: obs_vok(:)  => null() ! list of observation values
     real, pointer         :: obs_val(:)  => null() ! list of observation values
     !
     integer :: cpsp=0
     type(parse_pointer), pointer :: psp(:)=>null() ! parse pointer
     !
     CHARACTER*250 :: FLT250
     integer :: lenf=0
     logical ::  mpo_set=.false.
     integer :: cmpo=0
     character*80, allocatable :: mpo_var(:)   ! list of variable names
     integer, allocatable      :: mpo_lenv(:)  ! list of target name length
     logical, allocatable      :: mpo_req(:)   ! list of required variables
     logical, allocatable      :: mpo_vok(:)   ! list of valid varlues
     real, allocatable         :: mpo_val(:)   ! list of values
     type(parse_session), pointer :: psf => null()
     !
     ! output
     integer :: otrg=0                        ! number of allocated output target
     integer :: oloc=0                        ! number of allocated output locations
     real, pointer :: oval(:,:) => null()     ! output values
     logical, pointer :: oset(:,:) => null()  ! are output values set?
     !
     integer :: fid = 0 ! file id (index number)
     !
     type(mod_session), pointer :: prev => null()      ! linked list
     type(mod_session), pointer :: next => null()      ! linked list
  end type mod_session
  !
  integer :: maxid=0 ! session counter
  type(mod_session), pointer :: firstSession => null() ! linked list start
  type(mod_session), pointer :: lastSession => null()  ! linked list end
  !
CONTAINS
  !
  !
  !###############################################################################
  ! SESSION ROUTINES
  !###############################################################################
  !
  subroutine model_opensession(sid,css,crc250,irc)
    integer :: sid
    type(mod_session),pointer :: css  !  new session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_openSession"
    if (.not.associated(firstSession)) then
       allocate(firstSession, lastSession,stat=irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Unable to allocate 'firstSession/lastSession'.")
          call model_errorappend(crc250,"\n")
          return
       end if
       firstSession%next => lastSession
       lastSession%prev => firstSession
    end if
    nullify(css)
    allocate(css,stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate 'new session'.")
       call model_errorappend(crc250,"\n")
       return
    end if
    !
    call date_and_time(VALUES=css%values) ! get current date
    if (allocated(css%sys_var)) deallocate (css%sys_var)
    if (allocated(css%sys_val)) deallocate (css%sys_val)
    allocate(css%sys_var(2),css%sys_val(2),stat=irc)
    css%sys_var(1)="now"
    css%sys_var(2)="midnight"
    css%sys_val(1)=parse_f1970(&
         & real(css%values(1)),real(css%values(2)),&
         & real(css%values(3)),real(css%values(5)),&
         & real(css%values(6)),real(css%values(7)))
    css%sys_val(2)=parse_f1970(&
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
    css%nFileIndexes=0
    css%tsort=0
    !
    allocate(css%firstFile,css%lastFile, stat=irc) ! 
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate &
            & 'css%firstFile/css%lastFile'.")
       call model_errorappend(crc250,"\n")
       return
    end if
    css%firstFile%next => css%lastFile
    css%lastFile%prev => css%firstFile
    !
    allocate(css%firstLoc,css%lastLoc, stat=irc) ! 
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate &
            & 'css%firstLoc/css%lastLoc'.")
       call model_errorappend(crc250,"\n")
       return
    end if
    css%firstLoc%next => css%lastLoc
    css%lastLoc%prev => css%firstLoc
    css%locReady=.false.
    !
    allocate(css%firstTrg,css%lastTrg, stat=irc) ! 
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate &
            & 'css%firstTrg/css%lastTrg'.")
       call model_errorappend(crc250,"\n")
       return
    end if
    css%firstTrg%next => css%lastTrg
    css%lastTrg%prev => css%firstTrg
    !
    ! mark as prepared
    css%stackReady=.false.
    return
  end subroutine model_opensession

  subroutine model_getSession(css,sid,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: sid
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_getSession"
    if (.not.associated(firstSession)) then
       irc=911
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"No session is opened!")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    css => firstSession%next
    do while ( .not.associated(css,target=lastSession))
       if (css%sid .eq. sid) then
          !if (mod_bdeb) write(*,*)myname,'Exiting with sid:',sid,irc
          return
       end if
       css=>css%next
    end do
    nullify(css)
    irc=342
    call model_errorappend(crc250,myname)
    call model_errorappend(crc250,"Invalid session id:")
    call model_errorappendi(crc250,sid)
    call model_errorappend(crc250,"\n")
    return
  end subroutine model_getSession

  subroutine model_closeSession(css,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_closeSession"
    if(mod_bdeb)write(*,*)myname,'Entering.',irc
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       call model_removeSession(css,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from removeSession.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    else
       irc=599
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Attempt to close none-existent session.")
       call model_errorappend(crc250,"\n")
       return
    end if
    if(mod_bdeb)write(*,*)myname,' Done.',irc
    return
  end subroutine model_closeSession

  subroutine model_removeSession(css,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(mod_file), pointer :: cfile, cfilen
    type(mod_location), pointer :: cloc, clocn
    type(mod_target), pointer :: ctrg, ctrgn
    integer :: ii
    character*25 :: myname="model_removeSession"
    ! remove location stack
    if(mod_bdeb)write(*,*)myname,'Entering.',irc
    if(mod_bdeb)write(*,*)myname,'Un-slice.'
    !
    if (allocated(css%sys_var)) deallocate (css%sys_var)
    if (allocated(css%sys_val)) deallocate (css%sys_val)
    !
    ! remove global slice arrays
    if (allocated(css%sli_v80)) deallocate(css%sli_v80)
    if (allocated(css%sli_lenv)) deallocate(css%sli_lenv)
    if (allocated(css%sli_2trg)) deallocate(css%sli_2trg)
    !
    ! remove file-stack
    if (associated(css%filestack)) then
       do ii=1,size(css%filestack)
          if (associated(css%filestack(ii)%ptr)) then
             nullify(css%filestack(ii)%ptr)
          end if
       end do
       deallocate(css%filestack)
    end if
    if(mod_bdeb)write(*,*)myname,'Un-stack.'
    if (allocated(css%filestacksort)) deallocate(css%filestacksort)
    if (allocated(css%filestackind)) deallocate(css%filestackind)
    if (associated(css%firstFile)) then
       cfile => css%firstFile%next
       do while (.not.associated(cfile,target=css%lastFile))
          cfilen => cfile%next
          call model_deleteFile(css,cfile,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from deleteFile.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          cfile  => cfilen
       end do
       deallocate(css%firstFile,css%lastFile)
    end if
    !
    if(mod_bdeb)write(*,*)myname,'Un-Loc.'
    ! remove location arrays
    call model_clearLocList(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from clearLoc.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    ! remove location stack
    css%locReady=.false.
    if (associated(css%firstLoc)) then
       cloc => css%firstLoc%next
       do while (.not.associated(cloc,target=css%lastLoc))
          clocn => cloc%next
          call model_deleteLoc(css,cloc,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from deleteLoc.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          cloc  => clocn
       end do
       deallocate(css%firstLoc,css%lastLoc)
    end if
    !
    if(mod_bdeb)write(*,*)myname,'Un-Trg.'
    ! remove target stack
    if (associated(css%trg80)) deallocate(css%trg80)
    if (associated(css%trg_lent)) deallocate(css%trg_lent)
    if (associated(css%trg_v80)) deallocate(css%trg_v80)
    if (associated(css%trg_lenv)) deallocate(css%trg_lenv)
    if (associated(css%trg_dim)) deallocate(css%trg_dim)
    if (associated(css%trg_var)) deallocate(css%trg_var)
    if (associated(css%trg_min80)) deallocate(css%trg_min80)
    if (associated(css%trg_max80)) deallocate(css%trg_max80)
    if (associated(css%trg_minval)) deallocate(css%trg_minval)
    if (associated(css%trg_maxval)) deallocate(css%trg_maxval)
    if (associated(css%trg_sliceset)) deallocate(css%trg_sliceset)
    if (associated(css%trg_valset)) deallocate(css%trg_valset)
    if (associated(css%trg_minset)) deallocate(css%trg_minset)
    if (associated(css%trg_maxset)) deallocate(css%trg_maxset)
    if (associated(css%trg_req)) deallocate(css%trg_req)
    if (associated(css%trg_vok)) deallocate(css%trg_vok)
    if (associated(css%trg_val)) deallocate(css%trg_val)
    if (associated(css%trg_ook)) deallocate(css%trg_ook)
    if (associated(css%trg_orm)) deallocate(css%trg_orm)
    css%trg_set=.false.
    if (associated(css%firstTrg)) then
       ctrg => css%firstTrg%next
       do while (.not.associated(ctrg,target=css%lastTrg))
          ctrgn => ctrg%next
          call model_deleteTarget(css,ctrg,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from deleteTrg.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          ctrg  => ctrgn
       end do
       deallocate(css%firstTrg,css%lastTrg)
    end if
    if(mod_bdeb)write(*,*)myname,'Un-Out.'
    !
    ! remove output variables
    if (associated(css%oval)) deallocate(css%oval)
    if (associated(css%oset)) deallocate(css%oset)
    !
    if (associated(css%obs_var)) deallocate(css%obs_var)
    if (associated(css%obs_lenv)) deallocate(css%obs_lenv)
    if (associated(css%obs_req)) deallocate(css%obs_req)
    if (associated(css%obs_vok)) deallocate(css%obs_vok)
    if (associated(css%obs_val)) deallocate(css%obs_val)
    css%cobs=0
    !
    ! deallocate observation filter...
    if (css%lenf.ne.0) then
       call parse_close(css%psf,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Error return from 'parse_close'.")
          return
       end if
    end if
    !
    if (allocated(css%mpo_var)) deallocate(css%mpo_var)
    if (allocated(css%mpo_lenv)) deallocate(css%mpo_lenv)
    if (allocated(css%mpo_req)) deallocate(css%mpo_req)
    if (allocated(css%mpo_vok)) deallocate(css%mpo_vok)
    if (allocated(css%mpo_val)) deallocate(css%mpo_val)
    !
    call model_clearPSP(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,"clearPSP.")
       return
    end if
    !
    if(mod_bdeb)write(*,*)myname,'Un-link.'
    ! unlink from session-chain
    css%prev%next => css%next
    css%next%prev => css%prev
    deallocate(css)
    if(mod_bdeb)write(*,*)myname,' Done.',irc
    return
  end subroutine model_removeSession
  !
  !
  !###############################################################################
  ! STACK ROUTINES
  !###############################################################################
  ! initialise the stack
  !
  subroutine model_initfilestack(css,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_initfilestack"
    ! initialise chain
    allocate(css%firstFile,css%lastFile, stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate 'css%firstFile/css%lastFile'.")
       call model_errorappend(crc250,"\n")
       return
    end if
    css%firstFile%next => css%lastFile
    css%lastFile%prev => css%firstFile
    css%nFileIndexes=0
    css%tsort=0
    ! mark as prepared
    css%stackReady=.false.
  end subroutine model_initfilestack
  !
  ! clear the MODEL STACK
  !
  subroutine model_clearfilestack(css,var80,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*80 :: var80
    character*250 :: crc250
    integer :: irc
    type(mod_file), pointer :: currentFile => null()
    type(mod_file), pointer :: stackNext => null()
    integer, external :: length
    integer :: lens
    character*25 :: myname="model_clearfilestack"
    if(mod_bdeb)write(*,*)myname,' Entering.'
    if (.not.associated(css%firstFile)) then
       call model_initfilestack(css,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from rmItem.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    css%ind_var=var80
    call chop0(css%ind_var,80)
    lens=length(css%ind_var,80,10)
    currentFile => css%firstFile%next
    do while (.not.associated(currentFile,target=css%lastFile))
       stackNext => currentFile%next
       call model_deleteFile(css,currentFile,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from rmItem.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       currentFile => stackNext
    end do
    css%stackReady=.false.
    if (css%nFileIndexes .ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," System error:")
       call model_errorappendi(crc250,css%nFileIndexes)
       call model_errorappend(crc250,"\n")
       irc=940
       return
    end if
    if(mod_bdeb)write(*,*)myname,' Done.'
  end subroutine model_clearfilestack
  !
  ! remove item from model stack
  !
  subroutine model_deleteFile (css,df,crc250,irc)
    type(mod_session), pointer :: css !  current session
    type(mod_file), pointer :: df
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    character*25 :: myname="model_deleteFile"
    !if(mod_bdeb)write(*,*)myname,'Entering.'
    if (associated(df)) then
       css%nFileIndexes = css%nFileIndexes - 1
       css%tsort = css%tsort - df%nsort
       css%stackReady=.false.
       df%next%prev => df%prev
       df%prev%next => df%next
       css%currentfile => df
       call model_clearCurrentFile(css,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from clearCurrentFile.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    !if(mod_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine model_deleteFile
  !
  ! delete file contents
  !
  subroutine model_clearCurrentFile (css,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    character*25 :: myname="model_clearCurrentFile"
    type(mod_file), pointer :: df
    integer :: irc2
    integer :: ii
    !if(mod_bdeb)write(*,*)myname,'Entering.',associated(css%currentFile)
    df => css%currentFile
    if (associated(df)) then
       if (allocated(df%sort)) deallocate(df%sort,stat=irc2)
       if (allocated(df%indsort)) deallocate(df%indsort,stat=irc2)
       if (allocated(df%desc250)) deallocate(df%desc250,stat=irc2)
       if (allocated(df%dim80)) deallocate(df%dim80,stat=irc2)
       if (allocated(df%lend)) deallocate(df%lend,stat=irc2)
       if (allocated(df%dim_trg)) deallocate(df%dim_trg,stat=irc2)
       if (allocated(df%istart)) deallocate(df%istart,stat=irc2)
       if (allocated(df%istop)) deallocate(df%istop,stat=irc2)
       if (allocated(df%dim_var)) deallocate(df%dim_var,stat=irc2)
       if (allocated(df%dim_val)) deallocate(df%dim_val,stat=irc2)
       if (allocated(df%var80)) deallocate(df%var80,stat=irc2)
       if (allocated(df%lenv)) deallocate(df%lenv,stat=irc2)
       !if(mod_bdeb)write(*,*)myname,'Deleting vars:',df%nvar
       if (associated(df%var)) then
          do ii=1,df%nvar
             !if(mod_bdeb)write(*,*)myname,'Var:',ii !,associated(df%var(ii)%ptr)
             call model_deleteVariable(df%var(ii)%ptr,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from deleteVariable.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
             if (associated(df%var(ii)%ptr)) deallocate(df%var(ii)%ptr)
          end do
       end if
       if (associated(df%var)) deallocate(df%var,stat=irc2)
    end if
    !if(mod_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine model_clearCurrentFile
  !
  ! Add model-file (grib/netcdf) to the MODEL STACK
  !
  subroutine model_pushFile(css,path250,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(mod_file),pointer :: newFile
    logical  :: bok =.false.
    character(len=80), allocatable, dimension(:) :: dim80
    integer :: nvalues, tsize, ndims
    integer :: ii,jj,kk,tt
    CHARACTER(LEN=80)               :: var80
    integer, external :: length
    integer :: lenc,leni,lenv,lens,lenp,lend
    logical :: bbok
    integer :: nslice
    character*80, allocatable :: sdim80(:)
    character*25 :: myname="model_pushFile"
    if(mod_bdeb)write(*,*)myname,'Entering.',css%nFileIndexes,irc
    call chop0(path250,250)
    lenp=length(path250,250,20)
    if(mod_bdeb)write(*,*)myname," File: '"//path250(1:lenp)//"'"
    if (.not.associated(css%firstFile)) then
       call model_initfilestack(css,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from stackinit.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    ! create new stack-item
    bok=.true.
    allocate(newFile,stat=irc)
    if (irc.ne.0) then
       bok=.false.
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Unable to allocate new stack item.")
       call model_errorappend(crc250,"\n")
       return
    end if
    ! open file
    if (bok) then
       ! set file name...
       nslice=0
       allocate(sdim80(1))
       newFile%fn250=path250
       newFile%lenf=lenp
       newFile%cfileType=css%cfiletype
       ! open file
       call model_openFile(css,newFile,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from openFile.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    if (bok) then
       if (bok) then
          call model_readInventory(css,newFile,crc250,irc)
          if (irc.ne.0) then
             bok=.false.
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from readInventory.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
       end if
       if (bok) then
          call model_setRange(css,newFile,crc250,irc)
          if (irc.ne.0) then
             bok=.false.
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from setLimits.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if          
       end if
       if (bok) then
          call model_readSortVariable(css,newFile,crc250,irc)
          if (irc.ne.0) then
             bok=.false.
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from readSortVariable.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
       end if
       call model_closeFile(css,newFile,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from closeFile.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    ! push onto stack
    if (bok) then
       css%nFileIndexes=css%nFileIndexes + 1
       css%tsort=css%tsort + newFile%nsort
       css%stackReady=.false.
       newFile%prev => css%lastFile%prev
       newFile%next => css%lastFile
       newFile%prev%next => newFile
       newFile%next%prev => newFile
    else
       call deleteFile(css,newFile,crc250,irc)
       deallocate(newFile,stat=irc)
       irc=0 ! ignore any errors
    end if
    if(mod_bdeb)write(*,*)myname,' Done.',css%nFileIndexes
  end subroutine model_pushFile

  !
  ! Remove last model-file on the MODEL STACK
  !
  subroutine model_popfile(css,path250,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(mod_file), pointer :: currentFile => null()
    type(mod_file), pointer :: prevFile => null()
    character*25 :: myname="model_popfile"
    logical :: bdone
    integer, external :: length
    integer :: lenp
    if(mod_bdeb)write(*,*)myname,'Entering.',css%nFileIndexes,irc
    if (.not.associated(css%firstFile)) then
       call model_initfilestack(css,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from rmItem.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    call chop0(path250,250)
    lenp=length(path250,250,10)
    currentFile => css%lastFile%prev
    bdone=associated(currentFile,target=css%firstFile)
    do while (.not. bdone) 
       prevFile=>currentFile%prev
       if (currentFile%fn250(1:currentFile%lenf).eq.path250(1:lenp).or.lenp.eq.0) then
          if(mod_bdeb)write(*,*)myname," File: '"//path250(1:lenp)//"'"
          call model_deleteFile(css,currentFile,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from deleteFile.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          css%stackReady=.false.
          bdone=(lenp.eq.0)
       end if
       currentFile=>prevFile
       bdone=(bdone.or.associated(currentFile,target=css%firstFile))
    end do
    if(mod_bdeb)write(*,*)myname,' Done.',css%nFileIndexes
    return
  end subroutine model_popfile

  !
  ! Peek at last model-file put onto the MODEL STACK
  !
  subroutine model_peeklen(css,maxrep,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: maxrep
    character*250 :: crc250
    integer :: irc
    type(mod_file), pointer :: currentFile => null()
    integer :: ii,jj
    character*25 :: myname="model_peeklen"
    if (.not.associated(css%firstFile)) then
       call model_initfilestack(css,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from stackinit.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    currentFile => css%lastFile%prev
    maxrep=1
    if (.not.associated(currentFile,target=css%firstFile)) then
       ! file name
       maxrep=maxrep+1
       maxrep=maxrep+4
       maxrep=maxrep+currentFile%ndim*1
       do ii=1,currentFile%nvar
          maxrep=maxrep+1
          do jj=1,currentFile%var(ii)%ptr%ndim
             maxrep=maxrep+1
          end do
       end do
    end if
    if(mod_bdeb)write(*,*)myname,' Done.',maxrep
  end subroutine model_peeklen
  !
  subroutine model_peek(css,maxrep,nrep, rep250, crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: maxrep
    integer :: nrep
    character*250 :: rep250(maxrep)
    character*250 :: crc250
    integer :: irc
    character*50 :: s1, s2, s3
    integer, external :: length
    integer :: len1,len2,len3,lenm,lenv,lena,lenr,lend,lens
    type(mod_file), pointer :: currentFile => null()
    integer :: ii,jj
    character*80 :: var80
    character*25 :: myname="model_peek"
    if(mod_bdeb)write(*,*)myname,' Entering.'
    if (.not.associated(css%firstFile)) then
       call model_initfilestack(css,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from stackinit.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    currentFile => css%lastFile%prev
    nrep=0
    if (.not.associated(currentFile,target=css%firstFile)) then
       ! file name
       nrep=min(maxrep,nrep+1)               ! file name  +1
       call chop0(currentFile%fn250,250)
       lenm=length(currentFile%fn250,250,20)
       rep250(nrep)="file"//sep//"name"//sep//currentFile%fn250(1:lenm)
       ! sorting variable is available
       call chop0(css%ind_var,80)
       lens=length(css%ind_var,80,10)
       if (lens.gt.0) then
          nrep=min(maxrep,nrep+1)             ! target value   +1
          rep250(nrep)="file"//sep//"index"//sep//"variable"//sep//css%ind_var(1:lens)
          write(s1,'(I0)') currentFile%nsort; call chop0(s1,50); len1=length(s1,50,10)
          nrep=min(maxrep,nrep+1)          ! sort values      +nsort
          rep250(nrep)="file"//sep//"index"//sep//"len"//&
               & sep//s1(1:len1)
          if (currentFile%nsort.gt.0) then
             write(s1,'(F17.5)') currentFile%ind_start; call chop0(s1,50); len1=length(s1,50,10)
             nrep=min(maxrep,nrep+1)          ! sort values      +nsort
             rep250(nrep)="file"//sep//"index"//sep//"start"//&
                  & sep//s1(1:len1)
             write(s1,'(F17.5)') currentFile%ind_stop; call chop0(s1,50); len1=length(s1,50,10)
             nrep=min(maxrep,nrep+1)          ! sort values      +nsort
             rep250(nrep)="file"//sep//"index"//sep//"stop"//&
                  & sep//s1(1:len1)
          end if
       end if
       ! dimensions
       write(s1,'(I12)') currentFile%ndim; call chop0(s1,50); len1=length(s1,50,10)
       do ii=1,currentFile%ndim
          write(s2,'(I12)') ii; call chop0(s2,50); len2=length(s2,50,10)
          call chop0(currentFile%dim80(ii),80)
          lend=length(currentFile%dim80(ii),80,10)
          nrep=min(maxrep,nrep+1)              ! dimension size  +currentFile%ndim
          write(rep250(nrep),*) currentFile%istop(ii)
          lenr=length(rep250(nrep),250,10)
          rep250(nrep)="file"//sep//"dimension"//sep//s2(1:len2)//sep// &
               & currentFile%dim80(ii)(1:lend)//sep//"size"//sep//rep250(nrep)(1:lenr)
       end do
       if(mod_bdeb)write(*,*)myname,' Near.'
       ! variables (name, dimensions)
       write(s1,'(I12)') currentFile%nvar; call chop0(s1,50); len1=length(s1,50,10)
       do ii=1,currentFile%nvar
          write(s1,'(I12)') ii; call chop0(s1,50);len1=length(s1,50,10)
          var80=currentFile%var(ii)%ptr%var80
          call chop0(var80,80)
          lenv=length(var80,80,10)
          if(mod_bdeb)write(*,*)myname,'VAR80:',var80(1:lenv),lenv
          write(s2,'(I12)') currentFile%var(ii)%ptr%ndim; call chop0(s2,50);len2=length(s2,50,10)
          nrep=min(maxrep,nrep+1)                 ! var index  +currentFile%nvar
          rep250(nrep)="file"//sep//"variable"//sep//s1(1:len1)//sep//"name"//sep//var80(1:lenv)
          do jj=1,currentFile%var(ii)%ptr%ndim
             write(s2,'(I12)') jj; call chop0(s2,50);len2=length(s2,50,10)
             s3= currentFile%dim80(currentFile%var(ii)%ptr%ind(jj))(1:50);call chop0(s3,50);len3=length(s3,50,10)
             nrep=min(maxrep,nrep+1)              ! dimension index  +currentFile%nvar * currentFile%var(ii)%ptr%ndim
             rep250(nrep)="file"//sep//"variable"//sep//s1(1:len1)//sep &
                  & //"dimension"//sep//s2(1:len2)//sep//s3(1:len3)
          end do
       end do
    end if

    !do ii=1,nrep
    !  lenr=length(rep250(ii),250,100)
    !  write(*,*) myname,'REP:',ii,maxrep,rep250(ii)(1:lenr)
    !end do

    if(mod_bdeb)write(*,*)myname,' Done.',maxrep,nrep
  end subroutine model_peek
  !
  !
  !###############################################################################
  ! STACK LOOP ROUTINES
  !###############################################################################
  ! Forecasts from one analysis is pready at a time... (=the same parameters)
  ! Used in this way
  ! 1) model_setFileStackLimits: make index limits of files in stack
  ! 2) model_loopFileStack: loop over files until false return...
  integer function model_getFileId(css)
    type(mod_session), pointer :: css !  current session
    model_getFileId=css%currentFileIndex
    return
  end function model_getFileId
  !
  logical function model_loopFileStack(css,mod_lval,mod_minval,mod_maxval,crc250,irc)
    type(mod_session), pointer :: css !  current session
    logical :: mod_lval(2)
    real :: mod_minval
    real :: mod_maxval
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_loopFileStack"
    logical :: bdone,found
    found=.false.
    bdone=(.not. css%sortLimitsOk)
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
          if (.not.((mod_lval(1).and.mod_minval.gt.css%currentFile%ind_stop) .or.&
               & (mod_lval(2).and.mod_maxval.lt.css%currentFile%ind_start)).and. &
               & .not.((css%ind_lval(1).and.css%ind_minval.gt.css%currentFile%ind_stop) .or.&
               & (css%ind_lval(2).and.css%ind_maxval.lt.css%currentFile%ind_start))) then ! overlap
             found=.true.
             bdone=.true.
             if (mod_bdeb) write(*,*)myname,' Found:',css%sortLimitsOk,&
                  & css%currentFileSortIndex,css%leftFileSortIndex,css%rightFileSortIndex
          end if
       end if
    end do
    model_loopFileStack=found
    return
  end function model_loopFileStack
  !
  ! check if we immediately know that current file is outside target limits
  !
  logical function model_rangeCheck(css,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: ii
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_rangeCheck"
    logical :: bdone,found
    integer :: itrg,varid
    type(mod_variable),pointer :: v
    type(mod_file),pointer :: f
    ! check if we have targets with limits set
    if (associated(css%currentFile)) then
       f => css%currentFile
       call model_setTargetIndex(css,f,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from setTargetIndex.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       if(mod_bdeb)write(*,*)myname,'Checking target range:',css%ctrg
       do itrg=1,css%ctrg
          if (css%trg_minset(itrg).or.css%trg_maxset(itrg)) then !check variable max/min
             ! get variable          
             varid=css%trg_var(itrg) ! set by model_setTargetIndex
             v => f%var(varid)%ptr
             if (associated(v)) then
                if (v%mmrange) then ! we have limits
                   if (.not.model_variableCheck(css,v,itrg)) then
                      if(mod_bdeb)write(*,*)myname,'Failed:',varid," '"//v%var80(1:v%lenv)//"'",&
                           & itrg, v%mmrange,v%mmset,v%minval,v%maxval,&
                           & css%trg_minset(itrg),css%trg_maxset(itrg),&
                           & css%trg_minval(itrg),css%trg_maxval(itrg)
                      model_rangeCheck=.false.
                      return
                   end if
                end if
             end if
          end if
       end do
    end if
    model_rangeCheck=.true. ! file is ok
    return
  end function model_rangeCheck
  !
  ! Check that variable max/min is within target limits...
  !
  logical function model_variableCheck(css,v,itrg)
    type(mod_session), pointer :: css !  current session
    type(mod_variable), pointer :: v
    integer :: itrg
    if (.not.v%mmset) then
       model_variableCheck=.false. ! required values are all undefined...
       return
    else
       if (css%trg_minset(itrg)) then
          if (v%maxval.lt.css%trg_minval(itrg)) then
             model_variableCheck=.false.
             return
          end if
       end if
       if (css%trg_maxset(itrg)) then
          if (v%minval.gt.css%trg_maxval(itrg)) then
             model_variableCheck=.false.
             return
          end if
       end if
    end if
    model_variableCheck=.true. ! all ok so far...
    return
  end function model_variableCheck
  !
  ! sort the file stack
  !
  subroutine model_sortStack(css,crc250,irc)
    use sort
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(mod_file), pointer :: currentFile => null()
    integer :: ii,jj
    character*25 :: myname="model_sortStack"
    !
    ! make array of files
    if(mod_bdeb)write(*,*)myname,' Entering.'
    if (associated(css%fileStack)) deallocate(css%fileStack)
    if (allocated(css%fileStackSort)) deallocate(css%fileStackSort)
    if (allocated(css%fileStackInd)) deallocate(css%fileStackInd)
    if (mod_bdeb) write(*,*)myname,'Allocating sort stack:',css%nFileIndexes
    allocate(css%fileStack(max(1,css%nFileIndexes)),css%fileStackSort(max(1,css%nFileIndexes),2),&
         &css%fileStackInd(max(1,css%nFileIndexes),2),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from allocate (")
       call model_errorappendi(crc250,css%nFileIndexes)
       call model_errorappend(crc250,")")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    css%msort=0 ! max number of forecasts
    currentFile => css%firstFile%next
    ii=0
    do while (.not.associated(currentFile, target=css%lastFile))
       css%msort=max(css%msort,currentFile%nsort)
       ii=ii+1
       if (ii.le.css%nFileIndexes) then
          css%fileStack(ii)%ptr => currentFile
          if (currentFile%ind_lim) then
             css%fileStackInd(ii,1)=ii
             css%fileStackInd(ii,2)=ii
             css%fileStackSort(ii,1)=currentFile%ind_start
             css%fileStackSort(ii,2)=currentFile%ind_stop
          else ! no index available for file
             if (mod_bdeb)then
                write(*,*)myname,"Missing index limits in '"//&
                     & currentFile%fn250(1:currentFile%lenf)//&
                     & "', ignoring file."
                write(*,*)myname,'Indexes ',currentFile%ind_lim,&
                     & currentFile%ind_start,currentFile%ind_stop
             end if
             ii=ii-1 ! ignore file...
          end if
       end if
       currentFile => currentFile%next
    end do
    if (ii.ne.css%nFileIndexes) then
       irc=944
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Missing indexes:")
       call model_errorappendi(crc250,css%nFileIndexes)
       call model_errorappend(crc250,"!=")
       call model_errorappendi(crc250,ii)
       call model_errorappend(crc250,"\n")
       return
    end if
    ! make sorted index (chronologically)
    css%nFileSortIndexes=css%nFileIndexes
    css%newnFileSortIndexes(1)=css%nFileIndexes
    css%newnFileSortIndexes(2)=css%nFileIndexes
    call sort_heapsort1r(css%nFileIndexes,css%fileStackSort(1,1),1.0D-5,&
         & css%newnFileSortIndexes(1),css%nFileSortIndexes,css%fileStackInd(1,1),.false.)
    call sort_heapsort1r(css%nFileIndexes,css%fileStackSort(1,2),1.0D-5,&
         & css%newnFileSortIndexes(2),css%nFileSortIndexes,css%fileStackInd(1,2),.false.)
    css%stackReady = .true.
    if(mod_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine model_sortStack
  !
  subroutine model_findStackLimits(css,ind_lval,ind_minval,ind_maxval,crc250,irc)
    use sort
    type(mod_session), pointer :: css !  current session
    logical :: ind_lval(2)
    real :: ind_minval
    real :: ind_maxval
     character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_findStackLimits"
    integer :: leftmin,rightmin,leftmax,rightmax
    logical :: mod_lval(2)
    real :: mod_minval
    real :: mod_maxval
    if (mod_bdeb)write(*,*)myname,'Entering, Number of files:',css%nFileIndexes
    mod_lval(1)=ind_lval(1)
    mod_minval=ind_minval
    if (mod_lval(1).and.css%ind_lval(1)) then
       mod_minval=max(mod_minval,css%ind_minval)
    else if (css%ind_lval(1)) then
       mod_lval(1)=.true.
       mod_minval=css%ind_minval
    end if
    mod_lval(2)=ind_lval(2)
    mod_maxval=ind_maxval
    if (mod_lval(2).and.css%ind_lval(2)) then
       mod_maxval=min(mod_maxval,css%ind_maxval)
    else if (css%ind_lval(2)) then
       mod_lval(2)=.true.
       mod_maxval=css%ind_maxval
    end if
    if (mod_lval(1)) then
       call sort_heapsearch1r(css%nFileIndexes,css%fileStackSort(1,2),1.0D-5, &
            & css%nFileSortIndexes,css%fileStackInd(1,2),mod_minval,leftmin,rightmin)
       rightmin=max(leftmin,rightmin) ! ignore before first entry
       if (mod_bdeb) write(*,*)myname,' Minval:',mod_minval,&
            & css%fileStackSort(1,2),css%fileStackInd(1,2),&
            & leftmin,rightmin
    else
       leftmin=1
       rightmin=1
    end if
    if (mod_lval(2)) then
       call sort_heapsearch1r(css%nFileIndexes,css%fileStackSort(1,1),1.0D-5, &
            & css%nFileSortIndexes,css%fileStackInd(1,1),mod_maxval,leftmax,rightmax)
       leftmax=min(rightmax,leftmax) ! ignore after last entry
       if (mod_bdeb) write(*,*)myname,' Maxval:',mod_maxval,&
            & css%fileStackSort(1,1),css%fileStackInd(1,1),&
            & leftmax,rightmax
    else
       leftmax=css%nFileSortIndexes
       rightmax=css%nFileSortIndexes
    end if
    if (mod_bdeb)write(*,'(X,A,X,A,5(X,I0))')myname,'Limits.', &
         & leftmin,rightmin,leftmax,rightmax,css%nFileSortIndexes
    css%sortLimitsOk= (leftmin.le.css%nFileSortIndexes.and.rightmax.ge.1) ! check for overlap...
    if (css%sortLimitsOk) then
       css%leftFileSortIndex=min(leftmin,rightmin)
       css%rightFileSortIndex=max(leftmax,rightmax)
    else
       css%leftFileSortIndex=0
       css%rightFileSortIndex=0
    end if
    css%currentFileIndex=0
    if (mod_bdeb)write(*,*)myname,' Done.', css%sortLimitsOk,&
         & css%leftFileSortIndex, css%rightFileSortIndex,css%nFileIndexes
    return
  end subroutine model_findStackLimits
  !
  !###############################################################################
  ! CACHE ROUTINES
  !###############################################################################
  !
  subroutine model_makecache(css,path250,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(mod_file), pointer :: currentFile !  current file
    integer, external :: length,ftunit
    integer :: lenp,lenf,lenv,lend,unitr,ii,jj,kk
    character*22 :: myname="model_makeCache"
    character*50 :: minval50,maxval50
    integer :: lenmi,lenma,leno,cnt
    character*250 :: old250
    if(mod_bdeb)write(*,*) myname,' Entering.',irc
    call chop0(path250,250)
    lenp=length(path250,250,20)
    if(mod_bdeb)write(*,*)myname,' Path.',path250(1:lenp)
    ! open file
    unitr=ftunit(irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," no free unit number for:"//path250(1:lenp))
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    lenp=length(path250,250,10)
    open ( unit=unitr, status="unknown", form="formatted", &
         &        access="sequential", &
         &        iostat=irc, file=path250(1:lenp) )
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," unable to open:"//path250(1:lenp))
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    !
    call model_makeTargetList(css,crc250,irc) 
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from makeTargetList.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    !
    call model_sortStack(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from sortStack.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    !
    ! write number of files: css%nFileIndexes
    if(mod_bdeb)write(*,*) myname,' Stack entries.',css%nFileIndexes,unitr
    leno=0
    cnt=0
    do ii=1,css%newnFileSortIndexes(1)
       if (css%fileStackInd(ii,1).eq.0) cycle
       currentFile=>css%fileStack(css%fileStackInd(ii,1))%ptr
       if (old250(1:leno).ne.currentFile%fn250(1:currentFile%lenf)) then
          cnt=cnt+1
       end if
       old250=currentFile%fn250
       leno=currentFile%lenf
    end do
    if(mod_bdeb)write(*,*) myname,' Stack cnt.',cnt,&
         & css%newnFileSortIndexes(1),size(css%fileStackInd(:,1))
    write(unitr,'(I0)',iostat=irc) cnt
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," unable to write to:"//path250(1:lenp))
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    ! loop over file stack
    leno=0
    cnt=0
    do ii=1,css%newnFileSortIndexes(1)
       !if(mod_bdeb)write(*,*) myname,' Loop.',ii
       if (css%fileStackInd(ii,1).eq.0) cycle
       currentFile=>css%fileStack(css%fileStackInd(ii,1))%ptr
       if (old250(1:leno).ne.currentFile%fn250(1:currentFile%lenf)) then
          cnt=cnt+1
          lenf=currentFile%lenf
          write(unitr,'(L1,5(X,I0),X,A)',iostat=irc) &
               & currentFile%ind_lim,&
               & currentFile%tsort,&
               & currentFile%nsort,&
               & currentFile%ndim,&
               & currentFile%nvar,&
               & currentFile%lenf,&
               & currentFile%fn250(1:LENF)
          ! write category summary
          do jj=1,currentFile%nsort
             lend=length(currentFile%desc250(jj),250,5)
             write(unitr,'(X,F0.10,X,I0,X,A)',iostat=irc) &
                  & currentFile%sort(jj),&
                  & currentFile%indsort(jj),&
                  & currentFile%desc250(jj)(1:lend)
          end do
          do jj=1,currentFile%ndim
             lend=length(currentFile%dim80(jj),80,5)
             write(unitr,'(X,I0,X,A)',iostat=irc) currentFile%istop(jj),&
                  & currentFile%dim80(jj)(1:lend)
          end do
          do jj=1,currentFile%nvar
             !call chop0(currentFile%var(jj)%ptr%var80,80)
             lenv=length(currentFile%var(jj)%ptr%var80,80,5)
             write(unitr,'(I0,X,A)',advance="no",iostat=irc) &
                  & currentFile%var(jj)%ptr%ndim, currentFile%var(jj)%ptr%var80(1:lenv)
             do kk=1,currentFile%var(jj)%ptr%ndim
                write(unitr,'(X,I0)',advance="no",iostat=irc) currentFile%var(jj)%ptr%ind(kk)
             end do
             write(unitr,'(X,L1)',advance="no",iostat=irc) &
                  & currentFile%var(jj)%ptr%mmrange
             if (currentFile%var(jj)%ptr%mmrange) then
                write(unitr,'(X,L1)',advance="no",iostat=irc) &
                     & currentFile%var(jj)%ptr%mmset
                if (currentFile%var(jj)%ptr%mmset) then
                   call model_wash(currentFile%var(jj)%ptr%minval,minval50,lenmi)
                   call model_wash(currentFile%var(jj)%ptr%maxval,maxval50,lenma)
                   write(unitr,'(X,A,X,A)',iostat=irc) minval50(1:lenmi),maxval50(1:lenma)
                else
                   write(unitr,*,iostat=irc)
                end if
             else
                write(unitr,*,iostat=irc)
             end if
          end do
       end if
       old250=currentFile%fn250
       leno=currentFile%lenf
    end do
    if(mod_bdeb)write(*,*) myname,' Closing.',cnt,css%newnFileSortIndexes(1)
    ! close file
    close(unitr,iostat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," unable to close:"//path250(1:lenp))
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    if(mod_bdeb)write(*,*)myname,' Done.',irc
  end subroutine model_makecache
  !
  ! load cache file
  !
  subroutine model_loadcache(css,path250,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(mod_file), pointer :: cfile, cfilen
    type(mod_file),pointer :: newFile
    integer, external :: length
    integer :: lenp,lenb,lend,ii,jj,kk,opos,pos,unitr,cnt
    type(mod_variable),pointer :: v     ! variable
    character*250 :: buff250
    character*1 :: c1
    character*22 :: myname="model_loadCache"
    logical :: bok
    if(mod_bdeb)write(*,*) myname,' Entering.',irc
    call chop0(path250,250)
    lenp=length(path250,250,20)
    ! clear existing cache
    css%stackReady=.false.
    if (associated(css%firstFile)) then
       cfile => css%firstFile%next
       do while (.not.associated(cfile,target=css%lastFile))
          cfilen => cfile%next
          call model_deleteFile(css,cfile,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from deleteFile.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          cfile  => cfilen
       end do
    end if
    ! open cache file
    if(mod_bdeb)write(*,*)myname," Opening modcache: '",path250(1:lenp)//"'"
    open ( unit=unitr, status="old", form="formatted", &
         &        access="sequential", &
         &        iostat=irc, file=path250(1:lenp) )
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," unable to open:"//path250(1:lenp))
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    ! write number of files: css%nFileIndexes
    read(unitr,'(A)',iostat=irc) buff250
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," unable to read:"//path250(1:lenp))
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    cnt=0
    read(buff250,*,iostat=irc) css%nFileIndexes
    if (irc.ne.0) then
       if (mod_bdeb) write(*,*) myname," Unable to interprt nfileindexes."
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," unable to interpret:"//path250(1:lenp))
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    ! loop through cache file
    if (mod_bdeb)write(*,*)myname,'Looking for files:',css%nFileIndexes
    do ii=1,css%nFileIndexes
       !if (mod_bdeb) write(*,*) myname," file loop started.",ii,css%nFileIndexes
       bok=.true.
       allocate(newFile,stat=irc)
       if (irc.ne.0) then
          if (mod_bdeb) write(*,*) myname," Unable to allocate new File item."
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Unable to allocate new File item.")
          call model_errorappend(crc250,"\n")
          return
       end if
       css%stackReady=.false.
       newFile%prev => css%lastFile%prev
       newFile%next => css%lastFile
       newFile%prev%next => newFile
       newFile%next%prev => newFile
       !
       !if (mod_bdeb) write(*,*) myname," reading index.",ii
       read(unitr,'(A)',iostat=irc) buff250
       if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read new line."
       call chop0(buff250,250)
       lenb=length(buff250,250,10)
       pos=0
       opos=pos
       call findDelimiter(buff250(1:lenb)," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%ind_lim
       if(bok)bok=(irc.eq.0)
       if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' ind_lim"
       opos=pos
       call findDelimiter(buff250(1:lenb)," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%tsort
       if(bok)bok=(irc.eq.0)
       if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' tsort"
       opos=pos
       call findDelimiter(buff250(1:lenb)," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%nsort
       if(bok)bok=(irc.eq.0)
       if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' nsort"
       opos=pos
       call findDelimiter(buff250(1:lenb)," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%ndim
       if(bok)bok=(irc.eq.0)
       if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' ndim"
       opos=pos
       call findDelimiter(buff250(1:lenb)," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%nvar 
       if(bok)bok=(irc.eq.0)
       if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' nvar"
       opos=pos
       call findDelimiter(buff250(1:lenb)," ",pos)
       read(buff250(opos+1:pos-1),*,iostat=irc)newFile%lenf
       if(bok)bok=(irc.eq.0)
       if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' lenf"
       opos=pos
       pos=251 ! call findDelimiter(buff250(1:lenb)," ",pos)
       newFile%fn250=buff250(opos+1:pos-1)
       !
       if (mod_bdeb) write(*,*) myname," Loaded:'"//newFile%fn250(1:newFile%lenf)//"'",newFile%ind_lim,ii
       !
       allocate(newFile%sort(newFile%nsort),newFile%indsort(newFile%nsort),newFile%desc250(newFile%nsort),stat=irc)
       if (irc.ne.0) then
          if (mod_bdeb) write(*,*) myname," Unable to allocate new Sort item."
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Unable to allocate new Sort item.")
          call model_errorappend(crc250,"\n")
          return
       end if
       !if (mod_bdeb) write(*,*) myname," reading sort."
       do jj=1,newFile%nsort
          read(unitr,'(A)',iostat=irc) buff250
          if(bok)bok=(irc.eq.0)
          if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read new line."
          call chop0(buff250,250)
          lenb=length(buff250,250,10)
          pos=0
          opos=pos
          call findDelimiter(buff250(1:lenb)," ",pos)
          read(buff250(opos+1:pos-1),*,iostat=irc)newFile%sort(jj)
          if(bok)bok=(irc.eq.0)
          if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' sort(",jj,")"
          opos=pos
          call findDelimiter(buff250(1:lenb)," ",pos)
          read(buff250(opos+1:pos-1),*,iostat=irc)newFile%indsort(jj)
          if(bok)bok=(irc.eq.0)
          if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' indsort(",jj,")"
          opos=pos
          pos=251 ! call findDelimiter(buff250(1:lenb)," ",pos)
          newFile%desc250(jj)=buff250(opos+1:pos-1)
          if (jj.eq.1) then
             newFile%ind_start=newFile%sort(jj)
             newFile%ind_stop=newFile%sort(jj)
          else
             newFile%ind_start=min(newFile%ind_start,newFile%sort(jj))
             newFile%ind_stop=max(newFile%ind_stop,newFile%sort(jj))
          end if
       end do
       allocate(newFile%istart(newFile%ndim),newFile%istop(newFile%ndim),&
            & newFile%dim80(newFile%ndim),newFile%dim_var(newFile%ndim),&
            & newFile%dim_val(newFile%ndim),newFile%dim_trg(newFile%ndim),&
            & newFile%var80(newFile%nvar),newFile%lenv(newFile%nvar),stat=irc)
       if (irc.ne.0) then
          if (mod_bdeb) write(*,*) myname," Unable to allocate new sort item."
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Unable to allocate new Sort item.")
          call model_errorappend(crc250,"\n")
          return
       end if
       !if (mod_bdeb) write(*,*) myname," reading dims."
       do jj=1,newFile%ndim
          read(unitr,'(A)',iostat=irc) buff250
          if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read new line."
          call chop0(buff250,250)
          lenb=length(buff250,250,10)
          pos=0
          opos=pos
          call findDelimiter(buff250(1:lenb)," ",pos)
          read(buff250(opos+1:pos-1),*,iostat=irc)newFile%istop(jj)
          if(bok)bok=(irc.eq.0)
          if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' istop(",jj,")"
          newFile%istart(jj)=1
          opos=pos
          pos=251 ! call findDelimiter(buff250(1:lenb)," ",pos)
          newFile%dim80(jj)=buff250(opos+1:min(opos+80,pos-1))
          call chop0(newFile%dim80(jj),80)
          lend=length(newFile%dim80(jj),80,10)
          newFile%dim_var(jj)=newFile%dim80(jj)(1:lend)
          newFile%dim_val(jj)=real(newFile%istop(jj))
          newFile%dim_trg(jj)=0
       end do
       allocate(newFile%var(newFile%nvar),stat=irc)
       if (irc.ne.0) then
          if (mod_bdeb) write(*,*) myname," Unable to allocate new var item."
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Unable to allocate new Var item.")
          call model_errorappend(crc250,"\n")
          return
       end if
       !if (mod_bdeb) write(*,*) myname," reading vars."
       do jj=1,newFile%nvar
          allocate(newFile%var(jj)%ptr,stat=irc)
          if (irc.ne.0) then
             if (mod_bdeb) write(*,*) myname," Unable to allocate new var ptr."
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Unable to allocate new Var item.")
             call model_errorappend(crc250,"\n")
             return
          end if
          v => newFile%var(jj)%ptr
          read(unitr,'(A)',iostat=irc) buff250
          if(bok)bok=(irc.eq.0)
          if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read new line."
          call chop0(buff250,250)
          lenb=length(buff250,250,10)
          pos=0
          opos=pos
          call findDelimiter(buff250(1:lenb)," ",pos)
          read(buff250(opos+1:pos-1),*,iostat=irc)v%ndim
          if(bok)bok=(irc.eq.0)
          if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' ndim"
          opos=pos
          call findDelimiter(buff250(1:lenb)," ",pos)
          v%var80=buff250(opos+1:min(80+opos,pos-1))
          v%lenv=pos-opos-1
          newFile%var80(jj)=v%var80
          newFile%lenv(jj)=v%lenv
          allocate(v%ind(v%ndim),v%istart(v%ndim),v%icount(v%ndim),stat=irc)
          if (irc.ne.0) then
             if (mod_bdeb) write(*,*) myname," Unable to allocate new var dim."
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Unable to allocate new Var dim item.")
             call model_errorappendi(crc250,jj)
             call model_errorappendi(crc250,v%ndim)
             call model_errorappend(crc250,"\n")
             return
          end if
          do kk=1,v%ndim
             opos=pos
             call findDelimiter(buff250(1:lenb)," ",pos)
             read(buff250(opos+1:pos-1),*,iostat=irc) v%ind(kk)
             if(bok)bok=(irc.eq.0)
             if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' ind(",kk,")"
          end do
          opos=pos
          call findDelimiter(buff250(1:lenb)," ",pos)
          c1=buff250(opos+1:max(opos+1,min(opos+2,pos-1)))
          v%mmrange=(c1.eq."T")
          if (v%mmrange) then
             opos=pos
             call findDelimiter(buff250(1:lenb)," ",pos)
             v%mmset=(buff250(opos+1:pos-1).eq."T")
          else
             v%mmset=.false.
          end if
          if (v%mmset) then
             opos=pos
             call findDelimiter(buff250(1:lenb)," ",pos)
             read(buff250(opos+1:pos-1),*,iostat=irc)v%minval
             if(bok)bok=(irc.eq.0)
             if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' minval"
             opos=pos
             call findDelimiter(buff250(1:lenb)," ",pos)
             read(buff250(opos+1:pos-1),*,iostat=irc)v%maxval
             if(bok)bok=(irc.eq.0)
             if(mod_bdeb.and.irc.ne.0)write(*,*)myname,"Unable to read:'"//buff250(opos+1:pos-1)//"' maxval"
          end if
       end do
       if (bok) then
          cnt=cnt+1
       else
          irc=0
          if (mod_bdeb)write(*,*)myname,'System error! Deleting Invalid entry:',ii
          call model_deleteFile(css,newFile,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from deleteFile.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
       end if
       !if (mod_bdeb) write(*,*) myname," file loop ended.",ii,css%nFileIndexes
    end do
    ! close file
    close(unitr,iostat=irc)
    if (irc.ne.0) then
       if (mod_bdeb) write(*,*) myname," Unable to close:"//path250(1:lenp)
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," unable to close:"//path250(1:lenp))
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    css%nFileIndexes=cnt! should not be necessary...
    if(mod_bdeb)write(*,*)myname,' Done, files in stack:',css%nFileIndexes,irc
  end subroutine model_loadcache
  !
  !
  !###############################################################################
  ! TARGET ROUTINES (used for slicing fields)
  !###############################################################################
  ! clear the target stack
  !
  subroutine model_cleartargetStack(css,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(mod_target), pointer :: currentTarget => null() !  current session
    type(mod_target), pointer :: nextTarget => null() !  current session
    character*25 :: myname="model_cleartargetstack"
    currentTarget => css%firstTrg%next
    do while (.not.associated(currentTarget,target=css%lastTrg))
       nextTarget => currentTarget%next
       currentTarget%prev%next =>  currentTarget%next
       currentTarget%next%prev =>  currentTarget%prev
       deallocate(currentTarget,stat=irc)
       css%ntrg=css%ntrg-1
       currentTarget => nextTarget
    end do
    css%trg_set=.false.
    return
  end subroutine model_cleartargetstack
  !
  ! push target to the stack
  !
  subroutine model_pushtarget(css,n80,v80,min80,max80,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*80 :: n80        ! target name
    character*80 :: v80        ! variable
    character*80 :: min80        ! lower value
    character*80 :: max80        ! upper value
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    integer :: lenn
    type(mod_target), pointer :: newTarget !  the new target
    character*25 :: myname="model_pushtarget"
    call chop0(n80,80)
    lenn=length(n80,80,10)
    allocate(newtarget,stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate 'target'.")
       call model_errorappend(crc250,"\n")
       return
    end if
    newTarget%n80=n80
    newTarget%lenn=lenn
    newTarget%v80=v80
    newTarget%min80=min80
    newTarget%max80=max80
    css%ntrg=css%ntrg+1
    newTarget%prev => css%lastTrg%prev
    newTarget%next => css%lastTrg
    newTarget%prev%next => newTarget
    newTarget%next%prev => newTarget
    css%trg_set=.false.
    if(mod_bdeb)write(*,*)myname,"Target: '"//n80(1:lenn)//"'",css%ntrg
    return
  end subroutine model_pushtarget
  !
  logical function model_loopTarget(css,n80,v80,min80,max80,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    character*80  :: n80       ! target name
    character*80  :: v80       ! variable
    character*80  :: min80      ! min value
    character*80  :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_loopTarget"
    model_looptarget=.false. ! only true if all is ok...
    if (.not.associated(css%currentTrg)) then
       css%currentTrg =>  css%firstTrg%next 
    else
       css%currentTrg =>  css%currentTrg%next
    end if
    if (associated(css%currentTrg,css%lastTrg)) then
       nullify(css%currentTrg)
       model_loopTarget=.false.
    else
       n80=css%currentTrg%n80
       v80=css%currentTrg%v80
       min80=css%currentTrg%min80
       max80=css%currentTrg%max80
       model_looptarget=.true.
    end if
    return
  end function model_loopTarget
  !
  ! make target a slice variable
  !
  subroutine model_settargetslice(css,n80,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*80 :: n80        ! target name
    character*250 :: crc250
    integer :: irc
    type(mod_target), pointer :: ctrg !  the new target
    character*25 :: myname="model_settargetslice"
    integer, external :: length
    integer :: lenn
    lenn=length(n80,80,10)
    if(mod_bdeb)write(*,*)myname,'Make slice of target: ',n80(1:lenn)
    ctrg => css%firstTrg%next
    do while (.not.associated(ctrg,target=css%lastTrg))
       if (ctrg%n80(1:ctrg%lenn).eq.n80(1:lenn)) then
          ctrg%lslice=.true.
          return
       end if
       ctrg=>ctrg%next
    end do
    return
  end subroutine model_settargetslice
  !
  ! get number of targets
  !
  integer function model_targetCount(css,crc250,irc)
    type(mod_session), pointer :: css   ! session structure
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_targetCount "
    model_targetCount=css%ntrg
    return
  end function model_targetCount
  !
  integer function model_trgCount(css,crc250,irc)
    type(mod_session), pointer :: css   ! session structure
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_trgCount "
    model_trgCount=css%otrg
    return
  end function model_trgCount
  !
  ! get number of locations
  !
  integer function model_locationCount(css,crc250,irc)
    type(mod_session), pointer :: css   ! session structure
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_locationCount "
    model_locationCount=css%oloc
    return
  end function model_locationCount
  !
  ! make target list
  !
  subroutine model_getTrg80(css,var80,offset,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*80, allocatable :: var80(:)
    integer :: offset !
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_getTrg80"
    integer ii
    do ii=1,css%ctrg
       var80(ii+offset)=css%trg80(ii)
    end do
    return
  end subroutine model_getTrg80
  !
  ! get output values
  !
  subroutine model_getVal(css,iloc,val,offset,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: iloc
    real, allocatable :: val(:)
    integer :: offset
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_getVal"
    integer ii
    do ii=1,css%otrg
       if (css%oset(ii,iloc)) then
          val(ii+offset)=css%oval(ii,iloc)
       else
          val(ii+offset)=0.0D0
       end if
    end do
    return
  end subroutine model_getVal
  !
  ! update the session list over variables etc. on the target stack
  !
  subroutine model_maketargetlist(css,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_maketargetlist"
    type(mod_target), pointer :: currentTarget
    integer ii,lens,irc2
    integer, external :: length
    type(parse_session),pointer :: plim => null()  ! parse_session pointer must be set to null
    if(mod_bdeb)write(*,*)myname,'Entering.',irc,css%trg_set,css%ntrg
    if ( .not. css%trg_set ) then
       call parse_open(plim,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Error return from 'parse_open'.")
          return
       end if
       css%ctrg=css%ntrg
       if (css%ind_set) css%ctrg=css%ctrg+1
       if(associated(css%trg80)) deallocate(css%trg80)
       if(associated(css%trg_lent)) deallocate(css%trg_lent)
       if(associated(css%trg_v80)) deallocate(css%trg_v80)
       if(associated(css%trg_lenv)) deallocate(css%trg_lenv)
       if(associated(css%trg_dim)) deallocate(css%trg_dim)
       if(associated(css%trg_var)) deallocate(css%trg_var)
       if(associated(css%trg_min80)) deallocate(css%trg_min80)
       if(associated(css%trg_max80)) deallocate(css%trg_max80)
       if(associated(css%trg_minval)) deallocate(css%trg_minval)
       if(associated(css%trg_maxval)) deallocate(css%trg_maxval)
       if(associated(css%trg_sliceset)) deallocate(css%trg_sliceset)
       if(associated(css%trg_valset)) deallocate(css%trg_valset)
       if(associated(css%trg_minset)) deallocate(css%trg_minset)
       if(associated(css%trg_maxset)) deallocate(css%trg_maxset)
       if(associated(css%trg_req)) deallocate(css%trg_req)
       if(associated(css%trg_vok)) deallocate(css%trg_vok)
       if(associated(css%trg_val)) deallocate(css%trg_val)
       if(associated(css%trg_ook)) deallocate(css%trg_ook)
       if(associated(css%trg_orm)) deallocate(css%trg_orm)
       if (css%ctrg.ne.0) then
          allocate(css%trg80(css%ctrg), css%trg_lent(css%ctrg), css%trg_v80(css%ctrg),  &
               & css%trg_lenv(css%ctrg), css%trg_dim(css%ctrg), css%trg_var(css%ctrg), &
               & css%trg_min80(css%ctrg), css%trg_max80(css%ctrg),&
               & css%trg_minval(css%ctrg), css%trg_maxval(css%ctrg), &
               & css%trg_sliceset(css%ctrg), css%trg_valset(css%ctrg), &
               & css%trg_minset(css%ctrg), css%trg_maxset(css%ctrg), &
               & css%trg_req(css%ctrg), css%trg_vok(css%ctrg), css%trg_val(css%ctrg), &
               & css%trg_ook(0:css%ctrg), css%trg_orm(0:css%ctrg), stat=irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250,"Unable to allocate 'session:n80...'.")
             call model_errorappend(crc250,"\n")
             return
          end if
          ii=0
          css%trg_ook(ii)=0
          css%trg_orm(ii)=0
          currentTarget => css%firstTrg%next
          do while (.not.associated(currentTarget,target=css%lastTrg))
             ii=min(css%ctrg,ii+1)
             !
             css%trg80(ii)=currentTarget%n80
             call chop0(css%trg80(ii),80)
             css%trg_lent(ii)=length(css%trg80(ii),80,10)
             !
             css%trg_v80(ii)=currentTarget%v80
             call chop0(css%trg_v80(ii),80)
             css%trg_lenv(ii)=length(css%trg_v80(ii),80,10)
             !
             css%trg_valset(ii)=.true.
             if (mod_bdeb) write(*,'(2(X,A),X,I0,2(X,A))')myname,'Target:',II,&
                  & css%trg80(ii)(1:css%trg_lent(ii)),&
                  & css%trg_v80(ii)(1:css%trg_lenv(ii))
             !
             css%trg_min80(ii)=currentTarget%min80
             call chop0(css%trg_min80(ii),80)
             lens=length(css%trg_min80(ii),80,10)
             if (lens.ne.0) then
                call parse_parsef(plim,css%trg_min80(ii)(1:lens),css%sys_var,crc250,irc)
                if (irc.ne.0) then
                   call model_errorappend(crc250,myname)
                   call model_errorappend(crc250," Error return from parsef.")
                   call model_errorappendi(crc250,irc)
                   call model_errorappend(crc250,"\n")
                   return
                end if
                if (mod_bdeb)write(*,*)myname,' System:',css%sys_val
                css%trg_minval(ii)=parse_evalf(plim,css%sys_val,crc250,irc)
                if (irc.ne.0) then
                   call model_errorappend(crc250,myname)
                   call model_errorappend(crc250," Error return from evalf.")
                   call model_errorappendi(crc250,irc)
                   call model_errorappend(crc250,"\n")
                   return
                end if
                css%trg_minset(ii)=.true.
                if (mod_bdeb) write(*,*)myname,'Minval:',css%trg_minval(ii),css%trg_minset(ii)
             else
                css%trg_minset(ii)=.false.
             end if
             !read (css%trg_min80(ii)(1:lens),*,iostat=irc2)
             css%trg_max80(ii)=currentTarget%max80
             call chop0(css%trg_max80(ii),80)
             lens=length(css%trg_max80(ii),80,10)
             if (lens.ne.0) then
                call parse_parsef(plim,css%trg_max80(ii)(1:lens),css%sys_var,crc250,irc)
                if (irc.ne.0) then
                   call model_errorappend(crc250,myname)
                   call model_errorappend(crc250," Error return from parsef.")
                   call model_errorappendi(crc250,irc)
                   call model_errorappend(crc250,"\n")
                   return
                end if
                if (mod_bdeb)write(*,*)myname,' System:',css%sys_val
                css%trg_maxval(ii)=parse_evalf(plim,css%sys_val,crc250,irc)
                if (irc.ne.0) then
                   call model_errorappend(crc250,myname)
                   call model_errorappend(crc250," Error return from evalf.")
                   call model_errorappendi(crc250,irc)
                   call model_errorappend(crc250,"\n")
                   return
                end if
                css%trg_maxset(ii)=.true.
                if (mod_bdeb) write(*,*)myname,'Maxval:',css%trg_maxval(ii),css%trg_maxset(ii)
             else
                css%trg_maxset(ii)=.false.
             end if
             ! read (css%trg_max80(ii)(1:lens),*,iostat=irc2)css%trg_maxval(ii)
             !css%trg_maxset(ii)=(irc2.eq.0)
             css%trg_sliceset(ii)=currentTarget%lslice
             css%trg_req(ii)=.false.
             css%trg_vok(ii)=.false.
             css%trg_val(ii)=0.0D0
             css%trg_ook(ii)=0
             css%trg_orm(ii)=0
             currentTarget => currentTarget%next
             css%trg_dim(ii)=0
             css%trg_var(ii)=0
          end do
          if (css%ind_set) then
             ii=min(css%ctrg,ii+1)
             css%trg80(ii)=css%ind_trg
             css%trg_v80(ii)=css%ind_var
             call chop0(css%trg80(ii),80)
             css%trg_lent(ii)=length(css%trg80(ii),80,10)
             call chop0(css%trg_v80(ii),80)
             css%trg_lenv(ii)=length(css%trg_v80(ii),80,10)
             if (mod_bdeb) write(*,'(2(X,A),X,I0,2(X,A))')myname,'Target:',II,&
                  & css%trg80(ii)(1:css%trg_lent(ii)),&
                  & css%trg_v80(ii)(1:css%trg_lenv(ii))
             !
             css%trg_valset(ii)=.true.
             css%trg_minset(ii)=css%ind_lval(1)
             css%trg_maxset(ii)=css%ind_lval(2)
             css%trg_minval(ii)=css%ind_minval
             css%trg_maxval(ii)=css%ind_maxval
             css%trg_req(ii)=.false.
             css%trg_vok(ii)=.false.
             css%trg_val(ii)=0.0D0
             css%trg_ook(ii)=0
             css%trg_orm(ii)=0
             css%trg_dim(ii)=0
             css%trg_var(ii)=0
          end if
       end if
       call parse_close(plim,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Error return from 'parse_close'.")
          return
       end if
       css%trg_set=.true.
   end if 
   if(mod_bdeb)write(*,*)myname,' Done.',irc,css%ctrg
   return
 end subroutine model_maketargetlist
 !
 ! set observation target names
 !
 subroutine model_setObsTrg(css,nn,var,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: nn
    character*80 :: var(nn)
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setObsTrg"
    integer, external :: length
    integer :: ii
    if (css%cobs.ne.nn) then
       if (associated(css%obs_var)) deallocate(css%obs_var)
       if (associated(css%obs_lenv)) deallocate(css%obs_lenv)
       if (associated(css%obs_req)) deallocate(css%obs_req)
       if (associated(css%obs_vok)) deallocate(css%obs_vok)
       if (associated(css%obs_val)) deallocate(css%obs_val)
    end if
    css%cobs=nn
    allocate(css%obs_var(css%cobs),css%obs_lenv(css%cobs),&
         & css%obs_req(css%cobs),css%obs_vok(css%cobs),&
         & css%obs_val(css%cobs),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate 'obs'.")
       call model_errorappend(crc250,"\n")
       return
    end if
    do ii=1,css%cobs
       css%obs_var(ii)=var(ii)
       call chop0(css%obs_var(ii),80)
       css%obs_lenv(ii)=length(css%obs_var(ii),80,10)
       css%obs_req(ii)=.false.
    end do
    return
  end subroutine model_setObsTrg
 subroutine model_getObsReq(css,nn,req,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: nn
    logical :: req(nn)
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setObsReq"
    integer, external :: length
    integer :: ii
    do ii=1,min(nn,css%cobs)
       if (css%obs_req(ii))req(ii)=.true.
    end do
    return
  end subroutine model_getObsReq
 !
 ! set observation values
 !
 subroutine model_setObsVal(css,nn,val,vok,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: nn
    real :: val(nn)
    logical :: vok(nn)
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setObsVal"
    integer :: ii
    if (css%cobs.ne.nn) then
       if (associated(css%obs_vok)) deallocate(css%obs_vok)
       if (associated(css%obs_val)) deallocate(css%obs_val)
       irc=844
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Invalid ntrg.")
       call model_errorappendi(crc250,nn)
       call model_errorappendi(crc250,css%cobs)
       call model_errorappend(crc250,"\n")
       return
    end if
    css%cobs=nn
    allocate(css%obs_val(css%cobs),css%obs_vok(css%cobs),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate 'obs'.")
       call model_errorappend(crc250,"\n")
       return
    end if
    do ii=1,css%cobs
       css%obs_val(ii)=val(ii)
       css%obs_vok(ii)=vok(ii)
    end do
    return
  end subroutine model_setObsVal
  !
  ! set location target value
  !
  subroutine model_setLocTrgVal(css,ninn,inn,ind,val,loc,itrg,crc250,irc)
    implicit none
    type(mod_session), pointer :: css  ! current session
    integer :: ninn                    ! number of search dimensions
    integer :: inn(ninn)               ! search dimension index
    integer :: ind(ninn)               ! target index
    real :: val                        ! output value
    type(mod_location), pointer :: loc ! current location
    integer :: itrg                    ! target position
    character*250 :: crc250            ! error message string
    integer :: irc                     ! error return code(0=ok)
    integer :: ii,jj
    character*25 :: myname="model_setLocTrgVal"
    if (itrg.le.0.or.itrg.gt.css%otrg) then
       irc=944
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Invalid itrg.")
       call model_errorappendi(crc250,itrg)
       call model_errorappendi(crc250,css%otrg)
       call model_errorappend(crc250,"\n")
       return
    end if
    loc%trg_vok(itrg)=.true.
    loc%trg_val(itrg)=val
    loc%trg_set(itrg)=.true.
    do jj=1,ninn
       if (ind(jj).gt.0.and.ind(jj).le.loc%ctrg) then
          loc%trg_vok(ind(jj))=.true.
          loc%trg_val(ind(jj))=loc%rpos(inn(jj))
          loc%trg_set(ind(jj))=.true.
       else if (ind(jj).ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Invalid index.")
          call model_errorappendi(crc250,ind(jj))
          call model_errorappendi(crc250,loc%ctrg)
          call model_errorappend(crc250,"\n")
          return
       end if
    end do
    !
    if (mod_bdeb)write(*,'(X,A,X,A,X,I0,X,I0,X,A,F0.1,X,L1)')myname,' Assigned:',loc%locid,itrg,&
         & "'"//css%trg80(itrg)(1:css%trg_lent(itrg))//"' = ",loc%trg_val(itrg),loc%trg_vok(itrg)

    return
  end subroutine model_setLocTrgVal
  !
 !
 ! set target values given match values
 !
 subroutine model_setTargetVal(css,nn,ind,val,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: nn
    integer :: ind(nn)
    real :: val(nn)
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setTargetVal"
    integer :: ii
    do ii=1,nn
       if (ind(ii).gt.css%ctrg) then
          irc=457
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Target-n mismatch .")
          call model_errorappendi(crc250,ii)
          call model_errorappendi(crc250,css%ctrg)
          call model_errorappendi(crc250,ind(ii))
          call model_errorappend(crc250,"\n")
          return
       end if
       if (css%trg_valset(ind(ii))) then
          css%trg_vok(ind(ii))=.true.
          css%trg_val(ind(ii))=val(ii)
       else
          css%trg_vok(ind(ii))=.false.
          css%trg_val(ind(ii))=0.0D0
       end if
    end do
    return
  end subroutine model_setTargetVal
 !
 ! set target values given match values
 !
 subroutine model_setTargetDVal(css,nn,vset,val,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: nn
    logical :: vset(nn)
    real :: val(nn)
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setTargetDVal"
    integer :: ii
    if(mod_bdeb)write(*,*)myname,' Entering.',nn,css%ctrg
    if (nn.ne.css%ctrg) then
       if(mod_bdeb)write(*,*)myname,' Target mismatch.',nn,css%ctrg
       irc=457
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Target-n mismatch .")
       call model_errorappendi(crc250,css%ctrg)
       call model_errorappendi(crc250,nn)
       call model_errorappend(crc250,"\n")
       return
    end if
    do ii=1,nn
       css%trg_valset(ii)=vset(ii)
       if (vset(ii)) then
          css%trg_vok(ii)=.true.
          css%trg_val(ii)=val(ii)
       else
          css%trg_vok(ii)=.false.
          css%trg_val(ii)=0.0D0
       end if
    end do
    if(mod_bdeb)write(*,*)myname,' Done.',nn,css%ctrg
    return
  end subroutine model_setTargetDVal
  !
  subroutine model_setTarget(css,nn,ind,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: nn                     ! number of indexes
    integer :: ind(nn)                ! target index
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setTarget"
    integer :: ii
    do ii=1,nn
       css%trg_valset(ind(ii))=.true. ! target is set in search
       css%trg_req(ind(ii))=.true.    ! target variable is required (can not be undefined)
    end do
    return
  end subroutine model_setTarget
  !
  subroutine model_checkTargetVal(css,locid,bok,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: locid
    logical :: bok
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_checkTargetVal"
    type(mod_location), pointer :: loc
    real :: val
    integer :: pos,ii
    if (bok.and.css%locReady) then
       pos=locid-css%locoffset
       if (pos.gt.css%nloc) then
          if(mod_bdeb)write(*,*)myname,'Location pos out of range.',pos,'->',css%nloc,&
               & locid,css%locoffset
          bok=.false.
          irc=945
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Locid out of range:")
          call model_errorappendi(crc250,locid)
          call model_errorappend(crc250,"<>")
          call model_errorappendi(crc250,css%nloc)
          call model_errorappendi(crc250,css%locoffset)
          call model_errorappend(crc250,"\n")
          return
       end if
       if (bok) then
          loc => css%locData(pos)%ptr
          if (.not.associated(loc)) then
             if(mod_bdeb)write(*,*)myname,'Location invalid location at ',pos,'->',css%nloc
             bok=.false.
          end if
       end if
       if (bok) then
          bok=(loc%bok)
          if (bok) then
             css%currentFile%ook(2)=css%currentFile%ook(2)+1
          else
             if (mod_bdeb)write(*,*)myname,'Loc fail:',loc%bok
             css%currentFile%orm(2)=css%currentFile%orm(2)+1 ! location error
          end if
       end if
       do ii=1,loc%ctrg
          if (bok) then
             if (loc%trg_set(ii)) then
                if (css%trg_minset(ii)) then
                   if (loc%trg_val(ii).lt.css%trg_minval(ii)) bok=.false.
                end if
                if (css%trg_maxset(ii)) then
                   if (loc%trg_val(ii).gt.css%trg_maxval(ii)) bok=.false.
                end if
                if (bok) then
                   css%trg_ook(ii)=css%trg_ook(ii)+1
                else
                   css%trg_orm(ii)=css%trg_orm(ii)+1
                end if
             end if
             if (.not.bok.and.mod_bdeb)write(*,*)myname,'Rejected:',&
                  & ii,css%trg80(ii)(1:css%trg_lent(ii)), &
                  & loc%trg_val(ii),css%trg_minval(ii),css%trg_maxval(ii),&
                  & loc%trg_set(ii),css%trg_minset(ii),css%trg_maxset(ii)
          end if
       end do
       if (bok) then
          css%trg_ook(0)=css%trg_ook(0)+1
       else
          css%trg_orm(0)=css%trg_orm(0)+1
       end if
    end if
    return
  end subroutine model_checkTargetVal
  !
  ! delete target from stack
  !
  subroutine model_deleteTarget (css,trg, crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    type(mod_target), pointer :: trg
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    character*25 :: myname="model_deleteTarget"
    if (associated(trg)) then
       css%ntrg = css%ntrg - 1
       trg%next%prev => trg%prev
       trg%prev%next => trg%next
       deallocate(trg)
    end if
    css%trg_set=.false.
    return
  end subroutine model_deleteTarget
  !
  ! Set the target index in a file
  subroutine model_setTargetIndex(css,file,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    type(mod_file), pointer :: file
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    type(mod_variable), pointer :: var
    character*25 :: myname="model_setTargetIndex"
    integer, external :: length
    integer :: lend, leng, lenv
    integer ii,jj
    if(mod_bdeb)write(*,*)myname,' Entering.',file%nvar
    if (mod_bdeb) then
       do ii=1, css%ctrg
          write(*,*)myname," Target ",ii," '"//css%trg80(ii)(1:css%trg_lent(ii)),&
               & "' -> '"//css%trg_v80(ii)(1:css%trg_lenv(ii))//"'"
       end do
    end if
    if(mod_bdeb)write(*,*)myname,' Setting dims.',file%ndim
    do jj=1,file%ndim
       file%dim_trg(jj)=0
    end do
    if(mod_bdeb)write(*,*)myname,' Looping targets.',css%ctrg
    do ii=1,css%ctrg
       css%trg_var(ii)=0 ! global variable index
       css%trg_dim(ii)=0 ! global dimension index
       leng=css%trg_lenv(ii)
       if(mod_bdeb)write(*,*)myname," Checking for dimension '"//css%trg_v80(ii)(1:leng)//"'",file%ndim
       if (leng.gt.2) then
          if (css%trg_v80(ii)(1:1).eq."(".and.css%trg_v80(ii)(leng:leng).eq.")") then
             do jj=1,file%ndim
                lend=length(file%dim80(jj),80,10)
                if (css%trg_v80(ii)(2:leng-1).eq.file%dim80(jj)(1:lend)) then
                   
                   if(mod_bdeb)write(*,*) myname,'Dim: "'//css%trg_v80(ii)(2:leng-1)//&
                        & '"  "'//file%dim80(jj)(1:lend)//'"',ii,jj

                   css%trg_dim(ii)=jj ! global dimension index
                   file%dim_trg(jj)=ii
                end if
             end do
          end if
       end if
       if(mod_bdeb)write(*,*)myname," Checking for variable '"//css%trg_v80(ii)(1:leng)//"'",&
            & file%nvar,size(file%var80)
       if (css%trg_dim(ii).eq.0) then ! not a dimension, must be a variable
          do jj=1,file%nvar ! global variable index
             lenv=length(file%var80(jj),80,10)
             if(mod_bdeb)write(*,*)myname," Variable '"//file%var80(jj)(1:lenv)//"'",jj
             if (css%trg_v80(ii)(1:leng).eq.file%var80(jj)(1:lenv)) then
                css%trg_var(ii)=jj
                var => file%var(jj)%ptr
                var%itrg=ii
                if(mod_bdeb)write(*,'(X,A,A,I3,A,I3,A)') myname,&
                     & 'Target variable: ',&
                     & ii,' -> ',jj,&
                     & "  '"//css%trg_v80(ii)(1:leng)//"'"
             end if
          end do
       end if
       if (css%trg_dim(ii).eq.0.and.css%trg_var(ii).eq.0) then ! not dimension nor variable...
          if(mod_bdeb)write(*,*) myname,'Unrecognised target ignored:',&
               & css%trg_v80(ii)(1:leng)
       end if
    end do
    !
    if(mod_bdeb)write(*,*)myname,' Done.',irc
    return
  end subroutine model_setTargetIndex
  !
  ! Set the target index for a variable
  integer function model_getTargetIndex(css,var)
    implicit none
    type(mod_session), pointer :: css !  current session
    type(mod_variable), pointer :: var
    character*25 :: myname="model_getTargetIndex"
    integer :: ii
    if(mod_bdeb)write(*,*)myname,' Entering.',var%var80(1:var%lenv)
    LOOP: do ii = 1, css%ctrg
       if (var%var80(1:var%lenv).eq.css%trg_v80(ii)(1:css%trg_lenv(ii))) then
          if (mod_bdeb) then
             write(*,*) myname," Found target for: '"//var%var80(1:var%lenv)//&
                  & "' <- '"//css%trg80(ii)(1:css%trg_lent(ii))//"'",ii
          end if
           model_getTargetIndex=ii
           return
       end if
    end do LOOP
    if (mod_bdeb) then
       write(*,*) myname," No target for:"//var%var80(1:var%lenv)
    end if
    model_getTargetIndex=0
    if(mod_bdeb)write(*,*)myname,' Done.'
    return
  end function model_getTargetIndex
  !
  !###############################################################################
  ! LOCATION ROUTINES
  !###############################################################################
  ! initialise the MODEL location
  !
  subroutine model_initLocStack(css,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_initLocStack"
    ! initialise chain
    if (.not.associated(css%firstLoc)) then
       allocate(css%firstLoc,css%lastLoc, stat=irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Unable to allocate 'firstLoc/lastLoc'.")
          call model_errorappend(crc250,"\n")
          return
       end if
       css%firstLoc%next => css%lastLoc
       css%lastLoc%prev => css%firstLoc
       css%nloc=0
       css%locReady=.false.
       if(mod_bdeb)write(*,*)myname,' WARNING: locready=F'
    end if
  end subroutine model_initLocStack
  !
  ! clear the MODEL POS
  !
  subroutine model_sliceVariables(css,nslice,slice80,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: nslice
    character*80 :: slice80(nslice)
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_sliceVariables"
    integer :: ii, lens
    integer, external :: length
    if(mod_bdeb)write(*,*)myname,' Entering.'
    ! store slice variables/dimensions
    css%csli=0
    do ii=1,nslice
       call chop0(slice80(ii),80)
       lens=length(slice80(ii),80,10)
       if(mod_bdeb)write(*,*)myname,'Slice variable:', ii,slice80(ii)(1:lens)
       if (lens.ne.0) then
          css%csli=min(nslice,css%csli+1)
       end if
    end do
    if (allocated(css%sli_v80)) deallocate(css%sli_v80)
    if (allocated(css%sli_lenv)) deallocate(css%sli_lenv)
    if (allocated(css%sli_2trg)) deallocate(css%sli_2trg)
    allocate(css%sli_v80(css%csli),css%sli_lenv(css%csli),&
         & css%sli_2trg(css%csli),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate 'gslice'.")
       call model_errorappendi(crc250,css%csli)
       call model_errorappend(crc250,"\n")
       return
    end if
    css%csli=0
    do ii=1,nslice
       lens=length(slice80(ii),80,10)
       if(mod_bdeb)write(*,*)myname,' **** Slice variable:', ii,slice80(ii)(1:lens)
       if (lens.ne.0) then
          css%csli=min(nslice,css%csli+1)
          css%sli_v80(css%csli)=slice80(ii)
          css%sli_lenv(css%csli)=lens
          css%sli_2trg(css%csli)=ii
       end if
    end do
    if(mod_bdeb)write(*,*)myname,' Done.',css%csli
  end subroutine model_sliceVariables
  !
  subroutine model_setSliceIndex(css,nslice,ind,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: nslice
    integer :: ind(nslice)
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setSliceIndex"
    integer :: ii
    if(mod_bdeb)write(*,*)myname,' Entering.',nslice
    ! store slice variables/dimensions
    css%csli=nslice
    if (allocated(css%sli_v80)) deallocate(css%sli_v80)
    if (allocated(css%sli_lenv)) deallocate(css%sli_lenv)
    if (allocated(css%sli_2trg)) deallocate(css%sli_2trg)
    allocate(css%sli_v80(css%csli),css%sli_lenv(css%csli),&
         & css%sli_2trg(css%csli),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate 'gslice'.")
       call model_errorappendi(crc250,css%csli)
       call model_errorappend(crc250,"\n")
       return
    end if
    do ii=1,nslice
       css%sli_2trg(ii)=ind(ii)
       css%sli_v80(ii)=css%trg_v80(ind(ii))
       css%sli_lenv(ii)=css%trg_lenv(ind(ii))
       if (mod_bdeb) write(*,'(X,A,X,A,X,I0,A,I0,X,A)') myname,'Slice index:',ii," -> ",ind(ii), &
            & "'"//css%trg_v80(ind(ii))(1:css%trg_lenv(ind(ii)))//"'"
    end do
    if(mod_bdeb)write(*,*)myname,' Done.',css%csli
    return
  end subroutine model_setSliceIndex
  !
  ! Use marked target variables as slice variables
  subroutine model_sliceTrgVal(css,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_sliceTrgVal"
    integer :: ii, jj, lens
    integer, external :: length
    if(mod_bdeb)write(*,*)myname,' Entering.'
    ! store slice variables/dimensions
    css%csli=0
    if (allocated(css%sli_v80)) deallocate(css%sli_v80)
    if (allocated(css%sli_lenv)) deallocate(css%sli_lenv)
    if (allocated(css%sli_2trg)) deallocate(css%sli_2trg)
    ! count number of slice variables (for allocation)
    do ii=1,css%ctrg
       if (css%trg_sliceset(ii).and.css%trg_valset(ii)) then
          css%csli=css%csli+1
          if (mod_bdeb) then
             write(*,*)myname,' *** Slice variable: ',&
                  & css%trg_v80(ii)(1:css%trg_lenv(ii)),css%csli
          end if
       else
          if (mod_bdeb) then
             write(*,*)myname,' *** Not sliced    : ',&
                  & css%trg_v80(ii)(1:css%trg_lenv(ii)),&
                  & css%trg_sliceset(ii),css%trg_valset(ii)
          end if
       end if
    end do
    allocate(css%sli_v80(css%csli),css%sli_lenv(css%csli),&
         & css%sli_2trg(css%csli),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate 'gslice'.")
       call model_errorappendi(crc250,css%csli)
       call model_errorappend(crc250,"\n")
       return
    end if
    css%csli=0
    do ii=1,css%ctrg
       if (css%trg_sliceset(ii).and.css%trg_valset(ii)) then
          css%csli=css%csli+1
          css%sli_v80(css%csli)=css%trg_v80(ii)
          css%sli_lenv(css%csli)=css%trg_lenv(ii)
          css%sli_2trg(css%csli)=ii
          if (mod_bdeb) write(*,*) myname,'Slice index:',css%csli,ii
       end if
    end do
    if(mod_bdeb)write(*,*)myname,' Done.',css%csli
  end subroutine model_sliceTrgVal
  !
  subroutine model_clearLocStack(css,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(mod_location), pointer :: currentLoc => null()
    type(mod_location), pointer :: locNext => null()
    character*25 :: myname="model_clearLocStack"
    integer :: ii, lens
    integer, external :: length
    if(mod_bdeb)write(*,*)myname,' Entering.'
    call model_initLocStack(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from locinit.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    ! delete any existing location-entries
    currentLoc => css%firstLoc%next
    do while (.not.associated(currentLoc,target=css%lastLoc))
       locNext => currentLoc%next
       call model_deleteLoc(css,currentLoc,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from locrmitem.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       currentLoc => locNext
    end do
    if (css%nloc .ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," System error:")
       call model_errorappendi(crc250,css%nloc)
       call model_errorappend(crc250,"\n")
       irc=940
       return
    end if
    if(mod_bdeb)write(*,*)myname,' Done.'
  end subroutine model_clearLocStack
  !
  ! Add a "location", specified by slice variables...
  !
  subroutine model_locpushVariables(css,locid,csli,sli_val,bok,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: locid
    integer :: csli
    real:: sli_val(csli)
    logical :: bok
    character*250 :: crc250
    integer :: irc
    type(mod_location),pointer :: newLoc
    character*80 :: var80
    integer :: ii,yy,mm,dd,hh,mi
    real:: sec
    integer :: lenc
    integer, external :: length
    character*25 :: myname="model_locpushvariables"
    if(mod_bdeb)write(*,*)myname,' Entering. Loc%bok=',bok
    if(mod_bdeb)write(*,*)myname,' data.',csli,css%csli
    ! check number of slice-variables
    if(mod_bdeb)write(*,*)myname,'Slices:',csli,css%csli
    do ii=1,csli
       if(mod_bdeb)write(*,*) myname,'Slice:',ii,sli_val(ii)
    end do
    if (csli.ne.css%csli) then
       irc=346
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,":Location:")
       call model_errorappendi(crc250,locid)
       call model_errorappend(crc250,":Wrong number of slice-variables, expected:")
       call model_errorappendi(crc250,css%csli)
       call model_errorappend(crc250," got:")
       call model_errorappendi(crc250,csli)
       call model_errorappend(crc250,"\n")
       return
    end if
    ! initialise location stack
    if (css%nloc.eq.0) then
       css%locoffset=locid-1
    end if
    !
    call model_initLocStack(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from locinit.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    ! create new location-item
    allocate(newLoc,stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Unable to allocate new location.")
       call model_errorappend(crc250,"\n")
       return
    end if
    newLoc%csli=csli
    allocate(newLoc%sli_val(newLoc%csli),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Unable to allocate newLoc%sli_val.")
       call model_errorappendi(crc250,newLoc%csli)
       call model_errorappend(crc250,"\n")
       return
    end if
    newLoc%locid=locid
    do ii=1,newLoc%csli
       if(mod_bdeb)write(*,*)myname,'Slice target:',ii,sli_val(ii)
       newLoc%sli_val(ii)=sli_val(ii)
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
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Non-sequential locid:")
       call model_errorappendi(crc250,locid)
       call model_errorappend(crc250,"<>")
       call model_errorappendi(crc250,css%nloc+css%locoffset)
       call model_errorappend(crc250,"\n")
       return
    end if
    if(mod_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine model_locpushVariables
  !
  ! Add a "location", specified by slice variables...
  !
  subroutine model_locpushTarget(css,locid,bok,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: locid
    logical :: bok
    character*250 :: crc250
    integer :: irc
    type(mod_location),pointer :: newLoc
    character*80 :: var80
    integer :: ii,jj,yy,mm,dd,hh,mi
    real:: sec
    integer :: lenc
    integer, external :: length
    character*25 :: myname="model_locpushTarget"
    if(mod_bdeb.and.locid.lt.10)write(*,*)myname,' Entering, csli:',&
         & css%csli," Loc%bok=",bok,css%sli_2trg
    ! initialise location stack
    if (css%nloc.eq.0) then
       css%locoffset=locid-1
    end if
    !
    call model_initLocStack(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from locinit.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    ! create new location-item
    allocate(newLoc,stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Unable to allocate new location.")
       call model_errorappend(crc250,"\n")
       return
    end if
    newLoc%locid=locid
    newLoc%bok=bok
    newLoc%csli=css%csli
    allocate(newLoc%sli_val(newLoc%csli),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Unable to allocate newLoc%sli_val.")
       call model_errorappendi(crc250,newLoc%csli)
       call model_errorappend(crc250,"\n")
       return
    end if
    do ii=1,newLoc%csli
       if (css%sli_2trg(ii).le.0) then
          if (mod_bdeb) then
             write(*,*)myname,'Invalid index:',css%csli,ii
             do jj=1,css%csli
                write(*,*)myname,'   index:',jj,css%sli_2trg(jj)
             end do
          end if
          irc=344
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Invalid index.")
          call model_errorappendi(crc250,css%sli_2trg(ii))
          call model_errorappend(crc250,"\n")
          return
       end if
       if(mod_bdeb.and.locid.lt.10)write(*,*)myname,'Slice target:',&
            & ii,css%trg_val(css%sli_2trg(ii)),css%sli_2trg(ii)
       newLoc%sli_val(ii)=css%trg_val(css%sli_2trg(ii))
    end do
    newLoc%cobs=css%cobs
    allocate(newLoc%obs_val(newLoc%cobs),newLoc%obs_vok(newLoc%cobs),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Unable to allocate newLoc%obs_val.")
       call model_errorappendi(crc250,newLoc%cobs)
       call model_errorappend(crc250,"\n")
       return
    end if
    do ii=1,newLoc%cobs
       if(mod_bdeb.and.locid.lt.10)write(*,*)myname,'Obs target:',ii,css%obs_val(ii)
       newLoc%obs_val(ii)=css%obs_val(ii)
       newLoc%obs_vok(ii)=css%obs_vok(ii)
    end do
    newLoc%ctrg=css%ctrg
    allocate(newLoc%trg_val(newLoc%ctrg),&
         & newLoc%trg_vok(newLoc%ctrg),&
         & newLoc%trg_set(newLoc%ctrg),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Unable to allocate newLoc%trg_val.")
       call model_errorappendi(crc250,newLoc%csli)
       call model_errorappend(crc250,"\n")
       return
    end if
    do ii=1,newLoc%ctrg
       newLoc%trg_val(ii)=0.0D0
       newLoc%trg_set(ii)=.false.
    end do
    ! push onto stack
    css%nloc=css%nloc + 1
    newLoc%prev => css%lastLoc%prev
    newLoc%next => css%lastLoc
    newLoc%prev%next => newLoc
    newLoc%next%prev => newLoc
    css%locReady=.false.
    if (css%nloc+css%locoffset .ne. locid) then
       irc=346
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Non-sequential locid:")
       call model_errorappendi(crc250,locid)
       call model_errorappend(crc250,"<>")
       call model_errorappendi(crc250,css%nloc+css%locoffset)
       call model_errorappend(crc250,"\n")
       return
    end if
    if(mod_bdeb.and.locid.lt.10)write(*,*)myname,' Done.'
    return
  end subroutine model_locpushTarget
  !
  subroutine model_checkFilter(css,locid,bok,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: locid
    logical :: bok
    character*250 :: crc250
    integer :: irc
    integer :: pos, ii
    character*25 :: myname="model_checkFilter"
    type(mod_location), pointer :: loc
    real :: val
    if (bok.and.css%locReady) then
       css%currentFile%ook(1)=css%currentFile%ook(1)+1
       pos=locid-css%locoffset
       if (pos.gt.css%nloc) then
          if(mod_bdeb)write(*,*)myname,'Location pos out of range.',pos,'->',css%nloc,&
               & locid,css%locoffset
          bok=.false.
          irc=944
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Locid out of range:")
          call model_errorappendi(crc250,locid)
          call model_errorappend(crc250,"<>")
          call model_errorappendi(crc250,css%nloc)
          call model_errorappendi(crc250,css%locoffset)
          call model_errorappend(crc250,"\n")
          return
       end if
       if (bok) then
          loc => css%locData(pos)%ptr
          if (.not.associated(loc)) then
             if(mod_bdeb)write(*,*)myname,'Location invalid location at ',pos,'->',css%nloc
             bok=.false.
          end if
       end if
       if (bok) then
          bok=(loc%bok)
          if (bok) then
             css%currentFile%ook(2)=css%currentFile%ook(2)+1
          else
             if (mod_bdeb)write(*,*)myname,'Loc fail:',loc%bok
             css%currentFile%orm(2)=css%currentFile%orm(2)+1 ! location error
          end if
       end if
       if (bok) then
          bok=(loc%search.eq.0)
          if (bok) then
             css%currentFile%ook(3)=css%currentFile%ook(3)+1
          else
             if (mod_bdeb)write(*,*)myname,'Search fail:',loc%search
             css%currentFile%orm(3)=css%currentFile%orm(3)+1 ! search failed
          end if
       end if
       if (bok) then
          ! evaluate filter
          if (css%mpo_set) then
             if (mod_bdeb)write(*,*)myname,'Evaluating filter:',ii,&
                  & associated(css%psf),css%cmpo
             do ii=1,loc%ctrg
                css%mpo_val(ii)=loc%trg_val(ii)
                css%mpo_vok(ii)=loc%trg_vok(ii)
             end do
             do ii=1,loc%cobs
                css%mpo_val(ii+css%ctrg)=loc%obs_val(ii)
                css%mpo_vok(ii+css%ctrg)=loc%obs_vok(ii)
             end do
             if (mod_bdeb)write(*,*)myname,' MPO:',css%mpo_val
             val=parse_evalf(css%psf,css%mpo_val,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from evalf.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
             bok=(nint(val).ne.0) ! NB bok is local, reject obs using trg_set->.false.
             if (mod_bdeb)write(*,*)myname,'Returned:',val,bok
          else
             if (mod_bdeb)write(*,*)myname,'MPO fail:',css%mpo_set
             bok=.false.
          end if
          if (bok) then
             css%currentFile%ook(4)=css%currentFile%ook(4)+1
          else
             if (mod_bdeb)write(*,*)myname,'Filter fail:',loc%bok
             css%currentFile%orm(4)=css%currentFile%orm(4)+1 ! filter failed
          end if
       end if
    else
       if (mod_bdeb)write(*,*)myname,'Early fail, locready=',css%locReady
       css%currentFile%orm(1)=css%currentFile%orm(1)+1 ! failed earlier or no locations
       bok=.false.
    end if
    if (bok) then
       if (mod_bdeb)write(*,*)myname,'Obs OK:',locid,bok
    else
       if (mod_bdeb)write(*,*)myname,'Obs FAIL:',locid,bok,css%mpo_set
    end if
    return
  end subroutine model_checkFilter
  !
  subroutine model_setfilter(css,flt,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    character*(*) :: flt
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    character*22 :: myname="model_setfilter"
    !if(mod_bdeb)write(*,*)myname,' Entering.',irc
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       css%flt250=trim(flt)
       call chop0(css%flt250,250)
       css%lenf=length(css%flt250,250,10)
       if(mod_bdeb)write(*,*)myname,"Filter:'"//css%flt250(1:css%lenf)//"'",irc
 end if
    !
  end subroutine model_setfilter
  !
  subroutine model_compileFilter(css,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_compileFilter"
    integer :: ii
    if (css%lenf.ne.0) then
       call parse_open(css%psf,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Error return from 'parse_open'.")
          return
       end if
       call model_setMPO(css,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Error return from 'setMPO'.")
          return
       end if
       call parse_parsef(css%psf,css%flt250(1:css%lenf),css%mpo_var,crc250,irc)
       if (irc.ne.0) then
          if(mod_bdeb)then
             write(*,*)myname,"Unable to parse:'"//css%flt250(1:css%lenf)//"'"
             ! write(*,*)myname,'nvar:',css%ntrg
             ! do jj=1,css%ntrg
             ! write(*,*) myname,'var:',jj,css%mpo_var(jj)(1:css%mpo_lenv(jj))
             ! end do
          end if
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from parsef.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       call parse_used(css%psf,css%mpo_req)
       call model_getMPO(css,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from getMPO.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    return
  end subroutine model_compileFilter
  !
  ! compile expressions
  !
  subroutine model_compileExpr(css,nexp,exp250,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    integer :: nexp
    character*250,allocatable :: exp250(:)
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_compileExpr"
    integer :: ii,lene
    integer, external :: length
    if(mod_bdeb)write(*,*)myname,' Entering.',irc,nexp,size(exp250)
    call model_setMPO(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Error return from 'setMPO'.")
       return
    end if
    if(mod_bdeb)write(*,*)myname,'InitPSP.'
    call model_initPSP(css,nexp,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Error return from 'initPSP'.")
       return
    end if
    if(mod_bdeb)write(*,*)myname,'Checking.',associated(css),allocated(exp250)
    if(mod_bdeb)write(*,*)myname,'Checking.',css%cpsp,size(exp250)
    if (css%cpsp > size(exp250)) then
       irc=944
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"System error.")
       call model_errorappendi(crc250,nexp)
       call model_errorappendi(crc250,css%cpsp)
       call model_errorappendi(crc250,size(exp250))
       return
    end if
    if(mod_bdeb)write(*,*)myname,'Parse.',css%cpsp,size(css%mpo_var)
    do ii=1,css%cpsp
       lene=length(exp250(ii),250,10)
       if(mod_bdeb)write(*,*)myname,'Parse:',ii,exp250(ii)(1:lene)
       call parse_parsef(css%psp(ii)%ptr,exp250(ii)(1:lene),css%mpo_var,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,"parse_parsef")
          return
       end if
       call parse_used(css%psp(ii)%ptr,css%mpo_req)
    end do
    call model_getMPO(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from getMPO.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    if(mod_bdeb)write(*,*)myname,' Done.',irc
    return
  end subroutine model_compileExpr
  !
  ! evaluate expressions
  !
  subroutine model_evalExpr(css,nexp,val,set,crc250,irc)
    use parse
    implicit none
    type(mod_session), pointer :: css !  current session
    integer :: nexp
    real :: val(nexp)
    logical :: set(nexp)
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_evalExpr"
    integer :: ii
    do ii=1,css%cpsp
       call parse_evals(css%psp(ii)%ptr,css%mpo_val,css%mpo_vok,val(ii),set(ii),crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from evals.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end do
    return
  end subroutine model_evalExpr
  !
  ! initialise expression-parsing array (PSP)
  !
  subroutine model_initPSP(css,nexp,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    integer :: nexp
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_initPSP"
    integer :: ii
    call model_clearPSP(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,"clearPSP.")
       return
    end if
    css%cpsp=nexp
    allocate(css%psp(css%cpsp),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,"allocate PSP error.")
       return
    end if
    do ii=1,css%cpsp
       call parse_open (css%psp(ii)%ptr,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,"parse_open")
          return
       end if
    end do
  end subroutine model_initPSP
  !
  ! clear PSP array
  !
  subroutine model_clearPSP(css,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_clearPSP"
    integer :: ii
    if (associated(css%psp)) then
       do ii=1,css%cpsp
          if (associated(css%psp(ii)%ptr)) then
             call parse_close (css%psp(ii)%ptr,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,"parse_close")
                return
             end if
          end if
       end do
       deallocate(css%psp)
    end if
  end subroutine model_clearPSP
  !
  ! set Model- + Observation- variables (used for parsing)
  !
  subroutine model_setMPO(css,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_setMPO"
    integer :: ii
    if (css%mpo_set) return ! filter-variables is already set
    css%cmpo=css%ctrg+css%cobs
    if(mod_bdeb)write(*,*)myname,"Parsing: '"//css%flt250(1:css%lenf)//"'",&
         & css%cmpo,css%ctrg,css%cobs
    if (allocated(css%mpo_var)) deallocate(css%mpo_var)
    if (allocated(css%mpo_lenv)) deallocate(css%mpo_lenv)
    if (allocated(css%mpo_req)) deallocate(css%mpo_req)
    if (allocated(css%mpo_vok)) deallocate(css%mpo_vok)
    if (allocated(css%mpo_val)) deallocate(css%mpo_val)
    allocate(css%mpo_var(css%cmpo),css%mpo_lenv(css%cmpo),&
         & css%mpo_req(css%cmpo),css%mpo_vok(css%cmpo),&
         & css%mpo_val(css%cmpo),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate 'filter'.")
       call model_errorappend(crc250,"\n")
       return
    end if
    do ii=1,css%ctrg
       css%mpo_var(ii)=css%trg80(ii)(1:css%trg_lent(ii))
       css%mpo_lenv(ii)=css%trg_lent(ii)
       css%mpo_req(ii)=.false.
       css%mpo_vok(ii)=.false.
    end do
    do ii=1,css%cobs
       css%mpo_var(ii+css%ctrg)=css%obs_var(ii)(1:css%obs_lenv(ii))
       css%mpo_lenv(ii+css%ctrg)=css%obs_lenv(ii)
       css%mpo_req(ii+css%ctrg)=.false.
       css%mpo_vok(ii+css%ctrg)=.false.
    end do
    css%mpo_set=.true.
    if(mod_bdeb)write(*,*)myname,"Done.",css%ctrg,css%cobs,css%cmpo
    return
  end subroutine model_setMPO
  !
  subroutine model_getMPO(css,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_getMPO"
    integer :: ii
    do ii=1,css%ctrg
       if (css%mpo_req(ii)) then
          !write(*,'(X,A,X,A,X,I0,A)')myname,&
          !     & "Used: trg_mod(",ii,")->'"//css%trg80(ii)(1:css%trg_lent(ii))//"'"
          css%trg_req(ii)=.true.
       end if
    end do
    do ii=1,css%cobs
       if (css%mpo_req(ii+css%ctrg)) then
          !write(*,'(X,A,X,A,X,I0,A)')myname,&
          !     & "Used: trg_obs(",ii,")->'"//css%obs_var(ii)(1:css%obs_lenv(ii))//"'"
          css%obs_req(ii)=.true.
       end if
    end do
    if(mod_bdeb)write(*,*)myname,"Done.",css%ctrg,css%cobs,css%cmpo
    return
  end subroutine model_getMPO
  !
  subroutine model_getfilter(css,flt,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    character*(*) :: flt
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_getfilter"
    if(mod_bdeb)write(*,*)myname,' Entering.',irc
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       flt=css%flt250(1:css%lenf)
       if(mod_bdeb)write(*,*)myname,"Filter:'"//css%flt250(1:css%lenf)//"'",irc
    end if
    !if(mod_bdeb)write(*,*)myname,'Exiting.',irc
  end subroutine model_getfilter
  !
  ! Get arrays from MODEL POS
  !
  subroutine model_locprint(css,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(mod_location), pointer :: currentLoc => null()
    integer, external :: length
    integer :: lenn,lenb,leng,ii
    integer :: cnt=0
    character*250 :: buff250
    character*50 :: num50
    character*25 :: myname="model_locprint"
    if(mod_bdeb)write(*,*)myname,' Entering.'
    if (associated(css%firstLoc).and.css%nloc.gt.0) then
       currentLoc => css%firstLoc%next
       do while (.not.associated(currentLoc,target=css%lastLoc))
          cnt=cnt+1
          buff250=""
          lenb=0
          do ii=1,currentLoc%csli
             write(num50,*)currentLoc%sli_val(ii)
             call chop0(num50,50)
             lenn=length(num50,50,10)
             leng=css%sli_lenv(ii)
             buff250=buff250(1:lenb)//" "//css%sli_v80(ii)(1:leng)//"="//num50(1:lenn)
             call chop0(buff250,250)
             lenb=length(buff250,250,10)
          end do
          write(*,'(X,A,I0,X,A)') &
               & myname, &
               & currentLoc%locid, &
               & buff250(1:lenb)
          currentLoc => currentLoc%next
       end do
    end if
    if(mod_bdeb)write(*,*)myname,' Number of locations:',cnt
    if(mod_bdeb)write(*,*)myname,' Done.'
  end subroutine model_locprint
  !
  ! delete loc froms stack
  !
  subroutine model_deleteLoc (css,loc, crc250,irc)
    type(mod_session), pointer :: css !  current session
    type(mod_location), pointer :: loc
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    character*25 :: myname="model_deleteLoc"
    if (associated(loc)) then
       css%nloc = css%nloc - 1
       loc%next%prev => loc%prev
       loc%prev%next => loc%next
       if (allocated(loc%sli_val))  deallocate(loc%sli_val)
       if (allocated(loc%trg_val))  deallocate(loc%trg_val)
       if (allocated(loc%trg_vok))  deallocate(loc%trg_vok)
       if (allocated(loc%trg_set)) deallocate(loc%trg_set)
       if (allocated(loc%obs_val))  deallocate(loc%obs_val)
       if (allocated(loc%obs_vok))  deallocate(loc%obs_vok)
       if (allocated(loc%pos))      deallocate(loc%pos)
       if (allocated(loc%rpos))     deallocate(loc%rpos)
       if (allocated(loc%lstart))   deallocate(loc%lstart)
       if (allocated(loc%lstop))    deallocate(loc%lstop)
       if (allocated(loc%intpf))    deallocate(loc%intpf)
       deallocate(loc)
    end if
    return
  end subroutine model_deleteLoc
  !
  subroutine model_makeLocList(css,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(mod_location), pointer :: currentLoc => null()
    character*25 :: myname="model_makeLocList"
    integer :: ii
    if(mod_bdeb)write(*,*)myname,' Entering:',irc
    if (associated(css%firstLoc).and..not.css%locready.and.css%nloc.gt.0) then
       if (allocated(css%locdata)) deallocate(css%locdata)
       allocate(css%locdata(css%nloc),stat=irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Unable to allocate locid,lat,lon.")
          call model_errorappendi(crc250,css%nloc)
          call model_errorappend(crc250," , ")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
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
    if(mod_bdeb)write(*,*)myname,' Done:',css%locReady,irc
    return
  end subroutine model_makeLocList
  !
  !###############################################################################
  ! OUTPUT ROUTINES
  !###############################################################################
  ! allocate output
  !
  subroutine model_allocateTable(css,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250  ! error message string
    integer :: irc        ! error return code(0=ok)
    integer :: ii,jj
    character*25 :: myname="model_allocateTable"
    ! remove output variables
    if (associated(css%oval)) deallocate(css%oval)
    if (associated(css%oset)) deallocate(css%oset)
    css%oloc=css%nloc
    css%otrg=css%ctrg
    allocate(css%oval(css%otrg,css%oloc),css%oset(css%otrg,css%oloc),stat=irc)
    do ii=1,css%oloc
       do jj=1,css%otrg
          css%oset(jj,ii)=.false.
       end do
    end do
    return
  end subroutine model_allocateTable
  !
  subroutine model_deallocateTable(css,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    character*250 :: crc250  ! error message string
    integer :: irc        ! error return code(0=ok)
    integer :: ii,jj
    character*25 :: myname="model_allocateTable"
    ! remove output variables
    if (associated(css%oval)) deallocate(css%oval)
    if (associated(css%oset)) deallocate(css%oset)
    css%oloc=0
    css%otrg=0
    return
  end subroutine model_deallocateTable
  !
  subroutine model_setOutVal(css,val,itrg,iloc,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    real :: val           ! output value
    integer :: itrg       ! output target position
    integer :: iloc       ! output location position
    character*250 :: crc250  ! error message string
    integer :: irc        ! error return code(0=ok)
    integer :: ii,jj
    character*25 :: myname="model_setOutVal"
    if (.not.associated(css%oval).or..not.associated(css%oset)) then
       irc=946
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Output not allocated.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    if (iloc.le.0.or.iloc.gt.css%oloc) then
       irc=945
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Invalid iloc.")
       call model_errorappendi(crc250,iloc)
       call model_errorappendi(crc250,css%oloc)
       call model_errorappend(crc250,"\n")
       return
    end if
    if (itrg.le.0.or.itrg.gt.css%otrg) then
       irc=944
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Invalid itrg.")
       call model_errorappendi(crc250,itrg)
       call model_errorappendi(crc250,css%otrg)
       call model_errorappend(crc250,"\n")
       return
    end if
    css%oval(itrg,iloc)=val
    css%oset(itrg,iloc)=.true.
    return
  end subroutine model_setOutVal
  !
  logical function model_getOutput(css,val,itrg,iloc,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    real :: val           ! output value
    integer :: itrg       ! output target position
    integer :: iloc       ! output location position
    character*250 :: crc250  ! error message string
    integer :: irc        ! error return code(0=ok)
    integer :: ii,jj
    character*25 :: myname="model_getOutput"
    model_getOutput=.false.
    if (.not.associated(css%oval).or..not.associated(css%oset)) then
       irc=943
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Output not allocated.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    if (iloc.le.0.or.iloc.gt.css%oloc) then
       irc=942
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Invalid iloc.")
       call model_errorappendi(crc250,iloc)
       call model_errorappendi(crc250,css%oloc)
       call model_errorappend(crc250,"\n")
       return
    end if
    if (itrg.le.0.or.itrg.gt.css%otrg) then
       irc=941
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Invalid itrg.")
       call model_errorappendi(crc250,itrg)
       call model_errorappendi(crc250,css%otrg)
       call model_errorappend(crc250,"\n")
       return
    end if
    val=css%oval(itrg,iloc)
    model_getOutput=.true.
    return
  end function model_getOutput
  !
  !
  !###############################################################################
  ! STRING ROUTINES FOR REPORTING
  !###############################################################################
  ! get description of indexes
  !
  character*250 function model_getdesc250(fpos,f,v,crc250,irc)
    integer :: fpos                   ! current dimension index 
    type(mod_file),pointer :: f
    type(mod_variable),pointer :: v
    character*250 :: crc250  ! error message string
    integer :: irc        ! error return code(0=ok)
    integer, external :: length
    integer :: leni,lenc,lenb,lend,lens
    integer :: cpos,xsize
    integer :: ii,jj
    character*250 :: buff250
    character*10 :: ccdim10
    logical :: bok
    integer, allocatable :: cdim(:)
    character*25 :: myname="model_getdesc250"

    allocate(cdim(max(1,v%ndim)),stat=irc)
    cpos= (fpos-1)
    buff250=""
    lenb=0
    if (fpos.ne.-1) then
       do ii=1,v%ndim
          cdim(ii)=1+v%istart(ii)+mod(cpos,max(1,v%icount(ii)))
          cpos=int(cpos/max(1,(v%icount(ii))))
       end do
       ! print other dimensions
       do ii=1,v%ndim
          write(ccdim10,'(I10)')cdim(ii)
          call chop0(ccdim10,10)
          lenc=length(ccdim10,10,10)
          lend=length(f%dim80(v%ind(ii)),80,10)
          buff250=sep//f%dim80(v%ind(ii))(1:lend)//sep//ccdim10(1:lenc)//buff250(1:lenb)
          call chop0(buff250,250)
          lenb=length(buff250,250,max(1,min(250,lenb)))
       end do
    end if
    !write(*,*)myname,"DESC:",buff250(1:lenb),nslice
    call chop0(buff250,250)
    model_getdesc250=buff250
    if (allocated(cdim)) deallocate(cdim)
  end function model_getdesc250
  !
  ! get value and attributes
  !
  character*250 function model_getGrid250(f,v,loc,val,wgt,crc250,irc)
    type(mod_file),pointer :: f   ! file
    type(mod_variable),pointer :: v     ! variable
    type(mod_location),pointer :: loc   ! location
    real :: val                     ! value
    real :: wgt                     ! weight
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_getGrid250"
    integer ::lenp,lenv,lenw,ii
    character*250 :: pos250
    character*50 :: val50
    character*50  :: wgt50
    character*10 :: pos10
    integer, external :: length
    ! <value pos="1,21,100" val="3499.2"/>
    ! make dimension string

    pos250=model_getPos50(v,loc)
    call chop0(pos250,250)
    lenp=length(pos250,250,10)
    if (lenp.ne.0) then
       pos250=' pos="'//pos250(1:lenp)//'"'
       lenp=lenp+7
    end if
    !
    ! make value string
    if (val.eq.nf_fill_double) then
       if(mod_bdeb)write(*,*)myname,'Match:',val,nf_fill_double,&
            & val.eq.nf_fill_double,val-nf_fill_double
       val50=""
       lenv=0
    else
       if(mod_bdeb)write(*,*)myname,'No match:',val,nf_fill_double,&
            & val.eq.nf_fill_double,val-nf_fill_double
       call model_wash(val,val50,lenv)
       val50=' val="'//val50(1:lenv)//'"'
       lenv=lenv+7
    end if
    if (.false. .and. wgt.ge.0.0D0) then
       call model_wash(wgt,wgt50,lenw)
       wgt50=' wgt="'//wgt50(1:lenw)//"'"
       lenw=lenw+7
    else
       lenw=0
    end if

    !
    model_getGrid250='<grid '//pos250(1:lenp)//val50(1:lenv)//wgt50(1:lenw)//'/>'
    return
  end function model_getGrid250
  !
  ! write interpolated value
  !
  character*250 function model_getInt250(f,v,loc,val,crc250,irc)
    type(mod_file),pointer :: f   ! file
    type(mod_variable),pointer :: v     ! variable
    type(mod_location),pointer :: loc   ! location
    real :: val                         ! interpolated value
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_getInt250"
    integer ::lenv,ii
    character*50 :: val50
    character*10 :: pos10
    integer, external :: length
    ! <value val="3499.2"/>
    ! make dimension string

    ! make value string
    if (val.eq.nf_fill_double) then
       if(mod_bdeb)write(*,*)myname,'Match:',val,nf_fill_double,&
            & val.eq.nf_fill_double,val-nf_fill_double
       val50=""
       lenv=0
    else
       if(mod_bdeb)write(*,*)myname,'No match:',val,nf_fill_double,&
            & val.eq.nf_fill_double,val-nf_fill_double
       call model_wash(val,val50,lenv)
       val50=' val="'//val50(1:lenv)//'"'
       lenv=lenv+7
    end if
    !
    if (lenv.eq.0) then
       model_getInt250=''
    else
       model_getInt250='<interpolated'//val50(1:lenv)//'/>'
    end if
    return
  end function model_getInt250
  !
  ! get location attributes
  !
  subroutine model_getTarget50(trg,lent,trg50)
    real :: trg
    integer :: lent
    character*50 :: trg50
    integer, external :: length
    ! make target string
    call model_wash(trg,trg50,lent)
    trg50="val='"//trg50(1:lent)//"'"
    lent=lent+8
    return
  end subroutine model_getTarget50
  !
  ! get position string
  !
  character*50 function model_getPos50(v,loc)
    type(mod_variable), pointer :: v
    type(mod_location), pointer :: loc
    character*50 :: buff50
    character*10 :: item10
    integer :: lenb,leni
    integer, external :: length
    integer :: ii,jj,xx
    character*25 :: myname="model_writePos"
    buff50=""
    lenb=0
    do ii=1,v%ndim
       write(item10,'(I10)') loc%pos(v%ind(ii))
       call chop0(item10,10)
       leni=length(item10,10,3)
       if (lenb.eq.0) then
          buff50=item10(1:leni)
          lenb=leni
       else
          buff50=buff50(1:lenb)//","//item10(1:leni)
          lenb=lenb+leni+1
       end if
    end do
    model_getPos50=buff50
    return
  end function model_getPos50
  !
  ! get position string with weights
  !
  character*50 function model_getPosWgt50(css,v,loc)
    type(mod_session), pointer :: css   ! current session
    type(mod_variable),pointer :: v     ! variable
    type(mod_location),pointer :: loc   ! location
    character*50 :: buff50
    character*20 :: pos20
    integer :: lenb,lenp
    integer, external :: length
    integer :: ii,jj,xx
    character*25 :: myname="model_getPosWgt50"
    buff50=""
    lenb=0
    do ii=1,v%ndim
       if (loc%intpf(v%ind(ii)).lt.0.0D0.or.loc%intpf(v%ind(ii)).gt.1.0D0) then
          if (loc%lstart(v%ind(ii)).ne.loc%lstop(v%ind(ii))) then
             write(pos20,'(I0,"..",I0)') loc%lstart(v%ind(ii)),loc%lstop(v%ind(ii))
          else
             write(pos20,'(I0)') loc%lstart(v%ind(ii))
          end if
       else
          write(pos20,'(F20.4)') real(loc%lstart(v%ind(ii)))+loc%intpf(v%ind(ii))
       end if
       call chop0(pos20,20)
       lenp=length(pos20,20,3)
       if (lenb.eq.0) then
          buff50=pos20(1:lenp)
          lenb=lenp
       else
          buff50=buff50(1:lenb)//","//pos20(1:lenp)
          lenb=lenb+lenp+1
       end if
    end do
    model_getPosWgt50=buff50
    return
  end function model_getPosWgt50
  !
  ! set location rpos
  !
  subroutine model_setLocRpos(file,varid,loc)
    type(mod_file), pointer :: file     ! file
    integer :: varid                    ! variable id
    type(mod_location),pointer :: loc   ! location
    character*50 :: buff50
    character*20 :: pos20
    integer :: lenb,lenp
    integer, external :: length
    integer :: ii, itrg
    type(mod_variable),pointer :: v     ! variable
    character*25 :: myname="model_getPosWgt50"
    v => file%var(varid)%ptr
    do ii=1,v%ndim
       itrg=file%dim_trg(v%ind(ii))
       if (itrg.ne.0) then ! dimension is a target
          if (loc%intpf(v%ind(ii)).lt.0.0D0.or.loc%intpf(v%ind(ii)).gt.1.0D0) then
             loc%rpos(v%ind(ii))=loc%lstart(v%ind(ii))
          else
             loc%rpos(v%ind(ii))=real(loc%lstart(v%ind(ii)))+&
                  & loc%intpf(v%ind(ii))
          end if
          if (mod_bdeb)write(*,*)myname,"VVariable '"//&
               & v%var80(1:v%lenv)//"' dim:",ii," dim_trg=",&
               & itrg,loc%rpos(v%ind(ii))
       elseif (mod_bdeb) then
          write(*,*)myname,"VVariable '"//&
               & v%var80(1:v%lenv)//"' dim:",ii,' no dim target.'
       end if
    end do
    return
  end subroutine model_setLocRpos
  !
  ! returns time as character-string
  !
  character*21 function model_gettime(j2000) 
    implicit none
    real :: j2000
    integer :: yy,mm,dd,hh,mi
    real :: sec
    character*4 :: csec
    integer, external :: length
    integer :: lenp,lenc
    character*25 :: myname="model_gettime"
    call dj2000(j2000,yy,mm,dd,hh,mi,sec)
    write(csec,'(F4.1)') sec
    call chop0(csec,4)
    lenc=length(csec,4,4)
    if (sec.lt.10.0D0)  csec="0"//csec(1:lenc)
    write(model_gettime,'(I4.4,"/",I2.2,"/",I2.2," ",I2.2,":",I2.2,":",A4)') &
         & yy,mm,dd,hh,mi,csec
!!!!! write(*,*) myname,j2000,model_gettime
  end function model_gettime

  real function  model_getj2000(time50,crc250,irc) 
    implicit none
    character*50 :: time50
    character*250 :: crc250
    integer :: irc
    real :: j2000
    integer, external :: length
    integer :: lenp,lent
    character*25 :: myname="model_getj2000"
    integer :: yy,mm,dd,hh,mi
    real :: sec
    ! first try to read as formatted time
    read(time50,'(I4,X,I2,X,I2,X,I2,X,I2,X,F4.1)',iostat=irc)yy,mm,dd,hh,mi,sec
    if (irc.eq.0) then
       call jd2000(j2000,yy,mm,dd,hh,mi,sec)
    else
       read(time50,*,iostat=irc)j2000
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Unable to determine time from:")
          call model_errorappend(crc250,time50)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    model_getj2000=j2000
    !lent=length(time50,50,10)
    if(mod_bdeb)write(*,*)myname,' Time:',time50(1:lent)," Found:",j2000
  end function model_getj2000
  !
  character*250 function model_getvar250(f,varid)
    implicit none
    type(mod_file),pointer :: f
    integer :: varid
    character*50 :: dim50
    integer :: lend
    integer, external :: length
    dim50=model_getDim(f,f%var(varid)%ptr)
    call chop0(dim50,50)
    lend=length(dim50,50,10)
    if (lend.eq.0) then
       model_getvar250="variable='"// &
            & f%var(varid)%ptr%var80(1:f%var(varid)%ptr%lenv)//"'"
    else
       model_getvar250="variable='"// &
            & f%var(varid)%ptr%var80(1:f%var(varid)%ptr%lenv)&
            & //"' dim='"//dim50(1:lend)//"'"
    end if
    call chop0(model_getvar250,250)
    return
  end function model_getvar250
  !
  !###############################################################################
  ! ROUTINES FOR LOCATION SEARCH
  !###############################################################################
  ! location position
  !
  subroutine model_initLocPos(css,f,p,b,loc,crc250,irc)
    type(mod_session), pointer :: css   ! current session
    type(mod_file),pointer :: f   ! current file
    type(mod_batch), pointer :: b       ! current batch
    type(mod_location),pointer :: loc   ! location
    type(mod_plan),pointer :: p
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_initLocPos"
    integer :: ii
    logical :: changed
    !
    ! loc
    !
    if (allocated(loc%pos)) deallocate(loc%pos)
    if (allocated(loc%rpos)) deallocate(loc%rpos)
    if (allocated(loc%lstart)) deallocate(loc%lstart)
    if (allocated(loc%lstop)) deallocate(loc%lstop)
    if (allocated(loc%intpf)) deallocate(loc%intpf)
    loc%ndim=f%ndim
    allocate(loc%pos(loc%ndim),&
         & loc%rpos(loc%ndim),&
         & loc%lstart(loc%ndim),&
         & loc%lstop(loc%ndim),&
         & loc%intpf(loc%ndim),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate 'loc%pos/istop/ff'.")
       call model_errorappend(crc250,"\n")
       return
    end if
    do ii=1,loc%ndim
       loc%pos(ii)=1
       loc%rpos(ii)=0.0D0
       loc%lstart(ii)=1
       loc%lstop(ii)=f%istop(ii)
       loc%intpf(ii)=-1.0D0
    end do
    return
  end subroutine model_initLocPos
  !
  subroutine model_clearLocList(css,crc250,irc)
    type(mod_session), pointer :: css   ! current session
    type(mod_location),pointer :: loc   ! location
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_clearLocList"
    integer :: ii
    logical :: changed
    if (allocated(css%locData)) then
       do ii=1,css%nloc
          loc => css%locData(ii)%ptr
          call model_deleteLoc(css,loc,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,"model_deleteLoc")
             return
          end if
       end do
       css%nloc=0 ! should not be necessary
       deallocate(css%locData)
    end if
    css%locReady=.false.
    return
  end subroutine model_clearLocList
  !
  subroutine model_search(css,f,p,b,loc,crc250,irc)
    type(mod_session), pointer :: css   ! current session
    type(mod_file),pointer :: f   ! current file
    type(mod_batch), pointer :: b       ! current batch
    type(mod_location),pointer :: loc   ! location
    type(mod_plan),pointer :: p
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_search"
    integer :: ii
    logical :: changed
    !
    ! make position, variable and target vector
    !
    do ii=1,b%ndim
       loc%pos(b%ind(ii))=1
    end do
    call model_getTarget(f,p,b,loc,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from getTarget.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    !
    ! print searched dimensions
    !
    if(mod_bdeb)then
       write(*,*) myname,"Vars:",(" "//&
            & f%var80(b%var(ii))(1:f%lenv(b%var(ii))),ii=1,b%nvar)
       write(*,*) myname,"Dims:",(" "//&
            & f%dim80(b%ind(ii))(1:f%lend(b%ind(ii))),ii=1,b%ndim)
    end if
    !
    ! loop until position vector does not change
    !
    changed=.true.
    do while(changed)
       !
       ! get increment vectors
       !
       if (mod_bdeb) write(*,*)myname,'Looking for increments.'
       call model_getIncrements(f,b,loc,changed,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from getIncrements.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end do
    if (mod_bdeb) write(*,*)myname,'Setting search flags.'
    call model_setSearchFlag(f,b,loc,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from setSearchFlag.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    if (mod_bdeb) then
          do ii=1,b%ndim
             write(*,*)myname,"Search limits='"//f%dim80(b%ind(ii))(1:f%lend(b%ind(ii)))//"'",&
                  & loc%lstart(b%ind(ii)),loc%lstop(b%ind(ii)),' intp=',loc%intpf(b%ind(ii))
          end do
       write(*,*)myname,' Done.',loc%search
    end if
    return
  end subroutine model_search
  !
  ! set the batch target
  !
  subroutine model_getTarget(f,p,b,loc,crc250,irc)
    type(mod_file),pointer :: f   ! current file
    type(mod_batch), pointer :: b       ! current batch
    type(mod_location),pointer :: loc   ! location
    type(mod_plan),pointer :: p
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_getTarget"
    integer :: ii
    ! if target is a dimension, set pos, else set target
    do ii=1,b%nvar
       if (b%var2slc(ii).ne.0) then
          b%trgvar(ii)=loc%sli_val(b%var2slc(ii))
          p%trgvar(b%var(ii))=loc%sli_val(b%var2slc(ii))
       else
          irc=347
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Missing target variable:"//&
               & f%var80(b%var(ii)) )
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end do
    do ii=1,b%ndim
       if (b%dim2slc(ii).ne.0) then
          loc%pos(b%ind(ii)) =     floor(loc%sli_val(b%dim2slc(ii)))
          loc%lstart(b%ind(ii)) =  floor(loc%sli_val(b%dim2slc(ii)))
          loc%lstop(b%ind(ii)) = ceiling(loc%sli_val(b%dim2slc(ii)))
          loc%intpf(b%ind(ii)) = loc%sli_val(b%dim2slc(ii))-loc%pos(b%ind(ii))
          if(mod_bdeb)write(*,*) myname,">>>>>>>Loc:",ii,&
               & loc%pos(b%ind(ii)),loc%lstart(b%ind(ii)),loc%lstop(b%ind(ii))
       end if
    end do
    return
  end subroutine model_getTarget
  !
  ! get increment vectors (how much variables increase in each dimension)
  !
  subroutine model_getIncrements(f,b,loc,changed,crc250,irc)
    type(mod_file),pointer :: f   ! current file
    type(mod_batch), pointer :: b       ! current batch
    type(mod_location), pointer :: loc       ! current location
    logical :: changed              ! did the grid cell change?
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_getIncrements"
    integer :: ii,jj,buff
    real :: nv(max(1,b%ndim),0:b%ndim)     ! normalised vector, 0=unperturbed
    call model_getIncrement(f,b,loc,nv(1,0),crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from getIncrement.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    do jj=1,b%ndim
       if (b%dim2slc(jj).eq.0) then  ! this is a variable
          buff=loc%pos(b%ind(jj))
          loc%pos(b%ind(jj))=loc%pos(b%ind(jj))+b%inc(jj)
          b%inc(jj)=-b%inc(jj)
          !b%inc(jj)=-1
          call model_getIncrement(f,b,loc,nv(1,jj),crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from getIncrement.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          b%inc(jj)=-b%inc(jj)
          !b%inc(jj)=1
          loc%pos(b%ind(jj))=buff
       else                         ! this is a defined dimension
          do ii=1,b%ndim
             nv(ii,jj)=0.0D0
          end do
       end if
    end do
    call model_useIncrements(f,b,loc,nv,changed,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from useIncrement.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
  end subroutine model_getIncrements
  !
  ! calculate increments in each dimension
  !
  subroutine model_getIncrement(f,b,loc,nv,crc250,irc)
    type(mod_file),pointer :: f   ! current file
    type(mod_batch), pointer :: b       ! current batch
    type(mod_location),pointer :: loc   ! location
    real :: nv(b%ndim)                  ! normalised vector
    character*250 :: crc250             ! error message
    integer :: irc                      ! error return code (0=ok)
    character*25 :: myname="model_getIncrement"
    integer :: ii,jj,kk,buff
    real :: t(b%nvar)        ! target departure (using pos as origo)
    real :: v(b%nvar,b%ndim) ! value departure in grid dimension directions (gdd)
    real :: n(b%nvar,b%ndim) ! target departure projected to gdd
    real :: dot_nv, dot_vv   ! dot-products
    !
    ! get 0-value
    !
    call model_setBatchValue(f,b,loc,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from setBatchValue.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    dot_nv=0.0D0
    do ii=1,b%nvar
       t(ii)=b%trgvar(ii) - b%val(ii)
       do jj=1,b%ndim
          v(ii,jj)=b%val(ii)
       end do
       dot_nv=dot_nv+t(ii)*t(ii)
    end do
    !
    ! get perturbed values (at other grid points)
    !
    dot_vv=0.0D0
    do jj=1,b%ndim
       if (b%dim2slc(jj).eq.0) then 
          buff=loc%pos(b%ind(jj))
          loc%pos(b%ind(jj))=loc%pos(b%ind(jj))+b%inc(jj)
          call model_setBatchValue(f,b,loc,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from setBatchValue.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          loc%pos(b%ind(jj))=buff
          do ii=1,b%nvar
             v(ii,jj)=b%val(ii)-v(ii,jj)
             dot_vv=dot_vv+v(ii,jj)*v(ii,jj)
          end do

          if(mod_bdeb)write(*,*)myname,'D(var)=',(v(ii,jj),ii=1,b%nvar)

       end if
    end do
    !
    ! v = grid increment (variable, dimension)
    ! n = target relative to grid and along desired dimension
    ! loop over dimensions and make normalised variable vector increments
    ! ...n(ii,jj)=n(ii,jj)-(n(,jj).v(,kk))*v(ii,kk)/(v(,kk)*v(,kk))...
    !
    if (dot_vv.le.1.0D-10) then
       if (dot_nv.lt.1.0D-10) then ! match target matches single value array
          do jj=1,b%ndim  ! desired dimension
             nv(jj)=0.0D0 ! initial value matches...
          end do
       else                        ! no match between target and variable (with dim=1)
          do jj=1,b%ndim   ! desired dimension
             nv(jj)=-1.0D0 ! out of bounds... (below)
          end do
       end if
    else
       do jj=1,b%ndim ! desired dimension
          if (b%dim2slc(jj).eq.0) then 
             do ii=1,b%nvar ! variable loop
                n(ii,jj)=t(ii)
             end do
             do kk=1,b%ndim ! remove projection along other dimensions than the desired one
                if (kk.eq.jj) cycle
                dot_nv=0.0D0
                dot_vv=0.0D0
                do ii=1,b%nvar
                   dot_nv=dot_nv+n(ii,jj)*v(ii,kk)
                   dot_vv=dot_vv+v(ii,kk)*v(ii,kk)
                end do
                do ii=1,b%nvar ! variable loop
                   n(ii,jj)=n(ii,jj)-v(ii,kk)*dot_nv/dot_vv
                end do
             end do
             !
             ! calculate n . v and v . v
             !
             dot_nv=0.0D0
             dot_vv=0.0D0
             do ii=1,b%nvar
                dot_nv=dot_nv + n(ii,jj)*v(ii,jj)
                dot_vv=dot_vv + v(ii,jj)*v(ii,jj)
             end do
             ! 
             ! increment (nn) == n . v / v . v, or = |n|/|v| 

             if(mod_bdeb)write(*,*)myname,'N(dim)=',jj,dot_nv,dot_vv,dot_nv/dot_vv


             nv(jj)=dot_nv/dot_vv ! extrapolation factor for reciprocal grid point...
             ! if factor is negative, target is outside grid cell in current direction
          else
             nv(jj)=0.0D0
          end if
       end do
    end if
    return
  end subroutine model_getIncrement
  !
  ! use increments in batch job
  !
  subroutine model_useIncrements(f,b,loc,nv,changed,crc250,irc)
    type(mod_file),pointer :: f   ! current file
    type(mod_batch), pointer :: b       ! current batch
    type(mod_location),pointer :: loc   ! location
    real :: nv(b%ndim,0:b%ndim) ! extrapolation factors
    logical :: changed
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_useIncrements"
    integer :: jj
    real :: sum
    changed=.false.
    do jj=1,b%ndim
       if(mod_bdeb)write(*,*)myname,'Flags:',jj,(b%dim2slc(jj).ne.0),nv(jj,0),nv(jj,jj)
       if (b%dim2slc(jj).ne.0) cycle ! target is already set...
       if (nv(jj,0) .lt. 0) then ! decrement
          if (loc%pos(b%ind(jj)).gt.1) then
             loc%pos(b%ind(jj))=max(1,loc%pos(b%ind(jj))-&
                  & max(1,nint(-nv(jj,0)*0.61803399D0+0.5D0)))! -1
             changed=.true.
          end if
       else if (nv(jj,jj) .lt. 0) then ! increment
          if (loc%pos(b%ind(jj)).lt.f%istop(b%ind(jj))-1) then
             loc%pos(b%ind(jj))=min(f%istop(b%ind(jj))-1,&
                  & loc%pos(b%ind(jj))+max(1,-nint(nv(jj,jj)*0.61803399D0+0.5D0))) ! +1
             changed=.true.
          end if
       else
          sum=nv(jj,0)+nv(jj,jj)
          if (sum.gt.1.0D-10) then
             if (nv(jj,0).lt.1.0D-10) then ! target at initial grid point
                loc%lstart(b%ind(jj))=loc%pos(b%ind(jj))
                loc%lstop(b%ind(jj))=loc%pos(b%ind(jj))
                loc%intpf(b%ind(jj))=nv(jj,0)/sum
             else if(nv(jj,jj).lt.1.0D-10) then ! target at incremential grid point
                loc%lstart(b%ind(jj))=loc%pos(b%ind(jj))+1
                loc%lstop(b%ind(jj))=loc%pos(b%ind(jj))+1
                loc%intpf(b%ind(jj))=nv(jj,jj)/sum
             else ! target between initial and incremental grid points
                loc%lstart(b%ind(jj))=loc%pos(b%ind(jj))
                loc%lstop(b%ind(jj))=loc%pos(b%ind(jj))+1
                loc%intpf(b%ind(jj))=nv(jj,0)/sum
             end if
             ! write(*,*)myname,'FF:',loc%intpf(b%ind(jj)),nv(jj,0),nv(jj,jj),sum
          else ! grid points and target all coincide
             loc%lstart(b%ind(jj))=loc%pos(b%ind(jj))
             loc%lstop(b%ind(jj))=loc%pos(b%ind(jj))
             loc%intpf(b%ind(jj))=0.0D0
          end if
       end if
    end do
    return
  end subroutine model_useIncrements
  !
  subroutine model_setSearchFlag(f,b,loc,crc250,irc)
    type(mod_file),pointer :: f   ! current file
    type(mod_batch), pointer :: b       ! current batch
    type(mod_location),pointer :: loc   ! location
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setSearchFlag"
    integer :: jj
    real :: sum
    if (loc%bok) then
       loc%search=0
       do jj=1,b%ndim
          if (loc%lstart(b%ind(jj)).ne.loc%lstop(b%ind(jj)) .and. &
               & loc%lstart(b%ind(jj))+1.ne.loc%lstop(b%ind(jj)) .and. &
               & (loc%intpf(b%ind(jj)).lt.0.0D0.or.loc%intpf(b%ind(jj)).gt.1.0D0)) then
             loc%search=b%ind(jj)
          end if
       end do
    else
       loc%search=-1
    end if
    return
  end subroutine model_setSearchFlag
  !
  ! get current batch values
  !
  subroutine model_setBatchValue(f,b,loc,crc250,irc)
    type(mod_file),pointer :: f   ! current file
    type(mod_batch), pointer :: b       ! current batch
    type(mod_location), pointer :: loc       ! current batch
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setBatchValue"
    integer :: ii,kk,ll
    logical :: set
    type(mod_variable), pointer :: v
    do ii=1,b%nvar
       v => f%var(b%var(ii))%ptr
       set=.true.
       if (.not.model_getLocValue(v,loc,b%val(ii),crc250,irc)) then
          b%val(ii)=nf_fill_double
       end if
    end do
    return
  end subroutine model_setBatchValue
  !
  ! get current batch values
  !
  logical function model_getLocValue(v,loc,val,crc250,irc)
    type(mod_variable), pointer :: v       ! variable
    type(mod_location), pointer :: loc       ! current batch
    real :: val
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_getLocValue"
    integer :: ii
    character*50 :: pos50,loc50,val50
    integer :: lenp,lenl,lenv
    integer, external :: length
    if (v%ndim.gt.0) then
       ii=model_getLoc(v,loc)
    else 
       ii=1
    end if
    if (mod_bdeb) then
       write(loc50,'(I10)') ii
       call chop0(loc50,50,10)
       lenl=length(loc50,50,10)
       pos50=model_getPos50(v,loc)
       call chop0(pos50,50,10)
       lenp=length(pos50,50,10)
       if (ii.lt.1.or.ii.gt.v%len) then
          write(*,*)myname," *** Invalid pos: '"&
               & //v%var80(1:v%lenv)//"("//pos50(1:lenp)//&
               & ")' loc=",ii,"(max=",v%len,")"
       end if
    end if
    if (ii.eq.0) then
       model_getLocValue=.false.
    else
       model_getLocValue=model_getValue(v,ii,val,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from getValue.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    if(mod_bdeb)then
       write(val50,*)model_getLocValue
       call chop0(val50,50,10)
       lenv=length(val50,50,10)
       write(*,*)myname,"Pos="//pos50(1:lenp)//&
            & " ll="//loc50(1:lenl)//" val="//val50(1:lenv),&
            & " set=",model_getLocValue
    end if
    return
  end function model_getLocValue

  logical function model_getValue(v,ii,val,crc250,irc)
    type(mod_variable), pointer :: v       ! variable
    integer :: ii
    real :: val
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_getValue"
    if (ii.lt.1.or.ii.gt.v%len) then
       model_getValue=.false.
    else
       model_getValue=.true.
       select case (v%type)
       case (nf_char)
          model_getValue=.false.
       case (nf_int1)
          if (v%f1(ii).eq.v%m1) then
             model_getValue=.false.
          else
             val=v%f1(ii)*v%scale
          end if
       case (nf_int2)
          if (v%f2(ii).eq.v%m2) then
             model_getValue=.false.
          else
             val=v%f2(ii)*v%scale
          end if
       case (nf_int)
          if (v%f4(ii).eq.v%m4) then
             model_getValue=.false.
          else
             val=v%f4(ii)*v%scale
          end if
       case (nf_real)
          if (v%fr(ii).eq.v%mr) then
             model_getValue=.false.
          else
             val=v%fr(ii)*v%scale
          end if
       case (nf_double)
          if (v%fd(ii).eq.v%md) then
             model_getValue=.false.
          else
             val=v%fd(ii)*v%scale
          end if
       case DEFAULT
          model_getValue=.false.
       end select
    end if
    return
  end function model_getValue
  !
  integer function model_getLoc(v,loc)
    type(mod_variable),pointer :: v
    type(mod_location),pointer :: loc
    integer ii,ll
    model_getLoc=0
    do ii=v%ndim,1,-1 ! do ii=1,n
       if (loc%pos(v%ind(ii)).lt.v%istart(ii).or.(loc%pos(v%ind(ii))-v%istart(ii)).ge.v%icount(ii)) return
       model_getLoc=model_getLoc*v%icount(ii) + (loc%pos(v%ind(ii))-v%istart(ii))
    end do
    model_getLoc=model_getLoc+1
    return
  end function model_getLoc
  !
  ! set position
  !
  subroutine model_resetPos(n,ind,loc)
    integer :: n          ! selected dimensions
    integer :: ind(n)
    type(mod_location), pointer :: loc
    integer :: jj
    do jj=1,n
       loc%pos(ind(jj))=loc%lstart(ind(jj))
    end do
    return
  end subroutine model_resetPos
  !
  ! increment position
  !
  integer function model_incrementPos(n,ind,loc)
    integer :: n          ! selected dimensions
    integer :: ind(n)     ! global dimension index
    type(mod_location), pointer :: loc
    logical :: bdone
    integer :: jj
    model_incrementPos=0
    bdone=.false.
    jj=0
    do while (.not.bdone)
       jj=jj+1
       if (jj.le.n) then
          loc%pos(ind(jj))=loc%pos(ind(jj))+1
          if (loc%pos(ind(jj)).gt.loc%lstop(ind(jj))) then
             loc%pos(ind(jj))=loc%lstart(ind(jj))
          else
             model_incrementPos=jj
             bdone=.true.
          end if
       else
          bdone=.true.
       end if
    end do
    return
  end function model_incrementPos
  !
  ! get accumulated weight
  !
  real function model_getWeight(n,ind,loc)
    integer :: n          ! selected dimensions
    integer :: ind(n)
    type(mod_location), pointer :: loc
    logical :: bdone
    integer :: ii,di
    real :: ww
    character*25 :: myname="model_getWeight"

    bdone=.false.
    model_getWeight=1.0
    do ii=1,n
       di=loc%lstop(ind(ii))-loc%lstart(ind(ii))
       if (di.gt.0) then
          if (loc%pos(ind(ii)).eq.loc%lstart(ind(ii))) then
             ww=(1.0D0-loc%intpf(ind(ii)))
          else if (di.ge.1) then
             ww=(loc%intpf(ind(ii)))/real(di)
          else
             ww=1.0D0
          end if
          if(mod_bdeb)write(*,*)myname,'Weight:',ii,ww,&
               & loc%intpf(ind(ii)),loc%pos(ind(ii)),loc%lstart(ind(ii))
          model_getWeight=model_getWeight*ww
       end if
    end do
    return
  end function model_getWeight
  !
  !
  !###############################################################################
  ! PLAN/BATCH ROUTINES
  !###############################################################################
  ! make plan for target selection, find variables with overlapping dimensions
  !
  subroutine model_planBatch(css,f,p,crc250,irc)
    type(mod_session), pointer :: css !  current session
    type(mod_file),pointer :: f
    type(mod_plan),pointer  :: p
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_planBatch"
    integer :: ii,jj,kk,leng,lenv,lend
    integer, external :: length
    integer :: nslc2var
    type(mod_variable),pointer :: var
    type(mod_batch), pointer :: b
    logical, allocatable :: sliceProcessed(:)  ! is slice variable sliceProcesseded 
    logical, allocatable :: innerDim(:)  ! is inner dimension
    integer, allocatable :: slc2var(:)  ! index to global variable
    integer, allocatable :: slc2dim(:)  ! index to global dimension
    integer, allocatable :: var2slc(:)  ! index to slice variable
    integer, allocatable :: dim2slc(:)  ! index to slice dimension
    logical :: changed
    !
    ! make slice -- variable/dimension indexes
    !
    if(mod_bdeb)write(*,*)myname,' Entering.',css%csli,f%nvar
    call model_initPlan(p,css,f,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from initPlan.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    !
    ! find batch jobs...
    ! ...batch job variables must be searched together
    if(mod_bdeb)write(*,*)myname,'Find batch jobs.',css%csli
    !
    allocate(sliceProcessed(css%csli),slc2var(css%csli),var2slc(css%csli),&
         & innerDim(f%ndim),slc2dim(f%ndim),dim2slc(f%ndim),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Unable to allocate slc2var....")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if    
    do ii=1,css%csli ! initialise
       sliceProcessed(ii)=.false.
       slc2var(ii)=0 ! index to global variable array
       var2slc(ii)=0 ! index to slice array
    end do
    do ii=1,css%csli
       if(mod_bdeb)write(*,*)myname,'Processing slice.',ii,css%csli,sliceProcessed(ii)
       if (.not.sliceProcessed(ii)) then ! slice not searched yet
          allocate(b,stat=irc)
          b%nvar=0                ! variables in batch job
          b%ndim=0                ! dimensions in batch job
          do jj=1,f%ndim    ! initialise
             innerDim(jj)=.false. ! marked dimension in global array
             dim2slc(jj)=0        ! position in slice array
          end do
          !
          if (p%slc2var(ii).ne.0) then ! this is a variable

             if(mod_bdeb)write(*,*)myname,'Found Var:',ii,&
                  & "'"//f%var80(p%slc2var(ii))(1:&
                  & f%lenv(p%slc2var(ii)))//"'"

             var => f%var(p%slc2var(ii))%ptr
             ! mark all variables
             do kk=1,var%ndim
                if (.not. innerDim(var%ind(kk))) then
                   innerDim(var%ind(kk))=.true.
                   p%innerDim(var%ind(kk))=.true.
                   if(mod_bdeb)write(*,*)myname,'Mark Dim: ',ii,&
                        & "'"//f%dim80(var%ind(kk))(1:&
                        & f%lend(var%ind(kk)))//"'"
                   b%ndim=b%ndim+1
                   slc2dim(b%ndim)=var%ind(kk)
                   dim2slc(kk)=0
                end if
             end do
             b%nvar=b%nvar+1 ! this is the first variable
             slc2var(b%nvar)= p%slc2var(ii) ! index to global variable
             var2slc(b%nvar)= ii ! index to slice variable
             sliceProcessed(ii)=.true.
          else if (p%slc2dim(ii).ne.0) then ! this is a dimension

             if(mod_bdeb)write(*,*)myname,'Found Dim:',ii,&
                  & "'"//f%dim80(p%slc2dim(ii))(1:&
                  & f%lend(p%slc2dim(ii)))//"'",&
                  & p%slc2dim(ii),b%ndim

             innerDim(p%slc2dim(ii))=.true.
             p%innerDim(p%slc2dim(ii))=.true.
             b%ndim=b%ndim+1
             slc2dim(b%ndim)=p%slc2dim(ii)
             dim2slc(p%slc2dim(ii))= ii  ! f%dim( p%slc2dim(ii) )
             sliceProcessed(ii)=.true.
          end if
          ! model_add(innerDim,p%slc2var(ii))
          changed=(b%ndim .gt. b%nvar)
          if(mod_bdeb)write(*,*)myname,'Cross-checking.',changed,b%ndim,b%nvar
          LOOP: do while (changed)
             changed=.false.
             do jj=ii+1,css%csli
                if (.not.sliceProcessed(jj)) then ! not processed yet
                   if (p%slc2var(jj).ne.0) then ! is a variable

                      if(mod_bdeb)write(*,*)myname,'>>Found Var:',ii,&
                           & "'"//f%var80(p%slc2var(jj))(1:&
                           & f%lenv(p%slc2var(jj)))//"'"

                      var => f%var(p%slc2var(jj))%ptr
                      if (model_overlaps(f%ndim,innerDim,var)) then
                         do kk=1,var%ndim
                            if (.not. innerDim(var%ind(kk))) then
                               innerDim(var%ind(kk))=.true.
                               p%innerDim(var%ind(kk))=.true.
                               if(mod_bdeb)write(*,*)myname,'>>Mark Dim: ',ii,&
                                    & f%dim80(var%ind(kk))(1:10)
                               b%ndim=b%ndim+1
                               slc2dim(b%ndim)=var%ind(kk)
                               dim2slc(kk)=0
                            end if
                         end do
                         b%nvar=b%nvar+1
                         slc2var(b%nvar)=p%slc2var(jj) ! index to global variable
                         var2slc(b%nvar)=jj    ! index to slice variable
                         sliceProcessed(jj)=.true.
                         changed=.true.
                      end if
                   else if (p%slc2dim(jj).ne.0) then ! is a dimension
                      if (.not. innerDim(p%slc2dim(jj))) then

                         if(mod_bdeb)write(*,*)myname,'Found Dim:',ii,&
                              & "'"//f%dim80(p%slc2dim(jj))(1:&
                              & f%lend(p%slc2dim(jj)))//"'",&
                              & p%slc2dim(jj),b%ndim

                         innerDim(p%slc2dim(jj))=.true.
                         p%innerDim(p%slc2dim(jj))=.true.
                         b%ndim=b%ndim+1
                         slc2dim(b%ndim)=p%slc2dim(jj)
                         dim2slc(b%ndim)= jj
                         sliceProcessed(jj)=.true.
                         changed=.true.
                      end if
                   end if
                end if
                if (b%ndim .le. b%nvar) exit LOOP ! we have enough variables
             end do
          end do LOOP
          ! add batch jobb...
          if(mod_bdeb)write(*,'(X,A,4(A,I0))')myname,&
               & 'Allocating ndim=',b%ndim,&
               & ', nvar=',b%nvar,', batch=',ii,', csli=',css%csli
          allocate(b%ind(b%ndim),b%inc(b%ndim),b%dim2slc(b%ndim),&
               & b%trgdim(b%ndim),b%dim80(b%ndim),&
               & b%var(b%nvar),b%val(b%nvar),b%proc(b%nvar),&
               & b%var2slc(b%nvar),b%trgvar(b%nvar),b%var80(b%nvar),&
               & stat=irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Unable to allocate batch dimensions.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          !if(mod_bdeb)write(*,*)myname,'Store variables.',&
          !     & b%ndim,b%nvar,allocated(f%var80)
          do jj=1,b%nvar
             b%var(jj)=slc2var(jj)     ! position in global array
             b%proc(jj)=sliceProcessed(jj)
             b%var2slc(jj)=var2slc(jj) ! index to slice variable
             b%trgvar(jj)=0.0D0
             b%var80(jj)=f%var80(b%var(jj))
          end do
          !if(mod_bdeb)write(*,*)myname,'Store dimensions.',&
          !     & b%ndim,b%nvar,allocated(f%dim80)
          do jj=1,b%ndim
             b%ind(jj)=slc2dim(jj)
             if (b%ind(jj).eq.0) then ! insane dimension index
                if(mod_bdeb)write(*,'(X,A,A,I0)')myname,&
                     & 'Insane dimension index, batch index: ',jj
             end if
             !write(*,*)myname,'Debug:',jj,b%ind(jj),b%ndim
             if (f%istart(b%ind(jj)).eq.f%istop(b%ind(jj))) then
                b%inc(jj)=0                 ! no valid increment
             else
                b%inc(jj)=1                 ! increment
             end if
             b%dim2slc(jj)=dim2slc(jj)     ! position in slice target array, 0 if none
             b%trgdim(jj)=0.0D0
             !if(mod_bdeb)write(*,*)myname,'Store dimension loop.',jj,slc2dim(jj)

             if(mod_bdeb)write(*,'(X,A,A,I0,A,I0,A)')myname,&
                  & 'Slice dimension: ',&
                  & jj," -> ",b%ind(jj),"   '"//&
                  & f%dim80(b%ind(jj))(1:&
                  & f%lend(b%ind(jj)))//"'"

             b%dim80(jj)=f%dim80(b%ind(jj))
          end do
          !if(mod_bdeb)write(*,*)myname,'Store in plan chain.'
          ! store in plan....
          b%prev => p%last%prev
          p%last%prev%next => b
          b%next => p%last
          p%last%prev => b

          if (b%ndim.eq.0) then
             if (mod_bdeb) then
                write(*,*)myname,'Invalid batch.'
                call model_printBatch(b,crc250,irc)
             end if
             irc=845
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Unable to determine search dimensions.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if

          b => null() ! release pointer
       else
          if(mod_bdeb)write(*,*)myname,'Already sliced...'
       end if
    end do
    if (allocated(sliceProcessed)) deallocate(sliceProcessed)
    if (allocated(slc2var)) deallocate(slc2var)
    if (allocated(slc2dim)) deallocate(slc2dim)
    if (allocated(innerDim)) deallocate(innerDim)
    if (allocated(var2slc)) deallocate(var2slc)
    if (allocated(dim2slc)) deallocate(dim2slc)
    if(mod_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine model_planBatch
  !
  !
  !
  subroutine model_deleteBatch(b,crc250,irc)
    type(mod_batch), pointer :: b
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_deleteBatch"
    if (.not.associated(b)) return
    if (allocated(b%ind)) deallocate(b%ind)
    if (allocated(b%inc)) deallocate(b%inc)
    if (allocated(b%dim2slc)) deallocate(b%dim2slc)
    if (allocated(b%trgdim)) deallocate(b%trgdim)
    if (allocated(b%var)) deallocate(b%var)
    if (allocated(b%val)) deallocate(b%val)
    if (allocated(b%proc)) deallocate(b%proc)
    if (allocated(b%var2slc)) deallocate(b%var2slc)
    if (allocated(b%trgvar)) deallocate(b%trgvar)
    b%ndim=0
    b%nvar=0
    b%prev%next => b%next
    b%next%prev => b%prev
    deallocate(b)
    return
  end subroutine model_deleteBatch
  !
  !
  !
  subroutine model_clearPlan(p,crc250,irc)
    type(mod_plan),pointer  :: p
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_clearPlan"
    type(mod_batch), pointer :: b,nb
    if(mod_bdeb)write(*,*)myname,' Entering.',associated(p)
    if (associated(p)) then
       if (allocated(p%innerDim)) deallocate(p%innerDim)
       if (allocated(p%trgvar)) deallocate(p%trgvar)
       if (allocated(p%slc2var)) deallocate(p%slc2var)
       if (allocated(p%slc2dim)) deallocate(p%slc2dim)
       if (allocated(p%proc)) deallocate(p%proc)
       !
       if(mod_bdeb)write(*,*)myname,'Deleting batch jobs.',associated(p%first)
       b => p%first%next
       do while ( .not.associated(b,target=p%last))
          nb=>b%next
          call model_deleteBatch(b,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from deleteBatch.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          b => nb
       end do
    end if
    if(mod_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine model_clearPlan
  !
  !
  subroutine model_printPlan(p,crc250,irc)
    type(mod_plan),pointer  :: p
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_printPlan"
    type(mod_batch), pointer :: b,nb
    if(mod_bdeb)write(*,*)myname,' Entering.',associated(p)
    if (associated(p)) then
       b => p%first%next
       do while ( .not.associated(b,target=p%last))
          nb=>b%next
          call model_printBatch(b,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from printBatch.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          b => nb
       end do
    end if
    if(mod_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine model_printPlan
  !
  subroutine model_printBatch(b,crc250,irc)
    type(mod_batch), pointer :: b
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_printBatch"
    integer :: ndim,ii
    character*250 :: buff250, nuff250
    integer :: lenb,lenn,lend,lenv
    integer, external :: length
    !
    if(mod_bdeb)write(*,*)myname,' Entering.',irc,b%ndim,b%nvar
    buff250="Dims=("
    call chop0(buff250,250)
    lenb=length(buff250,250,lenb)
    do ii=1,b%ndim
       call chop0(b%dim80(ii),80)
       lend=length(b%dim80(ii),80,10)
       nuff250=b%dim80(ii)(1:lend)
       call chop0(nuff250,250)
       lenn=length(nuff250,250,10)
       buff250=buff250(1:lenb)//nuff250(1:lenn)//":"
       call chop0(buff250,250)
       lenb=length(buff250,250,lenb)
    end do
    buff250=buff250(1:lenb)//") Vars=("
    call chop0(buff250,250)
    lenb=length(buff250,250,lenb)
    do ii=1,b%nvar
       call chop0(b%var80(ii),80)
       lenv=length(b%var80(ii),80,10)
       nuff250="("//b%var80(ii)(1:lenv)//")"
       call chop0(nuff250,250)
       lenn=length(nuff250,250,10)
       buff250=buff250(1:lenb)//nuff250(1:lenn)//":"
       call chop0(buff250,250)
       lenb=length(buff250,250,lenb)
    end do
    buff250=buff250(1:lenb)//")"
    call chop0(buff250,250)
    lenb=length(buff250,250,lenb)
    write(*,'(A)') buff250(1:lenb)
    if(mod_bdeb)write(*,*)myname,' Done.',irc
    return
  end subroutine model_printBatch
  !
  !
  subroutine model_initPlan(p,css,f,crc250,irc)
    type(mod_plan),pointer  :: p
    type(mod_session), pointer :: css   ! current session
    type(mod_file),pointer :: f   ! current file
    character*250 :: crc250
    integer :: ii, jj
    integer :: leng, lenv, lend
    integer, external :: length
    logical :: lmd
    integer :: irc
    character*25 :: myname="model_initPlan"
    !
    if(mod_bdeb)write(*,*)myname,' Entering.',associated(p),&
               & allocated(p%slc2dim),associated(p%last),irc
    p%nvar=f%nvar
    p%ndim=f%ndim
    allocate(p%innerDim(p%ndim),p%trgvar(f%nvar),&
         & p%slc2var(css%csli),&
         & p%slc2dim(css%csli),p%proc(p%nvar),p%first,p%last,stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Unable to allocate batch contents.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    p%first%next => p%last
    p%last%prev => p%first
    !
    lmd=(css%csli.eq.0)  ! all dimensions are inner if no slice variables are defined...
    do ii=1,f%ndim
       p%innerDim(ii)=lmd
    end do
    !
 ! global dimension is used?
    do ii=1,f%nvar
       p%proc(ii)=.false.
       p%trgvar(ii)=0.0D0
    end do
    !
    if(mod_bdeb)write(*,*)myname,'Analysing slice variables.',lmd,css%csli
    do ii=1,css%csli
       p%slc2var(ii)=0 ! global variable index
       p%slc2dim(ii)=0 ! global dimension index
       leng=css%sli_lenv(ii)
       if (leng.gt.2) then
          if (css%sli_v80(ii)(1:1).eq."(".and.css%sli_v80(ii)(leng:leng).eq.")") then
             do jj=1,f%ndim
                lend=length(f%dim80(jj),80,10)
                if (css%sli_v80(ii)(2:leng-1).eq.f%dim80(jj)(1:lend)) then
                   
                   if(mod_bdeb)write(*,*) myname,'Dim: "'//css%sli_v80(ii)(2:leng-1)//&
                        & '"  "'//f%dim80(jj)(1:lend)//'"',ii,jj

                   p%slc2dim(ii)=jj ! global dimension index
                end if
             end do
          end if
       end if
       if (p%slc2dim(ii).eq.0) then ! not a dimension, must be a variable
          do jj=1,f%nvar ! global variable index
             lenv=length(f%var80(jj),80,10)
             if (css%sli_v80(ii)(1:leng).eq.f%var80(jj)(1:lenv)) then
                p%slc2var(ii)=jj
                if(mod_bdeb)write(*,'(X,A,A,I3,A,I3,A)') myname,&
                     & 'Slice variable: ',&
                     & ii,' -> ',jj,&
                     & "  '"//css%sli_v80(ii)(1:leng)//"'"
             end if
          end do
       end if
       if (p%slc2dim(ii).eq.0.and.p%slc2var(ii).eq.0) then ! not dimension nor variable...
          if(mod_bdeb)write(*,*) myname,'Unrecognised slice ignored:',&
               & css%sli_v80(ii)(1:leng)
       end if
    end do
    !
    if(mod_bdeb)write(*,*)myname,'done.',irc
    RETURN
  END subroutine model_initPlan
  !
  !
  !
  logical function model_overlaps(ndim,batch,var)
    integer :: ndim
    logical batch(ndim)
    type(mod_variable), pointer :: var
    integer ii
    model_overlaps=.false.
    do ii=1,var%ndim
       if (batch(var%ind(ii))) then
          model_overlaps=.true.
       end if
    end do
  end function model_overlaps
  !
  !
  !###############################################################################
  ! SLICE ROUTINES
  !###############################################################################
  ! slice current file
  !
  subroutine model_sliceTable(css,bok,crc250,irc)
    type(mod_session), pointer :: css !  current session
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*25 :: myname="model_sliceTable"
    logical :: bdone
    if(mod_bdeb)write(*,*)myname,' Entering.',&
         & associated(css%currentFile),css%locready
    bok=.false.
    if (.not.associated(css%currentFile)) return
    ! make location array
    call model_makeLocList(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from model_makeLocList.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    call model_allocateTable(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from 'allocateTable'.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    if(mod_bdeb)write(*,*)myname,' Locating.',css%locready
    call model_colocateTable(css,css%nloc,css%locData,css%currentFile,bok,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from colocateTable.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    if(mod_bdeb)write(*,*)myname,' Done.',bok
    return
  end subroutine model_sliceTable
  !
  subroutine model_openCurrentFile(css,crc250,irc)
    type(mod_session), pointer :: css   !  current session
    character*250 :: crc250
    integer :: irc
    logical :: bok
    character*25 :: myname="model_openCurrentFile"
    type(mod_file),pointer :: f ! stack item to process
    type(mod_plan),pointer :: p => null()
    f => css%currentFile
    if (associated(f)) then
       call model_openFile(css,f,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from openFile.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       call model_readInventory(css,f,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from readInventory.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    return
  end subroutine model_openCurrentFile
  !
  subroutine model_closeCurrentFile(css,crc250,irc)
    type(mod_session), pointer :: css   !  current session
    character*250 :: crc250
    integer :: irc
    logical :: bok
    character*25 :: myname="model_closeCurrentFile"
    type(mod_file),pointer :: f ! stack item to process
    type(mod_plan),pointer :: p => null()
    f => css%currentFile
    if (associated(f)) then
       call model_closeFile(css,f,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from openFile.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    return
  end subroutine model_closeCurrentFile
  !
  subroutine model_colocateTable(css,nloc,loc,f,bok,crc250,irc)
    type(mod_session), pointer :: css   !  current session
    integer :: nloc                 ! number of locations
    type(mod_locPointer) :: loc(nloc)   !  location pointer
    type(mod_file),pointer :: f ! stack item to process
    character*250 :: crc250
    integer :: irc
    logical :: bok
    character*25 :: myname="model_colocateTable"
    type(mod_plan),pointer :: p => null()
    !
    if(mod_bdeb)write(*,*)myname,' Entering.',irc
    bok=.true.
    !
    ! Connect file-variables => target-variables
    !
    if (bok) then
       call model_setTargetIndex(css,f,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from fileTargetIndex.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    !
    ! analyse slice
    !
    if (bok) then
       call model_clearPlan(p,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from clearPlan.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       allocate(p,stat=irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Unable to allocate plan.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       call model_planBatch(css,f,p,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from planBatch.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    !
    ! dump results
    !
    if (bok) then
       !nloc=min(1,nloc) ! debug
       call model_makeTable(css,nloc,loc,f,p,bok,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from makeTable.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    if (bok) then
       call model_clearPlan(p,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from clearPlan.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    if(mod_bdeb)write(*,*)myname,' Done.',bok
    return
  end subroutine model_colocateTable

  !XXXXXXXXXXXXXXXXXX slicetable generates oval(:,:), oset(:,:)- gives value of targets at all locations...
  ! Store oval(:,:) and oset(:,:) in model_session, to be retrieved later...
  ! call model_getSlice(css,nloc,ntrg,oval,oset)
  !
  !###############################################################################
  ! GRID SECTION ROUTINES (WHEN VARIABLE GRID IS TOO LARGE FOR MEMORY)
  !###############################################################################
  !
  logical function  model_locOffGrid(v,loc) ! is location outside grid section?
    type(mod_variable), pointer :: v
    type(mod_location), pointer :: loc
    character*25 :: myname="model_locOffGrid"
    integer :: ii
    model_locOffGrid=.false.
    do ii=1,v%ndim
       if (loc%lstart(v%ind(ii)).lt.v%istart(ii).or.&
            & (loc%lstop(v%ind(ii))-v%istart(ii)).ge.v%icount(ii)) then
          if (mod_bdeb) write(*,*)myname,'Grid.',&
               & loc%lstart(v%ind(ii)),loc%lstop(v%ind(ii)),&
               & v%istart(ii),v%istart(ii)+v%icount(ii)-1
          model_locOffGrid=.true.
       end if
    end do
    return
  end function model_locOffGrid
  !
  ! set variable grid to the search-cell...
  !
  logical function model_setLocGrid(v,loc,p)
    type(mod_location), pointer :: loc
    type(mod_variable), pointer :: v
    type(mod_plan), pointer :: p
    character*25 :: myname="model_setLocGrid"
    integer :: jj
    if (loc%bok.and.loc%search.eq.0) then
       do jj=1,v%ndim
          if (p%innerDim(v%ind(jj))) then
             v%istart(jj)=loc%lstart(v%ind(jj))
             v%icount(jj)=loc%lstop(v%ind(jj))-loc%lstart(v%ind(jj))+1
          end if
       end do
       if (mod_bdeb)then
          do jj=1,v%ndim
             write(*,*)myname,'Dim:',jj,v%istart(jj),v%istart(jj)+v%icount(jj)-1,p%innerDim(v%ind(jj)),&
                  & nint(loc%rpos(v%ind(jj))),loc%lstart(v%ind(jj)),loc%lstop(v%ind(jj)),&
                  & loc%intpf(v%ind(jj))
          end do
          write(*,*)myname,"Valid location at '"//v%var80(1:v%lenv)//"' dimlen=",model_varLen(v)," ndims=",v%ndim
       end if
       model_setLocGrid=.true.
    else
       model_setLocGrid=.false.
       if (mod_bdeb)write(*,*)myname,"Invalid location at '"//v%var80(1:v%lenv)//"'"
    end if
    return
  end function model_setLocGrid
  !
  ! set variable grid to the search-cell...
  !
  subroutine model_resetGrid(f,v)
    type(mod_file), pointer :: f
    type(mod_variable), pointer :: v
    type(mod_plan), pointer :: p
    character*25 :: myname="model_resetGrid"
    integer :: jj
    do jj=1,v%ndim
       v%istart(jj)=f%istart(v%ind(jj))
       v%icount(jj)=f%istop(v%ind(jj))-f%istart(v%ind(jj))+1
    end do
    if (mod_bdeb)then
       do jj=1,v%ndim
          write(*,*)myname,'Dim:',jj,v%istart(jj),v%istart(jj)+v%icount(jj)-1
       end do
    end if
    return
  end subroutine model_resetGrid
  !
  integer*8 function model_varLen(v)
    type(mod_variable), pointer :: v
    integer :: jj
    model_varLen=1
    do jj=1,v%ndim
       model_varLen=model_varLen*(v%icount(jj))
    end do
    return
  end function model_varLen
  !
  integer*8 function model_fileLen(f,v)
    type(mod_file), pointer :: f
    type(mod_variable), pointer :: v
    integer :: jj
    model_fileLen=1
    do jj=1,v%ndim
       model_fileLen=model_fileLen*(f%istop(v%ind(jj))-f%istart(v%ind(jj))+1)
    end do
    return
  end function model_fileLen
  !
  !###############################################################################
  ! TABLE ROUTINES
  !###############################################################################
  !
  subroutine model_makeTable(css,nloc,lp,f,p,bok,crc250,irc)
    type(mod_session), pointer :: css ! current session
    integer :: nloc                   ! number of locations
    type(mod_locPointer) :: lp(nloc) ! location pointer
    type(mod_file),pointer :: f
    type(mod_plan),pointer :: p
    logical :: bok ! was any data printed
    character*250 :: crc250
    integer :: irc
    type(mod_batch), pointer :: b
    character*25 :: myname="model_makeTable"
    integer :: ii,ll,varid
    character*250 :: var250
    character*50 :: sval50
    character*80 :: trg80
    integer :: lenv, lend, lent, lens, itrg
    integer, external :: length
    type(mod_variable),pointer :: v
    type(mod_location), pointer :: loc
    integer*8, parameter :: maxlen=2147483647
    !integer*8, parameter :: maxlen=1000
    integer*8 :: dimlen
     if(mod_bdeb)write(*,*)myname,' Entering.',nloc
    !
    ! initialise location positions
    !
    do ll=1,nloc
       !
       ! initialise location position
       !
       call model_initLocPos(css,f,p,b,lp(ll)%ptr,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from initLocPos.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end do
    !
    ! mark all variables as not-processed
    !
    do varid = 1, p%nvar
       p%proc(varid)=.false.
    end do
    !
    ! loop over batch-jobs
    !
    if(mod_bdeb)write(*,*)myname,'Batch job loop.'
    b=>p%first%next
    do while ( .not.associated(b,target=p%last))
       !
       ! read batch-variables into memory
       !
       do ii = 1, b%nvar
          varid=b%var(ii)
          call model_readVariable(css,f,varid,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from readVariable.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
       end do
       !
       ! loop over locations and determine slice indexes
       !
       if(mod_bdeb)write(*,*)myname,'Location loop.',nloc,b%nvar
       do ll=1,nloc
          loc => lp(ll)%ptr
          if (loc%bok.and.loc%search .eq. 0) then
             !
             ! search for batch-dimension values
             !
             call model_search(css,f,p,b,loc,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from search.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
          end if
       end do
       b=>b%next
    end do
    !
    ! clear batch-variables from memory
    !
    b=>p%first%next
    do while ( .not.associated(b,target=p%last))
       if(mod_bdeb)write(*,*)myname,'Variable loop.',b%nvar
       do ii = 1, b%nvar 
          varid=b%var(ii)
          p%proc(varid)=.true.
          var250=model_getVar250(f,varid)
          lenv=length(var250,250,10)
          if(mod_bdeb)write(*,*)myname,"Processed "//var250(1:lenv),&
               & varid,p%proc(varid)
          !
          itrg=css%sli_2trg(b%var2slc(ii))
          if (itrg.ne.0) then
             trg80=css%trg80(itrg)
             lent=css%trg_lent(itrg)
          else
             trg80=""
             lent=0
          end if
          !
          do ll=1,nloc
             if (lp(ll)%ptr%bok) then
                call model_getTarget50(lp(ll)%ptr%sli_val(b%var2slc(ii)),lens,sval50)
                !
                call model_setLocVal(css,f,varid,lenv,var250,&
                     & lent,trg80,lens,sval50,lp(ll)%ptr,p,bok,crc250,irc)
                if (irc.ne.0) then
                   call model_errorappend(crc250,myname)
                   call model_errorappend(crc250," Error return from setLoc.")
                   call model_errorappendi(crc250,irc)
                   call model_errorappend(crc250,"\n")
                   return
                end if
             end if
          end do
          varid=b%var(ii)
          call model_clearVariable(f%var(b%var(ii))%ptr,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from clearVariable.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          if(mod_bdeb)write(*,*)myname,'Processed done:',p%proc(1)
       end do
       !
       ! end loop over batch jobs
       !
       b=>b%next
    end do
    !
    if(mod_bdeb)write(*,*)myname,'Looping over remaining variables.',f%nvar, p%nvar
    write(*,*)myname,'Looping over remaining variables.',f%nvar, p%nvar
    !
    ! loop over remaining variables
    !
    VAR: do varid = 1, f%nvar
       v => f%var(varid)%ptr
       if (p%proc(varid)) then
          if(mod_bdeb)write(*,*)myname,'Already processed:',v%var80(1:v%lenv)
          cycle VAR ! already processed
       end if
       if (v%itrg.eq.0) then ! this is not a target variable
          if(mod_bdeb)write(*,*)myname,' ** Not a target:',v%var80(1:v%lenv)
          cycle VAR
       end if
       call model_resetGrid(f,v) ! reset grid in case earlier call to setLocGrid
       var250=model_getvar250(f,varid)
       lenv=length(var250,250,10)
       dimlen=model_varLen(v)
       if(mod_bdeb)write(*,'(X,A,X,A,X,A,"(",I0,")")')myname,' Target variable:',v%var80(1:v%lenv),dimlen
       write(*,'(X,A,X,A,X,A,"(",I0,")")')myname,' Target variable:',v%var80(1:v%lenv),dimlen
       if (dimlen.gt.maxlen) then ! read variable in segments
          do ll=1,nloc
             loc=>lp(ll)%ptr
             if (model_setLocGrid(v,loc,p)) then
                !
                ! read variable into memory
                !
                call model_readVariable(css,f,varid,crc250,irc)
                if (irc.ne.0) then
                   call model_errorappend(crc250,myname)
                   call model_errorappend(crc250," Error return from readVariable.")
                   call model_errorappendi(crc250,irc)
                   call model_errorappend(crc250,"\n")
                   return
                end if
                !
                ! should match v%itrg
                itrg=model_getTargetIndex(css,v)
                if (itrg.ne.0) then
                   trg80=css%trg80(itrg)
                   lent=css%trg_lent(itrg)
                else
                   trg80=""
                   lent=0
                end if
                !
                ! make output for variables at location
                !
                lens=0
                call model_setLocVal(css,f,varid,lenv,var250,&
                     & lent,trg80,lens,sval50,lp(ll)%ptr,p,bok,crc250,irc)
                if (irc.ne.0) then
                   call model_errorappend(crc250,myname)
                   call model_errorappend(crc250," Error return from setLoc.")
                   call model_errorappendi(crc250,irc)
                   call model_errorappend(crc250,"\n")
                   return
                end if
             end if
             !
             ! clear variable from memory
             !
             call model_clearVariable(v,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from clearVariable.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
          end do ! locations
          !
       else ! read variables once...
          !
          ! read variable into memory
          !
          call model_readVariable(css,f,varid,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from readVariable.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          !
          ! should match f%var(varid)%ptr%itrg
          itrg=model_getTargetIndex(css,f%var(varid)%ptr)
          if (itrg.ne.0) then
             trg80=css%trg80(itrg)
             lent=css%trg_lent(itrg)
          else
             trg80=""
             lent=0
          end if
          !
          ! loop over locations
          !
          do ll=1,nloc
             loc => lp(ll)%ptr
             if (loc%bok) then
                !
                ! make output for variables at location
                !
                lens=0
                call model_setLocVal(css,f,varid,lenv,var250,&
                     & lent,trg80,lens,sval50,loc,p,bok,crc250,irc)
                if (irc.ne.0) then
                   call model_errorappend(crc250,myname)
                   call model_errorappend(crc250," Error return from setLoc.")
                   call model_errorappendi(crc250,irc)
                   call model_errorappend(crc250,"\n")
                   return
                end if
             end if
             !
             ! end loop over locations
             !
          end do
          !
          ! clear variable from memory
          !
          call model_clearVariable(v,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from clearVariable.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
       end if
       !
       ! end loop over remaining variables
       !
    end do VAR
    ! !
    ! ! clear location positions
    ! !
    ! do ll=1,nloc
    !    !
    !    ! clear location position
    !    !
    !    call model_initLocPos(css,f,p,b,lp(ll)%ptr,crc250,irc)
    !    if (irc.ne.0) then
    !       call model_errorappend(crc250,myname)
    !       call model_errorappend(crc250," Error return from initLocPos.")
    !       call model_errorappendi(crc250,irc)
    !       call model_errorappend(crc250,"\n")
    !       return
    !    end if
    ! end do
    if(mod_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine model_makeTable
  !
  subroutine model_setLocVal(css,f,varid,lenv,&
               & var250,lent,trg80,lens,sval50,loc,p,bok,crc250,irc)
    type(mod_session), pointer :: css   ! current session
    type(mod_file),pointer :: f   ! current file
    integer :: varid                    ! current variable
    integer :: lenv
    character*250 :: var250
    integer :: lent
    character*80 :: trg80
    integer :: lens
    character*50 :: sval50
    type(mod_location),pointer :: loc   ! location
    type(mod_plan),pointer :: p         ! pointer to the current plan
    logical :: bok                      ! was any data printed?
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setLocVal"
    real :: val
    logical :: bout, binn
    integer ::lenb,lenp,len1
    character*50 :: s1
    integer, external :: length
    character*250 :: buff250, loc250,pos250
    type(mod_variable),pointer :: v   ! variable pointer
    integer :: ninn, nout, pinn,cinn,cout
    real :: sum,tsum,wgt,twgt,tval
    integer, allocatable :: inn(:), out(:), ind(:)
    integer ::cnt,cval,ctot,jj
    logical :: first,set
    !
    ! check if location is ok
    if (.not.loc%bok .or. loc%search .ne. 0) then
       if (mod_bdeb) write(*,*)myname,'Ignoring loc:',loc%bok,loc%search
       return
    end if
    v => f%var(varid)%ptr
    !
    ! Check if location is off grid section
    if (model_locOffGrid(v,loc)) then
       if (mod_bdeb) write(*,*)myname,'Loc off grid.',loc%locid
       return
    end if
    !
    ! get outer and inner loop indexes
    !
    if(allocated(out)) deallocate(out)
    if(allocated(inn)) deallocate(inn)
    if(allocated(ind)) deallocate(ind)
    allocate(out(p%ndim),inn(p%ndim),ind(p%ndim))
    !
    ! Split dimensions into inner and outer based on plan...
    !
    call model_planLoop(v,p,nout,out,ninn,inn,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from planLoop.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    !
    ! make target dimension index...
    !
    call model_planTrgInd(css,ninn,inn,ind,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from planTrgInd.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    !
    if (mod_bdeb) then
       write(*,'(X,A,3(A,I0))')myname,&
            & " Loc:",loc%iloc,", ninn=",ninn,", nout=",nout
       do jj=1,ninn
          write(*,*)myname,'Inner:',jj,inn(jj), &
               & css%currentFile%dim80(inn(jj))(1:css%currentFile%lend(inn(jj)))
       end do
       do jj=1,nout
          write(*,*)myname,'Outer:',jj,out(jj), &
               & css%currentFile%dim80(out(jj))(1:css%currentFile%lend(out(jj)))
       end do
    end if
    !
    ! write value information
    !
    twgt=0.0D0
    tsum=0.0D0
    cnt=0
    call model_resetPos(nout,out,loc)
    bout=.false.
    do while (.not. bout) ! OUTER DIMENSION LOOP
       pinn=0   ! previous  dimension index
       call model_resetPos(ninn,inn,loc)
       ctot=0 ! total number of grid points
       cval=0 ! number of valid grid points
       binn=.false.
       do while (.not. binn) ! INNER SEARCH LOOP
          set = model_getLocValue(v,loc,val,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from getLocValue.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          if(mod_bdeb)write(*,*)myname," Loc:",loc%iloc, "Value:",val
          cnt=cnt+1
          ctot=ctot+1
          if(mod_bdeb)write(*,*)myname," Loc:",loc%iloc,"Calling Incrementing position."
          if (set) then ! val.ne.nf_fill_double
             wgt=model_getWeight(ninn,inn,loc) ! current weight
             tsum=tsum+wgt*val
             twgt=twgt+wgt
             cval=cval+1
          end if
          !if(mod_bdeb)write(*,*) myname,'Weight:',wgt,twgt,val
          cinn=model_incrementPos(ninn,inn,loc) ! current inn
          binn=((cnt.gt.250).or.(cinn.eq.0))
          if(mod_bdeb)write(*,'(X,A,3(A,I0),A,L1,2(A,F15.3))')myname," Loc:",loc%iloc,&
               & ' Count inner A newLoc=',cinn,' cnt=',cnt,' done=',binn,' wgt=',wgt,' vsum=',tsum
       end do
       if (twgt.gt.1.0D-10.and.ctot.eq.cval.and.ctot.ge.1) then
          tval=tsum/twgt
          if (v%itrg.ne.0) then
             call model_setLocTrgVal(css,ninn,inn,ind,tval,loc,v%itrg,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from setLocTrgVal.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
             call model_setOutVal(css,tval,v%itrg,loc%iloc,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from setOutVal.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
          end if
       end if
       cout=model_incrementPos(nout,out,loc)
       bout=(cnt.gt.32.or.(cout.eq.0))
       if(mod_bdeb)write(*,*)myname," Loc:",loc%iloc,'Count outer:',cout,cnt,bout
    end do
    if(allocated(out)) deallocate(out)
    if(allocated(inn)) deallocate(inn)
    if(allocated(ind)) deallocate(ind)
    return
  end subroutine model_setLocVal
  !
  !###############################################################################
  ! OUTPUT-XML ROUTINES
  !###############################################################################
  subroutine model_sliceXML(css,ounit,bok,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: ounit
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*25 :: myname="sliceXML"
    logical :: bdone
    if(mod_bdeb)write(*,*)myname,' Entering.',&
         & associated(css%currentFile),' locready=',css%locready
    bok=.false.
    if (.not.associated(css%currentFile)) then
       if(mod_bdeb)write(*,*)myname,' No file specified, returning.'
       return
    end if
    ! make location array
    call model_makeLocList(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from model_makeLocList.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    call model_allocateTable(css,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from 'allocateTable'.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if 
    if(mod_bdeb)write(*,*)myname,' locready=',css%locready
    call model_colocateXML(css,ounit,css%nloc,css%locData,css%currentFile,bok,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from colocateXML.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    if(mod_bdeb)write(*,*)myname,' Done.',bok,' locready=',css%locready
    return
  end subroutine model_sliceXML
  !
  subroutine model_colocateXML(css,ounit,nloc,lp,f,bok,crc250,irc)
    type(mod_session), pointer :: css   !  current session
    integer :: ounit
    integer :: nloc                 ! number of locations
    type(mod_locPointer) :: lp(nloc)   !  location pointer
    type(mod_file),pointer :: f ! stack item to process
    character*250 :: crc250
    integer :: irc
    logical :: bok
    character*25 :: myname="model_colocateXML"
    type(mod_plan),pointer :: p => null()
    !
    if(mod_bdeb)write(*,*)myname,' Entering.',irc,' locready=',css%locready
    bok=.true.
    !
    ! Connect file-variables => target-variables
    !
    if(mod_bdeb)write(*,*)myname,' D locready=',css%locready
    if (bok) then
       call model_setTargetIndex(css,f,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from fileTargetIndex.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    !
    ! analyse slice
    !
    if(mod_bdeb)write(*,*)myname,' E locready=',css%locready
    if (bok) then
       call model_clearPlan(p,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from clearPlan.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       allocate(p,stat=irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Unable to allocate plan.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       call model_planBatch(css,f,p,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from planBatch.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    !
    ! dump results
    !
    if(mod_bdeb)write(*,*)myname,' Making XML. locready=',css%locready
    if (bok) then
       !nloc=min(1,nloc) ! debug
       call model_makeXML(css,ounit,nloc,lp,f,p,bok,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from makeTable.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    if(mod_bdeb)write(*,*)myname,' F locready=',css%locready
    if (bok) then
       call model_clearPlan(p,crc250,irc)
       if (irc.ne.0) then
          bok=.false.
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from clearPlan.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end if
    if(mod_bdeb)write(*,*)myname,' Done. locready=',css%locready
    return
  end subroutine model_colocateXML
  !
  ! colocate fields together with locations and write xml to standard output
  !
  subroutine model_makeXML(css,ounit,nloc,lp,f,p,bok,crc250,irc)
    type(mod_session), pointer :: css ! current session
    integer :: ounit                  ! output unit
    integer :: nloc                   ! number of locations
    type(mod_locPointer) :: lp(nloc) ! location pointer
    type(mod_file),pointer :: f
    type(mod_plan),pointer :: p
    logical :: bok ! was any data printed
    character*250 :: crc250
    integer :: irc
    type(mod_batch), pointer :: b
    character*25 :: myname="model_makeXML"
    integer :: ii,ll,varid
    character*250 :: var250
    character*50 :: sval50
    character*80 :: trg80
    integer :: lenv, lend, lens, lent, itrg
    type(mod_location), pointer :: loc
    type(mod_variable), pointer :: v
    !integer*8, parameter :: maxlen=2147483647
    integer*8, parameter :: maxlen=1000
    integer*8 :: dimlen
    integer, external :: length
    if(mod_bdeb)write(*,*)myname,' Entering. locready=',css%locready,nloc
    !
    ! initialise location positions
    !
    do ll=1,nloc
       !
       ! initialise location position
       !
       call model_initLocPos(css,f,p,b,lp(ll)%ptr,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from initLocPos.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    end do
    !
    if(mod_bdeb)write(*,*)myname,'G locready=',css%locready
    ! start xml output
    !
    if (nloc.eq.0) then
       write(ounit,'(3X,A,I0,A)')"<model loc='",nloc,"'/>"
    else
       write(ounit,'(3X,A,I0,A)')"<model loc='",nloc,"'>"
       !
       ! write general information
       !

       !
       ! mark all variables as not-processed
       !
       do varid = 1, p%nvar
          p%proc(varid)=.false.
       end do
       !
       ! loop over batch-jobs
       !
       if(mod_bdeb)write(*,*)myname,'Batch job loop.'
       b=>p%first%next
       do while ( .not.associated(b,target=p%last))
          !
          ! read batch-variables into memory
          !
          if(mod_bdeb)write(*,*)myname,'H locready=',css%locready
          do ii = 1, b%nvar
             varid=b%var(ii)
             call model_readVariable(css,f,varid,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from readVariable.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
          end do
          !
          ! loop over locations and determine slice indexes
          !
          if(mod_bdeb)write(*,*)myname,'I locready=',css%locready,nloc,b%nvar
          do ll=1,nloc
             loc => lp(ll)%ptr
             if (loc%bok.and.loc%search .eq. 0) then
                !
                ! search for batch-dimension values
                !
                call model_search(css,f,p,b,loc,crc250,irc)
                if (irc.ne.0) then
                   call model_errorappend(crc250,myname)
                   call model_errorappend(crc250," Error return from search.")
                   call model_errorappendi(crc250,irc)
                   call model_errorappend(crc250,"\n")
                   return
                end if
                if (mod_bdeb) then
                   call model_writeIgnoredLocXML(css,ounit,f,loc,p,crc250,irc)
                   if (irc.ne.0) then
                      call model_errorappend(crc250,myname)
                      call model_errorappend(crc250," Error return from writeSummaryXML.")
                      call model_errorappendi(crc250,irc)
                      call model_errorappend(crc250,"\n")
                      return
                   end if
                end if
             end if
          end do
          b=>b%next
       end do
       b=>p%first%next
       if(mod_bdeb)write(*,*)myname,'J locready=',css%locready
       do while ( .not.associated(b,target=p%last))
          !
          ! clear batch-variables from memory
          !
          if(mod_bdeb)write(*,*)myname,'K locready=',css%locready,b%nvar
          do ii = 1, b%nvar 
             varid=b%var(ii)
             p%proc(varid)=.true.
             if(mod_bdeb)write(*,*)myname,'Processed:',varid,p%proc(varid)
             var250=model_getVar250(f,varid)
             lenv=length(var250,250,10)
             if(mod_bdeb)write(*,*)myname,'Variable:'//var250(1:lenv)
             !
             itrg=css%sli_2trg(b%var2slc(ii))
             if (itrg.ne.0) then
                trg80=css%trg80(itrg)
                lent=css%trg_lent(itrg)
             else
                trg80=""
                lent=0
             end if
             !
             do ll=1,nloc
                !
                if (lp(ll)%ptr%bok) then
                   call model_getTarget50(lp(ll)%ptr%sli_val(b%var2slc(ii)),lens,sval50)
                   !
                   call model_setLocValXML(css,ounit,f,varid,lenv,var250,&
                        & lent,trg80,lens,sval50,lp(ll)%ptr,p,bok,crc250,irc)
                   if (irc.ne.0) then
                      call model_errorappend(crc250,myname)
                      call model_errorappend(crc250," Error return from setLocValXML.")
                      call model_errorappendi(crc250,irc)
                      call model_errorappend(crc250,"\n")
                      return
                   end if
                   !
                   call model_setLocRpos(f,varid,lp(ll)%ptr)
                   !
                end if
             end do
             varid=b%var(ii)
             call model_clearVariable(f%var(b%var(ii))%ptr,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from clearVariable.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
             if(mod_bdeb)write(*,*)myname,'Processed done:',p%proc(1)
          end do
          !
          ! end loop over batch jobs
          !
          b=>b%next
       end do
       !
       if(mod_bdeb)write(*,*)myname,'K locready=',css%locready,f%nvar, p%nvar
       !
       ! loop over remaining variables
       !
       VAR: do varid = 1, f%nvar
          v => f%var(varid)%ptr
          if (p%proc(varid)) then
             if(mod_bdeb)write(*,*)myname,'Already processed:',v%var80(1:v%lenv)
             cycle VAR ! already processed
          end if
          if (v%itrg.eq.0) then ! this is not a target variable
             if(mod_bdeb)write(*,*)myname,' ** Not a target:',v%var80(1:v%lenv)
             cycle VAR
          end if
          call model_resetGrid(f,v) ! reset grid in case earlier call to setLocGrid
          var250=model_getvar250(f,varid)
          lenv=length(var250,250,10)
          dimlen=model_varLen(v)
          if(mod_bdeb)write(*,'(X,A,X,A,X,A,"(",I0,")")')myname,' Target variable:',v%var80(1:v%lenv),dimlen
          if (dimlen.gt.maxlen) then ! read variable in segments
             do ll=1,nloc
                loc=>lp(ll)%ptr
                if (model_setLocGrid(v,loc,p)) then
                   !
                   ! read variable into memory
                   !
                   call model_readVariable(css,f,varid,crc250,irc)
                   if (irc.ne.0) then
                      call model_errorappend(crc250,myname)
                      call model_errorappend(crc250," Error return from readVariable.")
                      call model_errorappendi(crc250,irc)
                      call model_errorappend(crc250,"\n")
                      return
                   end if
                   !
                   ! should match v%itrg
                   itrg=model_getTargetIndex(css,v)
                   if (itrg.ne.0) then
                      trg80=css%trg80(itrg)
                      lent=css%trg_lent(itrg)
                   else
                      trg80=""
                      lent=0
                   end if
                   !
                   ! make XML for variables at location
                   !
                   lens=0
                   call model_setLocValXML(css,ounit,f,varid,lenv,var250,&
                        & lent,trg80,lens,sval50,lp(ll)%ptr,p,bok,crc250,irc)
                   if (irc.ne.0) then
                      call model_errorappend(crc250,myname)
                      call model_errorappend(crc250," Error return from setLocValXML.")
                      call model_errorappendi(crc250,irc)
                      call model_errorappend(crc250,"\n")
                      return
                   end if
                end if
                !
                ! clear variable from memory
                !
                call model_clearVariable(v,crc250,irc)
                if (irc.ne.0) then
                   call model_errorappend(crc250,myname)
                   call model_errorappend(crc250," Error return from clearVariable.")
                   call model_errorappendi(crc250,irc)
                   call model_errorappend(crc250,"\n")
                   return
                end if
                !
                ! end loop over locations
                !
             end do
          else ! read variables once
             !
             ! read variable into memory
             !
             call model_readVariable(css,f,varid,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from readVariable.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
             !
             itrg=model_getTargetIndex(css,v)
             if (itrg.ne.0) then
                trg80=css%trg80(itrg)
                lent=css%trg_lent(itrg)
             else
                trg80=""
                lent=0
             end if
             !
             ! loop over locations
             !
             do ll=1,nloc
                loc => lp(ll)%ptr
                if (loc%bok) then
                   !
                   ! make XML for variables at location
                   !
                   lens=0
                   call model_setLocValXML(css,ounit,f,varid,lenv,var250,&
                        & lent,trg80,lens,sval50,loc,p,bok,crc250,irc)
                   if (irc.ne.0) then
                      call model_errorappend(crc250,myname)
                      call model_errorappend(crc250," Error return from setLocValXML.")
                      call model_errorappendi(crc250,irc)
                      call model_errorappend(crc250,"\n")
                      return
                   end if
                end if
                !
                ! end loop over locations
                !
             end do
             !
             ! clear variable from memory
             !
             call model_clearVariable(v,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from clearVariable.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
          end if
       !
       ! end loop over remaining variables
       !
       end do VAR
       !
       ! stop xml output
       !
       write(ounit,'(3X,"</model>")')
    end if
    ! !
    ! ! clear location positions
    ! !
    ! if(mod_bdeb)write(*,*)myname,'L locready=',css%locready
    ! do ll=1,nloc
    !    ! clear location position
    !    call model_initLocPos(css,f,p,b,lp(ll)%ptr,crc250,irc)
    !    if (irc.ne.0) then
    !       call model_errorappend(crc250,myname)
    !       call model_errorappend(crc250," Error return from clearLocPos.")
    !       call model_errorappendi(crc250,irc)
    !       call model_errorappend(crc250,"\n")
    !       return
    !    end if
    ! end do
    if(mod_bdeb)write(*,*)myname,' Done locready=',css%locready
    return
  end subroutine model_makeXML
  !
  ! write location summary
  !
  subroutine model_writeIgnoredLocXML(css,ounit,f,loc,p,crc250,irc)
    type(mod_session), pointer :: css   ! current session
    integer :: ounit                  ! output unit
    type(mod_file),pointer :: f   ! current file
    type(mod_location),pointer :: loc   ! location
    type(mod_plan),pointer :: p         ! pointer to the current plan
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="writeIgnoredLocXML"
    character*250 :: buff250
    character*50 :: s1
    integer :: lenb,len1
    integer, external :: length
    if (loc%search .eq. 0) then
       ! write location information
       !
       write(s1,'(I0)')loc%locid
       call chop0(s1,50)
       len1=length(s1,50,10)
       buff250="id='"//s1(1:len1)//"' ignored='Search out of bounds for dimension:"// &
            & f%dim80(loc%search)(1:f%lend(loc%search))//"'"
       call chop0(buff250,250)
       lenb=length(buff250,250,10)
       write(ounit,'(4X,A)') "<field "//buff250(1:lenb)//"/>"
    end if
    return
  end subroutine model_writeIgnoredLocXML
  !
  ! make XML for variables at location
  !
  subroutine model_setLocValXML(css,ounit,f,varid,lenv,&
               & var250,lent,trg80,lens,sval50,loc,p,bok,crc250,irc)
    type(mod_session), pointer :: css   ! current session
    integer :: ounit                  ! output unit
    type(mod_file),pointer :: f   ! current file
    integer :: varid                    ! current variable
    integer :: lenv
    character*250 :: var250
    integer :: lent
    character*80 :: trg80
    integer :: lens
    character*50 :: sval50
    type(mod_location),pointer :: loc   ! location
    type(mod_plan),pointer :: p         ! pointer to the current plan
    logical :: bok                      ! was any data printed?
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setLocValXML"
    real :: val
    logical :: bout, binn
    integer ::lenb,lenp,lenl,len1
    character*50 :: s1
    integer, external :: length
    character*250 :: buff250, loc250,pos250
    type(mod_variable),pointer :: v   ! variable pointer
    integer :: ninn, nout, pinn,cinn,cout
    real :: sum,tsum,wgt,twgt,tval
    integer, allocatable :: inn(:), out(:),ind(:)
    integer ::cnt,cval,ctot,jj
    logical :: first,set
    if (.not.loc%bok .or. loc%search .ne. 0) return
    v => f%var(varid)%ptr
    !
    ! write location information
    !
    write(s1,'(I0)') loc%locid
    call chop0(s1,50)
    len1=length(s1,50,10)
    if (lent.ne.0) then
       loc250=" id='"//s1(1:len1)//"' target='"//trg80(1:lent)//"' "//sval50(1:lens)
    else
       loc250=" id='"//s1(1:len1)//"' "//sval50(1:lens)
    end if
    call chop0(loc250,250)
    lenl=length(loc250,250,10)
    !
    pos250=model_getPosWgt50(css,v,loc)
    call chop0(pos250,250)
    lenp=length(pos250,250,10)
    if (lenp.ne.0) then
       pos250=" pos='"//pos250(1:lenp)//"'"
       lenp=lenp+7
    end if
    !
    first=.true.
    !
    ! get outer and inner loop indexes
    !
    if(allocated(out)) deallocate(out)
    if(allocated(inn)) deallocate(inn)
    if(allocated(ind)) deallocate(ind)
    allocate(out(p%ndim),inn(p%ndim),ind(p%ndim))
    !
    ! plan the inner (search dimensions) and outer loops
    !
    call model_planLoop(v,p,nout,out,ninn,inn,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from planLoop.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    !
    call model_planTrgInd(css,ninn,inn,ind,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from planTrgInd.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    !
    if(mod_bdeb)write(*,*)myname,"Innout:",ninn,nout," Undef:",nf_fill_double
    if (mod_bdeb) then
       do jj=1,ninn
          write(*,*)myname,'Inner:',jj,inn(jj), &
               & css%currentFile%dim80(inn(jj))(1:css%currentFile%lend(inn(jj)))
       end do
       do jj=1,nout
          write(*,*)myname,'Outer:',jj,out(jj), &
               & css%currentFile%dim80(out(jj))(1:css%currentFile%lend(out(jj)))
       end do
    end if
    !
    ! write value information
    !
    twgt=0.0D0
    tsum=0.0D0
    cnt=0
    call model_resetPos(nout,out,loc)
    bout=.false.
    do while (.not. bout)
       pinn=0   ! previous  dimension index
       call model_resetPos(ninn,inn,loc)
       ctot=0 ! total number of grid points
       cval=0 ! number of valid grid points
       binn=.false.
       do while (.not. binn)
          if(mod_bdeb)write(*,*)myname,"Calling getLocValue."
          set = model_getLocValue(v,loc,val,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from getLocValue.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          cnt=cnt+1
          ctot=ctot+1
          if(mod_bdeb)write(*,*)myname,"Calling Incrementing position."
          if (set) then ! val.ne.nf_fill_double
             if (first) then
                write(ounit,'(4X,A)') "<field "//loc250(1:lenl)//" "//&
                     & var250(1:lenv)//pos250(1:lenp)//">"
                first=.false.
             end if
             wgt=model_getWeight(ninn,inn,loc) ! current weight
             buff250=model_getGrid250(f,v,loc,val,wgt,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from getGrid250.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
             call chop0(buff250,250)
             lenb=length(buff250,250,10)
             if (lenb.ne.0) then
                write(ounit,'(5X,A)') buff250(1:lenb)
             end if
             tsum=tsum+wgt*val
             twgt=twgt+wgt
             cval=cval+1
          end if
          !if(mod_bdeb)write(*,*) myname,'Weight:',twgt,wgt,val
          cinn=model_incrementPos(ninn,inn,loc) ! current inn
          binn=(cnt.gt.250.or.(cinn.eq.0))
          if(mod_bdeb)write(*,'(X,A,A,I0,X,I0,X,L1,X,F5.2,X,F10.3)')myname,&
               & 'Count inner B: ',cinn,cnt,binn,wgt,tsum
       end do
       if(mod_bdeb)write(*,*)myname,'Checking:',twgt,ctot,cval,v%itrg
       if (twgt.gt.1.0D-10.and.ctot.eq.cval.and.ctot.ge.1) then
          tval=tsum/twgt
          if (v%itrg.ne.0) then
             call model_setLocTrgVal(css,ninn,inn,ind,tval,loc,v%itrg,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from setLocTrgVal.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
             call model_setOutVal(css,tval,v%itrg,loc%iloc,crc250,irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Error return from setOutVal.")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
          end if
          buff250=model_getInt250(f,v,loc,tval,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from getInt250.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          call chop0(buff250,250)
          lenb=length(buff250,250,10)
          if (lenb.ne.0) then
             write(ounit,'(5X,A)') buff250(1:lenb)
          end if
       end if
       cout=model_incrementPos(nout,out,loc)
       bout=(cnt.gt.32.or.(cout.eq.0))
       if(mod_bdeb)write(*,*)myname,'Count outer:',cout,cnt,bout
    end do
    if (.not.first) then
       write(ounit,'(4X,A)') "</field>"
    end if
    if(allocated(out)) deallocate(out)
    if(allocated(inn)) deallocate(inn)
    if(allocated(ind)) deallocate(ind)
    return
  end subroutine model_setLocValXML
  !
  ! write location XML to file
  !
  subroutine model_writexml(css,ounit,locid,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: ounit
    integer :: locid
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_writexml"
    character*250 :: buff250
    type(mod_location), pointer :: loc
    integer, external :: length
    character*50 :: s1,s2,s3,s4
    integer :: len1,len2,len3,len4, pos,ii
    !
    pos=locid-css%locoffset
    loc => css%locData(pos)%ptr
    if (associated(loc)) then
       if (loc%bok) then
          write(ounit,'(3X,A,I0,A)') "<location id='",loc%locid,"' status='ok'>"
          write(ounit,'(4X,A,I0,A)') "<model targets='",loc%ctrg,"'>"
          do ii=1,loc%ctrg
             if (loc%trg_set(ii)) then
                call model_wash(loc%trg_val(ii),s3,len3)
                if (len3.ne.0) then
                   s3=" value='"//s3(1:len3)//"'"
                   len3=len3+9
                end if
             else
                len3=0
             end if
             write (ounit,'(5X,A)') "<target name='"//&
                  & css%trg80(ii)(1:css%trg_lent(ii))//"'"//&
                  & s3(1:len3)//">"
          end do
          write(ounit,'(4X,A)') "</model>"
          write(ounit,'(4X,A,I0,A)') "<observation targets='",loc%cobs,"'>"
          do ii=1,loc%cobs
             call model_wash(loc%obs_val(ii),s3,len3)
             if (len3.ne.0) then
                s3=" value='"//s3(1:len3)//"'"
                len3=len3+9
             end if
             write (ounit,'(5X,A)') "<target name='"//&
                  & css%obs_var(ii)(1:css%obs_lenv(ii))//"'"//&
                  & s3(1:len3)//">"
          end do
          write(ounit,'(4X,A)') "</observation>"
          write(ounit,'(3X,A)') "</location>"
       else
          write(ounit,'(3X,A,I0,A)') "<location id='",loc%locid,"' status='rejected'/>"
       end if
    end if
    return
  end subroutine model_writexml
  !
  subroutine model_writeModelDataXML(css,ounit,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: ounit                  ! output unit
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="writeModelDataXML"
    integer :: len1,len2,len3,len4
    integer, external :: length
    character*50 :: s1,s2,s3,s4
    integer :: ii
    write(ounit,'(1X,A,I0,A)')"<model targets='",css%ctrg,"'>"
    if (css%trg_orm(0).ne.0) then
       write(ounit,'(2X,A,I0,A)')"<check removed='",css%trg_orm(0),&
               & "' reason='outside target limits.'>"
    end if
    do ii=1,css%ctrg
       if (css%trg_orm(ii).ne.0) then
          s1=css%trg80(ii)(1:50) ; call chop0(s1,50); len1=length(s1,50,10)!id
          IF (len1.ne.0) then
             s1=" name='"//s1(1:len1)//"'"
             len1=len1+8
          end if
          if (css%trg_minset(ii)) then
             call model_wash(css%trg_minval(ii),s3,len3)
             if (len3.ne.0) then
                s3=" min='"//s3(1:len3)//"'"
                len3=len3+7
             end if
          else
             len3=0
          end if
          if (css%trg_maxset(ii)) then
             call model_wash(css%trg_maxval(ii),s4,len4)
             if (len4.ne.0) then
                s4=" max='"//s4(1:len4)//"'"
                len4=len4+7
             end if
          else
             len4=0
          end if
          write(ounit,'(3X,A,I0,A)')"<target removed='",css%trg_orm(ii),&
               & "'"//s1(1:len1)//s3(1:len3)//s4(1:len4)//"/>"
       end if
    end do
    if (css%trg_orm(0).ne.0) then
       write(ounit,'(2X,A,I0,A)')"</check>"
    end if
    write(ounit,'(1X,A)')"</model>"
    return
  end subroutine model_writeModelDataXML
  !
  ! variable dimensions
  ! 
  character*50 function model_getDim(file,var)
    type(mod_file),pointer :: file
    type(mod_variable),pointer :: var
    integer ii,leng
    model_getDim=""
    leng=0
    do ii=1,var%ndim
       if (leng.eq.0)then
          model_getDim=file%dim80(var%ind(ii))(1:file%lend(var%ind(ii)))
          leng=file%lend(var%ind(ii))
       else
          model_getDim=model_getDim(1:leng)//","//&
               & file%dim80(var%ind(ii))(1:file%lend(var%ind(ii)))
          leng=leng+1+file%lend(var%ind(ii))
       end if
    end do
    return
  end function model_getDim
  !
  ! write position
  !
  subroutine model_planLoop(v,p,nout,out,ninn,inn,crc250,irc) 
    type(mod_variable),pointer :: v   ! variable pointer
    type(mod_plan),pointer :: p       ! plan pointer
    integer :: nout
    integer, allocatable :: out(:)
    integer :: ninn
    integer, allocatable :: inn(:)
    character*250 :: crc250
    integer :: irc
    !
    integer :: jj,cnt
    character*25 :: myname="model_planLoop"
    !
    cnt=1
    nout=0
    ninn=0
    do jj=1,v%ndim
       if (p%innerDim(v%ind(jj))) then
          ninn=ninn+1
          inn(ninn)=v%ind(jj)
       else
          cnt=cnt*(v%icount(jj))
          nout=nout+1
          out(nout)=v%ind(jj)
       end if
    end do
    if (cnt.gt.250) then
       if (mod_bdeb) then
          write(*,*)myname,''
       end if
       irc=777
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Insufficient target matching. Too large count=")
       call model_errorappendi(crc250,cnt)
       return
    end if
    return
  end subroutine model_planLoop
  !
 subroutine model_planTrgInd(css,ninn,inn,ind,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: ninn
    integer, allocatable :: inn(:)
    integer, allocatable :: ind(:)
    character*250 :: crc250
    integer :: irc
    integer :: ii,jj
    do jj=1,ninn
       ind(jj)=0
       do ii=1,css%ntrg
          if (css%trg_dim(ii).eq.inn(jj)) then
             ind(jj)=ii
          end if
       end do
    end do
    return
  end subroutine model_planTrgInd
  !
  ! set model sorting index variable
  !
  subroutine model_setIndex(css,trgname,varname,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character(len=*) :: trgname
    character(len=*) :: varname
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setIndex"
    integer,external :: length
    integer :: lent
    character*80 :: t80
    t80=trgname
    call chop0(t80,80)
    lent=length(t80,80,10)
    if(mod_bdeb)write(*,*)myname,' Entering: ',t80(1:lent),irc
    css%ind_trg=t80
    css%ind_lent=lent
    css%ind_var=varname
    call chop0(css%ind_var,80)
    css%ind_lenv=length(css%ind_var,80,10)
    css%ind_set=.true.
    !if(mod_bdeb)write(*,*)myname,' Done.',irc
    return
  end subroutine model_setindex
  !
  subroutine model_getIndex(css,trg80,var80,crc250,irc)
    type(mod_session), pointer :: css !  current session
    character*80 :: trg80
    character*80 :: var80
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setIndex"
    integer,external :: length
    if(mod_bdeb)write(*,*)myname,' Entering.',irc
    trg80=css%ind_trg
    var80=css%ind_var
    if(mod_bdeb)write(*,*)myname,' Done.',irc
    return
  end subroutine model_getIndex
  !
  ! set the sort variable limits
  !
  subroutine model_setIndexLimits(css, smin, smax, crc250,irc)
    type(mod_session), pointer :: css !  current session
    character(LEN=*) :: smin
    character(LEN=*) :: smax
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_setIndexLimits"
    integer :: lens, lene, JJ
    integer, external :: length
    type(parse_session),pointer :: plim => null()  ! parse_session pointer must be se
    character*80, allocatable :: var(:)
    real, allocatable :: val(:)
    lens=len_trim(smin)
    lene=len_trim(smax)
    call parse_open(plim,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Error return from 'parse_open'.")
       return
    end if
    if (lens.ne.0) then
       call parse_parsef(plim,smin(1:lens),var,crc250,irc)
       if (irc.ne.0) then
          if(mod_bdeb)then
             write(*,*)myname," Compiling target limit: '"//&
                  & smin(1:lens)//"'"
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
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from evalf.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       css%ind_lval(1)=.true.
    else
       css%ind_lval(1)=.false.
    end if
    if (lene.ne.0) then
       call parse_parsef(plim,smax(1:lene),var,crc250,irc)
       if (irc.ne.0) then
          if(mod_bdeb)then
             write(*,*)myname," Compiling target limit: '"//&
                  & smax(1:lene)//"'"
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
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from evalf.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       css%ind_lval(2)=.true.
    else
       css%ind_lval(2)=.false.
    end if
    call parse_close(plim,crc250,irc)
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Error return from 'parse_close'.")
       return
    end if
    if(mod_bdeb)write(*,*)myname,"Done min='"//trim(smin)//"' max='"//smax//"'",&
         css%ind_lval,css%ind_minval,css%ind_maxval
    return
  end subroutine model_setIndexLimits
  !
  ! set stack file limits
  !
  subroutine model_setFileStackLimits(css,ind_lval,ind_minval,ind_maxval,crc250,irc)
    implicit none
    type(mod_session), pointer :: css !  current session
    logical :: ind_lval(2)
    real :: ind_minval,ind_maxval
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="model_setFileStackLimits"
    ! set limits
    if (mod_bdeb)write(*,*)myname,' Entering:',ind_lval,ind_minval,ind_maxval
    ! make sure file stack is sorted
    if (mod_bdeb)write(*,*)myname,'Sorting stack.'
    if (.not.css%stackReady) then
       call model_sortStack(css,crc250,irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250," Error return from model_sortStack.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       css%stackReady = .true.
    end if
    if (mod_bdeb)write(*,*)myname,'Find stack start/stop.'
    ! find index start/stop...
    call model_findStackLimits(css,ind_lval,ind_minval,ind_maxval,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from model_setStackLimits.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    css%currentFileSortIndex=0 ! reset index
    if (mod_bdeb)write(*,*)myname,' Done.',irc
    return
  end subroutine model_setFileStackLimits
  !
  ! private subroutine for setting start/end-search-dates
  !
  subroutine model_setIndexLimitsRaw(css,ind_lval,ind_minval,ind_maxval)
    type(mod_session), pointer :: css !  current session
    logical :: ind_lval(2)
    real :: ind_minval,ind_maxval
    character*22 :: myname="model_setIndexLimitsRaw"
    css%ind_lval(1)=ind_lval(1)
    css%ind_lval(2)=ind_lval(2)
    css%ind_minval=ind_minval
    css%ind_maxval=ind_maxval
  end subroutine model_setIndexLimitsRaw
  !
  ! retrieve target variable list
  !
  subroutine model_getvariables(css,nvar,var80,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: nvar
    character*80, allocatable :: var80(:)
    character*250 :: crc250
    integer :: irc
    type(mod_target), pointer :: currentTarget
    character*25 :: myname="model_getVariables"
    if (allocated(var80)) deallocate(var80)
    allocate(var80(css%ntrg),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Unable to allocate var80.")
       call model_errorappendi(crc250,css%ntrg)
       return
    end if
    nvar=0
    currentTarget => css%firstTrg%next
    do while (.not.associated(currentTarget,target=css%lastTrg))
       nvar=min(css%ntrg,nvar+1)
       var80(nvar)=currentTarget%n80
       currentTarget => currentTarget%next
    end do
    return
  end subroutine model_getvariables
  !
  subroutine model_filestartxml(css,ounit,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: ounit                  ! output unit
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_filestartxml"
    if (associated(css%currentFile)) then
       write(ounit,'(1X,A)')"<modelFile file='"//&
            & css%currentFile%fn250(1:css%currentFile%lenf)//"'>"
    else
       write(ounit,'(1X,A)')"<modelFile>"
    end if
    return
  end subroutine model_filestartxml
  !
  subroutine model_filestopxml(css,ounit,crc250,irc)
    type(mod_session), pointer :: css !  current session
    integer :: ounit                  ! output unit
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_filestopxml"
    integer :: ii
    write(ounit,'(2X,A)')"<summary>"
    if (css%currentFile%ook(1).eq.css%currentFile%ook(4)) then
       write(ounit,'(3X,A,I0,A,I0,A)')"<locations found='",css%currentFile%ook(1),&
            & "' accepted='",css%currentFile%ook(4),"'/>"
    else
       write(ounit,'(3X,A,I0,A,I0,A)')"<locations found='",css%currentFile%ook(1),&
            & "' accepted='",css%currentFile%ook(4),"'>"
       if (css%currentFile%orm(2).ne.0) write(ounit,'(4X,A,I0,A)')"<check removed='",&
            & css%currentFile%orm(2),&
            & "' reason='location error.'"
       if (css%currentFile%orm(3).ne.0) write(ounit,'(4X,A,I0,A)')"<check removed='",&
            & css%currentFile%orm(3),&
            & "' reason='search failed.'/>"
       if (css%currentFile%orm(4).ne.0) write(ounit,'(4X,A,I0,A)')"<check removed='",&
            & css%currentFile%orm(4),&
            & "' reason='rejected by model filter.'/>"
       write(ounit,'(3X,A)')"</locations>"
    end if
    write(ounit,'(2X,A)')"</summary>"
    write(ounit,'(1X,A)')"</modelFile>"
    return
  end subroutine model_filestopxml
  ! get nice real value
  subroutine model_wash(val,s2,len2)
    real :: val
    character*50 :: s2
    integer :: len2
    integer, external :: length
    integer :: jj
    write(s2,'(F0.10)') val; call chop0(s2,50); len2=length(s2,50,10) ! ignore last digit...
    if (len2.gt.1) then
       OUTER: do JJ=1,len2
          if (s2(JJ:JJ).eq.".") then
             INNER: do while (len2.gt.JJ.and.&
               & (s2(len2:len2).eq."0".or.s2(len2:len2).eq."."))
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
  end subroutine model_wash
  !
  !
  !###############################################################################
  ! NETCDF ROUTINES
  !###############################################################################
  ! open netcdf file
  !
  subroutine model_openFile(css,f,crc250,irc)
    type(mod_session), pointer :: css !  current session
    type(mod_file),pointer :: f
    character*250 :: crc250
    integer :: irc
    integer :: ret
    character*25 :: myname="model_openFile"
    INTEGER :: CHUNKSIZEHINT
    integer :: ii
    chunksizehint= 1024*1024*1024
    if (F%LENF.eq.0) then
       irc=999
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Attempt to open empty file.")
       call model_errorappend(crc250,"\n")
       return
    end if
    if(mod_bdeb)write(*,*)myname,'Opening modfile: ',f%fn250(1:f%lenf)
    ret = NF__OPEN(f%fn250(1:F%LENF),nf_nowrite,&
         & chunksizehint,f%ncid)
    if (ret .ne. NF_NOERR) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Unable to open "//f%fn250(1:f%lenf))
       call model_errorappendi(crc250,ret)
       call model_errorappend(crc250,"\n")
       irc=780
       return
    endif
    do ii=1,10
       f%ook(ii)=0
       f%orm(ii)=0
    end do
     return
  end subroutine model_openFile
  !
  ! read netcdf file inventory
  !
  subroutine model_readInventory(css,f,crc250,irc)
    type(mod_session), pointer :: css !  current session
    type(mod_file),pointer :: f
    character*250 :: crc250
    integer :: irc
    integer :: ret,ii,dd,varid,len,lena
    integer, external :: length
    character*25 :: myname="model_readInventory"
    character*1, pointer ::  ac(:)
    integer*1,  pointer::  a1(:)
    integer*2,  pointer::  a2(:)
    integer*4,  pointer::  a4(:)
    real*4,     pointer::  ar(:)
    real*8,     pointer::  ad(:)
    type(mod_attribute),pointer :: att
    type(mod_variable),pointer :: v
    !
    ! get number of dimension, variables, att, unlimited dimension id
    !
    if(mod_bdeb)write(*,*)myname,' Entering.'
    RET = NF_INQ(f%ncid,   f%ndim, f%nvar,  &
         &       f%ngatt, f%unlimdimid)
    if (ret .ne. NF_NOERR) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error from NF_INQ: "//nf_strerror(ret))
       call model_errorappendi(crc250,ret)
       call model_errorappend(crc250,"\n")
       irc=790
       return
    end if
    !
    if(mod_bdeb)write(*,*)myname,' Allocate file.'
    !     allocate variables in file
    if (allocated(f%dim80)) deallocate(f%dim80)
    if (allocated(f%lend)) deallocate(f%lend)
    if (allocated(f%dim_trg)) deallocate(f%dim_trg)
    if (allocated(f%istart)) deallocate(f%istart)
    if (allocated(f%istop)) deallocate(f%istop)
    if (allocated(f%dim_var)) deallocate(f%dim_var)
    if (allocated(f%dim_val)) deallocate(f%dim_val)
    if (allocated(f%var80)) deallocate(f%var80)
    if (allocated(f%lenv)) deallocate(f%lenv)
    allocate(f%dim80(f%ndim),&
         &   f%lend(f%ndim),&
         &   f%dim_trg(f%ndim),&
         &   f%istart(f%ndim),&
         &   f%istop(f%ndim),&
         &   f%dim_var(f%ndim),&
         &   f%dim_val(f%ndim),&
         &   f%var80(f%nvar),&
         &   f%lenv(f%nvar),&
         &   stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate file.")
       call model_errorappendi(crc250,f%ndim)
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    !     
    if(mod_bdeb)write(*,*)myname,' Store dim.'
    !     -> store dimension names
    do dd=1,f%ndim
       f%istart(dd)=1
       RET = NF_INQ_DIM(f%NCID, DD, &
            &           f%dim80(dd), &
            &           f%istop(dd))
       if (ret .ne. NF_NOERR) then
          irc=801
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Error return from NF_INQ_DIM."//nf_strerror(ret))
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       call chop0(f%dim80(dd),80)
       f%lend(dd)=length(f%dim80(dd),80,10)
       f%dim_var(dd)=f%dim80(dd)(1:f%lend(dd))
       f%dim_val(dd)=real(f%istop(dd))
       f%dim_trg(dd)=0
    end do
    !
    if(mod_bdeb)write(*,*)myname,' Allocate var pointers.',f%nvar
    !     allocate variables in file
    allocate(f%var(f%nvar),stat=irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250,"Unable to allocate vars.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    if(mod_bdeb)write(*,*)myname,' Process var.'
    !     -> store variable names
    do varid=1,f%nvar
       allocate(f%var(varid)%ptr,stat=irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Unable to allocate vars.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       v=>f%var(varid)%ptr
       v%ndim=0
       v%natt=0
       ret = NF_INQ_VARNDIMS (f%ncid, varid, v%ndim);
       if (ret .ne. NF_NOERR) then
          irc=802
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Error return from NF_INQ_VARNDIMS."//&
               & nf_strerror(ret))
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       allocate(v%ind(v%ndim),v%istart(v%ndim),v%icount(v%ndim),stat=irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Unable to allocate var.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       RET = NF_INQ_VAR(f%ncid, varid,v%var80,v%type,v%ndim,v%ind,v%natt)
       if (ret .ne. NF_NOERR) then
          irc=802
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Error return from NF_INQ_VAR."//nf_strerror(ret))
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       call chop0(v%var80,80)
       v%lenv=length(v%var80,80,10)
       f%var80(varid)=v%var80
       f%lenv(varid)=v%lenv
       do ii=1,v%ndim
          v%istart(ii)=1
          v%icount(ii)=f%istop(v%ind(ii))
       end do
       call chop0(v%var80,80)
       ! process attributes
       v%scale=1.0
       v%mc=char(nf_fill_char)
       v%m1=nf_fill_int1
       v%m2=nf_fill_int2
       v%m4=nf_fill_int
       v%mr=nf_fill_real
       v%md=nf_fill_double
       v%misstype=0
       !
       !if(mod_bdeb)write(*,*)myname,' Store attr.'
       allocate(v%att(v%natt),stat=irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Unable to allocate var-att.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       do ii=1,v%natt
          allocate(att,stat=irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250,"Unable to allocate att.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          RET=NF_INQ_ATTNAME(f%NCID,VARID,ii,att%att80)
          if (ret .ne. NF_NOERR) cycle
          RET=NF_INQ_ATT(f%NCID,VARID,att%att80,att%type,att%len)
          if (ret.ne.NF_NOERR) cycle
          if (att%len.eq.0) then
             irc=845
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250,"Zero length attribute ")
             call model_errorappend(crc250,att%att80)
             call model_errorappend(crc250,"\n")
             return
          end if
          select case (att%type)
          case (nf_char)
             if (allocated(att%ac)) deallocate(att%ac)
             allocate(att%ac(att%len),stat=irc)
             if (irc.eq.0) ret = nf_get_att_text (f%ncid, varid,att%att80,att%ac)
          case(nf_int1)
             if (allocated(att%a1)) deallocate(att%a1)
             allocate(att%a1(att%len),stat=irc)
             if (irc.eq.0) ret = nf_get_att_int1 (f%ncid, varid,att%att80,att%a1)
          case(nf_int2)
             if (allocated(att%a2)) deallocate(att%a2)
             allocate(att%a2(att%len),stat=irc)
             if (irc.eq.0) ret = nf_get_att_int2 (f%ncid, varid,att%att80,att%a2)
          case(nf_int)
             if (allocated(att%a4)) deallocate(att%a4)
             allocate(att%a4(att%len),stat=irc)
             if (irc.eq.0) ret = nf_get_att_int (f%ncid, varid,att%att80,att%a4)
          case(nf_real)
             if (allocated(att%ar)) deallocate(att%ar)
             allocate(att%ar(att%len),stat=irc)
             if (irc.eq.0) ret = nf_get_att_real (f%ncid, varid, att%att80,att%ar)
          case(nf_double)
             if (allocated(att%ad)) deallocate(att%ad)
             allocate(att%ad(att%len),stat=irc)
             if (irc.eq.0) ret = nf_get_att_double (f%ncid, varid,att%att80,att%ad)
          case DEFAULT
          end select
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250,"Unable to allocate att value.")
             call model_errorappendi(crc250,v%ndim)
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          if (ret.ne.NF_NOERR) cycle
          call chop0(att%att80,80)
          att%lena=length(att%att80,80,10)
          !if(mod_bdeb)write(*,*)myname,' get attr.',ii,att%att80(1:att%lena)
          !     check if we have scale-factor/missing value
          if (att%att80(1:att%lena).eq."scale_factor") then
             select case (att%type)
             case(nf_real)
                v%scale=att%ar(1)
             case(nf_double)
                v%scale=att%ad(1)
             case DEFAULT
             end select
          else if (att%att80(1:att%lena).eq."missing_value") then
             select case (att%type)
             case (nf_char)
                v%mc=att%ac(1)
                v%misstype=nf_char
             case (nf_int1)
                v%m1=att%a1(1)
                v%misstype=nf_int1
             case (nf_int2)
                v%m2=att%a2(1)
                v%misstype=nf_int2
             case (nf_int)
                v%m4=att%a4(1)
                v%misstype=nf_int
             case (nf_real)
                v%mr=att%ar(1)
                v%misstype=nf_real
             case (nf_double)
                v%md=att%ad(1)
                v%misstype=nf_double
             case DEFAULT
             end select
          else
             if (allocated(att%ac)) deallocate(att%ac,stat=irc)
             if (allocated(att%a1)) deallocate(att%a1,stat=irc)
             if (allocated(att%a2)) deallocate(att%a2,stat=irc)
             if (allocated(att%a4)) deallocate(att%a4,stat=irc)
             if (allocated(att%ar)) deallocate(att%ar,stat=irc)
             if (allocated(att%ad)) deallocate(att%ad,stat=irc)
          end if
          v%att(ii)%ptr => att
          att => null()
       end do
    end do
    if(mod_bdeb)write(*,*)myname,' Done.'
  end subroutine model_readInventory
  !
  ! check variable limits
  !
  subroutine model_setRange(css,f,crc250,irc)
    type(mod_session), pointer :: css !  current session
    type(mod_file),pointer :: f
    character*250 :: crc250
    integer :: irc
    integer :: varid,ii
    type(mod_variable),pointer :: v
    character*25 :: myname="model_setRange"
    real :: val
    integer*8, parameter :: maxlen=1000
    integer*8 :: dimlen
    do varid=1,f%nvar
       v=>f%var(varid)%ptr
       dimlen=model_varLen(v)
       if (dimlen.lt.maxlen) then
          if (mod_bdeb)write(*,*)myname,"Opening '"//v%var80(1:v%lenv)//"'"
          call model_readVariable(css,f,varid,crc250,irc)       
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from readVariable.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          if (mod_bdeb)write(*,*)myname,"Scanning:'"//v%var80(1:v%lenv)//"'",v%len,v%type
          v%mmrange=.true.
          select case (v%type)
          case (nf_char)
          case (nf_int1)
             if (mod_bdeb)write(*,*)myname,"Int1.",v%scale
             do ii=1,v%len
                val=real(v%f1(ii)*v%scale)
                if (v%f1(ii).ne.v%m1) then
                   if (v%mmset) then
                      v%minval=min(v%minval,val)
                      v%maxval=max(v%maxval,val)
                   else
                      v%minval=val
                      v%maxval=val
                      v%mmset=.true.
                   end if
                end if
             end do
          case (nf_int2)
             if (mod_bdeb)write(*,*)myname,"Int2.",v%scale
             do ii=1,v%len
                val=real(v%f2(ii)*v%scale)
                if (v%f2(ii).ne.v%m2) then
                   if (v%mmset) then
                      v%minval=min(v%minval,val)
                      v%maxval=max(v%maxval,val)
                   else
                      v%minval=val
                      v%maxval=val
                      v%mmset=.true.
                   end if
                end if
             end do
          case (nf_int)
             if (mod_bdeb)write(*,*)myname,"Int4.",v%scale
             do ii=1,v%len
                val=real(v%f4(ii)*v%scale)
                if (v%f4(ii).ne.v%m4) then
                   if (v%mmset) then
                      v%minval=min(v%minval,val)
                      v%maxval=max(v%maxval,val)
                   else
                      v%minval=val
                      v%maxval=val
                      v%mmset=.true.
                   end if
                end if
             end do
          case (nf_real)
             if (mod_bdeb)write(*,*)myname,"Real.",v%scale
             do ii=1,v%len
                val=real(v%fr(ii)*v%scale)
                if (v%fr(ii).ne.v%mr) then
                   if (v%mmset) then
                      v%minval=min(v%minval,val)
                      v%maxval=max(v%maxval,val)
                   else
                      v%minval=val
                      v%maxval=val
                      v%mmset=.true.
                   end if
                end if
             end do
          case (nf_double)
             if (mod_bdeb)write(*,*)myname,"Double.",v%scale
             do ii=1,v%len
                val=v%fd(ii)*v%scale
                if (v%fd(ii).ne.v%md) then
                   if (v%mmset) then
                      v%minval=min(v%minval,val)
                      v%maxval=max(v%maxval,val)
                   else
                      v%minval=val
                      v%maxval=val
                      v%mmset=.true.
                   end if
                end if
             end do
          case DEFAULT
          end select
          if (mod_bdeb)write(*,*)myname,"Clearing:'"//v%var80(1:v%lenv)//"'",&
               & v%len,v%mmset,v%minval,v%maxval
          call model_clearVariable(v,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from clearVariable.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
       end if
    end do
    if (mod_bdeb)write(*,*)myname,"Done."
    return
  end subroutine model_setRange
  !
  ! read variable values into memory...
  !
  subroutine model_readVariable(css,f,varid,crc250,irc)
    type(mod_session), pointer :: css !  current session
    type(mod_file),pointer :: f
    integer :: varid
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_readVariable"
    integer*1::  fill1
    integer*2::  fill2
    integer*4::  fill4
    real*4::     fillr
    real*8::     filld
    integer :: ii,ret,lend
    integer,external :: length
    character*50 :: dim50
    type(mod_variable),pointer :: v
    !
    v => f%var(varid)%ptr
    !
    if(mod_bdeb)then
       dim50=model_getDim(f,v)
       call chop0(dim50,50)
       lend=length(dim50,50,10)
       write(*,*)myname,"Processing '",&
            & v%var80(1:v%lenv)//"("//dim50(1:lend)//")'"
       do ii=1,v%ndim
          write(*,'(X,A,A," = "I0,":",I0,X,I0)') myname,&
               & "    dim '"//f%dim80(v%ind(ii))(1: &
               & f%lend(v%ind(ii)))//"'",v%istart(ii),v%istart(ii)+v%icount(ii)-1,f%istop(v%ind(ii))
       end do
    end if
    ! find length of grid
    v%len=1
    do ii=1,v%ndim
       v%len=v%len*(v%icount(ii))
    end do
    !
    if (mod_bdeb) then
       write(*,'(X,A,A,I0,A)')myname,&
            &" *** Allocating: '"//v%&
            & var80(1:v%lenv)//"(",&
            & v%len,")"
    end if
    !
    select case (v%type)
    case (nf_int1)
       if(mod_bdeb)write(*,*)myname,'Allocating Int 1.',v%len
       allocate( v%f1(v%len),stat=irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Unable to allocate var value.")
          call model_errorappendi8(crc250,v%len)
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       ret = nf_get_vara_int1(f%ncid,varid,v%istart,v%icount,v%f1)
       if (ret .ne. NF_NOERR) then
          irc=812
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Error return from NF_GET_VARA_INT1."//&
               & nf_strerror(ret))
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    case (nf_int2)
       if(mod_bdeb)write(*,*)myname,'Allocating Int 2.',v%len
       allocate( v%f2(v%len),stat=irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Unable to allocate var value.")
          call model_errorappendi8(crc250,v%len)
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       ret = nf_get_vara_int2(f%ncid,varid,v%istart,v%icount,v%f2)
       if (ret .ne. NF_NOERR) then
          irc=812
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Error return from NF_GET_VARA_INT2."//&
               & nf_strerror(ret))
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    case (nf_int)
       if(mod_bdeb)write(*,*)myname,'Allocating Int 4.',v%len
       allocate( v%f4(v%len),stat=irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Unable to allocate var value.")
          call model_errorappendi8(crc250,v%len)
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       if(mod_bdeb)write(*,*)myname,'Reading Int 4.'
       ret = nf_get_vara_int(f%ncid,varid,v%istart,v%icount,v%f4)
       if (ret .ne. NF_NOERR) then
          irc=812
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Error return from NF_GET_VARA_INT."//&
               & nf_strerror(ret))
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
    case (nf_real)
       if(mod_bdeb)write(*,*)myname,'Allocating Real.',v%len
       allocate( v%fr(v%len),stat=irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Unable to allocate var value.")
          call model_errorappendi8(crc250,v%len)
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       if(mod_bdeb)write(*,*)myname,'Reading.',v%istart,v%icount,v%len
       ret = nf_get_vara_real(f%ncid,varid,v%istart,v%icount,v%fr)
       if (ret .ne. NF_NOERR) then
          irc=812
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Error return from NF_GET_VARA_REAL."//&
               & nf_strerror(ret))
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       if(mod_bdeb)write(*,*)myname,'Done real'
    case (nf_double) 
       if(mod_bdeb)write(*,*)myname,'Allocating Double.'
       allocate( v%fd(v%len),stat=irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Unable to allocate var value.")
          call model_errorappendi(crc250,v%ndim)
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       ret = nf_get_vara_double(f%ncid,varid,v%istart,v%icount,v%fd)
       if (ret .ne. NF_NOERR) then
          write(*,*) myname,"ERROR from NF_GET_VARA_DOUBLE:",&
               &                    nf_strerror(ret)
          irc=812
          return
       end if
    case DEFAULT
       if(mod_bdeb)write(*,*)myname,'Undefined.'
       v%len=0
       deallocate( v%fd,stat=irc)
       if (irc.ne.0) then
          call model_errorappend(crc250,myname)
          call model_errorappend(crc250,"Unable to deallocate fd.")
          call model_errorappendi(crc250,irc)
          call model_errorappend(crc250,"\n")
          return
       end if
       return
    end select
    if(mod_bdeb)write(*,*)myname,' Done. ',v%type,v%len,allocated(v%fd)
    return
  end subroutine model_readVariable
  !
  ! clear variable values from memory
  !
  subroutine model_deleteVariable(v,crc250,irc)
    type(mod_variable),pointer :: v
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_deleteVariable"
    !if(mod_bdeb)write(*,*)myname,' Entering.',associated(v)
    if (associated(v)) then
       call model_deleteAttributes(v,crc250,irc)
       if (associated(v%att)) deallocate(v%att)
       if (allocated(v%ind)) deallocate(v%ind)
       if (allocated(v%icount)) deallocate(v%icount)
       if (allocated(v%istart)) deallocate(v%istart)
       if (allocated(v%icount)) deallocate(v%icount)
       v%ndim=0
       v%natt=0
       call model_clearVariable(v,crc250,irc)
    end if
    !if(mod_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine model_deleteVariable
  !
  subroutine model_clearVariable(v,crc250,irc)
    type(mod_variable),pointer :: v
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_clearVariable"
    !if(mod_bdeb)write(*,*)myname,' Entering.',associated(v)
    if (allocated(v%fc)) deallocate(v%fc)
    if (allocated(v%f1)) deallocate(v%f1)
    if (allocated(v%f2)) deallocate(v%f2)
    if (allocated(v%f4)) deallocate(v%f4)
    if (allocated(v%fr)) deallocate(v%fr)
    if (allocated(v%fd)) deallocate(v%fd)
    !if(mod_bdeb)write(*,*)myname,' Done.'
    return
  end subroutine model_clearVariable
  !
  ! clear attibute from memory
  !
  subroutine model_deleteAttributes(v,crc250,irc)
    type(mod_variable),pointer :: v
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_deleteAttributes"
    integer :: attid
    !if(mod_bdeb)write(*,*)myname,' Entering.',associated(v)
    !if(mod_bdeb)write(*,*)myname,'Have.',v%natt,associated(v%att)
    do attid=1,v%natt
       !if(mod_bdeb)write(*,*)myname,'Loop.',attid,v%natt,associated(v%att)
       if (associated(v%att(attid)%ptr)) then
          call model_clearAttribute(v%att(attid)%ptr,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from clearAttribute.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          deallocate(v%att(attid)%ptr)
       end if
    end do
    !if(mod_bdeb)write(*,*)myname,' Done.',irc
    return
  end subroutine model_deleteAttributes
  !
  subroutine model_clearAttribute(att,crc250,irc)
    type(mod_attribute),pointer :: att
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_clearAttribute"
    !if(mod_bdeb)write(*,*)myname,' Entering.',associated(att)
    if (allocated(att%ac)) deallocate(att%ac)
    if (allocated(att%a1)) deallocate(att%a1)
    if (allocated(att%a2)) deallocate(att%a2)
    if (allocated(att%a4)) deallocate(att%a4)
    if (allocated(att%ar)) deallocate(att%ar)
    if (allocated(att%ad)) deallocate(att%ad)
    !if(mod_bdeb)write(*,*)myname,' Done.',irc
    return
  end subroutine model_clearAttribute
  !
  ! read sort variable
  !
  subroutine model_readSortVariable(css,f,crc250,irc)
    use sort
    type(mod_session), pointer :: css !  current session
    type(mod_file),pointer :: f
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="model_readSortVariable"
    integer :: iisort,jj,ii
    integer, external :: length
    real :: val
    type(mod_variable),pointer :: v
    !
    ! load ind_var into memory
    !
    css%ind_lenv=length(css%ind_var,80,10)
    if (css%ind_lenv.eq.0) then ! no sort variable
       if(mod_bdeb)write(*,*)myname,'Warning: no file-sorting variable defined.'
       f%nsort=1
       allocate(f%sort(f%nsort),&
            & f%indsort(f%nsort), &
            & f%desc250(f%nsort),stat=irc)
       do jj=1,f%nsort
          f%sort(jj)=0.0D0
          f%indsort(jj)=jj
          f%desc250(jj)="undef"
       end do
       f%ind_lim=.false. ! no target to print...
       f%ind_start=0.0D0
       f%ind_stop=0.0D0
       f%trg=0.0D0
    else
       iisort=0
       do ii=1,f%nvar
!          write(*,*)myname,'Checking: ',ii,' "',f%var80(ii)(1:f%lenv(ii)),'"  "',css%ind_var(1:css%ind_lenv),'"'
          if (css%ind_var(1:css%ind_lenv).eq. &
               & f%var80(ii)(1:f%lenv(ii))) then
             iisort=ii
             exit
          end if
       end do
       if (iisort .ne. 0) then
          call model_readVariable(css,f,iisort,crc250,irc)
          if (irc.ne.0) then
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Error return from readVariable.")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
          v=>f%var(iisort)%ptr
          f%nsort=0
          do ii=1,v%len
             if (model_getValue(v,ii,val,crc250,irc)) then
                f%nsort=f%nsort+1
             else if (mod_bdeb) then
                write(*,*)myname,'Invalid sort variable at:',ii,v%type,nf_double
             end if
          end do
          if (f%nsort.gt.0.and.f%nsort .lt.1000) then
             allocate(f%sort(f%nsort),&
                  & f%indsort(f%nsort), &
                  & f%desc250(f%nsort),stat=irc)
             if (irc.ne.0) then
                call model_errorappend(crc250,myname)
                call model_errorappend(crc250," Unable to allocate var:")
                call model_errorappendi(crc250,abs(f%nsort))
                call model_errorappend(crc250,":")
                call model_errorappendi(crc250,irc)
                call model_errorappend(crc250,"\n")
                return
             end if
             do ii=1,v%len
                if (model_getValue(v,ii,val,crc250,irc)) then
                   jj=jj+1
                   f%sort(jj)=val
                   if (jj.eq.1) then
                      f%ind_start=f%sort(jj)
                      f%ind_stop=f%sort(jj)
                   else
                      f%ind_start=min(f%ind_start,f%sort(jj))
                      f%ind_stop=max(f%ind_stop,f%sort(jj))
                   end if
                   f%indsort(jj)=jj
                   f%desc250(jj)=model_getdesc250(jj,f,v,crc250,irc)
                end if
             end do
             if(mod_bdeb)write(*,*)myname,' Sorting.'
             f%tsort=f%nsort
             call sort_heapsort1r(f%nsort,f%sort,1.0D-5,&
                  & f%tsort,f%nsort,f%indsort,.false.)
             if(mod_bdeb)write(*,*)myname,' Sorting done.'
             f%ind_lim=.true. ! target is available
             f%ind_start=f%sort(f%indsort(1))
             f%ind_stop=f%sort(f%indsort(f%nsort))
             f%trg=f%ind_start
          else
             irc=453
             call model_errorappend(crc250,myname)
             call model_errorappend(crc250," Invalid "//v%var80(1:v%lenv)//" dimensions.")
             call model_errorappendi(crc250,f%nsort)
             call model_errorappend(crc250,"\n")
             call model_errorappendi(crc250,irc)
             call model_errorappend(crc250,"\n")
             return
          end if
       else
          if(mod_bdeb)write(*,*)myname,'Unable to find sort variable: "',&
               & css%ind_var(1:css%ind_lenv),'"'
       end if
    end if
  end subroutine model_readSortVariable
  !
  ! close file
  !
  subroutine model_closeFile(css,f,crc250,irc)
    type(mod_session), pointer :: css !  current session
    type(mod_file),pointer :: f
    character*250 :: crc250
    integer :: irc,ret
    character*25 :: myname="model_closeFile"
    ret=NF_CLOSE(f%ncid)        ! end definitions: leave define mode
    if (ret .ne. NF_NOERR) then
       irc=170
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Unable to close: "//&
               & f%fn250(1:f%lenf)//" "//nf_strerror(ret))
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    return
  end subroutine model_closeFile
  !
  !###############################################################################
  ! ERROR ROUTINES
  !###############################################################################
  !
  subroutine model_errorappend(crc250,string)
    implicit none
    character*250 :: crc250
    character*(*) :: string
    character*250 :: buff250
    integer :: lenc, lenb
    integer, external :: length
    call chop0(crc250,250)
    lenc=length(crc250,250,10)
    lenb=len(trim(string))
    buff250=string(1:lenb)
    if (lenc.eq.0) then
       crc250=buff250(1:lenb)
    else
       crc250=crc250(1:lenc)//" "//buff250(1:min(250-lenc-1,lenb))
    end if
  end subroutine model_errorappend
  subroutine model_errorappendi(crc250,inum)
    implicit none
    character*250 :: crc250
    integer :: inum
    character*250 :: buff250
    integer :: lenc, lenb
    integer, external :: length
    call chop0(crc250,250)
    lenc=length(crc250,250,10)
    write(buff250,'(I12)')inum
    call chop0(buff250,250)
    lenb=length(buff250,250,1)
    if (lenc.eq.0) then
       crc250=buff250(1:lenb)
    else
       crc250=crc250(1:lenc)//" "//buff250(1:min(250-lenc-1,lenb))
    end if
  end subroutine model_errorappendi
  !
  subroutine model_errorappendi8(crc250,inum)
    implicit none
    character*250 :: crc250
    integer*8 :: inum
    character*250 :: buff250
    integer :: lenc, lenb
    integer, external :: length
    call chop0(crc250,250)
    lenc=length(crc250,250,10)
    write(buff250,'(I12)')inum
    call chop0(buff250,250)
    lenb=length(buff250,250,1)
    if (lenc.eq.0) then
       crc250=buff250(1:lenb)
    else
       crc250=crc250(1:lenc)//" "//buff250(1:min(250-lenc-1,lenb))
    end if
  end subroutine model_errorappendi8
  !
  !###############################################################################
  ! STRING ROUTINES
  !###############################################################################
  !
  subroutine findDelimiter(string,del,pos)
    character(len=*) :: string
    character*1 :: del
    integer :: pos
    logical :: bdone
    integer :: lens
    lens=len(trim(string))
    pos=pos+1
    bdone=(pos.gt.lens)
    do while (.not.bdone)
       if (string(pos:pos).eq.del) then
          bdone=.true.
       else
          pos=pos+1
          bdone=(pos.gt.lens)
       end if
    end do
  end subroutine findDelimiter
  !
end module model
