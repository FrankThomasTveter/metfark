module table
  use parse
  IMPLICIT NONE
  !
  ! Global constants
  !
  logical     :: table_bdeb=.false.
  !
  ! SESSION VARIABLES
  !
  type :: table_attribute
     character*80 :: name80=""
     integer :: lenn=0
     character*250 :: value250=""
     integer :: lenv=0
     type(table_attribute), pointer :: prev => null()         ! linked list
     type(table_attribute), pointer :: next => null()         ! linked list
  end type table_attribute
  !
  ! Target item
  !   
  type :: table_obstrg
     character*80 :: trg80      ! target name
     character*250 :: pos250    ! position/sequence number
     character*80 :: descr80    ! descriptor
     character*250 :: info250   ! information
     character*80 :: min80      ! min value
     character*80 :: max80      ! max value
     type(table_obstrg), pointer :: prev => null()   ! linked list
     type(table_obstrg), pointer :: next => null()   ! linked list
  end type table_obstrg
  !
  type :: table_modtrg
     character*80 :: trg80      ! target name
     character*80 :: var80      ! variable name
     character*80 :: min80      ! min value
     character*80 :: max80      ! max value
     type(table_modtrg), pointer :: prev => null()   ! linked list
     type(table_modtrg), pointer :: next => null()   ! linked list
  end type table_modtrg
  !
  ! default values for the model targets
  !
  type :: table_default
     character*80 :: n80   ! name
     integer :: lenn       ! name length
     character*80 :: v80   ! value
     integer :: lenv       ! value length
     real :: val           ! value
     type(table_default), pointer :: prev => null()   ! linked list
     type(table_default), pointer :: next => null()   ! linked list
  end type table_default
  !
  type :: table_location
     type(table_default), pointer  :: cDef => null()
     type(table_default), pointer  :: firstDef => null()! linked list start
     type(table_default), pointer  :: lastDef => null() ! linked list end
     type(table_location), pointer :: prev => null()    ! linked list
     type(table_location), pointer :: next => null()    ! linked list
  end type table_location
  !
  type :: table_match
     character*80 :: n80 ! name
     character*250 :: e250 ! obs expression
     character*80 :: l80 ! lower limit
     character*80 :: u80 ! upper limit
     type(table_match), pointer :: prev => null()   ! linked list
     type(table_match), pointer :: next => null()   ! linked list
  end type table_match
  !
  type table_column
     character*80  :: name80
     integer :: lenn=0
     character*250  :: exp250
     integer :: lene=0
     real :: minc=0.0D0
     real :: maxc=0.0D0
     logical :: setc=.false.
     character*80 :: min80
     character*80 :: max80
     integer :: lenmin
     integer :: lenmax
     logical :: sets=.false.
     type(table_column), pointer  :: prev => null() ! linked list start
     type(table_column), pointer  :: next => null()  ! linked list end
  end type table_column
  !
  type table_columns
     integer :: ncols
     character*80, allocatable :: col80(:)
     integer, allocatable :: lenc(:)
     real, allocatable :: minc(:)
     real, allocatable :: maxc(:)
     logical, allocatable :: setc(:)
     character*80, allocatable :: min80(:)
     character*80, allocatable :: max80(:)
     integer, allocatable :: lenmin(:)
     integer, allocatable :: lenmax(:)
     logical, allocatable :: sets(:)
     integer, allocatable :: cok(:),crm(:)
  end type table_columns
  !
  type :: table_set
     character*80 :: name80=""
     integer :: id=0
     character*250, allocatable :: colv(:)
     character*250 :: leg250=""
     ! model data
     ! obs data
     CHARACTER(LEN=250)              :: tablepath=""
     integer :: category                   ! filter category/bufrType
     integer :: subCategory                ! filter subcategory/subType
     character*80                    :: ind_obs80=""          ! obs index target name
     character*250                   :: ind_exp250=""         ! index target expression
     logical                         :: ind(3)   ! true if both ind_start and ind_stop are valid
     real                            :: ind_start=0.0D0         ! lowest index
     real                            :: ind_stop=0.0D0          ! highest index
     character*250 :: obs250="" ! observation cache file
     character*250 :: mod250="" ! model cache file
     character*80                    :: ind_trg80=""            ! model index target name
     character*80                    :: ind_mod80=""            ! model index variable
     ! obs data
     type(table_obstrg), pointer :: firstObstrg => null()        ! linked list
     type(table_obstrg), pointer :: lastObstrg => null()         ! linked list
     integer :: nObsTrg = 0
     ! mod data
     type(table_modtrg), pointer :: firstModtrg => null()        ! linked list
     type(table_modtrg), pointer :: lastModtrg => null()         ! linked list
     integer :: nModTrg = 0
     type(table_obstrg), pointer :: cObstrg => null()            ! linked list
     type(table_modtrg), pointer :: cModtrg => null()            ! linked list
     ! colocation data
     type(table_location), pointer :: firstLoc => null()         ! linked list start
     type(table_location), pointer :: lastLoc => null()          ! linked list end
     type(table_location), pointer :: currentLoc => null()       ! current location
     type(table_location), pointer :: cLoc => null()             ! current location
     integer :: ndef = 0                                        ! number of defaults
     type(table_match), pointer :: firstMatch => null()          ! linked list
     type(table_match), pointer :: lastMatch => null()           ! linked list
     type(table_match), pointer :: cMatch => null()              ! linked list
     integer :: nmatch = 0
     character*250 :: fltmod250=""
     character*250 :: fltobs250=""
     !
     ! output columns 
     integer :: ccol=0    ! number of allocated columns
     character*80,allocatable    :: col80(:)
     integer, allocatable        :: col_lenn(:)
     character*250,allocatable   :: col_exp250(:)
     integer, allocatable        :: col_lene(:)
     !
     ! target lists
     integer :: nTrgMod=0
     character*80, pointer :: trgMod80(:) => null()       ! list of target names
     integer, pointer :: trgModLent(:) => null()         ! list of target name length
     logical :: trgModSet=.false. ! is target list set?
     ! linked list
     type(table_set), pointer :: prev => null()         ! linked list
     type(table_set), pointer :: next => null()         ! linked list
  end type table_set
  !
  ! BUFR FILE STACK
  !
  type :: table_file
     character*250  :: fn250 = ""          ! file name
     integer        :: lenf=0              ! length of file name string
     integer :: mok(10),mrm(10),ook(10),orm(10),lenh(10)
     integer :: unit=0
     logical :: fopen = .false.  ! is file open?
     character*80 :: hint80(10)
     character*250  :: type250 = ""          ! category
     integer        :: lent=0              ! length of category
     integer :: nleg=0
     character*80, allocatable :: nam80(:)
     integer, allocatable :: lenn(:)
     character*250, allocatable :: leg250(:)
     integer, allocatable :: lenl(:)
     integer, allocatable :: legind(:)
     integer :: ncol=0
     character*80, allocatable :: col80(:)
     integer, allocatable :: lenc(:)
     character*250, allocatable :: exp250(:)
     integer, allocatable :: lene(:)
     integer, allocatable :: colind(:)
     integer, allocatable :: invind(:)
     type(table_file), pointer :: prev => null() ! linked list
     type(table_file), pointer :: next => null() ! linked list
  end type table_file
  !
  type :: table_filePointer
     type(table_file), pointer :: ptr => null()
  end type table_filePointer
  !
  type :: table_session
     integer :: sid=0            ! session id
     character*250 :: type250="" ! type
     integer :: lenp =0
     !
     ! output
     character*250 :: tab250=""  ! table file name
     integer :: lent             ! length of tab250
     !
     character*250 :: gra250=""  ! graphics directory name
     integer :: leng             ! length of gra250
     !
     ! STACK
     !
     type(table_file), pointer :: firstFile => null()   ! linked list start
     type(table_file), pointer :: lastFile => null()    ! linked list end
     type(table_file), pointer :: currentFile => null()
     type(table_file), pointer :: nextFile => null()
     type(table_filePointer), pointer   :: fileStack(:) => null() ! array of the stack elements
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
     ! time information
     !
     ! time information
     !  VALUES(1):	The year
     !  VALUES(2):	The month
     !	VALUES(3):	The day of the month
     !	VALUES(4):	Time difference with UTC in minutes
     !	VALUES(5):	The hour of the day
     !	VALUES(6):	The minutes of the hour
     !	VALUES(7):	The seconds of the minute
     !	VALUES(8):	The milliseconds of the second
     integer :: values(8)
     !
     integer :: nleg=0
     character*80, allocatable :: nam80(:)
     integer, allocatable :: lenn(:)
     character*250, allocatable :: leg250(:)
     integer, allocatable :: lenl(:)
     integer :: ncol=0
     character*80, allocatable :: col80(:)
     integer, allocatable :: lenc(:)
     character*250, allocatable :: exp250(:)
     integer, allocatable :: lene(:)
     !
     integer :: ncols=0    ! number of columns in linked list
     type(table_column), pointer :: firstColumn => null()   ! linked list start
     type(table_column), pointer :: lastColumn => null()    ! linked list end
     !
     integer :: natt =0
     type(table_attribute), pointer :: firstAttribute => null()         ! linked list
     type(table_attribute), pointer :: lastAttribute => null()         ! linked list
     integer :: nset =0
     type(table_set), pointer :: firstSet => null()         ! linked list
     type(table_set), pointer :: lastSet => null()          ! linked list
     type(table_set), pointer :: currentSet => null()       ! linked list
     type(table_session), pointer :: prev => null()         ! linked list
     type(table_session), pointer :: next => null()         ! linked list
  end type table_session
  !
  integer :: maxid=0 ! session counter
  type(table_session), pointer :: firstSession => null()   ! linked list start
  type(table_session), pointer :: lastSession => null()    ! linked list end
  !
CONTAINS
  !
  !###############################################################################
  ! SESSION ROUTINES
  !###############################################################################
  !
  subroutine table_opensession(sid,tss,crc250,irc)
    integer :: sid
    character*250 :: crc250
    integer :: irc
    type(table_session),pointer :: tss  !  new session
    character*25 :: myname="table_openSession"
    if (.not.associated(firstSession)) then
       allocate(firstSession, lastSession,stat=irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250,"Unable to allocate 'firstSession/lastSession'.")
          call table_errorappend(crc250,"\n")
          return
       end if
       firstSession%next => lastSession
       lastSession%prev => firstSession
    end if
    nullify(tss)
    allocate(tss,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'new session'.")
       call table_errorappend(crc250,"\n")
       return
    end if
    maxid=maxid+1
    tss%sid=maxid
    tss%prev => lastSession%prev
    tss%next => lastSession
    tss%prev%next => tss
    tss%next%prev => tss
    ! file stack
    allocate(tss%firstFile,tss%lastFile, stat=irc) ! 
    if (irc.ne.0) then
       call observation_errorappend(crc250,myname)
       call observation_errorappend(crc250,"Unable to allocate &
            & 'tss%firstFile/tss%lastFile'.")
       call observation_errorappend(crc250,"\n")
       return
    end if
    tss%firstFile%next => tss%lastFile
    tss%lastFile%prev => tss%firstFile
    ! attributes
    allocate(tss%firstAttribute,tss%lastAttribute,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'attribute chain'.")
       call table_errorappend(crc250,"\n")
       return
    end if
    tss%firstAttribute%next => tss%lastAttribute
    tss%lastAttribute%prev => tss%firstAttribute
    tss%natt=0
    ! sets
    allocate(tss%firstSet,tss%lastSet,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'attribute chain'.")
       call table_errorappend(crc250,"\n")
       return
    end if
    tss%firstSet%next => tss%lastSet
    tss%lastSet%prev => tss%firstSet
    tss%nset=0
    ! columns
    allocate(tss%firstColumn,tss%lastColumn, stat=irc) ! 
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate &
            & 'tss%firstColumn/tss%lastColumn'.")
       call table_errorappend(crc250,"\n")
       return
    end if
    tss%firstColumn%next => tss%lastColumn
    tss%lastColumn%prev => tss%firstColumn
    !
    sid = tss%sid
    return
  end subroutine table_opensession

  subroutine table_getSession(tss,sid,crc250,irc)
    type(table_session), pointer :: tss !  current session
    integer :: sid
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="table_getSession"
    if (.not.associated(firstSession)) then
       irc=911
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"No session is opened!")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    tss => firstSession%next
    do while ( .not.associated(tss,target=lastSession))
       if (tss%sid .eq. sid) then
          !if (table_bdeb) write(*,*)myname,'Exiting with sid:',sid,irc
          return
       end if
       tss=>tss%next
    end do
    nullify(tss)
    irc=342
    call table_errorappend(crc250,myname)
    call table_errorappend(crc250,"Invalid session id:")
    call table_errorappendi(crc250,sid)
    call table_errorappend(crc250,"\n")
    return
  end subroutine table_getSession

  subroutine table_closeSession(tss,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="table_closeSession"
    if(table_bdeb)write(*,*)myname,'Entering.',irc
    if (associated(tss)  .and. .not.associated(tss,target=lastSession)) then
       call table_removeSession(tss,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from removeSession.")
          call table_errorappendi(crc250,irc)
          call table_errorappend(crc250,"\n")
          return
       end if
    else
       irc=599
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Attempt to close none-existent session.")
       call table_errorappend(crc250,"\n")
       return
    end if
    if(table_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine table_closeSession

  subroutine table_removeSession(tss,crc250,irc)
    type(table_session), pointer :: tss !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="table_removeSession"
    tss%prev%next => tss%next
    tss%next%prev => tss%prev
    call table_clearAttrStack(tss,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from clearAttrStack.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    call table_removeFiles(tss,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from removeFiles.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    !call table_clearSetStack(tss)
    deallocate(tss%firstAttribute,tss%lastAttribute,tss%firstSet,tss%lastSet)
    deallocate(tss)
  end subroutine table_removeSession
  !
  !###############################################################################
  ! TYPE + ATTRIBUTE ROUTINES
  !###############################################################################
  !
  ! set the observation graphivcs type ("rms+stdv", "scatter" etc).
  subroutine table_settype(tss,type250,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*250 :: type250
    character*250 :: crc250
    integer :: irc
    integer,external :: length
    character*22 :: myname="table_settype"
    tss%type250=type250
    tss%lenp=length(type250,250,10)
    return
  end subroutine table_settype
  !
  subroutine table_clearattrstack(tss,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*250 :: crc250
    integer :: irc
    integer :: irc2
    character*22 :: myname="table_clearattrstack"
    type(table_attribute), pointer :: cat,nat !  current attribute
    cat => tss%firstAttribute%next
    do while (.not.associated(cat,target=tss%lastAttribute))
       nat => cat%next
       call table_unlinkAttribute(cat)
       call table_deallocateAttribute(cat)
       tss%natt=tss%natt-1
       cat  => nat
    end do
    return
  end subroutine table_clearattrstack
  !
  subroutine table_unlinkAttribute(cat)
    implicit none
    type(table_attribute), pointer :: cat !  current attribute
    cat%prev%next => cat%next
    cat%next%prev => cat%prev
    return
  end subroutine table_unlinkAttribute
  !
  subroutine table_deallocateAttribute(cat)
    implicit none
    type(table_attribute), pointer :: cat !  current attribute
    integer :: irc2
    deallocate(cat,stat=irc2) ! ignore any errors
    return
  end subroutine table_deallocateAttribute
  !
  subroutine table_pushattr(tss,name80,val250,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*80 :: name80
    character*250 :: val250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_pushattr"
    integer :: lenn,lenv
    integer, external :: length
    type(table_attribute), pointer :: cat !  current attribute
    lenn=length(name80,80,10)
    lenv=length(val250,250,10)
    if (lenn.ne.0) then
       allocate(cat,stat=irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250,"Unable to allocate 'attribute'.")
       end if
       cat%name80=name80
       cat%lenn=lenn
       cat%value250=val250
       cat%lenv=lenv
       cat%next => tss%lastAttribute
       cat%prev => tss%lastAttribute%prev
       cat%prev%next => cat
       cat%next%prev => cat
       tss%natt=tss%natt+1
       nullify(cat)
    else
       irc=344
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Attempt to push empty attribute/value.")
       return
    end if
    return
  end subroutine table_pushattr
  !
  subroutine table_limitcolumn(tss,name,min,max,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character(LEN=*) :: name
    character(LEN=*) :: min
    character(LEN=*) :: max
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_limitcolumn"
    integer :: lenn,ircMin,ircMax
    real :: rmin,rmax
    integer, external :: length
    type(table_column), pointer :: col
    lenn=len_trim(name)
    if (lenn.ne.0) then
       col=>tss%firstColumn%next
       do while (.not.associated(col,target=tss%lastColumn))
          if (name .eq. col%name80(1:col%lenn)) then
             read(min,*,iostat=ircMin)rmin
             read(max,*,iostat=ircMax)rmax
             if (ircMin.eq.0.and.ircMax.eq.0) then
                col%setc=.true.
                col%minc=rmin
                col%maxc=rmax
             else
                col%sets=.true.
                col%min80=min
                col%lenmin=len_trim(min)
                col%max80=max
                col%lenmax=len_trim(max)
             end if
             return
          else
             col=>col%next
          end if
       end do
    end if
    irc=312
    call table_errorappend(crc250,myname)
    call table_errorappend(crc250,"Invalid column '"//name//"'")
    return
  end subroutine table_limitcolumn
  !
  !###############################################################################
  ! OUTPUT FILE ROUTINE
  !###############################################################################
  !
  subroutine table_setTablefile(tss,tab250,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*250 :: tab250 ! name of table file
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    tss%tab250=tab250
    CALL CHOP0(tss%tab250,250)
    TSS%LENT=LENGTH(tss%tab250,250,10)
    return
  end subroutine table_setTablefile
  !
  subroutine table_getTablefile(tss,tab250,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*250 :: tab250 ! name of table file
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    tab250=tss%tab250
    return
  end subroutine table_getTablefile
  !
  !
  subroutine table_setGraphicsdir(tss,gra250,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*250 :: gra250 ! name of table file
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    tss%gra250=gra250
    CALL CHOP0(tss%gra250,250)
    TSS%LENG=LENGTH(tss%gra250,250,10)
    if(table_bdeb)write(*,*)'setGrahicsDir:',tss%gra250(1:TSS%LENG)
    return
  end subroutine table_setGraphicsdir
  !
  subroutine table_getGraphicsdir(tss,gra250,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*250 :: gra250 ! name of table file
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    gra250=tss%gra250
    if(table_bdeb)write(*,*)'getGrahicsDir:',tss%gra250(1:TSS%LENG)
    return
  end subroutine table_getGraphicsdir
  !
  subroutine table_openfile(tss,ounit,tab250,lent,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    integer :: ounit
    character*250 :: tab250
    integer :: lent
    character*250 :: crc250
    integer :: irc
    integer, external :: ftunit,length
    character*22 :: myname="table_openFile"
    call chop0(tab250,250)
    lent=length(tab250,250,10)
    ounit=ftunit(irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from FTUNIT.")
       call table_errorappendi(crc250,irc)
       return
    end if
    write(*,'(X,A,X,A)')myname,"Table file: "//tab250(1:lent)
    open(unit=ounit,file=tab250(1:lent), &
         & access="sequential",form="formatted",status="unknown",iostat=irc)
    if(table_bdeb)write(*,*)myname,'Opened:',tab250(1:lent),ounit,irc
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to open table file '"//tab250(1:lent)//"'")
       call table_errorappendi(crc250,irc)
       return
    end if
    return
  end subroutine table_openfile
  !
  subroutine table_openfileapp(tss,ounit,tab250,lent,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    integer :: ounit
    character*250 :: tab250
    integer :: lent
    character*250 :: crc250
    integer :: irc
    integer, external :: ftunit,length
    character*22 :: myname="table_openFileApp"
    call chop0(tab250,250)
    lent=length(tab250,250,10)
    ounit=ftunit(irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from FTUNIT.")
       call table_errorappendi(crc250,irc)
       return
    end if
    open(unit=ounit,file=tab250(1:lent), &
         & access="append",form="formatted",status="old",iostat=irc)
    if(table_bdeb)write(*,*)myname,'Appending:',tab250(1:lent),ounit,irc
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to open table file '"//tab250(1:lent)//"'")
       call table_errorappendi(crc250,irc)
       return
    end if
    return
  end subroutine table_openfileapp
  !
  subroutine table_closeFile(tss,ounit,tab250,lent,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    integer :: ounit
    character*250 :: tab250
    integer :: lent
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_closeFile"
    close(unit=ounit,iostat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to close table file '"//tab250(1:lent)//"'")
       call table_errorappendi(crc250,irc)
       return
    end if
    ounit=0
    return
  end subroutine table_closeFile
  !
  subroutine table_addTopComments(tss,css,mss,oss,ounit,tab250,lent,gra250,cat250,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(table_session), pointer :: tss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    integer :: cid,mid,oid
    integer :: ounit
    character*250 :: tab250,gra250,cat250
    integer :: lent
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_addTopComments"
    integer, external :: length
    integer :: lenc,leng
    type(table_attribute), pointer :: attr => null()
    call chop0(cat250,250)
    lenc=length(cat250,250,10)
    call chop0(gra250,250)
    leng=length(gra250,250,10)
    write(ounit,'("# COMMAND: Rscript --vanilla ",A,X,A,X,A)',iostat=irc) &
         & cat250(1:lenc),tab250(1:lent),gra250(1:leng)
    !
    ! loop over set and write attributes and legend as comments
    !
    if(table_bdeb)write(*,*)myname,'Writing attributes and legends.',ounit
    write(ounit,'("#")',iostat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Error writing to table file'.")
       call table_errorappend(crc250,tss%tab250(1:tss%lent))
       return
    end if
    write(ounit,'("# TYPE:",A)',iostat=irc)tss%type250(1:tss%lenp)
    write(ounit,'("#")',iostat=irc)
    ! print attributes
    write(ounit,'("# ATTRIBUTES:",I0)',iostat=irc)tss%natt
    attr => tss%firstAttribute%next
    do while (.not.associated(attr,target=tss%lastAttribute))
       write(ounit,'("# ",A,":",A)',iostat=irc)&
            & attr%name80(1:attr%lenn),attr%value250(1:attr%lenv)
       attr => attr%next
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Error writing to Table file'.")
       call table_errorappend(crc250,tss%tab250(1:tss%lent))
       return
    end if
    return
  end subroutine table_addTopComments

  subroutine table_addSetComments(tss,css,mss,oss,ounit,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(table_session), pointer :: tss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    integer :: cid,mid,oid
    integer :: ounit
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_addSetComments"
    integer, external :: length
    integer :: lenc,lene,lenn,lenl,leng
    type(table_attribute), pointer :: attr => null()
    character*250 :: leg250
    character*80 :: name80
    character*80, allocatable :: var80(:)
    integer :: ii,jj
    integer :: ncols
    character*80, allocatable :: col80(:)
    character*250, allocatable :: exp250(:)
    ! print legend table
    write(ounit,'("#")',iostat=irc)
    write(ounit,'("# LEGENDS:",I0)',iostat=irc) tss%nset
    do while (table_loopset(tss,css,mss,oss,name80,ncols,col80,exp250,leg250,0,crc250,irc))
       call chop0(name80,80)
       lenn=length(name80,80,1)
       call chop0(leg250,250)
       lenl=length(leg250,250,1)
       if (table_bdeb) write(*,*)myname,"Inside loopSet '"//name80(1:lenn)//"'"
       write(ounit,'("#",X,A,":",A)',iostat=irc)name80(1:lenn),leg250(1:lenl)
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from loopset'.")
       return
    end if
    ! print legend table
    write(ounit,'("#")',iostat=irc)
    write(ounit,'("# COLUMNS:",I0)',iostat=irc) ncols+1
    write(ounit,'("#",X,A,":",A)',iostat=irc) "set","id"
    do ii=1,ncols
       call chop0(col80(ii),80)
       lenc=length(col80(ii),80,1)
       call chop0(exp250(ii),80)
       lene=length(exp250(ii),80,1)
       write(ounit,'("#",X,A,":",A)',iostat=irc)col80(ii)(1:lenc),exp250(ii)(1:lene)
    end do
    if(table_bdeb)write(*,*)myname,'Writing data.',ounit,ncols,size(col80),size(exp250)
    write(ounit,'("#")',iostat=irc)
    write(ounit,'("# Data table")',iostat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Error writing to Table file'.")
       call table_errorappend(crc250,tss%tab250(1:tss%lent))
       return
    end if
    if (ncols.eq.0) then
       write(ounit,'(X,A)',iostat=irc)"set"
    else
       write(ounit,'(X,A)',iostat=irc,advance="no")"set"
    end if
    do ii=1,ncols
       lenc=length(col80(ii),80,1)
       if (ii.eq.ncols) then
          write(ounit,'(X,A)',iostat=irc)col80(ii)(1:lenc)
       else
          write(ounit,'(X,A)',iostat=irc,advance="no")col80(ii)(1:lenc)
       end if
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Error writing to Table file'.")
       call table_errorappend(crc250,tss%tab250(1:tss%lent))
       return
    end if
    !
    if (allocated(var80)) deallocate(var80)
    if (allocated(col80)) deallocate(col80)
    if (allocated(exp250)) deallocate(exp250)
    !
  end subroutine table_addSetComments

  subroutine table_addJoinComments(tss,css,mss,oss,ounit,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(table_session), pointer :: tss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    integer :: cid,mid,oid
    integer :: ounit
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_addJoinComments"
    integer :: ii,jj
    ! print legend table
    write(ounit,'("#")',iostat=irc)
    write(ounit,'("# LEGENDS:",I0)',iostat=irc) tss%nleg
    do ii=1,tss%nleg
       write(ounit,'("#",X,A,":",A)',iostat=irc) &
            & tss%nam80(ii)(1:tss%lenn(ii)),tss%leg250(ii)(1:tss%lenl(ii))
    end do
    ! print legend table
    write(ounit,'("#")',iostat=irc)
    write(ounit,'("# COLUMNS:",I0)',iostat=irc) tss%ncol
    do ii=1,tss%ncol
       write(ounit,'("#",X,A,":",A)',iostat=irc)tss%col80(ii)(1:tss%lenc(ii)),tss%exp250(ii)(1:tss%lene(ii))
    end do
    write(ounit,'("#")',iostat=irc)
    write(ounit,'("# Data table")',iostat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Error writing to Table file'.")
       call table_errorappend(crc250,tss%tab250(1:tss%lent))
       return
    end if
    do ii=1,tss%ncol
       if (ii.eq.tss%ncol) then
          write(ounit,'(X,A)',iostat=irc)tss%col80(ii)(1:tss%lenc(ii))
       else
          write(ounit,'(X,A)',iostat=irc,advance="no")tss%col80(ii)(1:tss%lenc(ii))
       end if
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Error writing to Table file'.")
       call table_errorappend(crc250,tss%tab250(1:tss%lent))
       return
    end if
    !
    return
  end subroutine table_addJoinComments
  !
  subroutine table_setTime(tss,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_clearsetstack"
    !call date_and_time(VALUES=tss%values)
    call parse_date_and_time(tss%values)
    return
  end subroutine table_setTime
  !
  subroutine table_strepTablefile(tss,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_strepTablefile"
    integer,parameter  :: nn = 6
    character*100 :: src100(nn) = (/'YY','MM','DD','HH','MI','SS'/)
    character*100 :: rep100(nn)
    logical :: lrep(nn)
    call table_settime(tss,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from settime.")
       call table_errorappendi(crc250,irc)
       return
    end if
    !  values(1):	the year
    !  values(2):	the month
    !	values(3):	the day of the month
    !	values(4):	time difference with utc in minutes
    !	values(5):	the hour of the day
    !	values(6):	the minutes of the hour
    !	values(7):	the seconds of the minute
    !	values(8):	the milliseconds of the second
    write(rep100(1),'(i4.4)')tss%values(1)
    write(rep100(2),'(i2.2)')tss%values(2)
    write(rep100(3),'(i2.2)')tss%values(3)
    write(rep100(4),'(i2.2)')tss%values(5)
    write(rep100(5),'(i2.2)')tss%values(6)
    write(rep100(6),'(i2.2)')tss%values(7)
    if (tss%lent.ne.0)call table_strep(tss%tab250,nn,src100,rep100,lrep,irc)
    return
  end subroutine table_strepTablefile
  !
  subroutine table_strepGraphicsDir(tss,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_strepGraphicsDir"
    integer,parameter  :: nn = 6
    character*100 :: src100(nn) = (/'YY','MM','DD','HH','MI','SS'/)
    character*100 :: rep100(nn)
    logical :: lrep(nn)
    call table_settime(tss,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from settime.")
       call table_errorappendi(crc250,irc)
       return
    end if
    !  values(1):	the year
    !  values(2):	the month
    !	values(3):	the day of the month
    !	values(4):	time difference with utc in minutes
    !	values(5):	the hour of the day
    !	values(6):	the minutes of the hour
    !	values(7):	the seconds of the minute
    !	values(8):	the milliseconds of the second
    write(rep100(1),'(i4.4)')tss%values(1)
    write(rep100(2),'(i2.2)')tss%values(2)
    write(rep100(3),'(i2.2)')tss%values(3)
    write(rep100(4),'(i2.2)')tss%values(5)
    write(rep100(5),'(i2.2)')tss%values(6)
    write(rep100(6),'(i2.2)')tss%values(7)
    if (tss%leng.ne.0)call table_strep(tss%gra250,nn,src100,rep100,lrep,irc)
    return
  end subroutine table_strepGraphicsDir
  !
  subroutine table_setFile(tss,tab250,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*250 :: tab250
    character*250 :: gra250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_setfile"
    integer,parameter  :: nn = 6
    call table_setTablefile(tss,tab250,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from setTableFile.")
       return
    end if
    call table_strepTablefile(tss,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from strepTableFile.")
       return
    end if
    call table_getTablefile(tss,tab250,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from setTableFile.")
       return
    end if
    return
  end subroutine table_setFile
  !
  !###############################################################################
  ! SET ROUTINES
  !###############################################################################
  !
  subroutine table_clearsetstack(tss,crc250,irc)
    implicit none
    type(table_session), pointer :: tss !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_clearsetstack"
    type(table_set), pointer :: cset,nset !  current set
    cset => tss%firstSet%next
    do while (.not.associated(cset,target=tss%lastSet))
       nset => cset%next
       call table_unlinkSet(cset)
       call table_deallocateSet(cset)
       tss%nset=tss%nset-1
       cset  => nset
    end do
    return
  end subroutine table_clearsetstack
  !
  subroutine table_unlinkSet(set)
    implicit none
    type(table_set), pointer :: set !  current set
    set%prev%next => set%next
    set%next%prev => set%prev
    return
  end subroutine table_unlinkSet
  !
  subroutine table_allocateSet(set,crc250,irc)
    implicit none
    type(table_set), pointer ::  set !  current set
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_allocateset"
    allocate(set,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'set'.")
       return
    end if
    allocate(set%firstObstrg,set%lastObstrg,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'set-obstrg'.")
       return
    end if
    set%firstObstrg%next => set%lastObstrg
    set%lastObstrg%prev => set%firstObstrg
    allocate(set%firstModtrg,set%lastModtrg,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'set-modtrg'.")
    end if
    set%firstModtrg%next => set%lastModtrg
    set%lastModtrg%prev => set%firstModtrg
    allocate(set%firstLoc,set%lastLoc,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'set-def'.")
    end if
    set%firstLoc%next => set%lastLoc
    set%lastLoc%prev => set%firstLoc
    allocate(set%firstMatch,set%lastMatch,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'set-match'.")
    end if
    set%firstMatch%next => set%lastMatch
    set%lastMatch%prev => set%firstMatch
    return
  end subroutine table_allocateSet
  !
  subroutine table_deallocateSet(set)
    implicit none
    type(table_set), pointer :: set !  current set
    integer :: irc2
    character*250 :: crc250
    integer :: irc
    call table_clearObstrgStack(set,crc250,irc)
    call table_clearModtrgStack(set,crc250,irc)
    call table_clearMatchStack(set,crc250,irc)
    deallocate(set,stat=irc2) ! ignore any errors
    return
  end subroutine table_deallocateSet
  !
  subroutine table_clearcolumn(tss,crc250,irc)
    type(table_session), pointer :: tss !  current session
    character*250 :: crc250
    integer :: irc
    type(table_column), pointer :: col, ncol
    character*22 :: myname="table_clearcolumn"
    if(table_bdeb)write(*,*)myname, 'Entering.',irc,&
         & associated(tss%firstColumn),associated(tss%lastColumn)
    col=>tss%firstColumn%next
    do while (.not.associated(col,target=tss%lastColumn))
       ncol=>col%next
       col%prev%next => col%next
       col%next%prev => col%prev
       deallocate(col)
       nullify(col)
       col=>ncol
    end do
    tss%ncol=0
    !tss%firstColumn%next=>tss%lastColumn
    !tss%lastColumn%prev=>tss%firstColumn
    !if(table_bdeb)write(*,*)myname, 'Done.',irc
    return
  end subroutine table_clearcolumn
  !
  subroutine table_pushcolumn(tss,name80,exp250,crc250,irc)
    type(table_session), pointer :: tss !  current session
    character*80 :: name80
    character*250 :: exp250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_pushcolumn"
    type(table_column), pointer :: col
    integer, external :: length
    allocate(col,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'col'.")
       return
    end if
    col%name80=name80
    call chop0(col%name80,80)
    col%lenn=length(col%name80,80,10)
    col%exp250=exp250
    call chop0(col%exp250,250)
    col%lene=length(col%exp250,250,10)
    col%prev => tss%lastColumn%prev
    col%next => tss%lastColumn
    col%prev%next => col
    col%next%prev => col
    tss%ncols=tss%ncols+1
    return
  end subroutine table_pushcolumn
  !
  subroutine table_makeColumn(tss,set,crc250,irc) ! make set-column arrays
    type(table_session), pointer :: tss !  current session
    type(table_set), pointer     :: set !  current set
    character*250 :: crc250
    integer :: irc
    type(table_column), pointer :: col
    integer :: ii
    character*22 :: myname="table_makeColumn"
    if (allocated(set%col80)) deallocate(set%col80)
    if (allocated(set%col_lenn)) deallocate(set%col_lenn)
    if (allocated(set%col_exp250)) deallocate(set%col_exp250)
    if (allocated(set%col_lene)) deallocate(set%col_lene)
    if (tss%ncols > 0) then
       set%ccol=tss%ncols
       allocate(set%col80(set%ccol), &
            & set%col_lenn(set%ccol), &
            & set%col_exp250(set%ccol), &
            & set%col_lene(set%ccol), &
            & stat=irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250,"Unable to allocate 'set%col'.")
          return
       end if
       ii=0
       col=>tss%firstColumn%next
       do while (.not.associated(col,target=tss%lastColumn))
          ii=ii+1
          set%col80(ii)=col%name80
          set%col_lenn(ii)=col%lenn
          set%col_exp250(ii)=col%exp250
          set%col_lene(ii)=col%lene
          col=>col%next
       end do
    end if
    return
  end subroutine table_makeColumn
  !
  subroutine table_pushset(tss,css,mss,oss,name80,leg250,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(table_session), pointer :: tss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    character*80 :: name80
    character*250 leg250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_pushset"
    type(table_set), pointer :: set !  current set
    if(table_bdeb)write(*,*)myname,'Entering.',irc
    call table_allocateSet(set,crc250,irc)
    set%name80=name80
    set%leg250=leg250
    if(table_bdeb)write(*,*)myname,'Importing obs.',irc
    call table_obsImport(set,oss,crc250,irc) ! get obs data 
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from obsImport.")
       return
    end if
    if(table_bdeb)write(*,*)myname,'Importing model.',irc
    call table_modImport(set,mss,crc250,irc) ! get model data
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from modImport.")
       return
    end if
    if(table_bdeb)write(*,*)myname,'Importing colocation.',irc
    call table_colImport(set,css,crc250,irc) ! get colocation data
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from colImport.")
       return
    end if
    call table_makeColumn(tss,set,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from makeColumn.")
       return
    end if
    set%next => tss%lastSet
    set%prev => tss%lastSet%prev
    set%prev%next => set
    set%next%prev => set
    tss%nset=tss%nset+1
    nullify(set)
    !if(table_bdeb)write(*,*)myname,'Exiting.',irc
    return
  end subroutine table_pushset
  !
  ! loop over sets from top and delete them
  logical function table_pullset(tss,css,mss,oss,name80,leg250,level,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(table_session), pointer :: tss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    character*80 :: name80
    character*250 :: leg250
    integer :: level
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_pullset"
    type(table_set), pointer :: set !  current set
    table_pullset=.false. ! only true if all is ok
    set => tss%firstSet%next
    if (.not.associated(set,target=tss%lastSet)) then
       name80=set%name80
       leg250=set%leg250
       call table_obsExport(set,oss,crc250,irc) ! set obs data
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from obsExport.")
          return
       end if
       call table_modExport(set,mss,crc250,irc) ! set model data
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from modExport.")
          return
       end if
       call table_colExport(set,css,crc250,irc) ! set colocation data
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from colExport.")
          return
       end if
       if(table_bdeb)write(*,*)myname,"Cleaning.",irc
       call table_unlinkSet(set)       
       call table_deallocateSet(set)
       table_pullset=.true.
    end if
    if(table_bdeb)write(*,*)myname,"Done.",irc
    return
  end function table_pullset
  !
  ! loop over sets from bottom and delete them
  logical function table_popset(tss,css,mss,oss,name80,leg250,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(table_session), pointer :: tss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    character*80 :: name80
    character*250 :: leg250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_popset"
    type(table_set), pointer :: set !  current set
    table_popset=.false. ! only true if all is ok and irc==0
    set => tss%lastSet%prev
    if (.not.associated(set,target=tss%firstSet)) then
       name80=set%name80
       leg250=set%leg250
       call table_obsExport(set,oss,crc250,irc) ! set obs data
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from obsExport.")
          return
       end if
       call table_modExport(set,mss,crc250,irc) ! set model data
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from modExport.")
          return
       end if
       call table_colExport(set,css,crc250,irc) ! set colocation data
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from colExport.")
          return
       end if
       if(table_bdeb)write(*,*)myname,"Cleaning.",irc
       call table_unlinkSet(set)       
       call table_deallocateSet(set)
       table_popset=.true.
    end if
    if(table_bdeb)write(*,*)myname,"Done.",irc
    return
  end function table_popset
  !
  ! check that sets are comparable (TRUE=ok)
  logical function table_checkSets(tss)
    implicit none
    type(table_session), pointer :: tss !  current session
    type(table_set),pointer :: cSet
    logical ::first
    first = .true.
    table_checkSets=.false.
    if (.not.associated(tss)) return
    cSet => tss%firstSet%next
    do while (.not.associated(cSet,target=tss%lastSet))
       if (first) then
          first=.false.
       else if (.not.table_checkObsTrg(tss%firstSet%next,cSet)) then
          if(table_bdeb)write(*,*)'Obs targets differ.'
          return
       else if (tss%firstSet%next%category.ne.cSet%category) then
          if(table_bdeb)write(*,*)'BUFR categories differ:',&
               & tss%firstSet%next%category,cSet%category
          return
       else if (tss%firstSet%next%subcategory.ne.cSet%subcategory) then
          if(table_bdeb)write(*,*)'BUFR sub-categories differ:',&
               & tss%firstSet%next%subcategory,cSet%subcategory
          return
       end if
       cSet => cSet%next
    end do
    table_checkSets=.true.
    return
  end function table_checkSets
  !
  ! compare observation targets of the two sets (TRUE=ok)
  logical function table_checkObsTrg(set1,set2)
    implicit none
    type(table_set),pointer :: set1,set2
    type(table_obstrg),pointer :: t1,t2
    integer :: ii
    table_checkObsTrg=.false.
    t1 => set1%firstObsTrg%next
    t2 => set1%firstObsTrg%next
    ii=0
    do while (.not.associated(t1,set1%lastObsTrg))
       ii=ii+1
       if (t1%pos250.ne.t2%pos250) then
          if(table_bdeb)write(*,*)'Positions differ.',ii
          return
       else if (t1%descr80.ne.t2%descr80) then
          if(table_bdeb)write(*,*)'Descriptors differ.',ii
          return
       end if
       t1=>t1%next
       t2=>t2%next
    end do
    table_checkObsTrg=.true.
    return
  end function table_checkObsTrg
  !
  ! loop over sets from the top without deleting them...
  logical function table_loopSet(tss,css,mss,oss,name80,ncol,col80,exp250,leg250,level,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(table_session), pointer :: tss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    character*80 :: name80
    integer :: ncol
    character*80, allocatable :: col80(:)
    character*250, allocatable :: exp250(:)
    character*250 :: leg250
    integer :: level
    character*250 :: crc250
    integer :: irc,ii
    integer :: lenn
    integer, external :: length
    character*22 :: myname="table_loopSet"
    table_loopset=.false. ! only true if all is ok
    if (.not.associated(tss%currentSet)) then
       tss%currentSet =>  tss%firstSet%next 
    else
       tss%currentSet =>  tss%currentSet%next
    end if
    if (associated(tss%currentSet,target=tss%lastSet)) then
       nullify(tss%currentSet)
       table_loopset=.false.
    else
       name80=tss%currentSet%name80
       leg250=tss%currentSet%leg250
       if (level.ne.0) then
          if(table_bdeb)then
             lenn=length(tss%currentSet%name80,80,10)
             write(*,*)myname,"Set '"//tss%currentSet%name80(1:lenn)//"'"
          end if
          call table_obsExport(tss%currentSet,oss,crc250,irc) ! set obs data
          if (irc.ne.0) then
             call table_errorappend(crc250,myname)
             call table_errorappend(crc250," Error return from obsExport.")
             return
          end if
          call table_modExport(tss%currentSet,mss,crc250,irc) ! set model data
          if (irc.ne.0) then
             call table_errorappend(crc250,myname)
             call table_errorappend(crc250," Error return from modExport.")
             return
          end if
          call table_colExport(tss%currentSet,css,crc250,irc) ! set colocation data
          if (irc.ne.0) then
             call table_errorappend(crc250,myname)
             call table_errorappend(crc250," Error return from colExport.")
             return
          end if
          !
       end if
       if(table_bdeb)write(*,*)myname,"Make columns.",tss%currentSet%ccol,allocated(col80)
       ncol=tss%currentSet%ccol
       if (.not.allocated(col80).or..not.allocated(exp250).or.&
            & tss%currentSet%ccol > size(col80).or.tss%currentSet%ccol > size(exp250)) then
          if (allocated(col80)) deallocate(col80)
          if (allocated(exp250)) deallocate(exp250)
          if(table_bdeb)write(*,*)myname,"Allocating columns.",ncol
          allocate(col80(ncol),exp250(ncol),stat=irc)
          if (irc.ne.0) then
             call table_errorappend(crc250,myname)
             call table_errorappend(crc250," Unable to allocate columns.")
             return
          end if
       end if
       if(table_bdeb)write(*,*)myname,"Assigning columns.",ncol,&
            & allocated(col80),allocated(exp250),size(col80),size(exp250)
       do ii=1,ncol
          col80(ii)=tss%currentSet%col80(ii)
          exp250(ii)=tss%currentSet%col_exp250(ii)
       end do
       if (table_bdeb)write(*,*)myname,'Columns:',ncol,size(col80),size(exp250)
       !
       table_loopset=.true.
    end if
    if(table_bdeb)write(*,*)myname,"Done.",table_loopset
    return
  end function table_loopSet
  !
  !###############################################################################
  ! SUB DATA ROUTINES (OBS-, MODEL-, MATCH-TARGET)
  !###############################################################################
  !
  !
  subroutine table_clearobstrgstack(set,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_clearobstrgstack"
    type(table_obstrg), pointer :: ctrg,ntrg !  current target
    ctrg => set%firstObstrg%next
    do while (.not.associated(ctrg,target=set%lastObstrg))
       ntrg => ctrg%next
       call table_unlinkObstrg(ctrg)
       set%nObsTrg=set%nObsTrg-1
       call table_deallocateObstrg(ctrg)
       ctrg  => ntrg
    end do
    nullify(set%cobstrg)
    return
  end subroutine table_clearobstrgstack
  !
  subroutine table_clearmodtrgstack(set,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_clearmodtrgstack"
    type(table_modtrg), pointer :: ctrg,ntrg !  current target
    ctrg => set%firstModtrg%next
    do while (.not.associated(ctrg,target=set%lastModtrg))
       ntrg => ctrg%next
       call table_unlinkModtrg(ctrg)
       set%nModTrg=set%nModTrg-1
       call table_deallocateModtrg(ctrg)
       ctrg  => ntrg
    end do
    nullify(set%cmodtrg)
    return
  end subroutine table_clearmodtrgstack
  !
  subroutine table_clearmatchstack(set,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_clearmatchstack"
    type(table_match), pointer :: cmatch,nmatch !  current target
    cmatch => set%firstMatch%next
    do while (.not.associated(cmatch,target=set%lastMatch))
       nmatch => cmatch%next
       call table_unlinkMatch(cmatch)
       call table_deallocateMatch(cmatch)
       set%nmatch=set%nmatch-1
       cmatch  => nmatch
    end do
    nullify(set%cmatch)
    return
  end subroutine table_clearmatchstack
  !
  ! clear the default stack
  !
  subroutine table_cleardefaultStack(set,crc250,irc) 
    type(table_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    type(table_location), pointer :: cLoc !  the current default target
    type(table_location), pointer :: nLoc !  the next default target
    character*25 :: myname="table_cleardefaultStack"
    if (table_bdeb) write(*,*)myname,'Entering.',associated(set), associated(set%firstLoc)
    cLoc => set%firstLoc%next
    do while (.not.associated(cLoc,target=set%lastLoc))
       nLoc => cLoc%next
       cLoc%prev%next =>  cLoc%next
       cLoc%next%prev =>  cLoc%prev
       deallocate(cLoc,stat=irc)
       cLoc => nLoc
    end do
    set%ndef=0
    if (table_bdeb) write(*,*)myname,'Exiting.',irc
    return
  end subroutine table_cleardefaultStack
  !
  subroutine table_unlinkObstrg(trg)
    implicit none
    type(table_obstrg), pointer :: trg !  current target
    trg%prev%next => trg%next
    trg%next%prev => trg%prev
    return
  end subroutine table_unlinkObstrg
  !
  subroutine table_unlinkModtrg(trg)
    implicit none
    type(table_modtrg), pointer :: trg !  current target
    trg%prev%next => trg%next
    trg%next%prev => trg%prev
    return
  end subroutine table_unlinkModtrg
  !
  subroutine table_unlinkMatch(match)
    implicit none
    type(table_match), pointer :: match !  current target
    match%prev%next => match%next
    match%next%prev => match%prev
    return
  end subroutine table_unlinkMatch
  !
  subroutine table_unlinkDefault(def)
    implicit none
    type(table_default), pointer :: def !  current target
    def%prev%next => def%next
    def%next%prev => def%prev
    return
  end subroutine table_unlinkDefault
  !
  subroutine table_pushobstrg(set,trg80,pos250,descr80,info250,min80,max80,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*80 :: trg80      ! target name
    character*250 :: pos250    ! position/sequence number
    character*80 :: descr80    ! descriptor
    character*250 :: info250   ! information
    character*80 :: min80      ! min value
    character*80 :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_pushobstrg"
    type(table_obstrg), pointer :: trg !  current target
    allocate(trg,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'obstrg'.")
    end if
    trg%trg80=trg80
    trg%pos250=pos250
    trg%descr80=descr80
    trg%info250=info250
    trg%min80=min80
    trg%max80=max80
    trg%next => set%lastObstrg
    trg%prev => set%lastObstrg%prev
    set%nObsTrg=set%nObsTrg+1
    trg%prev%next => trg
    trg%next%prev => trg
    nullify(trg)
    return
  end subroutine table_pushobstrg
  !
  subroutine table_pushmodtrg(set,trg80,var80,min80,max80,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*80 :: trg80      ! target name
    character*80 :: var80    ! descriptor
    character*80 :: min80      ! min value
    character*80 :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_pushmodtrg"
    type(table_modtrg), pointer :: trg !  current target
    allocate(trg,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'modtrg'.")
    end if
    trg%trg80=trg80
    trg%var80=var80
    trg%min80=min80
    trg%max80=max80
    trg%next => set%lastModtrg
    trg%prev => set%lastModtrg%prev
    set%nModTrg=set%nModTrg+1
    trg%prev%next => trg
    trg%next%prev => trg
    nullify(trg)
    return
  end subroutine table_pushmodtrg
  !
  integer function table_countModTrg(set)
    type(table_set),pointer :: set
    table_countModTrg=set%nModTrg
    return
  end function table_countModTrg
  !
  integer function table_countObsTrg(set)
    type(table_set),pointer :: set
    table_countObsTrg=set%nObsTrg
    return
  end function table_countObsTrg
  !
  subroutine table_pushmatch(set,n80,e250,l80,u80,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*80 :: n80      ! target name
    character*250 :: e250    ! position/sequence number
    character*80 :: l80      ! lower
    character*80 :: u80      ! upper
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_pushmatch"
    integer :: lenn
    integer, external :: length
    type(table_match), pointer :: trg !  current target
    allocate(trg,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'match'.")
    end if
    trg%n80=n80
    trg%e250=e250
    trg%l80=l80
    trg%u80=u80
    trg%next => set%lastMatch
    trg%prev => set%lastMatch%prev
    trg%prev%next => trg
    trg%next%prev => trg
    set%nmatch=set%nmatch+1
    if (table_bdeb)then
       lenn=length(n80,80,10)
       write(*,*)myname,' Adding:',n80(1:lenn)
    end if
    nullify(trg)
    return
  end subroutine table_pushmatch
  !
  ! add default element
  !
  subroutine table_addDefault(set,n80,v80,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*80 :: n80 ! target name
    character*80 :: v80 ! target value
    character*250 :: crc250
    integer :: irc
    integer :: ii, irc2, lenv, lenn
    type(table_default),pointer :: newdef
    integer, external :: length
    character*25 :: myname="colocation_addDefault"
    if(table_bdeb)write(*,*)myname,'Entering.',associated(set%currentLoc),irc
    allocate(newdef,stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'newdef'.")
       call table_errorappend(crc250,"\n")
       return
    end if
    newdef%n80=n80
    call chop0(newdef%n80,80)
    newdef%lenn=length(newdef%n80,80,10)
    newdef%v80=v80
    call chop0(newdef%v80,80)
    newdef%lenv=length(newdef%v80,80,10)
    if(table_bdeb)write(*,*)myname," Assigning: '"//newdef%n80(1:newdef%lenn)//&
         & "' -> '"//newdef%v80(1:newdef%lenv)//"'"
    if (.not.associated(set%currentLoc)) then
       if(table_bdeb)write(*,*)myname,'New Default.'
       allocate(set%currentLoc, stat=irc)
       set%currentLoc%next => set%lastLoc
       set%currentLoc%prev => set%lastLoc%prev
       set%lastLoc%prev%next => set%currentLoc
       set%lastLoc%prev => set%currentLoc
       allocate(set%currentLoc%firstDef,set%currentLoc%lastDef, stat=irc) ! 
       set%currentLoc%firstDef%next => set%currentLoc%lastDef
       set%currentLoc%lastDef%prev => set%currentLoc%firstDef
       set%ndef=set%ndef+1
    end if
    if(table_bdeb)write(*,*)myname,'Adding default.'
    newdef%next => set%currentLoc%lastDef
    newdef%prev => set%currentLoc%lastDef%prev
    set%currentLoc%lastDef%prev%next => newdef
    set%currentLoc%lastDef%prev => newdef
    if(table_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine table_addDefault
  !
  subroutine table_maketargetlist(set,crc250,irc)
    type(table_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="table_maketargetlist"
    type(table_modtrg), pointer :: cTrg
    integer ii,lens,irc2
    integer, external :: length
    if(table_bdeb)write(*,*)myname,'Entering.',irc
    set%nTrgMod=0
    cTrg => set%firstModTrg%next
    do while (.not.associated(cTrg,target=set%lastModTrg))
       set%nTrgMod=set%nTrgMod+1
       cTrg => cTrg%next
    end do
    if(associated(set%trgMod80)) deallocate(set%trgMod80)
    if(associated(set%trgModLent)) deallocate(set%trgModLent)
    if (set%nTrgMod.ne.0) then
       allocate(set%trgMod80(set%nTrgMod), set%trgModLent(set%nTrgMod), stat=irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250,"Unable to allocate 'session:trg...'.")
          call table_errorappend(crc250,"\n")
          return
       end if
       ii=0
       cTrg => set%firstModTrg%next
       do while (.not.associated(cTrg,target=set%lastModTrg))
          ii=min(set%nTrgMod,ii+1)
          set%trgMod80(ii)=cTrg%trg80
          call chop0(set%trgMod80(ii),80)
          set%trgmodlent(ii)=length(set%trgMod80(ii),80,10)
          cTrg => cTrg%next
       end do
    end if
    set%trgModSet=.true.
    if(table_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine table_maketargetlist
  !
  ! push default values to the stack
  !
  subroutine table_pushDefault(set,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    type(table_location), pointer :: newLoc
    character*25 :: myname="table_pushDefault"
    if(table_bdeb)write(*,*)myname,'Entering.',irc
    if (.not.associated(set %firstLoc)) then
       allocate(set%firstLoc,set%lastLoc, stat=irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250,"Unable to allocate 'firstDef/lastDef'.")
          call table_errorappend(crc250,"\n")
          return
       end if
       set%firstLoc%next => set%lastLoc
       set%lastLoc%prev => set%firstLoc
       set%ndef=0
    end if
    if (associated(set%currentLoc)) then
       nullify(set%currentLoc)
    end if
    if(table_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine table_pushDefault
  !
  logical function table_popobstrg(set,trg80,pos250,descr80,info250,min80,max80,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*80  :: trg80      ! target name
    character*250 :: pos250    ! position/sequence number
    character*80  :: descr80    ! descriptor
    character*250 :: info250   ! information
    character*80  :: min80      ! min value
    character*80  :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_popobstrg"
    type(table_obstrg), pointer :: trg,ntrg !  current trg
    table_popobstrg=.false. ! only true if all is ok
    trg => set%lastObstrg%prev
    if (.not.associated(trg,set%firstObstrg)) then
       ntrg=>trg%next
       trg80=trg%trg80
       pos250=trg%pos250
       descr80=trg%descr80
       info250=trg%info250
       min80=trg%min80
       max80=trg%max80
       call table_unlinkObstrg(trg)
       set%nObsTrg=set%nObsTrg-1
       trg=>ntrg
       table_popobstrg=.true.
    end if
    return
  end function table_popobstrg
  !
  logical function table_popmodtrg(set,trg80,var80,min80,max80,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*80  :: trg80      ! target name
    character*80  :: var80    ! descriptor
    character*80  :: min80      ! min value
    character*80  :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_popmodtrg"
    type(table_modtrg), pointer :: trg,ntrg !  current trg
    table_popmodtrg=.false. ! only true if all is ok
    trg => set%lastModtrg%prev
    if (.not.associated(trg,set%firstModtrg)) then
       ntrg=>trg%next
       trg80=trg%trg80
       var80=trg%var80
       min80=trg%min80
       max80=trg%max80
       call table_unlinkModtrg(trg)
       set%nModTrg=set%nModTrg-1
       trg=>ntrg
       table_popmodtrg=.true.
    end if
    return
  end function table_popmodtrg
  !
  logical function table_popmatch(set,n80,e250,l80,u80,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*80  :: n80      ! target name
    character*250 :: e250    ! observation expression
    character*80  :: l80      ! lower
    character*80  :: u80      ! upper
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_popmatch"
    type(table_match), pointer :: trg,ntrg !  current trg
    table_popmatch=.false. ! only true if all is ok
    trg => set%lastMatch%prev
    if (.not.associated(trg,set%firstMatch)) then
       ntrg=>trg%next
       n80=trg%n80
       e250=trg%e250
       l80=trg%l80
       u80=trg%u80
      call table_unlinkMatch(trg)
       trg=>ntrg
       table_popmatch=.true.
       set%nmatch=set%nmatch-1
    end if
    return
  end function table_popmatch
  !
  logical function table_loopobstrg(set,trg80,pos250,descr80,info250,min80,max80,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*80  :: trg80      ! target name
    character*250 :: pos250    ! position/sequence number
    character*80  :: descr80    ! descriptor
    character*250 :: info250   ! information
    character*80  :: min80      ! min value
    character*80  :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_loopobstrg"
    table_loopobstrg=.false. ! only true if all is ok
    if (.not.associated(set%cobstrg)) then
       set%cobstrg =>  set%firstObstrg%next 
    else
       set%cobstrg =>  set%cobstrg%next
    end if
    if (associated(set%cobstrg,target=set%lastObstrg)) then
       nullify(set%cobstrg)
       table_loopobstrg=.false.
    else
       trg80=set%cobstrg%trg80
       pos250=set%cobstrg%pos250
       descr80=set%cobstrg%descr80
       info250=set%cobstrg%info250
       min80=set%cobstrg%min80
       max80=set%cobstrg%max80
       table_loopobstrg=.true.
    end if
    return
  end function table_loopobstrg
  !
  logical function table_loopmodtrg(set,trg80,var80,min80,max80,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*80  :: trg80      ! target name
    character*80  :: var80    ! descriptor
    character*80  :: min80      ! min value
    character*80  :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_loopmodtrg"
    !if (table_bdeb) write(*,*)myname,' Entering.',irc
    table_loopmodtrg=.false. ! only true if all is ok
    if (.not.associated(set%cmodtrg)) then
       set%cmodtrg =>  set%firstModtrg%next 
    else
       set%cmodtrg =>  set%cmodtrg%next
    end if
    if (associated(set%cmodtrg,target=set%lastModtrg)) then
       nullify(set%cmodtrg)
       table_loopmodtrg=.false.
    else
       trg80=set%cmodtrg%trg80
       var80=set%cmodtrg%var80
       min80=set%cmodtrg%min80
       max80=set%cmodtrg%max80
       table_loopmodtrg=.true.
    end if
    !if (table_bdeb) write(*,*)myname,' Done.',table_loopmodtrg
    return
  end function table_loopmodtrg
  !
  logical function table_loopLocation(set,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_loopLocation"
    if (table_bdeb) write(*,*)myname,'Entering.',irc
    table_loopLocation=.false. ! only true if all is ok
    if (.not.associated(set%cLoc)) then
       set%cLoc =>  set%firstLoc%next 
    else
       set%cLoc =>  set%cLoc%next
    end if
    if (associated(set%cLoc,target=set%lastLoc)) then
       nullify(set%cLoc)
       table_loopLocation=.false.
    else
       table_loopLocation=.true.
    end if
    return
  end function table_loopLocation
  !
  logical function table_loopDefault(set,n80,v80,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*80  :: n80      ! target name
    character*80  :: v80      ! variable
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_loopdefitem"
    if (table_bdeb) write(*,*)myname,'Entering.',irc
    table_loopDefault=.false. ! only true if all is ok
    if (.not.associated(set%cLoc%cDef)) then
       set%cLoc%cDef =>  set%cLoc%firstDef%next 
    else
       set%cLoc%cDef =>  set%cLoc%cDef%next
    end if
    if (associated(set%cLoc%cDef,target=set%cLoc%lastDef)) then
       nullify(set%cLoc%cDef)
       table_loopDefault=.false.
    else
       table_loopDefault=.true.
       n80=set%cLoc%cDef%n80
       v80=set%cLoc%cDef%v80
    end if
    return
  end function table_loopDefault
  !
  logical function table_loopmatch(set,n80,e250,l80,u80,crc250,irc)
    implicit none
    type(table_set), pointer :: set !  current session
    character*80  :: n80      ! target name
    character*250 :: e250   ! information
    character*80  :: l80      ! min value
    character*80  :: u80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_loopmatch"
    table_loopmatch=.false. ! only true if all is ok
    if (.not.associated(set%cmatch)) then
       set%cmatch =>  set%firstMatch%next 
    else
       set%cmatch =>  set%cmatch%next
    end if
    if (associated(set%cmatch,target=set%lastMatch)) then
       nullify(set%cmatch)
       table_loopmatch=.false.
    else
       n80=set%cmatch%n80
       e250=set%cmatch%e250
       l80=set%cmatch%l80
       u80=set%cmatch%u80
       table_loopmatch=.true.
    end if
    return
  end function table_loopmatch
  !
  subroutine table_deallocateObstrg(trg)
    implicit none
    type(table_obstrg), pointer :: trg !  current target
    integer :: irc2
    deallocate(trg,stat=irc2) ! ignore any errors
    return
  end subroutine table_deallocateObstrg
  !
  subroutine table_deallocateModtrg(trg)
    implicit none
    type(table_modtrg), pointer :: trg !  current target
    integer :: irc2
    deallocate(trg,stat=irc2) ! ignore any errors
    return
  end subroutine table_deallocateModtrg
  !
  subroutine table_deallocateMatch(match)
    implicit none
    type(table_match), pointer :: match !  current target
    integer :: irc2
    deallocate(match,stat=irc2) ! ignore any errors
    return
  end subroutine table_deallocateMatch
  !
  subroutine table_obsImport(set,oss,crc250,irc) ! get obs data
    use observations
    implicit none
    type(table_set), pointer :: set
    type(obs_session), pointer :: oss
    character*250 :: crc250
    integer :: irc
    character*80 :: trg80,descr80,min80,max80
    character*250 :: pos250,info250
    character*22 :: myname="table_obsImport"
    call observation_getTablePath(oss,set%tablepath,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from getTablePath.")
       return
    end if
    call observation_getBufrType(oss,set%category,set%subCategory,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from getBufrType.")
       return
    end if
    call observation_getIndex(oss,set%ind_obs80,set%ind_exp250,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from getIndex.")
       return
    end if
    call observation_getIndexLimitsRaw(oss,set%ind,set%ind_start,set%ind_stop)
    ! cache file is stored in colocation-module
    call table_clearObstrgStack(set,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from clearObstrgStack.")
       return
    end if
    do while (observation_loopTarget(oss,trg80,pos250,descr80,info250,min80,max80,crc250,irc))
       call table_pushobstrg(set,trg80,pos250,descr80,info250,min80,max80,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from pushobstrg.")
          return
       end if
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from loopTarget.")
       return
    end if
    call observation_getfilter(oss,set%fltobs250,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from getobsfilter.")
       return
    end if
    return
  end subroutine table_obsImport
  !
  subroutine table_obsExport(set,oss,crc250,irc) ! set obs data
    use observations
    implicit none
    type(table_set), pointer :: set
    type(obs_session), pointer :: oss
    integer :: cnt
    logical :: bex
    character*250 :: crc250
    integer :: irc
    character*80 :: trg80,descr80,min80,max80
    character*250 :: pos250,info250
    integer :: lenn
    integer, external :: length
    character*22 :: myname="table_obsExport"
    if (table_countObsTrg(set).eq.0) return ! no targets = nothing to do
    call observation_setTablePath(oss,set%tablepath,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from setTablePath.")
       return
    end if
    call observation_setBufrType(oss,set%category,set%subCategory,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from setBufrType.")
       return
    end if
    call observation_setIndex(oss,set%ind_obs80,set%ind_exp250,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from setIndex.")
       return
    end if
    call observation_setIndexLimitsRaw(oss,set%ind,set%ind_start,set%ind_stop)
    call observation_loadCache(oss,set%obs250,crc250,irc) ! stored in colocation module
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from loadCache.")
       return
    end if
    call observation_clearTargetStack(oss,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from clearTargetStack.")
       return
    end if
    do while (table_loopObstrg(set,trg80,pos250,descr80,info250,min80,max80,crc250,irc))
       if (table_bdeb) then
          lenn=length(trg80,80,10)
          write(*,*)myname,"Inside loopObsTrg '"//trg80(1:lenn)//"'"
       end if
       call observation_pushtarget(oss,trg80,pos250,descr80,info250,min80,max80,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from pushtarget.")
          return
       end if
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from loopObstrg.")
       return
    end if
    call observation_makeTargetList(oss,crc250,irc) ! make target list from target chain
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from makeTargetList.")
       return
    end if
    cnt = observation_targetCount(oss,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from targetCount.")
       return
    end if
    bex= observation_hasValidIndex(oss,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from hasValidIndex.")
       return
    end if
    call observation_setfilter(oss,set%fltobs250,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from setfilter.")
       return
    end if
    return
  end subroutine table_obsExport
  !
  subroutine table_modImport(set,mss,crc250,irc) ! get model data
    use model
    implicit none
    type(table_set), pointer :: set
    type(mod_session), pointer :: mss
    character*250 :: crc250
    integer :: irc
    character*80  :: n80       ! target name
    character*80  :: v80       ! variable
    character*80  :: l80      ! min value
    character*80  :: u80      ! max value
    character*22 :: myname="table_modImport"
    if (table_bdeb) write(*,*)myname,'Entering.',irc
    call model_getIndex(mss,set%ind_trg80,set%ind_mod80,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from getIndex.")
       return
    end if
    if (table_bdeb) write(*,*)myname,'Get target stack.',irc
    call table_clearModTrgStack(set,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from clearModTrgStack.")
       return
    end if
    do while (model_loopTarget(mss,n80,v80,l80,u80,crc250,irc))
       call table_pushModtrg(set,n80,v80,l80,u80,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from pushModtrg.")
          return
       end if
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from loopTarget.")
       return
    end if
    call model_getfilter(mss,set%fltmod250,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from getfilter.")
       return
    end if
    if (table_bdeb) write(*,*)myname,'Done.',irc
    return
  end subroutine table_modImport
  !
  subroutine table_modExport(set,mss,crc250,irc) ! set model data
    use model
    implicit none
    type(table_set), pointer :: set
    type(mod_session), pointer :: mss
    character*250 :: crc250
    integer :: irc
    character*80 :: n80,v80,l80,u80
    integer :: lenn
    integer, external :: length
    character*22 :: myname="table_modExport"
    if (table_countModTrg(set).eq.0) return ! no targets = nothing to do
    call model_setIndex(mss,set%ind_trg80,set%ind_mod80,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from setIndex.")
       return
    end if
    call model_loadCache(mss,set%mod250,crc250,irc) ! stored in colocation module
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from loadCache.")
       return
    end if
    !
    call model_clearTargetStack(mss,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from clearTargetStack.")
       return
    end if
    do while (table_loopModTrg(set,n80,v80,l80,u80,crc250,irc))
       if (table_bdeb) then
          lenn=length(n80,80,10)
          write(*,*)myname,"Inside loopModTrg '"//n80(1:lenn)//"'"
       end if
       call model_pushTarget(mss,n80,v80,l80,u80,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from pushTarget.")
          return
       end if
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from loopModTrg.")
       return
    end if
    call model_setfilter(mss,set%fltmod250,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from setfilter.")
       return
    end if
    !
    return
  end subroutine table_modExport
  !
  subroutine table_colImport(set,css,crc250,irc) ! get colocation data
    use colocation
    implicit none
    type(table_set), pointer :: set
    type(col_session), pointer :: css
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    character*80 :: n80,v80,l80,u80
    character*250 :: e250
    integer, external :: length
    integer :: leno,lenn
    character*22 :: myname="table_colImport"
    if (table_bdeb) write(*,*)myname,'Entering.'
    call colocation_getmodcache(css,set%mod250,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from getmodcache.")
       return
    end if
    call colocation_getobscache(css,set%obs250,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from getobscache.")
       return
    end if
    if (table_bdeb) then
       leno=length(set%obs250,250,10)
       write(*,*)myname,"Get default stack '"//set%obs250(1:leno)//"'",irc
    end if
    call table_clearDefaultStack(set,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from clearDefaultStack.")
       return
    end if
    if (table_bdeb)write(*,*)myname,'Looping over default.'
    do while (colocation_loopLocation(css,crc250,irc))
       do while (colocation_loopDefault(css,n80,v80,crc250,irc))
          if (table_bdeb) then
             lenn=length(n80,80,10)
             write(*,*)myname,"Inside loopDefault '"//n80(1:lenn)//"'"
          end if
          call table_addDefault(set,n80,v80,crc250,irc)
          if (irc.ne.0) then
             call table_errorappend(crc250,myname)
             call table_errorappend(crc250," Error return from addDefault.")
             return
          end if
       end do
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from loopDefaultItem.")
          return
       end if
       call table_pushDefault(set,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from pushDefault.")
          return
       end if
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from loopDefault.")
       return
    end if
    if (table_bdeb) write(*,*)myname,'Get match stack.',irc
    call table_clearMatchStack(set,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from clearMatchStack.")
       return
    end if
    if (table_bdeb) write(*,*)myname,'Starting match loop.',irc
    do while (colocation_loopMatch(css,n80,e250,l80,u80,crc250,irc))
       if (table_bdeb) then
          lenn=length(n80,80,10)
          write(*,*)myname,"Inside loopMatch '"//n80(1:lenn)//"'"
       end if
       call table_pushMatch(set,n80,e250,l80,u80,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from pushMatch.")
          return
       end if
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from loopMatch.")
       return
    end if
    call colocation_clearDefaultStack(css,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from clearDefaultStack.")
       return
    end if
    call colocation_clearMatchStack(css,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from clearMatchStack.")
       return
    end if
    return
  end subroutine table_colImport
  !
  subroutine table_colExport(set,css,crc250,irc) ! set colocation data
    use colocation
    use model
    use observations
    implicit none
    type(table_set), pointer :: set
    type(col_session), pointer :: css
    character*250 :: crc250
    integer :: irc
    character*80 :: n80,v80,l80,u80
    character*250 :: e250
    integer :: lenn
    integer, external :: length
    character*22 :: myname="table_colExport"
    if (table_bdeb) write(*,*)myname,'Entering.'
    call colocation_clearDefaultStack(css,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from clearDefaultStack.")
       return
    end if
    call colocation_clearMatchStack(css,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from clearMatchStack.")
       return
    end if
    if (table_bdeb) write(*,*)myname,'Looping defaults.'
    do while (table_loopLocation(set,crc250,irc))
       do while (table_loopDefault(set,n80,v80,crc250,irc))
          if (table_bdeb) then
             lenn=length(n80,80,10)
             write(*,*)myname,"Inside loopDeafult '"//n80(1:lenn)//"'"
          end if
          call colocation_addDefault(css,n80,v80,crc250,irc)
          if (irc.ne.0) then
             call table_errorappend(crc250,myname)
             call table_errorappend(crc250," Error return from addDefault.")
             return
          end if
       end do
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from loopDefault.")
          return
       end if
       call colocation_pushDefault(css,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from pushDefault.")
          return
       end if
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from loopLocation.")
       return
    end if
    !
    if(table_bdeb)write(*,*)myname,"Setting colocation match."
    do while (table_loopMatch(set,n80,e250,l80,u80,crc250,irc))
       if (table_bdeb) then
          lenn=length(n80,80,10)
          write(*,*)myname,"Inside loopMatch '"//n80(1:lenn)//"'",set%nmatch
       end if
       call colocation_pushMatch(css,n80,e250,l80,u80,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from pushMatch.")
          return
       end if
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from loopMatch.")
       return
    end if
    if(table_bdeb)write(*,*)myname,"Done.",irc
    !
    return
  end subroutine table_colExport
  !
  !
  !###############################################################################
  ! OUTPUT ROUTINES
  !###############################################################################
  !
  !
  subroutine table_maketablefile(tss,css,mss,oss,tab250,gra250,cat250,test,fill250,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(table_session), pointer :: tss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    integer :: cid,mid,oid
    character*250 :: tab250,gra250,cat250
    integer :: test
    character*250 :: fill250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_maketableFile"
    integer :: ounit, ocnt
    character*250 :: leg250
    character*80 :: name80
    integer, external :: length
    integer :: lenn,lent
    integer :: ii,jj
    integer :: ncol
    integer :: lcnt
    character*80, allocatable :: col80(:)
    character*250, allocatable :: exp250(:)
    !
    if(table_bdeb)write(*,*)myname,'Entering.',irc
    !
    ! open table file
    call table_openFile(tss,ounit,tab250,lent,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," unable to open "//tab250(1:lent))
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    ocnt=0;
    !
    call table_addTopComments(tss,css,mss,oss,ounit,tab250,lent,gra250,cat250,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from addTopComments.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    call table_addSetComments(tss,css,mss,oss,ounit,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from addSetComments.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    ! close table output file unit, ounit
    call table_closeFile(tss,ounit,tab250,lent,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," unable to close "//tab250(1:lent))
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    !
    if(table_bdeb)then
       if (table_checkSets(tss))then
          write(*,*)myname,'Observation targets differ...'
       end if
    end if
    lcnt=0
    do while (table_loopset(tss,css,mss,oss,name80,ncol,col80,exp250,leg250,1,crc250,irc))
       call observation_resetStat(oss,crc250,irc)
       call model_resetStat(mss,crc250,irc)
       lcnt=lcnt+1
       !if (lcnt.eq.2) mod_bdeb=.true.
       !parse_bdeb=mod_bdeb
       !if (table_bdeb) then
       lenn=length(name80,80,10)
       write(*,*)myname,"Processing set '"//name80(1:lenn)//"'"
       !end if
       ! append table file
       call table_openFileApp(tss,ounit,tab250,lent,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," unable to append "//tab250(1:lent))
          call table_errorappendi(crc250,irc)
          call table_errorappend(crc250,"\n")
          return
       end if
       ! make output data for this set
       irc=0
       if(table_bdeb)write(*,*)myname,'Making table.',ounit,ncol,size(col80),size(exp250)
       call colocation_makeTableFile(css,mss,oss,ounit,ocnt,name80,ncol,col80,&
            & exp250,leg250,test,fill250,crc250,irc)
       if (irc.ne.0) then
          call chop0(name80,80)
          lenn=length(name80,80,10)
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from makeOutput.")
          call table_errorappend(crc250,name80(1:lenn))
          return
       end if
       ! close table output file unit, ounit
       call table_closeFile(tss,ounit,tab250,lent,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," unable to close "//tab250(1:lent))
          call table_errorappendi(crc250,irc)
          call table_errorappend(crc250,"\n")
          return
       end if
       call model_printFStat(mss,crc250,irc)
       call observation_printStat(oss,crc250,irc)
       call model_printLStat(mss,crc250,irc)
    end do
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from loopset.")
       return
    end if
    !
    if (irc.ne.0) irc=0 ! oh well...
    !
    if (allocated(col80)) deallocate(col80)
    if (allocated(exp250)) deallocate(exp250)
    !
    write(*,'(X,A,I0,A)') myname,ocnt," locations written to table file "//tab250(1:lent)
    if(table_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine table_maketablefile
  !
  subroutine table_stackclear(tss,crc250,irc)
    type(table_session), pointer :: tss !  current session
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    integer :: lens
    character*22 :: myname="table_stackclear"
    if(table_bdeb)write(*,*)myname,' Entering.'
    ! mark as prepared
    tss%stackReady=.false.
    !
    if(table_bdeb)write(*,*)myname,' Removing files.'
    call table_removeFiles(tss,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from table_removeFiles.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    if (tss%nFileIndexes .ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," System error A:")
       call table_errorappendi(crc250,tss%nFileIndexes)
       call table_errorappend(crc250,"\n")
       irc=940
       return
    end if
    if(table_bdeb)write(*,*)myname,' Done.'
  end subroutine table_stackclear
  !
  subroutine table_stackpush(tss,path250,crc250,irc)
    type(table_session), pointer :: tss !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    type(table_file),pointer :: newFile
    logical  :: bok =.false.
    INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:) :: start, vsize, atypes
    character(len=80), allocatable, dimension(:) :: dimnames
    integer :: nvalues, tsize, ndims
    integer :: ii,jj,kk,tt
    CHARACTER(LEN=80)               :: varname
    integer, external :: length
    integer :: lenc,leni,lenv,lens,lenp,lend
    logical :: bbok
    character*22 :: myname="table_stackpush"
    if(table_bdeb)write(*,*) myname,' Entering.',irc
    call chop0(path250,250)
    lenp=length(path250,250,20)
    write(*,*)myname," Pushing '"//path250(1:lenp)//"'"
    ! create new stack-item
    bok=.true.
    allocate(newFile,stat=irc)
    if (irc.ne.0) then
       bok=.false.
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Unable to allocate new stack item.")
       call table_errorappend(crc250,"\n")
       return
    end if
    ! push onto stack
    if (bok) then
       tss%nFileIndexes=tss%nFileIndexes + 1
       tss%stackReady=.false.
       newFile%prev => tss%lastFile%prev
       newFile%next => tss%lastFile
       newFile%prev%next => newFile
       newFile%next%prev => newFile
       ! set file name...
       newFile%fn250=path250
       call chop0(newFile%fn250,250)
       newFile%lenf=length(newFile%fn250,250,20)
       tss%currentFile=>newFile
    end if
    ! open file
    if (bok) then
       ! open file
       call table_scanCurrentFile(tss,bok,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from table_scanCurrentFile.")
          call table_errorappendi(crc250,irc)
          call table_errorappend(crc250,"\n")
          return
       end if
    end if
    if (table_bdeb) call table_printFileStack(tss,crc250,irc)
    if(table_bdeb)write(*,*)myname,' Done.',irc
  end subroutine table_stackpush
  !
  subroutine table_removeFiles (tss,crc250,irc)
    type(table_session), pointer :: tss !  current session
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    type(table_file), pointer :: currentFile,nextFile
    character*22 :: myname="table_removeFiles "
    currentFile => tss%firstFile%next
    do while (.not.associated(currentFile,target=tss%lastFile))
       nextFile => currentFile%next
       call table_removeFile (tss,currentFile,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from table_removeFile.")
          call table_errorappendi(crc250,irc)
          call table_errorappend(crc250,"\n")
          return
       end if
       currentFile => nextFile
    end do
    tss%nleg=0
    tss%ncol=0
    if (allocated(tss%nam80)) deallocate(tss%nam80)
    if (allocated(tss%lenn)) deallocate(tss%lenn)
    if (allocated(tss%leg250)) deallocate(tss%leg250)
    if (allocated(tss%lenl)) deallocate(tss%lenl)
    if (allocated(tss%col80)) deallocate(tss%col80)
    if (allocated(tss%lenc)) deallocate(tss%lenc)
    if (allocated(tss%exp250)) deallocate(tss%exp250)
    if (allocated(tss%lene)) deallocate(tss%lene)
    return
  end subroutine table_removeFiles
  !
  subroutine table_removeFile (tss,currentFile,crc250,irc)
    type(table_session), pointer :: tss !  current session
    type(table_file), pointer :: currentFile
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    character*22 :: myname="table_removeFile"
    if (associated(currentFile)) then
       !if(obs_bdeb)write(*,*)myname,' Updating inventory.'
       tss%nFileIndexes = tss%nFileIndexes - 1
       tss%stackReady=.false.
       if (allocated(tss%currentFile%nam80)) deallocate(tss%currentFile%nam80)
       if (allocated(tss%currentFile%lenn)) deallocate(tss%currentFile%lenn)
       if (allocated(tss%currentFile%leg250)) deallocate(tss%currentFile%leg250)
       if (allocated(tss%currentFile%lenl)) deallocate(tss%currentFile%lenl)
       if (allocated(tss%currentFile%legind)) deallocate(tss%currentFile%legind)
       if (allocated(tss%currentFile%col80)) deallocate(tss%currentFile%col80)
       if (allocated(tss%currentFile%lenc)) deallocate(tss%currentFile%lenc)
       if (allocated(tss%currentFile%exp250)) deallocate(tss%currentFile%exp250)
       if (allocated(tss%currentFile%lene)) deallocate(tss%currentFile%lene)
       if (allocated(tss%currentFile%colind)) deallocate(tss%currentFile%colind)
       if (allocated(tss%currentFile%invind)) deallocate(tss%currentFile%invind)
       currentFile%next%prev => currentFile%prev
       currentFile%prev%next => currentFile%next
       nullify(currentFile%prev)
       nullify(currentFile%next)
       deallocate(currentFile) ! arrays are deallocated automatically
    end if
    return
  end subroutine table_removeFile
  !
  subroutine table_scanCurrentFile(tss,bok,crc250,irc)
    type(table_session), pointer :: tss !  current session
    logical :: bok
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_scanCurrentFile"
    integer :: cnt
    logical :: bbok
    character*1000 :: line1000
    integer :: ii,mode,trg,pos
    integer, external :: length
    logical :: bdone,bbdone
    if(table_bdeb)write(*,*)myname,' Entering.',irc
    if (.not.associated(tss%currentFile)) then
       irc=991
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Missing current file.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    !
    ! open file
    !
    call table_openCurrentFile(tss,bok,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from table_openCurrentFile.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    !
    ! loop over file and store JoinComments...
    if (tss%currentFile%unit .ne. 0) then
       ! read comments
       mode=0
       cnt=0
       trg=0
       bdone=.false.
       do while (.not. bdone)
          read(tss%currentFile%unit,'(A)',iostat=irc) line1000
          if(table_bdeb)write(*,'(X,A,A,I0,":",A)')myname,' Read ',mode,trim(line1000)
          if (irc.ne.0) then
             irc=0
             bdone=.true.
             if(table_bdeb)write(*,*)myname,'Corrupt comments:'//tss%currentFile%fn250(1:tss%currentFile%lenf)
          else if (line1000(1:7).eq."# TYPE:") then ! type
             tss%currentFile%type250=line1000(8:8+250)
             call chop0(tss%currentFile%type250,250)
             tss%currentFile%lent=length(tss%currentFile%type250,250,10)
          else if (mode.eq.1) then ! legend
             ii=1
             bbdone=(line1000(ii:ii).eq.":")
             do while(.not.bbdone)
                ii=ii+1
                if (ii.gt.1000) then
                   bbdone=.true.
                else if (line1000(ii:ii).eq.":") then
                   bbdone=.true.
                end if
             end do
             if (ii.lt.1000) then
                pos=max(1,min(tss%currentFile%nleg,cnt))
                tss%currentFile%nam80(pos)=line1000(2:ii-1)
                call chop0(tss%currentFile%nam80(pos),80)
                tss%currentFile%lenn(pos)=length(tss%currentFile%nam80(pos),80,10)
                tss%currentFile%leg250(pos)=line1000(ii+1:min(1000,ii+250))
                call chop0(tss%currentFile%leg250(pos),250)
                tss%currentFile%lenl(pos)=length(tss%currentFile%leg250(pos),250,10)
             end if
             cnt=cnt+1
             if (cnt.gt.trg) mode=0
          else if (line1000(1:10).eq."# LEGENDS:") then ! legend
             cnt=1
             mode=1
             read(line1000(11:1000),*,IOSTAT=irc) trg
             tss%currentFile%nleg=max(1,trg)
             if (allocated(tss%currentFile%nam80)) deallocate(tss%currentFile%nam80)
             if (allocated(tss%currentFile%lenn)) deallocate(tss%currentFile%lenn)
             if (allocated(tss%currentFile%leg250)) deallocate(tss%currentFile%leg250)
             if (allocated(tss%currentFile%lenl)) deallocate(tss%currentFile%lenl)
             if (allocated(tss%currentFile%legind)) deallocate(tss%currentFile%legind)
             allocate(tss%currentFile%nam80(tss%currentFile%nleg),&
                  & tss%currentFile%lenn(tss%currentFile%nleg),&
                  & tss%currentFile%leg250(tss%currentFile%nleg),&
                  & tss%currentFile%lenl(tss%currentFile%nleg),&
                  & tss%currentFile%legind(tss%currentFile%nleg),stat=irc)
             if (irc.ne.0) then
                call table_errorappend(crc250,myname)
                call table_errorappend(crc250,"Unable to allocate 'currentFile%leg'.")
                call table_errorappend(crc250,"\n")
                return
             end if
          else if (mode.eq.2) then ! columns
             ii=1
             bbdone=(line1000(ii:ii).eq.":")
             do while(.not.bbdone)
                ii=ii+1
                if (ii.gt.1000) then
                   bbdone=.true.
                else if (line1000(ii:ii).eq.":") then
                   bbdone=.true.
                end if
             end do
             if (ii.lt.1000) then
                pos=max(1,min(tss%currentFile%ncol,cnt))
                tss%currentFile%col80(pos)=line1000(2:ii-1)
                call chop0(tss%currentFile%col80(pos),80)
                tss%currentFile%lenc(pos)=length(tss%currentFile%col80(pos),80,10)
                tss%currentFile%exp250(pos)=line1000(ii+1:min(1000,ii+250))
                call chop0(tss%currentFile%exp250(pos),250)
                tss%currentFile%lene(pos)=length(tss%currentFile%exp250(pos),250,10)
             end if
             cnt=cnt+1
             if (cnt.gt.trg) mode=0
          else if (line1000(1:10).eq."# COLUMNS:") then ! columns
             cnt=1
             mode=2
             read(line1000(11:1000),*,IOSTAT=irc) trg
             tss%currentFile%ncol=max(1,trg)
             if (allocated(tss%currentFile%col80)) deallocate(tss%currentFile%col80)
             if (allocated(tss%currentFile%lenc)) deallocate(tss%currentFile%lenc)
             if (allocated(tss%currentFile%exp250)) deallocate(tss%currentFile%exp250)
             if (allocated(tss%currentFile%lene)) deallocate(tss%currentFile%lene)
             if (allocated(tss%currentFile%colind)) deallocate(tss%currentFile%colind)
             allocate(tss%currentFile%col80(tss%currentFile%ncol),&
                  & tss%currentFile%lenc(tss%currentFile%ncol),&
                  & tss%currentFile%exp250(tss%currentFile%ncol),&
                  & tss%currentFile%lene(tss%currentFile%ncol),&
                  & tss%currentFile%colind(tss%currentFile%ncol),stat=irc)
             if (irc.ne.0) then
                call table_errorappend(crc250,myname)
                call table_errorappend(crc250,"Unable to allocate 'currentFile%col'.")
                call table_errorappend(crc250,"\n")
                return
             end if
          else if (line1000(1:1).ne."#") then
             bdone=.true.
          end if
       end do
    end if
    !
    ! close file
    !
    call table_closeCurrentFile(tss,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from table_closeCurrentFile.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    !
    if (bok) then
       if(table_bdeb)write(*,*)myname,' Storing.',irc
       !
       call table_makeIndexes(tss,tss%currentFile,bok,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," Error return from table_makeIndexes.")
          call table_errorappendi(crc250,irc)
          call table_errorappend(crc250,"\n")
          return
       end if
    end if
    !
    if(table_bdeb)write(*,*)myname,' Done.',irc
    !
    return
  end subroutine table_scanCurrentFile

  subroutine table_makeIndexes(tss,file,bok,crc250,irc)
    type(table_session), pointer :: tss !  session
    type(table_file), pointer :: file   !  file
    logical :: bok
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="table_makeIndexes"
    !
    call table_makeLegInd(tss,file,bok,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from table_makeLegInd.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    !
    call table_makeColInd(tss,file,bok,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from table_makeColInd.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    return
  end subroutine table_makeIndexes

  subroutine table_makeLegInd(tss,file,bok,crc250,irc)
    type(table_session), pointer :: tss !  session
    type(table_file), pointer :: file   !  file
    logical :: bok
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="table_makeLegInd"
    integer :: ii,jj,nfound
    character*80 :: nam80(file%nleg+tss%nleg)
    integer :: lenn(file%nleg+tss%nleg)
    character*250 :: leg250(file%nleg+tss%nleg)
    integer :: lenl(file%nleg+tss%nleg)
    integer, external :: length
    !
    ! check if there are legends in file and not in session
    !
    nfound=0 ! legends not in session
    do ii=1,file%nleg
       file%legind(ii)=0
       do jj=1,tss%nleg
          if (tss%leg250(jj)(1:tss%lenl(jj)).eq.file%leg250(ii)(1:file%lenl(ii))) then
             file%legind(ii)=jj
          end if
       end do
       if (file%legind(ii).eq.0) then
          nfound=nfound+1
          jj=nfound+tss%nleg
          file%legind(ii)=jj
          leg250(jj)=file%leg250(ii)
          lenl(jj)=file%lenl(ii)
          ! make new global identifier for legend...
          write(nam80(jj),'(I0)') jj
          call chop0(nam80(jj),80)
          lenn(jj)=length(nam80(jj),80,10)
          ! nam80(jj)=file%nam80(ii)
          ! lenn(jj)=file%lenn(ii)
       end if
    end do
    if (nfound.ne.0) then ! add legens to session
       do ii=1,tss%nleg
          nam80(ii)=tss%nam80(ii)
          lenn(ii)=tss%lenn(ii)
          leg250(ii)=tss%leg250(ii)
          lenl(ii)=tss%lenl(ii)
       end do
       tss%nleg=nfound+tss%nleg
       if (allocated(tss%nam80)) deallocate(tss%nam80)
       if (allocated(tss%lenn)) deallocate(tss%lenn)
       if (allocated(tss%leg250)) deallocate(tss%leg250)
       if (allocated(tss%lenl)) deallocate(tss%lenl)
       allocate(tss%nam80(tss%nleg),&
            & tss%lenn(tss%nleg),&
            & tss%leg250(tss%nleg),&
            & tss%lenl(tss%nleg),stat=irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250,"Unable to allocate 'currentFile%leg'.")
          call table_errorappend(crc250,"\n")
          return
       end if
       do ii=1,tss%nleg
          tss%nam80(ii)=nam80(ii)
          tss%lenn(ii)=lenn(ii)
          tss%leg250(ii)=leg250(ii)
          tss%lenl(ii)=lenl(ii)
       end do
    end if
    return
  end subroutine table_makeLegInd
  !
  subroutine table_makeColInd(tss,file,bok,crc250,irc)
    type(table_session), pointer :: tss !  session
    type(table_file), pointer :: file   !  file
    logical :: bok
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="table_makeColInd"
    integer :: ii,jj,nfound
    character*80 :: col80(file%ncol+tss%ncol)
    integer :: lenc(file%ncol+tss%ncol)
    character*250 :: exp250(file%ncol+tss%ncol)
    integer :: lene(file%ncol+tss%ncol)
    !
    ! check if there are colends in file and not in session
    !
    nfound=0 ! columns not in session
    do ii=1,file%ncol
       file%colind(ii)=0
       do jj=1,tss%ncol
          if (tss%col80(jj)(1:tss%lenc(jj)).eq.file%col80(ii)(1:file%lenc(ii))) then
             file%colind(ii)=jj
          end if
       end do
       if (file%colind(ii).eq.0) then
          nfound=nfound+1
          jj=nfound+tss%ncol
          file%colind(ii)=jj
          col80(jj)=file%col80(ii)
          lenc(jj)=file%lenc(ii)
          exp250(jj)=file%exp250(ii)
          lene(jj)=file%lene(ii)
       end if
    end do
    if (nfound.ne.0) then ! add colens to session
       do ii=1,tss%ncol
          col80(ii)=tss%col80(ii)
          lenc(ii)=tss%lenc(ii)
          exp250(ii)=tss%exp250(ii)
          lene(ii)=tss%lene(ii)
       end do
       tss%ncol=nfound+tss%ncol
       if (allocated(tss%col80)) deallocate(tss%col80)
       if (allocated(tss%lenc)) deallocate(tss%lenc)
       if (allocated(tss%exp250)) deallocate(tss%exp250)
       if (allocated(tss%lene)) deallocate(tss%lene)
       allocate(tss%col80(tss%ncol),&
            & tss%lenc(tss%ncol),&
            & tss%exp250(tss%ncol),&
            & tss%lene(tss%ncol),stat=irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250,"Unable to allocate 'currentFile%col'.")
          call table_errorappend(crc250,"\n")
          return
       end if
       do ii=1,tss%ncol
          tss%col80(ii)=col80(ii)
          tss%lenc(ii)=lenc(ii)
          tss%exp250(ii)=exp250(ii)
          tss%lene(ii)=lene(ii)
       end do
    end if
    return
  end subroutine table_makeColInd
  !
  subroutine table_prepareIndexes(tss,file,crc250,irc)
    type(table_session), pointer :: tss !  session
    type(table_file), pointer :: file   !  file
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_prepareIndexes"
    integer :: ii
    ! make inverted indexes for printing columns...
    if (allocated(file%invind)) deallocate(file%invind)
    allocate(file%invind(tss%ncol),stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'file%invind'.")
       call table_errorappend(crc250,"\n")
       return
    end if
    do ii=1,tss%ncol
       file%invind(ii)=0
    end do
    do ii=1,file%ncol
       file%invind(file%colind(ii))=ii
    end do
    return
  end subroutine table_prepareIndexes
  !
  subroutine table_openCurrentFile(tss,bok,crc250,irc)
    type(table_session), pointer :: tss
    logical :: bok           ! was get successful?
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    integer :: lenf
    integer, external :: ftunit,length
    character*250 :: fn250
    character*3 :: mode
    character*22 :: myname="table_openCurrentFile"
    integer :: ii
    tss%currentfile%unit=ftunit(irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from FTUNIT.")
       call table_errorappendi(crc250,irc)
       return
    end if
    open(unit=tss%currentfile%unit,file=tss%currentfile%fn250(1:tss%currentfile%lenf), &
         & access="sequential",form="formatted",status="old",iostat=irc)
    !if(table_bdeb)write(*,*)myname,'Opened file: ',tss%currentfile%fn250(1:tss%currentfile%lenf)
    if (irc.ne.0) then
       if(table_bdeb)write(*,*)myname,'Unable to open file.',irc
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,tss%currentfile%fn250(1:tss%currentfile%lenf));
       call table_errorappend(crc250,"\n")
       bok=.false.
       return
    else
       bok=.true.
    end if
    tss%currentfile%fopen=.true.
    tss%fopened=tss%fopened+1
    return
  end subroutine table_openCurrentFile
  !
  ! close file
  !
  subroutine table_closeCurrentFile(tss,crc250,irc)
    type(table_session), pointer :: tss
    character*250 :: crc250  ! error message string
    integer :: irc           ! error return code (0=ok)
    character*22 :: myname="table_closeCurrentFile"
    close(tss%currentfile%unit,iostat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"table_closeCurrentFile")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    tss%currentfile%fopen=.false.
    tss%currentfile%unit=0
    !
  end subroutine table_closeCurrentFile

  subroutine table_printFileStack(tss,crc250,irc) 
    type(table_session), pointer :: tss   ! session structure
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_printFileStack "
    type(table_file), pointer :: currentfile => null()
    integer :: lent,lenp,lend,lens,lene,ii
    integer, external :: lengthg
    ii=0
    if(table_bdeb)write(*,*)myname,' Files:',tss%nfileIndexes
    currentfile => tss%firstfile%next
    do while (.not.associated(currentfile,target=tss%lastfile))
       ii=ii+1
       write(*,*) myname,' Stack:',ii,' file="'//currentfile%fn250(1:currentFile%lenf)//'"'
       currentfile => currentfile%next
    end do
    write(*,*) myname,' Stack entries:',ii
    return
  end subroutine table_printFileStack

  subroutine table_jointablefile(tss,css,mss,oss,tab250,gra250,cat250,&
       & test,fill250,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(table_session), pointer :: tss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    integer :: cid,mid,oid
    character*250 :: tab250,gra250,cat250
    integer :: test
    character*250 :: fill250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_joinTableFile"
    type(table_file), pointer :: currentFile
    type(table_columns) :: cols
    integer :: lent,lenr,ounit
    character*1000 :: row1000
    logical :: bok ! is line ok?
    !
    if(table_bdeb)write(*,*)myname,'Entering.',irc
    ! make new table file header...
    ! open table file
    call table_openFile(tss,ounit,tab250,lent,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," unable to open "//tab250(1:lent))
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    call table_addTopComments(tss,css,mss,oss,ounit,tab250,lent,gra250,cat250,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from addTopComments.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    call table_addJoinComments(tss,css,mss,oss,ounit,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from addJoinComments.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    ! make table columns and limits...
    call table_makeColumns(tss,cols,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from makecolumns.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    ! loop over table files...
    currentFile=>tss%firstFile%next
    do while (.not.associated(currentFile,target=tss%lastFile))
       call table_prepareIndexes(tss,currentFile,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," unable to index "//tab250(1:lent))
          call table_errorappendi(crc250,irc)
          call table_errorappend(crc250,"\n")
          return
       end if
       ! read next line...
       if(table_bdeb)write(*,*)myname,'Looping:'//currentFile%fn250(1:currentFile%lenf)
       do while (table_readNextRow(tss,currentFile,cols,row1000,lenr,bok,crc250,irc))
          ! write line to new table file...
          if (bok) then
             write(ounit,'(A)',iostat=irc) row1000(1:lenr)
             if (irc.ne.0) then
                call table_errorappend(crc250,myname)
                call table_errorappend(crc250," unable to write to "//tab250(1:lent))
                call table_errorappendi(crc250,irc)
                call table_errorappend(crc250,"\n")
                return
             end if
          end if
       end do
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," unable to read "//currentFile%fn250(1:currentFile%lenf))
          call table_errorappendi(crc250,irc)
          call table_errorappend(crc250,"\n")
          return
       end if
       currentFile=>currentFile%next
    end do
    ! clean up table columns and limits...
    call table_removeColumns(tss,cols,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," Error return from removecolumns.")
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    ! close table output file unit, ounit
    call table_closeFile(tss,ounit,tab250,lent,crc250,irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250," unable to close "//tab250(1:lent))
       call table_errorappendi(crc250,irc)
       call table_errorappend(crc250,"\n")
       return
    end if
    !
    if(table_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine table_jointablefile
  !
  logical function table_readNextRow(tss,currentFile,cols,row1000,lenr,bok,crc250,irc)
    type(table_session), pointer :: tss !  current session
    type(table_columns) :: cols
    character*1000 :: row1000
    integer :: lenr
    logical :: bok
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_readNextRow"
    type(table_file),pointer :: currentFile
    logical :: last,bdone
    character*1000 :: buff1000
    integer :: lenb
    integer, external :: length
    ! open file
    bok=.true.
    last=.false.
    if (currentFile%unit .eq. 0) then
       if(table_bdeb)write(*,*)myname,'Opening:'//currentFile%fn250(1:currentFile%lenf)
       call table_openfile(tss,currentFile%unit,&
            & currentFile%fn250,currentFile%lenf,crc250,irc)
       if (irc.ne.0) then
          call table_errorappend(crc250,myname)
          call table_errorappend(crc250," unable to open "//currentFile%fn250(1:currentFile%lenf))
          call table_errorappendi(crc250,irc)
          call table_errorappend(crc250,"\n")
          return
       end if
       ! read comments
       bdone=.false.
       do while (.not. bdone)
          read(currentFile%unit,'(A)',iostat=irc) buff1000
          if (irc.ne.0) then
             irc=0
             bdone=.true.
             if(table_bdeb)write(*,*)myname,'Corrupt comments:'//currentFile%fn250(1:currentFile%lenf)
             last=.true.
          else if (buff1000(1:1).ne."#") then ! header row
             bdone=.true.
          end if
       end do
    end if
    ! read next line
    if (currentFile%unit.ne.0) then
       lenr=0
       read(currentFile%unit,'(A)',iostat=irc) buff1000
       !if(table_bdeb)write(*,*)myname,'Read:'//trim(row1000)
       if (irc.ne.0) then
          irc=0
          last=.true.
       end if
       if (last) then
          if(table_bdeb)write(*,*)myname,'Closing:'//currentFile%fn250(1:currentFile%lenf)
          call table_closefile(tss,currentFile%unit,&
               & currentFile%fn250,currentFile%lenf,crc250,irc)
          if (irc.ne.0) then
             call table_errorappend(crc250,myname)
             call table_errorappend(crc250," unable to close "//currentFile%fn250(1:currentFile%lenf))
             call table_errorappendi(crc250,irc)
             call table_errorappend(crc250,"\n")
             return
          end if
       else
          call table_washRow(tss,currentFile,buff1000,row1000,lenr,cols,bok,crc250,irc)
          if (irc.ne.0) then
             lenb=length(buff1000,100,100)
             call table_errorappend(crc250,myname)
             call table_errorappend(crc250," unable to washRow "//currentFile%fn250(1:currentFile%lenf))
             call table_errorappend(crc250," "//buff1000(1:lenb))
             call table_errorappendi(crc250,irc)
             call table_errorappend(crc250,"\n")
             return
          end if
       end if
    else
       last=.true.
88  end if
    table_readNextRow=(.not.last)
    return
  end function table_readNextRow
  !
  subroutine  table_washRow(tss,file,inp1000,out1000,lenn,cols,bok,crc250,irc)
    type(table_session), pointer :: tss !  current session
    type(table_file), pointer :: file
    character*1000 :: inp1000
    character*1000 :: out1000
    integer :: lenn
    type(table_columns) :: cols
    logical :: bok
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_washRow"
    integer :: ii,jj,ll,pp,leno
    logical bdone, inside
    integer :: istart(tss%ncol), istop(tss%ncol)
    ll=len_trim(inp1000)
    if(table_bdeb)write(*,*)myname,'Checking hdr:',inp1000(1:ll),ll
    ! sanity check
    if (tss%ncol.le.1) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Invalid session column count:")
       call table_errorappendi(crc250,tss%ncol)
       irc=833
       return
    end if
    ! line is ok so far...
    bok=.true.
    ! make array with input elements
    ii=1
    pp=0
    inside=.false.
    bdone=(ii.gt.ll).or.(tss%ncol.eq.0)
    do while (.not. bdone)
       if (inp1000(ii:ii).eq.' ') then
          if (inside) then
             istop(pp)=ii-1
             inside=.false.
          end if
       else
          if (.not.inside) then
             pp=min(tss%ncol,pp+1)
             inside=.true.
             istart(pp)=ii
          end if
       end if
       ii=ii+1
       bdone=(ii.gt.ll)
    end do
    if (inside) then
       istop(pp)=ll
    end if
    leno=0
    lenn=0
    ! make first column of output...
    do  ii=1,file%nleg
       if (file%nam80(ii)(1:file%lenn(ii)).eq.inp1000(istart(1):istop(1))) then
          lenn=tss%lenn(file%legind(ii))
          out1000(leno+1:lenn)=tss%nam80(file%legind(ii))(1:tss%lenn(file%legind(ii)))
          leno=lenn
       end if
    end do
    if (.not.table_checkColumn(inp1000,tss%ncol,1,istart,istop,1,cols)) then
       bok=.false.
       cols%crm(1)=cols%crm(1)+1
       return
    else
       cols%cok(1)=cols%cok(1)+1
    end if
    ! make remaining columns
    do ii=2,tss%ncol
       jj=file%invind(ii)
       if (.not.table_checkColumn(inp1000,tss%ncol,jj,istart,istop,ii,cols)) then
          bok=.false.
          cols%crm(ii)=cols%crm(ii)+1
          return
       else
          cols%cok(ii)=cols%cok(ii)+1
       end if
       if (jj.eq.0) then
          lenn=leno+3
          out1000(leno:lenn)=" NA"
          leno=lenn
       else
          lenn=leno+1+istop(jj)-istart(jj)+1
          if (lenn.le.1000) then
             out1000(leno+1:lenn)=" "//inp1000(istart(jj):istop(jj))
          else
             call table_errorappend(crc250,myname)
             call table_errorappend(crc250,"Invalid column:")
             call table_errorappend(crc250,inp1000(1:100))
             irc=834
             return
          end if
          leno=lenn
       end if
    end do
    return
  end subroutine table_washRow
  !
  logical function table_checkColumn(inp1000,ncol,jj,istart,istop,ii,cols)
    character*1000 :: inp1000
    integer :: ncol
    integer :: jj
    integer :: istart(ncol)
    integer :: istop(ncol)
    integer :: ii
    type(table_columns) :: cols
    real :: val
    logical bok
    integer :: irc2
    if (jj.eq.0) then
       bok=.false.
    else
       bok=.true.
    end if
    if (bok) then
       if (cols%setc(ii)) then
          read(inp1000(istart(jj):istop(jj)),*,iostat=irc2) val
          if (irc2.ne.0) then
             bok=.false.
          else
             if (val.lt.cols%minc(ii)) then
                bok=.false.
             else if (val.gt.cols%maxc(ii)) then
                bok=.false.
             end if
          end if
       end if
    end if
    if (bok) then
       if (cols%sets(ii)) then
          if (cols%lenmin(ii).ne.0) then
             if (inp1000(istart(jj):istop(jj)).ne.cols%min80(ii)(1:cols%lenmin(ii))) then
                bok=.false.
             end if
          else if (cols%lenmax(ii).ne.0) then
             if (inp1000(istart(jj):istop(jj)).ne.cols%max80(ii)(1:cols%lenmax(ii))) then
                bok=.false.
             end if
          end if
       end if
    end if
    table_checkColumn=bok
    return
  end function table_checkColumn
  !
  integer function table_findNot(cc,jj,str1000,ll)
    character*1 :: cc
    integer :: jj
    character*1000 :: str1000
    integer :: ll
    logical bdone
    table_findNot=jj
    bdone=(table_findNot.gt.ll)
    do while (.not.bdone)
       if (str1000(table_findNot:table_findNot).ne.cc) then
          bdone=.true.
       else
          table_findNot=table_findNot+1
          bdone=(table_findNot.gt.ll)
       end if
    end do
    return
  end function table_findNot
  !
  integer function table_findMatch(cc,jj,str1000,ll)
    character*1 :: cc
    integer :: jj
    character*1000 :: str1000
    integer :: ll
    logical bdone
    table_findMatch=jj
    bdone=(table_findMatch.gt.ll)
    do while (.not.bdone)
       if (str1000(table_findMatch:table_findMatch).eq.cc) then
          bdone=.true.
       else
          table_findMatch=table_findMatch+1
          bdone=(table_findMatch.gt.ll)
       end if
    end do
    return
  end function table_findMatch
  !
  subroutine table_makeColumns(tss,cols,crc250,irc)
    type(table_session), pointer :: tss !  current session
    type(table_columns) :: cols
    character*250 :: crc250
    integer :: irc
    type(table_column), pointer :: col => null()
    integer :: ii
    character*22 :: myname="table_makeColumns"
    cols%ncols=tss%ncols+1
    allocate(cols%col80(cols%ncols),cols%lenc(cols%ncols),&
         & cols%minc(cols%ncols),cols%maxc(cols%ncols), &
         & cols%setc(cols%ncols),&
         & cols%min80(cols%ncols),cols%max80(cols%ncols), &
         & cols%lenmin(cols%ncols),cols%lenmax(cols%ncols), &
         & cols%sets(cols%ncols), &
         & cols%cok(cols%ncols),cols%crm(cols%ncols),stat=irc)
    if (irc.ne.0) then
       call table_errorappend(crc250,myname)
       call table_errorappend(crc250,"Unable to allocate 'cols%'.")
       return
    end if
    do ii=1,cols%ncols
       cols%cok(ii)=0
       cols%crm(ii)=0
    end do
    ii=1
    cols%col80(ii)="set"
    cols%lenc(ii)=3
    cols%minc(ii)=0.0D0
    cols%maxc(ii)=0.0D0
    cols%setc(ii)=.false.
    cols%min80(ii)=""
    cols%max80(ii)=""
    cols%lenmin(ii)=0
    cols%lenmax(ii)=0
    cols%sets(ii)=.false.
    col=>tss%firstColumn%next
    do while (.not.associated(col,target=tss%lastColumn))
       ii=ii+1
       cols%col80(ii)=col%name80
       cols%lenc(ii)=col%lenn
       cols%minc(ii)=col%minc
       cols%maxc(ii)=col%maxc
       cols%setc(ii)=col%setc
       cols%min80(ii)=col%min80
       cols%max80(ii)=col%max80
       cols%lenmin(ii)=col%lenmin
       cols%lenmax(ii)=col%lenmax
       cols%sets(ii)=col%sets
       col=>col%next
    end do
    return
  end subroutine table_makeColumns
  subroutine table_removeColumns(tss,cols,crc250,irc)
    type(table_session), pointer :: tss !  current session
    type(table_columns) :: cols
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="table_removeColumns"
    integer :: ii
    real :: pst
    !
    WRITE(*,*)
    WRITE(*,998) MYNAME,                      'Total rows:', cols%cok(1)+cols%crm(cols%ncols)
    do ii=1,cols%ncols
       pst=dfloat(cols%crm(ii))/max(1.0d0,dfloat(cols%crm(ii)+cols%cok(ii)))*100
       if (cols%crm(ii).ne.0) write(*,996) myname,'Join-filter "'//cols%col80(ii)(1:cols%lenc(ii))//'":',&
            & -cols%crm(ii),pst
    end do
    WRITE(*,997) MYNAME,     '--------------------------------------------------'
    pst=dfloat(cols%cok(cols%ncols))/max(1.0d0,dfloat(cols%crm(1)+cols%cok(1)))*100
    WRITE(*,996) MYNAME,                      'Accepted rows:', cols%cok(cols%ncols),pst
    !
999 FORMAT(X,A12,X,A,I13,' (',F6.2,'%)')
998 FORMAT(X,A12,X,A30,I13)
997 FORMAT(X,A12,X,A)
996 FORMAT(X,A12,X,A30,I13,' (',F6.2,'%)')
    !
    if (allocated(cols%col80)) deallocate(cols%col80)
    if (allocated(cols%lenc)) deallocate(cols%lenc)
    if (allocated(cols%minc)) deallocate(cols%minc)
    if (allocated(cols%maxc)) deallocate(cols%maxc)
    if (allocated(cols%setc)) deallocate(cols%setc)
    if (allocated(cols%min80)) deallocate(cols%min80)
    if (allocated(cols%max80)) deallocate(cols%max80)
    if (allocated(cols%lenmin)) deallocate(cols%lenmin)
    if (allocated(cols%lenmax)) deallocate(cols%lenmax)
    if (allocated(cols%sets)) deallocate(cols%sets)
    if (allocated(cols%cok)) deallocate(cols%cok)
    if (allocated(cols%crm)) deallocate(cols%crm)
    cols%ncols=0
    return
  end subroutine table_removeColumns
  !
  !###############################################################################
  ! ERROR ROUTINES
  !###############################################################################
  !
  !
  subroutine table_errorappend(crc250,string)
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
    call chop0(crc250,250)
  end subroutine table_errorappend
  subroutine table_errorappendi(crc250,inum)
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
    call chop0(crc250,250)
  end subroutine table_errorappendi
  !
  subroutine findDelimiter(var80,del,pos)
    character*80 :: var80
    character*1 :: del
    integer :: pos
    logical :: bdone
    pos=min(80,pos+1)
    bdone=(pos.eq.80)
    do while (.not.bdone)
       if (var80(pos:pos).eq.del) then
          bdone=.true.
       else
          pos=min(80,pos+1)
          bdone=(pos.eq.80)
       end if
    end do
  end subroutine findDelimiter
  !
  subroutine table_strep(str250,nn,src100,rep100,lrep,irc)
    implicit none
    ! search for, and replace, key words in string with given information...
    character*250 str250
    integer nn
    character*100 src100(nn)
    character*100 rep100(nn)
    logical lrep(nn)
    integer irc
    !
    logical bdone
    character*250 buff250
    character*1000 buff1000
    integer ii,jj,length,lens,lenr,leni
    !
    character*16 myname
    data myname /'strep'/
    !
    do ii=1,nn
       call chop0(src100(ii),100)
       call chop0(rep100(ii),100)
       leni=length(str250,250,10)
       lens=length(src100(ii),100,2)
       lenr=length(rep100(ii),100,2)
       bdone=(leni.eq.0)
       do while (.not.bdone)
          buff250=str250
          bdone=.true.
          do jj=1,leni-lens+1
             !     write(*,*) myname,'"',str250(jj:jj+lens-1),'"',
             !     &              src100(ii)(1:lens),'"'
             if (str250(jj:jj+lens-1).eq.&
                  &              src100(ii)(1:lens)) then
                buff1000=str250(1:jj-1)//&
                     &                 rep100(ii)(1:lenr)//&
                     &                 str250(jj+lens:leni)
                call chop0(buff1000,251)
                str250=buff1000(1:250)
                leni=length(str250,250,10)
                !     write(*,*) myname,str250(1:leni)
                lrep(ii)=.true.
                bdone=(buff250.eq.str250)
             end if
          end do
       end do
    end do
    !     
    return
  end subroutine table_strep
  !
end module table
