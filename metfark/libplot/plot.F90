module plot
  IMPLICIT NONE
  !
  ! Global constants
  !
  logical     :: plot_bdeb=.false.
  !
  ! SESSION VARIABLES
  !
  type :: plot_attribute
     character*80 :: name80=""
     integer :: lenn=0
     character*250 :: value250=""
     integer :: lenv=0
     type(plot_attribute), pointer :: prev => null()         ! linked list
     type(plot_attribute), pointer :: next => null()         ! linked list
  end type plot_attribute
  !
  ! Target item
  !   
  type :: plot_obstrg
     character*80 :: trg80      ! target name
     character*250 :: pos250    ! position/sequence number
     character*80 :: descr80    ! descriptor
     character*250 :: info250   ! information
     character*80 :: min80      ! min value
     character*80 :: max80      ! max value
     type(plot_obstrg), pointer :: prev => null()   ! linked list
     type(plot_obstrg), pointer :: next => null()   ! linked list
  end type plot_obstrg
  !
  type :: plot_modtrg
     character*80 :: trg80      ! target name
     character*80 :: var80      ! variable name
     character*80 :: min80      ! min value
     character*80 :: max80      ! max value
     type(plot_modtrg), pointer :: prev => null()   ! linked list
     type(plot_modtrg), pointer :: next => null()   ! linked list
  end type plot_modtrg
  !
  ! default values for the model targets
  !
  type :: plot_default
     character*80 :: n80   ! name
     integer :: lenn       ! name length
     character*80 :: v80   ! value
     integer :: lenv       ! value length
     real :: val           ! value
     type(plot_default), pointer :: prev => null()   ! linked list
     type(plot_default), pointer :: next => null()   ! linked list
  end type plot_default
  !
  type :: plot_location
     type(plot_default), pointer  :: cDef => null()
     type(plot_default), pointer  :: firstDef => null()! linked list start
     type(plot_default), pointer  :: lastDef => null() ! linked list end
     type(plot_location), pointer :: prev => null()    ! linked list
     type(plot_location), pointer :: next => null()    ! linked list
  end type plot_location
  !
  type :: plot_match
     character*80 :: n80 ! name
     character*250 :: e250 ! obs expression
     character*80 :: l80 ! lower limit
     character*80 :: u80 ! upper limit
     type(plot_match), pointer :: prev => null()   ! linked list
     type(plot_match), pointer :: next => null()   ! linked list
  end type plot_match
  !
  type plot_column
     character*80  :: name80
     integer :: lenn=0
     character*250  :: exp250
     integer :: lene=0
     type(plot_column), pointer  :: prev => null() ! linked list start
     type(plot_column), pointer  :: next => null()  ! linked list end
  end type plot_column
  !
  type :: plot_set
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
     type(plot_obstrg), pointer :: firstObstrg => null()        ! linked list
     type(plot_obstrg), pointer :: lastObstrg => null()         ! linked list
     integer :: nObsTrg = 0
     ! mod data
     type(plot_modtrg), pointer :: firstModtrg => null()        ! linked list
     type(plot_modtrg), pointer :: lastModtrg => null()         ! linked list
     integer :: nModTrg = 0
     type(plot_obstrg), pointer :: cObstrg => null()            ! linked list
     type(plot_modtrg), pointer :: cModtrg => null()            ! linked list
     ! colocation data
     type(plot_location), pointer :: firstLoc => null()         ! linked list start
     type(plot_location), pointer :: lastLoc => null()          ! linked list end
     type(plot_location), pointer :: currentLoc => null()       ! current location
     type(plot_location), pointer :: cLoc => null()             ! current location
     integer :: ndef = 0                                        ! number of defaults
     type(plot_match), pointer :: firstMatch => null()          ! linked list
     type(plot_match), pointer :: lastMatch => null()           ! linked list
     type(plot_match), pointer :: cMatch => null()              ! linked list
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
     type(plot_set), pointer :: prev => null()         ! linked list
     type(plot_set), pointer :: next => null()         ! linked list
  end type plot_set
  !
  type :: plot_session
     integer :: sid=0            ! session id
     character*250 :: type250="" ! type
     integer :: lenp =0
     !
     ! output
     character*250 :: tab250=""  ! table file name
     integer :: lent             ! length of tab250
     character*250 :: gra250=""  ! graphics file name
     integer :: leng             ! length of gra250
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
     integer :: ncol=0    ! number of columns in linked list
     type(plot_column), pointer :: firstColumn => null()   ! linked list start
     type(plot_column), pointer :: lastColumn => null()    ! linked list end
     !
     integer :: natt =0
     type(plot_attribute), pointer :: firstAttribute => null()         ! linked list
     type(plot_attribute), pointer :: lastAttribute => null()         ! linked list
     integer :: nset =0
     type(plot_set), pointer :: firstSet => null()         ! linked list
     type(plot_set), pointer :: lastSet => null()          ! linked list
     type(plot_set), pointer :: currentSet => null()       ! linked list
     type(plot_session), pointer :: prev => null()         ! linked list
     type(plot_session), pointer :: next => null()         ! linked list
  end type plot_session
  !
  integer :: maxid=0 ! session counter
  type(plot_session), pointer :: firstSession => null()   ! linked list start
  type(plot_session), pointer :: lastSession => null()    ! linked list end
  !
CONTAINS
  !
  !###############################################################################
  ! SESSION ROUTINES
  !###############################################################################
  !
  subroutine plot_opensession(sid,pss,crc250,irc)
    integer :: sid
    character*250 :: crc250
    integer :: irc
    type(plot_session),pointer :: pss  !  new session
    character*25 :: myname="plot_openSession"
    if (.not.associated(firstSession)) then
       allocate(firstSession, lastSession,stat=irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250,"Unable to allocate 'firstSession/lastSession'.")
          call plot_errorappend(crc250,"\n")
          return
       end if
       firstSession%next => lastSession
       lastSession%prev => firstSession
    end if
    nullify(pss)
    allocate(pss,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'new session'.")
       call plot_errorappend(crc250,"\n")
       return
    end if
    maxid=maxid+1
    pss%sid=maxid
    pss%prev => lastSession%prev
    pss%next => lastSession
    pss%prev%next => pss
    pss%next%prev => pss
    allocate(pss%firstAttribute,pss%lastAttribute,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'attribute chain'.")
       call plot_errorappend(crc250,"\n")
       return
    end if
    pss%firstAttribute%next => pss%lastAttribute
    pss%lastAttribute%prev => pss%firstAttribute
    pss%natt=0
    allocate(pss%firstSet,pss%lastSet,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'attribute chain'.")
       call plot_errorappend(crc250,"\n")
       return
    end if
    pss%firstSet%next => pss%lastSet
    pss%lastSet%prev => pss%firstSet
    pss%nset=0
    !
    allocate(pss%firstColumn,pss%lastColumn, stat=irc) ! 
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate &
            & 'pss%firstColumn/pss%lastColumn'.")
       call plot_errorappend(crc250,"\n")
       return
    end if
    pss%firstColumn%next => pss%lastColumn
    pss%lastColumn%prev => pss%firstColumn
    !
    sid = pss%sid
    return
  end subroutine plot_opensession

  subroutine plot_getSession(pss,sid,crc250,irc)
    type(plot_session), pointer :: pss !  current session
    integer :: sid
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="plot_getSession"
    if (.not.associated(firstSession)) then
       irc=911
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"No session is opened!")
       call plot_errorappendi(crc250,irc)
       call plot_errorappend(crc250,"\n")
       return
    end if
    pss => firstSession%next
    do while ( .not.associated(pss,target=lastSession))
       if (pss%sid .eq. sid) then
          !if (plot_bdeb) write(*,*)myname,'Exiting with sid:',sid,irc
          return
       end if
       pss=>pss%next
    end do
    nullify(pss)
    irc=342
    call plot_errorappend(crc250,myname)
    call plot_errorappend(crc250,"Invalid session id:")
    call plot_errorappendi(crc250,sid)
    call plot_errorappend(crc250,"\n")
    return
  end subroutine plot_getSession

  subroutine plot_closeSession(pss,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="plot_closeSession"
    if(plot_bdeb)write(*,*)myname,'Entering.',irc
    if (associated(pss)  .and. .not.associated(pss,target=lastSession)) then
       call plot_removeSession(pss,crc250,irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from removeSession.")
          call plot_errorappendi(crc250,irc)
          call plot_errorappend(crc250,"\n")
          return
       end if
    else
       irc=599
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Attempt to close none-existent session.")
       call plot_errorappend(crc250,"\n")
       return
    end if
    if(plot_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine plot_closeSession

  subroutine plot_removeSession(pss,crc250,irc)
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="plot_removeSession"
    pss%prev%next => pss%next
    pss%next%prev => pss%prev
    call plot_clearAttrStack(pss,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearAttrStack.")
       call plot_errorappendi(crc250,irc)
       call plot_errorappend(crc250,"\n")
       return
    end if
    !call plot_clearSetStack(pss)
    deallocate(pss%firstAttribute,pss%lastAttribute,pss%firstSet,pss%lastSet)
    deallocate(pss)
  end subroutine plot_removeSession
  !
  !###############################################################################
  ! TYPE + ATTRIBUTE ROUTINES
  !###############################################################################
  !
  ! set the observation graphivcs type ("rms+stdv", "scatter" etc).
  subroutine plot_settype(pss,type250,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: type250
    character*250 :: crc250
    integer :: irc
    integer,external :: length
    character*22 :: myname="plot_settype"
    pss%type250=type250
    pss%lenp=length(type250,250,10)
    return
  end subroutine plot_settype
  !
  subroutine plot_clearattrstack(pss,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    integer :: irc2
    character*22 :: myname="plot_clearattrstack"
    type(plot_attribute), pointer :: cat,nat !  current attribute
    cat => pss%firstAttribute%next
    do while (.not.associated(cat,target=pss%lastAttribute))
       nat => cat%next
       call plot_unlinkAttribute(cat)
       call plot_deallocateAttribute(cat)
       pss%natt=pss%natt-1
       cat  => nat
    end do
    return
  end subroutine plot_clearattrstack
  !
  subroutine plot_unlinkAttribute(cat)
    implicit none
    type(plot_attribute), pointer :: cat !  current attribute
    cat%prev%next => cat%next
    cat%next%prev => cat%prev
    return
  end subroutine plot_unlinkAttribute
  !
  subroutine plot_deallocateAttribute(cat)
    implicit none
    type(plot_attribute), pointer :: cat !  current attribute
    integer :: irc2
    deallocate(cat,stat=irc2) ! ignore any errors
    return
  end subroutine plot_deallocateAttribute
  !
  subroutine plot_pushattr(pss,name80,val250,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*80 :: name80
    character*250 :: val250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_pushattr"
    integer :: lenn,lenv
    integer, external :: length
    type(plot_attribute), pointer :: cat !  current attribute
    lenn=length(name80,80,10)
    lenv=length(val250,250,10)
    if (lenn.ne.0) then
       allocate(cat,stat=irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250,"Unable to allocate 'attribute'.")
       end if
       cat%name80=name80
       cat%lenn=lenn
       cat%value250=val250
       cat%lenv=lenv
       cat%next => pss%lastAttribute
       cat%prev => pss%lastAttribute%prev
       cat%prev%next => cat
       cat%next%prev => cat
       pss%natt=pss%natt+1
       nullify(cat)
    else
       irc=344
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Attempt to push empty attribute/value.")
       return
    end if
    return
  end subroutine plot_pushattr
  !
  !###############################################################################
  ! OUTPUT FILE ROUTINE
  !###############################################################################
  !
  subroutine plot_setTablefile(pss,tab250,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: tab250 ! name of table file
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    pss%tab250=tab250
    CALL CHOP0(pss%tab250,250)
    PSS%LENT=LENGTH(pss%tab250,250,10)
    return
  end subroutine plot_setTablefile
  !
  subroutine plot_getTablefile(pss,tab250,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: tab250 ! name of table file
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    tab250=pss%tab250
    return
  end subroutine plot_getTablefile
  !
  subroutine plot_openfile(pss,ounit,tab250,lent,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    integer :: ounit
    character*250 :: tab250
    integer :: lent
    character*250 :: crc250
    integer :: irc
    integer, external :: ftunit,length
    character*22 :: myname="plot_openFile"
    call chop0(tab250,250)
    lent=length(tab250,250,10)
    ounit=ftunit(irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from FTUNIT.")
       call plot_errorappendi(crc250,irc)
       return
    end if
    write(*,'(X,A,X,A)')myname,"Table file: "//tab250(1:lent)
    open(unit=ounit,file=tab250(1:lent), &
         & access="sequential",form="formatted",status="unknown",iostat=irc)
    if(plot_bdeb)write(*,*)myname,'Opened:',tab250(1:lent),ounit,irc
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to open table file '"//tab250(1:lent)//"'")
       call plot_errorappendi(crc250,irc)
       return
    end if
    return
  end subroutine plot_openfile
  !
  subroutine plot_openfileapp(pss,ounit,tab250,lent,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    integer :: ounit
    character*250 :: tab250
    integer :: lent
    character*250 :: crc250
    integer :: irc
    integer, external :: ftunit,length
    character*22 :: myname="plot_openFileApp"
    call chop0(tab250,250)
    lent=length(tab250,250,10)
    ounit=ftunit(irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from FTUNIT.")
       call plot_errorappendi(crc250,irc)
       return
    end if
    open(unit=ounit,file=tab250(1:lent), &
         & access="append",form="formatted",status="old",iostat=irc)
    if(plot_bdeb)write(*,*)myname,'Appending:',tab250(1:lent),ounit,irc
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to open table file '"//tab250(1:lent)//"'")
       call plot_errorappendi(crc250,irc)
       return
    end if
    return
  end subroutine plot_openfileapp
  !
  subroutine plot_closeFile(pss,ounit,tab250,lent,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    integer :: ounit
    character*250 :: tab250
    integer :: lent
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_closeFile"
    close(unit=ounit,iostat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to close table file '"//tab250(1:lent)//"'")
       call plot_errorappendi(crc250,irc)
       return
    end if
    return
  end subroutine plot_closeFile
  !
  subroutine plot_addComments(pss,css,mss,oss,ounit,tab250,lent,gra250,cat250,crc250,irc)
    use model
    use observations
    use colocation
    use parse
    implicit none
    type(plot_session), pointer :: pss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    integer :: cid,mid,oid
    integer :: ounit
    character*250 :: tab250,gra250,cat250
    integer :: lent
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_makecomments"
    integer, external :: length
    integer :: lenc,lene,lenn,lenl,leng
    type(plot_attribute), pointer :: attr => null()
    character*250 :: leg250
    character*80 :: name80
    character*80, allocatable :: var80(:)
    integer :: ii,jj
    integer :: ncol
    character*80, allocatable :: col80(:)
    character*250, allocatable :: exp250(:)
    call chop0(cat250,250)
    lenc=length(cat250,250,10)
    call chop0(gra250,250)
    leng=length(gra250,250,10)
    write(ounit,'("# COMMAND: Rscript --vanilla ",A,X,A,X,A)',iostat=irc) &
         & cat250(1:lenc),tab250(1:lent),gra250(1:leng)
    !
    ! loop over set and write attributes and legend as comments
    !
    if(plot_bdeb)write(*,*)myname,'Writing attributes and legends.',ounit
    write(ounit,'("#")',iostat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error writing to table file'.")
       call plot_errorappend(crc250,pss%tab250(1:pss%lent))
       return
    end if
    write(ounit,'("# TYPE:",A)',iostat=irc)pss%type250(1:pss%lenp)
    write(ounit,'("#")',iostat=irc)
    ! print attributes
    write(ounit,'("# ATTRIBUTES:",I0)',iostat=irc)pss%natt
    attr => pss%firstAttribute%next
    do while (.not.associated(attr,target=pss%lastAttribute))
       write(ounit,'("# ",A,":",A)',iostat=irc)&
            & attr%name80(1:attr%lenn),attr%value250(1:attr%lenv)
       attr => attr%next
    end do
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error writing to Table file'.")
       call plot_errorappend(crc250,pss%tab250(1:pss%lent))
       return
    end if
    ! print legend table
    write(ounit,'("#")',iostat=irc)
    write(ounit,'("# LEGENDS:",I0)',iostat=irc) pss%nset
    do while (plot_loopset(pss,css,mss,oss,name80,ncol,col80,exp250,leg250,0,crc250,irc))
       call chop0(name80,80)
       lenn=length(name80,80,1)
       call chop0(leg250,250)
       lenl=length(leg250,250,1)
       if (plot_bdeb) write(*,*)myname,"Inside loopSet '"//name80(1:lenn)//"'"
       write(ounit,'("#",X,A,":",A)',iostat=irc)name80(1:lenn),leg250(1:lenl)
    end do
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from loopset'.")
       return
    end if
    ! print legend table
    write(ounit,'("#")',iostat=irc)
    write(ounit,'("# COLUMNS:",I0)',iostat=irc) ncol+1
    write(ounit,'("#",X,A,":",A)',iostat=irc) "set","id"
    do ii=1,ncol
       call chop0(col80(ii),80)
       lenc=length(col80(ii),80,1)
       call chop0(exp250(ii),80)
       lene=length(exp250(ii),80,1)
       write(ounit,'("#",X,A,":",A)',iostat=irc)col80(ii)(1:lenc),exp250(ii)(1:lene)
    end do
    if(plot_bdeb)write(*,*)myname,'Writing data.',ounit,ncol,size(col80),size(exp250)
    write(ounit,'("#")',iostat=irc)
    write(ounit,'("# Data table")',iostat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error writing to Table file'.")
       call plot_errorappend(crc250,pss%tab250(1:pss%lent))
       return
    end if
    if (ncol.eq.0) then
       write(ounit,'(X,A)',iostat=irc)"set"
    else
       write(ounit,'(X,A)',iostat=irc,advance="no")"set"
    end if
    do ii=1,ncol
       lenc=length(col80(ii),80,1)
       if (ii.eq.ncol) then
          write(ounit,'(X,A)',iostat=irc)col80(ii)(1:lenc)
       else
          write(ounit,'(X,A)',iostat=irc,advance="no")col80(ii)(1:lenc)
       end if
    end do
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error writing to Table file'.")
       call plot_errorappend(crc250,pss%tab250(1:pss%lent))
       return
    end if
    !
    if (allocated(var80)) deallocate(var80)
    if (allocated(col80)) deallocate(col80)
    if (allocated(exp250)) deallocate(exp250)
    !
  end subroutine plot_addComments
  !
  subroutine plot_setTime(pss,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_clearsetstack"
    call date_and_time(VALUES=pss%values)
    return
  end subroutine plot_setTime
  !
  subroutine plot_strepfiles(pss,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_strepfiles"
    integer,parameter  :: nn = 6
    character*100 :: src100(nn) = (/'YY','MM','DD','HH','MI','SS'/)
    character*100 :: rep100(nn)
    logical :: lrep(nn)
    call plot_settime(pss,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from settime.")
       call plot_errorappendi(crc250,irc)
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
    write(rep100(1),'(i4.4)')pss%values(1)
    write(rep100(2),'(i2.2)')pss%values(2)
    write(rep100(3),'(i2.2)')pss%values(3)
    write(rep100(4),'(i2.2)')pss%values(5)
    write(rep100(5),'(i2.2)')pss%values(6)
    write(rep100(6),'(i2.2)')pss%values(7)
    if (pss%lent.ne.0)call plot_strep(pss%tab250,nn,src100,rep100,lrep,irc)
    if (pss%leng.ne.0)call plot_strep(pss%gra250,nn,src100,rep100,lrep,irc)
    return
  end subroutine plot_strepfiles
  !
  subroutine plot_setFiles(pss,tab250,gra250,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: tab250
    character*250 :: gra250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_strepfiles"
    integer,parameter  :: nn = 6
    call plot_setTablefile(pss,tab250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from setTableFile.")
       return
    end if
    call plot_setGraphicsfile(pss,gra250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from setGraphicsFile.")
       return
    end if
    call plot_strepfiles(pss,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from strepFiles.")
       return
    end if
    call plot_getTablefile(pss,tab250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from setTableFile.")
       return
    end if
    call plot_getGraphicsfile(pss,gra250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from setGraphicsFile.")
       return
    end if
    return
  end subroutine plot_setFiles
  !
  subroutine plot_setGraphicsfile(pss,gra250,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: gra250 ! name of graphics file
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    pss%gra250=gra250
    CALL CHOP0(pss%gra250,250)
    PSS%LENG=LENGTH(pss%gra250,250,10)
    return
  end subroutine plot_setGraphicsfile
  !
  subroutine plot_getGraphicsfile(pss,gra250,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: gra250 ! name of graphics file
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    gra250=pss%gra250
    return
  end subroutine plot_getGraphicsfile
  !
  !###############################################################################
  ! SET ROUTINES
  !###############################################################################
  !
  subroutine plot_clearsetstack(pss,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_clearsetstack"
    type(plot_set), pointer :: cset,nset !  current set
    cset => pss%firstSet%next
    do while (.not.associated(cset,target=pss%lastSet))
       nset => cset%next
       call plot_unlinkSet(cset)
       call plot_deallocateSet(cset)
       pss%nset=pss%nset-1
       cset  => nset
    end do
    return
  end subroutine plot_clearsetstack
  !
  subroutine plot_unlinkSet(set)
    implicit none
    type(plot_set), pointer :: set !  current set
    set%prev%next => set%next
    set%next%prev => set%prev
    return
  end subroutine plot_unlinkSet
  !
  subroutine plot_allocateSet(set,crc250,irc)
    implicit none
    type(plot_set), pointer ::  set !  current set
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_allocateset"
    allocate(set,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'set'.")
       return
    end if
    allocate(set%firstObstrg,set%lastObstrg,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'set-obstrg'.")
       return
    end if
    set%firstObstrg%next => set%lastObstrg
    set%lastObstrg%prev => set%firstObstrg
    allocate(set%firstModtrg,set%lastModtrg,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'set-modtrg'.")
    end if
    set%firstModtrg%next => set%lastModtrg
    set%lastModtrg%prev => set%firstModtrg
    allocate(set%firstLoc,set%lastLoc,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'set-def'.")
    end if
    set%firstLoc%next => set%lastLoc
    set%lastLoc%prev => set%firstLoc
    allocate(set%firstMatch,set%lastMatch,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'set-match'.")
    end if
    set%firstMatch%next => set%lastMatch
    set%lastMatch%prev => set%firstMatch
    return
  end subroutine plot_allocateSet
  !
  subroutine plot_deallocateSet(set)
    implicit none
    type(plot_set), pointer :: set !  current set
    integer :: irc2
    character*250 :: crc250
    integer :: irc
    call plot_clearObstrgStack(set,crc250,irc)
    call plot_clearModtrgStack(set,crc250,irc)
    call plot_clearMatchStack(set,crc250,irc)
    deallocate(set,stat=irc2) ! ignore any errors
    return
  end subroutine plot_deallocateSet
  !
  subroutine plot_clearcolumn(pss,crc250,irc)
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    type(plot_column), pointer :: col, ncol
    character*22 :: myname="plot_clearcolumn"
    if(plot_bdeb)write(*,*)myname, 'Entering.',irc,&
         & associated(pss%firstColumn),associated(pss%lastColumn)
    col=>pss%firstColumn%next
    do while (.not.associated(col,target=pss%lastColumn))
       ncol=>col%next
       col%prev%next => col%next
       col%next%prev => col%prev
       deallocate(col)
       nullify(col)
       col=>ncol
    end do
    pss%ncol=0
    !pss%firstColumn%next=>pss%lastColumn
    !pss%lastColumn%prev=>pss%firstColumn
    !if(plot_bdeb)write(*,*)myname, 'Done.',irc
    return
  end subroutine plot_clearcolumn
  !
  subroutine plot_pushcolumn(pss,name80,exp250,crc250,irc)
    type(plot_session), pointer :: pss !  current session
    character*80 :: name80
    character*250 :: exp250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_pushcolumn"
    type(plot_column), pointer :: col
    integer, external :: length
    allocate(col,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'col'.")
       return
    end if
    col%name80=name80
    call chop0(col%name80,80)
    col%lenn=length(col%name80,80,10)
    col%exp250=exp250
    call chop0(col%exp250,250)
    col%lene=length(col%exp250,250,10)
    col%prev => pss%lastColumn%prev
    col%next => pss%lastColumn
    col%prev%next => col
    col%next%prev => col
    pss%ncol=pss%ncol+1
    return
  end subroutine plot_pushcolumn
  !
  subroutine plot_makeColumn(pss,set,crc250,irc) ! make set-column arrays
    type(plot_session), pointer :: pss !  current session
    type(plot_set), pointer     :: set !  current set
    character*250 :: crc250
    integer :: irc
    type(plot_column), pointer :: col
    integer :: ii
    character*22 :: myname="plot_makeColumn"
    if (allocated(set%col80)) deallocate(set%col80)
    if (allocated(set%col_lenn)) deallocate(set%col_lenn)
    if (allocated(set%col_exp250)) deallocate(set%col_exp250)
    if (allocated(set%col_lene)) deallocate(set%col_lene)
    if (pss%ncol > 0) then
       set%ccol=pss%ncol
       allocate(set%col80(set%ccol), &
            & set%col_lenn(set%ccol), &
            & set%col_exp250(set%ccol), &
            & set%col_lene(set%ccol), &
            & stat=irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250,"Unable to allocate 'set%col'.")
          return
       end if
       ii=0
       col=>pss%firstColumn%next
       do while (.not.associated(col,target=pss%lastColumn))
          ii=ii+1
          set%col80(ii)=col%name80
          set%col_lenn(ii)=col%lenn
          set%col_exp250(ii)=col%exp250
          set%col_lene(ii)=col%lene
          col=>col%next
       end do
    end if
    return
  end subroutine plot_makeColumn
  !
  subroutine plot_pushset(pss,css,mss,oss,name80,leg250,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(plot_session), pointer :: pss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    character*80 :: name80
    character*250 leg250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_pushset"
    type(plot_set), pointer :: set !  current set
    if(plot_bdeb)write(*,*)myname,'Entering.',irc
    call plot_allocateSet(set,crc250,irc)
    set%name80=name80
    set%leg250=leg250
    if(plot_bdeb)write(*,*)myname,'Importing obs.',irc
    call plot_obsImport(set,oss,crc250,irc) ! get obs data 
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from obsImport.")
       return
    end if
    if(plot_bdeb)write(*,*)myname,'Importing model.',irc
    call plot_modImport(set,mss,crc250,irc) ! get model data
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from modImport.")
       return
    end if
    if(plot_bdeb)write(*,*)myname,'Importing colocation.',irc
    call plot_colImport(set,css,crc250,irc) ! get colocation data
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from colImport.")
       return
    end if
    call plot_makeColumn(pss,set,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from makeColumn.")
       return
    end if
    set%next => pss%lastSet
    set%prev => pss%lastSet%prev
    set%prev%next => set
    set%next%prev => set
    pss%nset=pss%nset+1
    nullify(set)
    !if(plot_bdeb)write(*,*)myname,'Exiting.',irc
    return
  end subroutine plot_pushset
  !
  ! loop over sets from top and delete them
  logical function plot_pullset(pss,css,mss,oss,name80,leg250,level,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(plot_session), pointer :: pss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    character*80 :: name80
    character*250 :: leg250
    integer :: level
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_pullset"
    type(plot_set), pointer :: set !  current set
    plot_pullset=.false. ! only true if all is ok
    set => pss%firstSet%next
    if (.not.associated(set,target=pss%lastSet)) then
       name80=set%name80
       leg250=set%leg250
       call plot_obsExport(set,oss,crc250,irc) ! set obs data
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from obsExport.")
          return
       end if
       call plot_modExport(set,mss,crc250,irc) ! set model data
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from modExport.")
          return
       end if
       call plot_colExport(set,css,crc250,irc) ! set colocation data
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from colExport.")
          return
       end if
       if(plot_bdeb)write(*,*)myname,"Cleaning.",irc
       call plot_unlinkSet(set)       
       call plot_deallocateSet(set)
       plot_pullset=.true.
    end if
    if(plot_bdeb)write(*,*)myname,"Done.",irc
    return
  end function plot_pullset
  !
  ! loop over sets from bottom and delete them
  logical function plot_popset(pss,css,mss,oss,name80,leg250,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(plot_session), pointer :: pss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    character*80 :: name80
    character*250 :: leg250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_popset"
    type(plot_set), pointer :: set !  current set
    plot_popset=.false. ! only true if all is ok and irc==0
    set => pss%lastSet%prev
    if (.not.associated(set,target=pss%firstSet)) then
       name80=set%name80
       leg250=set%leg250
       call plot_obsExport(set,oss,crc250,irc) ! set obs data
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from obsExport.")
          return
       end if
       call plot_modExport(set,mss,crc250,irc) ! set model data
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from modExport.")
          return
       end if
       call plot_colExport(set,css,crc250,irc) ! set colocation data
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from colExport.")
          return
       end if
       if(plot_bdeb)write(*,*)myname,"Cleaning.",irc
       call plot_unlinkSet(set)       
       call plot_deallocateSet(set)
       plot_popset=.true.
    end if
    if(plot_bdeb)write(*,*)myname,"Done.",irc
    return
  end function plot_popset
  !
  ! check that sets are comparable (TRUE=ok)
  logical function plot_checkSets(pss)
    implicit none
    type(plot_session), pointer :: pss !  current session
    type(plot_set),pointer :: cSet
    logical ::first
    first = .true.
    plot_checkSets=.false.
    if (.not.associated(pss)) return
    cSet => pss%firstSet%next
    do while (.not.associated(cSet,target=pss%lastSet))
       if (first) then
          first=.false.
       else if (.not.plot_checkObsTrg(pss%firstSet%next,cSet)) then
          if(plot_bdeb)write(*,*)'Obs targets differ.'
          return
       else if (pss%firstSet%next%category.ne.cSet%category) then
          if(plot_bdeb)write(*,*)'BUFR categories differ:',&
               & pss%firstSet%next%category,cSet%category
          return
       else if (pss%firstSet%next%subcategory.ne.cSet%subcategory) then
          if(plot_bdeb)write(*,*)'BUFR sub-categories differ:',&
               & pss%firstSet%next%subcategory,cSet%subcategory
          return
       end if
       cSet => cSet%next
    end do
    plot_checkSets=.true.
    return
  end function plot_checkSets
  !
  ! compare observation targets of the two sets (TRUE=ok)
  logical function plot_checkObsTrg(set1,set2)
    implicit none
    type(plot_set),pointer :: set1,set2
    type(plot_obstrg),pointer :: t1,t2
    integer :: ii
    plot_checkObsTrg=.false.
    t1 => set1%firstObsTrg%next
    t2 => set1%firstObsTrg%next
    ii=0
    do while (.not.associated(t1,set1%lastObsTrg))
       ii=ii+1
       if (t1%pos250.ne.t2%pos250) then
          if(plot_bdeb)write(*,*)'Positions differ.',ii
          return
       else if (t1%descr80.ne.t2%descr80) then
          if(plot_bdeb)write(*,*)'Descriptors differ.',ii
          return
       end if
       t1=>t1%next
       t2=>t2%next
    end do
    plot_checkObsTrg=.true.
    return
  end function plot_checkObsTrg
  !
  ! loop over sets from the top without deleting them...
  logical function plot_loopSet(pss,css,mss,oss,name80,ncol,col80,exp250,leg250,level,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(plot_session), pointer :: pss !  current session
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
    character*22 :: myname="plot_loopSet"
    plot_loopset=.false. ! only true if all is ok
    if (.not.associated(pss%currentSet)) then
       pss%currentSet =>  pss%firstSet%next 
    else
       pss%currentSet =>  pss%currentSet%next
    end if
    if (associated(pss%currentSet,target=pss%lastSet)) then
       nullify(pss%currentSet)
       plot_loopset=.false.
    else
       name80=pss%currentSet%name80
       leg250=pss%currentSet%leg250
       if (level.ne.0) then
          if(plot_bdeb)then
             lenn=length(pss%currentSet%name80,80,10)
             write(*,*)myname,"Set '"//pss%currentSet%name80(1:lenn)//"'"
          end if
          call plot_obsExport(pss%currentSet,oss,crc250,irc) ! set obs data
          if (irc.ne.0) then
             call plot_errorappend(crc250,myname)
             call plot_errorappend(crc250," Error return from obsExport.")
             return
          end if
          call plot_modExport(pss%currentSet,mss,crc250,irc) ! set model data
          if (irc.ne.0) then
             call plot_errorappend(crc250,myname)
             call plot_errorappend(crc250," Error return from modExport.")
             return
          end if
          call plot_colExport(pss%currentSet,css,crc250,irc) ! set colocation data
          if (irc.ne.0) then
             call plot_errorappend(crc250,myname)
             call plot_errorappend(crc250," Error return from colExport.")
             return
          end if
          !
       end if
       if(plot_bdeb)write(*,*)myname,"Make columns.",pss%currentSet%ccol,allocated(col80)
       ncol=pss%currentSet%ccol
       if (.not.allocated(col80).or..not.allocated(exp250).or.&
            & pss%currentSet%ccol > size(col80).or.pss%currentSet%ccol > size(exp250)) then
          if (allocated(col80)) deallocate(col80)
          if (allocated(exp250)) deallocate(exp250)
          if(plot_bdeb)write(*,*)myname,"Allocating columns.",ncol
          allocate(col80(ncol),exp250(ncol),stat=irc)
          if (irc.ne.0) then
             call plot_errorappend(crc250,myname)
             call plot_errorappend(crc250," Unable to allocate columns.")
             return
          end if
       end if
       if(plot_bdeb)write(*,*)myname,"Assigning columns.",ncol,&
            & allocated(col80),allocated(exp250),size(col80),size(exp250)
       do ii=1,ncol
          col80(ii)=pss%currentSet%col80(ii)
          exp250(ii)=pss%currentSet%col_exp250(ii)
       end do
       if (plot_bdeb)write(*,*)myname,'Columns:',ncol,size(col80),size(exp250)
       !
       plot_loopset=.true.
    end if
    if(plot_bdeb)write(*,*)myname,"Done.",plot_loopset
    return
  end function plot_loopSet
  !
  !###############################################################################
  ! SUB DATA ROUTINES (OBS-, MODEL-, MATCH-TARGET)
  !###############################################################################
  !
  !
  subroutine plot_clearobstrgstack(set,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_clearobstrgstack"
    type(plot_obstrg), pointer :: ctrg,ntrg !  current target
    ctrg => set%firstObstrg%next
    do while (.not.associated(ctrg,target=set%lastObstrg))
       ntrg => ctrg%next
       call plot_unlinkObstrg(ctrg)
       set%nObsTrg=set%nObsTrg-1
       call plot_deallocateObstrg(ctrg)
       ctrg  => ntrg
    end do
    nullify(set%cobstrg)
    return
  end subroutine plot_clearobstrgstack
  !
  subroutine plot_clearmodtrgstack(set,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_clearmodtrgstack"
    type(plot_modtrg), pointer :: ctrg,ntrg !  current target
    ctrg => set%firstModtrg%next
    do while (.not.associated(ctrg,target=set%lastModtrg))
       ntrg => ctrg%next
       call plot_unlinkModtrg(ctrg)
       set%nModTrg=set%nModTrg-1
       call plot_deallocateModtrg(ctrg)
       ctrg  => ntrg
    end do
    nullify(set%cmodtrg)
    return
  end subroutine plot_clearmodtrgstack
  !
  subroutine plot_clearmatchstack(set,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_clearmatchstack"
    type(plot_match), pointer :: cmatch,nmatch !  current target
    cmatch => set%firstMatch%next
    do while (.not.associated(cmatch,target=set%lastMatch))
       nmatch => cmatch%next
       call plot_unlinkMatch(cmatch)
       call plot_deallocateMatch(cmatch)
       set%nmatch=set%nmatch-1
       cmatch  => nmatch
    end do
    nullify(set%cmatch)
    return
  end subroutine plot_clearmatchstack
  !
  ! clear the default stack
  !
  subroutine plot_cleardefaultStack(set,crc250,irc) 
    type(plot_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    type(plot_location), pointer :: cLoc !  the current default target
    type(plot_location), pointer :: nLoc !  the next default target
    character*25 :: myname="plot_cleardefaultStack"
    if (plot_bdeb) write(*,*)myname,'Entering.',associated(set), associated(set%firstLoc)
    cLoc => set%firstLoc%next
    do while (.not.associated(cLoc,target=set%lastLoc))
       nLoc => cLoc%next
       cLoc%prev%next =>  cLoc%next
       cLoc%next%prev =>  cLoc%prev
       deallocate(cLoc,stat=irc)
       cLoc => nLoc
    end do
    set%ndef=0
    if (plot_bdeb) write(*,*)myname,'Exiting.',irc
    return
  end subroutine plot_cleardefaultStack
  !
  subroutine plot_unlinkObstrg(trg)
    implicit none
    type(plot_obstrg), pointer :: trg !  current target
    trg%prev%next => trg%next
    trg%next%prev => trg%prev
    return
  end subroutine plot_unlinkObstrg
  !
  subroutine plot_unlinkModtrg(trg)
    implicit none
    type(plot_modtrg), pointer :: trg !  current target
    trg%prev%next => trg%next
    trg%next%prev => trg%prev
    return
  end subroutine plot_unlinkModtrg
  !
  subroutine plot_unlinkMatch(match)
    implicit none
    type(plot_match), pointer :: match !  current target
    match%prev%next => match%next
    match%next%prev => match%prev
    return
  end subroutine plot_unlinkMatch
  !
  subroutine plot_unlinkDefault(def)
    implicit none
    type(plot_default), pointer :: def !  current target
    def%prev%next => def%next
    def%next%prev => def%prev
    return
  end subroutine plot_unlinkDefault
  !
  subroutine plot_pushobstrg(set,trg80,pos250,descr80,info250,min80,max80,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*80 :: trg80      ! target name
    character*250 :: pos250    ! position/sequence number
    character*80 :: descr80    ! descriptor
    character*250 :: info250   ! information
    character*80 :: min80      ! min value
    character*80 :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_pushobstrg"
    type(plot_obstrg), pointer :: trg !  current target
    allocate(trg,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'obstrg'.")
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
  end subroutine plot_pushobstrg
  !
  subroutine plot_pushmodtrg(set,trg80,var80,min80,max80,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*80 :: trg80      ! target name
    character*80 :: var80    ! descriptor
    character*80 :: min80      ! min value
    character*80 :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_pushmodtrg"
    type(plot_modtrg), pointer :: trg !  current target
    allocate(trg,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'modtrg'.")
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
  end subroutine plot_pushmodtrg
  !
  integer function plot_countModTrg(set)
    type(plot_set),pointer :: set
    plot_countModTrg=set%nModTrg
    return
  end function plot_countModTrg
  !
  integer function plot_countObsTrg(set)
    type(plot_set),pointer :: set
    plot_countObsTrg=set%nObsTrg
    return
  end function plot_countObsTrg
  !
  subroutine plot_pushmatch(set,n80,e250,l80,u80,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*80 :: n80      ! target name
    character*250 :: e250    ! position/sequence number
    character*80 :: l80      ! lower
    character*80 :: u80      ! upper
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_pushmatch"
    integer :: lenn
    integer, external :: length
    type(plot_match), pointer :: trg !  current target
    allocate(trg,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'match'.")
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
    if (plot_bdeb)then
       lenn=length(n80,80,10)
       write(*,*)myname,' Adding:',n80(1:lenn)
    end if
    nullify(trg)
    return
  end subroutine plot_pushmatch
  !
  ! add default element
  !
  subroutine plot_addDefault(set,n80,v80,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*80 :: n80 ! target name
    character*80 :: v80 ! target value
    character*250 :: crc250
    integer :: irc
    integer :: ii, irc2, lenv, lenn
    type(plot_default),pointer :: newdef
    integer, external :: length
    character*25 :: myname="colocation_addDefault"
    if(plot_bdeb)write(*,*)myname,'Entering.',associated(set%currentLoc),irc
    allocate(newdef,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'newdef'.")
       call plot_errorappend(crc250,"\n")
       return
    end if
    newdef%n80=n80
    call chop0(newdef%n80,80)
    newdef%lenn=length(newdef%n80,80,10)
    newdef%v80=v80
    call chop0(newdef%v80,80)
    newdef%lenv=length(newdef%v80,80,10)
    if(plot_bdeb)write(*,*)myname," Assigning: '"//newdef%n80(1:newdef%lenn)//&
         & "' -> '"//newdef%v80(1:newdef%lenv)//"'"
    if (.not.associated(set%currentLoc)) then
       if(plot_bdeb)write(*,*)myname,'New Default.'
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
    if(plot_bdeb)write(*,*)myname,'Adding default.'
    newdef%next => set%currentLoc%lastDef
    newdef%prev => set%currentLoc%lastDef%prev
    set%currentLoc%lastDef%prev%next => newdef
    set%currentLoc%lastDef%prev => newdef
    if(plot_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine plot_addDefault
  !
  subroutine plot_maketargetlist(set,crc250,irc)
    type(plot_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="plot_maketargetlist"
    type(plot_modtrg), pointer :: cTrg
    integer ii,lens,irc2
    integer, external :: length
    if(plot_bdeb)write(*,*)myname,'Entering.',irc
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
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250,"Unable to allocate 'session:trg...'.")
          call plot_errorappend(crc250,"\n")
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
    if(plot_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine plot_maketargetlist
  !
  ! push default values to the stack
  !
  subroutine plot_pushDefault(set,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    type(plot_location), pointer :: newLoc
    character*25 :: myname="plot_pushDefault"
    if(plot_bdeb)write(*,*)myname,'Entering.',irc
    if (.not.associated(set %firstLoc)) then
       allocate(set%firstLoc,set%lastLoc, stat=irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250,"Unable to allocate 'firstDef/lastDef'.")
          call plot_errorappend(crc250,"\n")
          return
       end if
       set%firstLoc%next => set%lastLoc
       set%lastLoc%prev => set%firstLoc
       set%ndef=0
    end if
    if (associated(set%currentLoc)) then
       nullify(set%currentLoc)
    end if
    if(plot_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine plot_pushDefault
  !
  logical function plot_popobstrg(set,trg80,pos250,descr80,info250,min80,max80,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*80  :: trg80      ! target name
    character*250 :: pos250    ! position/sequence number
    character*80  :: descr80    ! descriptor
    character*250 :: info250   ! information
    character*80  :: min80      ! min value
    character*80  :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_popobstrg"
    type(plot_obstrg), pointer :: trg,ntrg !  current trg
    plot_popobstrg=.false. ! only true if all is ok
    trg => set%lastObstrg%prev
    if (.not.associated(trg,set%firstObstrg)) then
       ntrg=>trg%next
       trg80=trg%trg80
       pos250=trg%pos250
       descr80=trg%descr80
       info250=trg%info250
       min80=trg%min80
       max80=trg%max80
       call plot_unlinkObstrg(trg)
       set%nObsTrg=set%nObsTrg-1
       trg=>ntrg
       plot_popobstrg=.true.
    end if
    return
  end function plot_popobstrg
  !
  logical function plot_popmodtrg(set,trg80,var80,min80,max80,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*80  :: trg80      ! target name
    character*80  :: var80    ! descriptor
    character*80  :: min80      ! min value
    character*80  :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_popmodtrg"
    type(plot_modtrg), pointer :: trg,ntrg !  current trg
    plot_popmodtrg=.false. ! only true if all is ok
    trg => set%lastModtrg%prev
    if (.not.associated(trg,set%firstModtrg)) then
       ntrg=>trg%next
       trg80=trg%trg80
       var80=trg%var80
       min80=trg%min80
       max80=trg%max80
       call plot_unlinkModtrg(trg)
       set%nModTrg=set%nModTrg-1
       trg=>ntrg
       plot_popmodtrg=.true.
    end if
    return
  end function plot_popmodtrg
  !
  logical function plot_popmatch(set,n80,e250,l80,u80,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*80  :: n80      ! target name
    character*250 :: e250    ! observation expression
    character*80  :: l80      ! lower
    character*80  :: u80      ! upper
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_popmatch"
    type(plot_match), pointer :: trg,ntrg !  current trg
    plot_popmatch=.false. ! only true if all is ok
    trg => set%lastMatch%prev
    if (.not.associated(trg,set%firstMatch)) then
       ntrg=>trg%next
       n80=trg%n80
       e250=trg%e250
       l80=trg%l80
       u80=trg%u80
      call plot_unlinkMatch(trg)
       trg=>ntrg
       plot_popmatch=.true.
       set%nmatch=set%nmatch-1
    end if
    return
  end function plot_popmatch
  !
  logical function plot_loopobstrg(set,trg80,pos250,descr80,info250,min80,max80,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*80  :: trg80      ! target name
    character*250 :: pos250    ! position/sequence number
    character*80  :: descr80    ! descriptor
    character*250 :: info250   ! information
    character*80  :: min80      ! min value
    character*80  :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_loopobstrg"
    plot_loopobstrg=.false. ! only true if all is ok
    if (.not.associated(set%cobstrg)) then
       set%cobstrg =>  set%firstObstrg%next 
    else
       set%cobstrg =>  set%cobstrg%next
    end if
    if (associated(set%cobstrg,target=set%lastObstrg)) then
       nullify(set%cobstrg)
       plot_loopobstrg=.false.
    else
       trg80=set%cobstrg%trg80
       pos250=set%cobstrg%pos250
       descr80=set%cobstrg%descr80
       info250=set%cobstrg%info250
       min80=set%cobstrg%min80
       max80=set%cobstrg%max80
       plot_loopobstrg=.true.
    end if
    return
  end function plot_loopobstrg
  !
  logical function plot_loopmodtrg(set,trg80,var80,min80,max80,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*80  :: trg80      ! target name
    character*80  :: var80    ! descriptor
    character*80  :: min80      ! min value
    character*80  :: max80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_loopmodtrg"
    !if (plot_bdeb) write(*,*)myname,' Entering.',irc
    plot_loopmodtrg=.false. ! only true if all is ok
    if (.not.associated(set%cmodtrg)) then
       set%cmodtrg =>  set%firstModtrg%next 
    else
       set%cmodtrg =>  set%cmodtrg%next
    end if
    if (associated(set%cmodtrg,target=set%lastModtrg)) then
       nullify(set%cmodtrg)
       plot_loopmodtrg=.false.
    else
       trg80=set%cmodtrg%trg80
       var80=set%cmodtrg%var80
       min80=set%cmodtrg%min80
       max80=set%cmodtrg%max80
       plot_loopmodtrg=.true.
    end if
    !if (plot_bdeb) write(*,*)myname,' Done.',plot_loopmodtrg
    return
  end function plot_loopmodtrg
  !
  logical function plot_loopLocation(set,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_loopLocation"
    if (plot_bdeb) write(*,*)myname,'Entering.',irc
    plot_loopLocation=.false. ! only true if all is ok
    if (.not.associated(set%cLoc)) then
       set%cLoc =>  set%firstLoc%next 
    else
       set%cLoc =>  set%cLoc%next
    end if
    if (associated(set%cLoc,target=set%lastLoc)) then
       nullify(set%cLoc)
       plot_loopLocation=.false.
    else
       plot_loopLocation=.true.
    end if
    return
  end function plot_loopLocation
  !
  logical function plot_loopDefault(set,n80,v80,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*80  :: n80      ! target name
    character*80  :: v80      ! variable
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_loopdefitem"
    if (plot_bdeb) write(*,*)myname,'Entering.',irc
    plot_loopDefault=.false. ! only true if all is ok
    if (.not.associated(set%cLoc%cDef)) then
       set%cLoc%cDef =>  set%cLoc%firstDef%next 
    else
       set%cLoc%cDef =>  set%cLoc%cDef%next
    end if
    if (associated(set%cLoc%cDef,target=set%cLoc%lastDef)) then
       nullify(set%cLoc%cDef)
       plot_loopDefault=.false.
    else
       plot_loopDefault=.true.
       n80=set%cLoc%cDef%n80
       v80=set%cLoc%cDef%v80
    end if
    return
  end function plot_loopDefault
  !
  logical function plot_loopmatch(set,n80,e250,l80,u80,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*80  :: n80      ! target name
    character*250 :: e250   ! information
    character*80  :: l80      ! min value
    character*80  :: u80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_loopmatch"
    plot_loopmatch=.false. ! only true if all is ok
    if (.not.associated(set%cmatch)) then
       set%cmatch =>  set%firstMatch%next 
    else
       set%cmatch =>  set%cmatch%next
    end if
    if (associated(set%cmatch,target=set%lastMatch)) then
       nullify(set%cmatch)
       plot_loopmatch=.false.
    else
       n80=set%cmatch%n80
       e250=set%cmatch%e250
       l80=set%cmatch%l80
       u80=set%cmatch%u80
       plot_loopmatch=.true.
    end if
    return
  end function plot_loopmatch
  !
  subroutine plot_deallocateObstrg(trg)
    implicit none
    type(plot_obstrg), pointer :: trg !  current target
    integer :: irc2
    deallocate(trg,stat=irc2) ! ignore any errors
    return
  end subroutine plot_deallocateObstrg
  !
  subroutine plot_deallocateModtrg(trg)
    implicit none
    type(plot_modtrg), pointer :: trg !  current target
    integer :: irc2
    deallocate(trg,stat=irc2) ! ignore any errors
    return
  end subroutine plot_deallocateModtrg
  !
  subroutine plot_deallocateMatch(match)
    implicit none
    type(plot_match), pointer :: match !  current target
    integer :: irc2
    deallocate(match,stat=irc2) ! ignore any errors
    return
  end subroutine plot_deallocateMatch
  !
  subroutine plot_obsImport(set,oss,crc250,irc) ! get obs data
    use observations
    implicit none
    type(plot_set), pointer :: set
    type(obs_session), pointer :: oss
    character*250 :: crc250
    integer :: irc
    character*80 :: trg80,descr80,min80,max80
    character*250 :: pos250,info250
    character*22 :: myname="plot_obsImport"
    call observation_getTablePath(oss,set%tablepath,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from getTablePath.")
       return
    end if
    call observation_getBufrType(oss,set%category,set%subCategory,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from getBufrType.")
       return
    end if
    call observation_getIndex(oss,set%ind_obs80,set%ind_exp250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from getIndex.")
       return
    end if
    call observation_getIndexLimitsRaw(oss,set%ind,set%ind_start,set%ind_stop)
    ! cache file is stored in colocation-module
    call plot_clearObstrgStack(set,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearObstrgStack.")
       return
    end if
    do while (observation_loopTarget(oss,trg80,pos250,descr80,info250,min80,max80,crc250,irc))
       call plot_pushobstrg(set,trg80,pos250,descr80,info250,min80,max80,crc250,irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from pushobstrg.")
          return
       end if
    end do
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from loopTarget.")
       return
    end if
    call observation_getfilter(oss,set%fltobs250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from getobsfilter.")
       return
    end if
    return
  end subroutine plot_obsImport
  !
  subroutine plot_obsExport(set,oss,crc250,irc) ! set obs data
    use observations
    implicit none
    type(plot_set), pointer :: set
    type(obs_session), pointer :: oss
    integer :: cnt
    logical :: bex
    character*250 :: crc250
    integer :: irc
    character*80 :: trg80,descr80,min80,max80
    character*250 :: pos250,info250
    integer :: lenn
    integer, external :: length
    character*22 :: myname="plot_obsExport"
    if (plot_countObsTrg(set).eq.0) return ! no targets = nothing to do
    call observation_setTablePath(oss,set%tablepath,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from setTablePath.")
       return
    end if
    call observation_setBufrType(oss,set%category,set%subCategory,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from setBufrType.")
       return
    end if
    call observation_setIndex(oss,set%ind_obs80,set%ind_exp250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from setIndex.")
       return
    end if
    call observation_setIndexLimitsRaw(oss,set%ind,set%ind_start,set%ind_stop)
    call observation_loadCache(oss,set%obs250,crc250,irc) ! stored in colocation module
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from loadCache.")
       return
    end if
    call observation_clearTargetStack(oss,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearTargetStack.")
       return
    end if
    do while (plot_loopObstrg(set,trg80,pos250,descr80,info250,min80,max80,crc250,irc))
       if (plot_bdeb) then
          lenn=length(trg80,80,10)
          write(*,*)myname,"Inside loopObsTrg '"//trg80(1:lenn)//"'"
       end if
       call observation_pushtarget(oss,trg80,pos250,descr80,info250,min80,max80,crc250,irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from pushtarget.")
          return
       end if
    end do
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from loopObstrg.")
       return
    end if
    call observation_makeTargetList(oss,crc250,irc) ! make target list from target chain
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from makeTargetList.")
       return
    end if
    cnt = observation_targetCount(oss,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from targetCount.")
       return
    end if
    bex= observation_hasValidIndex(oss,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from hasValidIndex.")
       return
    end if
    call observation_setfilter(oss,set%fltobs250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from setfilter.")
       return
    end if
    return
  end subroutine plot_obsExport
  !
  subroutine plot_modImport(set,mss,crc250,irc) ! get model data
    use model
    implicit none
    type(plot_set), pointer :: set
    type(mod_session), pointer :: mss
    character*250 :: crc250
    integer :: irc
    character*80  :: n80       ! target name
    character*80  :: v80       ! variable
    character*80  :: l80      ! min value
    character*80  :: u80      ! max value
    character*22 :: myname="plot_modImport"
    if (plot_bdeb) write(*,*)myname,'Entering.',irc
    call model_getIndex(mss,set%ind_trg80,set%ind_mod80,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from getIndex.")
       return
    end if
    if (plot_bdeb) write(*,*)myname,'Get target stack.',irc
    call plot_clearModTrgStack(set,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearModTrgStack.")
       return
    end if
    do while (model_loopTarget(mss,n80,v80,l80,u80,crc250,irc))
       call plot_pushModtrg(set,n80,v80,l80,u80,crc250,irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from pushModtrg.")
          return
       end if
    end do
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from loopTarget.")
       return
    end if
    call model_getfilter(mss,set%fltmod250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from getfilter.")
       return
    end if
    if (plot_bdeb) write(*,*)myname,'Done.',irc
    return
  end subroutine plot_modImport
  !
  subroutine plot_modExport(set,mss,crc250,irc) ! set model data
    use model
    implicit none
    type(plot_set), pointer :: set
    type(mod_session), pointer :: mss
    character*250 :: crc250
    integer :: irc
    character*80 :: n80,v80,l80,u80
    integer :: lenn
    integer, external :: length
    character*22 :: myname="plot_modExport"
    if (plot_countModTrg(set).eq.0) return ! no targets = nothing to do
    call model_setIndex(mss,set%ind_trg80,set%ind_mod80,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from setIndex.")
       return
    end if
    call model_loadCache(mss,set%mod250,crc250,irc) ! stored in colocation module
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from loadCache.")
       return
    end if
    !
    call model_clearTargetStack(mss,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearTargetStack.")
       return
    end if
    do while (plot_loopModTrg(set,n80,v80,l80,u80,crc250,irc))
       if (plot_bdeb) then
          lenn=length(n80,80,10)
          write(*,*)myname,"Inside loopModTrg '"//n80(1:lenn)//"'"
       end if
       call model_pushTarget(mss,n80,v80,l80,u80,crc250,irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from pushTarget.")
          return
       end if
    end do
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from loopModTrg.")
       return
    end if
    call model_setfilter(mss,set%fltmod250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from setfilter.")
       return
    end if
    !
    return
  end subroutine plot_modExport
  !
  subroutine plot_colImport(set,css,crc250,irc) ! get colocation data
    use colocation
    implicit none
    type(plot_set), pointer :: set
    type(col_session), pointer :: css
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    character*80 :: n80,v80,l80,u80
    character*250 :: e250
    integer, external :: length
    integer :: leno,lenn
    character*22 :: myname="plot_colImport"
    if (plot_bdeb) write(*,*)myname,'Entering.'
    call colocation_getmodcache(css,set%mod250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from getmodcache.")
       return
    end if
    call colocation_getobscache(css,set%obs250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from getobscache.")
       return
    end if
    if (plot_bdeb) then
       leno=length(set%obs250,250,10)
       write(*,*)myname,"Get default stack '"//set%obs250(1:leno)//"'",irc
    end if
    call plot_clearDefaultStack(set,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearDefaultStack.")
       return
    end if
    if (plot_bdeb)write(*,*)myname,'Looping over default.'
    do while (colocation_loopLocation(css,crc250,irc))
       do while (colocation_loopDefault(css,n80,v80,crc250,irc))
          if (plot_bdeb) then
             lenn=length(n80,80,10)
             write(*,*)myname,"Inside loopDefault '"//n80(1:lenn)//"'"
          end if
          call plot_addDefault(set,n80,v80,crc250,irc)
          if (irc.ne.0) then
             call plot_errorappend(crc250,myname)
             call plot_errorappend(crc250," Error return from addDefault.")
             return
          end if
       end do
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from loopDefaultItem.")
          return
       end if
       call plot_pushDefault(set,crc250,irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from pushDefault.")
          return
       end if
    end do
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from loopDefault.")
       return
    end if
    if (plot_bdeb) write(*,*)myname,'Get match stack.',irc
    call plot_clearMatchStack(set,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearMatchStack.")
       return
    end if
    if (plot_bdeb) write(*,*)myname,'Starting match loop.',irc
    do while (colocation_loopMatch(css,n80,e250,l80,u80,crc250,irc))
       if (plot_bdeb) then
          lenn=length(n80,80,10)
          write(*,*)myname,"Inside loopMatch '"//n80(1:lenn)//"'"
       end if
       call plot_pushMatch(set,n80,e250,l80,u80,crc250,irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from pushMatch.")
          return
       end if
    end do
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from loopMatch.")
       return
    end if
    call colocation_clearDefaultStack(css,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearDefaultStack.")
       return
    end if
    call colocation_clearMatchStack(css,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearMatchStack.")
       return
    end if
    return
  end subroutine plot_colImport
  !
  subroutine plot_colExport(set,css,crc250,irc) ! set colocation data
    use colocation
    use model
    use observations
    implicit none
    type(plot_set), pointer :: set
    type(col_session), pointer :: css
    character*250 :: crc250
    integer :: irc
    character*80 :: n80,v80,l80,u80
    character*250 :: e250
    integer :: lenn
    integer, external :: length
    character*22 :: myname="plot_colExport"
    if (plot_bdeb) write(*,*)myname,'Entering.'
    call colocation_clearDefaultStack(css,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearDefaultStack.")
       return
    end if
    call colocation_clearMatchStack(css,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearMatchStack.")
       return
    end if
    if (plot_bdeb) write(*,*)myname,'Looping defaults.'
    do while (plot_loopLocation(set,crc250,irc))
       do while (plot_loopDefault(set,n80,v80,crc250,irc))
          if (plot_bdeb) then
             lenn=length(n80,80,10)
             write(*,*)myname,"Inside loopDeafult '"//n80(1:lenn)//"'"
          end if
          call colocation_addDefault(css,n80,v80,crc250,irc)
          if (irc.ne.0) then
             call plot_errorappend(crc250,myname)
             call plot_errorappend(crc250," Error return from addDefault.")
             return
          end if
       end do
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from loopDefault.")
          return
       end if
       call colocation_pushDefault(css,crc250,irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from pushDefault.")
          return
       end if
    end do
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from loopLocation.")
       return
    end if
    !
    if(plot_bdeb)write(*,*)myname,"Setting colocation match."
    do while (plot_loopMatch(set,n80,e250,l80,u80,crc250,irc))
       if (plot_bdeb) then
          lenn=length(n80,80,10)
          write(*,*)myname,"Inside loopMatch '"//n80(1:lenn)//"'",set%nmatch
       end if
       call colocation_pushMatch(css,n80,e250,l80,u80,crc250,irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from pushMatch.")
          return
       end if
    end do
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from loopMatch.")
       return
    end if
    if(plot_bdeb)write(*,*)myname,"Done.",irc
    !
    return
  end subroutine plot_colExport
  !
  !
  !###############################################################################
  ! OUTPUT ROUTINES
  !###############################################################################
  !
  !
  subroutine plot_maketable(pss,css,mss,oss,tab250,gra250,cat250,test,fill250,crc250,irc)
    use model
    use observations
    use colocation
    use parse
    implicit none
    type(plot_session), pointer :: pss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    integer :: cid,mid,oid
    character*250 :: tab250,gra250,cat250
    integer :: test
    character*250 :: fill250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="plot_maketable"
    integer :: ounit, ocnt
    character*250 :: leg250
    character*80 :: name80
    integer, external :: length
    integer :: lenc,lene,lenn,lenl,lent,leng
    integer :: ii,jj
    integer :: ncol
    integer :: lcnt
    character*80, allocatable :: col80(:)
    character*250, allocatable :: exp250(:)
    !
    if(plot_bdeb)write(*,*)myname,'Entering.',irc
    !
    ! open table file
    call plot_openFile(pss,ounit,tab250,lent,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," unable to open "//tab250(1:lent))
       call plot_errorappendi(crc250,irc)
       call plot_errorappend(crc250,"\n")
       return
    end if
    ocnt=0;
    !
    call plot_addComments(pss,css,mss,oss,ounit,tab250,lent,gra250,cat250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from makecomments.")
       call plot_errorappendi(crc250,irc)
       call plot_errorappend(crc250,"\n")
       return
    end if
    ! close table output file unit, ounit
    call plot_closeFile(pss,ounit,tab250,lent,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," unable to close "//tab250(1:lent))
       call plot_errorappendi(crc250,irc)
       call plot_errorappend(crc250,"\n")
       return
    end if
    !
    if(plot_bdeb)then
       if (plot_checkSets(pss))then
          write(*,*)myname,'Observation targets differ...'
       end if
    end if
    lcnt=0
    do while (plot_loopset(pss,css,mss,oss,name80,ncol,col80,exp250,leg250,1,crc250,irc))
       call observation_resetStat(oss,crc250,irc)
       call model_resetStat(mss,crc250,irc)
       lcnt=lcnt+1
       !if (lcnt.eq.2) mod_bdeb=.true.
       !parse_bdeb=mod_bdeb
       !if (plot_bdeb) then
       lenn=length(name80,80,10)
       write(*,*)myname,"Processing set '"//name80(1:lenn)//"'"
       !end if
       ! append table file
       call plot_openFileApp(pss,ounit,tab250,lent,crc250,irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," unable to append "//tab250(1:lent))
          call plot_errorappendi(crc250,irc)
          call plot_errorappend(crc250,"\n")
          return
       end if
       ! make output data for this set
       irc=0
       if(plot_bdeb)write(*,*)myname,'Making table.',ounit,ncol,size(col80),size(exp250)
       call colocation_makeTable(css,mss,oss,ounit,ocnt,name80,ncol,col80,&
            & exp250,leg250,test,fill250,crc250,irc)
       if (irc.ne.0) then
          call chop0(name80,80)
          lenn=length(name80,80,10)
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," Error return from makeOutput.")
          call plot_errorappend(crc250,name80(1:lenn))
          return
       end if
       ! close table output file unit, ounit
       call plot_closeFile(pss,ounit,tab250,lent,crc250,irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250," unable to close "//tab250(1:lent))
          call plot_errorappendi(crc250,irc)
          call plot_errorappend(crc250,"\n")
          return
       end if
       call model_printFStat(mss,crc250,irc)
       call observation_printStat(oss,crc250,irc)
       call model_printLStat(mss,crc250,irc)
    end do
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from loopset.")
       return
    end if
    !
    if (irc.ne.0) irc=0 ! oh well...
    !
    if (allocated(col80)) deallocate(col80)
    if (allocated(exp250)) deallocate(exp250)
    !
    write(*,'(X,A,I0,A)') myname,ocnt," locations written to table file "//tab250(1:lent)
    if(plot_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine plot_maketable
  !
  !###############################################################################
  ! ERROR ROUTINES
  !###############################################################################
  !
  !
  subroutine plot_errorappend(crc250,string)
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
  end subroutine plot_errorappend
  subroutine plot_errorappendi(crc250,inum)
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
  end subroutine plot_errorappendi
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
  subroutine plot_strep(str250,nn,src100,rep100,lrep,irc)
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
  end subroutine plot_strep
  !
end module plot
