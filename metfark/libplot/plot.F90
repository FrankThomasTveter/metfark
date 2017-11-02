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
     character*250 :: name250=""
     character*250 :: value250=""
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
     integer :: nTrg
     integer :: cii = 0
     logical, pointer :: vset(:)       ! is value set?
     character*80, pointer :: v80(:)   ! value
     integer, pointer :: vlen(:)       ! value length
     real, pointer :: val(:)   ! value
     type(plot_default), pointer :: prev => null()   ! linked list
     type(plot_default), pointer :: next => null()   ! linked list
  end type plot_default
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
  type :: plot_set
     character*250 :: name250=""
     integer :: id=0
     character*250 :: x250=""
     character*250 :: y250=""
     character*250 :: leg250=""
     ! model data
     ! obs data
     CHARACTER(LEN=250)              :: tablepath=""
     integer :: category                   ! filter category/bufrType
     integer :: subCategory                ! filter subcategory/subType
     character*80                    :: ind_obs80=""          ! obs index target name
     character*250                   :: ind_exp250=""         ! index target expression
     logical                         :: ind   ! true if both ind_start and ind_stop are valid
     real                            :: ind_start=0.0D0         ! lowest index
     real                            :: ind_stop=0.0D0          ! highest index
     character*250 :: obs250 ! observation cache file
     character*250 :: mod250 ! model cache file
     character*80                    :: ind_trg80=""            ! model index target name
     character*80                    :: ind_mod80=""            ! model index variable
     ! obs data
     type(plot_obstrg), pointer :: firstObstrg => null()        ! linked list
     type(plot_obstrg), pointer :: lastObstrg => null()         ! linked list
     ! mod data
     type(plot_modtrg), pointer :: firstModtrg => null()        ! linked list
     type(plot_modtrg), pointer :: lastModtrg => null()         ! linked list
     type(plot_obstrg), pointer :: cObstrg => null()            ! linked list
     type(plot_modtrg), pointer :: cModtrg => null()            ! linked list
     ! colocation data
     type(plot_default), pointer :: firstDef => null()          ! linked list start
     type(plot_default), pointer :: lastDef => null()           ! linked list end
     type(plot_default), pointer :: cDef => null()              ! current default
     integer :: ndef = 0                                        ! number of defaults
     type(plot_match), pointer :: firstMatch => null()          ! linked list
     type(plot_match), pointer :: lastMatch => null()           ! linked list
     type(plot_match), pointer :: cMatch => null()              ! linked list
     character*250 :: filter250=""
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
     !
     ! output
     character*250 :: tab250=""  ! table file name
     integer :: lent             ! length of tab250
     integer :: tunit = 0        ! table file unit
     character*250 :: gra250=""  ! graphics file name
     integer :: leng             ! length of gra250
     integer :: gunit = 0        ! graphics file unit
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
     type(plot_attribute), pointer :: firstAttribute => null()         ! linked list
     type(plot_attribute), pointer :: lastAttribute => null()         ! linked list
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
    character*25 :: myname = "plot_openSession"
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
    allocate(pss%firstSet,pss%lastSet,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'attribute chain'.")
       call plot_errorappend(crc250,"\n")
       return
    end if
    pss%firstSet%next => pss%lastSet
    pss%lastSet%prev => pss%firstSet
    sid = pss%sid
    return
  end subroutine plot_opensession

  subroutine plot_getSession(pss,sid,crc250,irc)
    type(plot_session), pointer :: pss !  current session
    integer :: sid
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "plot_getSession"
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
          if (plot_bdeb) write(*,*)myname,'Exiting with sid:',sid,irc
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
    character*25 :: myname = "plot_closeSession"
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
    character*25 :: myname = "plot_removeSession"
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
    character*22 :: myname = "settype"
    pss%type250=type250
    return
  end subroutine plot_settype
  !
  subroutine plot_clearattrstack(pss,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    integer :: irc2
    character*22 :: myname = "clearattrstack"
    type(plot_attribute), pointer :: cat,nat !  current attribute
    cat => pss%firstAttribute%next
    do while (.not.associated(cat,target=pss%lastAttribute))
       nat => cat%next
       call plot_unlinkAttribute(cat)
       call plot_deallocateAttribute(cat)
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
  subroutine plot_pushattr(pss,nam250,val250,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: nam250
    character*250 :: val250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname = "pushattr"
    type(plot_attribute), pointer :: cat !  current attribute
    allocate(cat,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'attribute'.")
    end if
    cat%name250=nam250
    cat%value250=val250
    cat%next => pss%lastAttribute
    cat%prev => pss%lastAttribute%prev
    cat%prev%next => cat
    cat%next%prev => cat
    nullify(cat)
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
  subroutine plot_openTablefile(pss,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    integer, external :: ftunit
    character*22 :: myname = "openTablefile"
    pss%tunit=ftunit(irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error return from FTUNIT.")
       call plot_errorappendi(crc250,irc)
       return
    end if
    open(unit=pss%tunit,file=pss%tab250(1:pss%lent), &
         & access="append",form="formatted",status="unknown",iostat=irc)
    if(plot_bdeb)write(*,*)myname,'Opened:',pss%tab250(1:pss%lent),pss%tunit,irc
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to open table file '"//pss%tab250(1:pss%lent)//"'")
       call plot_errorappendi(crc250,irc)
       return
    end if
    return
  end subroutine plot_openTablefile
  !
  subroutine plot_closeTablefile(pss,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname = "closeTablefile"
    close(unit=pss%tunit,iostat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to close table file '"//pss%tab250(1:pss%lent)//"'")
       call plot_errorappendi(crc250,irc)
       return
    end if
    return
  end subroutine plot_closeTablefile
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
  subroutine plot_openGraphicsfile(pss,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    integer, external :: ftunit
    character*22 :: myname = "openGraphicsfile"
    pss%gunit=ftunit(irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error return from FTUNIT.")
       call plot_errorappendi(crc250,irc)
       return
    end if
    open(unit=pss%gunit,file=pss%gra250(1:pss%leng),&
         & access="sequential",form="formatted",status="unknown",iostat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to open graphics file '"//pss%gra250(1:pss%leng)//"'")
       call plot_errorappendi(crc250,irc)
       return
    end if
    return
  end subroutine plot_openGraphicsfile
  !
  subroutine plot_closeGraphicsfile(pss,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname = "closeGraphicsfile"
    close(unit=pss%gunit,iostat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to close graphics file '"//pss%gra250(1:pss%leng)//"'")
       call plot_errorappendi(crc250,irc)
       return
    end if
    return
  end subroutine plot_closeGraphicsfile
  !
  subroutine plot_setTime(pss,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname = "clearsetstack"
    call date_and_time(VALUES=pss%values)
    return
  end subroutine plot_setTime
  !
  subroutine plot_strepfiles(pss,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname = "strepfiles"
    integer,parameter  :: nn = 6
    character*100 :: src100(nn) = (/'YY','MM','DD','HH','MI','SS'/)
    character*100 :: rep100(nn)
    logical :: lrep(nn)
    call plot_settime(pss,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error return from 'settime'.")
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
    character*22 :: myname = "strepfiles"
    integer,parameter  :: nn = 6
    call plot_setTablefile(pss,tab250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error return from 'setTableFile'.")
       return
    end if
    call plot_setGraphicsfile(pss,gra250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error return from 'setGraphicsFile'.")
       return
    end if
    call plot_strepfiles(pss,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error return from 'strepFiles'.")
       return
    end if
    call plot_getTablefile(pss,tab250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error return from 'setTableFile'.")
       return
    end if
    call plot_getGraphicsfile(pss,gra250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error return from 'setGraphicsFile'.")
       return
    end if
    return
  end subroutine plot_setFiles
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
    character*22 :: myname = "clearsetstack"
    type(plot_set), pointer :: cset,nset !  current set
    cset => pss%firstSet%next
    do while (.not.associated(cset,target=pss%lastSet))
       nset => cset%next
       call plot_unlinkSet(cset)
       call plot_deallocateSet(cset)
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
    character*22 :: myname = "allocateset"
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
    allocate(set%firstDef,set%lastDef,stat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to allocate 'set-def'.")
    end if
    set%firstDef%next => set%lastDef
    set%lastDef%prev => set%firstDef
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
  subroutine plot_pushset(pss,css,mss,oss,nam250,x250,y250,leg250,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(plot_session), pointer :: pss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    character*250 :: nam250
    character*250 x250,y250,leg250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="pushset"
    type(plot_set), pointer :: set !  current set
    if(plot_bdeb) write(*,*)myname,'Entering.',irc
    call plot_allocateSet(set,crc250,irc)
    set%name250=nam250
    set%x250=x250
    set%y250=y250
    set%leg250=leg250
    if(plot_bdeb) write(*,*)myname,'Importing obs.',irc
    call plot_obsImport(set,oss,crc250,irc) ! get obs data 
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from obsImport.")
       return
    end if
    if(plot_bdeb) write(*,*)myname,'Importing model.',irc
    call plot_modImport(set,mss,crc250,irc) ! get model data
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from modImport.")
       return
    end if
    if(plot_bdeb) write(*,*)myname,'Importing colocation.',irc
    call plot_colImport(set,css,crc250,irc) ! get colocation data
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from colImport.")
       return
    end if
    set%next => pss%lastSet
    set%prev => pss%lastSet%prev
    set%prev%next => set
    set%next%prev => set
    nullify(set)
    if(plot_bdeb) write(*,*)myname,'Exiting.',irc
    return
  end subroutine plot_pushset
  !
  ! loop over sets from top and delete them
  logical function plot_pullset(pss,css,mss,oss,nam250,x250,y250,leg250,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(plot_session), pointer :: pss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    character*250 :: nam250
    character*250 :: x250,y250,leg250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="pullset"
    type(plot_set), pointer :: set !  current set
    plot_pullset=.false. ! only true if all is ok
    set => pss%firstSet%next
    if (.not.associated(set,pss%lastSet)) then
       nam250=set%name250
       x250=set%x250
       y250=set%y250
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
       call plot_unlinkSet(set)       
       call plot_deallocateSet(set)
       plot_pullset=.true.
    end if
    return
  end function plot_pullset
  !
  ! loop over sets from bottom and delete them
  logical function plot_popset(pss,css,mss,oss,nam250,x250,y250,leg250,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(plot_session), pointer :: pss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    character*250 :: nam250
    character*250 :: x250,y250,leg250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="popset"
    type(plot_set), pointer :: set !  current set
    plot_popset=.false. ! only true if all is ok and irc==0
    set => pss%lastSet%prev
    if (.not.associated(set,pss%firstSet)) then
       nam250=set%name250
       x250=set%x250
       y250=set%y250
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
       call plot_unlinkSet(set)       
       call plot_deallocateSet(set)
       plot_popset=.true.
    end if
    return
  end function plot_popset
  !
  ! loop over sets from the top without deleting them...
  logical function plot_loopSet(pss,css,mss,oss,nam250,x250,y250,leg250,crc250,irc)
    use model
    use observations
    use colocation
    implicit none
    type(plot_session), pointer :: pss !  current session
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    character*250 :: nam250
    character*250 :: x250,y250,leg250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="loopSet"
    plot_loopset=.false. ! only true if all is ok
    if (.not.associated(pss%currentSet)) then
       pss%currentSet =>  pss%firstSet%next 
    else
       pss%currentSet =>  pss%currentSet%next
    end if
    if (associated(pss%currentSet,pss%lastSet)) then
       nullify(pss%currentSet)
       plot_loopset=.false.
    else
       nam250=pss%currentSet%name250
       x250=pss%currentSet%x250
       y250=pss%currentSet%y250
       leg250=pss%currentSet%leg250
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
       plot_loopset=.true.
    end if
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
    character*22 :: myname = "clearobstrgstack"
    type(plot_obstrg), pointer :: ctrg,ntrg !  current target
    ctrg => set%firstObstrg%next
    do while (.not.associated(ctrg,target=set%lastObstrg))
       ntrg => ctrg%next
       call plot_unlinkObstrg(ctrg)
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
    character*22 :: myname = "clearmodtrgstack"
    type(plot_modtrg), pointer :: ctrg,ntrg !  current target
    ctrg => set%firstModtrg%next
    do while (.not.associated(ctrg,target=set%lastModtrg))
       ntrg => ctrg%next
       call plot_unlinkModtrg(ctrg)
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
    character*22 :: myname = "clearmatchstack"
    type(plot_match), pointer :: cmatch,nmatch !  current target
    cmatch => set%firstMatch%next
    do while (.not.associated(cmatch,target=set%lastMatch))
       nmatch => cmatch%next
       call plot_unlinkMatch(cmatch)
       call plot_deallocateMatch(cmatch)
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
    type(plot_default), pointer :: cDef !  the current default target
    type(plot_default), pointer :: nDef !  the next default target
    character*25 :: myname = "plot_cleardefault"
    if (plot_bdeb) write(*,*)myname,'Entering.',irc
    if (plot_bdeb) write(*,*)myname,'Association.',associated(set), associated(set%firstDef)
    cDef => set%firstDef%next
    do while (.not.associated(cDef,target=set%lastDef))
       nDef => cDef%next
       cDef%prev%next =>  cDef%next
       cDef%next%prev =>  cDef%prev
       if (associated(cDef%vset)) deallocate(cDef%vset)
       if (associated(cDef%v80)) deallocate(cDef%v80)
       if (associated(cDef%vlen)) deallocate(cDef%vlen)
       if (associated(cDef%val)) deallocate(cDef%val)
       deallocate(cDef,stat=irc)
       cDef => nDef
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
    character*22 :: myname = "pushobstrg"
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
    character*22 :: myname = "pushmodtrg"
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
    trg%prev%next => trg
    trg%next%prev => trg
    nullify(trg)
    return
  end subroutine plot_pushmodtrg
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
    character*22 :: myname = "pushmatch"
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
    nullify(trg)
    return
  end subroutine plot_pushmatch
  !
  ! add default element
  !
  subroutine plot_addDefault(set,n80,v80,crc250,irc)
    type(plot_set), pointer :: set !  current session
    character*80 :: n80 ! target name
    character*80 :: v80 ! target value
    character*250 :: crc250
    integer :: irc
    integer :: ii, irc2, lenv, lenn
    integer, external :: length
    character*25 :: myname = "plot_addDefault"
    if(plot_bdeb)write(*,*)myname,'Entering.',irc
    call plot_makeTargetList(set,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from makeTargetList.")
       call plot_errorappendi(crc250,irc)
       call plot_errorappend(crc250,"\n")
       return
    end if
    if(plot_bdeb)write(*,*)myname,'Here.',associated(set%cDef)
    if (.not.associated(set%cDef)) then
       allocate(set%cDef, stat=irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250,"Unable to allocate 'firstItm/lastItm'.")
          call plot_errorappend(crc250,"\n")
          return
       end if
       set%cDef%nTrg=set%nTrgMod
       allocate(set%cDef%vset(set%cDef%nTrg), set%cDef%v80(set%cDef%nTrg), &
            & set%cDef%vlen(set%cDef%nTrg),  set%cDef%val(set%cDef%nTrg), stat=irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250,"Unable to allocate 'session: current Default'.")
          call plot_errorappend(crc250,"\n")
          return
       end if
    end if
    call chop0(n80,80)
    lenn=length(n80,80,10)
    if(plot_bdeb)write(*,*)myname,'Looking for target:',n80(1:lenn)
    ii=1
    SEEK:do while (ii.le.set%nTrgMod)
       if (set%trgMod80(ii)(1:set%trgModLent(ii)).eq.n80(1:lenn)) exit SEEK
       ii=ii+1
    end do SEEK
    if (ii.le.set%nTrgMod) then
       set%cDef%vset(ii)=.true.
       call chop0(v80,80)
       lenv=length(v80,80,10)
       set%cDef%v80(ii)=v80 ! value
       set%cDef%vlen(ii)=lenv
       read(v80(1:lenv),*,iostat=irc2) set%cDef%val(ii)
    else
       irc=220
       ! write(*,*)myname,'Targets:',set%nTrgMod,set%ntarget
       ! do ii=1,set%nTrgMod
       !    write(*,*)myname,'Target:',ii,set%trgMod80(ii)(1:set%trgModLent(ii))
       ! end do
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Target not found:"//n80(1:lenn))
       return
    end if
    if(plot_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine plot_addDefault
  !
  subroutine plot_maketargetlist(set,crc250,irc)
    type(plot_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "plot_maketargetlist"
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
    type(plot_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    type(plot_default), pointer :: newDefault
    character*25 :: myname = "plot_pushDefault"
    if(plot_bdeb)write(*,*)myname,'Entering.',irc
    if (.not.associated(set%firstDef)) then
       allocate(set%firstDef,set%lastDef, stat=irc)
       if (irc.ne.0) then
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250,"Unable to allocate 'firstDef/lastDef'.")
          call plot_errorappend(crc250,"\n")
          return
       end if
       set%firstDef%next => set%lastDef
       set%lastDef%prev => set%firstDef
       set%ndef=0
    end if
    if (associated(set%cDef)) then
       newDefault => set%cDef
       set%ndef=set%ndef+1
       newDefault%prev => set%lastDef%prev
       newDefault%next => set%lastDef
       newDefault%prev%next => newDefault
       newDefault%next%prev => newDefault
       nullify(set%cDef)
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
    character*22 :: myname ="popobstrg"
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
    character*22 :: myname ="popmodtrg"
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
    character*22 :: myname ="popmatch"
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
    character*22 :: myname ="loopobstrg"
    plot_loopobstrg=.false. ! only true if all is ok
    if (.not.associated(set%cobstrg)) then
       set%cobstrg =>  set%firstObstrg%next 
    else
       set%cobstrg =>  set%cobstrg%next
    end if
    if (associated(set%cobstrg,set%lastObstrg)) then
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
    character*22 :: myname ="loopmodtrg"
    plot_loopmodtrg=.false. ! only true if all is ok
    if (.not.associated(set%cmodtrg)) then
       set%cmodtrg =>  set%firstModtrg%next 
    else
       set%cmodtrg =>  set%cmodtrg%next
    end if
    if (associated(set%cmodtrg,set%lastModtrg)) then
       nullify(set%cmodtrg)
       plot_loopmodtrg=.false.
    else
       trg80=set%cmodtrg%trg80
       var80=set%cmodtrg%var80
       min80=set%cmodtrg%min80
       max80=set%cmodtrg%max80
       plot_loopmodtrg=.true.
    end if
    return
  end function plot_loopmodtrg
  !
  logical function plot_loopDefault(set,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="loopdefault"
    plot_loopdefault=.false. ! only true if all is ok
    if (.not.associated(set%cDef)) then
       set%cDef =>  set%firstDef%next 
    else
       set%cDef =>  set%cDef%next
    end if
    if (associated(set%cDef,set%lastDef)) then
       nullify(set%cDef)
       plot_loopdefault=.false.
    else
       plot_loopdefault=.true.
       set%cDef%cii=0
    end if
    return
  end function plot_loopdefault
  !
  logical function plot_loopDefaultItem(set,trg80,var80,crc250,irc)
    implicit none
    type(plot_set), pointer :: set !  current session
    character*80  :: trg80      ! target name
    character*80  :: var80      ! variable
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="loopdefitem"
    plot_loopdefaultitem=.false. ! only true if all is ok
    if (.not.associated(set%cdef)) then
       plot_loopdefaultitem=.false.
    else
       if (set%cdef%cii .eq. 0) then
          set%cdef%cii=1
       else
          set%cdef%cii = set%cdef%cii +1
       end if
       if (set%cdef%cii .gt. set%cdef%nTrg) then
          set%cdef%cii=0
          plot_loopdefaultitem=.false.
       else
          trg80=set%trgMod80(set%cdef%cii)
          var80=set%cdef%v80(set%cdef%cii)
          plot_loopdefaultitem=.true.
       end if
    end if
    return
  end function plot_loopDefaultItem
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
    character*22 :: myname ="loopmatch"
    plot_loopmatch=.false. ! only true if all is ok
    if (.not.associated(set%cmatch)) then
       set%cmatch =>  set%firstMatch%next 
    else
       set%cmatch =>  set%cmatch%next
    end if
    if (associated(set%cmatch,set%lastMatch)) then
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
    character*22 :: myname ="obsImport"
    call observation_getTablePath(oss,set%tablepath,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from setTablePath.")
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
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from getIndexLimitsRaw.")
       return
    end if
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
    character*22 :: myname ="obsExport"
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
       call plot_errorappend(crc250," Error return from setIndexLimitsRaw.")
       return
    end if
    call observation_clearTargetStack(oss,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearTargetStack.")
       return
    end if
    do while (plot_loopObstrg(set,trg80,pos250,descr80,info250,min80,max80,crc250,irc))
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
    character*22 :: myname ="modImport"
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
    character*22 :: myname ="modExport"
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
    character*22 :: myname ="colImport"
    call colocation_getfilter(css,set%filter250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from getfilter.")
       return
    end if
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
    if (plot_bdeb) write(*,*)myname,'Get default stack.',irc
    call plot_clearDefaultStack(set,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearDefaultStack.")
       return
    end if
    do while (colocation_loopDefault(css,crc250,irc))
       do while (colocation_loopDefaultItem(css,n80,v80,crc250,irc))
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
       if (plot_bdeb) write(*,*)myname,'Inside loop.',irc
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
    character*22 :: myname ="colExport"
    call colocation_setfilter(css,set%filter250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from setfilter.")
       return
    end if
    !
    call colocation_clearDefaultStack(css,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearDefaultStack.")
       return
    end if
    do while (plot_loopDefault(set,crc250,irc))
       do while (plot_loopDefaultItem(set,n80,v80,crc250,irc))
          call colocation_addDefault(css,n80,v80,crc250,irc)
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
       call colocation_pushDefault(css,crc250,irc)
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
    !
    call colocation_clearMatchStack(css,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250," Error return from clearMatchStack.")
       return
    end if
    do while (plot_loopMatch(set,n80,e250,l80,u80,crc250,irc))
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
  subroutine plot_maketable(pss,css,mss,oss,tab250,gra250,test,crc250,irc)
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
    character*250 :: tab250,gra250
    integer :: test
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="maketable"
    type(parse_session), pointer :: psx,psy
    integer :: nvar,ounit,lent
    character*250 :: leg250,x250,y250,nam250
    character*80, allocatable :: var80(:)
    real, allocatable :: val(:)
    integer :: mloc,mtrg,oloc,otrg
    integer :: lenc,lene,lenn,lenl
    integer, external :: length,ftunit
    real :: valx, valy
    integer :: tmod,emod,dmod,tobs,ii,jj,ind_ii,nfunc
    logical :: bobsind
    type(parse_session), pointer :: pse
    type(parse_pointer), pointer :: ppt(:) ! parse sessions
    integer :: locid,locstart
    !
    real :: mod_start = 0.0D0
    real :: mod_stop = 0.0D0
    real :: obs_start = 0.0D0
    real :: obs_stop = 0.0D0
    logical :: mod_lim,obs_lim,bok,first,lok
    !
    ! open file
    call chop0(tab250,250)
    lent=length(tab250,250,10)
    ounit=ftunit(irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," no free unit number for:"//tab250(1:lent))
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    open ( unit=ounit, status="unknown", form="formatted", &
         &        access="sequential", &
         &        iostat=irc, file=tab250(1:lent) )
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," unable to open:"//tab250(1:lent))
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
    !
    if(plot_bdeb)write(*,*)myname,'Entering.',irc
    !
    ! loop over set and write attributes and legend as comments
    !
    if(plot_bdeb)write(*,*)myname,'Writing attributes and legends.',pss%tunit
    write(pss%tunit,'("#")',iostat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error writing to table file'.")
       call plot_errorappend(crc250,pss%tab250(1:pss%lent))
       return
    end if
    write(pss%tunit,'("# Legend table")',iostat=irc)
    do while (plot_loopset(pss,css,mss,oss,nam250,x250,y250,leg250,crc250,irc))
       call chop0(nam250,250)
       lenn=length(nam250,250,1)
       call chop0(leg250,250)
       lenl=length(leg250,250,1)
       write(pss%tunit,'("#",X,A,X,A)',iostat=irc)nam250(1:lenn),leg250(1:lenl)
    end do
    !
    if(plot_bdeb)write(*,*)myname,'Writing data.',pss%tunit
    write(pss%tunit,'("#")',iostat=irc)
    write(pss%tunit,'("# Data table")',iostat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error writing to Table file'.")
       call plot_errorappend(crc250,pss%tab250(1:pss%lent))
       return
    end if
    do while (plot_loopset(pss,css,mss,oss,nam250,x250,y250,leg250,crc250,irc))
       ! make output data for this set
       irc=0
       call colocation_makeTable(css,mss,oss,ounit,nam250,x250,y250,leg250,test,crc250,irc)
       if (irc.ne.0) then
          call chop0(nam250,250)
          lenn=length(nam250,250,10)
          call plot_errorappend(crc250,myname)
          call plot_errorappend(crc250,"Error return from 'makeOutput'.")
          call plot_errorappend(crc250,nam250(1:lenn))
          return
       end if
    end do
    !
    ! close table output file unit, ounit
    !
    close (unit=ounit,iostat=irc)
    if (irc.ne.0) irc=0 ! oh well...
    if(plot_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine plot_maketable

  !
  subroutine plot_makegraphics(pss,tab250,gra250,test,crc250,irc)
    implicit none
    type(plot_session), pointer :: pss !  current session
    character*250 :: tab250,gra250
    integer :: test
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="makegraphics"
    call plot_setFiles(pss,tab250,gra250,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error return from 'setFiles'.")
       return
    end if
    call plot_openGraphicsfile(pss,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error return from 'openGraphicsFile'.")
       return
    end if
    !
    ! plot
    write(pss%gunit,'("Graphics-file generation not implemented...")',iostat=irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Unable to write to")
       call plot_errorappend(crc250,pss%gra250(1:pss%leng))
       return
    end if
    !
    call plot_closeGraphicsfile(pss,crc250,irc)
    if (irc.ne.0) then
       call plot_errorappend(crc250,myname)
       call plot_errorappend(crc250,"Error return from 'closeGraphicsFile'.")
       return
    end if
    !
    !irc=999
    !call plot_errorappend(crc250,myname)
    !call plot_errorappend(crc250," Not implemented.")
    !call plot_errorappendi(crc250,irc)
    !call plot_errorappend(crc250,"\n")
    !
    return
  end subroutine plot_makegraphics
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
  end subroutine plot_errorappendi
  !
  !
  !###############################################################################
  ! SORTING ROUTINES
  !###############################################################################
  !
  !
  subroutine plot_heapsearch1r(maxnn,key,eps,nn,ind,tkey,left,right)
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
    logical :: bdone
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
       kfl=plot_cmpr(tkey,key(ind(mfl)),eps)
       kcl=plot_cmpr(tkey,key(ind(mcl)),eps)
       !write(*,'(X,A,X,I3,F9.2,5(X,I3),3(X,F9.2),2(X,I5))')'plot_heapsearch:',left,mid,right,mfl,mcl,kfl,kcl,&
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
       mch=plot_cmpr(tkey, key(ind(left-1)),eps)
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
       mch=plot_cmpr(tkey, key(ind(right+1)),eps)
       if (mch == 0) then ! equal or target is above
          right=right+1
          bdone=(right>nn-1)
       else
          bdone=.true.
       end if
    end do
    !
  end subroutine plot_heapsearch1r
  !
  subroutine plot_heapsort1r(mm,key1,eps,newnn,nn,ind,uniq)
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
    logical :: uniq               ! Ignore duplicate records
    !
    integer :: ii,dmp

    if (nn.eq.0) then
       newnn=0
       return
    end if
    !
    do ii = nn/2, 1, -1
       call plot_pushdownr(ii, nn, mm,key1,eps,newnn,nn,ind)
    end do
    do ii = nn, 2, -1
       call plot_swap(ind(1), ind(ii))
       call plot_pushdownr(1, ii-1, mm,key1,eps,newnn,nn,ind)
    end do
    !
    if (uniq) then
       dmp=0
       newnn=1
       do ii=2,nn
          if (plot_cmpr(key1(ind(ii-1)),key1(ind(ii)),eps) /= 0) then
             ! Keep ind(ii)
             newnn = newnn+1
             ind(newnn) = ind(ii)
          else
             dmp=dmp+1
          end if
       end do
       if(plot_bdeb)write(*,*) "PLOT_HEAPSORT dumped elements:",dmp
    else
       newnn=nn
    end if
    !
    !
  end subroutine plot_heapsort1r
  !
  subroutine plot_heapsort1i(mm,key1,newnn,nn,ind,uniq)
    !
    !! Generate sorted index for key1 
    !
    implicit none

    integer :: mm                ! Number of elements
    integer :: key1(mm)             ! key
    integer :: newnn             ! new number of keys
    integer :: nn                ! Number of elements
    integer :: ind(nn)           ! Resulting sorted index
    logical :: uniq               ! Ignore duplicate records
    !
    integer :: ii,dmp

    if (nn.eq.0) then
       newnn=0
       return
    end if
    !
    do ii = nn/2, 1, -1
       call plot_pushdowni(ii, nn, mm,key1,newnn,nn,ind)
    end do
    do ii = nn, 2, -1
       call plot_swap(ind(1), ind(ii))
       call plot_pushdowni(1, ii-1, mm,key1,newnn,nn,ind)
    end do
    !
    if (uniq) then
       dmp=0
       newnn=1
       do ii=2,nn
          if (plot_cmpi(key1(ind(ii-1)),key1(ind(ii))) /= 0) then
             ! Keep ind(ii)
             newnn = newnn+1
             ind(newnn) = ind(ii)
          else
             dmp=dmp+1
          end if
       end do
       if(plot_bdeb)write(*,*) "PLOT_HEAPSORT dumped elements:",dmp
    else
       newnn=nn
    end if
    !
    !
  end subroutine plot_heapsort1i
  !
  subroutine plot_pushdownr(first, last,mm,key1,eps,newnn,nn,ind)
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
          if (plot_cmpr(key1(ind(r)),key1(ind( 2*r)),eps) > 0) then
             call plot_swap(ind(r), ind(2*r))
          end if
          exit MAINLOOP
       else
          if (plot_cmpr(key1(ind(r)),key1(ind(2*r)),eps) > 0 .and. &
               & plot_cmpr(key1(ind(2*r)),key1(ind(2*r+1)),eps) <= 0) then
             call plot_swap(ind(r), ind(2*r))
             r = 2*r
          else if (plot_cmpr(key1(ind(r)),key1(ind(2*r+1)),eps)>0 .and. &
               & plot_cmpr(key1(ind(2*r+1)),key1(ind(2*r)),eps)<0) then
             call plot_swap(ind(r), ind(2*r+1))
             r = 2*r+1
          else
             exit MAINLOOP
          end if
       end if
    end do MAINLOOP
    !
  end subroutine plot_pushdownr
  !
  subroutine plot_pushdowni(first, last,mm,key1,newnn,nn,ind)
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
          if (plot_cmpi(key1(ind(r)),key1(ind( 2*r))) > 0) then
             call plot_swap(ind(r), ind(2*r))
          end if
          exit MAINLOOP
       else
          if (plot_cmpi(key1(ind(r)),key1(ind(2*r))) > 0 .and. &
               & plot_cmpi(key1(ind(2*r)),key1(ind(2*r+1))) <= 0) then
             call plot_swap(ind(r), ind(2*r))
             r = 2*r
          else if (plot_cmpi(key1(ind(r)),key1(ind(2*r+1)))>0 .and. &
               & plot_cmpi(key1(ind(2*r+1)),key1(ind(2*r)))<0) then
             call plot_swap(ind(r), ind(2*r+1))
             r = 2*r+1
          else
             exit MAINLOOP
          end if
       end if
    end do MAINLOOP
    !
  end subroutine plot_pushdowni
  !
  !
  integer function plot_cmpr(a,b,eps)
    real :: a
    real :: b
    real :: eps
    if (abs(a-b) < eps) then
       plot_cmpr = 0
    else if (a < b) then
       plot_cmpr = 1
    else
       plot_cmpr = -1
    end if
  end function plot_cmpr
  !
  integer function plot_cmpi(a,b)
    integer :: a
    integer :: b
    if (a == b) then
       plot_cmpi = 0
    else if (a < b) then
       plot_cmpi = 1
    else
       plot_cmpi = -1
    end if
  end function plot_cmpi
  !
  !
  subroutine plot_swap(k1, k2)
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
  end subroutine plot_swap
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
