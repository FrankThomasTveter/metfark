module colocation
  use parse
  IMPLICIT NONE
  !
  ! Global constants
  !
  logical     :: col_bdeb=.false.
  !
  ! default values for the model targets
  !
  type :: col_default
     integer :: cDef = 0
     integer :: cii = 0
     logical, pointer :: vset(:)       ! is value set?
     character*80, pointer :: v80(:)   ! value
     integer, pointer :: vlen(:)       ! value length
     real, pointer :: val(:)   ! value
     type(col_default), pointer :: prev => null()   ! linked list
     type(col_default), pointer :: next => null()   ! linked list
  end type col_default
  !
  ! match rules for model targets and observation targets
  !
  type :: col_match
     integer :: cMatch = 0
     integer :: cii = 0
     logical, pointer :: vset(:)                         ! is value set?
     character*80, pointer  :: n80(:)                    ! name
     integer, pointer  :: lenn(:)
     character*250, pointer  :: e250(:)                  ! match
     integer, pointer :: lene(:)
     character*80, pointer  :: l80(:)                    ! lower limit
     character*80, pointer  :: u80(:)                    ! upper limit
     type(parse_pointer), pointer  :: psp(:) => null()   ! parse-pointer
     real, pointer  :: val(:)
     type(col_match), pointer :: prev => null()   ! linked list
     type(col_match), pointer :: next => null()   ! linked list
  end type col_match
  !
  ! SESSION VARIABLES
  !
  type :: col_session
     integer                         :: sid
     character*250 :: obs250=""
     character*250 :: mod250=""
     character*250 :: filter250=""
     !
     ! imported model targets
     integer :: ctrg = 0 ! targets                  ! number of targets allocated in array
     character*80, pointer :: trg80(:) => null()       ! list of target names
     integer, pointer :: trg_lent(:) => null()         ! list of target name length
     !
     ! default
     type(col_default), pointer :: firstDef => null()   ! linked list start
     type(col_default), pointer :: lastDef => null()    ! linked list end
     type(col_default), pointer :: currentDef => null() ! current default input variable
     type(col_default), pointer :: cDef => null()       ! current default loop variable
     integer :: ndef=0                                  ! number of items in target-chain
     !
     ! match rules
     type(col_match), pointer :: firstMatch => null()   ! linked list start
     type(col_match), pointer :: lastMatch => null()    ! linked list end
     type(col_match), pointer :: currentMatch => null() ! current match input
     type(col_match), pointer :: cMatch => null()       ! current match loop
     integer :: nmatch=0                               ! number of items in match-chain
     !
     ! output
     logical :: pxml=.false.     ! print XML to stdout?
     integer :: tunit = 0        ! table file unit
     !
     type(col_session), pointer :: prev => null()         ! linked list
     type(col_session), pointer :: next => null()         ! linked list
  end type col_session
  !
  integer :: maxid=0 ! session counter
  type(col_session), pointer :: firstSession => null()   ! linked list start
  type(col_session), pointer :: lastSession => null()    ! linked list end
  !
CONTAINS
  !
  !###############################################################################
  ! SESSION ROUTINES
  !###############################################################################
  !
  subroutine colocation_opensession(sid,css,crc250,irc)
    implicit none
    integer :: sid
    type(col_session),pointer :: css  !  new session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "colocation_openSession"
    if (.not.associated(firstSession)) then
       allocate(firstSession, lastSession,stat=irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Unable to allocate 'firstSession/lastSession'.")
          call colocation_errorappend(crc250,"\n")
          return
       end if
       firstSession%next => lastSession
       lastSession%prev => firstSession
    end if
    nullify(css)
    allocate(css,stat=irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"Unable to allocate 'new session'.")
       call colocation_errorappend(crc250,"\n")
       return
    end if
    !
    allocate(css%firstDef,css%lastDef, stat=irc) ! 
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"Unable to allocate &
            & 'css%firstDef/css%lastDef'.")
       call colocation_errorappend(crc250,"\n")
       return
    end if
    css%firstDef%next => css%lastDef
    css%lastDef%prev => css%firstDef
    !
    allocate(css%firstMatch,css%lastMatch, stat=irc) ! 
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"Unable to allocate &
            & 'css%firstMatch/css%lastMatch'.")
       call colocation_errorappend(crc250,"\n")
       return
    end if
    css%firstMatch%next => css%lastMatch
    css%lastMatch%prev => css%firstMatch
    css%nmatch=0
    !
    maxid=maxid+1
    css%sid=maxid
    css%prev => lastSession%prev
    css%next => lastSession
    css%prev%next => css
    css%next%prev => css
    sid = css%sid
    return
  end subroutine colocation_opensession

  subroutine colocation_getSession(css,sid,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    integer :: sid
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "colocation_getSession"
    if (.not.associated(firstSession)) then
       irc=911
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"No session is opened!")
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
    css => firstSession%next
    do while ( .not.associated(css,target=lastSession))
       if (css%sid .eq. sid) then
          return
       end if
       css=>css%next
    end do
    nullify(css)
    irc=342
    call colocation_errorappend(crc250,myname)
    call colocation_errorappend(crc250,"Invalid session id:")
    call colocation_errorappendi(crc250,sid)
    call colocation_errorappend(crc250,"\n")
    return
  end subroutine colocation_getSession

  subroutine colocation_closeSession(css,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "colocation_closeSession"
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       call colocation_removeSession(css,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250," Error return from removeSession.")
          call colocation_errorappendi(crc250,irc)
          call colocation_errorappend(crc250,"\n")
          return
       end if
    else
       irc=599
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"Attempt to close none-existent session.")
       call colocation_errorappend(crc250,"\n")
       return
    end if
    if(col_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine colocation_closeSession

  subroutine colocation_removeSession(css,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "colocation_removeSession"
    type(col_default), pointer :: cdef, cdefn
    type(col_match), pointer :: cmatch, cmatchn
    !
    ! remove default stack
    nullify(css%currentDef)
    if (associated(css%firstDef)) then
       cdef => css%firstDef%next
       do while (.not.associated(cdef,target=css%lastDef))
          cdefn => cdef%next
          call colocation_deleteDef(css,cdef,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,myname)
             call colocation_errorappend(crc250," Error return from deleteDef.")
             call colocation_errorappendi(crc250,irc)
             call colocation_errorappend(crc250,"\n")
             return
          end if
          cdef  => cdefn
       end do
       deallocate(css%firstDef,css%lastDef)
    end if
    !
    ! remove match stack
    if (associated(css%firstMatch)) then
       cmatch => css%firstMatch%next
       do while (.not.associated(cmatch,target=css%lastMatch))
          cmatchn => cmatch%next
          call colocation_deleteMatch(css,cmatch,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,myname)
             call colocation_errorappend(crc250," Error return from deleteMatch.")
             call colocation_errorappendi(crc250,irc)
             call colocation_errorappend(crc250,"\n")
             return
          end if
          cmatch  => cmatchn
       end do
       deallocate(css%firstMatch,css%lastMatch)
       nullify(css%currentMatch)
    end if
    !
    if (associated(css%trg80)) deallocate(css%trg80)
    if (associated(css%trg_lent)) deallocate(css%trg_lent)
    !
    css%prev%next => css%next
    css%next%prev => css%prev
    deallocate(css)
  end subroutine colocation_removeSession 
  !
  !
  !###############################################################################
  ! TARGET ROUTINES
  !###############################################################################
  !
  subroutine colocation_importTargets(css,mss,crc250,irc)
    use model
    implicit none
    type(col_session), pointer :: css !  current session
    type(mod_session), pointer :: mss !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "colocation_importTargets"
    integer :: ii
    if(associated(css%trg80)) deallocate(css%trg80)
    if(associated(css%trg_lent)) deallocate(css%trg_lent)
    css%ctrg=mss%ctrg
    if (css%ctrg.ne.0) then
       allocate(css%trg80(css%ctrg), css%trg_lent(css%ctrg),stat=irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Unable to allocate 'trg80'.")
          call colocation_errorappend(crc250,"\n")
          return
       end if
       do ii=1,mss%ctrg
          css%trg80(ii)=mss%trg80(ii)
       end do
    end if
    return
  end subroutine colocation_importTargets
  !
  !
  !###############################################################################
  ! DEFAULT ROUTINES
  !###############################################################################
  ! clear the default stack
  !
  subroutine colocation_cleardefaultstack(css,crc250,irc) 
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(col_default), pointer :: currentDefault !  the current default target
    type(col_default), pointer :: nextDef !  the next default target
    character*25 :: myname = "colocation_cleardefaultstack"
    currentDefault => css%firstDef%next
    do while (.not.associated(currentDefault,target=css%lastDef))
       nextDef => currentDefault%next
       currentDefault%prev%next =>  currentDefault%next
       currentDefault%next%prev =>  currentDefault%prev
       if (associated(currentDefault%vset)) deallocate(currentDefault%vset)
       if (associated(currentDefault%v80)) deallocate(currentDefault%v80)
       if (associated(currentDefault%vlen)) deallocate(currentDefault%vlen)
       if (associated(currentDefault%val)) deallocate(currentDefault%val)
       deallocate(currentDefault,stat=irc)
       css%ndef=css%ndef-1
       currentDefault => nextDef
    end do
  end subroutine colocation_cleardefaultstack
  !
  ! push default values to the stack
  !
  subroutine colocation_pushDefault(css,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(col_default), pointer :: newDefault
    character*25 :: myname = "colocation_pushDefault"
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    if (.not.associated(css%firstDef)) then
       allocate(css%firstDef,css%lastDef, stat=irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Unable to allocate 'firstDef/lastDef'.")
          call colocation_errorappend(crc250,"\n")
          return
       end if
       css%firstDef%next => css%lastDef
       css%lastDef%prev => css%firstDef
       css%ndef=0
    end if
    if (associated(css%currentDef)) then
       newDefault => css%currentDef
       css%ndef=css%ndef+1
       newDefault%prev => css%lastDef%prev
       newDefault%next => css%lastDef
       newDefault%prev%next => newDefault
       newDefault%next%prev => newDefault
       nullify(css%currentDef)
    end if
    if(col_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine colocation_pushDefault
  !
  logical function colocation_loopDefault(css,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*80  :: n80       ! target name
    character*80  :: v80       ! variable
    character*80  :: l80      ! min value
    character*80  :: u80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="loopDefault"
    colocation_loopdefault=.false. ! only true if all is ok...
    if (.not.associated(css%cdef)) then
       css%cdef =>  css%firstDef%next 
    else
       css%cdef =>  css%cdef%next
    end if
    if (associated(css%cdef,css%lastDef)) then
       nullify(css%cdef)
       colocation_loopdefault=.false.
    else
       colocation_loopdefault=.true.
    end if
    return
  end function colocation_loopdefault
  !
  logical function colocation_loopdefaultItem(css,n80,v80,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*80  :: n80       ! target name
    character*80  :: v80       ! variable
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="loopdefitem"
    colocation_loopdefaultitem=.false. ! only true if all is ok...
    if (css%cdef%cii.eq.0) then
       css%cdef%cii = 1
    else
       css%cdef%cii = css%cdef%cii + 1
    end if
    if (css%cdef%cii.gt.css%cdef%cDef) then
       css%cdef%cii = 0
       colocation_loopdefaultitem=.false.
    else
       n80=css%trg80(css%cdef%cii)
       v80=css%cdef%v80(css%cdef%cii)
       colocation_loopdefaultitem=.true.
    end if
    return
  end function colocation_loopdefaultItem
  !
  ! add default element
  !
  subroutine colocation_addDefault(css,n80,v80,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*80 :: n80 ! target name
    character*80 :: v80 ! target value
    character*250 :: crc250
    integer :: irc
    integer :: ii, irc2, lenv, lenn
    integer, external :: length
    character*25 :: myname = "colocation_addDefault"
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    if (css%ctrg.eq.0) then
       irc=347
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," No targets defined!")
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
    if(col_bdeb)write(*,*)myname,'Here.',associated(css%currentDef)
    if (.not.associated(css%currentDef)) then
       allocate(css%currentDef, stat=irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Unable to allocate 'firstItm/lastItm'.")
          call colocation_errorappend(crc250,"\n")
          return
       end if
       css%currentDef%cDef=css%ctrg
       allocate(css%currentDef%vset(css%currentDef%cDef), css%currentDef%v80(css%currentDef%cDef), &
            & css%currentDef%vlen(css%currentDef%cDef),  css%currentDef%val(css%currentDef%cDef), stat=irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Unable to allocate 'session: current Default'.")
          call colocation_errorappend(crc250,"\n")
          return
       end if
    end if
    call chop0(n80,80)
    lenn=length(n80,80,10)
    if(col_bdeb)write(*,*)myname,'Looking for target:',n80(1:lenn)
    ii=1
    SEEK:do while (ii.le.css%ctrg)
       if (css%trg80(ii)(1:css%trg_lent(ii)).eq.n80(1:lenn)) exit SEEK
       ii=ii+1
    end do SEEK
    if (ii.le.css%ctrg) then
       css%currentDef%vset(ii)=.true.
       call chop0(v80,80)
       lenv=length(v80,80,10)
       css%currentDef%v80(ii)=v80 ! value
       css%currentDef%vlen(ii)=lenv
       read(v80(1:lenv),*,iostat=irc2) css%currentDef%val(ii)
    else
       irc=220
       ! write(*,*)myname,'Targets:',css%ctrg,css%ctrg
       ! do ii=1,css%ctrg
       !    write(*,*)myname,'Target:',ii,css%trg80(ii)(1:css%trg_lent(ii))
       ! end do
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"Target not found:"//n80(1:lenn))
       return
    end if
    if(col_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine colocation_addDefault
  !
  ! retrieve next default values
  !
  subroutine colocation_getdefault(css,nvar,var,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    integer :: nvar
    real, allocatable :: var(:)
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "colocation_getdefault"
    integer :: ii
    integer, external :: length
    if (nvar.ne.css%ctrg.and.allocated(var)) deallocate(var)
    if (.not.allocated(var)) then
       allocate(var(css%ctrg),stat=irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250," Unable to allocate var.")
          call colocation_errorappendi(crc250,css%ctrg)
          return
       end if
    end if
    if (.not.associated(css%currentDef)) then
       css%currentDef => css%firstDef%next
    end if
    if (.not.associated(css%currentDef,target=css%lastDef)) then
       nvar=css%ctrg
       do ii=1,css%ctrg
          var(ii)=css%currentDef%val(ii)
       end do
       css%currentDef => css%currentDef%next
    else
       nvar=0
    end if
    return
  end subroutine colocation_getdefault
  !
  ! delete default from stack
  !
  subroutine colocation_deleteDef (css,def, crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    type(col_default), pointer :: def
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    character*25 :: myname = "colocation_deleteDef"
    if (associated(def)) then
       css%ndef = css%ndef - 1
       def%next%prev => def%prev
       def%prev%next => def%next
       if (associated(def%vset)) deallocate(def%vset)
       if (associated(def%v80)) deallocate(def%v80)
       if (associated(def%vlen)) deallocate(def%vlen)
       if (associated(def%val)) deallocate(def%val)
       deallocate(def)
    end if
    return
  end subroutine colocation_deleteDef
  !
  ! default locations
  !
  integer function colocation_defaultCount(css,crc250,irc)
    implicit none
    type(col_session), pointer :: css   ! session structure
    character*250 :: crc250
    integer :: irc
    character*22 :: myname = "colocation_defaultCount "
    colocation_defaultCount=css%ndef
    return
  end function colocation_defaultCount
  !
  !
  !###############################################################################
  ! MATCH ROUTINES
  !###############################################################################
  ! clear the match stack
  !
  subroutine colocation_clearmatchstack(css,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(col_match), pointer :: currentMatch => null() !  current session
    type(col_match), pointer :: nextMatch => null() !  current session
    character*25 :: myname = "colocation_clearmatchstack"
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    currentMatch => css%firstMatch%next
    do while (.not.associated(currentMatch,target=css%lastMatch))
       nextMatch => currentMatch%next
       currentMatch%prev%next =>  currentMatch%next
       currentMatch%next%prev =>  currentMatch%prev
       deallocate(currentMatch,stat=irc)
       css%nmatch=css%nmatch-1
       currentMatch => nextMatch
    end do
    if(col_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine colocation_clearmatchstack
  !
  ! add match to current
  !
  subroutine colocation_addmatch(css,n80,e250,l80,u80,crc250,irc)
    use parse
    implicit none
    type(col_session), pointer :: css !  current session
    character*80 :: n80
    character*250 :: e250
    character*80 :: l80
    character*80 :: u80
    character*250 :: crc250
    integer :: irc
    type(col_match), pointer :: match
    integer :: ii, irc2, lenv, lenn
    integer, external :: length
    character*25 :: myname = "colocation_addMatch"
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    if (css%ctrg.eq.0) then
       irc=347
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," No targets defined!")
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
    if (.not.associated(css%currentMatch)) then
       allocate(css%currentMatch, stat=irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Unable to allocate 'firstItm/lastItm'.")
          call colocation_errorappend(crc250,"\n")
          return
       end if
       css%currentMatch%cMatch=css%ctrg
       allocate(css%currentMatch%vset(css%currentMatch%cMatch), &
            & css%currentMatch%n80(css%currentMatch%cMatch), &
            & css%currentMatch%lenn(css%currentMatch%cMatch), &
            & css%currentMatch%e250(css%currentMatch%cMatch), &
            & css%currentMatch%lene(css%currentMatch%cMatch), &
            & css%currentMatch%l80(css%currentMatch%cMatch),  &
            & css%currentMatch%u80(css%currentMatch%cMatch), &
            & css%currentMatch%val(css%currentMatch%cMatch), &
            & css%currentMatch%psp(css%currentMatch%cMatch), stat=irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Unable to allocate 'session: current Match'.")
          call colocation_errorappend(crc250,"\n")
          return
       end if
       match => css%currentMatch
       do ii=1,match%cMatch
          match%lene(ii)=0
          match%val(ii)=0.0D0
          call parse_open(match%psp(ii)%ptr,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,myname)
             call colocation_errorappend(crc250,"Error return from 'parse_open'.")
             return
          end if
       end do
    end if
    call chop0(n80,80)
    lenn=length(n80,80,10)
    ii=1
    SEEK:do while (ii.le.css%ctrg)
       if (css%trg80(ii)(1:css%trg_lent(ii)).eq.n80(1:lenn)) exit SEEK
       ii=ii+1
    end do SEEK
    if (ii.le.css%ctrg) then
       css%currentMatch%vset(ii)=.true.
       css%currentMatch%n80(ii)=n80
       css%currentMatch%lenn(ii)=lenn
       css%currentMatch%e250(ii)=e250
       call chop0(css%currentMatch%e250(ii),250)
       css%currentMatch%lene(ii)=length(css%currentMatch%e250(ii),250,10)
       if(col_bdeb)write(*,*)myname,'Match.',css%currentMatch%e250(ii)(1:css%currentMatch%lene(ii))
       css%currentMatch%l80(ii)=l80
       css%currentMatch%u80(ii)=u80
       css%currentMatch%val(ii)=0.0D0
    else
       irc=221
       write(*,*)myname,'Targets:',css%ctrg,css%ctrg
       do ii=1,css%ctrg
          write(*,*)myname,'Target:',ii,css%trg80(ii)(1:css%trg_lent(ii))
       end do
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"Target not found:"//n80(1:lenn))
       return
    end if
    if(col_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine colocation_addmatch
  !
  logical function colocation_loopMatch(css,n80,e250,l80,u80,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*80 :: n80
    character*250 :: e250
    character*80 :: l80
    character*80 :: u80
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="loopmatch"
    if (col_bdeb) write(*,*)myname,'Entering.',associated(css%cmatch),irc
    colocation_loopmatch=.false. ! only true if all is ok...
    if (.not.associated(css%currentMatch)) return ! no data
    if (css%currentMatch%cii.eq.0) then
       css%currentMatch%cii = 1
    else
       css%currentMatch%cii = css%currentMatch%cii + 1
    end if
    if (css%currentMatch%cii.gt.css%currentMatch%cMatch) then
       css%currentMatch%cii = 0
       colocation_loopmatch=.false.
    else
       n80=css%currentMatch%n80(css%currentMatch%cii)
       e250=css%currentMatch%e250(css%currentMatch%cii)
       l80=css%currentMatch%l80(css%currentMatch%cii)
       u80=css%currentMatch%u80(css%currentMatch%cii)
       colocation_loopmatch=.true.
    end if
    if (col_bdeb) write(*,*)myname,'Exiting.',irc
    return
  end function colocation_loopMatch
  !
  ! push current match to the stack
  !
  subroutine colocation_pushMatch(css,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    type(col_match), pointer :: newMatch
    character*25 :: myname = "colocation_pushMatch"
    if(col_bdeb)write(*,*) myname,'Entering.'
    if (associated(css%currentMatch)) then
       newMatch => css%currentMatch
       css%nmatch=css%nmatch+1
       newMatch%prev => css%lastMatch%prev
       newMatch%next => css%lastMatch
       newMatch%prev%next => newMatch
       newMatch%next%prev => newMatch
       nullify(css%currentMatch)
    end if
    if(col_bdeb)write(*,*) myname,'Done.'
    return
  end subroutine colocation_pushMatch
  !
  ! delete match rule from stack
  !
  subroutine colocation_deleteMatch (css,match, crc250,irc)
    use parse
    implicit none
    type(col_session), pointer :: css !  current session
    type(col_match), pointer :: match
    character*250 :: crc250
    integer :: irc  ! error return code (0=ok)
    character*25 :: myname = "colocation_deleteMatch"
    integer :: ii
    if (associated(match)) then
       css%nmatch = css%nmatch - 1
       match%next%prev => match%prev
       match%prev%next => match%next
       if (associated(match%vset)) deallocate(match%vset)
       if (associated(match%n80)) deallocate(match%n80)
       if (associated(match%lenn)) deallocate(match%lenn)
       if (associated(match%e250)) deallocate(match%e250)
       if (associated(match%lene)) deallocate(match%lene)
       if (associated(match%l80)) deallocate(match%l80)
       if (associated(match%u80)) deallocate(match%u80)
       if (associated(match%val)) deallocate(match%val)
       if (associated(match%psp)) then
          do ii=1,match%cMatch
             call parse_close(match%psp(ii)%ptr,crc250,irc)
          end do
          deallocate(match%psp)
       end if
       match%cMatch=0
       deallocate(match)
    end if
    return
  end subroutine colocation_deleteMatch
  !
  ! get number of matchs
  !
  integer function colocation_matchCount(css,crc250,irc)
    implicit none
    type(col_session), pointer :: css   ! session structure
    character*250 :: crc250
    integer :: irc
    character*22 :: myname = "colocation_matchCount "
    if (associated(css%currentMatch)) then
       colocation_matchCount=css%currentMatch%cMatch
    else
       colocation_matchCount=0
    end if
    return
  end function colocation_matchCount
  !
  ! compile match
  !
  subroutine colocation_compileMatch(css,var80,crc250,irc)
    use parse
    implicit none
    type(col_session), pointer :: css !  current session
    character*80, allocatable :: var80(:) ! variables
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "colocation_compileMatch"
    type(col_match), pointer :: match
    integer :: lene
    integer :: ii ! match number
    if(col_bdeb)write(*,*)myname,'Entering.',ii,var80
    match => css%currentMatch
    if (associated(match)) then
       do ii=1,match%cmatch
          lene=match%lene(ii)
          if(col_bdeb)write(*,*)myname,'Calling parsef.'
          call parse_parsef(match%psp(ii)%ptr,match%e250(ii)(1:lene),var80,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,myname)
             call colocation_errorappend(crc250," Error return from parsef.")
             call colocation_errorappendi(crc250,irc)
             call colocation_errorappend(crc250,"\n")
             return
          end if
       end do
    else
       irc=342
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," No matchs specified.")
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
    return
  end subroutine colocation_compileMatch
  !
  ! evaluate match
  !
  subroutine colocation_evalMatch(css,val,crc250,irc)
    use parse
    implicit none
    type(col_session), pointer :: css !  current session
    real, allocatable :: val(:)    ! values
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "colocation_evalMatch"
    integer :: ii                     ! match number
    type(col_match), pointer :: match
    match => css%currentMatch
    if (associated(match)) then
       do ii=1,match%cmatch
          match%val(ii)=parse_evalf(match%psp(ii)%ptr,val)
       end do
    else
       irc=341
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," No matchs specified.")
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
    return
  end subroutine colocation_evalMatch
  !
  !
  !###############################################################################
  ! CACHE ROUTINES
  !###############################################################################
  ! clear the match stack
  !
  !
  subroutine colocation_setobscache(css,path250,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="setobscache"
    INTEGER                         :: nvar = 0
    !
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       css%obs250=path250
    end if
    !
  end subroutine colocation_setobscache
  !
  subroutine colocation_getobscache(css,path250,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="getobscache"
    INTEGER                         :: nvar = 0
    !
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       path250=css%obs250
    end if
    !
  end subroutine colocation_getobscache
  !
  subroutine colocation_setmodcache(css,path250,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="setmodcache"
    INTEGER                         :: nvar = 0
    !
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       css%mod250=path250
    end if
    !
  end subroutine colocation_setmodcache
  !
  subroutine colocation_getmodcache(css,path250,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: path250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="getmodcache"
    INTEGER                         :: nvar = 0
    !
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       path250=css%mod250
    end if
    !
  end subroutine colocation_getmodcache
  !
  !
  !###############################################################################
  ! FILTER/EXPRESSION ROUTINES
  !###############################################################################
  !
  !
  subroutine colocation_setfilter(css,filter250,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: filter250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="setfilter"
    INTEGER                         :: nvar = 0
    !
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       css%filter250=filter250
    end if
    if(col_bdeb)write(*,*)myname,'Exiting.',irc
    !
  end subroutine colocation_setfilter
  !
  subroutine colocation_getfilter(css,filter250,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: filter250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="getfilter"
    INTEGER                         :: nvar = 0
    !
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       filter250=css%filter250
    end if
    if(col_bdeb)write(*,*)myname,'Exiting.',irc
    !
  end subroutine colocation_getfilter
  !
  subroutine colocation_expression(exp250,crc250,irc)
    use parse
    character*250 :: exp250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="expression"
    INTEGER                         :: nvar = 0
    CHARACTER (LEN=80), allocatable :: var(:)
    REAL(rn),           allocatable :: val(:)
    REAL(rn)                                       :: res
    INTEGER                                        :: i
    REAL(rn)                                       :: a
    type(parse_session),pointer :: pss => null()
    integer :: lene
    integer, external :: length
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    !
    if (col_bdeb) write(*,*) myname,'Entering.',irc
    call chop0(exp250,250)
    lene=length(exp250,250,10)
    if (col_bdeb) write(*,*) myname,'Init.'
    call parse_open (pss,crc250,irc) ! open parse session
    if(irc.ne.0) return
    if (col_bdeb) write(*,*) myname,'Parse.',exp250(1:lene)
    call parse_parsef (pss, exp250(1:lene), var,crc250,irc)        ! parse and bytecompile ith function string 
    if(irc.ne.0) return
    if (col_bdeb) write(*,*) myname,'Eval.', val
    res = parse_evalf (pss, val)                 ! interprete bytecode representation of ith function
    write(exp250,*) res
    call chop0(exp250,250)
    if (col_bdeb) write(*,*) myname,'Done.',irc,res
    call parse_close (pss,crc250,irc) ! open parse session
    if(irc.ne.0) return
    return
  end subroutine colocation_expression
  !
  !
  !###############################################################################
  ! OUTPUT-XML ROUTINES
  !###############################################################################
  ! colocate data and write to xml/table file
  !
  subroutine colocation_makeOutput(css,mss,oss,nam250,x250,y250,leg250,crc250,irc)
    use model
    use observations
    use parse
    implicit none
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    character*250 :: nam250,x250,y250,leg250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="makeoutput"
    integer :: nvar
    character*80, allocatable :: var80(:)
    real, allocatable :: val(:)
    integer :: mloc,mtrg,oloc,otrg
    integer :: lenc,lene,lenn,lenl
    integer, external :: length
    real :: valx, valy
    integer :: tmod,emod,dmod,tobs,ii,jj,ind_ii,nfunc
    logical :: bobsind
    type(parse_session), pointer :: psx,psy,pse ! parse sessions
    type(parse_pointer), pointer :: psp(:) => null()! parse sessions
    integer :: locid,locstart
    !
    real :: mod_start = 0.0D0
    real :: mod_stop = 0.0D0
    real :: obs_start = 0.0D0
    real :: obs_stop = 0.0D0
    logical :: mod_lim,obs_lim,bok,first,lok
    ! check what we should plot
       bok=.true.
    tmod=model_targetCount(mss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_targetCount")
       return
    end if
    emod=colocation_matchCount(css,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_matchCount")
       return
    end if
    dmod=colocation_defaultCount(css,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_defaultCount")
       return
    end if
    tobs=observation_targetCount(oss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"observation_targetCount")
       return
    end if
    if (tmod.ne.0.and.tobs.ne.0.and.(emod.ne.tmod.and.dmod.eq.0)) then
       irc=232
       call colocation_errorappend(crc250,"Missing model target matchs, expected ")
       call colocation_errorappendi(crc250,tmod)
       call colocation_errorappend(crc250," got ")
       call colocation_errorappendi(crc250,emod)
       return
    else if (tmod.ne.0.and.tobs.eq.0.and.(emod.eq.0.and.dmod.eq.0)) then
       irc=233
       call colocation_errorappend(crc250,"Missing model default values.")
       return
    end if
    !
    ! make target lists
    if (tmod.eq.0) then
       bobsind=.false.
    else
       bobsind=observation_hasValidIndex(oss,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"observation_hasValidIndex")
          return
       end if
    end if
    ! make expression lists
    ! count expressions (match-expressions + obs-index-expression)
    allocate(pse,psp(emod),stat=irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"allocate")
       return
    end if
    call parse_open (pse,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"parse_open")
       return
    end if
    do ii=1,emod
       call parse_open(psp(ii)%ptr,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"parse_open")
          return
       end if
    end do
    ! compile all expressions
    call model_makeTargetList(mss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_makeTargetList")
       return
    end if
    call observation_makeTargetList(oss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"observation_makeTargetList")
       return
    end if
    ind_ii=0  ! mark expression that corresponds to index variable
    if (bobsind) then ! obs-index-expression
       lene=length(oss%ind_exp250,250,10)
       call parse_parsef(pse, oss%ind_exp250(1:lene), oss%trg80, crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"parse_parsef")
          return
       end if
    end if
    if(col_bdeb)write(*,*)myname,'Processing expressions.',emod,associated(css%cmatch)
    do ii=1,emod ! match-expressions + obs-index-expression
       call parse_parsef(psp(ii)%ptr, css%cmatch%e250(ii)(1:css%cmatch%lene(ii)), &
            & oss%trg80, crc250,irc)
       if (irc.ne.0) then
          ! write(*,*) myname,'Expr:', ii, css%cmatch%lene(ii),&
          !      &  css%cmatch%ntrgmatch,  css%cmatch%e250(ii)(1:css%cmatch%lene(ii))
          ! 
          ! do jj=1,size(oss%trg80)
          !    write(*,*) myname,'var:',jj,oss%ntrg,oss%ntarget,oss%trg80(jj)(1:oss%trgmodlent(jj))
          ! end do
          call colocation_errorappend(crc250,"parse_parsef_loop")
          return
       end if
       if (css%cmatch%n80(ii)(1:css%cmatch%lenn(ii)).eq.mss%ind_var80(1:mss%ind_lenv)) then
          ind_ii=ii
       end if
    end do
    if(col_bdeb)write(*,*)myname,'Calculating limits.'
    !
    ! convert obs start/end limits (time) to model start/end limits if possible
    if(col_bdeb)write(*,*)myname,'Setting limits.',mss%ind_lim,mss%ind_start,&
         & mss%ind_stop,oss%ind_lim(3),oss%ind_start,oss%ind_stop
    mod_lim=.false. ! are model limits available?
    if (oss%ind_lim(3) .and. ind_ii.ne.0) then ! convert obs_limits to mod_limits
       oss%trg_val(oss%ntrg)=oss%ind_start
       mod_start=parse_evalf(psp(ind_ii)%ptr,oss%trg_val)
       oss%trg_val(oss%ntrg)=oss%ind_stop
       mod_stop=parse_evalf(psp(ind_ii)%ptr,oss%trg_val)
       mod_lim=.true.
    end if
    if (mss%ind_lim) then
       if (mod_lim) then
          mod_start=max(mod_start,mss%ind_start)
          mod_stop=min(mod_stop,mss%ind_stop)
       else
          mod_start=mss%ind_start
          mod_stop=mss%ind_stop
       end if
       mod_lim=.true.
    end if
    ! initial observation limits are for the whole index range
    if (mod_lim) then
       obs_lim=mod_lim
       obs_start=mod_start
       obs_stop=mod_stop
    else
       obs_lim=oss%ind_lim(3)
       obs_start=oss%ind_start
       obs_stop=oss%ind_stop
    end if
    if(col_bdeb)write(*,*)myname,'Entering model file loop.',mod_lim,mod_start,mod_stop,obs_lim,obs_start,obs_stop
    locid=0 ! observation count (= identification)
    MODFILE: do
       if (tmod.ne.0) then ! we have model targets specified
          ! loop over data
          bok=.true.
          if(col_bdeb)write(*,*)myname,'Calling model nextfile.',mod_lim,mod_start,mod_stop
          call model_getnextfile(mss,mod_lim,mod_start,mod_stop,bok,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,"model_getnextfile")
             return
          end if
          if (.not.bok) then
             if(col_bdeb)write(*,*)myname,'No more model files.'
             exit MODFILE ! no more files to process
          else 
             if(col_bdeb)write(*,*)myname,'Found model file.'
          end if
          !
          ! write file opening xml-tag
          !
          call model_filestartxml(mss,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,"model_fileStartXml")
             return
          end if
          ! get observation file limits    
          if (mss%currentFile%ind_lim) then
             obs_lim=mss%currentFile%ind_lim ! are observation limits available?
             if (mod_lim) then
                obs_start=max(mod_start,mss%currentFile%ind_start)
                obs_stop=min(mod_stop,mss%currentFile%ind_stop)
             else
                obs_start=mss%currentFile%ind_start
                obs_stop=mss%currentFile%ind_stop
             end if
          end if
       end if
       !
       if(col_bdeb)write(*,*)myname,'Entering obs file loop.'

       ! loop over model data, using model/obs start/end limits
       OBSFILE : do
          if (tobs.ne.0) then ! we have observation targets available
             bok=.true.
             if(col_bdeb)write(*,*)myname,'Calling observation nextfile.',obs_lim,obs_start,obs_stop
             call observation_getnextfile(oss,obs_lim,obs_start,obs_stop,bok,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"observation_getnextfile")
                return
             end if
             if (.not.bok) then
                if(col_bdeb)write(*,*)myname,'No more observation files to process.',obs_lim,obs_start,obs_stop
                exit OBSFILE ! no more files to process
             end if
             !
             ! write file opening xml-tag
             !
             call observation_filestartxml(oss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"observation_fileStartXml")
                return
             end if
          end if
          if (tobs.ne.0.and.tmod.ne.0) then ! we have observation targets available
             ! initialise the location list
             if(col_bdeb)write(*,*)myname,'Clear model locations.'
             call observation_locclear(oss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"observation_locclear")
                return
             end if
             call model_locclear(mss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_locclear")
                return
             end if
             call model_sliceTarget(mss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_slicetarget")
                return
             end if

             if(col_bdeb)write(*,*)myname,'Compile expressions.',associated(css%cmatch)
             if (associated(css%cmatch)) then
                ! compile match-experssions
                call colocation_compileMatch(css,oss%trg80,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_compileMatch")
                   return
                end if
             else
                irc=123
                call colocation_errorappend(crc250,"No matchs available")
                return
             end if
             if(col_bdeb)write(*,*)myname,'Entering observation loop.'

             locstart=locid
             ! loop over obs data, using model start/end limits
             LOCATION : do
                if(col_bdeb)write(*,*)myname,'Slice observation file.'
                ! read next observation into static BUFR memory, evaluate expressions...
                call observation_sliceCurrentFile(oss,bok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"observation_sliceCurrentFile")
                   return
                end if
                if (.not.bok) then
                   if(col_bdeb)write(*,*)myname,'No more observations to process.'
                   exit LOCATION
                end if
                !
                locid=locid+1
                !
                !write(*,*)myname,'Evaluate expressions.'
                ! evaluate experessions
                call colocation_evalMatch(css,oss%trg_val,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"colocation_evalMatch")
                   return
                end if

                ! make target values
                !write(*,*)myname,'Set model targets.'
                call  model_setTargetVal(mss,css%currentMatch%cMatch,css%currentMatch%val,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_setTargetVal")
                   return
                end if

                ! check target values
                lok=.true.
                call  model_checkTargetVal(mss,lok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_setTargetVal")
                   return
                end if
                !
                ! make new location from observation
                if(col_bdeb)write(*,*)myname,'Push location.',locid
                call observation_locpushtarget(oss,locid,lok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"observation_locPush")
                   return
                end if
                call model_locpushtarget(mss,locid,lok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_locPush")
                   return
                end if
             end do LOCATION
          else if (tobs.eq.0.and.tmod.ne.0) then ! use model default values

             if(col_bdeb)write(*,*)myname,'Clearing loc stack.',mss%ctrg,associated(mss%trg_v80)

             ! initialise the location list
             call model_locclear(mss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_locclear")
                return
             end if
             call model_sliceTarget(mss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_sliceTarget")
                return
             end if

             if(col_bdeb)write(*,*)myname,'Creating locations from default.',associated(css%firstDef)
             if(col_bdeb)write(*,*)myname,'...:',associated(css%firstDef%next)
             css%currentDef=>css%firstDef%next
             do while (.not.associated(css%currentDef,target=css%lastDef))
                if(col_bdeb)write(*,*)myname,'Make target values from default.'
                ! make target values
                call  model_setTargetVal(mss,css%currentDef%cDef,css%currentDef%val,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_setTargetVal")
                   return
                end if
                ! make new location from observation
                locid=locid+1
                ! check target values
                lok=.true.
                call  model_checkTargetVal(mss,lok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_setTargetVal")
                   return
                end if
                !
                if(col_bdeb)write(*,*)myname,'Creating location:',locid
                call model_locpushtarget(mss,locid,lok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_locPush")
                   return
                end if
                css%currentDef=>css%currentDef%next
             end do
          end if
          if (tmod.ne.0) then
             ! finally slice the model file and write model XML to stdout
             call model_slicecurrentfile(mss,bok,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_stackslicecurrentfile")
                return
             end if
             ! make observation location-arrays
             call observation_makeLocList(oss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"makeloclist")
                return
             end if
             ! make variable arrays
             mloc = model_locationCount(mss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,myname)
                call colocation_errorappend(crc250,"Error return from 'locationcount'.")
                return
             end if
             mtrg = model_trgCount(mss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,myname)
                call colocation_errorappend(crc250,"Error return from 'trgcount'.")
                return
             end if
             oloc=observation_locationCount(oss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,myname)
                call colocation_errorappend(crc250,"Error return from 'locationcount'.")
                return
             end if
             otrg=observation_trgCount(oss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,myname)
                call colocation_errorappend(crc250,"Error return from 'trgcount'.")
                return
             end if
             nvar=mtrg+otrg
             if (allocated(val)) deallocate(val)
             if (allocated(var80)) deallocate(var80)
             allocate(var80(nvar),val(nvar),stat=irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,myname)
                call colocation_errorappend(crc250,"Unable to allocate 'var'.")
                return
             end if
             call model_getTrg80(mss,var80,0,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"mod_getTrg80")
                return
             end if
             call observation_getTrg80(oss,var80,mtrg,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"mod_getTrg80")
                return
             end if
             ! open expression
             call parse_open (psx,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"parse_open")
                return
             end if
             call parse_open (psy,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"parse_open")
                return
             end if
             ! compile expressions
             call parse_parsef(psx,x250,var80, crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"parse_parsef")
                return
             end if
             call parse_parsef(psy,y250,var80, crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"parse_parsef")
                return
             end if
             ! loop over output data
             if (css%tunit.ne.0) then
                call chop0(nam250,250)
                lenn=length(nam250,250,1)
                do ii=1,mloc
                   call model_getVal(mss,ii,val,0,crc250,irc)
                   if (irc.ne.0) then
                      call colocation_errorappend(crc250,"mod_getTrg80")
                      return
                   end if
                   call observation_getVal(oss,ii,val,mtrg,crc250,irc)
                   if (irc.ne.0) then
                      call colocation_errorappend(crc250,"obs_getTrg80")
                      return
                   end if
                   ! evaluate expressions
                   valx=parse_evalf(psx,val)
                   valy=parse_evalf(psy,val)
                   ! store x and y in output file
                   write(css%tunit,'(2X,A,X,F27.15,X,F27.15)')nam250(1:lenn),valx,valy
                end do
             end if
          end if
          !
          ! loop over observations, write valid observations to XML...
          !         
          if (tobs.ne.0) then
             first=.true.
             locid=locstart
             OBSERVATION : do
                ! read next observation into static BUFR memory
                call observation_sliceCurrentFile(oss,bok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"observation_sliceCurrentFile")
                   return
                end if
                if (.not.bok) then
                   if(col_bdeb)write(*,*)myname,'No more observations to process.'
                   exit OBSERVATION
                end if
                !
                locid=locid+1
                bok=.true.
                if (tmod.ne.0) then ! we have match expressions specified
                   call model_locSearchOk(mss,locid,bok)        
                   if (bok) then
                      oss%currentFile%ook(5)=oss%currentFile%ook(5)+1
                   else
                      oss%currentFile%orm(5)=oss%currentFile%orm(5)+1
                   end if
                else
                   oss%currentFile%ook(5)=oss%currentFile%ook(5)+1
                end if
                !
                if (bok) then
                   if (first) then
                      call observation_obsstartxml(oss,crc250,irc)
                      if (irc.ne.0) then
                         call colocation_errorappend(crc250,"observation_obsStartXml")
                         return
                      end if
                      first=.false.
                   end if
                   call observation_writeoutput(oss,locid,crc250,irc)
                   if (irc.ne.0) then
                      call colocation_errorappend(crc250,"observation_writeoutput")
                      return
                   end if
                end if
             end do OBSERVATION
             if (.not.first) then
                call observation_obsstopxml(oss,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"observation_obsstopXml")
                   return
                end if
             end if
          end if
          ! end obs data loop
          if (tobs.ne.0) then
             call observation_filestopxml(oss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"observation_fileStopxml")
                return
             end if
          else 
             exit OBSFILE
          end if
       end do OBSFILE

       ! end model loop
       if (tmod.ne.0)  then
          call model_writeSummary(mss,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,"model_writeSummary")
             return
          end if
          ! write file opening xml-tag
          call model_filestopxml(mss,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,"model_fileStopxml")
             return
          end if
       else 
          exit MODFILE
       end if
    end do MODFILE
    !
    call parse_close (pse,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"parse_close")
       return
    end if
    !
    do ii=1,emod
       call parse_close (psp(ii)%ptr,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"parse_close_loop")
          return
       end if
    end do
    if (associated(psp)) deallocate(psp)
    if (allocated(var80)) deallocate(var80)
    if (allocated(val)) deallocate(val)
    !write(*,*) myname,'Done.'
    return
  end subroutine colocation_makeOutput
  !
  subroutine colocation_makeOutput2(css,mss,oss,crc250, irc)
    use model
    use observations
    use parse
    implicit none
    type(col_session), pointer :: css !  current session
    type(mod_session), pointer :: mss !  current session
    type(obs_session), pointer :: oss !  current session
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    integer :: lenc,lene
    character*25 :: myname = "colocation_makeOutput2"
    integer :: tmod,emod,dmod,tobs,ii,jj,ind_ii,nfunc
    logical :: bobsind
    type(parse_session), pointer :: pse
    type(parse_pointer), pointer :: pss(:) ! parse pointer
    integer :: locid,locstart
    !
    real :: mod_start = 0.0D0
    real :: mod_stop = 0.0D0
    real :: obs_start = 0.0D0
    real :: obs_stop = 0.0D0
    logical :: mod_lim,obs_lim,bok,first,lok
    !
    irc=0
    bok=.true.
    ! check what we should colocated
    tmod=model_targetCount(mss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_targetCount")
       return
    end if
    tobs=observation_targetCount(oss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"observation_targetCount")
       return
    end if
    emod=colocation_matchCount(css,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"colocation_matchCount")
       return
    end if
    dmod=colocation_defaultCount(css,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"colocation_defaultCount")
       return
    end if
    if (tmod.ne.0.and.tobs.ne.0.and.(emod.ne.tmod.and.dmod.eq.0)) then
       irc=232
       call colocation_errorappend(crc250,"Missing model target matchs, expected ")
       call colocation_errorappendi(crc250,tmod)
       call colocation_errorappend(crc250," got ")
       call colocation_errorappendi(crc250,emod)
       return
    else if (tmod.ne.0.and.tobs.eq.0.and.(emod.eq.0.and.dmod.eq.0)) then
       irc=233
       call colocation_errorappend(crc250,"Missing model default values.")
       return
    end if
    !
    ! make target lists
    if (tmod.eq.0) then
       bobsind=.false.
    else
       bobsind=observation_hasValidIndex(oss,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"observation_hasValidIndex")
          return
       end if
    end if
    ! make expression lists
    ! count expressions (match-expressions + obs-index-expression)
    allocate(pse,pss(emod),stat=irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"allocate")
       return
    end if
    call parse_open (pse,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"parse_open")
       return
    end if
    do ii=1,emod
       call parse_open (pss(ii)%ptr,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"parse_open")
          return
       end if
    end do
    ! compile all expressions
    call model_makeTargetList(mss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_makeTargetList")
       return
    end if
    call observation_makeTargetList(oss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"observation_makeTargetList")
       return
    end if
    call colocation_importTargets(css,mss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"colocation_importTargets")
       return
    end if
    ind_ii=0  ! mark expression that corresponds to index variable
    if (bobsind) then ! obs-index-expression
       lene=length(oss%ind_exp250,250,10)
       call parse_parsef(pse, oss%ind_exp250(1:lene), oss%trg80, crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"parse_parsef")
          return
       end if
    end if
    if(col_bdeb)write(*,*)myname,'Processing expressions.',emod,associated(css%currentMatch)
    do ii=1,emod ! match-expressions + obs-index-expression
       call parse_parsef(pss(ii)%ptr, css%currentMatch%e250(ii)(1:css%currentMatch%lene(ii)), &
            & oss%trg80, crc250,irc)
       if (irc.ne.0) then
          ! write(*,*) myname,'Expr:', ii, css%currentMatch%lene(ii),&
          !      &  css%currentMatch%cmatch,  css%currentMatch%e250(ii)(1:css%currentMatch%lene(ii))
          ! 
          ! do jj=1,size(oss%trg80)
          !    write(*,*) myname,'var:',jj,oss%ntrg,oss%ctrg,oss%trg80(jj)(1:oss%trg_lent(jj))
          ! end do
          call colocation_errorappend(crc250,"parse_parsef_loop")
          return
       end if
       if (css%currentMatch%n80(ii)(1:css%currentMatch%lenn(ii)).eq.mss%ind_var80(1:mss%ind_lenv)) then
          ind_ii=ii
       end if
    end do
    if(col_bdeb)write(*,*)myname,'Calculating limits.'
    !
    ! convert obs start/end limits (time) to model start/end limits if possible
    if(col_bdeb)write(*,*)myname,'Setting limits.',mss%ind_lim,mss%ind_start,mss%ind_stop,oss%ind_lim(3),oss%ind_start,oss%ind_stop
    mod_lim=.false. ! are model limits available?
    if (oss%ind_lim(3) .and. ind_ii.ne.0) then ! convert obs_limits to mod_limits
       oss%trg_val(oss%ntrg)=oss%ind_start
       mod_start=parse_evalf(pss(ind_ii)%ptr,oss%trg_val)
       oss%trg_val(oss%ntrg)=oss%ind_stop
       mod_stop=parse_evalf(pss(ind_ii)%ptr,oss%trg_val)
       mod_lim=.true.
    end if
    if (mss%ind_lim) then
       if (mod_lim) then
          mod_start=max(mod_start,mss%ind_start)
          mod_stop=min(mod_stop,mss%ind_stop)
       else
          mod_start=mss%ind_start
          mod_stop=mss%ind_stop
       end if
       mod_lim=.true.
    end if
    ! initial observation limits are for the whole index range
    if (mod_lim) then
       obs_lim=mod_lim
       obs_start=mod_start
       obs_stop=mod_stop
    else
       obs_lim=oss%ind_lim(3)
       obs_start=oss%ind_start
       obs_stop=oss%ind_stop
    end if
    if(col_bdeb)write(*,*)myname,'Entering model file loop.',mod_lim,mod_start,mod_stop,obs_lim,obs_start,obs_stop
    locid=0 ! observation count (= identification)
    MODFILE: do
       if (tmod.ne.0) then ! we have model targets specified
          ! loop over data
          bok=.true.
          if(col_bdeb)write(*,*)myname,'Calling model nextfile.',mod_lim,mod_start,mod_stop
          call model_getnextfile(mss,mod_lim,mod_start,mod_stop,bok,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,"model_getnextfile")
             return
          end if
          if (.not.bok) then
             if(col_bdeb)write(*,*)myname,'No more model files.'
             exit MODFILE ! no more files to process
          else 
             if(col_bdeb)write(*,*)myname,'Found model file.'
          end if
          !
          ! write file opening xml-tag
          !
          call model_filestartxml(mss,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,"model_filestartxml")
             return
          end if
          ! get observation file limits    
          if (mss%currentFile%ind_lim) then
             obs_lim=mss%currentFile%ind_lim ! are observation limits available?
             if (mod_lim) then
                obs_start=max(mod_start,mss%currentFile%ind_start)
                obs_stop=min(mod_stop,mss%currentFile%ind_stop)
             else
                obs_start=mss%currentFile%ind_start
                obs_stop=mss%currentFile%ind_stop
             end if
          end if
       end if
       !
       if(col_bdeb)write(*,*)myname,'Entering obs file loop.'

       ! loop over model data, using model/obs start/end limits
       OBSFILE : do
          if (tobs.ne.0) then ! we have observation targets available
             bok=.true.
             if(col_bdeb)write(*,*)myname,'Calling observation nextfile.',obs_lim,obs_start,obs_stop
             call observation_getnextfile(oss,obs_lim,obs_start,obs_stop,bok,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"observation_getnextfile")
                return
             end if
             if (.not.bok) then
                if(col_bdeb)write(*,*)myname,'No more observation files to process.',obs_lim,obs_start,obs_stop
                exit OBSFILE ! no more files to process
             end if
             !
             ! write file opening xml-tag
             !
             call observation_filestartxml(oss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"observation_filestartxml")
                return
             end if
          end if
          if (tobs.ne.0.and.tmod.ne.0) then ! we have observation targets available
             ! initialise the location list
             if(col_bdeb)write(*,*)myname,'Clear model locations.'
             call model_locclear(mss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_locclear")
                return
             end if
             call model_sliceTarget(mss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_sliceTarget")
                return
             end if

             if(col_bdeb)write(*,*)myname,'Compile expressions.',associated(css%currentMatch)
             if (associated(css%currentMatch)) then
                ! compile match-experssions
                call colocation_compileMatch(css,oss%trg80,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_compileMatch")
                   return
                end if
             else
                irc=123
                call colocation_errorappend(crc250,"No expressions available")
                return
             end if
             if(col_bdeb)write(*,*)myname,'Entering observation loop.'

             locstart=locid
             ! loop over obs data, using model start/end limits
             LOCATION : do
                if(col_bdeb)write(*,*)myname,'Slice observation file.'
                ! read next observation into static BUFR memory and set oss%trg_val

                
                !XXXXXXXXXXXXXXXXXXXX make sure obs-target order is never changed internally... (in client)
                !XXXXXXXXXXXXXXXXXXXX pass "obsfilter" to sliceCurrentFile...
                !XXXXXXXXXXXXXXXXXXXX ...functions: "member(trg,1,2,3...)" "closest(trg,value,min,max)" etc.

                call observation_sliceCurrentFile(oss,bok,crc250,irc) !XXXXXXXXXXxx start loop over location-sets (delayedReplicator)



                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"observation_sliceCurrentFile")
                   return
                end if
                if (.not.bok) then
                   if(col_bdeb)write(*,*)myname,'No more observations to process.'
                   exit LOCATION
                end if
                locid=locid+1
                !
                !write(*,*)myname,'Evaluate expressions.'
                ! evaluate experessions
                call colocation_evalMatch(css,oss%trg_val,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_evalMatch")
                   return
                end if
                !
                ! make target values
                !write(*,*)myname,'Set model targets.'
                call  model_setTargetVal(mss,css%currentMatch%cMatch,css%currentMatch%val,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_setTargetVal")
                   return
                end if

                ! check target values
                lok=.true.
                call  model_checkTargetVal(mss,lok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_checkTargetVal")
                   return
                end if
                !
                ! make new location from observation
                if(col_bdeb)write(*,*)myname,'Push location.',locid
                call model_locpushtarget(mss,locid,lok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_locPush")
                   return
                end if
                !XXXXXXXXXXXXXXXXX end loop over location-sets (delayedReplicator)
             end do LOCATION
          else if (tmod.ne.0) then ! use model default values

             if(col_bdeb)write(*,*)myname,'Clearing loc stack.',mss%ctrg,associated(mss%trg_v80)

             ! initialise the location list
             call model_locclear(mss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_locclear")
                return
             end if
             call model_sliceTarget(mss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_sliceTarget")
                return
             end if
             if(col_bdeb)write(*,*)myname,'Creating locations from default.',associated(css%firstDef)
             if(col_bdeb)write(*,*)myname,'...:',associated(css%firstDef%next)
             css%currentDef=>css%firstDef%next
             do while (.not.associated(css%currentDef,target=css%lastDef))
                if(col_bdeb)write(*,*)myname,'Make target values from default.'
                ! make target values
                call  model_setTargetVal(mss,css%currentDef%cdef,css%currentDef%val,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_setTargetVal")
                   return
                end if
                ! make new location from observation
                locid=locid+1
                ! check target values
                lok=.true.
                call  model_checkTargetVal(mss,lok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_checkTargetVal")
                   return
                end if
                !
                if(col_bdeb)write(*,*)myname,'Creating location:',locid
                call model_locpushtarget(mss,locid,lok,crc250,irc) !XXXXXXXXXXx use match-variables
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_locPush")
                   return
                end if
                css%currentDef=>css%currentDef%next
             end do
          end if
          if (tmod.ne.0) then
             ! finally slice the model file and write model XML to stdout
             call model_slicecurrentfile(mss,bok,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_stackslicecurrentfile")
                return
             end if

             !
             !XXXXXXXXXXXXXXXXXXXXX check colfilter value here
             !

             !XXXXXXXXXXXXXXXX write output to table-file...



          end if
          !
          ! loop over observations again, write valid observations to XML...
          !         
          if (tobs.ne.0) then
             first=.true.
             locid=locstart
             OBSERVATION : do
                ! read next observation into static BUFR memory
                call observation_sliceCurrentFile(oss,bok,crc250,irc) !XXXXXXXXXXXXXXXXXX location-set loop
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"observation_sliceCurrentFile")
                   return
                end if
                if (.not.bok) then
                   if(col_bdeb)write(*,*)myname,'No more observations to process.'
                   exit OBSERVATION
                end if
                !
                locid=locid+1
                bok=.true.
                if (tmod.ne.0) then ! we have match expressions specified
                   call model_locSearchOk(mss,locid,bok)        
                   if (bok) then
                      oss%currentFile%ook(5)=oss%currentFile%ook(5)+1
                   else
                      oss%currentFile%orm(5)=oss%currentFile%orm(5)+1
                   end if
                else
                   oss%currentFile%ook(5)=oss%currentFile%ook(5)+1
                end if
                !
                if (bok) then
                   if (first) then
                      call observation_obsstartxml(oss,crc250,irc)
                      if (irc.ne.0) then
                         call colocation_errorappend(crc250,"observation_obsStartxml")
                         return
                      end if
                      first=.false.
                   end if
                   call observation_writeOutput(oss,locid,crc250,irc)
                   if (irc.ne.0) then
                      call colocation_errorappend(crc250,"observation_writeOutput")
                      return
                   end if
                end if
             end do OBSERVATION
             if (.not.first) then
                call observation_obsstopXml(oss,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"observation_obsstopxml")
                   return
                end if
             end if
          end if
          ! end obs data loop
          if (tobs.ne.0) then
             call observation_filestopxml(oss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"observation_filestopxml")
                return
             end if
          else 
             exit OBSFILE
          end if
       end do OBSFILE

       ! end model loop
       if (tmod.ne.0)  then
          call model_writeSummary(mss,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,"model_writeSummary")
             return
          end if
          ! write file opening xml-tag
          call model_filestopxml(mss,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,"model_filestopxml")
             return
          end if
       else 
          exit MODFILE
       end if
    end do MODFILE
    !
    call parse_close (pse,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"parse_close")
       return
    end if
    !
    do ii=1,emod
       call parse_close (pss(ii)%ptr,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"parse_close_loop")
          return
       end if
    end do

    deallocate(pss,stat=irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"deallocate")
       return
    end if
    !write(*,*) myname,'Done.'
    return
  end subroutine colocation_makeOutput2
  !
  !
  !###############################################################################
  ! ERROR ROUTINES
  !###############################################################################
  !
  !
  subroutine colocation_errorappend(crc250,string)
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
  end subroutine colocation_errorappend
  subroutine colocation_errorappendi(crc250,inum)
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
  end subroutine colocation_errorappendi
  !
  !
  !###############################################################################
  ! SORTING ROUTINES
  !###############################################################################
  !
  !
  subroutine colocation_heapsearch1r(maxnn,key,eps,nn,ind,tkey,left,right)
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
       kfl=colocation_cmpr(tkey,key(ind(mfl)),eps)
       kcl=colocation_cmpr(tkey,key(ind(mcl)),eps)
       !write(*,'(X,A,X,I3,F9.2,5(X,I3),3(X,F9.2),2(X,I5))')'colocation_heapsearch:',left,mid,right,mfl,mcl,kfl,kcl,&
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
       mch=colocation_cmpr(tkey, key(ind(left-1)),eps)
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
       mch=colocation_cmpr(tkey, key(ind(right+1)),eps)
       if (mch == 0) then ! equal or target is above
          right=right+1
          bdone=(right>nn-1)
       else
          bdone=.true.
       end if
    end do
    !
  end subroutine colocation_heapsearch1r
  !
  subroutine colocation_heapsort1r(mm,key1,eps,newnn,nn,ind,uniq)
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
       call colocation_pushdownr(ii, nn, mm,key1,eps,newnn,nn,ind)
    end do
    do ii = nn, 2, -1
       call colocation_swap(ind(1), ind(ii))
       call colocation_pushdownr(1, ii-1, mm,key1,eps,newnn,nn,ind)
    end do
    !
    if (uniq) then
       dmp=0
       newnn=1
       do ii=2,nn
          if (colocation_cmpr(key1(ind(ii-1)),key1(ind(ii)),eps) /= 0) then
             ! Keep ind(ii)
             newnn = newnn+1
             ind(newnn) = ind(ii)
          else
             dmp=dmp+1
          end if
       end do
       if(col_bdeb)write(*,*) "COLOCATION_HEAPSORT dumped elements:",dmp
    else
       newnn=nn
    end if
    !
    !
  end subroutine colocation_heapsort1r
  !
  subroutine colocation_heapsort1i(mm,key1,newnn,nn,ind,uniq)
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
       call colocation_pushdowni(ii, nn, mm,key1,newnn,nn,ind)
    end do
    do ii = nn, 2, -1
       call colocation_swap(ind(1), ind(ii))
       call colocation_pushdowni(1, ii-1, mm,key1,newnn,nn,ind)
    end do
    !
    if (uniq) then
       dmp=0
       newnn=1
       do ii=2,nn
          if (colocation_cmpi(key1(ind(ii-1)),key1(ind(ii))) /= 0) then
             ! Keep ind(ii)
             newnn = newnn+1
             ind(newnn) = ind(ii)
          else
             dmp=dmp+1
          end if
       end do
       if(col_bdeb)write(*,*) "COLOCATION_HEAPSORT dumped elements:",dmp
    else
       newnn=nn
    end if
    !
    !
  end subroutine colocation_heapsort1i
  !
  subroutine colocation_pushdownr(first, last,mm,key1,eps,newnn,nn,ind)
    !
    implicit none
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
          if (colocation_cmpr(key1(ind(r)),key1(ind( 2*r)),eps) > 0) then
             call colocation_swap(ind(r), ind(2*r))
          end if
          exit MAINLOOP
       else
          if (colocation_cmpr(key1(ind(r)),key1(ind(2*r)),eps) > 0 .and. &
               & colocation_cmpr(key1(ind(2*r)),key1(ind(2*r+1)),eps) <= 0) then
             call colocation_swap(ind(r), ind(2*r))
             r = 2*r
          else if (colocation_cmpr(key1(ind(r)),key1(ind(2*r+1)),eps)>0 .and. &
               & colocation_cmpr(key1(ind(2*r+1)),key1(ind(2*r)),eps)<0) then
             call colocation_swap(ind(r), ind(2*r+1))
             r = 2*r+1
          else
             exit MAINLOOP
          end if
       end if
    end do MAINLOOP
    !
  end subroutine colocation_pushdownr
  !
  subroutine colocation_pushdowni(first, last,mm,key1,newnn,nn,ind)
    !
    implicit none
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
          if (colocation_cmpi(key1(ind(r)),key1(ind( 2*r))) > 0) then
             call colocation_swap(ind(r), ind(2*r))
          end if
          exit MAINLOOP
       else
          if (colocation_cmpi(key1(ind(r)),key1(ind(2*r))) > 0 .and. &
               & colocation_cmpi(key1(ind(2*r)),key1(ind(2*r+1))) <= 0) then
             call colocation_swap(ind(r), ind(2*r))
             r = 2*r
          else if (colocation_cmpi(key1(ind(r)),key1(ind(2*r+1)))>0 .and. &
               & colocation_cmpi(key1(ind(2*r+1)),key1(ind(2*r)))<0) then
             call colocation_swap(ind(r), ind(2*r+1))
             r = 2*r+1
          else
             exit MAINLOOP
          end if
       end if
    end do MAINLOOP
    !
  end subroutine colocation_pushdowni
  !
  !
  integer function colocation_cmpr(a,b,eps)
    implicit none
    real :: a
    real :: b
    real :: eps
    if (abs(a-b) < eps) then
       colocation_cmpr = 0
    else if (a < b) then
       colocation_cmpr = 1
    else
       colocation_cmpr = -1
    end if
  end function colocation_cmpr
  !
  integer function colocation_cmpi(a,b)
    implicit none
    integer :: a
    integer :: b
    if (a == b) then
       colocation_cmpi = 0
    else if (a < b) then
       colocation_cmpi = 1
    else
       colocation_cmpi = -1
    end if
  end function colocation_cmpi
  !
  !
  subroutine colocation_swap(k1, k2)
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
  end subroutine colocation_swap
  !
  !
  !###############################################################################
  ! STRING ROUTINES
  !###############################################################################
  !
  !
  subroutine findDelimiter(var80,del,pos)
    implicit none
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
  !
end module colocation
