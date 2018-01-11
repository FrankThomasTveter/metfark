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
     character*80, pointer :: v80(:)   ! value
     integer, pointer :: vlen(:)       ! value length
     real, pointer :: val(:)   ! value
     logical, pointer :: vset(:)       ! is value set?
     type(col_default), pointer :: prev => null()   ! linked list
     type(col_default), pointer :: next => null()   ! linked list
  end type col_default
  !
  ! match rules for model targets and observation targets
  !
  type :: col_match
     character*80 :: n80     ! name
     character*250 :: e250   ! obs expression
     integer :: itrg=0      ! index to model target array position
     character*80 :: l80     ! lower limit
     character*80 :: u80     ! upper limit
     type(col_match), pointer :: prev => null()   ! linked list
     type(col_match), pointer :: next => null()   ! linked list
  end type col_match
  !
  ! SESSION VARIABLES
  !
  type :: col_session
     integer       :: sid            ! session id...
     character*250 :: obs250=""      ! observation cache file
     character*250 :: mod250=""      ! model cache file
     character*250 :: xml250 =""     ! xml output file...
     integer :: lenx =0
     !
     ! imported model targets
     integer :: ctrg = 0 ! targets                  ! number of targets allocated
     character*80, pointer :: trg80(:) => null()    ! list of target names
     integer, pointer      :: trg_lent(:) => null() ! list of target name length
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
     type(col_match), pointer :: currentMatch => null() ! current match loop
     integer :: nmatch=0                                ! number of items in match-chain
     integer :: cMatch = 0                              ! number of allocated matches
     character*80, pointer :: mat_n80(:)                ! name
     integer, pointer      :: mat_lenn(:)               ! length of name
     character*250, pointer:: mat_e250(:)               ! match expression
     integer, pointer      :: mat_lene(:)               ! length of match expression
     integer, pointer      :: mat_2trg(:)                ! index to model target
     character*80, pointer :: mat_l80(:)                ! lower limit
     character*80, pointer :: mat_u80(:)                ! upper limit
     type(parse_pointer), pointer  :: mat_psp(:) => null()  ! match expression parser
     real, pointer         :: mat_val(:)                ! match value
     integer :: cii                                     ! loop index
     !
     ! all variables used by filter...
     integer ::               nall=0                    ! number of variables
     integer ::               nmod=0                    ! number of model targets
     integer ::               nobs=0                    ! number of obs targets
     integer ::               nind=0                    ! number of index variables
     character*80, pointer :: all_var(:)                ! all variables
     character*80, pointer :: all_lenv(:)               ! length of variable
     real, pointer         :: all_val(:)                ! all values
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
    character*26 :: myname="colocation_openSession"
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
    character*26 :: myname="colocation_getSession"
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
          !if (col_bdeb) write(*,*)myname,'Exiting with sid:',sid,irc
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
    character*25 :: myname="colocation_closeSession"
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
    character*25 :: myname="colocation_removeSession"
    type(col_default), pointer :: cdef, ndef
    type(col_match), pointer :: currmatch, nextmatch
    integer :: ii
    !
    if(col_bdeb)write(*,*)myname,'Removing default stack.'
    ! remove default stack
    nullify(css%currentDef)
    if (associated(css%firstDef)) then
       cdef => css%firstDef%next
       do while (.not.associated(cdef,target=css%lastDef))
          ndef => cdef%next
          call colocation_deleteDef(css,cdef,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,myname)
             call colocation_errorappend(crc250," Error return from deleteDef.")
             call colocation_errorappendi(crc250,irc)
             call colocation_errorappend(crc250,"\n")
             return
          end if
          cdef  => ndef
       end do
       deallocate(css%firstDef,css%lastDef)
    end if
    !
    if(col_bdeb)write(*,*)myname,'Removing match stack.'
    ! remove match stack
    if (associated(css%firstMatch)) then
       currmatch => css%firstMatch%next
       do while (.not.associated(currmatch,target=css%lastMatch))
          nextmatch => currmatch%next
          call colocation_deleteMatch(css,currmatch,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,myname)
             call colocation_errorappend(crc250," Error return from deleteMatch.")
             call colocation_errorappendi(crc250,irc)
             call colocation_errorappend(crc250,"\n")
             return
          end if
          currmatch  => nextmatch
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
    if(col_bdeb)write(*,*)myname,'Done.',irc
    return
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
    character*25 :: myname="colocation_importTargets"
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
!       if(mod_bdeb)write(*,*) myname,'Model targets:',mss%trg80
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
    character*25 :: myname="colocation_cleardefaultstack"
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
    character*25 :: myname="colocation_pushDefault"
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
    character*22 :: myname="colocation_loopDefault"
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
    character*22 :: myname="colocation_loopdefitem"
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
    character*25 :: myname="colocation_addDefault"
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    if (css%ctrg.eq.0) then
       irc=147
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
       call colocation_errorappend(crc250,"Model target not found:"//n80(1:lenn))
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
    character*25 :: myname="colocation_getdefault"
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
    character*25 :: myname="colocation_deleteDef"
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
    character*22 :: myname="colocation_defaultCount "
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
    type(col_match), pointer :: curmatch => null() !  current match
    type(col_match), pointer :: nextmatch => null() !  next match session
    character*28 :: myname="colocation_clearmatchstack"
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    curmatch => css%firstMatch%next
    do while (.not.associated(curmatch,target=css%lastMatch))
       nextmatch => curmatch%next
       call colocation_unlinkMatch(curmatch)
       call colocation_deallocateMatch(curmatch)
       css%nmatch=css%nmatch-1
       curmatch => nextmatch
    end do
    if(col_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine colocation_clearmatchstack
  !
  subroutine colocation_unlinkMatch(match)
    implicit none
    type(col_match), pointer :: match !  current target
    match%prev%next => match%next
    match%next%prev => match%prev
    return
  end subroutine colocation_unlinkMatch
  !
  subroutine colocation_deallocateMatch(match)
    implicit none
    type(col_match), pointer :: match !  current target
    integer :: irc2
    deallocate(match,stat=irc2) ! ignore any errors
    return
  end subroutine colocation_deallocateMatch
  !
  !
  ! add match to current
  !
  subroutine colocation_makeMatchList(css,mss,crc250,irc)
    use model
    use parse
    implicit none
    type(col_session), pointer :: css !  current session
    type(mod_session), pointer :: mss !  model session
    character*80 :: n80
    character*250 :: e250
    character*80 :: l80
    character*80 :: u80
    character*250 :: crc250
    integer :: irc
    type(col_match), pointer :: match
    integer :: ii, irc2, lenv, lenn,lene
    integer, external :: length
    character*25 :: myname="colocation_makeMatchList"
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    if (mss%ctrg.eq.0) then
       if (col_bdeb) write(*,*)myname,'No model targets defined:',mss%ctrg
       irc=347
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," No targets defined!")
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
    ! clean up
    if (css%cMatch .ne.  0) then
       do ii=1,css%cMatch
          call parse_close(css%mat_psp(ii)%ptr,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,myname)
             call colocation_errorappend(crc250,"Error return from 'parse_close'.")
             return
          end if
       end do
       if(associated(css%mat_n80)) deallocate(css%mat_n80)
       if(associated(css%mat_lenn)) deallocate(css%mat_lenn)
       if(associated(css%mat_e250)) deallocate(css%mat_e250)
       if(associated(css%mat_lene)) deallocate(css%mat_lene)
       if(associated(css%mat_2trg)) deallocate(css%mat_2trg)
       if(associated(css%mat_l80)) deallocate(css%mat_l80)
       if(associated(css%mat_u80)) deallocate(css%mat_u80)
       css%cMatch = 0
    end if
    ! allocate match-list if we have target-list
    if (mss%ctrg .ne.  0) then
       css%cMatch=0
       do while (colocation_loopMatch(css,n80,e250,l80,u80,crc250,irc))
          ! find index in model target array
          lenn=length(n80,80,10)
          lene=length(e250,250,10)
          if (lenn.ne.0.and.lene.ne.0) then
             ii=1
             SEEK:do while (ii.le.mss%ctrg)
                if (mss%trg80(ii)(1:mss%trg_lent(ii)).eq.n80(1:lenn)) exit SEEK
                ii=ii+1
             end do SEEK
          else
             ii=css%ctrg+1
          end if
          if (ii.le.css%ctrg) then
             css%currentMatch%itrg=ii
             css%cMatch=css%cMatch+1
          else
             css%currentMatch%itrg=0
          end if
       end do
       if (col_bdeb) write(*,*)myname,'Allocating Match-lists: ',css%cMatch
       allocate(css%mat_n80(css%cMatch), &
            & css%mat_lenn(css%cMatch), &
            & css%mat_e250(css%cMatch), &
            & css%mat_lene(css%cMatch), &
            & css%mat_2trg(css%cMatch), &
            & css%mat_l80(css%cMatch),  &
            & css%mat_u80(css%cMatch), &
            & css%mat_val(css%cMatch), &
            & css%mat_psp(css%cMatch), stat=irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Unable to allocate 'session:Match'.")
          call colocation_errorappend(crc250,"\n")
          return
       end if
       do ii=1,css%cMatch
          css%mat_lenn(ii)=0
          css%mat_lene(ii)=0
          css%mat_2trg(ii)=0
          css%mat_val(ii)=0.0D0
       end do
       ! loop over matches and find corresponding targets...
       css%cMatch=0
       do while (colocation_loopMatch(css,n80,e250,l80,u80,crc250,irc))
          ii=css%currentMatch%itrg
          if (ii.ne.0) then
             css%cMatch=css%cMatch+1
             lenn=length(n80,80,10)
             lene=length(e250,250,10)
             css%mat_n80(css%cMatch)=n80
             css%mat_lenn(css%cMatch)=lenn
             css%mat_e250(css%cMatch)=e250
             css%mat_lene(css%cMatch)=lene
             css%mat_2trg(css%cMatch)=css%currentMatch%itrg
             if(col_bdeb)write(*,*)myname,"Match: '"//&
                  & css%mat_n80(css%cMatch)(1:css%mat_lenn(css%cMatch))//"' -> '"//&
                  & css%mat_e250(css%cMatch)(1:css%mat_lene(css%cMatch))//"'",ii
             css%mat_l80(css%cMatch)=l80
             css%mat_u80(css%cMatch)=u80
             css%mat_val(css%cMatch)=0.0D0
             call parse_open(css%mat_psp(css%cMatch)%ptr,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,myname)
                call colocation_errorappend(crc250,"Error return from 'parse_open'.")
                return
             end if
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
       end do
    else if (css%nMatch.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"No targets found.")
       call colocation_errorappend(crc250,"\n")
       return
    end if
    ! tell model which targetvalues are set by the match rules.... 
    call  model_setTarget(mss,css%cmatch,css%mat_2trg,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_setTarget")
       return
    end if
    if(col_bdeb)write(*,*)myname,'Done.',irc
    return
  end subroutine colocation_makeMatchList
  !
  ! push current match to the stack
  !
  subroutine colocation_pushmatch(css,n80,e250,l80,u80,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*80 :: n80      ! target name
    character*250 :: e250    ! position/sequence number
    character*80 :: l80      ! lower
    character*80 :: u80      ! upper
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="colocation_pushmatch"
    type(col_match), pointer :: match !  current target
    integer :: lenn,lene
    integer,external :: length
    lenn=length(n80,80,10)
    lene=length(e250,250,10)
    if (lenn.ne.0.and.lene.ne.0) then
       allocate(match,stat=irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Unable to allocate 'match'.")
       end if
       match%n80=n80
       match%e250=e250
       match%l80=l80
       match%u80=u80
       match%next => css%lastMatch
       match%prev => css%lastMatch%prev
       match%prev%next => match
       match%next%prev => match
       nullify(match)
       css%nmatch=css%nmatch+1
    end if
    if(col_bdeb)write(*,*)myname,"Match: '"//n80(1:lenn)&
         & //"' <-> '"//e250(1:lene)//"'",css%nmatch
    return
  end subroutine colocation_pushmatch
  !
  logical function colocation_loopMatch(css,n80,e250,l80,u80,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*80  :: n80       ! target name
    character*250  :: e250       ! variable
    character*80  :: l80      ! min value
    character*80  :: u80      ! max value
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="colocation_loopMatch"
    colocation_loopmatch=.false. ! only true if all is ok...
    if (.not.associated(css%currentMatch)) then
       css%currentMatch =>  css%firstMatch%next 
    else
       css%currentMatch =>  css%currentMatch%next
    end if
    if (associated(css%currentMatch,css%lastMatch)) then
       nullify(css%currentMatch)
       colocation_loopMatch=.false.
    else
       n80=css%currentMatch%n80
       e250=css%currentMatch%e250
       l80=css%currentMatch%l80
       u80=css%currentMatch%u80
       colocation_loopmatch=.true.
    end if
    return
  end function colocation_loopMatch
  !
  logical function colocation_loopMatchList(css,n80,e250,l80,u80,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*80 :: n80
    character*250 :: e250
    character*80 :: l80
    character*80 :: u80
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="colocation_loopmatchlist"
    if (col_bdeb) write(*,*)myname,'Entering.',irc
    colocation_loopmatchlist=.false. ! only true if all is ok...
    if (.not.associated(css)) return ! no data
    if (css%cii.eq.0) then
       css%cii = 1
    else
       css%cii = css%cii + 1
    end if
    if (css%cii.gt.css%cMatch) then
       css%cii = 0
       colocation_loopmatchlist=.false.
    else
       n80=css%mat_n80(css%cii)
       e250=css%mat_e250(css%cii)
       l80=css%mat_l80(css%cii)
       u80=css%mat_u80(css%cii)
       colocation_loopmatchlist=.true.
    end if
    if (col_bdeb) write(*,*)myname,'Exiting.',irc
    return
  end function colocation_loopmatchlist
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
    character*25 :: myname="colocation_deleteMatch"
    integer :: ii
    if (associated(match)) then
       css%nmatch = css%nmatch - 1
       match%next%prev => match%prev
       match%prev%next => match%next
       call colocation_deallocateMatch(match)
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
    character*22 :: myname="colocation_matchCount "
    colocation_matchCount=css%nMatch
    return
  end function colocation_matchCount
  !
  ! compile match
  !
  subroutine colocation_compileMatch(css,oss,crc250,irc)
    use observations
    use parse
    implicit none
    type(col_session), pointer :: css !  current session
    type(obs_session), pointer :: oss !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="colocation_compileMatch"
    integer :: lene
    integer :: ii,jj ! match number
    if(col_bdeb)write(*,*)myname,'Entering.',size(oss%trg80),css%cmatch
    if (css%cmatch.ne.0) then
       do ii=1,css%cmatch
          if(col_bdeb)then
             write(*,*)myname,'nvar:',size(oss%trg80),allocated(oss%trg80)
             do jj=1,size(oss%trg80)
                write(*,'(X,A,A,I0,A)') myname,"      var(",jj,")='"//trim(oss%trg80(jj))//"'"
             end do
             write(*,*)myname,"'Calling parsef: '"//css%mat_e250(ii)(1:css%mat_lene(ii))//"'"
          end if
          call parse_parsef(css%mat_psp(ii)%ptr,css%mat_e250(ii)(1:css%mat_lene(ii)),&
               & oss%trg80,crc250,irc)
          if (irc.ne.0) then
             if(col_bdeb)then
                write(*,*)myname,"Unable to parse:'"//&
                     & css%mat_e250(ii)(1:css%mat_lene(ii))//"'",ii
!                write(*,*)myname,'nvar:',size(oss%trg80)
!                do jj=1,size(oss%trg80)
!                   write(*,*) myname,'var:',jj,trim(oss%trg80(jj))
!                end do
             end if
             call colocation_errorappend(crc250,myname)
             call colocation_errorappend(crc250," Error return from parsef.")
             call colocation_errorappendi(crc250,irc)
             call colocation_errorappend(crc250,"\n")
             return
          end if
          call parse_used(css%mat_psp(ii)%ptr,oss%trg_req)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,myname)
             call colocation_errorappend(crc250," Error return from used.")
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
    if(col_bdeb)write(*,*)myname,'Done.'
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
    character*25 :: myname="colocation_evalMatch"
    integer :: ii                     ! match number
    type(col_match), pointer :: match
    if (css%cmatch.ne.0) then
       do ii=1,css%cmatch
          if (col_bdeb)write(*,*)myname,' Local:',val
          css%mat_val(ii)=parse_evalf(css%mat_psp(ii)%ptr,val)
       end do
    else
       irc=341
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," No matches specified.")
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
    return
  end subroutine colocation_evalMatch
  !
  subroutine colocation_removeMatchList(css,crc250,irc)
    use parse
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname="colocation_removeMatchList"
    integer :: ii                     ! match number
    type(col_match), pointer :: match
    do ii=1,css%cmatch
       call parse_close(css%mat_psp(ii)%ptr,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"parse_close")
          return
       end if
    end do
    if (col_bdeb) write(*,*)myname,'De-allocating Match-list. ',css%cMatch
    if (associated(css%mat_psp)) deallocate(css%mat_psp)
    if(associated(css%mat_n80))  deallocate(css%mat_n80)
    if(associated(css%mat_lenn)) deallocate(css%mat_lenn)
    if(associated(css%mat_e250)) deallocate(css%mat_e250)
    if(associated(css%mat_lene)) deallocate(css%mat_lene)
    if(associated(css%mat_2trg))  deallocate(css%mat_2trg)
    if(associated(css%mat_l80))  deallocate(css%mat_l80)
    if(associated(css%mat_u80))  deallocate(css%mat_u80)
    css%cMatch = 0
    return
  end subroutine colocation_removeMatchList
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
    character*22 :: myname="colocation_setobscache"
    INTEGER                         :: nvar = 0
    !
    if(col_bdeb)write(*,*)myname,' Entering.',irc
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
    character*22 :: myname="colocation_getobscache"
    INTEGER                         :: nvar = 0
    integer :: lenp
    integer, external :: length
    !
    if (associated(css)  .and. .not.associated(css,target=lastSession)) then
       path250=css%obs250
       if(col_bdeb) then
          lenp=length(path250,250,10)
          write(*,*)myname,"Path: '"//path250(1:lenp)//"'"
       end if
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
    character*22 :: myname="colocation_setmodcache"
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
    character*22 :: myname="colocation_getmodcache"
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
  subroutine colocation_expression(exp250,crc250,irc)
    use parse
    character*250 :: exp250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="colocation_expression"
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
    if (col_bdeb)write(*,*)myname,' Local:',val
    res = parse_evalf (pss, val)                 ! interprete bytecode representation of ith function
    write(exp250,*) res
    call chop0(exp250,250)
    if (col_bdeb) write(*,*) myname,'Done.',irc,res
    call parse_close (pss,crc250,irc) ! open parse session
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"parse_close")
       return
    end if
    return
  end subroutine colocation_expression
  !
  !
  !###############################################################################
  ! OUTPUT-TABLE ROUTINES
  !###############################################################################
  ! colocate data and write to table file
  !
  subroutine colocation_makeTable(css,mss,oss,ounit,name80,&
       & ncol,col80,exp250,leg250,test,fill250,crc250,irc)
    use model
    use observations
    use parse
    implicit none
    type(col_session), pointer ::  css !  current session
    type(mod_session), pointer ::  mss !  current session
    type(obs_session), pointer ::  oss !  current session
    integer :: ounit ! output unit
    character*80 :: name80
    integer :: ncol
    character*80, allocatable :: col80(:)
    character*250, allocatable :: exp250(:)
    character*250 :: leg250
    integer :: test
    character*250 :: fill250
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="colocation_maketable"
    !
    integer :: mloc,mtrg,oloc,otrg
    integer :: tmod,emod,dmod,tobs,ii,jj,nfunc
    logical :: bobsind
    logical :: mod_lval(2),obs_lval(2),bok,lok
    integer :: locid,locstart
    integer :: lena,lenc,lene,lenn,lenl
    integer, external :: length
    integer :: irc2
    real:: val(ncol)            ! internal column values
    logical:: vok(ncol)            ! internal column values
    !
    integer :: mod_cnt=0
    integer :: obs_cnt=0
    real :: mod_minval = 0.0D0
    real :: mod_maxval = 0.0D0
    real :: obs_minval = 0.0D0
    real :: obs_maxval = 0.0D0
    logical :: bbok,first
    character*50 :: s2
    integer :: len2,lenf
    logical :: fill, fillx,bdeb
    !
    call chop0(fill250,250)
    lenf=length(fill250,250,10)
    fillx=(lenf.ne.0)
    fill=.false.
    !
    irc=0
    !
    lena=length(name80,80,10)
    bok=.true.
    if(col_bdeb)write(*,*)myname,'Entering.',ounit,ncol,size(col80),size(exp250)
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
    if (tmod.ne.0.and.tobs.ne.0.and.(emod.eq.0.and.dmod.eq.0)) then
       irc=234
       call colocation_errorappend(crc250,"Missing model match rules,")
       call colocation_errorappendi(crc250,tmod)
       call colocation_errorappendi(crc250,emod)
       call colocation_errorappendi(crc250,dmod)
       call colocation_errorappendi(crc250,tobs)
       return
    else if (tmod.ne.0.and.tobs.eq.0.and.(emod.eq.0.and.dmod.eq.0)) then
       irc=235
       call colocation_errorappend(crc250,"Missing model default values.")
       call colocation_errorappendi(crc250,tmod)
       call colocation_errorappendi(crc250,emod)
       call colocation_errorappendi(crc250,dmod)
       call colocation_errorappendi(crc250,tobs)
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
    ! make lists
    if(col_bdeb)write(*,*)myname,'Make model target list.'
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
    if (tobs.ne.0.and.tmod.ne.0) then
       call colocation_makeMatchList(css,mss,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"colocation_makeMatchList")
          return
       end if
    end if
    ! set slice model variables
    call model_sliceIndex(mss,css%cmatch,css%mat_2trg,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_sliceIndex")
       return
    end if
    if(col_bdeb)write(*,*)myname,'Compile expressions.',emod,associated(css)
    if (associated(css)) then
       ! compile match-experssions
       call colocation_compileMatch(css,oss,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"model_compileMatch")
          return
       end if
    else
       irc=123
       call colocation_errorappend(crc250,"No matchs available")
       return
    end if
   ! convert obs start/end limits (time) to model start/end limits if possible
    if(col_bdeb)write(*,*)myname,'Setting limits.',mss%ind_lval,mss%ind_minval,&
         & mss%ind_maxval,oss%ind_lval(2),oss%ind_minval,oss%ind_maxval
    !
    ! get absolute model and obs limits
    !
    mod_lval(1)=.false. ! are model limits available?
    mod_lval(2)=.false. ! are model limits available?
    ! set observation transformation...
    ! ...the first match expression is always the index-transformation...
    call observation_setTransformation(oss,css%mat_psp(1)%ptr,crc250,irc) ! css%cmatch
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"Error return from 'setTransformation'.")
       return
    end if
    ! set observation target names
    call  model_setObsVar(mss,oss%ntrg,oss%trg80,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_setObsVar")
       return
    end if
    ! compile model filter (called after model_setFilter and model_setObsVar)
    call model_compileFilter(mss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_compileFilter")
       return
    end if
    ! flag used observation targets
    call  model_getObsReq(mss,oss%ntrg,oss%trg_req,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_getObsReq")
       return
    end if
    ! compile obs filter
    call observation_compileFilter(oss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"obs_compileFilter")
       return
    end if
    if(col_bdeb)write(*,*)myname,'Compiling.',ncol,size(col80),size(exp250)
    ! compile column expressions
    call model_compileExpr(mss,ncol,exp250,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_compileExpr")
       return
    end if
    if(col_bdeb)write(*,*)myname,'Analyse limits.',ncol,size(col80),size(exp250)
    ! get overall max/min limits...
    if (mss%ind_lval(1).and.oss%ind_lval(1)) then
       mod_minval=max(mss%ind_minval,oss%ind_minval)
       mod_lval(1)=.true.
    else if (mss%ind_lval(1)) then
       mod_minval=mss%ind_minval
       mod_lval(1)=.true.
    else if (oss%ind_lval(1)) then
       mod_minval=oss%ind_minval
       mod_lval(1)=.true.
    else
       mod_lval(1)=.false.
    end if
    if (mss%ind_lval(2).and.oss%ind_lval(2)) then
       mod_maxval=min(mss%ind_maxval,oss%ind_maxval)
       mod_lval(2)=.true.
    else if(mss%ind_lval(2)) then
       mod_maxval=mss%ind_maxval
       mod_lval(2)=.true.
    else if(oss%ind_lval(2)) then
       mod_maxval=oss%ind_maxval
       mod_lval(2)=.true.
    else
       mod_lval(2)=.false.
    end if
    if(col_bdeb)write(*,*)myname,'Adjusting mod limits:',mod_lval,mod_minval,mod_maxval,&
         & associated(mss),associated(oss),associated(css)
    call model_setFileStackLimits(mss,mod_lval,mod_minval,mod_maxval,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"Error return from 'model_setFileStackLimits'.")
       return
    end if
    !
    if(col_bdeb)write(*,*)myname,'Entering model file loop.',mod_lval,mod_minval,mod_maxval,obs_lval,obs_minval,obs_maxval
    locid=0 ! observation count (= identification)
    MODFILE: do ! need to enter loop if (tmod.eq.0)
       if (tmod.ne.0) then ! we have model targets specified
          ! loop over data
          bok= model_loopFileStack(mss,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,"model_loopFileStack")
             return
          end if
          if (.not.bok) then
             if(col_bdeb)write(*,*)myname,'No more model files.'
             exit MODFILE ! no more files to process
          else 
             mod_cnt=mod_cnt+1
             call observation_setModelFileId(oss,model_getFileId(mss))
             if(col_bdeb)write(*,*)myname,"Found model file: '"//&
                  & mss%currentFile%fn250(1:mss%currentFile%lenf)//"'",&
                  & mod_cnt,model_getFileId(mss)
          end if
          ! get observation file limits
          if (mod_lval(1).and.mss%currentFile%ind_lim) then
             obs_minval=max(mod_minval,mss%currentFile%ind_start)
             obs_lval(1)=.true.
          else if (mod_lval(1)) then
             obs_minval=mod_minval
             obs_lval(1)=.true.
          else if (mss%currentFile%ind_lim) then
             obs_minval=mss%currentFile%ind_start
             obs_lval(1)=.true.
          else
             obs_lval(1)=.false.
          end if
          obs_lval(2)=mod_lval(2)
          obs_maxval=mod_maxval
          if (mod_lval(2).and.mss%currentFile%ind_lim) then
             obs_maxval=max(mod_maxval,mss%currentFile%ind_stop)
          else if (mod_lval(2)) then
             obs_maxval=mod_maxval
             obs_lval(2)=.true.
          else if (mss%currentFile%ind_lim) then
             obs_maxval=mss%currentFile%ind_stop
             obs_lval(2)=.true.
          else
             obs_lval(2)=.false.
          end if
       end if
       !
       !
       if(col_bdeb)write(*,*)myname,'Adjusting obs limits:',obs_lval,mss%currentFile%ind_lim,obs_minval,obs_maxval
       call observation_setFileStackLimits(oss,obs_lval,obs_minval,obs_maxval,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Error return from 'observation_setFileStackLimits'.")
          return
       end if
       ! calculate observation target limits...
       call observation_setTargetLimits(oss,mss%currentfile%dim_var,&
            & mss%currentfile%dim_val,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Error return from 'observation_setFileStackLimits'.")
          return
       end if
       !
       if(col_bdeb)write(*,*)myname,'Entering obs file loop.'
       ! loop over model data, using model/obs start/end limits
       OBSFILE : do ! need to enter loop if (tobs.eq.0)
          if (tobs.ne.0) then ! we have observation targets available
             if (col_bdeb) then
                bdeb=obs_bdeb
                obs_bdeb=.true.
             end if
             bok=observation_loopFileStack(oss,crc250,irc)
             if (col_bdeb) obs_bdeb=bdeb
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"observation_loopFileStack")
                return
             end if
             if (.not.bok) then
                if(col_bdeb)write(*,*)myname,'No more obs files.'
                exit OBSFILE ! no more files to process
             else 
                obs_cnt=obs_cnt+1
                if(col_bdeb)write(*,*)myname,"Found obs file: '"//&
                     & oss%currentFile%fn250(1:oss%currentFile%lenf)//"'"
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
             ! call model_sliceTarget(mss,crc250,irc)
             ! if (irc.ne.0) then
             !    call colocation_errorappend(crc250,"model_sliceTarget")
             !    return
             ! end if

             if(col_bdeb)write(*,*)myname,'Entering observation loop.'

             locstart=locid
             ! loop over obs data, using model start/end limits
             LOCATION : do
                !if(col_bdeb)write(*,*)myname,'Slice observation file.'
                ! read next observation into static BUFR memory and set oss%trg_val
                call observation_sliceCurrentFile(oss,bok,crc250,irc)
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
                ! set observation variables...
                !write(*,*)myname,'Set model targets.'
                call  model_setObsVal(mss,oss%ntrg,oss%trg_val,oss%trg_vok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_setObsVal")
                   return
                end if

                ! set match variables...
                !write(*,*)myname,'Set model targets.'
                call  model_setTargetVal(mss,css%cMatch,css%mat_2trg,css%mat_val,&
                     & crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_setTargetVal")
                   return
                end if

                lok=.true.
                ! make new location from observation
                if(col_bdeb.and.locid.lt.100)write(*,*)myname,'Push location.',locid
                call model_locpushtarget(mss,locid,lok,crc250,irc) ! uses match variables
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_locPush")
                   return
                end if
             end do LOCATION
          else if (tmod.ne.0) then ! use model default values

             if(col_bdeb)write(*,*)myname,'Clearing loc stack.',mss%ctrg,associated(mss%trg_v80)

             ! initialise the location list
             call model_locclear(mss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_locclear")
                return
             end if
             ! call model_sliceTarget(mss,crc250,irc)
             ! if (irc.ne.0) then
             !    call colocation_errorappend(crc250,"model_sliceTarget")
             !    return
             ! end if
             if(col_bdeb)write(*,*)myname,'Creating locations from default.',associated(css%firstDef)
             if(col_bdeb)write(*,*)myname,'...:',associated(css%firstDef%next)
             css%currentDef=>css%firstDef%next
             do while (.not.associated(css%currentDef,target=css%lastDef))
                if(col_bdeb)write(*,*)myname,'Make target values from default.'
                ! make target values
                call  model_setTargetDVal(mss,css%currentDef%cdef,&
                     & css%currentDef%vset,css%currentDef%val,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_setTargetDVal")
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
                call model_locpushtarget(mss,locid,lok,crc250,irc) ! uses match variables
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_locPush")
                   return
                end if
                css%currentDef=>css%currentDef%next
             end do
          end if
          !
          if (tmod.ne.0) then
             ! finally slice the model file and write model XML to stdout
             call model_slicecurrentfile(mss,bok,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_stackslicecurrentfile")
                return
             end if
          end if
          !
          ! loop over observations again, write valid observations to XML...
          !         
          if (tobs.ne.0) then
             first=.true.
             locid=locstart
             if (mod_bdeb)write(*,*)myname,'locready=',mss%locReady
             OBSERVATION : do
                ! read next observation into static memory, return next obs
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
                bbok=.true.
                if (tmod.ne.0) then ! we have match expressions specified
                   call model_locSearchOk(mss,locid,bbok)        
                end if
                !if (col_bdeb)write(*,*)myname,' OOK:',oss%currentFile%ook
                !
                if (bbok) then
                   call model_evalExpr(mss,ncol,val,vok,crc250,irc)
                   if (irc.ne.0) then
                      call colocation_errorappend(crc250,"evalExpr")
                      return
                   end if
                   if (ncol.ne.0) then
                      write(ounit,"(X,A)",advance="no") name80(1:lena)
                   else
                      write(ounit,"(X,A)") name80(1:lena)
                   end if
                   fill=fillx
                   do ii=1,ncol
                      if (vok(ii)) then
                         call colocation_wash(val(ii),s2,len2)
                      else
                         s2="NA"
                         len2=2
                      end if
                      if (ii.ne.ncol) then
                         write(ounit,"(X,A)",advance="no") s2(1:len2)
                      else
                         write(ounit,"(X,A)") s2(1:len2)
                      end if
                   end do
                   if(col_bdeb.and.locid.lt.100)then
                      write(*,'(2(X,A),100(X,F0.1))')&
                           & myname,'MPO:',mss%mpo_val
                      write(*,'(2(X,A),100(X,L1))')&
                           & myname,'mpo:',mss%mpo_vok
                      write(*,'(2(X,A),100(X,F0.1))')&
                           & myname,'VAL:',val
                      write(*,'(2(X,A),100(X,L1))')&
                           & myname,'val:',vok
                   end if
                end if
             end do OBSERVATION
          end if
          ! end obs data loop
          if (tobs.eq.0.or.test.ne.0) then
             exit OBSFILE
          end if
       end do OBSFILE
       ! end model loop
       if (tmod.eq.0.or.test.ne.0)  then
          exit MODFILE
       end if
    end do MODFILE
    !
    call colocation_removeMatchList(css,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"closeMatch")
       return
    end if
    ! remove location arrays
    call model_clearLoc(mss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," Error return from clearLoc.")
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
    !
    if (fill) then
       call colocation_fill(fill250,lenf,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250," Error return from fill.")
          call colocation_errorappendi(crc250,irc)
          call colocation_errorappend(crc250,"\n")
          return
       end if
    end if
    !write(*,*) myname,'Done.'
    return
  end subroutine colocation_makeTable
  !
  subroutine colocation_setXmlfile(css,xml250,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: xml250 ! name of xml file
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    character*22 :: myname="colocation_setXmlFile"
    css%xml250=xml250
    css%lenx=length(xml250,250,10)
    if(col_bdeb)write(*,*)myname,' Path: ',css%xml250(1:css%lenx)
    return
  end subroutine colocation_setXmlfile
  !
  subroutine colocation_strepfiles(css,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="colocation_strepfiles"
    integer,parameter  :: nn = 6
    character*100 :: src100(nn) = (/'YY','MM','DD','HH','MI','SS'/)
    character*100 :: rep100(nn)
    logical :: lrep(nn)
    integer, external :: length
    call colocation_settime(css,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"Error return from 'settime'.")
       call colocation_errorappendi(crc250,irc)
       return
    end if
    ! values(1):	the year
    ! values(2):	the month
    ! values(3):	the day of the month
    ! values(4):	time difference with utc in minutes
    ! values(5):	the hour of the day
    ! values(6):	the minutes of the hour
    ! values(7):	the seconds of the minute
    ! values(8):	the milliseconds of the second
    write(rep100(1),'(i4.4)')css%values(1)
    write(rep100(2),'(i2.2)')css%values(2)
    write(rep100(3),'(i2.2)')css%values(3)
    write(rep100(4),'(i2.2)')css%values(5)
    write(rep100(5),'(i2.2)')css%values(6)
    write(rep100(6),'(i2.2)')css%values(7)
    if(col_bdeb)write(*,*)myname,' Values: ',css%values
    if (css%lenx.ne.0)then
       call colocation_strep(css%xml250,nn,src100,rep100,lrep,irc)
       css%lenx=length(css%xml250,250,10)
    end if
    return
  end subroutine colocation_strepfiles
  !
  subroutine colocation_setTime(css,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="colocation_clearsetstack"
    call date_and_time(VALUES=css%values)
    return
  end subroutine colocation_setTime
  !
  subroutine colocation_strep(str250,nn,src100,rep100,lrep,irc)
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
  end subroutine colocation_strep
  !
  subroutine colocation_getXmlfile(css,xml250,crc250,irc)
    implicit none
    type(col_session), pointer :: css !  current session
    character*250 :: xml250 ! name of xml file
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    character*22 :: myname="colocation_getXmlFile"
    call colocation_strepfiles(css,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"colocation_strepfiles")
       return
    end if
    xml250=css%xml250
    if(col_bdeb)write(*,*)myname,' Path: ',css%xml250(1:css%lenx)
    return
  end subroutine colocation_getXmlfile
  !
  subroutine colocation_makeXML(css,mss,oss,xml250,test,fill250,crc250, irc)
    use model
    use observations
    use parse
    implicit none
    type(col_session), pointer :: css !  current session
    type(mod_session), pointer :: mss !  current session
    type(obs_session), pointer :: oss !  current session
    character*250 :: xml250
    integer :: test
    character*250 :: fill250
    character*250 :: crc250
    integer :: irc
    integer, external :: length,ftunit
    integer :: lenc,lene
    character*26 :: myname="colocation_makeXML"
    integer :: tmod,emod,dmod,tobs,ii,jj,nfunc
    logical :: bobsind
    integer :: locid,locstart,ounit,lenx
    integer :: irc2
    !
    integer :: mod_cnt=0
    integer :: obs_cnt=0
    real :: mod_minval = 0.0D0
    real :: mod_maxval = 0.0D0
    real :: obs_minval = 0.0D0
    real :: obs_maxval = 0.0D0
    logical :: mod_lval(2),obs_lval(2),bok,bbok,first,lok,bdeb
    integer :: lenf
    logical :: fill, fillx
    !
    call chop0(fill250,250)
    lenf=length(fill250,250,10)
    fillx=(lenf.ne.0)
    fill=.false.
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
    if (tmod.ne.0.and.tobs.ne.0.and.(emod.eq.0.and.dmod.eq.0)) then
       irc=234
       call colocation_errorappend(crc250,"Missing model match rules,")
       call colocation_errorappendi(crc250,tmod)
       call colocation_errorappendi(crc250,emod)
       call colocation_errorappendi(crc250,dmod)
       call colocation_errorappendi(crc250,tobs)
       return
    else if (tmod.ne.0.and.tobs.eq.0.and.(emod.eq.0.and.dmod.eq.0)) then
       irc=235
       call colocation_errorappend(crc250,"Missing model default values.")
       call colocation_errorappendi(crc250,tmod)
       call colocation_errorappendi(crc250,emod)
       call colocation_errorappendi(crc250,dmod)
       call colocation_errorappendi(crc250,tobs)
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
    ! make lists
    if(col_bdeb)write(*,*)myname,'Make model target list.',test
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
    if (tobs.ne.0.and.tmod.ne.0) then
       call colocation_makeMatchList(css,mss,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"colocation_makeMatchList")
          return
       end if
    end if
    ! set slice model variables
    call model_sliceIndex(mss,css%cmatch,css%mat_2trg,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_sliceIndex")
       return
    end if
    if(col_bdeb)write(*,*)myname,'Compile expressions.',emod,associated(css)
    if (associated(css)) then
       ! compile match-experssions
       call colocation_compileMatch(css,oss,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"model_compileMatch")
          return
       end if
    else
       irc=123
       call colocation_errorappend(crc250,"No matchs available")
       return
    end if
   ! convert obs start/end limits (time) to model start/end limits if possible
    if(col_bdeb)write(*,*)myname,'Setting limits.',mss%ind_lval,mss%ind_minval,&
         & mss%ind_maxval,oss%ind_lval(2),oss%ind_minval,oss%ind_maxval
    !
    ! get absolute model and obs limits
    !
    mod_lval(1)=.false. ! are model limits available?
    mod_lval(2)=.false. ! are model limits available?
    ! set observation transformation...
    ! ...the first match expression is always the index-transformation...
    call observation_setTransformation(oss,css%mat_psp(1)%ptr,crc250,irc) ! css%cmatch
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"Error return from 'setTransformation'.")
       return
    end if
    ! set observation target names
    call  model_setObsVar(mss,oss%ntrg,oss%trg80,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_setObsVar")
       return
    end if
    ! compile model filter (called after model_setFilter and model_setObsVar)
    call model_compileFilter(mss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_compileFilter")
       return
    end if
    ! compile obs filter
    call observation_compileFilter(oss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"obs_compileFilter")
       return
    end if
    ! convert obs-limits to mod-limits
    mod_lval(1)= mss%ind_lval(1)
    mod_minval = mss%ind_minval
    if (mod_lval(1).and.oss%ind_lval(1)) then
       mod_minval=max(mod_minval,oss%ind_minval)
    else if (oss%ind_lval(1)) then
       mod_minval=oss%ind_minval
       mod_lval(1)=.true.
    end if
    mod_lval(2)= mss%ind_lval(2)
    mod_maxval = mss%ind_maxval
    if (mod_lval(2).and.oss%ind_lval(2)) then
       mod_maxval=min(mod_maxval,oss%ind_maxval)
    else if(oss%ind_lval(2)) then
       mod_maxval=oss%ind_maxval
       mod_lval(2)=.true.
    end if
    if(col_bdeb)write(*,*)myname,'Adjusting mod limits:',mod_lval,mod_minval,mod_maxval,&
         & associated(mss),associated(oss)
    call model_setFileStackLimits(mss,mod_lval,mod_minval,mod_maxval,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"Error return from 'model_setFileStackLimits'.")
       return
    end if
    !
    ! open output xml file unit, ounit
    !
    call chop0(xml250,250)
    lenx=length(xml250,250,20)
    if(col_bdeb)write(*,*)myname,' Path: ',xml250(1:lenx),test
    ! open file
    ounit=ftunit(irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," no free unit number for:"//xml250(1:lenx))
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
    open ( unit=ounit, status="unknown", form="formatted", &
         &        access="sequential", &
         &        iostat=irc, file=xml250(1:lenx) )
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," unable to open:"//xml250(1:lenx))
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
    ! overwrite xml-file...
    write(ounit,'("<colocation>")',iostat=irc2)
    !
    if(col_bdeb)write(*,*)myname,'Entering model file loop.',mod_lval,mod_minval,mod_maxval,obs_lval,obs_minval,obs_maxval
    locid=0 ! observation count (= identification)
    MODFILE: do ! need to enter loop if (tmod.eq.0)
       if (tmod.ne.0) then ! we have model targets specified
          ! loop over data
          bok= model_loopFileStack(mss,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,"model_loopFileStack")
             return
          end if
          if (.not.bok) then
             if(col_bdeb)write(*,*)myname,'No more model files.'
             exit MODFILE ! no more files to process
          else 
             mod_cnt=mod_cnt+1
             call observation_setModelFileId(oss,model_getFileId(mss))
             if(col_bdeb)write(*,*)myname,"Found model file: '"//&
                  & mss%currentFile%fn250(1:mss%currentFile%lenf)//"'",&
                  & mod_cnt,model_getFileId(mss)
          end if
          !
          ! write file opening xml-tag
          !
          call model_filestartxml(mss,ounit,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,"model_filestartxml")
             return
          end if
          ! adjust observation file limits
          obs_lval(1)=mod_lval(1)
          obs_minval=mod_minval
          if (obs_lval(1).and.mss%currentFile%ind_lim) then
             obs_minval=max(obs_minval,mss%currentFile%ind_start)
          else if (mss%currentFile%ind_lim) then
             obs_minval=mss%currentFile%ind_start
             obs_lval(1)=.true.
          end if
          obs_lval(2)=mod_lval(2)
          obs_maxval=mod_maxval
          if (obs_lval(2).and.mss%currentFile%ind_lim) then
             obs_maxval=max(obs_maxval,mss%currentFile%ind_stop)
          else if (mss%currentFile%ind_lim) then
             obs_maxval=mss%currentFile%ind_stop
             obs_lval(2)=.true.
          end if
       end if
       !
       !
       if(col_bdeb)write(*,*)myname,'Adjusting obs limits:',obs_lval,mss%currentFile%ind_lim,obs_minval,obs_maxval
       call observation_setFileStackLimits(oss,obs_lval,obs_minval,obs_maxval,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Error return from 'observation_setFileStackLimits'.")
          return
       end if
       ! calculate observation target limits...
       call observation_setTargetLimits(oss,mss%currentfile%dim_var,&
            & mss%currentfile%dim_val,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250,"Error return from 'observation_setFileStackLimits'.")
          return
       end if
       !
       if(col_bdeb)write(*,*)myname,'Entering obs file loop.'
       ! loop over model data, using model/obs start/end limits
       OBSFILE : do ! need to enter loop if (tobs.eq.0)
          if (tobs.ne.0) then ! we have observation targets available
             if (col_bdeb) then
                bdeb=obs_bdeb
                obs_bdeb=.true.
             end if
             bok=observation_loopFileStack(oss,crc250,irc)
             if (col_bdeb) obs_bdeb=bdeb
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"observation_loopFileStack")
                return
             end if
             if (.not.bok) then
                if(col_bdeb)write(*,*)myname,'No more obs files.'
                exit OBSFILE ! no more files to process
             else 
                obs_cnt=obs_cnt+1
                if(col_bdeb)write(*,*)myname,"Found obs file: '"//&
                     & oss%currentFile%fn250(1:oss%currentFile%lenf)//"'"
             end if
             !
             ! write file opening xml-tag
             !
             call observation_filestartxml(oss,ounit,crc250,irc)
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
             ! call model_sliceTarget(mss,crc250,irc)
             ! if (irc.ne.0) then
             !    call colocation_errorappend(crc250,"model_sliceTarget")
             !    return
             ! end if

             if(col_bdeb)write(*,*)myname,'Entering observation loop.'

             locstart=locid
             ! loop over obs data, using model start/end limits
             LOCATION : do
                !if(col_bdeb)write(*,*)myname,'Slice observation file.'
                ! read next observation into static BUFR memory and set oss%trg_val
                call observation_sliceCurrentFile(oss,bok,crc250,irc)
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
                ! set observation variables...
                !write(*,*)myname,'Set model targets.'
                call  model_setObsVal(mss,oss%ntrg,oss%trg_val,oss%trg_vok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_setObsVal")
                   return
                end if

                ! set match variables...
                !write(*,*)myname,'Set model targets.'
                call  model_setTargetVal(mss,css%cMatch,css%mat_2trg,css%mat_val,&
                     & crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_setTargetVal")
                   return
                end if

                lok=.true.
                ! make new location from observation
                if(col_bdeb.and.locid.lt.100)write(*,*)myname,'Push location.',locid
                call model_locpushtarget(mss,locid,lok,crc250,irc) ! uses match variables
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_locPush")
                   return
                end if
             end do LOCATION
          else if (tmod.ne.0) then ! use model default values

             if(col_bdeb)write(*,*)myname,'Clearing loc stack.',mss%ctrg,associated(mss%trg_v80)

             ! initialise the location list
             call model_locclear(mss,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_locclear")
                return
             end if
             ! call model_sliceTarget(mss,crc250,irc)
             ! if (irc.ne.0) then
             !    call colocation_errorappend(crc250,"model_sliceTarget")
             !    return
             ! end if
             if(col_bdeb)write(*,*)myname,'Creating locations from default.',associated(css%firstDef)
             if(col_bdeb)write(*,*)myname,'...:',associated(css%firstDef%next)
             css%currentDef=>css%firstDef%next
             do while (.not.associated(css%currentDef,target=css%lastDef))
                if(col_bdeb)write(*,*)myname,'Make target values from default.'
                ! make target values
                call  model_setTargetDVal(mss,css%currentDef%cdef,&
                     & css%currentDef%vset,css%currentDef%val,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_setTargetDVal")
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
                call model_locpushtarget(mss,locid,lok,crc250,irc) ! uses match variables
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_locPush")
                   return
                end if
                css%currentDef=>css%currentDef%next
             end do
          end if
          !
          if (tmod.ne.0) then
             if (mod_bdeb)write(*,*)myname,'A locready=',mss%locReady
             ! finally slice the model file and write model XML to stdout
             call model_slicecurrentfileXML(mss,ounit,bok,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_stackslicecurrentfile")
                return
             end if
          end if
          !
          ! loop over observations again, write valid observations to XML...
          !         
          if (tobs.ne.0) then
             first=.true.
             locid=locstart
             if (mod_bdeb)write(*,*)myname,'B locready=',mss%locReady
             OBSERVATION : do
                ! read next observation into static memory, return next obs
                call observation_sliceCurrentFile(oss,bok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"observation_sliceCurrentFile")
                   return
                end if
                if (.not.bok) then
                   if(col_bdeb)write(*,*)myname,'NO MORE OBSERVATIONS TO PROCESS.'
                   exit OBSERVATION
                end if
                !
                locid=locid+1
                bbok=.true.
                if (tmod.ne.0) then ! we have match expressions specified
                   call model_locSearchOk(mss,locid,bbok)        
                end if
                !if (col_bdeb)write(*,*)myname,' OOK:',oss%currentFile%ook
                !
                if (bbok) then
                   call observation_writeXML(oss,ounit,locid,crc250,irc)
                   if (irc.ne.0) then
                      call colocation_errorappend(crc250,"observation_writeXML")
                      return
                   end if
                   call model_writeXML(mss,ounit,locid,crc250,irc)
                   if (irc.ne.0) then
                      call colocation_errorappend(crc250,"observation_writeXML")
                      return
                   end if
                   fill=fillx
                end if
             end do OBSERVATION
          end if
          ! end obs data loop
          if (tobs.ne.0.or.test.ne.0) then
             call observation_filestopxml(oss,ounit,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"observation_filestopxml")
                return
             end if
          else 
             exit OBSFILE
          end if
       end do OBSFILE

       ! end model loop
       if (tmod.ne.0.or.test.ne.0)  then
          ! write file stop xml-tag
          call model_filestopxml(mss,ounit,crc250,irc)
          if (irc.ne.0) then
             call colocation_errorappend(crc250,"model_filestopxml")
             return
          end if
       else 
          exit MODFILE
       end if
    end do MODFILE
    !
    call colocation_removeMatchList(css,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"closeMatch")
       return
    end if
    !
    ! close xml output file unit, ounit
    !
    call model_writeModelDataXML(mss,ounit,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_writeModelDataXML")
       return
    end if
    write(ounit,'(A,I0,A,I0,A)',iostat=irc2) " <file_overlap mod='",mod_cnt,"' obs='",obs_cnt,"'/>"
    write(ounit,'("</colocation>")',iostat=irc2)
    close (unit=ounit,iostat=irc)
    if (irc.ne.0) irc=0 ! oh well...
    ! remove location arrays
    call model_clearLoc(mss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," Error return from clearLoc.")
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
    !
    if (fill) then
       call colocation_fill(fill250,lenf,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,myname)
          call colocation_errorappend(crc250," Error return from fill.")
          call colocation_errorappendi(crc250,irc)
          call colocation_errorappend(crc250,"\n")
          return
       end if
    end if
    !write(*,*) myname,'Done.'
    return
  end subroutine colocation_makeXML
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
  subroutine colocation_wash(val,s2,len2)
    real :: val
    character*50 :: s2
    integer :: len2
    integer, external :: length
    integer :: jj,irc
    write(s2,'(F0.10)',iostat=irc) val; 
    call chop0(s2,50); 
    len2=length(s2,50,10) ! ignore last digit...
    if (irc.ne.0) then
       s2="NA"
       len2=2
    else
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
    end if
    return
  end subroutine colocation_wash
  !
  subroutine colocation_fill(fill250,lenf,crc250,irc)
    character*250 :: fill250
    integer :: lenf
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="colocation_fill"
    integer, external :: ftunit
    integer unitw
    unitw=ftunit(irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,'Error return from ftunit.')
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       if (col_bdeb) write(*,*) myname,'Error return from ftunit.', irc
       return
    endif
    open (unit=unitw,form="formatted",action="write",iostat=irc,file=fill250(1:lenf))
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," Unable to open:"//fill250(1:lenf))
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       if (col_bdeb)write(*,*) myname,'unable to open:'//fill250(1:lenf)
       return
    end if
    write(unitw,'("Data available.")')
    close (unitw)
    irc=chmod(fill250(1:lenf),"a+w")
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," Error chmod:"//fill250(1:lenf))
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
    end if
    return
  end subroutine colocation_fill
  !
end module colocation
