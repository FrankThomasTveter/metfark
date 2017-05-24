module colocation
  IMPLICIT NONE
  !
  ! Global constants
  !
  logical     :: col_bdeb=.false.
  !
  ! SESSION VARIABLES
  !
  type :: col_session
     integer                         :: sid
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
  subroutine colocation_opensession(sid,crc250,irc)
    integer :: sid
    character*250 :: crc250
    integer :: irc
    type(col_session),pointer :: newSession  !  new session
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
    allocate(newSession,stat=irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250,"Unable to allocate 'new session'.")
       call colocation_errorappend(crc250,"\n")
       return
    end if
    maxid=maxid+1
    newSession%sid=maxid
    newSession%prev => lastSession%prev
    newSession%next => lastSession
    newSession%prev%next => newSession
    newSession%next%prev => newSession
    sid = newSession%sid
    return
  end subroutine colocation_opensession

  subroutine colocation_getSession(css,sid,crc250,irc)
    type(col_session), pointer :: css !  current session
    integer :: sid
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "colocation_getSession"
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

  subroutine colocation_closeSession(sid,crc250,irc)
    integer :: sid
    character*250 :: crc250
    integer :: irc
    type(col_session), pointer :: css !  current session
    character*25 :: myname = "colocation_closeSession"
    if(col_bdeb)write(*,*)myname,'Entering.',irc
    call colocation_getSession(css,sid,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," Error return from getSession.")
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
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
    type(col_session), pointer :: css !  current session
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "colocation_removeSession"
    css%prev%next => css%next
    css%next%prev => css%prev
    deallocate(css)
  end subroutine colocation_removeSession
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
  ! E R R O R    R O U T I N E S
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
  ! S O R T I N G   R O U T I N E S
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

  subroutine colocation_makeXML(cid,mid,bid,crc250, irc)
    use model
    use observations
    use parse
    implicit none
    integer :: cid ! colocation session id
    integer :: mid ! model session id
    integer :: bid ! observation session id
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    integer :: lenc,lene
    character*25 :: myname = "colocation_makeXML"
    integer :: tmod,emod,dmod,tobs,ii,jj,ind_ii,nfunc
    logical :: bobsexp
    type(mod_session), pointer :: mss !  current session
    type(obs_session), pointer :: oss !  current session
    type(col_session), pointer :: css !  current session
    type(parse_session), pointer :: pse
    type(parse_pointer), pointer :: pss(:) ! parse sessions
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
    ! get session objects
    call model_getSession(mss,mid,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_getSession")
       return
    end if
    call observation_getSession(oss,bid,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"observation_getSession")
       return
    end if
    call colocation_getSession(css,cid,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"colocation_getSession")
       return
    end if
    ! check what we should colocated
    tmod=model_targetCount(mss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_targetCount")
       return
    end if
    emod=model_expressionCount(mss,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,"model_expressionCount")
       return
    end if
    dmod=model_defaultCount(mss,crc250,irc)
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
       call colocation_errorappend(crc250,"Missing model target expressions, expected ")
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
       bobsexp=.false.
    else
       bobsexp=observation_hasExpression(oss,crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"observation_hasExpression")
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
    ind_ii=0  ! mark expression that corresponds to index variable
    if (bobsexp) then ! obs-index-expression
       lene=length(oss%ind_exp250,250,10)
       call parse_parsef(pse, oss%ind_exp250(1:lene), oss%trg80, crc250,irc)
       if (irc.ne.0) then
          call colocation_errorappend(crc250,"parse_parsef")
          return
       end if
    end if
    if(col_bdeb)write(*,*)myname,'Processing expressions.',emod,associated(mss%currentExp)
    do ii=1,emod ! match-expressions + obs-index-expression
       call parse_parsef(pss(ii)%ptr, mss%currentExp%e250(ii)(1:mss%currentExp%lene(ii)), &
            & oss%trg80, crc250,irc)
       if (irc.ne.0) then
          ! write(*,*) myname,'Expr:', ii, mss%currentExp%lene(ii),&
          !      &  mss%currentExp%ntrgexp,  mss%currentExp%e250(ii)(1:mss%currentExp%lene(ii))
          ! 
          ! do jj=1,size(oss%trg80)
          !    write(*,*) myname,'var:',jj,oss%ntrg,oss%ntarget,oss%trg80(jj)(1:oss%trg_lent(jj))
          ! end do
          call colocation_errorappend(crc250,"parse_parsef_loop")
          return
       end if
       if (mss%currentExp%n80(ii)(1:mss%currentExp%lenn(ii)).eq.mss%ind_var80(1:mss%ind_lenv)) then
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
             call model_locclear(mss,mss%ctarget,mss%trg_v80,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_locclear")
                return
             end if

             if(col_bdeb)write(*,*)myname,'Compile expressions.',associated(mss%currentExp)
             if (associated(mss%currentExp)) then
                ! compile match-experssions
                do ii=1,mss%currentExp%ntrgexp
                   call model_compileExpression(mss,ii,oss%trg80,crc250,irc)
                   if (irc.ne.0) then
                      call colocation_errorappend(crc250,"model_compileExpression")
                      return
                   end if
                end do
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
                ! read next observation into static BUFR memory
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
                do ii=1,mss%currentExp%ntrgexp
                   call model_evalExpression(mss,ii,oss%trg_val,crc250,irc)
                   if (irc.ne.0) then
                      call colocation_errorappend(crc250,"model_evalExpression")
                      return
                   end if
                end do

                ! make target values
                !write(*,*)myname,'Set model targets.'
                call  model_setTargetVal(mss,crc250,irc)
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
                call model_locpush(mss,locid,mss%ctarget,mss%trg_val,lok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_locPush")
                   return
                end if
             end do LOCATION
          else if (tmod.ne.0) then ! use model default values

             if(col_bdeb)write(*,*)myname,'Clearing loc stack.',mss%ctarget,associated(mss%trg_v80)

             ! initialise the location list
             call model_locclear(mss,mss%ctarget,mss%trg_v80,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_locclear")
                return
             end if
             
             if(col_bdeb)write(*,*)myname,'Creating locations from default.',associated(mss%firstDef)
             if(col_bdeb)write(*,*)myname,'...:',associated(mss%firstDef%next)
             mss%currentDef=>mss%firstDef%next
             do while (.not.associated(mss%currentDef,target=mss%lastDef))
                if(col_bdeb)write(*,*)myname,'Make target values from default.'
                ! make target values
                call  model_setTargetVal(mss,crc250,irc)
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
                call model_locpush(mss,locid,mss%ctarget,mss%trg_val,lok,crc250,irc)
                if (irc.ne.0) then
                   call colocation_errorappend(crc250,"model_locPush")
                   return
                end if
                mss%currentDef=>mss%currentDef%next
             end do
          end if
          if (tmod.ne.0) then
             ! finally slice the model file and write model XML to stdout
             call model_slicecurrentfile(mss,bok,crc250,irc)
             if (irc.ne.0) then
                call colocation_errorappend(crc250,"model_stackslicecurrentfile")
                return
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
                   call observation_writexml(oss,locid,crc250,irc)
                   if (irc.ne.0) then
                      call colocation_errorappend(crc250,"observation_writeXML")
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
  end subroutine colocation_makeXML
  !
end module colocation
