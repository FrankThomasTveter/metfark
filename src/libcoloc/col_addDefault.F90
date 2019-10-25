subroutine col_adddefault(sid, t80, v80, crc250, irc)
  use colocation
  implicit none
  integer :: sid             ! session id
  character*80 :: t80        ! target name
  character*80 :: v80        ! value
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "adddefault"
  type(col_session), pointer :: css !  current session
  !write(*,*) myname, 'Entering.',irc,sid,varname
    call colocation_getSession(css,sid,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," Error return from getSession.")
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
  call colocation_adddefault(css,t80,v80,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,myname)
     call colocation_errorappend(crc250," Error return from colocation_stackclear.")
     call colocation_errorappendi(crc250,irc)
     call colocation_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine col_adddefault
