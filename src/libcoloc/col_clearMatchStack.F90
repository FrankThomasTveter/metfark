subroutine col_clearmatchstack(sid, crc250, irc)
  use colocation
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "clearMatchStack"
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
  call colocation_clearmatchstack(css,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,myname)
     call colocation_errorappend(crc250," Error return from colocation_stackclear.")
     call colocation_errorappendi(crc250,irc)
     call colocation_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine col_clearmatchstack
