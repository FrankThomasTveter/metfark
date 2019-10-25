subroutine tab_closesession(sid, crc250, irc)
  use table
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "tab_closesession"
  type(table_session), pointer :: css !  current session
  !write(*,*) myname,'Entering.',irc
  call table_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  call table_closesession(css,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from tab_closeSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,'Done.'
  return
end subroutine tab_closeSession
