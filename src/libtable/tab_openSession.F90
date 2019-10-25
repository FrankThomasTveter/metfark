subroutine tab_opensession(sid, crc250, irc)
  use table
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "tab_opensession"
  type(table_session), pointer :: css !  current session
  !write(*,*) myname,'Entering.',irc
  call table_opensession(sid,css,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from tab_openSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,'Done.',sid
  return
end subroutine tab_opensession
