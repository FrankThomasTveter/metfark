subroutine tab_pushattr(sid,name80,val250,crc250, irc)
  use table
  implicit none
  integer :: sid ! table session id
  character*80 :: name80
  character*250 :: val250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "tab_pushattr"
  type(table_session), pointer :: css !  current session
  !
  call table_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !
  call  table_pushattr(css,name80,val250,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from table_pushattr.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  return
end subroutine tab_pushattr
