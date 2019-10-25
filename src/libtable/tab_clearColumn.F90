subroutine tab_clearcolumn(sid, crc250, irc)
  use table
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "tab_clearcolumn"
  type(table_session), pointer :: css !  current session
  !if(table_bdeb)write(*,*) myname, 'Entering.',irc
  call table_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  call table_clearcolumn(css,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from tab_clearcolumn.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !if(table_bdeb)write(*,*) myname,' Done.'
  return
end subroutine tab_clearcolumn
