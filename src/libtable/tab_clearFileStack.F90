subroutine tab_clearfilestack(tid, crc250, irc)
  use table
  implicit none
  integer :: tid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "clearFileStack"
  type(table_session), pointer :: tss !  current session
  !write(*,*) myname, 'Entering.',irc,tid,varname
  call table_getSession(tss,tid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  call table_stackclear(tss,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from table_stackclear.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine tab_clearfilestack
