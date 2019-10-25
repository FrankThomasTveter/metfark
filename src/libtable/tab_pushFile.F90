subroutine tab_pushfile(tid, path250, crc250, irc)
  use table
  implicit none
  integer :: tid             ! session id
  character*250 :: path250
  character*250 :: crc250
  integer :: irc
  character*250 :: buff250
  integer :: lenc
  character*25 :: myname = "tab_pushFile"
  type(table_session), pointer :: tss !  current session
  !write(*,*) myname,'Entering.',irc,tid,path250
  call table_getSession(tss,tid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  call table_stackpush(tss,path250,crc250,irc)
  if (irc.ne.0) then
     !write(*,*) 'pushFile Error.'
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from table_stackpush.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,'Done.',irc,tid
  return
end subroutine tab_pushfile
