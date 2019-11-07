subroutine tab_setfilter(tid,flt,crc250, irc)
  use table
  implicit none
  integer :: tid             ! session id
  character*(*) :: flt
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setfilter"
  type(table_session), pointer :: tss !  current session
  !write(*,*) myname, 'Entering.',irc,filter250,
  call table_getSession(tss,tid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  call table_setfilter(tss,flt,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from table_setfilter.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine tab_setfilter
