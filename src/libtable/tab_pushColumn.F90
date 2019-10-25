subroutine tab_pushcolumn(sid,nam80,exp250,crc250, irc)
  use table
  implicit none
  integer :: sid ! table session id
  character*80 :: nam80
  character*250 :: exp250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "tab_pushcolumn"
  type(table_session), pointer :: pss !  current session
  !write(*,*)myname,'Entering:',irc,sid,cid,mid,oid,nam250
  ! get session objects
  call table_getSession(pss,sid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !
  call  table_pushcolumn(pss,nam80,exp250,crc250, irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from table_pushcolumn.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !write(*,*)myname,'Exiting:',irc,sid,nam80
  return
end subroutine tab_pushcolumn
