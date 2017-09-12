subroutine col_opensession(sid, crc250, irc)
  use colocation
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "col_opensession"
  type(col_session), pointer :: css !  current session
  !write(*,*) myname,'Entering.',irc
  call colocation_opensession(sid,css,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,"|")
     call colocation_errorappend(crc250,myname)
     call colocation_errorappend(crc250," Error return from colocation_openSession.")
     call colocation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.',sid
  return
end subroutine col_opensession
