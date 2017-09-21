subroutine col_pushmatch(sid, crc250, irc)
  use colocation
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*250 :: buff250
  integer :: lenc
  character*25 :: myname = "pushMatch"
  type(col_session), pointer :: css !  current session
  !write(*,*) myname,'Entering.',irc,sid
  call colocation_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,myname)
     call colocation_errorappend(crc250," Error return from getSession.")
     call colocation_errorappendi(crc250,irc)
     call colocation_errorappend(crc250,"\n")
     return
  end if
  call colocation_pushmatch(css,crc250,irc)
  if (irc.ne.0) then
     !write(*,*) 'pushMatch Error.'
     call colocation_errorappend(crc250,"|")
     call colocation_errorappend(crc250,myname)
     call colocation_errorappend(crc250," Error return from colocation_pushexp.")
     call colocation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.'
  return
end subroutine col_pushmatch
