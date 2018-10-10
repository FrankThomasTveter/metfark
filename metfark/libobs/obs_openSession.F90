subroutine obs_opensession(sid, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "obs_opensession"
  type(obs_session), pointer :: css !  current session
  !write(*,*) myname,'Entering.',irc
  call observation_opensession(sid,css,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_openSession.")
     call observation_errorappendi(crc250,irc)
     call observation_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,'Done.',sid
  return
end subroutine obs_opensession
