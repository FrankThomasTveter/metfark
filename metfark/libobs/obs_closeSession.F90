subroutine obs_closesession(sid, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "obs_closesession"
  !write(*,*) myname,'Entering.',irc
  call observation_closesession(sid,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_closeSession.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.'
  return
end subroutine obs_closeSession
