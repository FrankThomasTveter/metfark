subroutine obs_setIndexLimits(sid, s25,e25, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*25 :: s25,e25      ! start/end index
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setIndexLimits"
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_setIndexLimits(sid,s25,e25,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_setIndexLimits.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_setIndexLimits
