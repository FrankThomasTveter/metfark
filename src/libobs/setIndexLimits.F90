subroutine obs_setIndexLimits(sid, s25,e25, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*25 :: s25,e25    ! start/end index
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "clearFileStack"
  type(obs_session), pointer :: css !  current session
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from getSession.")
     call observation_errorappendi(crc250,irc)
     call observation_errorappend(crc250,"\n")
     return
  end if
  call observation_setIndexLimits(css,s25,e25,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_setIndexLimits.")
     call observation_errorappendi(crc250,irc)
     call observation_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_setIndexLimits
