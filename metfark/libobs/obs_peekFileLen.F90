subroutine obs_peekfilelen(sid,maxrep, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "peekFileLen"
  type(obs_session), pointer :: css !  current session
  !write(*,*)myname,'Entering.'
  call observation_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from getSession.")
     call observation_errorappendi(crc250,irc)
     call observation_errorappend(crc250,"\n")
     return
  end if
  call observation_stackpeeklen(css,maxrep,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_stackpeeklen.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*)myname,'Done.',maxrep
  return
end subroutine obs_peekfilelen
