subroutine obs_peekfile(sid,maxrep, nrep, rep250, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  integer :: nrep
  character*250 :: rep250(maxrep)
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "peekFile"
  type(obs_session), pointer :: css !  current session
  call observation_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from getSession.")
     call observation_errorappendi(crc250,irc)
     call observation_errorappend(crc250,"\n")
     return
  end if
  call observation_stackpeek(css,maxrep,nrep,rep250,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_stackpeek.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  return
end subroutine obs_peekfile
