subroutine obs_popfile(sid,path250,crc250,irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: path250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "popFile"
  type(obs_session), pointer :: css !  current session
  call observation_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from getSession.")
     call observation_errorappendi(crc250,irc)
     call observation_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,'Entering.',irc
  call observation_stackpop(css,path250,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_stackpop.")
     call observation_errorappendi(crc250,irc)
     call observation_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,'Done.'
  return
end subroutine obs_popfile
