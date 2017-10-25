subroutine obs_makeCache(sid, path, test, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: path
  integer :: test
  character*250 :: crc250
  integer :: irc
  character*250 :: buff250
  integer :: lenc
  character*25 :: myname = "obs_makeCache"
  type(obs_session), pointer :: css !  current session
  !write(*,*) myname,'Entering.',irc,sid,path
  call observation_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from getSession.")
     call observation_errorappendi(crc250,irc)
     call observation_errorappend(crc250,"\n")
     return
  end if
  call observation_makecache(css,path,test,crc250,irc)
  if (irc.ne.0) then
     !write(*,*) 'pushFile Error.'
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_makecache.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.',irc,sid
  return
end subroutine obs_makeCache
