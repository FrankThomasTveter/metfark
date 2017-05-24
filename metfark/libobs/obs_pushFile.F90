subroutine obs_pushfile(sid, path, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: path
  character*250 :: crc250
  integer :: irc
  character*250 :: buff250
  integer :: lenc
  character*25 :: myname = "obs_pushFile"
  !write(*,*) myname,'Entering.',irc,sid,path
  call observation_stackpush(sid,path,crc250,irc)
  if (irc.ne.0) then
     !write(*,*) 'pushFile Error.'
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_stackpush.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.',irc,sid
  return
end subroutine obs_pushfile
