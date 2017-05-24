subroutine obs_setTablePath(sid, path250, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: path250      ! bufr table path
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "obs_setTablePath"
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_settablepath(sid,path250,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_stackclear.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_setTablePath
