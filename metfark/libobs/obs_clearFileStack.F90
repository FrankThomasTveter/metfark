subroutine obs_clearfilestack(sid, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "clearFileStack"
  !write(*,*) myname, 'Entering.',irc,sid,varname
  call observation_stackclear(sid,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_stackclear.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_clearfilestack
