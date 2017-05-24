subroutine obs_peekfilelen(sid,maxrep, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "peekFileLen"
  !write(*,*)myname,'Entering.'
  call observation_stackpeeklen(sid,maxrep,crc250,irc)
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
