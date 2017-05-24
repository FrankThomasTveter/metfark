subroutine obs_getTableCValue(sid, code, subcode, val250, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: code
  integer :: subcode
  character*250 :: val250      ! bufr table c path
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "obs_getTableCValue"
  !write(*,*) myname, 'Entering.',irc,sid
  val250=observation_getCodeValue(code,subcode,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_getCodeValue.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_getTableCValue
