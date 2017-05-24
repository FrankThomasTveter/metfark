subroutine mod_peekfilelen(sid,maxrep, crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "peekFileLen"
  call model_peeklen(sid,maxrep,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from model_stackpeeklen.")
     call model_errorappendi(crc250,irc)
     return
  end if
  return
end subroutine mod_peekfilelen
