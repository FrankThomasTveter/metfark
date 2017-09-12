subroutine mod_peekfilelen(sid, maxrep, crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "peekFileLen"
  type(mod_session), pointer :: css !  current session
  call model_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from getSession.")
     call model_errorappendi(crc250,irc)
     call model_errorappend(crc250,"\n")
     return
  end if
  call model_peeklen(css,maxrep,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from model_stackpeeklen.")
     call model_errorappendi(crc250,irc)
     return
  end if
  return
end subroutine mod_peekfilelen
