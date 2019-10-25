subroutine mod_peekfile(sid, maxrep, nrep, rep250, crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  integer :: maxrep
  integer :: nrep
  character*250 :: rep250(maxrep)
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "peekFile"
  type(mod_session), pointer :: css !  current session
  call model_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from getSession.")
     call model_errorappendi(crc250,irc)
     call model_errorappend(crc250,"\n")
     return
  end if
  call model_peek(css,maxrep,nrep,rep250,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from model_stackpeek.")
     call model_errorappendi(crc250,irc)
     call model_errorappend(crc250,"\n")
     return
  end if
  return
end subroutine mod_peekfile
