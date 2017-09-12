!void mod_setindexlimits_(int* sid, char* min80, char* max80, char* crc250, int* irc, int len1);
subroutine mod_setIndexlimits(sid, smin, smax, crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  character(LEN=*) :: smin,smax
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "mod_setindexlimits"
  type(mod_session), pointer :: css !  current session
  if(bdeb)write(*,*) myname, 'Entering.',irc,sid
  call model_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from getSession.")
     call model_errorappendi(crc250,irc)
     call model_errorappend(crc250,"\n")
     return
  end if
  call model_setindexlimits(css, smin, smax, crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,trim(myname))
     call model_errorappend(crc250," Error return from model_setindexlimits.")
     call model_errorappendi(crc250,irc)
     return
  end if
  if(bdeb)write(*,*) myname,' Done.'
  return
end subroutine mod_setIndexlimits
