!void mod_setdefault_(int* sid, int* nt, char* trg80, char* crc250, int* irc, int len1, int len2, int len3);
subroutine mod_setDefault(sid, nt, trg80, crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  integer :: nt
  character*80 :: trg80(nt)
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setdefault"
  type(mod_session), pointer :: css !  current session
  !write(*,*) myname, 'Entering.',irc,sid
  call model_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from getSession.")
     call model_errorappendi(crc250,irc)
     call model_errorappend(crc250,"\n")
     return
  end if
  call model_setdefault(css, nt, trg80, crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,trim(myname))
     call model_errorappend(crc250," Error return from model_setdefault.")
     call model_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine mod_setDefault
