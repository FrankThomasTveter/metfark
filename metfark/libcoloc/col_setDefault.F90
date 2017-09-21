!void col_setdefault_(int* sid, int* nt, char* trg80, char* crc250, int* irc, int len1, int len2, int len3);
subroutine col_setDefault(sid, nt, trg80, crc250, irc)
  use colocation
  implicit none
  integer :: sid             ! session id
  integer :: nt
  character*80 :: trg80(nt)
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setdefault"
  type(col_session), pointer :: css !  current session
  !write(*,*) myname, 'Entering.',irc,sid
  call colocation_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,myname)
     call colocation_errorappend(crc250," Error return from getSession.")
     call colocation_errorappendi(crc250,irc)
     call colocation_errorappend(crc250,"\n")
     return
  end if
  call colocation_setdefault(css, nt, trg80, crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,"|")
     call colocation_errorappend(crc250,trim(myname))
     call colocation_errorappend(crc250," Error return from colocation_setdefault.")
     call colocation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine col_setDefault
