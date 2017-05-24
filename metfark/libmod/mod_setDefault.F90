!void mod_setdefault_(int* mid, int* nt, char* trg80, char* crc250, int* irc, int len1, int len2, int len3);
subroutine mod_setDefault(mid, nt, trg80, crc250, irc)
  use model
  implicit none
  integer :: mid             ! session id
  integer :: nt
  character*80 :: trg80(nt)
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setdefault"
  !write(*,*) myname, 'Entering.',irc,mid
  call model_setdefault(mid, nt, trg80, crc250,irc)
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
