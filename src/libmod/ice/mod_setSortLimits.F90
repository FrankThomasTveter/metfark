!void mod_setsortlimits_(int* mid, char* min80, char* max80, char* crc250, int* irc, int len1);
subroutine mod_setSortlimits(mid, min80, max80, crc250, irc)
  use model
  implicit none
  integer :: mid             ! session id
  character*80 :: min80,max80
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setsortlimits"
  !write(*,*) myname, 'Entering.',irc,mid
  call model_setsortlimits(mid, min80, max80, crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,trim(myname))
     call model_errorappend(crc250," Error return from model_setsortlimits.")
     call model_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine mod_setSortlimits
