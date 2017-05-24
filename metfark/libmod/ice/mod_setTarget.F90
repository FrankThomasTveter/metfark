!void void mod_settarget_(int* mid, char* name80, char* var80, char* min80, char* max80, char* crc250, int* irc, int len1, int len2, int len3);
subroutine mod_setTarget(mid, nam80, var80, min80, max80, crc250, irc)
  use model
  implicit none
  integer :: mid             ! session id
  character*80 :: nam80
  character*80 :: var80
  character*80 :: min80
  character*80 :: max80
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "settarget"
  !write(*,*) myname, 'Entering.',irc,mid
  call model_settarget(mid,nam80, var80, min80, max80,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,trim(myname))
     call model_errorappend(crc250," Error return from model_settarget.")
     call model_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine mod_setTarget
