subroutine mod_pushdefault(cid, crc250, irc)
  use model
  implicit none
  integer :: cid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "pushdefault"
  !write(*,*) myname, 'Entering.',irc,cid
  call model_pushdefault(cid,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,trim(myname))
     call model_errorappend(crc250," Error return from model_pushdefault.")
     call model_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine mod_pushdefault
