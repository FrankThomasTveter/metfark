subroutine mod_clearfilestack(sid, varname, crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  character*80 :: varname
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "clearFileStack"
  !write(*,*) myname, 'Entering.',irc,sid,varname
  call model_clearfilestack(sid,varname,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,trim(myname))
     call model_errorappend(crc250," Error return from model_stackclear.")
     call model_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine mod_clearfilestack
