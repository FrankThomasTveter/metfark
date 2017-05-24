subroutine mod_pushexpression(sid, crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*250 :: buff250
  integer :: lenc
  character*25 :: myname = "pushExpression"
  !write(*,*) myname,'Entering.',irc,sid
  call model_pushexpression(sid,crc250,irc)
  if (irc.ne.0) then
     !write(*,*) 'pushExpression Error.'
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from model_pushexp.")
     call model_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.'
  return
end subroutine mod_pushexpression
