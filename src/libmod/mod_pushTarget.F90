subroutine mod_pushtarget(sid, t80, v80, l80, u80, crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  character*80 :: t80        ! target name
  character*80 :: v80        ! variable
  character*80 :: l80        ! lower value
  character*80 :: u80        ! upper value
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "pushtarget"
  type(mod_session), pointer :: css !  current session
  !write(*,*) myname, 'Entering.',irc,sid,varname
  call model_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from getSession.")
     call model_errorappendi(crc250,irc)
     call model_errorappend(crc250,"\n")
     return
  end if
  call model_pushtarget(css,t80,v80,l80,u80,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from model_stackclear.")
     call model_errorappendi(crc250,irc)
     call model_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine mod_pushtarget
