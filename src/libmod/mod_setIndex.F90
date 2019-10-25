subroutine mod_setIndex(sid, trgname,varname, crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  character(len=*) :: trgname
  character(len=*) :: varname
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setindex"
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
  call model_setindex(css,trgname,varname, crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from model_setindex.")
     call model_errorappendi(crc250,irc)
     call model_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine mod_setIndex
