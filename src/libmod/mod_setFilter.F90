subroutine mod_setfilter(sid,flt,crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  character*(*) :: flt
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setfilter"
  type(mod_session), pointer :: css !  current session
  !write(*,*) myname, 'Entering.',irc,filter250,
  call model_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from getSession.")
     call model_errorappendi(crc250,irc)
     call model_errorappend(crc250,"\n")
     return
  end if
  call model_setfilter(css,flt,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from col_setobscache.")
     call model_errorappendi(crc250,irc)
     call model_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine mod_setfilter
