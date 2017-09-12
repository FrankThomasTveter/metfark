subroutine plo_makegraphics(sid,tab250,gra250,crc250, irc)
  use plot
  implicit none
  integer :: sid ! plot session id
  character*250 :: tab250
  character*250 :: gra250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "plo_makegraphics"
  type(plot_session), pointer :: css !  current session
  !
  call plot_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from getSession.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  call  plot_makegraphics(css,tab250,gra250,crc250, irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,"|")
     call plot_errorappend(crc250,trim(myname))
     call plot_errorappend(crc250," Error return from plot_makegraphics.")
     call plot_errorappendi(crc250,irc)
     return
  end if
  return
end subroutine plo_makegraphics
