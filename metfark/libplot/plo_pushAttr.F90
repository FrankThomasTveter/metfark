subroutine plo_pushattr(sid,name80,val250,crc250, irc)
  use plot
  implicit none
  integer :: sid ! plot session id
  character*80 :: name80
  character*250 :: val250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "plo_pushattr"
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
  !
  call  plot_pushattr(css,name80,val250,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from plot_pushattr.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  return
end subroutine plo_pushattr
