subroutine plo_clearcolumn(sid, crc250, irc)
  use plot
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "plo_clearcolumn"
  type(plot_session), pointer :: css !  current session
  !if(plot_bdeb)write(*,*) myname, 'Entering.',irc
  call plot_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from getSession.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  call plot_clearcolumn(css,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from plo_clearcolumn.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  !if(plot_bdeb)write(*,*) myname,' Done.'
  return
end subroutine plo_clearcolumn
