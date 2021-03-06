subroutine plo_getgraphicsfile(pid, gra250, crc250, irc)
  use plot
  implicit none
  integer :: pid             ! session id
  character*250 :: gra250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "getgraphicsfile"
  type(plot_session), pointer :: pss !  current session
  !write(*,*) myname, 'Entering.',irc,gra250
  !
  call plot_getSession(pss,pid,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from getSession.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  call plot_getgraphicsfile(pss,gra250,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from plo_getgraphicsfile.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine plo_getgraphicsfile
