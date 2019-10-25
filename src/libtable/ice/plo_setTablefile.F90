subroutine plo_settablefile(pid, tab250, crc250, irc)
  use plot
  implicit none
  integer :: pid             ! session id
  character*250 :: tab250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "settablefile"
  type(plot_session), pointer :: pss !  current session
  !write(*,*) myname, 'Entering.',irc,tab250
  !
  call plot_getSession(pss,pid,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from getSession.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  call plot_settablefile(pss,tab250,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from plo_settablefile.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine plo_settablefile
