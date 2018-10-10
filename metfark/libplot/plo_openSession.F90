subroutine plo_opensession(sid, crc250, irc)
  use plot
  implicit none
  integer :: sid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "plo_opensession"
  type(plot_session), pointer :: css !  current session
  !write(*,*) myname,'Entering.',irc
  call plot_opensession(sid,css,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from plo_openSession.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,'Done.',sid
  return
end subroutine plo_opensession
