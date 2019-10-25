subroutine plo_pushcolumn(sid,nam80,exp250,crc250, irc)
  use plot
  implicit none
  integer :: sid ! plot session id
  character*80 :: nam80
  character*250 :: exp250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "plo_pushcolumn"
  type(plot_session), pointer :: pss !  current session
  !write(*,*)myname,'Entering:',irc,sid,cid,mid,oid,nam250
  ! get session objects
  call plot_getSession(pss,sid,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from getSession.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  !
  call  plot_pushcolumn(pss,nam80,exp250,crc250, irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from plot_pushcolumn.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  !write(*,*)myname,'Exiting:',irc,sid,nam80
  return
end subroutine plo_pushcolumn
