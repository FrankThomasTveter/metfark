subroutine tab_strepgraphicsdir(pid, crc250, irc)
  use table
  implicit none
  integer :: pid             ! session id
  character*250 :: gra250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "strepgraphicsdir"
  type(table_session), pointer :: pss !  current session
  !write(*,*) myname, 'Entering.',irc,gra250
  !
  call table_getSession(pss,pid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  call table_strepgraphicsdir(pss,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from tab_strepgraphicsdir.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine tab_strepgraphicsdir
