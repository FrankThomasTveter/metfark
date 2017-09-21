subroutine col_addMatch(sid, nam80, expr250, l80, u80, crc250, irc)
  use colocation
  implicit none
  integer :: sid             ! session id
  character*80 :: nam80
  character*250 :: expr250
  character*80 :: l80
  character*80 :: u80
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "pushmatchrule"
  type(col_session), pointer :: css !  current session
  !write(*,*) myname, 'Entering.',irc,sid
    call colocation_getSession(css,sid,crc250,irc)
    if (irc.ne.0) then
       call colocation_errorappend(crc250,myname)
       call colocation_errorappend(crc250," Error return from getSession.")
       call colocation_errorappendi(crc250,irc)
       call colocation_errorappend(crc250,"\n")
       return
    end if
  call colocation_addmatch(css,nam80,expr250,l80,u80,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,"|")
     call colocation_errorappend(crc250,trim(myname))
     call colocation_errorappend(crc250," Error return from colocation_addMatch.")
     call colocation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine col_addMatch
