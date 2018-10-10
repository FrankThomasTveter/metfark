subroutine obs_setType(sid, category, subCategory, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  integer :: category
  integer :: subCategory
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setType"
  type(obs_session), pointer :: css !  current session
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from getSession.")
     call observation_errorappendi(crc250,irc)
     call observation_errorappend(crc250,"\n")
     return
  end if
  call observation_setType(css,category,subCategory,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,myname)
     call observation_errorappend(crc250," Error return from observation_setType.")
     call observation_errorappendi(crc250,irc)
     call observation_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_setType
