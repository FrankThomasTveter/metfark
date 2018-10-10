subroutine col_getxmlfile(cid, xml250, crc250, irc)
  use colocation
  implicit none
  integer :: cid             ! session id
  character*250 :: xml250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "getXMLfile"
  type(col_session), pointer :: css !  current session
  !write(*,*) myname, 'Entering.',irc,xml250
  !
  call colocation_getSession(css,cid,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,myname)
     call colocation_errorappend(crc250," Error return from getSession.")
     call colocation_errorappendi(crc250,irc)
     call colocation_errorappend(crc250,"\n")
     return
  end if
  call colocation_getxmlfile(css,xml250,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,myname)
     call colocation_errorappend(crc250," Error return from col_getxmlfile.")
     call colocation_errorappendi(crc250,irc)
     call colocation_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine col_getxmlfile
