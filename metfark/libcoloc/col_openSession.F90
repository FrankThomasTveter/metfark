subroutine col_opensession(cid, crc250, irc)
  use colocation
  implicit none
  integer :: cid             ! session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "col_opensession"
  !write(*,*) myname,'Entering.',irc
  call colocation_opensession(cid,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,"|")
     call colocation_errorappend(crc250,myname)
     call colocation_errorappend(crc250," Error return from colocation_openSession.")
     call colocation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.',cid
  return
end subroutine col_opensession
