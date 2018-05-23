subroutine par_clearshapefile(crc250, irc)
  use parse
  implicit none
  character*250 :: fn250
  character*11 :: cn11
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "clearshapefile"
  !write(*,*) myname, 'Entering.',irc
  call parse_clearshapefile(crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,"|")
     call colocation_errorappend(crc250,trim(myname))
     call colocation_errorappend(crc250," Error return from parse_clearshapefile.")
     call colocation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine par_clearshapefile
