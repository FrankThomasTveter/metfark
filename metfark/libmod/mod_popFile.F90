subroutine mod_popfile(sid,path250,crc250,irc)
  use model
  implicit none
  integer :: sid             ! session id
  character*250 :: path250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "popFile"
  !write(*,*) myname,'Entering.',irc
  call model_popfile(sid,path250,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from model_stackpop.")
     call model_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.'
  return
end subroutine mod_popfile
