subroutine mod_savedata(sid, path, crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  character*250 :: path
  character*250 :: crc250
  integer :: irc
  character*250 :: buff250
  integer :: lenc
  character*25 :: myname = "mod_savedata"
  !write(*,*) myname,'Entering.',irc,sid,path
  call model_savedata(sid,path,crc250,irc)
  if (irc.ne.0) then
     !write(*,*) 'savedata Error.'
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from model_savedata.")
     call model_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.',irc,sid
  return
end subroutine mod_savedata
