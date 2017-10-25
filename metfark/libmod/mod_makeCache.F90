subroutine mod_makeCache(sid, path, test, crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  character*250 :: path
  integer :: test
  character*250 :: crc250
  integer :: irc
  character*250 :: buff250
  integer :: lenc
  character*25 :: myname = "mod_makeCache"
  type(mod_session), pointer :: css !  current session
  !write(*,*) myname,'Entering.',irc,sid,path
  call model_getSession(css,sid,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from getSession.")
     call model_errorappendi(crc250,irc)
     call model_errorappend(crc250,"\n")
     return
  end if
  call model_makecache(css,path,test,crc250,irc)
  if (irc.ne.0) then
     !write(*,*) 'pushFile Error.'
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,myname)
     call model_errorappend(crc250," Error return from model_makecache.")
     call model_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,'Done.',irc,sid
  return
end subroutine mod_makeCache
