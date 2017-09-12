subroutine mod_addExpression(sid, nam80, expr250, l80, u80, crc250, irc)
  use model
  implicit none
  integer :: sid             ! session id
  character*80 :: nam80
  character*250 :: expr250
  character*80 :: l80
  character*80 :: u80
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "pushmatchrule"
  type(mod_session), pointer :: css !  current session
  !write(*,*) myname, 'Entering.',irc,sid
    call model_getSession(css,sid,crc250,irc)
    if (irc.ne.0) then
       call model_errorappend(crc250,myname)
       call model_errorappend(crc250," Error return from getSession.")
       call model_errorappendi(crc250,irc)
       call model_errorappend(crc250,"\n")
       return
    end if
  call model_addexpression(css,nam80,expr250,l80,u80,crc250,irc)
  if (irc.ne.0) then
     call model_errorappend(crc250,"|")
     call model_errorappend(crc250,trim(myname))
     call model_errorappend(crc250," Error return from model_addExpression.")
     call model_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine mod_addExpression
