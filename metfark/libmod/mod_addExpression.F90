subroutine mod_addExpression(cid, nam80, expr250, l80, u80, crc250, irc)
  use model
  implicit none
  integer :: cid             ! session id
  character*80 :: nam80
  character*250 :: expr250
  character*80 :: l80
  character*80 :: u80
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "pushmatchrule"
  !write(*,*) myname, 'Entering.',irc,cid
  call model_addexpression(cid,nam80,expr250,l80,u80,crc250,irc)
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
