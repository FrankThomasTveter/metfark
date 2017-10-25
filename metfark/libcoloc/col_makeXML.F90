subroutine col_makexml(cid,mid,oid,xml250,test,crc250, irc)
  use model
  use observations
  use colocation
  implicit none
  integer :: cid ! coloc session id
  integer :: mid ! model session id
  integer :: oid ! observation session id
  character*250 :: xml250
  integer :: test
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "col_makexml"
  type(col_session), pointer ::  css !  current session
  type(mod_session), pointer ::  mss !  current session
  type(obs_session), pointer ::  oss !  current session
  if (col_bdeb)write(*,*)myname,'Entering.',irc,test
  ! get session objects
  call model_getSession(mss,mid,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,myname)
     call colocation_errorappend(crc250," Error return from mod_getSession.")
     call colocation_errorappendi(crc250,irc)
     call colocation_errorappend(crc250,"\n")
     return
  end if
  call observation_getSession(oss,oid,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,myname)
     call colocation_errorappend(crc250," Error return from obs_getSession.")
     call colocation_errorappendi(crc250,irc)
     call colocation_errorappend(crc250,"\n")
     return
  end if
  call colocation_getSession(css,cid,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,myname)
     call colocation_errorappend(crc250," Error return from colocation_getSession.")
     call colocation_errorappendi(crc250,irc)
     call colocation_errorappend(crc250,"\n")
     return
  end if
  !
  call  colocation_makexml(css,mss,oss,xml250,test,crc250,irc)
  if (irc.ne.0) then
     call colocation_errorappend(crc250,"|")
     call colocation_errorappend(crc250,trim(myname))
     call colocation_errorappend(crc250," Error return from colocation_makexml.")
     call colocation_errorappendi(crc250,irc)
     return
  end if
  return
end subroutine col_makexml
