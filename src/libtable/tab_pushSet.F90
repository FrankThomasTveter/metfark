subroutine tab_pushset(sid,cid,mid,oid,nam80,leg250,crc250, irc)
  use table
  use model
  use observations
  use colocation
  implicit none
  integer :: sid ! table session id
  integer :: cid ! coloc session id
  integer :: mid ! model session id
  integer :: oid ! observation session id
  character*80 :: nam80
  character*250 :: leg250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "tab_pushset"
  type(table_session), pointer :: pss !  current session
  type(col_session), pointer ::  css !  current session
  type(mod_session), pointer ::  mss !  current session
  type(obs_session), pointer ::  oss !  current session
  !write(*,*)myname,'Entering:',irc,sid,cid,mid,oid,nam80
  ! get session objects
  call model_getSession(mss,mid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from mod_getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  call observation_getSession(oss,oid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from obs_getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  call colocation_getSession(css,cid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from table_getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !
  call table_getSession(pss,sid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !
  call  table_pushset(pss,css,mss,oss,nam80,leg250,crc250, irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from table_pushset.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !write(*,*)myname,'Exiting:',irc,sid,cid,mid,oid,nam80
  return
end subroutine tab_pushset
