subroutine tab_jointablefile(sid,cid,mid,oid,tab250,gra250,cat250,test,fill250,crc250, irc)
  use table
  use model
  use observations
  use colocation
  implicit none
  integer :: sid ! table session id
  integer :: cid ! coloc session id
  integer :: mid ! model session id
  integer :: oid ! observation session id
  character*250 :: tab250
  character*250 :: gra250
  character*250 :: cat250
  integer :: test
  character*250 :: fill250
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "tab_jointablefile"
  type(table_session), pointer :: pss !  current session
  type(col_session), pointer ::  css !  current session
  type(mod_session), pointer ::  mss !  current session
  type(obs_session), pointer ::  oss !  current session
  !if (table_bdeb)write(*,*)myname,'Entering.',irc,test
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
  call table_getSession(pss,sid,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from table_getSession.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  !
  call  table_jointablefile(pss,css,mss,oss,tab250,gra250,cat250,test,fill250,crc250,irc)
  if (irc.ne.0) then
     call table_errorappend(crc250,myname)
     call table_errorappend(crc250," Error return from table_jointablefile.")
     call table_errorappendi(crc250,irc)
     call table_errorappend(crc250,"\n")
     return
  end if
  return
end subroutine tab_jointablefile
