subroutine plo_maketable(sid,cid,mid,oid,tab250,gra250,cat250,test,fill250,crc250, irc)
  use plot
  use model
  use observations
  use colocation
  implicit none
  integer :: sid ! plot session id
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
  character*25 :: myname = "plo_maketable"
  type(plot_session), pointer :: pss !  current session
  type(col_session), pointer ::  css !  current session
  type(mod_session), pointer ::  mss !  current session
  type(obs_session), pointer ::  oss !  current session
  if (plot_bdeb)write(*,*)myname,'Entering.',irc,test
  ! get session objects
  call model_getSession(mss,mid,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from mod_getSession.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  call observation_getSession(oss,oid,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from obs_getSession.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  call colocation_getSession(css,cid,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from plot_getSession.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  call plot_getSession(pss,sid,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,myname)
     call plot_errorappend(crc250," Error return from plot_getSession.")
     call plot_errorappendi(crc250,irc)
     call plot_errorappend(crc250,"\n")
     return
  end if
  !
  call  plot_maketable(pss,css,mss,oss,tab250,gra250,cat250,test,fill250,crc250,irc)
  if (irc.ne.0) then
     call plot_errorappend(crc250,"|")
     call plot_errorappend(crc250,trim(myname))
     call plot_errorappend(crc250," Error return from plot_maketable.")
     call plot_errorappendi(crc250,irc)
     return
  end if
  return
end subroutine plo_maketable
