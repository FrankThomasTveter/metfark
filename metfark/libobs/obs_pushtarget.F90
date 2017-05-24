subroutine obs_pushtarget(sid,trg,pos,descr,info,&
     & min,max,crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character(len=*) :: trg      ! target name
  character(len=*) :: pos      ! target name
  character(len=*) :: descr      ! target name
  character(len=*) :: info      ! target name
  character(len=*) :: min      ! target name
  character(len=*) :: max      ! target name
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "pushtarget"
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_pushtarget(sid,trg,pos,descr,info,&
       & min,max,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_pushtarget.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_pushtarget
