subroutine obs_setIndex(sid, trg80, exp250, crc250, irc)
  use observations
  implicit none
  integer :: sid             ! session id
  character*80 :: trg80      ! target name
  character*250 :: exp250    ! expression
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setIndex"
  !write(*,*) myname, 'Entering.',irc,sid
  call observation_setIndex(sid,trg80,exp250,crc250,irc)
  if (irc.ne.0) then
     call observation_errorappend(crc250,"|")
     call observation_errorappend(crc250,trim(myname))
     call observation_errorappend(crc250," Error return from observation_setIndex.")
     call observation_errorappendi(crc250,irc)
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine obs_setIndex
