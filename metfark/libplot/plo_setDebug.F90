subroutine plo_setdebug(ideb)
  use observations
  use model
  use colocation
  use plot
  use parse
  implicit none
  integer :: ideb
  logical :: bdeb
  if (ideb.eq.0) then
     obs_bdeb=.false.   ! observations
     model_bdeb=.false. ! model
     col_bdeb=.false.   ! colocation
     plot_bdeb=.false.  ! plot
     parse_bdeb=.false. ! parse
  else if (abs(ideb).eq.1) then ! obs
     if (ideb.gt.0) then
        obs_bdeb=.true.
     else
        obs_bdeb=.false.
     end if
  else if (abs(ideb).eq.2) then ! model
     if (ideb.gt.0) then
        model_bdeb=.true.
     else
        model_bdeb=.false.
     end if
  else if (abs(ideb).eq.3) then ! col
     if (ideb.gt.0) then
        col_bdeb=.true.
     else
        col_bdeb=.false.
     end if
  else if (abs(ideb).eq.4) then ! plot
     if (ideb.gt.0) then
        plot_bdeb=.true.
     else
        plot_bdeb=.false.
     end if
  else if (abs(ideb).eq.5) then ! parse
     if (ideb.gt.0) then
        parse_bdeb=.true.
     else
        parse_bdeb=.false.
     end if
  end if
  return
end subroutine plo_setdebug
