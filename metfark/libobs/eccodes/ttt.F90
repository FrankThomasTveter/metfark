program ttt
  !
  ! FORTRAN PROGRAM TO TEST ECACCESS API FOR READING BUFR MESSAGES
  !
  use eccodes
  implicit none
  !
  integer :: nval   ! length of value sequence
  integer :: nsub   ! number of sub messages
  integer :: cval=0 ! allocated number of value sequence
  integer :: csub=0 ! allocated number of sub messages
  !
  real(kind=8), dimension(:,:), allocatable :: values
  real(kind=4), dimension(:,:), allocatable :: blockNumber
  character(len=32),dimension(:)            :: units, index, code
  !
  integer            :: ifile
  integer            :: iret
  integer            :: ibufr
  integer            :: i
  integer            :: count=0
  integer(kind=4)    :: numberOfValues
  !
  integer(kind=4)    :: blockNumber
  !
  INTEGER :: iarg
  CHARACTER(len=64) :: arg
  integer :: irc

  iarg=1
  CALL GET_COMMAND_ARGUMENT(IARG,arg,irc)
  do while (irc.eq.0)
     call codes_open_file(ifile,trim(arg),'r')     
     call codes_bufr_new_from_file(ifile,ibufr,iret)
     ...
     ! wmo block number
     call codes_get(ibufr,'blockNumber',blockNumber)    ! This is the value
     call codes_get(ibufr,'blockNumber->units',units)   ! This is the units
     call codes_get(ibufr,'blockNumber->index',index)
     call codes_get(ibufr,'blockNumber->code',code)     ! This is the descriptor
     write(*,FMT='(A5,A8,A32,I6,A32)') trim(index), trim(code), "blockNumber",blockNumber, trim(units)
     !
     IARG=IARG+1
     CALL GET_COMMAND_ARGUMENT(IARG,arg,irc)
  end do
  !
end program ttt
   
