subroutine col_makeXML(cid,mid,bid,crc250, irc)
  use colocation
  implicit none
  integer :: cid ! colocation session id
  integer :: mid ! model session id
  integer :: bid ! observation session id
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "col_makeXML"
  !
  write(*,'(A)') "Content-type: text/plain;"
  write(*,*)
  write(*,'(A)') "<?xml version='1.0' encoding='utf-8'?>";
  write(*,'(A)') "<url>"
  !
  call  colocation_makeXML(cid,mid,bid,crc250, irc)
  if (irc.ne.0) then
     call printError(crc250)
  end if
  !
  write(*,'(A)') "</url>"

  return
contains
  !
  subroutine printError(crc250)
    character*250 :: crc250
    integer, external :: length
    integer :: lenc
    call chop0(crc250,250)
    lenc=length(crc250,250,10)
    call replaceError(lenc,crc250,":"," ")
    call replaceError(lenc,crc250,";"," ")
    call replaceError(lenc,crc250,'"'," ")
    call replaceError(lenc,crc250,"'"," ")
    call replaceError(lenc,crc250,"/"," ")
    call replaceError(lenc,crc250,"\\"," ")
    call chop0(crc250,250)
    lenc=length(crc250,250,10)
    write(*,'(A)') "<error message=\'"//crc250(1:lenc)//"\'/>"
    return
  end subroutine printError
  !
  subroutine replaceError(lenc,crc250,c1,t1)
    integer lenc
    character*250 :: crc250
    character*1 :: c1,t1
    integer :: ii
    do ii=1,lenc
       if (crc250(ii:ii).eq.c1) then
          crc250(ii:ii)=t1
       end if
    end do
  end subroutine replaceError
  !
end subroutine col_makeXML
