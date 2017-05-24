      PROGRAM NCLL
C
C     NCLL PROGRAM FOR DATA TEST READ
C
      IMPLICIT NONE
      SAVE
C
      INTEGER  UNITI,IRC
      CHARACTER*10 MYNAME
      DATA MYNAME /'NCLL'/
      LOGICAL  BDEB,ACTIVE
      DATA BDEB /.FALSE./
      DATA ACTIVE /.FALSE./
C
      integer ii
      integer lenr, length
      external length
C
      IRC=0
      UNITI=5
C
C     Debug System.
C
      IF(.NOT.ACTIVE)CALL DEBUG(MYNAME,BDEB,ACTIVE)
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Program starts.',IRC
C      CALL BLOBB

      CALL MNCLL(UNITI,IRC)
      IF (IRC.NE.0) THEN
         WRITE(*,*) MYNAME,'Error return from MNCLL.',IRC
      ENDIF
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Program ends.',IRC
C
      IF (IRC.EQ.0) THEN
         WRITE(*,*) MYNAME,'-------NORMAL END OF PROGRAM-------'
      ELSE
         WRITE(*,*) MYNAME,'--------ERROR WHILE RUNNING--------',IRC
      ENDIF
C
      CALL exit(IRC)
C
      END
