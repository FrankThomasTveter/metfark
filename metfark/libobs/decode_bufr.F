C Copyright 1981-2007 ECMWF
C 
C Licensed under the GNU Lesser General Public License which
C incorporates the terms and conditions of version 3 of the GNU
C General Public License.
C See LICENSE and gpl-3.0.txt for details.
C

       PROGRAM BUFR
C
C**** *BUFR*
C
C
C     PURPOSE.
C     --------
C         EXAMPLE OF USING BUFR UNPACKING/PACKING SOFTWARE.
C
C
C**   INTERFACE.
C     ----------
C
C          NONE.
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         CALL BUSEL2
C         CALL BUFREX
C         CALL BUFREN
C         CALL BUPRS0
C         CALL BUPRS1
C         CALL BUPRS2
C         CALL BUPRS3
C         CALL BUPRT
C         CALL BUUKEY
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2=4096,JSEC3=   4,
     1          JSEC4=2,JELEM=320000,JSUBS=400,JCVAL=150 ,JBUFL=512000,
#ifdef JBPW_64
     2          JBPW =  64,JTAB =3000,JCTAB=3000,JCTST=9000,JCTEXT=9000,
#else
     2          JBPW =  32,JTAB =3000,JCTAB=3000,JCTST=9000,JCTEXT=9000,
#endif
     3          JWORK=4096000,JKEY=46, JTMAX=10,JTCLAS=64,JTEL=255)

C
      PARAMETER (KELEM=40000)
      PARAMETER (KVALS=4096000)
C 
      DIMENSION KBUFF(JBUFL)
      DIMENSION KBUFR(JBUFL)
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY  (JKEY),KREQ(2)
      DIMENSION NREQUEST(2)
C
      REAL*8 VALUES(KVALS),VALUE(KVALS)
      DIMENSION KTDLST(JELEM),KTDEXP(JELEM),KRQ(KELEM)
      REAL*8 RQV(KELEM)
      DIMENSION KDATA(200),KBOXR(JELEM*4)
      REAL*8 VALS(KVALS)
C
      CHARACTER*256 CF,COUT,CARG(4)
      CHARACTER*64 CNAMES(KELEM),CBOXN(JELEM*4)
      CHARACTER*24 CUNITS(KELEM),CBOXU(JELEM*4)
      CHARACTER*80 CVALS(kelem)
      CHARACTER*80 CVAL(kelem)
      CHARACTER*80 YENC
      REAL*8 RVIND
      REAL*8 EPS
C
C     ------------------------------------------------------------------
C*          1. INITIALIZE CONSTANTS AND VARIABLES.
C              -----------------------------------
 100  CONTINUE
C
C     MISSING VALUE INDICATOR
C 
      NBYTPW=JBPW/8
      RVIND=1.7D38
      NVIND=2147483647
      IOBS=0
      EPS=10.D-8
      NPACK=0
      IYEAR=NVIND
      N=0
      NCOM=0
      OO=.FALSE.
C
C
C
C     GET INPUT AND OUTPUT FILE NAME.
C
      NARG=IARGC()
C
C
      DO 104 J=1,NARG
      CALL GETARG(J,CARG(J))
 104  CONTINUE

      II=0
      IO=0
      DO 105 J=1,NARG
      IF(CARG(J).EQ.'-i') THEN
         IN=J
      ELSEIF(CARG(J).EQ.'-o') THEN
         IO=J
      END IF
 105  CONTINUE
      IF(IN.EQ.0) THEN
         PRINT*,'USAGE -- decode_bufr -i infile'
         STOP
      END IF
      IF(IO.EQ.0.and.IN.EQ.0) THEN
         PRINT*,'USAGE -- decode_bufr -i infile -o outfile'
         STOP
      END IF

C
      IF(IO.NE.0)  COUT=CARG(IO+1)
C
      IF(IO.LT.IN) THEN
         IST=IN+1
         IEND=NARG
      ELSE
         IST=IN+1
         IEND=IO-1
      END IF
C
C
      IF(IO.NE.0) THEN
         JJ=INDEX(COUT,' ')
         JJ=JJ-1
         CALL PBOPEN(IUNIT1,COUT(1:JJ),'W',IRET)
         IF(IRET.EQ.-1) STOP 'OPEN FAILED ON BUFR.DAT'
         IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
         IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
      END IF
C
      DO 101 II=IST,IEND

      CF=CARG(II)
      ILN=INDEX(CF,' ')
      ILN=ILN-1

      KRQL=0
      NR=0
      KREQ(1)=0
      KREQ(2)=0
      DO 103 I=1,KELEM
      RQV(I)=RVIND
      KRQ(I)=NVIND
 103  CONTINUE
C
C*          1.2 OPEN FILE CONTAINING BUFR DATA.
C               -------------------------------
 120  CONTINUE
C
      IRET=0 
      CALL PBOPEN(IUNIT,CF(1:ILN),'R',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
C
      IF(IO.NE.0) THEN
         CALL PBOPEN(IUNIT1,COUT(1:JJ),'W',IRET)
         IF(IRET.EQ.-1) STOP 'OPEN FAILED ON BUFR.DAT'
         IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
         IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
      END IF
C

C
C
C     ----------------------------------------------------------------- 
C*          2. SET REQUEST FOR EXPANSION.
C              --------------------------
 200  CONTINUE
C
      OPRT=.FALSE.
      OENC=.FALSE.
      WRITE(*,'(A,$)') ' DO YOU WANT TO PRINT( Y/N ) : '
      READ (*,'(A)') YENC
      IF(YENC(1:1).EQ.'Y'.OR.YENC(1:1).EQ.'y') THEN
         OPRT=.TRUE.
      END IF
      ICODE=0
      WRITE(*,'(A,$)') ' CODE TABLES TO BE PRINTED ( Y/N ) : '
      READ (*,'(A)') YCODC
      IF(YCODC(1:1).EQ.'Y'.OR.YCODC(1:1).EQ.'y') THEN
         ICODE=1
      END IF
      WRITE(*,'(A,$)') ' DO YOU WANT ENCODING( Y/N ) : '
      READ (*,'(A)') YENC
      IF(YENC(1:1).EQ.'Y'.OR.YENC(1:1).EQ.'y') THEN
          OENC=.TRUE.
         WRITE(*,'(A,$)') ' NUMBER OF SUBSETS TO PACK : '
         READ(*,'(BN,I4)')   NCOM
         OCOMP=.FALSE.
         WRITE(*,'(A,$)') ' DO YOU WANT COMPRESSION( Y/N ) : '
         READ (*,'(A)') YCOMP
         IF(YCOMP(1:1).EQ.'Y'.OR.YCOMP(1:1).EQ.'y') OCOMP=.TRUE.
      END IF
      WRITE(*,'(A,$)') ' RECORD NUMBER TO START FROM : '
      READ(*,'(BN,I6)')   NR
C
 201  CONTINUE
C
      WRITE(*,'(A,$)') ' REQUESTED ELEMENT : ' 
      READ(*,'(BN,I6)')   IEL 
      WRITE(*,'(A,$)') ' REQUESTED VALUE   : '
      READ(*,'(BN,F12.2)')   VAL
      IF(IEL.EQ.0) THEN
         KRQL=J
      ELSE
         J=J+1
         KRQ(J)=IEL
         RQV(J)=VAL
         IF(VAL.EQ.0.) RQV(J)=RVIND
         GO TO 201
      END IF
C
      WRITE(*,'(A,$)') ' REQUESTED FLAG 1  : '
      READ(*,'(BN,I6)')  KREQ(1)
C
      WRITE(*,'(A,$)') ' REQUESTED FLAG 2  : '
      READ(*,'(BN,I6)')  KREQ(2)
C
      WRITE(*,'(A,$)') ' DO YOU WANT TO PRINT SECTION 0-3( Y/N ) : '
      READ (*,'(A,$)') YENC
      OSEC3=.FALSE.
      IF(YENC(1:1).EQ.'Y'.OR.YENC(1:1).EQ.'y') OSEC3=.TRUE.
C
C*          2.1 SET REQUEST FOR PARTIAL EXPANSION.
C               ----------------------------------
 210  CONTINUE
C
      IERR=0
      CALL BUSRQ(KREQ,KRQL,KRQ,RQV,IERR)
C
C     SET VARIABLE TO PACK BIG VALUES AS MISSING VALUE INDICATOR
C
      KPMISS=1
      KPRUS=0
      KOKEY=0
      CALL BUPRQ(KPMISS,KPRUS,KOKEY)
C
C     -----------------------------------------------------------------
      IF(NCOM.NE.0) THEN
         KEL1=KVALS/NCOM
         IF(KEL1.GT.KELEM) KEL1=KELEM
      END IF
C
C*          3.  READ BUFR MESSAGE.
C               ------------------
 300  CONTINUE
C
      IERR=0
      KBUFL=0
C
      CALL PBBUFR(IUNIT,KBUFF,JBUFL,KBUFL,IERR) 
      IF(IERR.EQ.-1) THEN
         IF(NPACK.NE.0) GO TO 600
         PRINT*,'NUMBER OF SUBSETS     ',IOBS
         PRINT*,'NUMBER OF MESSAGES    ',N
         STOP 'EOF'
      END IF
      IF(IERR.EQ.-2) STOP 'FILE HANDLING PROBLEM' 
      IF(IERR.EQ.-3) STOP 'ARRAY TOO SMALL FOR PRODUCT'
C
      N=N+1
c     PRINT*,'----------------------------------',N,' ',KBUFL
      KBUFL=KBUFL/NBYTPW+1
      IF(N.LT.NR) GO TO 300
C
C     -----------------------------------------------------------------
C*          4. EXPAND BUFR MESSAGE.
C              --------------------
 400  CONTINUE
C
      CALL BUS0123( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KSEC3,IERR)
      IF(IERR.NE.0) THEN
         PRINT*,'ERROR IN BUS012: ',IERR
         PRINT*,' BUFR MESSAGE NUMBER ',N,' CORRUPTED.'
         IERR=0
         GO TO 300
      END IF
C
      KEL=KVALS/KSEC3(3)
      IF(KEL.GT.KELEM) KEL=KELEM
C
      CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,KSEC1,KSEC2 ,KSEC3 ,KSEC4,
     1            KEL,CNAMES,CUNITS,KVALS,VALUES,CVALS,IERR)
C
      IF(IERR.NE.0) THEN
         CALL EXIT(2)
      END IF
C
C
      IOBS=IOBS+KSEC3(3)
C
C
      ISUBSET=1
      CALL BUSEL2(ISUBSET,KEL,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,
     1            CUNITS,IERR)
C     IF(IERR.NE.0) CALL EXIT(2)
C
C
c      DO 401 IK=1,KSEC3(3)
c
c      CALL BUSEL2(IK,KEL,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,
c    1             CUNITS,IERR)
c      KSUP(5)=KTDEXL
c      CALL BUBOX(IK,KSUP,KEL,KTDEXP,CNAMES,CUNITS,KVALS,VALUES,
c    1            KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU,IERR)
C
c 401  CONTINUE
C
C
C*          4.1 PRINT CONTENT OF EXPANDED DATA.
C               -------------------------------
 410  CONTINUE
C
      IF(.NOT.OPRT) GO TO 500
      IF(.NOT.OSEC3) GO TO 450
C
C*          4.2 PRINT SECTION ZERO OF BUFR MESSAGE.
C               -----------------------------------
 420  CONTINUE
C

      CALL BUPRS0(KSEC0)
C
C*          4.3 PRINT SECTION ONE OF BUFR MESSAGE.
C               -----------------------------------
 430  CONTINUE
C
      CALL BUPRS1(KSEC1)
C
C
C*          4.4 PRINT SECTION TWO OF BUFR MESSAGE.
C               -----------------------------------
 440  CONTINUE
C
C              AT ECMWF SECTION 2 CONTAINS RDB KEY.
C              SO UNPACK KEY
C
      CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,IERR)
C
C              PRINT KEY
C
      CALL BUPRS2(KSUP ,KEY)
C
C*          4.5 PRINT SECTION 3 OF BUFR MESSAGE.
C               -----------------------------------
 450  CONTINUE
C
C               FIRST GET DATA DESCRIPTORS
C
C     Multi subset uncompressed data descriptors for the 1st subset
C     Each subset can contain completly different list of expanded
C     discriptors
C
      ISUBSET=1
      CALL BUSEL2(ISUBSET,KEL,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,
     1            CUNITS,IERR)
      IF(IERR.NE.0) CALL EXIT(2)
C
C               PRINT  CONTENT
C
      IF(OSEC3) THEN
         CALL BUPRS3(KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,KEL,CNAMES)
      END IF
C
C*         4.6 PRINT SECTION 4 (DATA).
C              -----------------------
 460  CONTINUE
C
C          IN THE CASE OF MANY SUBSETS DEFINE RANGE OF SUBSETS
C
      IF(.NOT.OO) THEN
      WRITE(*,'(A,$)') ' STARTING SUBSET TO BE PRINTED : '
      READ(*,'(BN,I4)')   IST
      WRITE(*,'(A,$)') ' ENDING SUBSET TO BE PRINTED : '
      READ(*,'(BN,I4)')   IEND
      OO=.FALSE.
      END IF
C
C              PRINT DATA
C
C     IF(KSEC1(6).EQ.11) THEN
C
C        IST=1
C        IEND=KSEC3(3)
C
         CALL BUPRT(ICODE,IST,IEND,KEL,CNAMES,CUNITS,CVALS,
     1              KVALS,VALUES,KSUP,KSEC1,IERR)
C     ELSE
C
C              RESOLVE BIT MAPS FOR EACH SUBSET
C
C        ist=1
C        iend=ksec3(3)
C
C        IF(IEND.GT.KSEC3(3)) IEND=KSEC3(3)
C
C        DO 461 IK=IST,IEND
C 
C        CALL BUSEL2(IK,KEL,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,
C    1               CUNITS,IERR)
C
C        KSUP(5)=KTDEXL
C        CALL BUBOX(IK,KSUP,KEL,KTDEXP,CNAMES,CUNITS,KVALS,VALUES,
C    1              KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU,IERR)
C        IF(IERR.NE.0) CALL EXIT(2)
C 
C        CALL BUPRTBOX(KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU)
C
C461     CONTINUE
C     END IF
C
C     -----------------------------------------------------------------
C*          5. COLLECT DATA FOR REPACKING.
C              ---------------------------
 500  CONTINUE
C      
C 
      IF(.NOT.OENC) GO TO 300
C
      ISUBS=KSEC3(3)
      DO J=1,ISUBS
C
      NPACK=NPACK+1
C
C            FIRST GET DATA DESCRIPTORS
C
      CALL BUSEL2(J,KEL,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,
     1            CUNITS,IERR)
      IF(IERR.NE.0) CALL EXIT(2)
C
      DO I=1,KTDEXL
      IO=I+(NPACK-1)*KEL1
      IN=I+(J-1)*KEL
C
      IF(CUNITS(I).EQ.'CCITTIA5') THEN
         IPOS =VALUES(IN)/1000.
         ICH=NINT(VALUES(IN)-IPOS*1000)
         KKK=KKK+1
         VALUE(IO)=KKK*1000+ICH
         CVAL(KKK)=CVALS(IPOS)
      ELSE
         VALUE(IO)=VALUES(IN)
      END IF
      IF(KTDEXP(I).EQ.31001.OR.KTDEXP(I).EQ.31002) THEN
          KK=KK+1
          KDATA(KK)=NINT(VALUE(IO))
       END IF
       IF(KTDEXP(I).EQ.004001) THEN
          IYEAR=NINT(VALUE(IO))
       END IF

       END DO
C
       KDLEN=KK
       IF(NPACK.EQ.NCOM) THEN
         

          KSEC3(3)=NPACK
          KSEC1(5)=0
          KSEC1(8)=1
          KSEC1(15)=12
          IF(KSEC0(3).LT.4) THEN
             KSEC1(17)=255
             KSEC1(18)=0
          END IF
          KSEC0(3)=3            ! EDITION 4 OF BUFR MESSAGE
          IF(KSEC0(3).GE.4) KSEC1(1)=22
          KSEC3(4)=0            ! NO COMPRESSION
          IF(KSEC0(3).LE.3) THEN
             IF(IYEAR.GT.2000) THEN
                KSEC1(9)=IYEAR-2000
             ELSE
                KSEC1(9)=IYEAR-1900
             END IF
          ELSE
             KSEC1(9)=IYEAR
          END IF
          IF(OCOMP) KSEC3(4)=64  ! COMPRESSION
          KBUFL=JBUFL
          CALL BUFREN( KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     1             KTDLEN,KTDLST,KDLEN,KDATA,KEL1,
     2             KVALS,VALUE,CVAL,KBUFL,KBUFR,IERR)
          IF(IERR.NE.0) THEN
             PRINT*,'ERROR IS ',IERR
             PRINT*,'ERROR DURING ENCODING.'
             CALL EXIT(2)
          END IF
C
          ILEN=KBUFL*NBYTPW
C
          IERR=0
C
          CALL PBWRITE(IUNIT1,KBUFR,ILEN,IERR)
          IF(IERR.LT.0) THEN
             PRINT*,'ERROR WRITING INTO TARGET FILE.'
             CALL EXIT(2)
          END IF
          PRINT*,'RECORD WRITTEN INTO FILE '
C
          NPACK=0
          KKK=0
          KK=0
       END IF
C
       END DO
C
       GO TO 300
C     -----------------------------------------------------------------
C*          6. PACK BUFR MESSAGE BACK INTO BUFR.
C              ---------------------------------
 600  CONTINUE
C
     
       KSEC3(3)=NPACK
       KSEC1(8)=1
       KSEC1(15)=12
       KSEC0(3)=4            ! EDITION 4 OF BUFR MESSAGE
       IF(KSEC0(3).GE.4) KSEC1(1)=22
       IF(KSEC0(3).LT.4) THEN
          KSEC1(17)=255
          KSEC1(18)=0
       END IF

       KSEC3(4)=0            ! NO COMPRESSION
       IF(KSEC1(9).LT.101) THEN
          KSEC1(9)=IYEAR
       END IF
       
C      
       IF(OCOMP) KSEC3(4)=64  ! COMPRESSION  
       KBUFL=JBUFL
C
C
C*          6.2 ENCODE DATA INTO BUFR MESSAGE.
C               ------------------------------
 620   CONTINUE
C
       CALL BUFREN( KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     1             KTDLEN,KTDLST,KDLEN,KDATA,KEL1,
     2             KVALS,VALUE,CVAL,KBUFL,KBUFR,IERR)
       IF(IERR.NE.0) THEN
          PRINT*,'ERROR IS ',IERR
          PRINT*,'ERROR DURING ENCODING.'
          CALL EXIT(2)
       END IF
C
C           6.3 WRITE PACKED BUFR MESSAGE INTO FILE.
C               ------------------------------------
 630   CONTINUE
C
       ILEN=KBUFL*NBYTPW
C    
       CALL PBWRITE(IUNIT1,KBUFR,ILEN,IERR)
       IF(IERR.LT.0) THEN
          PRINT*,'ERROR WRITING INTO TARGET FILE.'
          CALL EXIT(2)
       END IF
       PRINT*,'RECORD WRITTEN INTO FILE ' 
C
       NPACK=0
       KKK=0
C
       GO TO 300
C     -----------------------------------------------------------------
C
 810  CONTINUE
C
      WRITE(*,'(1H ,A)') 'OPEN ERROR ON INPUT FILE'
      GO TO 900
C      
 800  CONTINUE
C
      IF(IRET.EQ.-1) THEN
         PRINT*,'NUMBER OF RECORDS PROCESSED ',N
         PRINT*,'NUMBER OF OBSERVATIONS      ',IOBS
      ELSE
         PRINT*,' BUFR : ERROR= ',IERR
      END IF
C
 900  CONTINUE
C
      CALL PBCLOSE(IUNIT,IRET)
 101  CONTINUE
      CALL PBCLOSE(IUNIT1,IRET)
C
      END
