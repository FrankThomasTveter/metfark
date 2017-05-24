C Copyright 1981-2012 ECMWF. 
C
C This software is licensed under the terms of the GNU Lesser 
C General Public License Version 3 which can be obtained at 
C http://www.gnu.org/licenses/lgpl.html.  
C 
C In applying this licence, ECMWF does not waive the privileges 
C and immunities granted to it by virtue of its status as an 
C intergovernmental organisation nor does it submit to any
C jurisdiction. 
C 

	SUBROUTINE PBGRIB(KUNIT,KARRAY,KINLEN,KOUTLEN,KRET)
C
C---->
C**** PBGRIB
C
C     PURPOSE
C     _______
C
C     Reads next GRIB product from a file.
C
C     INTERFACE
C     _________
C
C     CALL PBGRIB(KUNIT,KARRAY,KINLEN,KOUTLEN,KRET)
C
C
C     Input parameters
C     ________________
C
C     KUNIT  - 	Unit number for the file returned from PBOPEN
C     KARRAY -	FORTRAN array big enough to hold the GRIB product
C     KINLEN -	Size in BYTES of the FORTRAN array
C
C
C     Output parameters
C     ________________
C
C     KOUTLEN -	Size in BYTES of the GRIB product read	
C     KRET    -	 0  if a GRIB product has been successfully read
C		-1  if end-of-file is hit before a GRIB product is read
C		-3  if the size of KARRAY is not sufficient for the 
C                   GRIB product
C
C
C     Common block usage
C     __________________
C
C     None.
C
C
C     Method
C     ______
C
C     Calls GRIBREAD.
C
C
C     Externals
C     _________
C
C     GRIBREAD - Read next GRIB product.
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers       ECMWF
C
C
C     MODIFICATIONS
C     _____________
C
C     None.
C
C******************************************************************************
C----<
C
C     Subroutine arguments
C
      INTEGER KARRAY(1)
      INTEGER KUNIT,KINLEN,KOUTLEN,KRET
C
C     Local argument(s)
C
      INTEGER NREAD, IRET
C
C     Get the GRIB product
C
      CALL GRIBREAD( KARRAY, KINLEN, NREAD, IRET, KUNIT )
C
C     Escape if the user buffer is too small to hold even the early sections of
C     the product or EOF encountered
C
      IF( IRET.EQ.-4 ) THEN
        KOUTLEN = NREAD
        KRET = -1
        RETURN
      ENDIF
C
C     Escape if no GRIB product is found in the file
C
      IF( IRET.EQ.-1 ) THEN
        KOUTLEN = 0
    	KRET = -1
    	RETURN
      ENDIF
C
C     Check if the array is big enough for the GRIB product
C
      IF( IRET.EQ.-3 ) THEN
        KOUTLEN = NREAD
    	KRET = -3
    	RETURN
      ENDIF
C
C     Set success code if product retrieved
C
      IF( NREAD.GE.0 )  THEN
    	KOUTLEN = NREAD
    	KRET = 0
      ENDIF
C
      RETURN 
      END
