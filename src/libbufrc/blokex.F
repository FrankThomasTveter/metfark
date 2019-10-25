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


      Subroutine BLOKEX (Ksec0,Ksec1,Ksec2,Klen2,
     &                   Kblok,Kbdim,Kword,Kmaxw,Hoper,Hform,Kret)
C---->
c**
c****  Name  *BLOKEX*
c****  ----
c**
c**   Purpose
c**   -------
c**       1) Block data in BLOK code, Edition 0.
c**       2) Unblock data from BLOK code.
c**       3) Decode only identification section of BLOK
c**          coded data ie Sections 0 and 1.
c**
c**          A number of options exist when coding or decoding data.
c**          See values allowed for requested function, Hoper, Below.
c**
c**   Interface
c**   ---------
c**
c**       Call BLOKEX (Ksec0,Ksec1,Ksec2,Klen2,
c**  &                 Kblok,Kbdim,Kword,Kmaxw,Hoper,Hform,Kret)
c**
c**
c**       Integer   K*
c**       Character H*
c**       
c**       Input parameters for all functions:
c**       -----------------------------------
c**
c**             Hoper - Requested function:
c**
c**                     'B' To code data in BLOK code. The input, 
c**                         when using this function, is the full
c**                         data array (original parent form) Ksec2.
c**                         The output array, Kblok, in this case
c**                         contains all related BLOK messages. The
c**                         length of each BLOK message is Kmaxw,
c**                         except the last one which length is 
c**                         (Kword-(Ksec0(5)-1)*Kmaxw).
c**
c**                     'C' To code data in BLOK code. The input,
c**                         when using this function, is the part of the
c**                         data array (origianl parent form) Ksec2 
c**                         which will be coded in BLOK code.
c**
c**                     'F' To unblock data from BLOK code. By 
c**                         successive calling Blokex with this
c**                         function, it will collect related BLOK
c**                         messages until the original parent form is
c**                         achieved (when the return code is 0).
c**
c**                     'I' To decode only identification
c**                         sections 0 and 1 of BLOK code.
c**
c**                     'L' Return length of BLOK message, in
c**                         bytes, and BLOK edition number only.
c**
c**                     'U' To unblock single BLOK message.
c**
c**             Kbdim - Dimension of array Kblok
c**
c**             Kret  - Response to error indicator
c**
c**                     0         , Abort if error encountered.
c**                     Non-zero  , Return to calling routine
c**                                 even if error encountered.
c**
c**       Input parameters for coding functions.
c**       --------------------------------------
c**             Section   Word   Contents:
c**             -------   ----   ---------
c**             Ksec0       4    Flag;
c**
c**                        decimal
c**                        value     meaning
c**                        -----     -------
c**                           0      Section 1 to be omitted
c**                         128      Section 1 to be present
c**
c**       Input parameters for coding functions.
c**       Output parameters for decoding functions.
c**       -----------------------------------------
c**             Ksec2 - Integer parameters of section 2 (Data
c**                     section) of BLOK code.
c**
c**             Klen2 - Length of array Ksec2
c**
c**             Kmaxw - Maximun length of single BLOK message
c**
c**       Output parameters for coding functions.
c**       Input parameters for decoding functions.
c**       ----------------------------------------
c**             Kblok - Integer parameters of BLOK coded data array.
c**
c**       Output parameters for decoding functions.
c**       -----------------------------------------
c**             Ksec0      - Integer parameters of section 0 of 
c**                          BLOK code.
c**                          Integer array of 7 words.
c**
c**                 Word   Contents:
c**                 ----   ---------
c**                   1    Total length of this BLOK
c**                   2    BLOK code edition number
c**                        (currently edition 0)
c**                   3    Total length of parent form
c**                   4    Flag;
c**
c**                        decimal
c**                        value     meaning
c**                        -----     -------
c**                           0      Section 1 omitted
c**                         128      Section 1 present
c**
c**                   5    Total number of related BLOKs
c**                   6    Sequence number of this BLOK
c**                   7    Reserved
c**
c**
c**             Ksec1      - Integer parameters of section 1 
c**                          of BLOK code (contains integer parameters
c**                          of section 0 and identification section
c**                          of the parent form).
c**                          When coding data, if Section 1 requested to be
c**                          present (Ksec0(4)=128) then:
c**                          Function 'B' requested - Ksec1 is extracted
c**                          from the parent form and inserted in each related
c**                          BLOK.
c**                          Function 'C' requested - Ksec1 is extracted from
c**                          the parent form when the Ksec0(6)=1 and saved to
c**                          be inserted in each related BLOK (untill the
c**                          Ksec0(6)=1 again).
c**
c**       Output parameters for all functions.
c**       ------------------------------------
c**             Kword      - Number of words of Kblok occupied by
c**                          coded data. Output parameter for coding
c**                          function only. Not required as input
c**                          for decoding.
c**
c**             Hform      - Parent data format description ('GRIB',
c**                          'BUFR','    ').
c**
c**             Kret       - return code.
c**
c**                           0  , no errors encountered (when 'F'
c**                                function requested, also means
c**                                that the all related blocks unblocked
c**                                and original parent form achieved).
c**
c**                          Kret leter then 0 - Informative codes for
c**                                              decoding functions.
c**
c**                          -1  , input array (Kblok) does not contain
c**                                BLOK coded data.
c**                          -2  , 'F' function requested, all 
c**                                related blocks not yet unblocked and
c**                                and the original parent form not yet
c**                                achieved.
c**                                
c**                          Kret greater then 0 - Error codes.
c**
c**                               Error codes are described by the
c**                               appropriate output error messages when
c**                               encountered.
c**
c**    Method
c**    ------
c**       Input data packed in BLOK code in accordance with
c**       parameters given or set by user or fully or partially
c**       unpacked, depending on function used.
c**
c**    Externals
c**    ---------
c**       *Abortx* - Terminate execution of the job.
c**       *Inxbit* - Contains calls to the routines gbyte(s)
c**                  and sbyte(s) or their equivalents.
c**       *Setpar* - Set number of bits in the computer word
c**                  (and largest negative number).
c**
c**   Reference.
c**   ----------
c**       WMO manual on codes for BLOK definition.
c**
c**   Comments.
c**   ---------
c**       Versions of source code exist for the vax, cyber, ibm
c**       and sun workstation, as well as the cray.
c**
c**             The following defaults are used. They have been selected
c**             to facilitate the most frequent usage at ECMWF.
c**
c**             1) by default debug printout is switched off.
c**                Call grsdbg (i) where
c**                            i=non-zero, to switch on debug printout.
c**                                     0, to switch off debug printout.
c**
c**             2) by default the rounding of 120 octets is switched on.
c**                Call grsrnd (i) where
c**                            i=non-zero, to switch on rounding
c**                                     0, to switch off rounding
c**
c**    Author
c**    ------
c**       D.Jokic,  ECMWF,  Feb-1992. 
c**
C----<
c*
c*    Prefix conventions for variable names.
c
c     Character    H, dummy arguments,
c                  Y, local variables.
c     Integer      N, global.
c                  K, dummy arguments.
c                  I, local variables.
c
      Implicit none
c
      Character*1 Hoper
      Character*4 Hform
c
      Character*1 Ycoper
      Character*1 Ydoper
      Character*1 Yoper
c
      Integer i
      Integer Ibits
      Integer Ibufr
      Integer Icoun
      Integer Idata
      Integer Ie
      Integer Ieblk
      Integer Iedit
      Integer Iendm
      Integer Igrib
      Integer Iilen
      Integer Ilen1
      Integer Ilen
      Integer Iname
      Integer Inblok
      Integer Initial
      Integer Inspt
      Integer Insptl
      Integer Imisng
      Integer Ioff
      Integer Iorder
      Integer Iret
      Integer Is
      Integer Isec1
      Integer Itblen
      Integer Itplen
      Integer Itemp
      Integer Iword2
      Integer Iword
c
      Integer Jbedno
      Integer Jplen1
c
      Integer Kblok
      Integer Kbdim
      Integer Klen2
      Integer Kret
      Integer Ksec0
      Integer Ksec1
      Integer Ksec2
      Integer Kword
      Integer Kmaxw
c
      Integer Nblok
      Integer Nbcoun
      Integer Nbseqn
      Integer Ndbg
      Integer Nfcoun
      Integer Nflag
      Integer Nrnd
      Integer N7777
      Integer Nuser
c
c     Blok code version number used when coding data.
c
      Parameter (Jbedno=0)
c
c     Length (in octets) used for section 1, when coding data.
c
      Parameter (Jplen1=36)
c
      Dimension Kblok(*)
      Dimension Ksec0(*)
      Dimension Ksec1(*)
      Dimension Ksec2(*)
c
      Dimension Nblok(4)
      Dimension Ibufr(4)
#ifdef CRAY
      Dimension Idata(Kbdim)
#else
      Dimension Idata(34000)
#endif
      Dimension Ieblk(4)
      Dimension Iendm(4)
      Dimension Igrib(4)
      Dimension Iname(4)
      Dimension N7777(4)
      Dimension Isec1(Jplen1)
c
c
c     Characters BLOK and 7777 in ascii for use in sections 0 and 3
c     of BLOK code.
c
      Data Nblok /66,76,79,75/
      Data N7777 /55,55,55,55/
c
c     Characters GRIB and BUFR in ascii.
c
      Data Igrib /71,82,73,66/
      Data Ibufr /66,85,70,82/
c
c     Ycoper - Default operator value for all coding functions	
c     Ydoper - Default operator value for all decoding functions	
c
      Data Ycoper /'C'/
      Data Ydoper /'D'/
c
c     First Call flag
c
      Data Initial /0/
c
      Real Fref
      Integer Nfref,Nvck
      Common /GRBCOM/ Fref,Nfref,Nrnd,Ndbg,Nvck,Nuser
c
      Save Nblok,N7777
      Save Nbseqn,Nbcoun,Nfcoun
      Save Nflag
      Save Initial,Ibits,Itplen
      Save Ilen1,Isec1
c
c     ------------------------------------------------------------------
c
c*        1.   Initialise
c              ----------
  100 Continue
c
c     Set number of bits per computer word and debug indicator,
c     if first time through (Imisng is not using).
c
      If (Initial.eq.0) Then
          Call Setpar (Ibits,Imisng,Ndbg)
          Initial=1
c
c         Internal length of the parent form
c
          Itplen=0
c
c         Nflag - Flag ; 0 - 'F' function not requested
c                        1 - 'F' function requested and full
c                             parent form uncompleted
c                        2 - 'F' function requested, full
c                             parent form uncompleted and fields are
c                             not in right order
c                        3 - 'F' function requested, full
c                             parent form completed and fields are
c                             not in right order
c
          Nflag=0
c
c         Field counters
c
          Nbcoun=1
          Nfcoun=0
c
c         Sequence number
c
          Nbseqn=0
c
      End if
c
c*        1.1  Initialise variables
c
  110 Continue
c
c     Internal return code
c
      Iret=0
c
c     Bit-pointer
c
      Inspt=0
c
c     Number of words in the single BLOK message
c
      Iword=0
      Kword=0
c
c*        1.2  Set parameters
c
  120 Continue
c
c     Set default values for parameters, if values not already set,
c     either on previous call or by user via calls to the grs--- 
c     routines.
c
      If (Nuser.ne.11041967) Then
c
c         Set rounding to 120 bytes on
c
          Nrnd=1
c
c         Set debug print off
c
c         Ndbg=0
c
c         Mark common values set
c
          Nuser=11041967
c
      End if
c
c     When coding, print input parameters, if required.
c
      If (Ndbg.eq.1) Then
          Write (*,*) '/Blokex/ Paragraph 1.'
          If (Hoper.eq.'C') Then
              Call Blprs0 (Ksec0)
          End if
      End if
c
c
c     ------------------------------------------------------------------
c
c*        2.   Check input parameters and reset appropriate variables
c              -----------------------------------------------------
  200 Continue
c
      If (Ndbg.eq.1) Write (*,*) '/Blokex/ Paragraph 2.'
c
c*        2.1  Check that valid function has been requested
c
  210 Continue
      If (Hoper.ne.'B'.and.Hoper.ne.'C'.and.
     &    Hoper.ne.'I'.and.Hoper.ne.'L'.and.
     &    Hoper.ne.'U'.and.Hoper.ne.'F'     ) Then
          Iret=210
          Write (*,9210) Hoper,Iret
          Go to 910
      End if
c
c     Set internal function parameter
c
      If (Hoper.eq.'B'.or.Hoper.eq.'C') Then
          Yoper='B'
      Else
          Yoper='U'
      End if
c
c*        2.2  Rounding of 120 bytes
c
  220 Continue
      If (Yoper.eq.'B') Then
c
          If (Nrnd.eq.1) Then
              If (Mod(Kmaxw*Ibits,120).ne.0) Then
                  Iret=220
                  Write(*,9220) Kmaxw,Nrnd,Iret
                  Go to 910
              End if
          End if
c
c*        2.3  Check BLOK identification section flag - Ksec0(4)
c
  230 Continue
          If (Ksec0(4).ne.0.and.Ksec0(4).ne.128) Then
              Iret=230
              Write(*,9230) Ksec0(4),Iret
              Go to 900
          End if
c
c         Reset length of BLOK identification section
c
          If (Ksec0(4).eq.0) Ilen1=0
c
c*        2.4  Check dimensions and preset output array to 0
c
  240 Continue
c
c         Check dimension Kbdim of the output array Kblok against the
c         the length of the parent form Klen2
c
          If (Kbdim.le.Klen2) Then
              Iret=240
              Write(*,9240) Kbdim,Klen2,Iret
              Go to 900
          End if
c
c         Array to receive coded BLOK data
c
          Do 242 i=1,Kbdim
          Idata(i)=0
  242     Continue
c
c         First and last position for the extraction from the Ksec2
c
          Is=0
          Ie=0
c
      End if
c
c*        2.5  Extract section 0 and section 1 of the parent code and
c*             make identification section of BLOK code (if BLOK
c*             identification section requested Ksec0(4)=128)
c
  250 Continue
c
c     Do the extraction if function 'B' requested or
c     function 'C' requested and BLOK sequence number is 1
c
      If (Hoper.eq.'B'.or.(Hoper.eq.'C'.and.Ksec0(6).eq.1)) Then
c
          If (Ksec0(4).ne.0) Then
c
c             Parent code edition number; Extract field
c
              Itemp=56
              Call Inxbit (Ksec2,Klen2,Itemp,Iedit,1,Ibits,8,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=251
                  Write (*,9101) 'parent code edition number.',Iret
                  Go to 900
              End if
c
              If (Iedit.eq.0) Itemp=32
c
c             Length of the parent identification section;
c             Extract field
c
              Call Inxbit (Ksec2,Klen2,Itemp,Ilen1,1,Ibits,24,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=252
                  Write (*,9101)
     &            'length of the pareant identification section.',Iret
                  Go to 900
              End if
              If (Ilen1.gt.Jplen1) Then
                  Iret=2521
                  Write (*,9251) Iret
                  Go to 900
              End if
c
c             Length of the BLOK identification section
c
              Itemp=Itemp-24
              Ilen1=Ilen1+Itemp/8
c
c             Parent section 0 and section 1;
c             Extract field
c
              Itemp=0
              Call Inxbit (Ksec2,Klen2,Itemp,Isec1(1),Ilen1,Ibits,8,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=253
                  Write (*,9101) 'Parent section 0 and section 1.',Iret
                  Go to 900
              End if
c
          End if
c
      End if
c
c*        2.6  Reset internal parameters
c
  260 Continue
      If (Hoper.eq.'B') Then
c
c         Total number of related bloks
c
          If (Kmaxw.le.0) Then
              Iret=260
              Write (*,9260) Kmaxw,Iret
              Go to 900
          End if
          Inblok=Klen2/Kmaxw+1
c
c         Total length of all BLOK messages
c
          Itblen=Inblok*Kmaxw
          Itemp=Klen2+(Inblok*(120*8/Ibits))
          If (Itblen.lt.Itemp) Inblok=Inblok+1
c
          Ksec0(5)=Inblok
c
c         Total length of the parent form
c
          Ksec0(3)=Klen2*Ibits/8
c
c         Blok sequence number
c
          Nbseqn=0
c
      End if
c
c     Number of Ksec2 words which will be inserted 
c     into one BLOK message
c
      If (Hoper.eq.'C') Then
c
          If (Kmaxw.le.Klen2) Then
              Iret=263
              Write (*,9263) Kmaxw,Klen2,Iret
              Go to 900
          End if
c
          Iword2=Klen2
c
      Else if (Hoper.eq.'B') Then
c
          Iword2=Kmaxw-(((16+4+Ilen1)*8/Ibits)+1)
          Iret=Iword2*Ibits
          If (Mod(Iret,960).ne.0) Then
              Iret=Iret/960
              Iret=Iret*960
              Iword2=Iret/Ibits
          End if
c
      End if
c
c     ------------------------------------------------------------------
c
c*        3.   BLOK Section 0
c              --------------
  300 Continue
c
c     Section 0 shall always be 16 octets long
c
      If (Ndbg.eq.1) Write(*,*) '/Blokex/ Paragraph 3.'
c
c*        3.1  Octets 1-4; The letters B L O K
c
  310 Continue
c
c     Four 8 bit fields
c
      If (Yoper.eq.'B') Then
c
c         Insert fields
c
          Call Inxbit (Idata,Kbdim,Inspt,Nblok(1),4,Ibits,8,
     &                 Ycoper,Iret)
          If (Iret.ne.0) Then
              Iret=310
              Write (*,9100) 'Octets 1-4; The letters B L O K',Iret
              Go to 900
          End if
c
      Else
c
c         Extract fields
c
          Call Inxbit (Kblok,Kbdim,Inspt,Ieblk(1),4,Ibits,8,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=311
              Write (*,9101) 'Octets 1-4; The letters B L O K',Iret
              Go to 900
          End if
          Do 312 i=1,4
          If (Ieblk(i).ne.Nblok(i)) Then
              Iret=-1
              Go to 910
          End if
  312     Continue
c
      End if
c
c*        3.2  Octets 5-7; Length of message
c
  320 Continue
c
c     One 24 bit field
c
      If (Yoper.eq.'B') Then
c
c         When coding data, skip field. Length is inserted
c         later, when known.
c
          Insptl=Inspt
c
c         Update bit-pointer.
c
          Inspt=Inspt+24
c
       Else
c
c         Extract field
c
          Call Inxbit (Kblok,Kbdim,Inspt,Ksec0(1),1,Ibits,24,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=320
              Write (*,9101) 'Octets 5-7; Length of message.',Iret
              Go to 900
          End if
c
c
c         If only length is required, go to paragraph 9.
c
          If (Hoper.eq.'L') Then
              Write(*,9321) Ksec0(1)
              Go to 900
          End if
c
          Kword=(Ksec0(1)+Ibits/8-1)*8/Ibits
c
      End if
c
c*        3.3  Octet 8; Blok edition number
c
  330 Continue
c
c     One 8 bit field
c
      If (Yoper.eq.'B') Then
c
c         Set value
c
          Ksec0(2)=Jbedno
c
c         Insert field
c
          Call Inxbit (Idata,Kbdim,Inspt,Ksec0(2),1,Ibits,8,
     &                 Ycoper,Iret)
          If (Iret.ne.0) Then
              Iret=330
              Write (*,9100) 'Octet 8; Blok edition number.',Iret
              Go to 900
          End if
c
      Else
c
c         Extract field
c
          Call Inxbit (Kblok,Kbdim,Inspt,Ksec0(2),1,Ibits,8,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=330
              Write (*,9101) 'Octet 8; Blok edition number.',Iret
              Go to 900
          End if
c
      End if
c
c*        3.4  Octets 9-11; Total length of parent form
c
  340 Continue
c
c     One 24 bit field
c
      If (Yoper.eq.'B') Then
c
c         Insert field
c
          Call Inxbit (Idata,Kbdim,Inspt,Ksec0(3),1,Ibits,24,
     &                 Ycoper,Iret)
          If (Iret.ne.0) Then
              Iret=340
              Write (*,9100) 'Octets 9-11; Length of parent form.',Iret
              Go to 900
          End if
c
       Else
c
c         Extract field
c
          Call Inxbit (Kblok,Kbdim,Inspt,Ksec0(3),1,Ibits,24,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=341
              Write (*,9101) 'Octets 9-11; Length of parent form.',Iret
              Go to 900
          End if
      End if
c
c*        3.5  Octet 12; Flag (Code table 1)
c
  350 Continue
c
c     One 8 bit field
c
      If (Yoper.eq.'B') Then
c
c         Insert field
c
          Call Inxbit (Idata,Kbdim,Inspt,Ksec0(4),1,Ibits,8,
     &                 Ycoper,Iret)
          If (Iret.ne.0) Then
              Iret=350
              Write (*,9100) 'Octet 12; Flag.',Iret
              Go to 900
          End if
c
      Else
c
c         Extract field
c
          Call Inxbit (Kblok,Kbdim,Inspt,Ksec0(4),1,Ibits,8,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=352
              Write (*,9101) 'Octet 12; Flag.',Iret
              Go to 900
          End if
c
      End if
c
c
c*        3.6  Octet 13; Total number of related BLOKS
c
  360 Continue
c
c     One 8 bit field
c
      If (Yoper.eq.'B') Then
c
c         Insert field
c
          Call Inxbit (Idata,Kbdim,Inspt,Ksec0(5),1,Ibits,8,
     &                 Ycoper,Iret)
          If (Iret.ne.0) Then
              Iret=360
              Write (*,9100) 'Octet 13; Total number of related bloks.',
     &                        Iret
              Go to 900
          End if
c
      Else
c
c         Extract field
c
          Call Inxbit (Kblok,Kbdim,Inspt,Ksec0(5),1,Ibits,8,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=361
              Write (*,9101) 'Octet 13; Total number of related bloks.',
     &                        Iret
              Go to 900
          End if
c
c         Check order of fields if function 'F' requested
c
          If (Nflag.ne.0) Then
              If (Nbseqn/1000.ne.Ksec0(5)) Then
                  Iret=-362
                  Write (*,9425) Iret
                  Call Blprs0 (Ksec0)
                  Go to 739
              End if
          End if
c
      End if
c
c*        3.7  Octet 14; Sequence number of this BLOK
c
  370 Continue
c
c     One 8 bit field
c
      If (Hoper.eq.'B') Then
c
c         Reset value
c
          Nbseqn=Nbseqn+1
          Ksec0(6)=Nbseqn
c
      End if
c
      If (Yoper.eq.'B') Then
c
c         Insert field
c
          Call Inxbit (Idata,Kbdim,Inspt,Ksec0(6),1,Ibits,8,
     &                 Ycoper,Iret)
          If (Iret.ne.0) Then
              Iret=370
              Write (*,9100) 'Octet 14; Sequence number of this BLOK.',
     &                        Iret
              Go to 900
          End if
c
      Else
c
c         Extract field
c
          Call Inxbit (Kblok,Kbdim,Inspt,Ksec0(6),1,Ibits,8,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=371
              Write (*,9101) 'Octet 14; Sequence number of this BLOK.',
     &                        Iret
              Go to 900
          End if
c
      End if
c
c*        3.8  Octets 15-16; Reserved
c
  380 Continue
c
c     One 16 bit field
c
      If (Yoper.eq.'B') Then
c
c         When coding data, skip field.
c
          Ksec0(7)=0
c
c         Update bit-pointer.
c
          Inspt=Inspt+16
c
      Else
c
c         Extract field
c
          Call Inxbit (Kblok,Kbdim,Inspt,Ksec0(7),1,Ibits,16,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=381
              Write (*,9101) 'Octets 15-16;',Iret
              Go to 900
          End if
c
      End if
c
c     ------------------------------------------------------------------
c
c*        4.   BLOK Section 1 - Identification section
c              ---------------------------------------
  400 Continue
      If (Ndbg.eq.1) Write (*,*) '/Blokex/ Paragraph 4.'
      If (Ksec0(4).ne.128) Go to 499
c
c*        4.1  Coding data
c
  410 Continue
c
c     Copy section 0 and identification section of the parent code
c     into the section 1 of BLOK code
c
      If (Yoper.eq.'B') Then
c
c         Insert field
c
          Call Inxbit (Idata,Kbdim,Inspt,Isec1(1),Ilen1,Ibits,8,
     &                 Ycoper,Iret)
          If (Iret.ne.0) Then
              Iret=415
              Write (*,9100) 'bit fields into BLOK section 1.',Iret
              Go to 900
          End if
c
c*        4.2  Decoding data
c
  420 Continue
      Else
          Itemp=Inspt
c
c         Name of the parent form
c
          Call Inxbit (Kblok,Kbdim,Itemp,Iname(1),4,Ibits,8,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=421
              Write (*,9101) 'Name of the parent form.',Iret
              Go to 900
          End if
          Icoun=0
          Hform='GRIB'
          Do 422 i=1,4
          If (Iname(i).eq.Igrib(i)) Icoun=Icoun+1
  422     Continue
          If (Icoun.ne.4) Then
              Icoun=0
              Do 423 i=1,4
              If (Iname(i).eq.Ibufr(i)) Icoun=Icoun+1
  423         Continue
              If (Icoun.eq.4) Then
                  Hform='BUFR'
              Else
                  Iret=423
                  Write (*,9423) Iret
                  Go to 900
              End if
          End if
c
c         Parent code edition number; Extract field
c
          Itemp=Inspt+56
          Call Inxbit (Kblok,Kbdim,Itemp,Iedit,1,Ibits,8,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=424
              Write (*,9101) 'Parent code edition number.',Iret
              Go to 900
          End if
c
c         Set first word in the output array Ksec1 and check order
c         of fields if 'F' function requested
c
          If (Nflag.ne.0) Then
              If (Iedit.ne.Ksec1(1)) Then
                  Iret=-425
                  Write (*,9425) Iret
                  Call Blprs0 (Ksec0)
                  Call Blprs1 (Ksec1,Hform)
                  Go to 739
              End if
          End if
          Ksec1(1)=Iedit
c
c         Reset position of the length of the parent section 1 for
c         the different parent code versions
c
          If (Hform.eq.'GRIB'.and.Iedit.lt.1) Then
              Itemp=Inspt+32
          Else if (Hform.eq.'BUFR'.and.Iedit.lt.2) Then
              Itemp=Inspt+32
          End if
c
c         Length of the identification section; Extract field
c
          Call Inxbit (Kblok,Kbdim,Itemp,Ilen1,1,Ibits,24,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=426
              Write (*,9101) 'Length of the section 1.',Iret
              Go to 900
          End if
          Inspt=Itemp
c
c*        4.2.1 GRIB code identification section
c
 4210 Continue
          If (Hform.eq.'GRIB') Then
c
c             Octet 4  : Version Number of Table 2.
c             Octet 5  : Identification of centre.
c             Octet 6  : Generating process identification.
c             Octet 7  : Grid definition.
c             Octet 8  : Flag.
c             Octet 9  : Indicator of parameter.
c             Octet 10 : Indicator of type of level.
c
c             Seven 8 bit fields.
c
              Call Inxbit (Kblok,Kbdim,Inspt,Isec1(1),7,Ibits,8,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=4210
                  Write (*,9101) 'Octets 4-10; Section 1.',Iret
                  Go to 900
              End if
c
c             Octets 11 - 12 : Height, pressure etc of levels
c
              Call Inxbit (Kblok,Kbdim,Inspt,Isec1(8),2,Ibits,8,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=4211
                  Write (*,9101) 'Octets 11-12; Section 1.',Iret
                  Go to 900
              End if
c
c             Octet 13 : Year of century.
c             Octet 14 : Month.
c             Octet 15 : Day.
c             Octet 16 : Hour.
c             Octet 17 : Minute.
c             Octet 18 : Indicator of unit of time range..
c
c             Six 8 bit fields.
c
              Call Inxbit (Kblok,Kbdim,Inspt,Isec1(10),6,Ibits,8,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=4212
                  Write (*,9101) 'Octets 13-18; Section 1.',Iret
                  Go to 900
              End if
c
c             Octets 19 - 20 : Period of time.
c             One 16 bit field or two 8 bit fields.
c
              Call Inxbit (Kblok,Kbdim,Inspt,Isec1(16),2,Ibits,8,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=4213
                  Write (*,9101) 'Octets 19-20; Section 1.',Iret
                  Go to 900
              End if
c
c             Octet 21 : Time range indicator.
c             One 8 bit field.
c
              Call Inxbit (Kblok,Kbdim,Inspt,Isec1(18),1,Ibits,8,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=4214
                  Write (*,9101) 'Octet 21; Section 1.',Iret
                  Go to 900
              End if
c
c             Octet 22 - 23 : Number averaged.
c             One 16 bit field.
c
              Call Inxbit (Kblok,Kbdim,Inspt,Isec1(19),1,Ibits,16,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=4215
                  Write (*,9101) 'Octets 22-23; Section 1.',Iret
                  Go to 900
              End if
c
c             Octet 24 : Number missing from averages etc.
c             Octet 25 : Century of data.
c             Octet 26 : Reserved field. (set to 0)
c             Three 8 bit field.
c
              Itemp=3
              Ilen=20
              If (Iedit.le.0) Itemp=1
c
              Call Inxbit (Kblok,Kbdim,Inspt,Isec1(20),Itemp,Ibits,8,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=4216
                  Write (*,9101) 'Octets 24-26; Section 1.',Iret
                  Go to 900
              End if
c
c             Octets 27 - 28 : Units decimal scale factor.
c             One 16 bit field.
c
              If (Iedit.gt.0) Then
c
c                 Extract field
c
                  Call Inxbit (Kblok,Kbdim,Inspt,Isec1(23),1,Ibits,16,
     &                     Ydoper,Iret)
                  If (Iret.ne.0) Then
                      Iret=4217
                      Write (*,9101) 'Octets 27-28; Section 1.',Iret
                      Go to 900
                  End if
                  Ilen=24
c
              End if
c
c             Set output array Ksec1 and check order of fields if
c             'F' function requested
c
              Do 4218 i=1,Ilen
              If (Nflag.ne.0) Then
                  If (Ksec1(i+1).ne.Isec1(i)) Then
                      Iret=-4218
                      Write (*,9425) Iret
                      Call Blprs0 (Ksec0)
                      Call Blprs1 (Ksec1,Hform)
                      Go to 739
                  End if
              Else
                  Ksec1(i+1)=Isec1(i)
              End if
4218          Continue
c
c*        4.2.2 BUFR code identification section
c
 4220 Continue
          Else if (Hform.eq.'BUFR') Then
c
c             Octet 4  : BUFR master table
c             One 8 bit field.
c
              Call Inxbit (Kblok,Kbdim,Inspt,Isec1(1),1,Ibits,8,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=4220
                  Write (*,9101) 'Octet 4; Section 1.',Iret
                  Go to 900
              End if
c
c             Octet 5-6  : Originating centre
c             One 16 bit field.
c
              Call Inxbit (Kblok,Kbdim,Inspt,Isec1(2),1,Ibits,16,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=4221
                  Write (*,9101) 'Octets 5-6; Section 1.',Iret
                  Go to 900
              End if
c
c             Octet 7  : Update sequence number
c             Octet 8  : Flag.
c             Octet 9  : BUFR message type
c             Octet 10 : BUFR message sub-type
c             Octet 11 : Version number of master table used
c             Octet 12 : Version number of local tables used to augment
c                        the master table in use
c             Octet 13 : Year of century.
c             Octet 14 : Month.
c             Octet 15 : Day.
c             Octet 16 : Hour.
c             Octet 17 : Minute.
c             Octet 18 : Indicator of unit of time range..
c
c             Twelve 8 bit fields.
c
              Call Inxbit (Kblok,Kbdim,Inspt,Isec1(3),12,Ibits,8,
     &                     Ydoper,Iret)
              If (Iret.ne.0) Then
                  Iret=4212
                  Write (*,9101) 'Octets 7-18; Section 1.',Iret
                  Go to 900
              End if
c
          End if
c
      End if
  499 Continue
c
c     Print content of Section 0 and Section 1 if function 'I' requested
c
      If (Hoper.eq.'I') Go to 900
c
c     ------------------------------------------------------------------
c
c*        5.   BLOK Section 2 - Binary Data Section
c              ------------------------------------
  500 Continue
      If (Ndbg.eq.1) Write(*,*) '/Blokex/ Paragraph 5.'
c
c*        5.1  Coding data
c
  510 Continue
      If (Yoper.eq.'B') Then
c
c         First and last position of the Ksec2 array for the
c         data which will be inserted in the current BLOK message
c
          Is=Ie+1
          If (Ksec0(5).ne.Ksec0(6)) Then
              Ie=Is+Iword2-1
          Else
              Ie=Klen2
          End if
c
c         Data to be inserted in the current blok
c
          Iilen=0
          Do 511 i=Is,Ie
          Iilen=Iilen+1
          Kblok(Iilen)=Ksec2(i)
  511     Continue
c
c         Insert field
c
          Call Inxbit (Idata,Kbdim,Inspt,Kblok,Iilen,Ibits,Ibits,
     &                 Ycoper,Iret)
          If (Iret.ne.0) Then
              Iret=512
              Write (*,9100) 'binary data section (Section 2).',Iret
              Go to 900
          End if
c
c*        5.2  Decoding data
c
  520 Continue
      Else
c
c         Length of the data section
c
          Iilen=(Ksec0(1)*8-Inspt)/Ibits
c
c         Data section; Extract field
c
          Call Inxbit (Kblok,Kbdim,Inspt,Idata,Iilen,Ibits,Ibits,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=522
              Write (*,9101) 'binary data section (Section 2).',Iret
              Go to 900
          End if
      End if          
c
c     ------------------------------------------------------------------
c
c*        6.   BLOK Section 3 - Ascii 7777
c              ---------------------------
  600 Continue
      If (Ndbg.eq.1) Write (*,*) '/Blokex/ Paragraph 6.'
c
c     Four 8 bit fields
c
c*        6.1  Coding data
c
  610 Continue
      If (Yoper.eq.'B') Then
c
c         Insert field
c
          Call Inxbit (Idata,Kbdim,Inspt,N7777(1),4,Ibits,8,
     &                 Ycoper,Iret)
          If (Iret.ne.0) Then
              Iret=611
              Write (*,9100) 'BLOK Section 3 (7777).',Iret
              Go to 900
          End if
c
c         Length of BLOK message
c
          Ksec0(1)=(Inspt-Iword*Ibits)/8
c
c         Insert field
c
          Call Inxbit (Idata,Kbdim,Insptl,Ksec0(1),1,Ibits,24,
     &                 Ycoper,Iret)
          If (Iret.ne.0) Then
              Iret=802
              Write (*,9100) 'Total length of this message',Iret
              Go to 900
          End if
c
c*        6.2  Decoding data
c
  620 Continue
      Else
c
c         Reset counter
c
          Itemp=0
  621     Continue
c
c         Extract field
c
          Call Inxbit (Kblok,Kbdim,Inspt,Iendm(1),4,Ibits,8,
     &                 Ydoper,Iret)
          If (Iret.ne.0) Then
              Iret=621
              Write (*,9101) 'BLOK Section 3 (7777).',Iret
              Go to 900
          End if
c
c         Check that 7777 group is found where expected
c
          Icoun=0
          Do 622 i=1,4
          If (Iendm(i).ne.N7777(i)) Icoun=Icoun+1
  622     Continue
          If (Icoun.ne.0) Then
c
c             Search backward
c
              Inspt=Inspt-8-4*8
              Itemp=Itemp+1
              If (Itemp.gt.8) Then
                  Iret=623
                  Write (*,9623) Iret
                  Go to 900
              End if
              Go to 621
          Else
c
c             Update length of the data section
c
              If (Itemp.eq.0) Then
                  Iword=Iilen
              Else
                  Iword=Iilen-(Itemp*8/Ibits)
              End if
          End if
c
      End if
c
c     ------------------------------------------------------------------
c
c*        7.   Complete single message
c              -----------------------
  700 Continue
      If (Ndbg.eq.1) Write(*,*) '/Blokex/ Paragraph 7.'
c
c*        7.1  Coding data
c
  710 Continue
      If (Yoper.eq.'B') Then
c
c         Any unused part of last word is already set to binary zeroes
c         Increment pointers as necessary
c
          Iword=Inspt/Ibits
          Itemp=Iword*Ibits
          Ioff =Inspt-Itemp
          If (Ioff.ne.0) Then
              Inspt=Inspt+Ibits-Ioff
              Iword=Iword+1
          End if
c
c         Round length to a multiple of 120 octets, if required,
c         any additional words are already set to 0.
c
          If (Nrnd.eq.1) Then
              i=Inspt/960
              i=i*960
              i=Inspt-i
              If (i.ne.0) i=(960-i)/Ibits
              Iword=Iword+i
              Inspt=Iword*Ibits
          End if
c
c         Output BLOK coded data array
c
          If (Nbseqn.eq.Inblok.or.Hoper.eq.'C') Then
              Do 711 i=1,Iword
              Kblok(i)=Idata(i)
  711         Continue
          End if
          Kword=Iword
c
c*        7.2  Decoding data; Function 'U'
c
  720 Continue
      Else if (Hoper.eq.'U') Then
c
          Do 721 i=1,Iword
          Ksec2(i)=Idata(i)
  721     Continue
          Klen2=Iword
c
c*        7.3  Decoding data; Function 'F'
c
  730 Continue
      Else if (Hoper.eq.'F') Then
c
c         Collect fields
c
          If (Nflag.eq.0) Then
c
c         Nflag=0; Function 'F' requested, first time through
c
c             Check the order of the first field
c
              Nflag=1
              If (Ksec0(6).ne.Nbcoun) Then
c
c                 Field is not in right order
c
                  Nflag=2
c
c                 Suppress current unblocking stage
c
                  Nfcoun=1
                  Ksec2(Itplen+Iword+1)=Ksec0(6)*1000000+Iword
c
              End if
c
          Else if (Nflag.eq.1) Then
c
c         Nflag=1; Function 'F' requested, fields in order
c
c             Check the order of the current field
c
              If (Ksec0(6).ne.Nbcoun) Then
c
c                 Fields are not in right order
c
                  Nflag=2
c
c                 Suppress previous and current unblocking stage
c
                  If (Nfcoun.eq.0) Then
                      Itemp=Nbseqn/1000
                      Itemp=Nbseqn-Itemp*1000
                      Ksec2(Itplen+Iword+1)=Itemp*1000000+Itplen
                  End if
                  Nfcoun=2
                  Ksec2(Itplen+Iword+2)=Ksec0(6)*1000000+Iword
c
              End if
c
          Else if (Nflag.eq.2) Then
c
c         Nflag=2; Unblocked fields are not in right order
c
c             Increase counter of the fields which are not in order
c
              Nfcoun=Nfcoun+1
c
c             Suppress current unblocking stage
c
              Ksec2(Itplen+Nfcoun)=Ksec0(6)*1000000+Iword
c
c             Update internally used part of Ksec2
c
              Do 733 i=Nfcoun,1,-1
              Ksec2(Itplen+Iword+i)=Ksec2(Itplen+i)
  733         Continue
c
c             Reset flag if total number of related bloks achieved
c
              If (Nbcoun.eq.Ksec0(5)) Nflag=3
c
          End if
c
          Do 734 i=1,Iword
          Itplen=Itplen+1
          Ksec2(Itplen)=Idata(i)
  734     Continue
          Klen2=Itplen
          Nbseqn=Ksec0(5)*1000+Ksec0(6)
c
c         Repack parent form if fields are not in right order
c
          If (Nflag.eq.3) Then
c
c             Find first field in right order, if any
c
              If (Nfcoun.eq.Nbcoun) Then
c
c                 No fields in order
c
                  Iilen=0
                  Icoun=0
                  Inblok=1
                  Itblen=0
c
              Else
c
c                 Number of the fields in right order
c
                  Icoun=Ksec2(Itplen+1)/1000000
                  Inblok=2
                  Itblen=Icoun-1
c
c                 Length of the fields in right order
c
                  Iilen=Ksec2(Itplen+1)-Icoun*1000000
c
              End if
c
              Do 7350 Ie=Inblok,Nfcoun
              Icoun=Icoun+1
              Is=Iilen
c
              Do 7351 i=Ie,Nfcoun
c
c             Sequence number of the field
c
              Iorder=Ksec2(Itplen+i)/1000000
c
c             Length of the field
c
              Ilen=Ksec2(Itplen+i)-Iorder*1000000
c
c             Field starting index in the array Ksec2
c
              If (Iorder.eq.Icoun) Then
                  Itemp=i
                  Go to 7352
              Else
                  Is=Is+Ilen
              End if
c
 7351         Continue
              Iret=-7351
              Write(*,97351) Icoun,Iret
              Call Blprs1(Ksec1,Hform)
              Go to 739
c
c             Repack parent form so that 'Icoun' field is 
c             in right order
c
 7352         Continue
c
              Do 7353 i=1,Ilen
              Idata(i)=Ksec2(Is+i)
 7353         Continue
              Do 7354 i=Is+Ilen+1,Itplen
              Ksec2(i-Ilen)=Ksec2(i)
 7354         Continue
              Do 7355 i=Itplen-Ilen,Iilen+1,-1
              Ksec2(i+Ilen)=Ksec2(i)
 7355         Continue
              Do 7356 i=1,Ilen
              Iilen=Iilen+1
              Ksec2(Iilen)=Idata(i)
 7356         Continue
c
c             Repack internaly used part of Ksec2
c
              Idata(1)=Ksec2(Itplen+Itemp)
              Do 7357 i=Itplen+Itemp+1,Itplen+Nfcoun
              Ksec2(i-1)=Ksec2(i)
 7357         Continue
              Do 7358 i=Itplen+Nfcoun,Itplen+Iorder-Itblen,-1
              Ksec2(i)=Ksec2(i-1)
 7358         Continue
              Ksec2(Itplen+Iorder-Itblen)=Idata(1)
c
 7350         Continue
c
          End if
c
c         Reset internal total parent length if it is achieved
c
          Itemp=Ksec0(3)*8/Ibits
          If (Nbcoun.eq.Ksec0(5)) Then
              Iret=0
              If (Itplen.ne.Itemp) Then
                  Iret=-739
                  Write(*,9739) Itemp,Itplen,Iret
              End if
          Else
              Iret=-2
              Nbcoun=Nbcoun+1
              Go to 800
          End if
c
      End if
c
c     Reset flags to default values
c
  739 Continue
      If (Hoper.eq.'F') Then
          Nflag=0
          Itplen=0
          Nbseqn=0
          Nbcoun=1
          Nfcoun=0
      End if
c
c     ------------------------------------------------------------------
c
c*        8.   Continue loop over all single BLOK messages
c              -------------------------------------------
  800 Continue
      If (Ndbg.eq.1) Write(*,*) '/Blokex/ Paragraph 8.'
c
      If (Nbseqn.ne.Inblok.and.Hoper.eq.'B') Then
          Go to 300
      End if
c
c     ------------------------------------------------------------------
c
c*        9.   Return code handling
c              --------------------
  900 Continue
c
      If (Ndbg.eq.1) Write(*,*) '/Blokex/ Paragraph 9.'
      If (Ndbg.eq.1.or.Hoper.eq.'I') Then
          Call Blprs0 (Ksec0)
          If (Ksec0(4).eq.128.and.Yoper.ne.'B') Then
              Call Blprs1 (Ksec1,Hform)
          End if
      End if
c
c     If no error has been encountered, set return code to informative
c     value, if required.
c
c     Abort if any error has been encountered and user has requested
c     an abort. Informative values are negative and do not cause an
c     abort.
c
  910 Continue
      If (Iret.gt.0.and.Kret.eq.0) Then
          Call abortx ('BLOKEX')
      Else
          Kret=Iret
          Return
      End if
c
 9100 Format (' Blokex: Error inserting ',a,' Return code=',i4)
 9101 Format (' Blokex: Error extracting ',a,' Return code=',i4)
 9210 Format (' Blokex: Error, Unrecognised function requested: ',a/
     &        ' ','         Return code=',i3)
 9220 Format (' Blokex: Error, Requested length of single',
     &        ' BLOK message ',i10,' words'/
     &        '                 is not multiple of 120 bytes.'/
     &        ' ','         Rounding of 120 bytes included; Nrnd=',i3/
     &        ' ','         Return code=',i3) 
 9230 Format (' Blokex: Error, Unrecognised entry Ksec0(4)=',i6/
     &        ' ','         Valid entry: 0 /or/ 128.'/
     &        ' ','         Return code=',i3) 
 9240 Format (' Blokex: Error, Output array too small to receive',
     &            ' BLOK coded field.'/
     &        ' ','         Kbdim=',i9,' Klen2=',i9/
     &        ' ','         Return code=',i3) 
 9251 Format (' Blokex: Error, Unrecognised parent data format.'/
     &        ' ','         BLOK Section 1 requested to be',
     &            ' included (Ksec0(4)=128) for the'/
     &        ' ','         unrecognised parent data format.'/
     &        ' ','         Return code=',i4) 
 9260 Format (' Blokex: Error, Maximum length of single BLOK ',
     &            ' message Kmaxw=',i9/
     &        ' ','                not valid.'/
     &        ' ','         Return code=',i3)
 9263 Format (' Blokex: Error, Required single BLOK length Kmaxw=',
     &            i9,' too small'/
     &        ' ','         to receive input field Klen2=',i9,
     &            '; Fuction C requested.'/
     &        ' ','         Return code=',i3) 
 9321 Format (' Blokex: Total length of this BLOK is ',i8,
     &            ' octets.')
 9423 Format (' Blokex: Error, Unrecognised parent code name.'/
     &        ' ','         Return code=',i3) 
 9425 Format (' Blokex: Error, Input fields are not in right order.'
     &       /' ','         Unblocking uncompleted.'/
     &        ' ','         Function requested F.'/
     &        ' ','         Return code=',i5) 
 9623 Format (' Blokex: Error, End of message 7777 group not',
     &            ' found.'/
     &        ' ','         Return code=',i3) 
 9739 Format (' Blokex: Error, Original parent length: ',i12,/
     &        ' ','         differ from the length achieved after full',
     &            ' decoding: ',i12/
     &        ' ','         Return code=',i4) 
97351 Format (' Blokex: Error, BLOK sequence ',i3,' missing.'/
     &        ' ','         Unblocking uncompleted.'/
     &        ' ','         Return code=',i5) 

c
      End
      Subroutine Blprs0(Ksec0)
c**
c****  Name  *Blprs0*
c****  ----
c**
c**   Purpose
c**   -------
c**         Print the information in the Indicator
c**         Section (Section 0) of BLOK data.
c**
c**   Interface
c**   ---------
c**       Input: Ksec0 - Array of decoded parameters from Section 0.
c**
c**       Output: Printed data on the standard output.
c**
c**    Method
c**    ------
c**       Fields are printed as integers.
c**
c**    Externals
c**    ---------
c**       None.
c**
c**    Author
c**    ------
c**       D.Jokic,  ECMWF,  February-1992. 
c**
      Integer Ksec0(*)
c
c     ------------------------------------------------------------------
c
c*        1.   Write Ksec0
c              -----------
  100 Continue
      Write (*,9000)
      Write (*,9001)
      Write (*,9002)
      Write (*,9003) Ksec0(1)
      Write (*,9004) Ksec0(2)
      Write (*,9005) Ksec0(3)
      Write (*,9006) Ksec0(4)
      Write (*,9007) Ksec0(5)
      Write (*,9008) Ksec0(6)
c
 9000 Format (' ')
 9001 Format (' ','BLOK Section 0 - Indicator Section.     ')
 9002 Format (' ','----------------------------------------')
 9003 Format (' ','Total length of this BLOK.           ',I9)
 9004 Format (' ','BLOK Edition Number.                 ',I9)
 9005 Format (' ','Total length of parent form.         ',I9)
 9006 Format (' ','Flag (Code Table 1).                 ',I9)
 9007 Format (' ','Total number of related BLOKs.       ',I9)
 9008 Format (' ','Sequence number of this BLOK.        ',I9)
c
      Return
      End
      Subroutine Blprs1(Ksec1,Yform)
c**
c****  Name  *Blprs1*
c****  ----
c**
c**   Purpose
c**   -------
c**         Print the information in the Identification
c**         Section (Section 1) of BLOK data.
c**
c**   Interface
c**   ---------
c**       Input: Ksec1 - Array of decoded parameters from Section 1.
c**              Yform - Format of data
c**
c**       Output: Printed data on the standard output.
c**
c**    Method
c**    ------
c**       Fields are printed as integers.
c**
c**    Externals
c**    ---------
c**       None.
c**
c**    Author
c**    ------
c**       D.Jokic,  ECMWF,  February-1992. 
c**
      Character*4 Yform
c
      Integer Ksec1(*)
c
c     ------------------------------------------------------------------
c
c*        1.   Initialise
c              ----------
  100 Continue
      Write (*,9000)
      Write (*,9001)
      Write (*,9002)
      Write (*,9003) Yform
      Write (*,9004) Ksec1(1)
c
c     ------------------------------------------------------------------
c
c*        2.   GRIB data
c              ---------
  200 Continue
      If (Yform.eq.'GRIB') Then
c
          Write (*,9202) Ksec1(2)
          Write (*,9203) Ksec1(3)
          Write (*,9204) Ksec1(4)
          Write (*,9205) Ksec1(5)
          Write (*,9206) Ksec1(6)
          Write (*,9207) Ksec1(7)
          Write (*,9208) Ksec1(8)
          Write (*,9209) Ksec1(9)
          Write (*,9210) Ksec1(10)
          Write (*,9211) Ksec1(11)
          Write (*,9212) Ksec1(12)
          Write (*,9213) Ksec1(13)
          Write (*,9214) Ksec1(14)
          Write (*,9215) Ksec1(15)
          Write (*,9216) Ksec1(16)
          Write (*,9217) Ksec1(17)
          Write (*,9218) Ksec1(18)
          Write (*,9219) Ksec1(19)
          Write (*,9220) Ksec1(20)
          Write (*,9221) Ksec1(21)
          Write (*,9222) Ksec1(22)
          Write (*,9223) Ksec1(23)
c
c     ------------------------------------------------------------------
c
c*        3.   BUFR data
c              ---------
  300 Continue
      Else if (Yform.eq.'BUFR') Then
c
          Write (*,9302) Ksec1(2)
          Write (*,9303) Ksec1(3)
          Write (*,9304) Ksec1(4)
          Write (*,9305) Ksec1(5)
          Write (*,9306) Ksec1(6)
          Write (*,9307) Ksec1(7)
          Write (*,9308) Ksec1(8)
          Write (*,9309) Ksec1(9)
          Write (*,9310) Ksec1(10)
          Write (*,9311) Ksec1(11)
          Write (*,9312) Ksec1(12)
          Write (*,9313) Ksec1(13)
          Write (*,9314) Ksec1(14)
          Write (*,9315) Ksec1(15)
c
      Else
c
          Write (*,9400) Yform
c
      End if
c
 9000 Format (' ')
 9001 Format (' ','BLOK Section 1 - Identification Section.')
 9002 Format (' ','----------------------------------------')
 9003 Format (' ','Binary data format.                       ',A4)
 9004 Format (' ','Parent edition number.               ',I9)
c
 9202 Format (' ','Code Table 2 Version Number.         ',I9)
 9203 Format (' ','Originating centre identifier.       ',I9)
 9204 Format (' ','Model identification.                ',I9)
 9205 Format (' ','Grid definition.                     ',I9)
 9206 Format (' ','Flag (Code Table 1)                  ',I9)
 9207 Format (' ','Parameter identifier (Code Table 2). ',I9)
 9208 Format (' ','Type of level (Code Table 3).        ',I9)
 9209 Format (' ','Value 1 of level (Code Table 3).     ',I9)
 9210 Format (' ','Value 2 of level (Code Table 3).     ',I9)
 9211 Format (' ','Year of data.                        ',I9)
 9212 Format (' ','Month of data.                       ',I9)
 9213 Format (' ','Day of data.                         ',I9)
 9214 Format (' ','Hour of data.                        ',I9)
 9215 Format (' ','Minute of data.                      ',I9)
 9216 Format (' ','Time unit (Code Table 4).            ',I9)
 9217 Format (' ','Time range one.                      ',I9)
 9218 Format (' ','Time range two.                      ',I9)
 9219 Format (' ','Time range indicator (Code Table 5)  ',I9)
 9220 Format (' ','Number averaged.                     ',I9)
 9221 Format (' ','Number missing from average.         ',I9)
 9222 Format (' ','Century of data.                     ',I9)
 9223 Format (' ','Units decimal scaling factor.        ',I9)
c
 9302 Format (' ','BUFR master table.                   ',I9)
 9303 Format (' ','Originating centre identifier.       ',I9)
 9304 Format (' ','Update sequence number.              ',I9)
 9305 Format (' ','Flag.                                ',I9)
 9306 Format (' ','BUFR message type.                   ',I9)
 9307 Format (' ','BUFR message subtype.                ',I9)
 9308 Format (' ','Version number of master table used. ',I9)
 9309 Format (' ','Version number of local table used.  ',I9)
 9310 Format (' ','Year of data.                        ',I9)
 9311 Format (' ','Month of data.                       ',I9)
 9312 Format (' ','Day of data.                         ',I9)
 9313 Format (' ','Hour of data.                        ',I9)
 9314 Format (' ','Minute of data.                      ',I9)
 9315 Format (' ','Indicator of unit of time range.     ',I9)
c
 9400 Format (' Blprs1: Error, Unknown data format: ',a)
c
      Return
      End
