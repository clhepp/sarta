C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS (Atmospheric Infra-Red Sounder)
C
C    incFTC
C
!F77====================================================================


!ROUTINE NAME:
C    incFTC (include file)


!ABSTRACT:
C    Include file consisting of parameter statements to size various
C    arrays in the USEFAST related routines source code.


!CALL PROTOCOL:
C    none (include file)


!INPUT PARAMETERS:
C    none


!OUTPUT PARAMETERS:
C    none


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    CALOKW
C    CALOWP
C    CALPAR
C    CALRAD
C    CALT1
C    CALT2
C    CALT3
C    CALT4
C    CALT5
C    CALT6
C    CALT7
C    FAKETZ
C    RDCOEF
C    RDLIST
C    RDPROF
C    RDSUN
C    SUNPAR
C    SARTA


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    none


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    Include file for the May 2008 100 layer AIRS fast
C    Stand Alone RTA (SARTA) code by L.L.Strow/S.Hannon.
C
C    Parameter statements for the FTC routines.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C Date        Programmer     Comments
C ----------- -------------- -------------------------------------------
C  1 Dec 1994 Scott Hannon   Created
C 31 Jan 1997 Scott Hannon   Re-wrote for FWO+FOW+FMW+FCOW=Feb97 FTC
C  3 Sep 1997 Scott Hannon   Re-wrote for sets 1 - 7
C 30 Sep 1997 Scott Hannon   Added NCO2 and MXCHNC
C 26 Feb 1998 Scott Hannon   Added OPTRAN variables for water, and
C                            changed both N1H2O & N3H2O from 13 to 11
C 23 Sep 1999 Scott Hannon   Change number of channel dimensions for
C                            new Sep99 version of fast model.
C  5 Apr 2000 Scott Hannon   Added MXEMIS
C  4 Aug 2000 Scott Hannon   Changes values for use with testfast
C 11 Aug 2000 Scott Hannon   Change from 4 to 5 term H2O continuum
C 23 Jan 2001 Scott Hannon   Update values of C1 & C2
C 15 Feb 2001 Scott Hannon   Add MAXPRO, CO2STD, IOERR, IOINFO,
C                            MXGAS, CSARTA, and all filenames
C 24 Apr 2001 Scott Hannon   Add MXMIEA and FNMIEA
C 14 Aug 2001 Scott Hannon   Add FNMIEE and FNMIEG
C 21 Nov 2001 Scott Hannon   Add VSARTA, VSCOEF, & VCLOUD; remove CSARTA
C 12 Sep 2002 Scott Hannon   Updated for m135f (-13.5 um with fringes)
C 17 Dec 2002 Scott Hannon   Updated for revised(Dec02) m135f 
C  3 Jan 2003 Scott Hannon   Updated VSARTA for version 1.04
C 06 Feb 2004 Scott Hannon   Add FNTMLT & update VSARTA for v1.05
C 07 Apr 2005 Scott Hannon   NFCOEF increased from 5 to 6 for v1.06
C 18 May 2005 Scott Hannon   update for HNO3 version
C 29 Jun 2005 Scott Hannon   "trace" version v1.07 with CO2,SO2,HNO3,N2O
C 13 Oct 2005 Scott Hannon   Add variables for non-LTE
C 22 Nov 2005 Scott Hannon   Replace set1,set2,CO2 coefs for new M12
C 02 May 2007 Scott Hannon   Added XSALT
C 08 May 2008 Scott Hannon   Updated for v1.08; change most filenames
C                            from FN* to double filenames FA* and FB*;
C                            add YOFFA,YOFFB,YOFMIN,YOFMAX,YOFDEF
C 13 May 2008 Scott Hannon   Add CO2NTE and NTEBOT for non-LTE calc and
C                            increase NNCOEF from 6 to 7
C 12 May 2009 Scott Hannon   Add VTUNNG string; delete VCLOUD


!END====================================================================
C
C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
C Note: having an "implicit none" in both the include file & the main
C source code will cause some compilers to complain.
c       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      EXECUTABLE CODE
C-----------------------------------------------------------------------
C      none

C      -----------------------------------------------------------------
C      Assign SARTA version strings
C      -----------------------------------------------------------------
C      The version strings consists of 3 parts: version number, date,
C      and comment.  The version date should be updated to the
C      current date whenever any portion of the code is updated.  The
C      version number consists of two parts; a major version to the
C      left of the decimal point, and a minor version to the right.
C      The major number should be incremented only when major changes
C      have been made to the overall SARTA code.  The minor number
C      should be incremented only when minor but non-trivial changes
C      are made to the code.  Bug fixes should generally be handled
C      with the version date, but a fix for a serious bug may warrant
C      a change to the minor version number.
C      See the "Doc/last_update.txt" file for a description of the
C      changes associated with every change of VSARTA.
C
       CHARACTER*40 VSARTA  ! SARTA source code version
       CHARACTER*40 VSCOEF  ! SARTA coefficient version
       CHARACTER*40 VTUNNG  ! optical depth tuning version
C      version template    '#.## YYYY-MM-DD <--------comment------->'
       PARAMETER( VSARTA = '1.08 2010-09-14 dual-freq calXSPECCALX' )
       PARAMETER( VSCOEF = 'AIRS 2008-05-07 m130 m150 CO2=385' )
       PARAMETER( VTUNNG = 'v6 standard; refprof N2O x1/1.04' )

C      *********
C      VARIABLES
C      *********
C      Note: these should not be changed by the user
C
C      ------------------------
C      Constants and other data
C      ------------------------
       REAL     PI ! pi, circle circumference/diameter (3.1415926)
       REAL RADSUN ! radius of the sun (6.956E+8 m)
       REAL     C1 ! radiation constant c1 (1.1911E-8  W/(m2.st.(cm-1)4)
       REAL     C2 ! radiation constant c2 (1.4387863 K/cm-1)
       PARAMETER(    PI = 3.1415926)
       PARAMETER(RADSUN = 6.956E+8)
C
Ccc    Previously used values; agrees w/JPL pre-Dec2000
Ccc    PARAMETER(    C1 = 1.1910439E-8)  ! JPL value is 1E+3 bigger
Ccc    PARAMETER(    C2 = 1.4387687)
C
C      Current values (CODATA98 from NIST); agrees w/JPL Dec2000
       PARAMETER(  C1 = 1.191042722E-8)  ! JPL value is 1E+3 bigger
       PARAMETER(  C2 = 1.4387752)
C
       REAL CO2STD ! standard CO2 PPMV mixing ratio (385)
C      Note: mean global CO2 approximately 385 in 2009
       PARAMETER( CO2STD = 385.0 )
C
       REAL  XSALT ! expected nominal satellite altitude (km)
       PARAMETER( XSALT = 705.0 )
C
C
C      -----------------------------------
C      Channels and layers other variables
C      -----------------------------------
       INTEGER MAXLAY ! # of layers (100)
       INTEGER   NSET ! # of coefficient data sets (7)
       INTEGER MXCHAN ! max total # of channels (2378)
       INTEGER NFCOEF ! # of downwelling thermal "F" factor coefs 
       INTEGER MXEMIS ! max # of input emis/rho data points
       INTEGER MAXPRO ! max # of user specified profiles
       INTEGER  MXGAS ! max # of gases in user profile
       INTEGER MXMIEA ! max # of mie particle sizes
       PARAMETER(MAXLAY = 100)
       PARAMETER(  NSET = 7)
       PARAMETER(MXCHAN = 2834)
       PARAMETER(NFCOEF = 6)
       PARAMETER(MXEMIS = 100)
       PARAMETER(MAXPRO = 25)
       PARAMETER( MXGAS = 44)
       PARAMETER(MXMIEA = 10)
C
C***********************************************************************
C      Variables for the coefficient sets
C***********************************************************************
C
C      --------------
C      For set1 = FWO
C      -------------
C      Used in part by modules: 12, 11, 10, 9, 8, 7, 6, 5, 3, 4b, 4a
       INTEGER MXCHN1 ! max # of channels for set1 = FWO (1461)
       INTEGER  N1CON ! # of water con predictors/coefs for set1 (5)
       INTEGER  N1FIX ! # of "fixed" predictors/coefs for set1 (8)
       INTEGER  N1H2O ! # of water predictors/coefs for set1 (13)
       INTEGER   N1O3 ! # of ozone predictors/coefs for set1 (5)
       INTEGER N1COEF ! total # of coefs for set1
       PARAMETER(MXCHN1 = 1461)
       PARAMETER( N1CON = 7)
       PARAMETER( N1FIX = 8)
       PARAMETER( N1H2O = 11)
       PARAMETER(  N1O3 = 5)
       PARAMETER(N1COEF = N1CON + N1FIX + N1H2O + N1O3 )
C
C
C      --------------
C      For set2 = FOW
C      --------------
C      Used in part by modules: 6, 5
       INTEGER MXCHN2 ! max # of channels for set2 = FOW  (325)
       INTEGER  N2CON ! # of water con predictors/coefs for set2 (5)
       INTEGER  N2FIX ! # of "fixed" predictors/coefs for set2 (8)
       INTEGER   N2O3 ! # of ozone predictors/coefs for set2 (10)
       INTEGER  N2H2O ! # of water predictors/coefs for set2 (11)
       INTEGER N2COEF ! total # of coefs for set2
       PARAMETER(MXCHN2 = 325)
       PARAMETER( N2CON = 7)
       PARAMETER( N2FIX = 8)
       PARAMETER(  N2O3 = 10)
       PARAMETER( N2H2O = 11)
       PARAMETER(N2COEF = N2CON + N2FIX + N2O3 + N2H2O )
C
C
C      --------------
C      For set3 = FMW
C      --------------
C      Used in part by modules: 4d, 4c, 3
       INTEGER MXCHN3 ! max # of channels for set3 = FMW  (396)
       INTEGER  N3CON ! # of water con predictors/coefs for set3 (5)
       INTEGER  N3FIX ! # of "fixed" predictors/coefs for set3 (8)
       INTEGER  N3CH4 ! # of methane predictors/coefs for set3 (9)
       INTEGER  N3H2O ! # of water predictors/coefs for set3 (13)
       INTEGER N3COEF ! total # of coefs for set3
       PARAMETER(MXCHN3 = 396)
       PARAMETER( N3CON = 7)
       PARAMETER( N3FIX = 8)
       PARAMETER( N3CH4 = 9)
       PARAMETER( N3H2O = 11)
       PARAMETER(N3COEF = N3CON + N3FIX + N3CH4 + N3H2O )
C
C
C      ---------------
C      For set4 = sun FCOW
C      ---------------
C      Used in part by modules: 2b
       INTEGER MXCHN4 ! max # of channels for set4 = FCOW (85)
       INTEGER  N4CON ! # of water con predictors/coefs for set4 (5)
       INTEGER  N4FIX ! # of "fixed" predictors/coefs for set4 (11)
       INTEGER   N4CO ! # of CO predictors/coefs for set4 (11)
       INTEGER   N4O3 ! # of ozone predictors/coefs for set4 (3)
       INTEGER  N4H2O ! # of water predictors/coefs for set4 (13)
       INTEGER N4COEF ! total # of coefs for set4
       PARAMETER(MXCHN4 = 85)
       PARAMETER( N4CON = 7)
       PARAMETER( N4FIX = 11)
       PARAMETER(  N4CO = 11)
       PARAMETER(  N4O3 = 3)
       PARAMETER( N4H2O = 13)
       PARAMETER(N4COEF = N4CON + N4FIX + N4CO + N4O3 + N4H2O )
C
C
C      -----------------------
C      For set5 = sun BFSW
C      -----------------------
C      Used in part by modules: 2b, 1b
       INTEGER MXCHN5 ! max # of channels for set5 = BFSW (210)
       INTEGER  N5CON ! # of water con predictors/coefs for set5 (5)
       INTEGER  N5FIX ! # of "fixed" predictors/coefs for set5 (11)
       INTEGER  N5H2O ! # of water predictors/coefs for set5 (3)
       INTEGER   N5O3 ! # of ozone predictors/coefs for set5 (1)
       INTEGER N5COEF ! total # of coefs for set5
       PARAMETER(MXCHN5 = 210)
       PARAMETER( N5CON = 7)
       PARAMETER( N5FIX = 11)
       PARAMETER( N5H2O = 3)
       PARAMETER(  N5O3 = 1)
       PARAMETER(N5COEF = N5CON + N5FIX + N5H2O + N5O3 )
C
C
C      -----------------------
C      For set6 = sun MFMW
C      -----------------------
C      Used in part by modules: 1b, 2a
       INTEGER MXCHN6 ! max # of channels for set6 = MFMW (217)
       INTEGER  N6CON ! # of water con predictors/coefs for set6 (5)
       INTEGER  N6FIX ! # of "fixed" predictors/coefs for set6 (8)
       INTEGER  N6H2O ! # of water predictors/coefs for set6 (7)
       INTEGER   N6O3 ! # of ozone predictors/coefs for set6 (1)
       INTEGER N6COEF ! total # of coefs for set6
       PARAMETER(MXCHN6 = 217)
       PARAMETER( N6CON = 7 )
       PARAMETER( N6FIX = 8 )
       PARAMETER( N6H2O = 7 )
       PARAMETER(  N6O3 = 1 )
       PARAMETER(N6COEF = N6CON + N6FIX + N6H2O + N6O3 )
C
C
C      -----------------------
C      For set7 = sun MFBW
C      -----------------------
C      Used in part by modules: 2a, 1a
       INTEGER MXCHN7 ! max # of channels for set7 = MFBW (140)
       INTEGER  N7CON ! # of water con predictors/coefs for set7 (5)
       INTEGER  N7FIX ! # of "fixed" predictors/coefs for set7 (8)
       INTEGER  N7H2O ! # of water predictors/coefs for set7 (13)
       INTEGER   N7O3 ! # of ozone predictors/coefs for set7 (1)
       INTEGER N7COEF ! total # of coefs for set7
       PARAMETER(MXCHN7 = 140)
       PARAMETER( N7CON = 7)
       PARAMETER( N7FIX = 8)
       PARAMETER( N7H2O = 13)
       PARAMETER(  N7O3 = 1)
       PARAMETER(N7COEF = N7CON + N7FIX + N7H2O + N7O3 )
C
C
C      ---------------
C      For trace gases predictors
C      ---------------
       INTEGER NTRACE ! number of trace gas perturbation predictors (7)
       PARAMETER(NTRACE = 7)
C
C
C      ----------------
C      For variable CO2
C      ----------------
C      Used in part by modules: 12, 11, 10, 9, 7, 6, 5, 2b, 1b, 2a
       INTEGER MXCHNC ! max # of channels with CO2 pert coefs (1082)
       INTEGER   NCO2 ! number of CO2 coefficients
       PARAMETER(MXCHNC = 1082)
       PARAMETER(  NCO2 = 5)
C
C
C      ----------------
C      For variable SO2
C      ----------------
       INTEGER MXCHNS ! max # of channels with SO2 pert coefs (602)
       INTEGER   NSO2 ! number of SO2 coefficients
       PARAMETER(MXCHNS = 602)
       PARAMETER(  NSO2 = 4)
C
C
C      -----------------
C      For variable HNO3
C      -----------------
       INTEGER MXCHNH ! max # of channels with HNO3 pert coefs (383)
       INTEGER  NHNO3 ! number of HNO3 coefficients
       PARAMETER(MXCHNH = 383)
       PARAMETER( NHNO3 = 4)
C
C
C      -----------------
C      For variable N2O
C      -----------------
       INTEGER MXCHNN ! max # of channels with N2O pert coefs (586)
       INTEGER   NN2O ! number of N2O coefficients
       PARAMETER(MXCHNN = 586)
       PARAMETER(  NN2O = 7)
C
C
C      ----------------------
C      For OPTRAN water coefs
C      ----------------------
C      Used in part by modules:
       INTEGER MXCHNW ! max # of channelss with OPTRAN H2O coefs (754)
       INTEGER MXOWLY ! number of OPTRAN water layers
       INTEGER NOWAVG ! # of OPTRAN water average profile values (4)
       INTEGER NH2O   ! number of OPTRAN H2O predictors/coefs (9)
       PARAMETER(MXCHNW = 754)
       PARAMETER(MXOWLY = 300)
       PARAMETER(NOWAVG = 4)
       PARAMETER(  NH2O = 9)
C
C      -----------
C      For non-LTE
C      -----------
       INTEGER MXCNTE ! max # of channels for non-LTE (203)
       INTEGER NNCOEF ! # of coefs for non-LTE
       INTEGER NTEBOT ! bottom layer for CO2TOP calc
       REAL CO2NTE ! ref CO2 mixing ratio for non-LTE coefs (ppmv)
       PARAMETER(MXCNTE = 203)
       PARAMETER(NNCOEF = 7)
       PARAMETER(NTEBOT = 10)
       PARAMETER(CO2NTE = 370.0)
C
C      ----------------
C      Single filenames
C      ----------------
       CHARACTER*80 FNFX   ! coef fx
       CHARACTER*80 FNPREF ! reference profile
       PARAMETER(FNFX  =
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/fx.txt')
       PARAMETER(FNPREF=
c     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/profref_trace385'
C      refprof N2O scaled by 1/1.04 for tuning purposes
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'profref_trace385tuned')
C
C      ----------------
C      Double filenames
C      ----------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       REAL YOFFA          ! yoffset for database "A"
       CHARACTER*80 FACOF1 ! coef set1 
       CHARACTER*80 FACOF2 ! coef set2 
       CHARACTER*80 FACOF3 ! coef set3 
       CHARACTER*80 FACOF4 ! coef set4 
       CHARACTER*80 FACOF5 ! coef set5 
       CHARACTER*80 FACOF6 ! coef set6 
       CHARACTER*80 FACOF7 ! coef set7 
       CHARACTER*80 FACO2  ! coef CO2
       CHARACTER*80 FAN2O  ! coef N2O
       CHARACTER*80 FASO2  ! coef SO2
       CHARACTER*80 FAHNO3 ! coef HNO3
       CHARACTER*80 FAOPTR ! coef optran
       CHARACTER*80 FATHER ! coef therm
       CHARACTER*80 FACOFN ! coef non-LTE
       CHARACTER*80 FASUN  ! solar data
       CHARACTER*80 FATMLT ! tuning multiplier
C
       PARAMETER(YOFFA=-13.0)
       PARAMETER(FACOF1=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set1_m130.dat')
       PARAMETER(FACOF2=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set2_m130.dat')
       PARAMETER(FACOF3=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set3_m130.dat')
       PARAMETER(FACOF4=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set4_m130.dat')
       PARAMETER(FACOF5=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set5_m130.dat')
       PARAMETER(FACOF6=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set6_m130.dat')
       PARAMETER(FACOF7=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set7_m130.dat')
       PARAMETER(FACO2 =
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'CO2_5term_m130.dat')
       PARAMETER(FAN2O =
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'N2O_m130.dat')
       PARAMETER(FASO2 =
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'SO2_m130.dat')
       PARAMETER(FAHNO3 =
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'HNO3_m130.dat')
       PARAMETER(FAOPTR=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'optran_m130.dat')
       PARAMETER(FATHER=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'therm_m130.dat')
       PARAMETER(FACOFN=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'nonLTE7_m130.dat')
       PARAMETER(FASUN =
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Solar/solar_m130.txt')
       PARAMETER(FATMLT=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'tunmlt_m130.txt')
ccc
c     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
c     $ // 'tunmlt_ones.txt')
ccc
c     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
c     $ // 'tunmlt_wcon_nte.txt')
ccc
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
       REAL YOFFB          ! yoffset for database "B"
       CHARACTER*80 FBCOF1 ! coef set1 
       CHARACTER*80 FBCOF2 ! coef set2 
       CHARACTER*80 FBCOF3 ! coef set3 
       CHARACTER*80 FBCOF4 ! coef set4 
       CHARACTER*80 FBCOF5 ! coef set5 
       CHARACTER*80 FBCOF6 ! coef set6 
       CHARACTER*80 FBCOF7 ! coef set7 
       CHARACTER*80 FBCO2  ! coef CO2
       CHARACTER*80 FBN2O  ! coef N2O
       CHARACTER*80 FBSO2  ! coef SO2
       CHARACTER*80 FBHNO3 ! coef HNO3
       CHARACTER*80 FBOPTR ! coef optran
       CHARACTER*80 FBTHER ! coef therm
       CHARACTER*80 FBCOFN ! coef non-LTE
       CHARACTER*80 FBSUN  ! solar data
       CHARACTER*80 FBTMLT ! tuning multiplier
C
       PARAMETER(YOFFB=-15.0)
       PARAMETER(FBCOF1=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set1_m150.dat')
       PARAMETER(FBCOF2=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set2_m150.dat')
       PARAMETER(FBCOF3=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set3_m150.dat')
       PARAMETER(FBCOF4=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set4_m150.dat')
       PARAMETER(FBCOF5=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set5_m150.dat')
       PARAMETER(FBCOF6=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set6_m150.dat')
       PARAMETER(FBCOF7=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'set7_m150.dat')
       PARAMETER(FBCO2 =
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'CO2_5term_m150.dat')
       PARAMETER(FBN2O =
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'N2O_m150.dat')
       PARAMETER(FBSO2 =
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'SO2_m150.dat')
       PARAMETER(FBHNO3 =
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'HNO3_m150.dat')
       PARAMETER(FBOPTR=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'optran_m150.dat')
       PARAMETER(FBTHER=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'therm_m150.dat')
       PARAMETER(FBCOFN=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'nonLTE7_m150.dat')
       PARAMETER(FBSUN =
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Solar/solar_m150.txt')
       PARAMETER(FBTMLT=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'tunmlt_m150.txt')
ccc
c     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
c     $ // 'tunmlt_ones.txt')
ccc
c     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
c     $ // 'tunmlt_wcon_nte.txt')
ccc
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       CHARACTER*80 FNSCAL ! spectral calibration
       PARAMETER(FNSCAL=
     $ '/asl/data/sarta_database/Data_AIRS_apr08/Coef/'
     $ // 'speccal_XSPECCALX.txt')
c     $ // 'speccal_20031119.txt')
c     $ // 'speccal_20050301.txt')
C
C      ----------------------
C      Default & min/max YOFF
C      ----------------------
C      Note: these should be based YOFFA and YOFFB as well as the
C      expected range of variation of YOFF
       REAL YOFDEF   ! default YOFF
       REAL YOFMIN   ! min allowed YOFF
       REAL YOFMAX   ! max allowed YOFF
       PARAMETER(YOFDEF=-14.0)
       PARAMETER(YOFMIN=-16.0)
       PARAMETER(YOFMAX=-11.0)

C      ----------------
C      I/O unit numbers
C      ----------------
C      Note: these units are not explicitly openned by the sarta code,
C      they should be set to standard I/O units for your compiler
       INTEGER IOINFO  ! unit number for non-error info messages (6)
       INTEGER IOERR   ! unit number for error messages (2 or 6)
       PARAMETER( IOINFO = 6 )
       PARAMETER( IOERR = 0 )
C
C
C      -----------------
C      Allowed input GUC (Gas Units Code number)
C      -----------------
       INTEGER GUCIN  ! The one & only allowed input GUC number
C      Note: GUCIN must be 1 or 2.  All gases in the input RTP
C      must be of this type.
       PARAMETER( GUCIN = 1 ) ! GUC number for:  molecules/cm^2
c       PARAMETER( GUCIN = 2 ) ! GUC number for:  kilomoles/cm^2

C      End of include file
