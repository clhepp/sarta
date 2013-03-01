README.TXT file for /asl/packages/sartaV108/BinV201

Cloudy SARTA:
The filename of cloudy SARTA executables notes the
included aerosol types it uses, eg "iceaggr", "waterdrop",
"desertdust", AmNit", "Amsul", "biom", and others.

Tuning:
Minimal tuning is usually noted by "_wcon_nte", which
means the only tuning is the water continuum and the
non-LTE.

-------------------------------------------------------------------------------
AIRS: "sarta_airs_PGEv6"

Dual-frequency version of sartaV108.  Uses prof.pnote to import the full
17 module Yoffsets, orbit phase, and Doppler (ppm) data.  Will revert to
using prof.freqcal Yoffset for all models (and no Doppler) if pnote is not
used.

preNov2003 = pre November 2003 with official v6 tuning; nominal 370 ppmv CO2
preNov2003_wcon_nte = pre November 2003 without tuning; nominal 370 ppmv CO2

postNov2003 = post November 2003 with official v6 tuning; nominal 385 ppmv CO2
postNov2003_wcon_nte = post November 2003 without tuning; nominal 385 ppmv CO2


-------------------------------------------------------------------------------
AIRS: "sarta_apr08"

Obsolete AIRS excecutable filenames begin "sarta_apr08". The
"apr08" stands for April 2008, which is the month & year
the coefficient database was constructed.

There are numberous variants of the SARTA code.  These
variants generally consist of three types: spectral
calibration, clouds, and tuning.

Yoffset spectral calibration is noted by "m###", which refers
to the overall Yoffset of the grating model used to generate
the SRFs. The means are:
m130x = pre Nov 2003,  Yoffset = -13.0
m140x = pre Nov 2003,  Yoffset = -14.0
Note: "_370" following the m#### refers to the default CO2 ppmv.
m130  = post Nov 2003, Yoffset = -13.0
m140  = post Nov 2003, Yoffset = -14.0
m150  = post Nov 2003, Yoffset = -15.0
Filenames with two "m###" substrings are for the dual frequency
(aka "df") variant of SARTA which interpolates/extrapolates to
any Yoffset using the two indicated coef databases.

Variants include "nh3" for non-standard breakout gas NH3.
 
------------------------------------------------------------------------------
IASI: "sarta_iasi_may09"

All IASI excecutable filenames begin "sarta_iasi_may09". The
"may09" stands for May 2009, which is the month & year
the coefficient database was constructed.

Variants include "swch4" for non-standard breakout gas
shortwave CH4.

------------------------------------------------------------------------------
CrIS: "sarta_cris"

All CrIS excecutable filenames begin "sarta_cris". The
"sarta_cris_apr09" executables use the April 2009 database
(which has no guard channels).  The "sarta_crisg4_nov09"
uses a modified version of the database (created in November
2009) with 4 guard channels at each end of each band.

The "sarta_OSScris2008_hamming" executable uses the 2008
AER,Inc CrIS fast model algoithm (with Hamming apodization
coefficients) put into a SARTA-like wrapper.

All our CriS fast models use Hamming apodization.  We
do not have any codes with other apodizations.

---end of file---

total 7855
 20 drwxr-xr-x  6 hannon pi_strow     21 Nov 21 12:46 .
  6 drwxr-xr-x 20 hannon pi_strow     34 Nov 18 15:13 ..
  3 drwxr-xr-x  3 hannon pi_strow      7 Nov 21 12:32 Junk
 20 drwxr-xr-x  2 hannon pi_strow     39 Nov 21 12:33 Old
 20 drwxr-xr-x  2 hannon pi_strow     35 Dec 20  2010 Old_AIRS
 20 drwxr-xr-x  2 hannon pi_strow      6 Sep 14  2010 Old_IASI
  3 -rw-r--r--  1 hannon pi_strow   2555 Oct  7 16:35 README.TXT
557 -rwxr-xr-x  1 hannon pi_strow 810603 Nov 18 15:15 sarta_airs_PGEv6_postNov2003
557 -rwxr-xr-x  1 hannon pi_strow 810603 Nov 18 15:15 sarta_airs_PGEv6_postNov2003_wcon_nte
557 -rwxr-xr-x  1 hannon pi_strow 810611 Nov 18 15:14 sarta_airs_PGEv6_preNov2003
557 -rwxr-xr-x  1 hannon pi_strow 810611 Nov 18 15:14 sarta_airs_PGEv6_preNov2003_wcon_nte
538 -rwxr-xr-x  1 hannon pi_strow 785168 Nov 21 12:46 sarta_apr08_m140
576 -rwxr-xr-x  1 hannon pi_strow 849113 Nov 21 12:45 sarta_apr08_m140_iceaggr_waterdrop_desertdust_slabcloud_hg3
576 -rwxr-xr-x  1 hannon pi_strow 849113 Nov 21 12:42 sarta_apr08_m140_iceaggr_waterdrop_desertdust_slabcloud_hg3_wcon_nte
538 -rwxr-xr-x  1 hannon pi_strow 785168 Nov 21 12:37 sarta_apr08_m140_wcon_nte
548 -rwxr-xr-x  1 hannon pi_strow 792022 Nov 21 12:40 sarta_apr08_m140_wcon_nte_nh3
538 -rwxr-xr-x  1 hannon pi_strow 785168 Nov 21 12:36 sarta_crisg4_nov09_wcon_nte
548 -rwxr-xr-x  1 hannon pi_strow 789942 Nov 21 12:39 sarta_crisg4_nov09_wcon_nte_nh3
577 -rwxr-xr-x  1 hannon pi_strow 849560 Nov 21 12:41 sarta_iasi_may09_iceaggr_waterdrop_desertdust_slabcloud_hg3_wcon_nte_swch4
548 -rwxr-xr-x  1 hannon pi_strow 789917 Nov 21 12:38 sarta_iasi_may09_wcon_nte_swch4
552 -rwxr-xr-x  1 hannon pi_strow 796721 Nov 21 12:39 sarta_iasi_may09_wcon_nte_swch4_nh3
