#
# Makefile for SARTA v1.08.  Most of the work is done by make_sarta.
#

# ------
# Builds
# ------

apr08_m130_m150_cal20050301_wcon_nte:
	rm -f *.o incFTC.f
	sed 's/XSPECCALX/20050301/g' incFTC_airs_apr08_m130_m150_template_cal_wcon_nte.f > incFTC.f
	make -f make_sarta_df
	mv a.out ../BinV201/sarta_apr08_m130_m150_cal20050301_wcon_nte

### AIRS pre-Nov2003 ###

apr08_m130x_m140x_370_cal20020830:
	rm -f *.o incFTC.f
	sed 's/XSPECCALX/20020830/g' incFTC_airs_apr08_m130x_m140x_370_template_cal.f > incFTC.f
	make -f make_sarta_df
	mv a.out ../BinV201/sarta_apr08_m130x_m140x_370_cal20020830

apr08_m130x_m140x_370_cal20020917:
	rm -f *.o incFTC.f
	sed 's/XSPECCALX/20020917/g' incFTC_airs_apr08_m130x_m140x_370_template_cal.f > incFTC.f
	make -f make_sarta_df
	mv a.out ../BinV201/sarta_apr08_m130x_m140x_370_cal20020917

apr08_m130x_m140x_370_cal20021022:
	rm -f *.o incFTC.f
	sed 's/XSPECCALX/20021022/g' incFTC_airs_apr08_m130x_m140x_370_template_cal.f > incFTC.f
	make -f make_sarta_df
	mv a.out ../BinV201/sarta_apr08_m130x_m140x_370_cal20021022

apr08_m130x_m140x_370_cal20030110:
	rm -f *.o incFTC.f
	sed 's/XSPECCALX/20030110/g' incFTC_airs_apr08_m130x_m140x_370_template_cal.f > incFTC.f
	make -f make_sarta_df
	mv a.out ../BinV201/sarta_apr08_m130x_m140x_370_cal20030110

apr08_m140x_370:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140x/g' incFTC_airs_apr08_template_m130x_m140x_370.f > incFTC.f
	make -f make_sarta
	mv a.out ../BinV201/sarta_apr08_m140x_370

apr08_m140x_370_wcon_nte:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140x/g' incFTC_airs_apr08_template_m130x_m140x_370_wcon_nte.f > incFTC.f
	make -f make_sarta
	mv a.out ../BinV201/sarta_apr08_m140x_370_wcon_nte


### AIRS post-Nov2003 ###

apr08_m130_m150_cal20031119:
	rm -f *.o incFTC.f
	sed 's/XSPECCALX/20030110/g' incFTC_airs_apr08_m130_m150_template_cal.f > incFTC.f
	make -f make_sarta_df
	mv a.out ../BinV201/sarta_apr08_m130_m150_cal20031119

apr08_m130_m150_cal20050301:
	rm -f *.o incFTC.f
	sed 's/XSPECCALX/20050301/g' incFTC_airs_apr08_m130_m150_template_cal.f > incFTC.f
	make -f make_sarta_df
	mv a.out ../BinV201/sarta_apr08_m130_m150_cal20050301

apr08_m140:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150.f > incFTC.f
	make -f make_sarta
	mv a.out ../BinV201/sarta_apr08_m140

apr08_m140_wf:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150.f > incFTC.f
	make -f make_sarta_wf
	mv a.out ../BinV201/sarta_apr08_m140_wf

apr08_m140_wcon_nte:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150_wcon_nte.f > incFTC.f
	make -f make_sarta
	mv a.out ../BinV201/sarta_apr08_m140_wcon_nte


### IASI ###

iasi_may09_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi_may09_wcon_nte.f incFTC.f
	make -f make_sarta
	mv a.out ../BinV201/sarta_iasi_may09_wcon_nte


### CrIS ###

cris_apr09_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_cris_apr09_wcon_nte.f incFTC.f
	make -f make_sarta
	mv a.out ../BinV201/sarta_cris_apr09_wcon_nte

crisg4_nov09_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_crisg4_nov09_wcon_nte.f incFTC.f
	make -f make_sarta
	mv a.out ../BinV201/sarta_crisg4_nov09_wcon_nte


# ---------------
# File management
# ---------------

clean:
	rm -f *.o
