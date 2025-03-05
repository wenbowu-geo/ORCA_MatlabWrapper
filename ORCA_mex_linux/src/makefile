# build orca

EXEPATH = .
LIBPATH = .
OBJPATH = ./obj
MEXLIB = /usr/local/MATLAB/R2017b/extern/include

FLAGS =	-Og -fno-underscoring -fPIC -std=legacy -cpp -ffree-line-length-none

OBJS = airy_hsp.o alloc_vars.o blug.o contour_find.o cub_fit_new.o \
 cub_root_new.o duct_calcs.o eig_disp.o eig_findm.o em_calc.o ep_calc.o \
 fix_path.o freq_init.o h_space.o mfun_fill.o mode_branch.o mode_find.o \
 mode_fun.o opt_read.o phz_calc.o r1r2_calc.o rp_calc.o rp_flay.o \
 rp_nomm.o rp_nomm0.o rp_sfint.o rp_slay.o rp_slay0.o scairy2.o sr_geom.o \
 svp_check.o svp_read.o tools.o traj_mag.o vbl_init.o xi_cut_set.o xkh_init.o zmx_init.o

MODS = Parms_com.o gen_com.o i_o_com.o scairy_com.o

sub_orca.mexa64: sub_orca.c liborca.a
	mex FFLAGS='$FFLAGS -cpp' -g sub_orca.c liborca.a -lgfortran

liborca.a:	${OBJS} ${MODS} orca_mex.o
		ar urv ${LIBPATH}/liborca.a orca_mex.o ${OBJS} ${MODS}
		rm ${OBJS} ${MODS} orca_mex.o \
                   parms_com.mod gen_com.mod i_o_com.mod scairy_com.mod

# modules
Parms_com.mod: Parms_com.o Parms_com.f90
	gfortran -c ${FLAGS} Parms_com.f90
Parms_com.o: Parms_com.f90
	gfortran -c ${FLAGS} Parms_com.f90
gen_com.mod: Parms_com.mod gen_com.o gen_com.f90
	gfortran -c ${FLAGS} gen_com.f90
gen_com.o: gen_com.f90
	gfortran -c ${FLAGS} gen_com.f90
i_o_com.mod: Parms_com.mod i_o_com.o i_o_com.f90
	gfortran -c ${FLAGS} i_o_com.f90
i_o_com.o: i_o_com.f90
	gfortran -c ${FLAGS} i_o_com.f90
scairy_com.mod: scairy_com.o scairy_com.f90
	gfortran -c ${FLAGS} scairy_com.f90
scairy_com.o: scairy_com.f90
	gfortran -c ${FLAGS} scairy_com.f90

# subroutines
orca_mex.o: Parms_com.mod gen_com.mod i_o_com.mod orca_mex.f90
	gfortran ${FLAGS} -c orca_mex.f90
airy_hsp.o: Parms_com.mod gen_com.mod i_o_com.mod airy_hsp.f
	gfortran -c ${FLAGS} airy_hsp.f
alloc_vars.o: Parms_com.mod gen_com.mod i_o_com.mod alloc_vars.f
	gfortran -c ${FLAGS} alloc_vars.f
blug.o: Parms_com.mod blug.f
	gfortran -c ${FLAGS} blug.f
contour_find.o: Parms_com.mod gen_com.mod i_o_com.mod contour_find.f
	gfortran -c ${FLAGS} contour_find.f
cub_fit_new.o: cub_fit_new.f
	gfortran -c ${FLAGS} cub_fit_new.f
cub_root_new.o: cub_root_new.f
	gfortran -c ${FLAGS} cub_root_new.f
duct_calcs.o: Parms_com.mod gen_com.mod i_o_com.mod duct_calcs.f
	gfortran -c ${FLAGS} duct_calcs.f
eig_disp.o: eig_disp.f
	gfortran -c ${FLAGS} eig_disp.f
eig_findm.o: Parms_com.mod gen_com.mod i_o_com.mod eig_findm.f
	gfortran -c ${FLAGS} eig_findm.f
em_calc.o: scairy_com.mod em_calc.f
	gfortran -c ${FLAGS} em_calc.f
ep_calc.o: scairy_com.mod ep_calc.f
	gfortran -c ${FLAGS} ep_calc.f
fix_path.o: Parms_com.mod gen_com.mod i_o_com.mod fix_path.f
	gfortran -c ${FLAGS} fix_path.f
freq_init.o: Parms_com.mod gen_com.mod i_o_com.mod freq_init.f
	gfortran -c ${FLAGS} freq_init.f
h_space.o: scairy_com.mod h_space.f
	gfortran -c ${FLAGS} h_space.f
mfun_fill.o: Parms_com.mod gen_com.mod i_o_com.mod mfun_fill.f
	gfortran -c ${FLAGS} mfun_fill.f
mode_branch.o: Parms_com.mod gen_com.mod i_o_com.mod mode_branch.f
	gfortran -c ${FLAGS} mode_branch.f
mode_find.o: Parms_com.mod gen_com.mod i_o_com.mod mode_find.f
	gfortran -c ${FLAGS} mode_find.f
mode_fun.o: Parms_com.mod gen_com.mod i_o_com.mod mode_fun.f
	gfortran -c ${FLAGS} mode_fun.f
opt_read.o: Parms_com.mod gen_com.mod i_o_com.mod opt_read.f
	gfortran -c ${FLAGS} opt_read.f
phz_calc.o: Parms_com.mod gen_com.mod i_o_com.mod phz_calc.f
	gfortran -c ${FLAGS} phz_calc.f
r1r2_calc.o: Parms_com.mod gen_com.mod r1r2_calc.f
	gfortran -c ${FLAGS} r1r2_calc.f
rp_calc.o: Parms_com.mod gen_com.mod i_o_com.mod rp_calc.f
	gfortran -c ${FLAGS} rp_calc.f
rp_flay.o: rp_flay.f
	gfortran -c ${FLAGS} rp_flay.f
rp_nomm.o: rp_nomm.f
	gfortran -c ${FLAGS} rp_nomm.f
rp_nomm0.o: rp_nomm0.f
	gfortran -c ${FLAGS} rp_nomm0.f
rp_sfint.o: rp_sfint.f
	gfortran -c ${FLAGS} rp_sfint.f
rp_slay.o: rp_slay.f
	gfortran -c ${FLAGS} rp_slay.f
rp_slay0.o: rp_slay0.f
	gfortran -c ${FLAGS} rp_slay0.f
scairy2.o: scairy_com.mod scairy2.f
	gfortran -c ${FLAGS} scairy2.f
sr_geom.o: Parms_com.mod gen_com.mod i_o_com.mod sr_geom.f
	gfortran -c ${FLAGS} sr_geom.f
svp_check.o: Parms_com.mod gen_com.mod i_o_com.mod svp_check.f
	gfortran -c ${FLAGS} svp_check.f
svp_read.o: Parms_com.mod i_o_com.mod svp_read.f
	gfortran -c ${FLAGS} svp_read.f
tools.o: tools.f
#	gfortran -ffixed-line-length-0 -fPIC -std=legacy -c tools.f
	gfortran -c ${FLAGS} -ffixed-line-length-0 tools.f
traj_mag.o: Parms_com.mod gen_com.mod i_o_com.mod traj_mag.f
	gfortran -c ${FLAGS} traj_mag.f
vbl_init.o: scairy_com.mod vbl_init.f
	gfortran -c ${FLAGS} vbl_init.f
xi_cut_set.o: Parms_com.mod gen_com.mod i_o_com.mod xi_cut_set.f
	gfortran -c ${FLAGS} xi_cut_set.f
xkh_init.o: Parms_com.mod gen_com.mod i_o_com.mod xkh_init.f
	gfortran -c ${FLAGS} xkh_init.f
zmx_init.o: Parms_com.mod gen_com.mod i_o_com.mod zmx_init.f
	gfortran -c ${FLAGS} zmx_init.f
