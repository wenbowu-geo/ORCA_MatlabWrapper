# makefile
# library name
orca_mex: Parms_com.o airy_hsp.o alloc_vars.o blug.o contour_find.o cub_fit_new.o \
 cub_root_new.o duct_calcs.o eig_disp.o eig_findm.0 em_calc.o ep_calc.o \
 fix_path.o freq_init.o h_space.o mfun_fill.o mode_branch.o mode_find.o \
 mode_fun.o opt_read.o orca_mex.o phz_calc.o r1r2_calc.o rp_calc.o rp_flay.o \
 rp_nomm.o rp_nomm0.o rp_sfint.o rp_slay.o rp_slay0.o scairy2.o sr_geom.o \
 svp_check.o svp_read.o tools.o traj_mag.o vbl_init.o xi_cut_set.o xkh_init.o zmx_init.o

	gfortran -o orca_mex Parms_com.o gen_com.o \
         i_o_com.o scairy_com.o airy_hsp.o alloc_vars.o blug.o \
         contour_find.o cub_fit_new.o cub_root_new.o duct_calcs.o eig_disp.o \
         eig_findm.0 em_calc.o ep_calc.o fix_path.o freq_init.o h_space.o \
         mfun_fill.o mode_branch.o mode_find.o mode_fun.o opt_read.o orca_mex.o \
         phz_calc.o r1r2_calc.o rp_calc.o rp_flay.o rp_nomm.o rp_nomm0.o rp_sfint.o \
         rp_slay.o rp_slay0.o scairy2.o sr_geom.o svp_check.o svp_read.o tools.o \
         traj_mag.o vbl_init.o xi_cut_set.o xkh_init.o zmx_init.o

        # modules
	Parms_com.mod: Parms_com.o Parms_com.f90
	 gfortran -c Parms_com.f90
	Parms_com.o: Parms_com.f90
	 gfortran -c Parms_com.f90
	gen_com.mod: Parms_com.mod gen_com.o gen_com.f90
	 gfortran -c gen_com.f90
	gen_com.o: gen_com.f90
	 gfortran -c gen_com.f90
	i_o_com.mod: Parms_com.mod i_o_com.o i_o_com.f90
	 gfortran -c i_o_com.f90
	i_o_com.o: i_o_com.f90
	 gfortran -c i_o_com.f90
	scairy_com.mod: scairy_com.o scairy_com.f90
	 gfortran -c scairy_com.f90
	scairy_com.o: scairy_com.f90
	 gfortran -c scairy_com.f90
        # subroutines
	airy_hsp.o: Parms_com.mod gen_com.mod i_o_com.mod airy_hsp.f
	gfortran -c airy_hsp.f
	alloc_vars.o: Parms_com.mod gen_com.mod i_o_com.mod alloc_vars.f
	gfortran -c alloc_vars.f90
	blug.o: Parms_com.mod blug.f
	gfortran -c  blug.o
	contour_find.o: Parms_com.mod gen_com.mod i_o_com.mod contour_find.f
	gfortran -c contour_find.f
	cub_fit_new.o: cub_fit_new.f
	gfortran -c cub_fit_new.f
	cub_root_new.o: cub_root_new.f
	gfortran -c cub_root_new.f
	duct_calcs.o: Parms_com.mod gen_com.mod i_o_com.mod duct_calcs.f
	gfortran -c  duct_calcs.f
	eig_disp.o: eig_disp.f
	gfortran -c eig_disp.f
	eig_findm.o: Parms_com.mod gen_com.mod i_o_com.mod eig_findm.f
	gfortran -c eig_findm.f
	em_calc.o: scairy_com.mod em_calc.f
	gfortran -c em_calc.f
	ep_calc.o: scairy_com.mod ep_calc.f
	gfortran -c ep_calc.f
	fix_path.o: Parms_com.mod gen_com.mod i_o_com.mod fix_path.f
	gfortran -c fix_path.f
	freq_init.o Parms_com.mod gen_com.mod i_o_com.mod freq_init.f
	gfortran -c freq_init.f
	h_space.o: scairy_com.mod h_space.f
	gfortran -c h_space.f
	mfun_fill.o: Parms_com.mod gen_com.mod i_o_com.mod mfun_fill.f
	gfortran -c mfun_fill.f
	mode_branch.o: Parms_com.mod gen_com.mod i_o_com.mod mode_branch.f
	gfortran -c mode_branch.f
	mode_find.o: Parms_com.mod gen_com.mod i_o_com.mod mode_find.f
	gfortran -c mode_find.f
	mode_fun.o: Parms_com.mod gen_com.mod i_o_com.mod mode_fun.f
	gfortran -c mode_fun.f
	opt_read.o: Parms_com.mod gen_com.mod i_o_com.mod opt_read.f
	gfortran -c opt_read.f
	orca_mex.o: Parms_com.mod gen_com.mod i_o_com.mod orca_mex.f90
	gfortran -c orca_mex.f90
	phz_calc.o: Parms_com.mod gen_com.mod i_o_com.mod phz_calc.f
	gfortran -c phz_calc.f
	r1r2_calc.o: Parms_com.mod gen_com.mod r1r2_calc.f
	gfortran -c r1r2_calc.f
	rp_calc.o: Parms_com.mod gen_com.mod i_o_com.mod rp_calc.f
	gfortran -c rp_calc.f
	rp_flay.o: rp_flay.f
	gfortran -c rp_flay.f
	rp_nomm.o: rp_nomm.f
	gfortran -c rp_nomm.f
	rp_nomm0.o: rp_nomm0.f
	gfortran -c rp_nomm0.f
	rp_sfint.o: rp_sfint.f
	gfortran -c rp_sfint.f
	rp_slay.o: rp_slay.f
	gfortran -c rp_slay.f
	rp_slay0.o: rp_slay0.f
	gfortran -c rp_slay0.f
	scairy2.o: scairy_com.mod scairy2.f
	gfortran -c scairy2.f
	sr_geom.o: Parms_com.mod gen_com.mod i_o_com.mod sr_geom.f
	gfortran -c sr_geom.f
	svp_check.o: Parms_com.mod gen_com.mod i_o_com.mod svp_check.f
	gfortran -c svp_check.f
	svp_read.o: Parms_com.mod i_o_com.mod svp_read.f
	gfortran -c svp_read.f
	tools.o: tools.f
	gfortran -c tools.f
	traj_mag.o: Parms_com.mod gen_com.mod i_o_com.mod traj_mag.f
	gfortran -c traj_mag.f
	vbl_init.o: scairy_com.mod vbl_init.f
	gfortran -c vbl_init.f
	xi_cut_set.o: Parms_com.mod gen_com.mod i_o_com.mod xi_cut_set.f
	gfortran -c xi_cut_set.f
	xkh_init.o: Parms_com.mod gen_com.mod i_o_com.mod xkh_init.f
	gfortran -c xkh_init.f
	zmx_init.o: Parms_com.mod gen_com.mod i_o_com.mod zmx_init.f
	gfortran -c zmx_init.f

        # clean files
	clean:
	rm  airy_hsp.o alloc_vars.o blug.o contour_find.o cub_fit_new.o cub_root_new.o duct_calcs.o eig_disp.o eig_findm.0 em_calc.o ep_calc.o fix_path.o freq_init.o h_space.o mfun_fill.o mode_branch.o mode_find.o mode_fun.o opt_read.o orca_mex.o phz_calc.o r1r2_calc.o rp_calc.o rp_flay.o rp_nomm.o rp_nomm0.o rp_sfint.o rp_slay.o rp_slay0.o scairy2.o sr_geom.o svp_check.o svp_read.o tools.o traj_mag.o vbl_init.o xi_cut_set.o xkh_init.o zmx_init.o
	rm Parms_com.o gen_com.o i_o_com.o scairy_com.o
	rm Parms_com.mod gen_com.mod i_o_com.mod scairy_com.mod
