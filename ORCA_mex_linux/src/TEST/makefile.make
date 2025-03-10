# makefile for orca debugging

MEXLIB = /usr/local/MATLAB/R2017b/extern/include

FLAGS =	-Og -fno-underscoring -fPIC -std=legacy -cpp -ffree-line-length-none
OBJS = Parms_com.o gen_com.o i_o_com.o \
 svp_read.o opt_read.o tools.o svp_check.o airy_hsp.o blug.o

sub_orca2.mexa64: sub_orca2.c liborca.a
	mex FFLAGS='FFLAGS -cpp' -g sub_orca2.c liborca.a -lgfortran

liborca.a:	${OBJS} orca_mex2.o
	ar urv liborca.a orca_mex2.o ${OBJS}

# subroutines
orca_mex2.o: Parms_com.mod gen_com.mod i_o_com.mod orca_mex2.f90
	gfortran ${FLAGS} -c orca_mex2.f90
svp_read.o: Parms_com.mod i_o_com.mod svp_read.f
	gfortran ${FLAGS} -c svp_read.f
opt_read.o: Parms_com.mod i_o_com.mod opt_read.f
	gfortran ${FLAGS} -c opt_read.f
tools.o: Parms_com.mod i_o_com.mod tools.f
	gfortran ${FLAGS} -c tools.f
svp_check.o: Parms_com.mod gen_com.mod i_o_com.mod svp_check.f
	gfortran -fno-underscoring -fPIC -std=legacy -ffree-line-length-none -cpp -I${MEXLIB} -include fintrf.h -c svp_check.f
airy_hsp.o: airy_hsp.f
	gfortran ${FLAGS} -c airy_hsp.f
blug.o: Parms_com.mod blug.f
	gfortran ${FLAGS} -c blug.f

# modules
Parms_com.mod: Parms_com.o Parms_com.f90
	gfortran ${FLAGS} -c Parms_com.f90
Parms_com.o: Parms_com.f90
	gfortran ${FLAGS} -c Parms_com.f90
gen_com.mod: gen_com.o gen_com.f90
	gfortran ${FLAGS} -c gen_com.f90
gen_com.o: gen_com.f90
	gfortran ${FLAGS} -c gen_com.f90
i_o_com.mod: i_o_com.o i_o_com.f90
	gfortran ${FLAGS} -c i_o_com.f90
i_o_com.o: i_o_com.f90
	gfortran ${FLAGS} -c i_o_com.f90
