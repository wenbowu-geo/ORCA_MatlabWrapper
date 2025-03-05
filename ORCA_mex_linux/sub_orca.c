#include "mex.h"
#include "matrix.h"

/*  sub_orca.c - Mex function gateway function for orca.f 

      function [nmode, eig_re, eig_im, vg, freq, mf_re, mf_im, mfz] = sub_ORCA(svp_in, opt_in, iiMF)

		allows multiple frequency calculation

		Input structure and variables:
		svp_in:  input env profiles (a Matlab structure)
		opt_in:  input ORCA options (a Matlab structure)
		iiMF:  a flag indicating calculating mode functions (a Matlab variable)

		Output variables:
		nmode: number of valid modes in the output variables [1 x Nfreq]
		eig_re, eig_im:  mode eigenvalues (real and imaginary parts) [Nmode x Nfreq]
		vg:  modal group speed, its dimension [Nmode x Nfreq]
		freq: frequencies in Hz [1 x Nfreq]
		mf_re, mf_im:  mode function (real and imaginary parts), if iiMF set to 1.  Dimension [Nz x Nmode x Nfreq]   
		mfz:  depth points of the mode function,  Dimension [Nz x 1]   
*/

void orca_mex (double * uphalf_cp, double * uphalf_cs, double * uphalf_rho, double * uphalf_ap, double * uphalf_as, \
/* void ORCA_MEX (double * uphalf_cp, double * uphalf_cs, double * uphalf_rho, double * uphalf_ap, double * uphalf_as, \ */
     int * nsvp, double * ctol, \
     double * wssp, double * wrho, double * walphs, \
     int * nlayb, \
     double * btm_env, \
     double * lowhalf_cp, double * lowhalf_cs, double * lowhalf_rho, double * lowhalf_ap, double * lowhalf_as, int * ntop, \
     double * above_sea, \
     int * iimf, \
     double * cphmax, double * rmin, double * rmax, double * phfac, double * dbcut, double * Aih_l, double * Aih_u, \
     int * nf, int * fcw_n, double * fcw, \
     int * nzm, int * zm_n, double * zm, \
     int * mex_out_NM, int * mex_out_NF, int * mex_out_NZ, \
     double * mex_out_eig_re, double * mex_out_eig_im, double * mex_out_vg, \
	 float * mex_out_phi_re, float * mex_out_phi_im, float * mex_out_phi_z, \
	 int * mex_out_nmode, double * mex_out_frq, int * mex_out_isrun );

void mexFunction( int nlhs, mxArray *plhs[], 
				 int nrhs, const mxArray *prhs[])  
{
      double uphalf_cp,uphalf_cs,uphalf_rho,uphalf_ap,uphalf_as;
      int nsvp;
      double ctol;
      double *wssp,wrho,walphs;
      int nlayb;
      double *btm_env;
      double lowhalf_cp,lowhalf_cs,lowhalf_rho,lowhalf_ap,lowhalf_as;
      int ntop;
      double *above_sea;
      int iimf;
      double cphmax, rmin, rmax, phfac, dbcut, Aih_l, Aih_u;
      int nf, fcw_n;
      double *fcw;
      int nzm, zm_n;
      double *zm;

      int mex_out_NM, mex_out_NF, mex_out_NZ; 
      double *mex_out_eig_re, *mex_out_eig_im, *mex_out_vg, *mex_out_frq;
      float *mex_out_phi_re, *mex_out_phi_im, *mex_out_phi_z;
	  int *mex_out_nmode; 
	  int mex_out_isrun;

	  mwSize dims_2D[3]={0,0};
	  mwSize dims_3D[3]={0,0,0};

/* The parameters must be consistent between this program and the */
/* ORCA program to avoid scrambling the arrays or crashing.     */

      if (nrhs != 3) {
          mexErrMsgTxt("ORCA_MEX requires 3 input arguments");
      } 

      uphalf_cp =  mxGetScalar(mxGetField(prhs[0], 0, "uphalf_cp"));
      uphalf_cs =  mxGetScalar(mxGetField(prhs[0], 0, "uphalf_cs"));
      uphalf_rho =  mxGetScalar(mxGetField(prhs[0], 0, "uphalf_rho"));
      uphalf_ap =  mxGetScalar(mxGetField(prhs[0], 0, "uphalf_ap"));
      uphalf_as =  mxGetScalar(mxGetField(prhs[0], 0, "uphalf_as"));
      nsvp =  (int)mxGetScalar(mxGetField(prhs[0], 0, "nsvp"));
      ctol =  mxGetScalar(mxGetField(prhs[0], 0, "ctol"));
      wssp =  mxGetPr(mxGetField(prhs[0], 0, "wssp"));
      wrho =  mxGetScalar(mxGetField(prhs[0], 0, "wrho"));
      walphs =  mxGetScalar(mxGetField(prhs[0], 0, "walphs"));
      nlayb =  (int)mxGetScalar(mxGetField(prhs[0], 0, "nlayb"));
      btm_env =  mxGetPr(mxGetField(prhs[0], 0, "btm_env"));
      lowhalf_cp =  mxGetScalar(mxGetField(prhs[0], 0, "lowhalf_cp"));
      lowhalf_cs =  mxGetScalar(mxGetField(prhs[0], 0, "lowhalf_cs"));
      lowhalf_rho =  mxGetScalar(mxGetField(prhs[0], 0, "lowhalf_rho"));
      lowhalf_ap =  mxGetScalar(mxGetField(prhs[0], 0, "lowhalf_ap"));
      lowhalf_as =  mxGetScalar(mxGetField(prhs[0], 0, "lowhalf_as"));
      ntop =  (int)mxGetScalar(mxGetField(prhs[0], 0, "ntop"));
      above_sea =  mxGetPr(mxGetField(prhs[0], 0, "above_sea"));

      cphmax =  mxGetScalar(mxGetField(prhs[1], 0, "cphmax"));
      rmin =   mxGetScalar(mxGetField(prhs[1], 0, "rmin"));
      rmax =  mxGetScalar(mxGetField(prhs[1], 0, "rmax"));
      phfac =  mxGetScalar(mxGetField(prhs[1], 0, "phfac"));
      dbcut =  mxGetScalar(mxGetField(prhs[1], 0, "dbcut"));
      Aih_l =  mxGetScalar(mxGetField(prhs[1], 0, "Aih_l"));
      Aih_u =  mxGetScalar(mxGetField(prhs[1], 0, "Aih_u"));
	  nf =  (int)mxGetScalar(mxGetField(prhs[1], 0, "nf"));
      fcw_n =  (int)mxGetScalar(mxGetField(prhs[1], 0, "fcw_n"));
      fcw =  mxGetPr(mxGetField(prhs[1], 0, "fcw"));
      nzm =  (int)mxGetScalar(mxGetField(prhs[1], 0, "nzm"));
      zm_n =  (int)mxGetScalar(mxGetField(prhs[1], 0, "zm_n"));
      zm =  mxGetPr(mxGetField(prhs[1], 0, "zm"));

      iimf =  (int)mxGetScalar(prhs[2]); 

	  if (iimf == 0) {
		  nzm = 1;
	  }

	  mex_out_NZ = abs(nzm);
	  mex_out_NM = (int)mxGetScalar(mxGetField(prhs[1], 0, "nmode"));
	  mex_out_NF = abs(nf);

	  if (mex_out_isrun = 0) {
			mexErrMsgTxt("The ORCA mode finder does not run sucessfully!!");
	  } 
	  else {
	       if (nlhs < 5) {
             mexErrMsgTxt("ORCA_MEX requires at least 5 output argument");
		   }	    
		    dims_2D[0] = 1;
			dims_2D[1] = mex_out_NF;
		   
			plhs[0] = mxCreateNumericArray(2,dims_2D,mxINT32_CLASS, mxREAL);
			plhs[1] = mxCreateDoubleMatrix(mex_out_NM, mex_out_NF, mxREAL);
			plhs[2] = mxCreateDoubleMatrix(mex_out_NM, mex_out_NF, mxREAL);
			plhs[3] = mxCreateDoubleMatrix(mex_out_NM, mex_out_NF, mxREAL);
			plhs[4] = mxCreateDoubleMatrix(1,mex_out_NF,mxREAL); 
			mex_out_nmode = mxGetData(plhs[0]);
			mex_out_eig_re = mxGetPr(plhs[1]);
			mex_out_eig_im = mxGetPr(plhs[2]);
			mex_out_vg = mxGetPr(plhs[3]);
			mex_out_frq = mxGetPr(plhs[4]);
			
			if (iimf == 1) {
    	       if (nlhs < 8) {
                   mexErrMsgTxt("ORCA_MEX needs 8 output arguments for mode function");
	     	       }	    
			dims_3D[0] = mex_out_NZ;
			dims_3D[1] = mex_out_NM;
			dims_3D[2] = mex_out_NF;
			plhs[5] = mxCreateNumericArray(3,dims_3D,mxSINGLE_CLASS, mxREAL);
			plhs[6] = mxCreateNumericArray(3,dims_3D,mxSINGLE_CLASS, mxREAL);
		    dims_2D[0] = mex_out_NZ;
			dims_2D[1] = 1;
	   		plhs[7] = mxCreateNumericArray(2,dims_2D,mxSINGLE_CLASS, mxREAL);
			mex_out_phi_re = mxGetData(plhs[5]);
			mex_out_phi_im = mxGetData(plhs[6]);
			mex_out_phi_z = mxGetData(plhs[7]); 
			}
	  }

/* Need to send pointers to ORCA. */
/* The arrays are pointers already (* in front), but the scalars need the & */
orca_mex (&uphalf_cp, &uphalf_cs, &uphalf_rho, &uphalf_ap, &uphalf_as, \
/* ORCA_MEX (&uphalf_cp, &uphalf_cs, &uphalf_rho, &uphalf_ap, &uphalf_as, \ */
     &nsvp, &ctol, \
     wssp, &wrho, &walphs, \
     &nlayb, \
     btm_env, \
     &lowhalf_cp, &lowhalf_cs, &lowhalf_rho, &lowhalf_ap, &lowhalf_as, &ntop, \
     above_sea, \
     &iimf, \
     &cphmax, &rmin, &rmax, &phfac, &dbcut, &Aih_l, &Aih_u, \
     &nf, &fcw_n, fcw, \
     &nzm, &zm_n, zm, \
     &mex_out_NM, &mex_out_NF, &mex_out_NZ, \
     mex_out_eig_re, mex_out_eig_im, mex_out_vg, \
	 mex_out_phi_re, mex_out_phi_im, mex_out_phi_z, \
	 mex_out_nmode, mex_out_frq, &mex_out_isrun );
}
