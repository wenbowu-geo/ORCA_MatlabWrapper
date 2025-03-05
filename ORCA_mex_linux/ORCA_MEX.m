function [varargout] = ORCA_MEX(svp_in,opt_in,iimf)

%     function [nmodes, eig, freq, mf, mfz, vg] = ORCA_MEX(svp_in, opt_in, iiMF)
%  --------------------------------------------------------------------------------
% 		This ORCA MEX subroutine allows multiple frequency calculation.
% 
% 		Input structure and variables:
% 		    svp_in:  input env profiles (a Matlab structure with double fields)
% 		    opt_in:  input ORCA options (a Matlab structure with double fields)
% 		    iiMF:  a flag indicating calculating mode functions of p-waves (0 or 1)
% 
% 		Output variables:
%   		nmodes: number of valid modes in the output variables  [1 x Nfreq]
%   		eig:  mode eigenvalues (complex)  [Nmode x Nfreq]
%           freq:  frequency (Hz)   [1 x Nfreq]
% 	    	mf:  mode function (complex)  [Nz x Nmode x Nfreq], returns empty array if iiMF = 0 
% 		    mfz:  depth points of the mode function  [Nz x 1], returns empty array if iiMF = 0   
% 		    vg:  modal group speed [Nmode x Nfreq]
% 
% ---------------------------------------------------------------------------------
%      svp_in and opt_in
%     
%  This ORCA mex provides only ONE function of the ORCA program, cwmode, solving 
%     for complex modes of p-waves. 
%     The input strcutres svp_in and opt_in are described below, which 
%     are in fact part of the ORCA input variables (other variables are hardwired).
%     Please refer to the original ORCA readme file for a complete explanation. 
%
%       svp_in = 
%           uphalf_cp: 
%           uphalf_cs: 
%          uphalf_rho: upper halfspace env
%           uphalf_ap: 
%           uphalf_as: 
%                nsvp: number of SVP points in ocean
%                ctol: tolerance used in 1/c^2 fitting SVP to eliminate layers (0=keep all layers)
%                wssp: Ocean SVP Profile [depth x sound speed]
%                wrho: water density
%              walphs: water ap
%               nlayb: # bottom layers 
%             btm_env: [each layer is a 1x16 double matrix, and has the following entries
%                      type h cp1 g cs1 cs2 rho1 rho2 ap1 ap2 as1 as2 beta ctol fexpp fexps
%                      SPECIAL NOTE:  When nlayb>0, btm_env should have exact nlayb rows,
%                                       or ORCA program will go crazy!!  
%          lowhalf_cp: 
%          lowhalf_cs: 
%         lowhalf_rho: lower halfspace
%          lowhalf_ap: 
%          lowhalf_as: 
%                ntop: number of layers above the sea surface, can be 0.
%           above_sea: same format as btm_env
%
%         opt_in = 
%              nmode: number of modes to output from ORCA (not an original ORCA variable)  
%             cphmax: Max phase speed of p wave mode to calculate. Combining with rmin controls how 
%                               many modes are calculated in ORCA. SEE ORCA user guide for details.   
%               rmin: Min range (km) of interest (must be positive) 
%                               Combining with cphmax controls how many modes are calculated in ORCA.
%                               When rmin > 999.0 only use cphmax to control how many modes are calculated.
%               rmax: Max range (km) of interest, controls the accuracy of modal wavenumbers 
%              phfac: Phase Step Parm: Step by 2*pi/phfac (set to 4-8, 0=default==>4)
%              dbcut: Modes Weaker By db_cut Ignored (set to 30-60, 0=default==>50)
%              Aih_l: Gradient lower h-space: 0=default,-1=homogeneous, >0=da_bar
%              Aih_u: Gradient lower h-space: 0=default,-1=homogeneous, >0=da_bar
%                 nf: Frequencies (nf>0 ==> List fcw's; nf<0 ==> List first,last f)
%                fcw: [f1 f2 ... ]
%              fcw_n: size of the fcw array 
%                nzm: Depth points in modes (nzm>0 ==> List zm's; nf<0 ==> List first,last zm)
%                 zm: [z1 z2 ... ]
%               zm_n: size of the zm array 
%
%           Special Note:  nf, nz can not be simultaneously big, otherwise the Matlab will
%                          go out-of-memory. nmode should be smaller than the preset maximal
%                          value, nmodeMax = 2000.  Also, nfMax = 5001, and zmMAX = 10000.  
%
%  --------------------------------------------------------------------------------
%     Experienced user can directly call the MEX file to save some CPU time. 
%
%     Syntax:
%
%     [nmode, eig_re, eig_im, vg, freq, (mf_re, mf_im, mfz)] = sub_orca(svp_in, opt_in, iiMF)
%
% 		Input structure and variables:
% 		    svp_in:  input env profiles, see above (a Matlab structure)
% 		    opt_in:  input ORCA options, see above (a Matlab structure)
% 		    iiMF:  a flag indicating calculating mode functions of p-waves (0 or 1)
% 
% 		Output variables:
% 		    nmodes: number of valid modes in the output variables [1 x Nfreq]
% 		    eig_re, eig_im: mode eigenvalues (real and imaginary parts) [Nmode x Nfreq]
% 		    vg:  modal group speed [Nmode x Nfreq]
%           freq:  frequencies in Hz [1 x Nfreq]
%
%           NOTE:!! DO NOT key in the next three output arguments unless iiMF = 1 !!
% 		    mf_re, mf_im:  mode function (real and imaginary parts) [Nz x Nmode x Nfreq] 
% 		    mfz: depth points of the mode function [Nz x 1]   
%

% Y.-T. Lin @ WHOI


if nargin == 2, iimf = 0; end

if iimf == 0,
    [nMODES, kn_re, kn_im, vg, freq] = sub_orca(svp_in, opt_in, iimf);
    varargout(1) = {nMODES};
    varargout(2) = {kn_re+1i*kn_im};
    if nargout >= 3, varargout(3) = {freq}; end
    if nargout >= 4, varargout(4) = {[]}; end
    if nargout >= 5, varargout(5) = {[]}; end
    if nargout == 6, varargout(6) = {vg}; end
else
    [nMODES, kn_re, kn_im, vg, freq, phi_re, phi_im, phi_z] = sub_orca(svp_in, opt_in, iimf);
    varargout(1) = {nMODES};
    varargout(2) = {kn_re+1i*kn_im};
    if nargout >= 3, varargout(3) = {freq}; end
    if nargout >= 4, varargout(4) = {phi_re+1i*phi_im}; end
    if nargout >= 5, varargout(5) = {phi_z}; end
    if nargout == 6, varargout(6) = {vg}; end
end

return

