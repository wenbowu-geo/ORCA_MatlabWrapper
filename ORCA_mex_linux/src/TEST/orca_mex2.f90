subroutine orca_mex2(mex_in_uphalf_cp,mex_in_uphalf_cs, mex_in_uphalf_rho,mex_in_uphalf_ap, &
     mex_in_uphalf_as,mex_in_nsvp, mex_in_ctol, &
     mex_in_wssp, mex_in_wrho,mex_in_walphs, &
     mex_in_nlayb, &
     mex_in_btm_env, &
     mex_in_lowhalf_cp,mex_in_lowhalf_cs,mex_in_lowhalf_rho,mex_in_lowhalf_ap,mex_in_lowhalf_as, &
     mex_in_ntop, &
     mex_in_above_sea, &
     mex_in_iimf, mex_in_cphmax,&
     mex_in_rmin, mex_in_rmax, mex_in_phfac, mex_in_dbcut, mex_in_Aih_l, mex_in_Aih_u, &
     mex_in_nf, mex_in_fcw_n, mex_in_fcw, &
     mex_in_nzm, mex_in_zm_n, mex_in_zm, &
     mex_out_NM, mex_out_NF, mex_out_NZ, &
     mex_out_eig_re, mex_out_eig_im, mex_out_vg, &
     mex_out_phi_re, mex_out_phi_im, mex_out_phi_z, &
     mex_out_nmode, mex_out_frq, mex_out_isrun)
! 
!yt
!yt  ORCA subroutine for making a matlab mex file -- Y.-T. Lin @ WHOI 
!yt  
!yt: Normal mode model for acoustic propagation in range independent ocean
!yt: environments with fluid and/or solid layered structures above and below.  
!yt: Computes leaky modes and seismic modes.   Users must construct an SVP
!yt: file (see the template file orca_svp) that specifies the ocean
!yt: environment and an OPTION file (see the template file orca_opt) that 
!yt: specifies the run parameters and outputs desired.
!yt:
!yt: Questions or comments should be referred to:
!yt:
!yt: Evan Westwood
!yt: Applied Research Laboratories
!yt: The University of Texas at Austin
!yt: P. O. Box 8029
!yt: Austin, TX  78713-8029
!yt
!yt:   References for the complex k-plane model at this time (05-21-96) are:
!yt:	E. K. Westwood, !yt. T. Tindle, and N. R. Chapman, "A normal mode
!yt:         model for acousto-elastic ocean environments," J. Acoust. Soc. Am.,
!yt:         100, 3631-3645 (1996).
!yt:	E. K. Westwood, !yt. T. Tindle, and N. R. Chapman, "A normal mode 
!yt:	   model for multilayered acoustoelastic ocean environments based on 
!yt:	   an analytic reflection coefficient method," J. Acoust. Soc. Am., 
!yt:	   95, No. 5, Pt. 2, 2908 (1994).
!yt:	E. K. Westwood, "An efficient broadband normal-mode model for 
!yt:	   acoustoelastic ocean environments," J. Acoust. Soc. Am., 96, 
!yt:	   No. 5, Pt. 2, 3352 (1994).
!yt:   References for the real k-axis option are:
!yt:	E. K. Westwood, "Improvements to narrow-band and broadband normal-mode
!yt:	   algorithms for fluid ocean environments," J. Acoust. Soc. Am.,
!yt:	   99, No. 4, Pt. 2, 2524 (1996).
!yt:	S. J. Levinson, E. K. Westwood, R. A. Koch, S. K. Mitchell, and 
!yt:	   !yt. V. Sheppard, "An efficient and robust method for underwater 
!yt:	   acoustic normal-mode computations," J. Acoust. Soc. Am., 97, 
!yt:	   1576-1585 (1995).
!yt
      use Parms_com
      use i_o_com
      use gen_com
!
!yt  SVP file Line 2
      real*8 :: mex_in_uphalf_cp,mex_in_uphalf_cs,mex_in_uphalf_rho,mex_in_uphalf_ap,mex_in_uphalf_as
!yt SVP file Line 3
      integer*4 :: mex_in_nsvp
      real*8 :: mex_in_ctol
!yt SVP file Line 4
      real*8 :: mex_in_wrho,mex_in_walphs,mex_in_wssp(mex_in_nsvp,2)
!yt SVP file Line 5
      integer*4 :: mex_in_nlayb
!yt SVP file Line 6
      real*8 :: mex_in_btm_env(mex_in_nlayb,16)
!yt SVP file Line 7 
      real*8 :: mex_in_lowhalf_cp,mex_in_lowhalf_cs,mex_in_lowhalf_rho, mex_in_lowhalf_ap,mex_in_lowhalf_as
!yt SVP file Line 8
      integer*4 :: mex_in_ntop
!yt SVP file Line 9
      real*8 :: mex_in_above_sea(mex_in_ntop,16)
!yt OPT file Line 2
      integer*4 :: mex_in_iimf
      real*8 :: mex_in_cphmax, mex_in_rmin, mex_in_rmax, mex_in_phfac, mex_in_dbcut, mex_in_Aih_l, mex_in_Aih_u
!yt OPT file Line 3     
      integer*4 :: mex_in_nf, mex_in_fcw_n
      real*8 :: mex_in_fcw(mex_in_fcw_n,1)
!yt OPT file Line 5
      integer*4 :: mex_in_nzm, mex_in_zm_n
      real*8 :: mex_in_zm(mex_in_zm_n,1)
!yt output
      integer*4, intent(in) :: mex_out_NM, mex_out_NF, mex_out_NZ
      real*8 :: mex_out_eig_re(mex_out_NM, mex_out_NF), mex_out_eig_im(mex_out_NM, mex_out_NF), mex_out_vg(mex_out_NM, mex_out_NF), mex_out_frq(mex_out_NF)
      real*4 :: mex_out_phi_re(mex_out_NZ, mex_out_NM, mex_out_NF), mex_out_phi_im(mex_out_NZ, mex_out_NM, mex_out_NF), mex_out_phi_z(mex_out_NZ)
      integer*4 :: mex_out_nmode(mex_out_NF)
      
!yt            
      integer*4 :: mex_out_isrun
      mex_out_isrun = 0
!
!yt      write(*,*) 'entered orca'
! BD      call vbl_init
      ver_cur=2.0
      
!
!yt: Read input svp file in file name svp_file(1:lsvp):
!yt read input svp from mex input
!yt allocate mex input variable
  call svp_read(mex_in_uphalf_cp,mex_in_uphalf_cs, mex_in_uphalf_rho,mex_in_uphalf_ap, &
     mex_in_uphalf_as,mex_in_nsvp, mex_in_ctol, &
     mex_in_wssp, mex_in_wrho,mex_in_walphs, &
     mex_in_nlayb, &
     mex_in_btm_env, &
     mex_in_lowhalf_cp,mex_in_lowhalf_cs,mex_in_lowhalf_rho,mex_in_lowhalf_ap,mex_in_lowhalf_as, &
     mex_in_ntop, &
     mex_in_above_sea)
!
!yt: Read input option file in file name opt_file(1:lopt):
  call opt_read(mex_in_iimf, &
     mex_in_cphmax, mex_in_rmin, mex_in_rmax, mex_in_phfac, mex_in_dbcut, mex_in_Aih_l, mex_in_Aih_u, &
     mex_in_nf, mex_in_fcw_n, mex_in_fcw, &
     mex_in_nzm, mex_in_zm_n, mex_in_zm)

!yt: Check input parameters and set up master files:
     call svp_check(0)
     call svp_check2
!yt
! BD      call alloc_vars

!yt      write(*,*) 'calling cw_modes'
!       call cw_modes(mex_out_NM, mex_out_NF, mex_out_NZ, &
!     mex_out_eig_re, mex_out_eig_im, mex_out_vg, &
!     mex_out_phi_re, mex_out_phi_im, mex_out_phi_z, &
!     mex_out_nmode, mex_out_frq)
!yt  instead of calling cw_modes, directly run mode_find and grab the answer
      iiwrt = 0
      iibad=0
! BD      call uni_space(nfcw,fcw,1.e0)
! BD      call sr_geom(1)
! BD      call zmx_init
! BD      do jfcw=1,nfcw
! BD         lsuf0=lsuf
! BD         nctot=0
! BD         nclast=0
! BD         f_hz=fcw(jfcw)
! BD         mex_out_frq(jfcw) = f_hz
! BD            call mode_find(iiwrt)
! BD            call eig_disp(xmode,nmode,1,kn(1),iikn,kw0,r4mat1,r4mat2)
! BD            mex_out_nmode(jfcw) = nmode
! BD            mex_out_eig_re(1:min(nmode,mex_out_NM),jfcw) = r4mat1(1:min(nmode,mex_out_NM))
! BD            mex_out_eig_im(1:min(nmode,mex_out_NM),jfcw) = r4mat2(1:min(nmode,mex_out_NM))

! BD         if (iimf .eq. 1 .or. iimf .eq. 3) then
! BD            if(nzmf*nmode .gt. NHDFMAX) then
! BD               NHDFMAX=nzmf*nmode
! BD               deallocate(r4mat1)
! BD               deallocate(r4mat2)
! BD               allocate(r4mat1(NHDFMAX))
! BD               allocate(r4mat2(NHDFMAX))
! BD            endif
! BD            call mfun_fill(phi,r4mat1,r4mat2,hdf_suf,lsuf)
! BD            do j = 1,nzmf
! BD              mex_out_phi_z(j) = zmf(j)
! BD              do jm = 1,min(nmode,mex_out_NM)
! BD                mex_out_phi_re(j,jm,jfcw)=real(r4mat1((j-1)*nmode+jm),4)
! BD                mex_out_phi_im(j,jm,jfcw)=real(r4mat2((j-1)*nmode+jm),4)
! BD              enddo
! BD            enddo

! BD         endif
! BD            do jm=1,min(nmode,mex_out_NM)
! BD               vph=w/dreal(kn(jm))
! BD               vg=dreal(eig_char(4,jm))
! BD               mex_out_vg(jm,jfcw) = vg
! BD            enddo
! BD      enddo
!yt       write(*,*) 'retrieving modes'
! BD      if(iifail .eq. 0) then
! BD        mex_out_isrun = 1
! BD      endif
     
!yt deallocate all variables      
! BD      deallocate(r4mat1)
! BD      deallocate(r4mat2)
! BD      deallocate(range)
! BD      deallocate(sq2pir)
! BD      deallocate(mzsrc)
! BD      deallocate(mzrec)
! BD      deallocate(mzmf)
! BD      deallocate(nrec_jr)
! BD      deallocate(krec_jr)
! BD      deallocate(zsr_indx)
! BD      deallocate(jrec_jr)
! BD      deallocate(xrec)
! BD      deallocate(yrec)
! BD      deallocate(rng_sr)
!yt The rest of allocatable variables need to deallocate if used.      
!yt knbb
!yt eig_bb
!yt vg_bb
!yt Dvg_w
!yt phi_bb
!yt tf
!yt iish_bb
!yt faxbb
!yt nmbb
!yt wbb
!yt kim_bb
!yt phibb
!yt dpsibb
      
!yt
      return
end  subroutine orca_mex2
