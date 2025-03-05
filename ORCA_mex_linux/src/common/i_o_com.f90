module i_o_com
use Parms_com
implicit none
save

!: VARIABLES FROM SVP FILE (read by svp_read):
!     common /svp_com0/ nsvp,nlayb,nlayt,ktb(NLMAX),ktt(NLMAX),svp_ver
!     integer*4 nsvp,nlayb,nlayt,ktb,ktt
!     real*4 svp_ver
integer*4 :: nsvp,nlayb,nlayt,ktb(NLMAX),ktt(NLMAX)
!yt real*4 :: svp_ver
!     common /svp_com/ 
!    .   svp_title,ctol,zsvp(5*NLMAX),csvp(5*NLMAX),rho_svp,alpha_svp,
!    .   hb(NLMAX),geob(2,5,NLMAX),bpb(4,NLMAX),
!    .   ht(NLMAX),geot(2,5,NLMAX),bpt(4,NLMAX),htop,Htot,cs_cw_rat,
!    .   tau_2way,gbar
!     real*8 ctol,zsvp,csvp,rho_svp,alpha_svp,hb,geob,bpb,ht,geot,bpt,
!    .   htop,Htot,cs_cw_rat,tau_2way,gbar
!     character*64 svp_title
real*8 :: ctol,zsvp(5*NLMAX),csvp(5*NLMAX),rho_svp,alpha_svp,&
	hb(NLMAX),geob(2,5,NLMAX),bpb(4,NLMAX),ht(NLMAX),&
	geot(2,5,NLMAX),bpt(4,NLMAX),htop,Htot,cs_cw_rat,&
	tau_2way,gbar
! yt character*64 :: svp_title
!: VARIABLES FROM OPTION FILE (read by opt_read):
!     common /opt_com/ 
!: Line 1: (set iiwrite=1 to output files, 0 to suppress output)
!    .   ver_cur,ver_no,iicw,iikpl,iirc,iiparm,n_env,iifmt,iiwrite,
!: Line 2:
!    .   iirx,cphmin,cphmax,rmin,rmax,phfac,db_cut,iiAih(2),kkblm,
!    .   iigbs,iidiag,
!: Line 3: 
!    .   nfcw,fcw(NFCWMAX),
!: Line 4:
!    .   iitl,iimf,iisig,iimt,iidc,iikn,iilist,iikrak,iioas,iifepe,
!    .   iimlab,iitsp,
!: Lines 5 or 8:
!    .   nzs,zsrc(NSRMAX),nrec,zrec(NSRMAX),nsrc,rkm(NSRMAX),
!    .   nth_gbs,nb_gbs,th_gbs(NSRMAX),b_gbs(NSRMAX),
!: Line 6:
!    .   iiri,iimp,nzmf,zmf(NSRMAX),
!: Line 7 (deleted):
!    .   nr_tsp,r1_tsp,r2_tsp,nt_tsp,Tw_tsp,iimex,mr_tsp,pct_tsp,
!    .   nrm_tsp,
!: Line 7:
!    .   fsbb,Tw,fmin,fmax,iifft,iiout,iift,
!: Line 9:
!    .   nrun,nparm,rseed,kvar(4,NPMAX),xvar(2,NPMAX),
!: Line 10 and 11:
!    .   nseg,geom_file,lgeom,iitype(NSEGMAX),iicont(NSEGMAX),
!    .   vs(NSEGMAX),t1(NSEGMAX),t2(NSEGMAX),dt(NSEGMAX),cpa(NSEGMAX),
!    .   phid(NSEGMAX),x2(NSEGMAX),y2(NSEGMAX),
!: Line 12:
!    .   fkpl,iivar,iiform,xkhr1,xkhr2,nreal,xkhi1,xkhi2,nimag,kduc,
!    .      iiwr,iishp,iishs,
!: Line 13 and 14:
!    .   freq1,freq2,nfreq,iilog,th1,th2,nang,fsrc,nfft
!     integer*4 iicw,iikpl,iirc,iiparm,n_env,iifmt,iiwrite,kkblm,iigbs,
!    .   iidiag,iirx,iitl,iimf,iisig,iimt,nfcw,nzs,nrec,nsrc,
!    .   nth_gbs,nb_gbs,iiri,iimp,nzmf,nr_tsp,nt_tsp,iimex,
!    .   mr_tsp,nrm_tsp,iifft,iiout,iift,
!    .   iidc,iikn,iilist,iikrak,iioas,iifepe,iimlab,iitsp,
!    .   nrun,nparm,rseed,kvar,nseg,lgeom,iitype,iicont,iivar,
!    .   iiform,nreal,nimag,kduc,iiwr,iishp,iishs,nfreq,iilog,nang,nfft
!     real*4 ver_cur,ver_no,cphmin,cphmax,rmin,rmax,phfac,db_cut,iiAih,
!    .   fcw,zsrc,zrec,rkm,th_gbs,b_gbs,zmf,r1_tsp,r2_tsp,Tw_tsp,
!    .   pct_tsp,fsbb,Tw,fmin,fmax,xvar,vs,t1,t2,dt,cpa,phid,x2,y2,fkpl,
!    .   xkhr1,xkhr2,xkhi1,xkhi2,freq1,freq2,th1,th2,fsrc
!     character*64 geom_file
integer*4 :: iicw,iikpl,iirc,iiparm,n_env,iifmt,iiwrite,kkblm,iigbs,&
	iidiag,iirx,iitl,iimf,iisig,iimt,nfcw,nzs,nrec,nsrc,&
	nth_gbs,nb_gbs,iiri,iimp,nzmf,nr_tsp,nt_tsp,iimex,&
	mr_tsp,nrm_tsp,iifft,iiout,iift,iidc,iikn,&
	iilist,iikrak,iioas,iifepe,iimlab,iitsp,nrun,nparm,rseed,&
	kvar(4,NPMAX),nseg,lgeom,iitype(NSEGMAX),iicont(NSEGMAX),iivar,&
	iiform,nreal,nimag,kduc,iiwr,iishp,iishs,nfreq,iilog,nang,nfft
real*4 :: ver_cur,ver_no,cphmin,cphmax,rmin,rmax,phfac,db_cut,iiAih(2),&
	fcw(NFCWMAX),zsrc(NSRMAX),zrec(NSRMAX),rkm(NSRMAX),&
	th_gbs(NSRMAX),b_gbs(NSRMAX),zmf(NSRMAX),r1_tsp,r2_tsp,Tw_tsp,&
	pct_tsp,fsbb,Tw,fmin,fmax,xvar(4,NPMAX),vs(NSEGMAX),t1(NSEGMAX),&
	t2(NSEGMAX),dt(NSEGMAX),cpa(NSEGMAX),phid(NSEGMAX),x2(NSEGMAX),&
	y2(NSEGMAX),fkpl,xkhr1,xkhr2,xkhi1,xkhi2,freq1,freq2,&
	th1,th2,fsrc
character(len=64) :: geom_file
!
!: OUTPUT VARIABLES:
!     common /out_com/ svp_file,opt_file,outroot,outfile,
!: Geoacoustic profile (geo index 1:1=top, 2=bottom;
!: index 2: 1=cp, 2=cs, 3=rho, 4=alphap, 5=alphas; index 3=layer #):
!    .   geo(2,5,NLMAX),h(NLMAX),zdep(NLMAX),fexp(2,NLMAX),
!: Coherent and incoherent TL [refer to as tlc(1:nzs,1:nsrc,1:nrec)]:
!    .   tlc(NTLMAX),tli(NTLMAX),
!: Mode eigenvalues [refer to kn(1:nmode); kn(0) is dummy]:
!    .   kn(0:NM_MAX),
!: Mode functions [refer to phi,dphi,psi,dpsi as phi(1:nzsr,1:nmode)]:
!    .   phi(NSR_NM_MAX),dphi(NSR_NM_MAX),psi(NSR_NM_MAX),
!    .   dpsi(NSR_NM_MAX),exp_gbs(NSR_NM_MAX),
!: Mode characteristics (row 1=L=ln(R1*R2), 2=dL/dk, 3=dL/dw, 4=vG, 5=R1):
!    .   eig_char(5,NM_MAX),
!: Miscellaneous arrays:
!    .   zsr(NSRMAX),zsr_im_gbs(NSRMAX),rho_sr(NSRMAX),cp_sr(NSRMAX),
!    .   cs_sr(NSRMAX),range(NSNRMAX),
!    .   sq2pir(NSNRMAX),ksm2_sr(NSRMAX),
!: Broadband eigenvalues and characteristics:
!    .   knbb(NM_NF_MAX),eig_bb(5*NM_NF_MAX),
!: Broadband transfer functions [refer to as tf(nfbb,nrec)]:
!    .   tf(NTFMAX)
!     complex*16 kn,eig_char,knbb,eig_bb,ksm2_sr
!     character*64 svp_file,opt_file,outroot,outfile
!     real*8 geo,h,zdep,fexp,zsr,zsr_im_gbs,rho_sr,range,exp_gbs,
!    .   cp_sr,cs_sr
!     complex*8 tlc,phi,dphi,psi,dpsi,tf,sq2pir
!     real*4 tli
complex*16 :: kn(0:NM_MAX),eig_char(5,NM_MAX),ksm2_sr(NSRMAX)
!yt complex*16, dimension(:), allocatable :: knbb,eig_bb	! NM_NF_MAX,5*NM_NF_MAX
!yt real*8, dimension(:), allocatable :: vg_bb,Dvg_w	! for rx_bb
!yt real*4, dimension(:), allocatable :: phi_bb	        ! for rx_bb
!yt character(len=64) :: svp_file,opt_file,outroot,outfile
real*8 :: geo(2,5,NLMAX),h(NLMAX),zdep(NLMAX),fexp(2,NLMAX)
real*8 :: zsr(NSRMAX),zsr_im_gbs(NSRMAX),rho_sr(NSRMAX),cp_sr(NSRMAX),&
	cs_sr(NSRMAX)
real*8, dimension(:), allocatable :: range	! NSNRMAX
!real*8, dimension(:), allocatable :: exp_gbs	! NSR_NM_MAX
!complex*8, dimension(:), allocatable :: phi,dphi,psi,dpsi	! NSR_NM_MAX
complex*8 :: phi(NSR_NM_MAX),dphi(NSR_NM_MAX),psi(NSR_NM_MAX),dpsi(NSR_NM_MAX)
real*8 :: exp_gbs(NSR_NM_MAX)
!yt complex*8, dimension(:), allocatable :: tlc	! NTLMAX
!yt complex*8, dimension(:), allocatable :: tf	! NTFMAX
complex*8, dimension(:), allocatable :: sq2pir	! NSNRMAX
!yt real*4, dimension(:), allocatable :: tli	! NTLMAX
!cc
!cc
!: Integer*4 and real*4 arrays:
!     common /out_com2/
!: nlay=# layers in geo,h,zdep.
!    .   nlay,lsvp,lopt,out,lout,loutf,nzref(NM_MAX),kksh(NSRMAX),
!: Number of modes, mode numbers, max # modes for dispersion curves:
!    .   nmode,nm_put,nm_tot,nm_lim,mode_no(3,NM_MAX),nm_miss,
!    .   mode_phz(3,0:NM_MAX),nm_cw_max,iishn(NM_MAX),
!: iifail=1 when failure occurs in cw_modes or bb_modes:
!    .   iifail,iidone,xlam_fb1,xlam_fb2,
!: Arrays for mapping zsr(1:nzsr) to zsrc,zrec,zmf (see opt_com for nsrc, etc):
!    .   nzsr,mzsrc(NSRMAX),mzrec(NSRMAX),mzmf(NSRMAX),rng_sr(NSNRMAX),
!: Arrays for mapping sorted ranges to tlc(1:nsrc,1:nrec):
!    .   nrng,nrec_jr(NSNRMAX),krec_jr(NSNRMAX),jrec_jr(2,NSNRMAX),
!: Broadband nfft, # frequencies, frequency axis:
!    .   nfftbb,nfbb,faxbb(NFBBMAX),iish_bb(NM_NF_MAX),
!: TL array [refer to as tl(1:nzs,1:nsrc,1:nrec)]:
!    .   tl(NTLMAX),
!: Source/receiver geometry arrays (see opt_com for zsrc,zrec,xsrc):
!    .   xsrc(NRNGMAX),ysrc(NRNGMAX),xrec(NSRMAX),yrec(NSRMAX),iigeom,
!    .   kn_indx(NM_MAX),zsr_indx(NSNRMAX)
!     integer*4 nlay,lsvp,lopt,out,lout,loutf,nmode,nm_put,nm_tot,
!    .   nm_lim,mode_no,
!    .   nm_miss,nm_cw_max,iishn,iifail,iidone,nzref,nzsr,kksh,mzsrc,
!    .   mzrec,mzmf,nrng,nrec_jr,krec_jr,jrec_jr,nfftbb,nfbb,iish_bb,
!    .   iigeom,kn_indx,zsr_indx
!     real*4 tl,faxbb,xsrc,ysrc,xrec,yrec,rng_sr,mode_phz,
!    .   xlam_fb1,xlam_fb2
integer*4 :: nlay,lsvp,lopt,out,lout,loutf,nmode,nm_put,nm_tot,&
	nm_lim,mode_no(3,NM_MAX),nm_miss,nm_cw_max,iishn(NM_MAX),&
	iifail,iidone,nzref(NM_MAX),nzsr,kksh(NSRMAX),nrng,iigeom,&
	nfftbb,nfbb,kn_indx(NM_MAX)
integer*4, dimension(:), allocatable :: mzsrc,mzrec,mzmf	! NSRMAX
integer*4, dimension(:), allocatable :: nrec_jr,krec_jr,zsr_indx ! NSNRMAX 
integer*4, dimension(:,:), allocatable :: jrec_jr	! (2,NSNRMAX)
!yt integer*4, dimension(:), allocatable :: iish_bb		! NM_NF_MAX
!yt real*4, dimension(:), allocatable :: tl			! NTLMAX
!yt real*4, dimension(:), allocatable :: faxbb		! NFBBMAX
real*4 :: xsrc(NRNGMAX),ysrc(NRNGMAX),mode_phz(3,0:NM_MAX)
real*4, dimension(:), allocatable :: xrec,yrec	! NSRMAX
real*4, dimension(:,:), allocatable :: rng_sr	! NSNRMAX (nsrc,nrec)
real*4 :: xlam_fb1,xlam_fb2
!
end module i_o_com
