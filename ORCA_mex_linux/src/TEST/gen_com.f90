!     common /bb_com/ wbb(NFBBMAX),nffth1bb,xhbb(20),nf1,nf2,iifull,df,
!    .   phibb(NZ_NF_MAX),dpsibb(NZ_NF_MAX),nmbb(NFBBMAX),
!    .   kim_bb(NFBBMAX),nh_off,lheadw,lrecw,phim_bb(NFBBMAX)
!     integer*4 nffth1bb,nf1,nf2,iifull,nmbb,nh_off,lheadw,lrecw
!     real*8 wbb,df,kim_bb
!     real*4 xhbb,phim_bb
!     complex*8 phibb,dpsibb
module gen_com
use Parms_com
implicit none
save

!yt integer*4, dimension(:), allocatable :: nmbb	! NFBBMAX
integer*4 :: nffth1bb,nf1,nf2,iifull,nh_off,lheadw,lrecw
!yt real*8, dimension(:), allocatable :: wbb,kim_bb	! NFBBMAX
real*8 :: df
!real*4, dimension(:), allocatable :: phim_bb	! NFBBMAX
!yt real*4 :: xhbb(20)
!yt complex*8, dimension(:), allocatable :: phibb,dpsibb	! NZ_NF_MAX
!cc
!cc
!     common /geo_com/ 
!    .   Pcon(3,NLMAX),Qcon(3,NLMAX),Ucon(3,NLMAX),Vcon(3,NLMAX),
!    .   Alay(3,2),Blay(3,2),ikcon(3),rholay(2),f_hz,w,wsq,xkh,xkhsq,
!    .   xkhrat,xkhratp,xkref,kw,kw0,cref,pie,phtot,gami(3,2,NLMAX),
!    .   beti(3,2,NLMAX),xk(2,NLMAX),xb(2,NLMAX),xksq(2,NLMAX),
!    .   xbsq(2,NLMAX),eta(NLMAX),etb(NLMAX),etasq(NLMAX),
!    .   etbsq(NLMAX),rhorat(NLMAX),gamiref,xbsqinv(2,NLMAX),
!    .   xkbp(2,2),xkrat(2,2),xkrat_ref(2),cfmin,csmin,cpfake(2),crmax,
!    .   kcrmin,cphlo,cphhi,zduct(NLMAX),rho_duct,twpie,kcut,lncut,
!    .   phcut,chspmax,khspmin,f_min,f_max,xi_hsp(2,2),
!    .   geo_fake(2,5,NDMAX)
!     complex*16 Pcon,Qcon,Ucon,Vcon,Alay,Blay,ikcon,xkh,xkhsq,xkhrat,
!    .   xkhratp,xkref,gami,beti,xk,xb,xksq,xbsq,eta,etb,
!    .   etasq,etbsq,xbsqinv,xkbp,xkrat,xkrat_ref,gamiref,kcut,lncut,
!    .   xi_hsp
!     real*8 rholay,f_hz,w,wsq,kw,kw0,cref,pie,phtot,rhorat,cfmin,
!    .   csmin,cpfake,crmax,kcrmin,cphlo,cphhi,zduct,rho_duct,twpie,
!    .   phcut,chspmax,khspmin,f_min,f_max,geo_fake
complex*16 :: Pcon(3,NLMAX),Qcon(3,NLMAX),Ucon(3,NLMAX),Vcon(3,NLMAX),&
	Alay(3,2),Blay(3,2),ikcon(3),xkh,xkhsq,xkhrat,&
	xkhratp,xkref,gami(3,2,NLMAX),beti(3,2,NLMAX),&
	xk(2,NLMAX),xb(2,NLMAX),xksq(2,NLMAX),xbsq(2,NLMAX),&
	eta(NLMAX),etb(NLMAX),etasq(NLMAX),etbsq(NLMAX),&
	xbsqinv(2,NLMAX),xkbp(2,2),xkrat(2,2),xkrat_ref(2),&
	gamiref,kcut,lncut,xi_hsp(2,2)
real*8 :: rholay(2),f_hz,w,wsq,kw,kw0,cref,phtot,rhorat(NLMAX),&
	cfmin,csmin,cpfake(2),crmax,kcrmin,cphlo,cphhi,zduct(NLMAX),&
	rho_duct,phcut,chspmax,khspmin,f_min,f_max !yt,&
! yt	geo_fake(2,5,NDMAX)
real*8, parameter :: pie=3.14159265358979d0,twpie=6.28318530717959d0
!cc
!cc
!     common /geo_com2/ isp(NLMAX),iss(NLMAX),iiww(NLMAX),
!    .   mm(NLMAX),jflu(2,5),jsol(2,2),nduct,jduct(5,NDMAX),
!    .   mzduct(NDMAX),nsvmin,nsvmin0,isvmin,kduct,kduct0,allf(2),
!    .   jsurf,jobot,iich,iich_ref,iish(2,2),iish_ref(2),iish0(2,2),
!    .   iishr0(2),iisol(NLMAX),jlmin,jlmax,jhsp(2),nhigh,nrise,
!    .   jlfake(2),iimst,n_int,j_int(NLMAX),mzint(NLMAX),iilk,iiccw,
!    .   iicut,ihf(NLMAX),jd_ch(NDMAX),phi_mag_max,iiblm,iishx(2,2),
!    .   iish_refx(2),nblm,nblm_max,nhigh_max,ii_xi_mv(2),
!    .   Aih_mag(2),Aih_dir(2),nthorpe,jthorpe(NLMAX)
!     integer*4 isp,iss,iiww,mm,jflu,jsol,nduct,jduct,mzduct,nsvmin,
!    .   nsvmin0,isvmin,kduct,kduct0,allf,jsurf,jobot,iich,iich_ref,
!    .   iish,iish_ref,iish0,iishr0,iisol,jlmin,jlmax,jhsp,nhigh,
!    .   nrise,jlfake,iimst,n_int,j_int,mzint,iilk,iiccw,iicut,ihf,
!    .   jd_ch,iiblm,iishx,iish_refx,nblm,nblm_max,nhigh_max,
!    .   ii_xi_mv,nthorpe,jthorpe
!     real*4 phi_mag_max,Aih_mag,Aih_dir
integer*4 :: isp(NLMAX),iss(NLMAX),iiww(NLMAX),mm(NLMAX),jflu(2,5),&
	jsol(2,2),nduct,jduct(5,NDMAX),mzduct(NDMAX),nsvmin,&
	nsvmin0,isvmin,kduct,kduct0,allf(2),jsurf,jobot,&
	iich,iich_ref,iish(2,2),iish_ref(2),iish0(2,2),iishr0(2),&
	iisol(NLMAX),jlmin,jlmax,jhsp(2),nhigh,nrise,jlfake(2),&
	iimst,n_int,j_int(NLMAX),mzint(NLMAX),iilk,iiccw,iicut,&
	ihf(NLMAX),jd_ch(NDMAX),iiblm,iishx(2,2),iish_refx(2),&
	nblm,nblm_max,nhigh_max,ii_xi_mv(2),nthorpe,jthorpe(NLMAX)
real*4 :: phi_mag_max,Aih_mag(2),Aih_dir(2)
!cc
!     common /var_com/ theta(NSRMAX),xh(20),xkhr(NSRMAX),xkhi(NSRMAX),
!    .   fftfile
!     real*4
!     character*64 fftfile
!yt real*4 :: theta(NSRMAX),xh(20),xkhr(NSRMAX),xkhi(NSRMAX)
character(len=64) :: fftfile
!cc
!     common /vw_com/ Vmat(3,5,NLMAX,2),Wmat(6,NLMAX,2)
!     complex*16 Vmat,Wmat
complex*16 :: Vmat(3,5,NLMAX,2),Wmat(6,NLMAX,2)
!cc
!     common /zsr_com/ jsr2j(NSRMAX),nzmx(NLMAX),nzmxtot,
!    .   jzmx(NLMAX),jsrmx(NSRMAX),mx_m(NSRMAX),xmode(NM_MAX),
!    .   nctot,ncalc(NM_MAX),nclast,ncall,ncmat,nm_max2,
!    .   nrleg(NSEGMAX),t_src(NRNGMAX),rec_lab(NSRMAX),
!    .   r4mat1(NHDFMAX),r4mat2(NHDFMAX),aisoln(2,NLMAX),
!    .   zsrmin,zsrmax,nsave,nmerge,nskip
!     integer*4 nzmx,nzmxtot,jzmx,jsrmx,mx_m,nctot,ncalc,nclast,
!    .   nrleg,ncall,ncmat,nm_max2,jsr2j,aisoln,nsave,nmerge,nskip
!     real*4 xmode,r4mat1,r4mat2,t_src,rec_lab,zsrmin,zsrmax
!yt integer*4 :: nzmx(NLMAX),nzmxtot,jzmx(NLMAX),jsrmx(NSRMAX),&
!yt	 mx_m(NSRMAX),nctot,ncalc(NM_MAX),nclast,nrleg(NSEGMAX),&
!yt	 ncall,ncmat,nm_max2,jsr2j(NSRMAX),aisoln(2,NLMAX),nsave,&
!yt	 nmerge,nskip
integer*4 :: nzmx(NLMAX),nzmxtot,jzmx(NLMAX),jsrmx(NSRMAX),&
	mx_m(NSRMAX),nctot,ncalc(NM_MAX),nclast,nrleg(NSEGMAX),&
	nm_max2,jsr2j(NSRMAX),aisoln(2,NLMAX),nsave,&
	nmerge,nskip
real*8, dimension(:), allocatable :: r4mat1,r4mat2	! NHDFMAX
real*8 :: xmode(NM_MAX),t_src(NRNGMAX),rec_lab(NSRMAX),zsrmin,zsrmax
!cc
!cc
!     common /zsr_com2/ zmx(NSRMAX),zmx_im_gbs(NSRMAX),phisr(NSRMAX),
!    .   dpsisr(NSRMAX),dkim,kim_min,kim_max,kim_fac,errdkms,errdk2,
!    .   errdk100,kremin,phfac0,magfac,magfac0,ph_step,mag_step,
!    .   phix(NSRMAX),dphix(NSRMAX),psix(NSRMAX),dpsix(NSRMAX),
!    .   expx_gbs(NSRMAX),
!    .   xi(NSRMAX),ai(NSRMAX),aip(NSRMAX),bi(NSRMAX),bip(NSRMAX),
!    .   xis(NSRMAX),ais(NSRMAX),aips(NSRMAX),bis(NSRMAX),
!    .   bips(NSRMAX),zzexp(NSRMAX),zzexps(NSRMAX),r8mat(NHDFMAX)
!     real*8 zmx,zmx_im_gbs,dkim,kim_min,kim_max,kim_fac,errdkms,
!    .   errdk2,errdk100,kremin,phfac0,magfac,magfac0,
!    .   ph_step,mag_step,r8mat,expx_gbs
!     complex*8 phisr,dpsisr
!     complex*16 phix,dphix,psix,dpsix,xi,ai,aip,bi,bip,xis,ais,aips,
!    .   bis,bips,zzexp,zzexps
real*8 :: zmx(NSRMAX),zmx_im_gbs(NSRMAX),dkim,kim_min,kim_max,&
	kim_fac,errdkms,errdk2,errdk100,kremin,phfac0,magfac,&
	magfac0,ph_step,mag_step,expx_gbs(NSRMAX)
!yt complex*8 :: phisr(NSRMAX),dpsisr(NSRMAX)
complex*16 :: phix(NSRMAX),dphix(NSRMAX),psix(NSRMAX),dpsix(NSRMAX),&
	xi(NSRMAX),ai(NSRMAX),aip(NSRMAX),bi(NSRMAX),bip(NSRMAX),&
	xis(NSRMAX),ais(NSRMAX),aips(NSRMAX),bis(NSRMAX),&
	bips(NSRMAX),zzexp(NSRMAX),zzexps(NSRMAX)
!cc
!cc
!     common /phz_com/ ailay(2,2,2,NLMAX),bilay(2,2,2,NLMAX),
!    .   zetalay(2,2,NLMAX),philay(2,NLMAX),dphilay(2,NLMAX),
!    .   psilay(2,NLMAX),dpsilay(2,NLMAX),Aplay(2,NLMAX),
!    .   Aslay(2,NLMAX)
!     complex*16 ailay,bilay,zetalay,philay,dphilay,psilay,dpsilay
!     real*8 Aplay,Aslay
complex*16 :: ailay(2,2,2,NLMAX),bilay(2,2,2,NLMAX),&
	zetalay(2,2,NLMAX),philay(2,NLMAX),dphilay(2,NLMAX),&
	psilay(2,NLMAX),dpsilay(2,NLMAX)
real*8 :: Aplay(2,NLMAX),Aslay(2,NLMAX)
!cc
!cc
!     common /traj_com/ k_sdp,ln_sdp,dln_sdp,k_spt,ln_spt,dln_spt,
!    .   k_cont(150),ln_cont(150),dln_cont(150)
!     complex*16 k_sdp,ln_sdp,dln_sdp,k_spt,ln_spt,dln_spt,
!    .   k_cont,ln_cont,dln_cont
complex*16 :: k_sdp,ln_sdp,dln_sdp,k_spt,ln_spt,dln_spt,&
	k_cont(150),ln_cont(150),dln_cont(150)
!cc
!cc
!     common /traj_com2/ npt,nmin_dln,jmin_dln(32),dln_deep(32),deepest
!     integer*4 npt,nmin_dln,jmin_dln
!     real*4 dln_deep,deepest
integer*4 :: npt,nmin_dln,jmin_dln(32)
real*4 :: dln_deep(32),deepest
!cc
!cc
!     common /rx_com/ dk_max,dk_max0,g11(6,2,NLMAX),g12(6,2,NLMAX),
!    .   g21(6,2,NLMAX),g22(6,2,NLMAX),g_exp(6,NLMAX),h11(6,2,NLMAX),
!    .   h12(6,2,NLMAX),h21(6,2,NLMAX),h22(6,2,NLMAX),h_exp(6,NLMAX),
!    .   u11(6,NLMAX),u12(6,NLMAX),u21(6,NLMAX),u22(6,NLMAX),
!    .   u_exp(6,NLMAX),migam2(6),ekn(6,0:NM_MAX),rhofac(NLMAX),
!    .   rho_prod(2,NLMAX),Dphix_w(NSRMAX),Ddphix_w(NSRMAX),
!    .   Dphi_w(2,NLMAX),Ddphi_w(2,NLMAX),Dphiz_w(NSR_NM_MAX),
!    .   Ddphiz_w(NSR_NM_MAX),am(NLMAX),bm(NLMAX),betm(NLMAX),
!    .   gm(NLMAX),rhom(NLMAX),xilay(2,NLMAX),cspan(0:NDMAX),
!    .   cduct(NDMAX),dz_duct(NDMAX),zpeak(NDMAX+1),ccr_lo,ccr_hi,
!    .   iia1b1(NLMAX),indx_duct(NDMAX),ndrx,jl2jd(NLMAX),
!    .   jpeak(NDMAX),jval(2,NDMAX),jmlink(NM_MAX),nlwz,jlwz(NLMAX),
!    .   nzoff,jlayx(NDMAX)
!     integer*4 iia1b1,indx_duct,ndrx,jl2jd,jpeak,jmlink,jval,nlwz,
!    .   jlwz,nzoff,jlayx
!     real*8 dk_max,dk_max0,g11,g12,g21,g22,g_exp,h11,h12,h21,h22,
!    .   h_exp,u11,u12,u21,u22,u_exp,migam2,ekn,rhofac,rho_prod,
!    .   Dphix_w,Ddphix_w,Dphi_w,Ddphi_w,Dphiz_w,Ddphiz_w,
!    .   am,bm,betm,gm,rhom,xilay,cspan,cduct,dz_duct,zpeak,
!    .   ccr_lo,ccr_hi
integer*4 :: iia1b1(NLMAX),indx_duct(NDMAX),ndrx,jl2jd(NLMAX),&
	jpeak(NDMAX),jmlink(NM_MAX),jval(2,NDMAX),nlwz,jlwz(NLMAX),&
	nzoff,jlayx(NDMAX)
real*8 :: dk_max,dk_max0,g11(6,2,NLMAX),g12(6,2,NLMAX),&
	g21(6,2,NLMAX),g22(6,2,NLMAX),&
	cspan(0:NDMAX),dz_duct(NDMAX),&
	zpeak(NDMAX+1),ccr_lo,ccr_hi
!yt	Dphix_w(NSRMAX),Ddphix_w(NSRMAX),Dphi_w(2,NLMAX),&
!yt		Ddphi_w(2,NLMAX),Dphiz_w(NSR_NM_MAX),Ddphiz_w(NSR_NM_MAX),&
!yt		am(NLMAX),bm(NLMAX),betm(NLMAX),gm(NLMAX),rhom(NLMAX),&
!yt		ekn(6,0:NM_MAX),rhofac(NLMAX),rho_prod(2,NLMAX),&
!yt		xilay(2,NLMAX),	cduct(NDMAX),
!yt		h22(6,2,NLMAX),h_exp(6,NLMAX),u11(6,NLMAX),u12(6,NLMAX),&
!yt		u21(6,NLMAX),u22(6,NLMAX),u_exp(6,NLMAX),migam2(6),&
!yt		g_exp(6,NLMAX),	h11(6,2,NLMAX),h12(6,2,NLMAX),h21(6,2,NLMAX),&

!cc
!cc
!     common /rx_com2/ rx_r1r2(6),R1(6),R2(6),TC1(6),TC2(6),Wr(6)
!     complex*16 rx_r1r2,R1,R2,TC1,TC2,Wr
complex*16 rx_r1r2(6),R1(6),R2(6),TC1(6),TC2(6),Wr(6)
end module gen_com
