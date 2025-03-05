      subroutine opt_read(mex_in_iimf, 
     & mex_in_cphmax, mex_in_rmin, mex_in_rmax, mex_in_phfac, 
     & mex_in_dbcut, mex_in_Aih_l, mex_in_Aih_u, mex_in_nf, 
     & mex_in_fcw_n, mex_in_fcw, mex_in_nzm, mex_in_zm_n, mex_in_zm)
c
c: Reads option file.
c
      use Parms_com
      use i_o_com
      use gen_com
cyt mex in
      integer*4 mex_in_iimf
      real*8 mex_in_cphmax, mex_in_rmin, mex_in_rmax, 
     & mex_in_phfac, mex_in_dbcut, mex_in_Aih_l, mex_in_Aih_u
      integer*4 mex_in_nf, mex_in_fcw_n
      real*8 mex_in_fcw(mex_in_fcw_n)
      integer*4 mex_in_nzm, mex_in_zm_n
      real*8 mex_in_zm(mex_in_zm_n)
cyt       integer*4 nline,j,iierr,nrd
      integer*4 j,nrd
      real*4 rminmin
cyt       character*64 eline
cyt       data eline/'INVALID INPUT IN OPT FILE: '/
c
cyt       write(2,95)
cyt 95    format(/'### OPTION FILE INFORMATION ###')
cyt       open(46,file=opt_file(1:lopt),form='formatted',err=110)
cyt       nline=0
cyt       iierr=0
cyt       iigeom=0
cyt       call star2(46,nline,2,opt_file,1,iiwrite)
cyt       read(46,*,end=110,err=110) ver_no
cyt       call check_val_r4(ver_no,0.90e0,2.01e0,eline,27,nline,'ver_no',
cyt      .   6,iierr)
cyt       backspace(46)
cyt       if(ver_no .lt. 1.10) then
cyt          read(46,*,end=110,err=110) ver_no,iicw,iikpl,iirc,n_env
cyt          iifmt=1
cyt          iiparm=0
cyt          if(iicw .eq. 3) then
cyt             iiparm=1
cyt             iicw=1
cyt          endif
cyt       else
cyt          read(46,*,end=110,err=110) ver_no,iicw,iikpl,iirc,iiparm,
cyt      .      n_env,iifmt
         ver_no = 2.01
         iicw = 1
         iikpl = 0
         iirc = 0
         iiparm = 0
         n_env = 0
         iifmt = 3
cyt          call check_val_i(iifmt,0,3,eline,27,nline,'iifmt',5,iierr)
cyt          call check_val_i(iiparm,0,1,eline,27,nline,'iiparm',6,iierr)
cyt       endif
cyt       call check_val_i(iicw,0,2,eline,27,nline,'iicw',4,iierr)
cyt       call check_val_i(iikpl,-4,5,eline,27,nline,'iikpl',5,iierr)
cyt       call check_val_i(iirc,0,2,eline,27,nline,'iirc',4,iierr)
cyt       call check_val_i(n_env,0,10000,eline,27,nline,'n_env',5,iierr)
c
cyt       iiAih(1)=-1.
cyt       iiAih(2)=-1.
      kkblm=1
cyt       call star2(46,nline,2,opt_file,1,iiwrite)
cyt       if(ver_no .ge. 2.01e0) then
cyt          read(46,*,end=110,err=110) iirx,cphmin,cphmax,rmin,rmax,
cyt      .      phfac,db_cut,iiAih(1),iiAih(2),kkblm,iigbs,iidiag
      iirx = 0
      cphmin = 0.e0
      cphmax = mex_in_cphmax
      rmin = mex_in_rmin
      rmax = mex_in_rmax
      phfac = mex_in_phfac
      db_cut = mex_in_dbcut
      iiAih(1) = mex_in_Aih_l
      iiAih(2) = mex_in_Aih_u
      iigbs = 0
      iidiag = 0
cyt       elseif(ver_no .ge. 2.0e0) then
cyt          read(46,*,end=110,err=110) iirx,cphmin,cphmax,rmin,rmax,
cyt      .      phfac,db_cut,iiAih(1),iiAih(2),iigbs,iidiag
cyt       elseif(ver_no .ge. 1.6e0) then
cyt          read(46,*,end=110,err=110) iirx,cphmin,cphmax,rmin,rmax,
cyt      .      phfac,db_cut,iiAih(1),iigbs,iidiag
cyt       elseif(ver_no .ge. 1.e0) then
cyt          read(46,*,end=110,err=110) iirx,cphmin,cphmax,rmin,rmax,
cyt      .      phfac,db_cut,iiAih(1),iidiag
cyt       elseif(ver_no .ge. 0.95e0) then
cyt          read(46,*,end=110,err=110) cphmin,cphmax,rmin,rmax,phfac,
cyt      .      db_cut,iidiag
cyt          iirx=0
cyt          iigbs=0
cyt       else
cyt          read(46,*,end=110,err=110) cphmin,cphmax,rmin,rmax,phfac,
cyt      .      db_cut
cyt          iidiag=0
cyt          iirx=0
cyt          iigbs=0
cyt       endif
cyt       call check_val_r4(cphmin,-1.e0,1.e10,eline,27,nline,
cyt      .   'cphmin',6,iierr)
cyt       call check_val_r4(cphmax,-89.99e0,1.e10,eline,27,nline,
cyt      .   'cphmax',6,iierr)
      rminmin=-float(NM_MAX)
cyt       call check_val_r4(rmin,rminmin,10000.,eline,27,nline,
cyt      .   'rmin',4,iierr)
cyt       call check_val_r4(rmax,0.e0,1.e10,eline,27,nline,
cyt      .   'rmax',4,iierr)
cyt       call check_val_r4(phfac,0.e0,512.e0,eline,27,nline,
cyt      .   'phfac',5,iierr)
cyt       call check_val_r4(db_cut,0.e0,120.e0,eline,27,nline,
cyt      .   'db_cut',6,iierr)
cyt       call check_val_i(iirx,-1,2,eline,27,nline,'iirx',4,iierr)
cyt       call check_val_r4(iiAih(1),-1.,361.,eline,27,nline,'iiAih(1)',
cyt      .   7,iierr)
cyt       call check_val_r4(iiAih(2),-1.,361.,eline,27,nline,'iiAih(2)',
cyt      .   7,iierr)
cyt       call check_val_i(kkblm,0,1,eline,27,nline,'kkblm',5,iierr)
cyt       call check_val_i(iigbs,0,1,eline,27,nline,'iigbs',5,iierr)
cyt       if(cphmax .ne. 0. .and. rmin .lt. 999.) then
cyt          iierr=1
cyt          print *,'Limit # modes using cphmax or rmin, but not both!'
cyt          print *,'Set cphmax=0 to disable phase speed limit OR '
cyt          print *,'Set rmin>=999 to disable minimum range limit.'
cyt       elseif(cphmax .eq. 0. .and. rmin .ge. 999.) then
cyt          iierr=1
cyt          print *,'Must use cphmax or rmin to limit # modes!'
cyt          print *,'Set cphmax nonzero to enable phase speed limit OR '
cyt          print *,'Set rmin<999 to enable minimum range limit.'
cyt       endif
c
      f_min=1.d20
      f_max=0.d0
c
cyt       if(iicw .eq. 1) then
         iikn=0
cyt          call star2(46,nline,2,opt_file,1,iiwrite)
cyt          read(46,*,end=110,err=110) nfcw,(fcw(j),j=1,nrd(nfcw))
         nfcw = mex_in_nf
         do j = 1,nrd(nfcw)
            fcw(j) = mex_in_fcw(j)
         enddo
cyt          call star2(46,nline,2,opt_file,1,iiwrite)
cyt          if(ver_no .ge. 1.e0) then
cyt             read(46,*,end=110,err=110) iitl,iimf,iisig,iimt,iidc,
cyt      .         iikn,iilist,iikrak,iioas,iifepe,iimlab
          iitl = 0
          iimf = mex_in_iimf
          iisig = 0 
          iimt = 0
          iidc = 1
          iikn = 1
          iilist = 0
          iikrak = 0
          iioas = 0
          iifepe = 0
          iimlab= 0 
cyt             call check_val_i(iikn,0,1,eline,27,nline,'iikn',4,iierr)
cyt             call check_val_i(iikrak,0,100,eline,27,nline,'iikrak',6,
cyt      .         iierr)
cyt             call check_val_i(iioas,0,5,eline,27,nline,'iioas',5,
cyt      .         iierr)
cyt             call check_val_i(iifepe,0,2,eline,27,nline,'iifepe',6,
cyt      .         iierr)
cyt             call check_val_i(iimlab,0,1,eline,27,nline,'iimlab',6,
cyt      .         iierr)
            iitsp=0
cyt c         elseif(ver_no .ge. 0.96e0) then
cyt             read(46,*,end=110,err=110) iitl,iimf,iisig,iimt,iidc,
cyt      .         iilist,iitsp,iikrak,iikn
cyt             call check_val_i(iikn,0,1,eline,27,nline,'iikn',4,iierr)
cyt             call check_val_i(iikrak,-3,100,eline,27,nline,'iikrak',6,
cyt      .         iierr)
cyt          elseif(ver_no .ge. 0.95e0) then
cyt             read(46,*,end=110,err=110) iitl,iimf,iisig,iimt,iidc,
cyt      .         iilist,iitsp,iikrak
cyt             call check_val_i(iikrak,-1,100,eline,27,nline,'iikrak',6,
cyt      .         iierr)
cyt          elseif(ver_no .ge. 0.92e0) then
cyt             read(46,*,end=110,err=110) iitl,iimf,iisig,iimt,iidc,
cyt      .         iilist,iitsp,iidiag,iikrak
cyt             call check_val_i(iikrak,-1,100,eline,27,nline,'iikrak',6,
cyt      .         iierr)
cyt          elseif(ver_no .ge. 0.91e0) then
cyt             read(46,*,end=110,err=110) iitl,iimf,iisig,iimt,iidc,
cyt      .         iilist,iitsp,iidiag
cyt             iikrak=0
cyt          else
cyt             print *,'ver_no<0.91 no longer supported'
cyt             stop
cyt          endif
cyt c
         call mem_lim(nfcw,NFCWMAX,MLINE,LML,'nfcw',4,'NFCWMAX',7,
     .      1,1)
cyt          call check_val_i(iitl,-3,3,eline,27,nline,'iitl',4,iierr)
cyt          call check_val_i(iimf,0,3,eline,27,nline,'iimf',4,iierr)
cyt          call check_val_i(iisig,0,1,eline,27,nline,'iisig',5,iierr)
cyt          call check_val_i(iimt,0,1,eline,27,nline,'iimt',4,iierr)
cyt          call check_val_i(iidc,0,3,eline,27,nline,'iidc',4,iierr)
cyt          call check_val_i(iilist,0,1,eline,27,nline,'iilist',6,iierr)
cyt          call check_val_i(iitsp,0,1,eline,27,nline,'iitsp',5,iierr)
cyt          call check_val_i(iidiag,-10,3,eline,27,nline,'iidiag',6,
cyt      .      iierr)
cyt          do j=1,nrd(nfcw)
cyt             call check_val_r4(fcw(j),0.e0,1.e10,eline,27,nline,
cyt      .         'fcw(j)',6,iierr)
cyt          enddo
         f_min=fcw(1)
         f_max=fcw(1)
         do j=2,nrd(nfcw)
            f_min=amin1(fcw(j),sngl(f_min))
            f_max=amax1(fcw(j),sngl(f_max))
         enddo
         iigeom = 1
         nzs = 1
         zsrc = 1
         nrec = 1
         zrec = 1
         nsrc = 1
         rkm = rmax
cyt c
cyt          if(iabs(iitl) .eq. 1 .or. iabs(iitl) .eq. 3) then                
cyt             iigeom=1                                                      
cyt             call star2(46,nline,2,opt_file,0,iiwrite)                 
cyt             if(iigbs .eq. 0) then                                         
cyt                read(46,*,end=110,err=110) nzs,(zsrc(j),                   
cyt      .            j=1,nrd(nzs)),nrec,(zrec(j),j=1,nrd(nrec)),             
cyt      .            nsrc,(rkm(j),j=1,nrd(nsrc))                             
cyt                write(2,202) nzs,(zsrc(j),j=1,nrd(nzs))                    
cyt                write(2,202) nrec,(zrec(j),j=1,nrd(nrec))                  
cyt                write(2,202) nsrc,(rkm(j),j=1,nrd(nsrc))                   
cyt             else                                                          
cyt                read(46,*,end=110,err=110) nzs,(zsrc(j),                   
cyt      .            j=1,nrd(nzs)),nrec,(zrec(j),j=1,nrd(nrec)),             
cyt      .            nsrc,(rkm(j),j=1,nrd(nsrc)),nth_gbs,(th_gbs(j),         
cyt      .            j=1,nrd(nth_gbs)),nb_gbs,(b_gbs(j),j=1,nrd(nb_gbs))     
cyt                write(2,202) nzs,(zsrc(j),j=1,nrd(nzs))                    
cyt                write(2,202) nrec,(zrec(j),j=1,nrd(nrec))                  
cyt                write(2,202) nsrc,(rkm(j),j=1,nrd(nsrc))                   
cyt                write(2,202) nth_gbs,(th_gbs(j),j=1,nrd(nth_gbs))          
cyt                write(2,202) nb_gbs,(b_gbs(j),j=1,nrd(nb_gbs))             
cyt                call check_val_i(iabs(nth_gbs),iabs(nzs),iabs(nzs),    
cyt      .            eline,27,nline,'nzs for Gaussian beam source angle',
cyt      .            34,iierr)                                           
cyt                call check_val_i(iabs(nb_gbs),iabs(nzs),iabs(nzs),     
cyt      .            eline,27,nline,'nzs for Gaussian beam beamwidth',   
cyt      .            31,iierr)                                           
cyt             endif                                                         
cyt 202         format(i4,2x,7(f9.3,1x)/1000(6x,7(f9.3,1x)/))                 
cyt             call check_val_i(iabs(nzs),1,NSRMAX,eline,27,nline,       
cyt      .         'nzs',7,iierr)                                         
cyt             call check_val_i(iabs(nrec),1,NSRMAX,eline,27,nline,      
cyt      .         'nrec',7,iierr)                                        
cyt             call check_val_i(iabs(nsrc),1,NRNGMAX,eline,27,nline,     
cyt      .         'nsrc',4,iierr)                                        
cyt          elseif(iabs(iitl) .eq. 2) then                                   
cyt             call star2(46,nline,2,opt_file,0,iiwrite)                 
cyt             iigeom=2                                                      
cyt             if(iigbs .eq. 1) then                                         
cyt                print *,'Gaussian Beam Source not implemented for ',       
cyt      .            'array geometry file. Setting to zero'                  
cyt                iigbs=0                                                    
cyt             endif                                                         
cyt          else                                                             
cyt             call star2(46,nline,2,opt_file,0,iiwrite)                 
cyt             iigbs=0                                                       
cyt          endif                                                            

cyt          if(iimf .ne. 0 .or. iisig .ne. 0) then
cyt             call star2(46,nline,2,opt_file,1,iiwrite)
cyt             read(46,*,end=110,err=110) iiri,iimp,nzmf,
cyt      .         (zmf(j),j=1,nrd(nzmf))
      iiri = 3
      iimp = 0
      nzmf = mex_in_nzm
      do j = 1,nrd(nzmf)
         zmf(j) = mex_in_zm(j)
      enddo
cyt             call check_val_i(iiri,0,3,eline,27,nline,'iiri',4,iierr)
cyt             call check_val_i(iimp,0,3,eline,27,nline,'iimp',4,iierr)
cyt             call check_val_i(iabs(nzmf),0,NSRMAX,eline,27,nline,
cyt      .         'nzmf',4,iierr)
cyt          else
cyt             call star2(46,nline,2,opt_file,0,iiwrite)
cyt          endif
c
cyt          if(iitsp .eq. 1) then
cyt             call star2(46,nline,2,opt_file,1,iiwrite)
cyt             read(46,*,end=110,err=110) nr_tsp,r1_tsp,r2_tsp,
cyt      .         nt_tsp,Tw_tsp,iimex,mr_tsp,pct_tsp,nrm_tsp
cyt             call check_val_i(nr_tsp,1,10000,eline,27,nline,
cyt      .         'nr_tsp',6,iierr)
cyt             call check_val_r4(r1_tsp,0.e0,r2_tsp,eline,27,nline,
cyt      .         'r1_tsp',6,iierr)
cyt             call check_val_r4(r2_tsp,r1_tsp,1.e10,eline,27,nline,
cyt      .         'r2_tsp',6,iierr)
cyt             call check_val_r4(Tw_tsp,1.e-6,1.e10,eline,27,nline,
cyt      .         'Tw_tsp',6,iierr)
cyt             call check_val_i(iimex,0,1,eline,27,nline,'iimex',5,
cyt      .         iierr)
cyt             call check_val_i(mr_tsp,1,3000,eline,27,nline,
cyt      .         'mr_tsp',6,iierr)
cyt             call check_val_r4(pct_tsp,1.e0,100.e0,eline,27,nline,
cyt      .         'pct_tsp',7,iierr)
cyt             call check_val_i(nrm_tsp,0,1,eline,27,nline,
cyt      .         'nrm_tsp',7,iierr)
cyt          elseif(ver_no .lt. 1.e0) then
cyt c: For ver_no < 1.0, skip line of inputs for time-spread plot:
cyt             call star2(46,nline,2,opt_file,0,iiwrite)
cyt          endif
cyt       else
cyt          call star2(46,nline,2,opt_file,1,iiwrite)
cyt cc       read(46,*,end=110,err=110) nfcw,(fcw(j),j=1,nrd(nfcw))
cyt          call star2(46,nline,2,opt_file,1,iiwrite)
cyt          if(ver_no .ge. 1.e0) then
cyt             read(46,*,end=110,err=110) iitl,iimf,iisig,iimt,iidc,
cyt      .         iikn,iilist,iikrak,iioas,iifepe,iimlab
cyt          endif
cyt          do j=1,2
cyt             call star2(46,nline,2,opt_file,0,iiwrite)
cyt          enddo
cyt          if(ver_no .lt. 1.e0) then
cyt c: For ver_no < 1.0, skip line of inputs for time-spread plot:
cyt             call star2(46,nline,2,opt_file,0,iiwrite)
cyt          endif
cyt       endif
cyt c
cyt       if(iicw .eq. 2) then
cyt          call star2(46,nline,2,opt_file,1,iiwrite)
cyt          if(ver_no .ge. 1.01) then
cyt             read(46,*,end=110,err=110) fsbb,Tw,fmin,fmax,iifft,
cyt      .         iiout,iift,iimt,iidc,iimf
cyt             call check_val_i(iimf,0,2,eline,27,nline,'iimf',4,iierr)
cyt          else
cyt             read(46,*,end=110,err=110) fsbb,Tw,fmin,fmax,iifft,
cyt      .         iiout,iift,iimt,iidc
cyt             iimf=0
cyt          endif
cyt          call check_val_r4(fsbb,0.e0,1.e10,eline,27,nline,
cyt      .      'fsbb',4,iierr)
cyt          call check_val_r4(Tw,-1.e10,131072e0,eline,27,nline,
cyt      .      'nfft/Tw',7,iierr)
cyt          if(fmax .lt. 0.d0) then
cyt             read(46,*,end=110,err=110) nfcw,(fcw(j),j=1,nrd(nfcw))
cyt             do j=1,nrd(nfcw)
cyt                call check_val_r4(fcw(j),0.e0,1.e10,eline,27,nline,
cyt      .            'fcw(j)',6,iierr)
cyt             enddo
cyt             f_min=fcw(1)
cyt             f_max=fcw(1)
cyt             do j=2,nrd(nfcw)
cyt                f_min=amin1(fcw(j),sngl(f_min))
cyt                f_max=amax1(fcw(j),sngl(f_max))
cyt             enddo
cyt             call uni_space(nfcw,fcw,1.e0)
cyt c: Make sure we do bb_brute or rx_bb_brute at list of frequencies:
cyt             if(iirx .ne. 0) iirx=2
cyt c: Make sure we do not try to output an FFT for a list of frequencies:
cyt             if(iifft .ne. 0) then
cyt                print *,'iifft set to 0 for list of frequencies'
cyt                write(2,*) 'iifft set to 0 for list of frequencies'
cyt                iifft=0
cyt             endif
cyt          else
cyt             call check_val_r4(fmin,1.e-3,fmax,eline,27,nline,
cyt      .         'fmin',4,iierr)
cyt             call check_val_r4(fmax,fmin,fsbb/2.e0,eline,27,nline,
cyt      .         'fmax',4,iierr)
cyt             f_min=fmin
cyt             f_max=fmax
cyt          endif
cyt          call check_val_i(iifft,0,2,eline,27,nline,'iifft',5,iierr)
cyt          call check_val_i(iiout,0,2,eline,27,nline,'iiout',5,iierr)
cyt          call check_val_i(iift,0,5,eline,27,nline,'iift',4,iierr)
cyt          call check_val_i(iimt,0,1,eline,27,nline,'iimt',4,iierr)
cyt          call check_val_i(iidc,0,3,eline,27,nline,'iidc',4,iierr)
cyt          if(iifft*iiout .ne. 0 .and. iifft .ne. iiout) then
cyt             print *,'Require iifft=iiout when iifft>0 and iiout>0.'
cyt             print *,'Assuming zs,zr,r to be read according to iifft.'
cyt             iiout=iifft
cyt          endif
cyt c
cyt          call star2(46,nline,2,opt_file,0,iiwrite)
cyt          if(iifft .eq. 1 .or. iiout .eq. 1 .or. iimf .ne. 0) then
cyt             iigeom=1
cyt             if(iigbs .eq. 0) then
cyt                read(46,*,end=110,err=110) nzs,(zsrc(j),
cyt      .            j=1,nrd(nzs)),nrec,(zrec(j),j=1,nrd(nrec)),
cyt      .            nsrc,(rkm(j),j=1,nrd(nsrc))
cyt                write(2,202) nzs,(zsrc(j),j=1,nrd(nzs))
cyt                write(2,202) nrec,(zrec(j),j=1,nrd(nrec))
cyt                write(2,202) nsrc,(rkm(j),j=1,nrd(nsrc))
cyt             else
cyt                read(46,*,end=110,err=110) nzs,(zsrc(j),
cyt      .            j=1,nrd(nzs)),nrec,(zrec(j),j=1,nrd(nrec)),
cyt      .            nsrc,(rkm(j),j=1,nrd(nsrc)),nth_gbs,(th_gbs(j),
cyt      .            j=1,nrd(nth_gbs)),nb_gbs,(b_gbs(j),j=1,nrd(nb_gbs))
cyt                write(2,202) nzs,(zsrc(j),j=1,nrd(nzs))
cyt                write(2,202) nrec,(zrec(j),j=1,nrd(nrec))
cyt                write(2,202) nsrc,(rkm(j),j=1,nrd(nsrc))
cyt                write(2,202) nth_gbs,(th_gbs(j),j=1,nrd(nth_gbs))
cyt                write(2,202) nb_gbs,(b_gbs(j),j=1,nrd(nb_gbs))
cyt                call check_val_i(iabs(nth_gbs),iabs(nzs),iabs(nzs),
cyt      .            eline,27,nline,'nzs for Gaussian beam source angle',
cyt      .            34,iierr)
cyt                call check_val_i(iabs(nb_gbs),iabs(nzs),iabs(nzs),
cyt      .            eline,27,nline,'nzs for Gaussian beam beamwidth',
cyt      .            31,iierr)
cyt             endif
cyt             call check_val_i(iabs(nzs),1,NSRMAX,eline,27,nline,
cyt      .         'nzs',7,iierr)
cyt             call check_val_i(iabs(nrec),1,NSRMAX,eline,27,nline,
cyt      .         'nrec',7,iierr)
cyt             call check_val_i(iabs(nsrc),1,NRNGMAX,eline,27,nline,
cyt      .         'nsrc',4,iierr)
cyt          elseif(iifft .eq. 2 .or. iiout .eq. 2) then
cyt             iigeom=2
cyt             if(iigbs .eq. 1) then
cyt                print *,'Gaussian Beam Source not implemented for ',
cyt      .            'array geometry file. Setting to zero'
cyt                iigbs=0
cyt             endif
cyt          endif
cyt       else
cyt          call star2(46,nline,2,opt_file,0,iiwrite)
cyt          call star2(46,nline,2,opt_file,0,iiwrite)
cyt       endif
cyt       
cyt       if(iiparm .eq. 1) then
cyt          call star2(46,nline,2,opt_file,0,iiwrite)
cyt c: Make sure we write out nparm lines to output file:
cyt          read(46,*,end=110,err=110) nrun,nparm
cyt          backspace(46)
cyt          backspace(46)
cyt          backspace(2)
cyt          call star2(46,nline,2,opt_file,nparm,iiwrite)
cyt          if(ver_no .lt. 1.5) then
cyt             read(46,*,end=110,err=110) nrun,nparm,(kvar(1,j),
cyt      .         kvar(2,j),kvar(3,j),kvar(4,j),xvar(1,j),xvar(2,j),
cyt      .         j=1,min(NPMAX,iabs(nparm)))
cyt          else
cyt             read(46,*,end=110,err=110) nrun,nparm,rseed,(kvar(1,j),
cyt      .         kvar(2,j),kvar(3,j),kvar(4,j),xvar(1,j),xvar(2,j),
cyt      .         j=1,min(NPMAX,iabs(nparm)))
cyt          endif
cyt          call check_val_i(nrun,-1000,1000,eline,27,nline,
cyt      .      'nrun',4,iierr)
cyt          call check_val_i(nparm,1,NPMAX,eline,27,nline,
cyt      .      'nparm',5,iierr)
cyt       else
cyt          call star2(46,nline,2,opt_file,0,iiwrite)
cyt       endif
cytc
cyt       if(iigeom .eq. 2) then
cyt          call star2(46,nline,2,opt_file,1,iiwrite)
cyt          nzs=1
cyt          read(46,*,end=110,err=110) zsrc(1),nseg,geom_file
cyt          call lname64(geom_file,lgeom)
cyt          call check_val_i(nseg,1,NSEGMAX,eline,27,nline,
cyt      .      'nseg',4,iierr)
cyt          call src_track(nline,eline,iierr)
cyt       else
cyt          call star2(46,nline,2,opt_file,0,iiwrite)
cyt          call star2(46,nline,2,opt_file,0,iiwrite)
cyt       endif
cyt c
      nreal = 1
      nimag = 1
cyt       if(iikpl .ne. 0) then
cyt          call star2(46,nline,2,opt_file,1,iiwrite)
cyt          read(46,*,end=110,err=110) fkpl,iivar,iiform,xkhr1,xkhr2,
cyt      .      nreal,xkhi1,xkhi2,nimag,kduc,iiwr,iishp,iishs
cyt          call check_val_r4(fkpl,1.e-3,1.e10,eline,27,nline,
cyt      .      'f for iikpl=1',11,iierr)
cyt          call check_val_i(iivar,1,3,eline,27,nline,'iivar',5,iierr)
cyt          call check_val_i(iiform,1,3,eline,27,nline,'iikf',6,iierr)
cyt          call check_val_r4(xkhr1,-1.e10,xkhr2,eline,27,nline,
cyt      .      'kr1',5,iierr)
cyt          call check_val_r4(xkhr2,xkhr1,1.e10,eline,27,nline,
cyt      .      'kr2',5,iierr)
cyt          call check_val_i(nreal,1,100000,eline,27,nline,'nkr',3,iierr)
cyt          call check_val_r4(xkhi1,-1.e10,1.e10,eline,27,nline,
cyt      .      'ki1',5,iierr)
cyt          call check_val_r4(xkhi2,-1.e10,1.e10,eline,27,nline,
cyt      .      'ki2',5,iierr)
cyt          call check_val_i(nimag,1,100000,eline,27,nline,'nki',3,iierr)
cyt          call check_val_i(kduc,0,25,eline,27,nline,'nduct',5,iierr)
cyt          call check_val_i(iiwr,1,2,eline,27,nline,'iiph',4,iierr)
cyt          call check_val_i(iishp,-1,1,eline,27,nline,'iishp',5,iierr)
cyt          call check_val_i(iishs,-1,1,eline,27,nline,'iishs',5,iierr)
cyt          f_max=amax1(sngl(f_max),fkpl)
cyt          f_min=amin1(sngl(f_min),fkpl)
cyt       else
cyt          nreal=0
cyt          nimag=0
cyt          call star2(46,nline,2,opt_file,0,iiwrite)
cyt       endif
cyt c
      nfreq = 1
      nang = 1
cyt       if(iirc .eq. 1) then
cyt          call star2(46,nline,2,opt_file,1,iiwrite)
cyt          read(46,*,end=110,err=110) freq1,freq2,nfreq,iilog,
cyt      .      th1,th2,nang
cyt          call check_val_r4(freq1,1.e-3,freq2,eline,27,nline,
cyt      .      'freq1',5,iierr)
cyt          call check_val_r4(freq2,freq1,1.e10,eline,27,nline,
cyt      .      'freq2',5,iierr)
cyt          call check_val_i(nfreq,1,100000,eline,27,nline,
cyt      .      'nfreq',5,iierr)
cyt          call check_val_i(iilog,0,1,eline,27,nline,
cyt      .      'iilog',5,iierr)
cyt          call check_val_r4(th1,0.e0,90.e0,eline,27,nline,
cyt      .      'theta1',5,iierr)
cyt          call check_val_r4(th2,0.e0,90.e0,eline,27,nline,
cyt      .      'theta2',5,iierr)
cyt          call check_val_i(nang,1,10000,eline,27,nline,
cyt      .      'ntheta',6,iierr)
cyt          f_max=amax1(sngl(f_max),freq2)
cyt          f_min=amin1(sngl(f_min),freq1)
cyt       else
cyt          call star2(46,nline,2,opt_file,0,iiwrite)
cyt       endif
cyt c
cyt       if(iirc .eq. 2) then
cyt          call star2(46,nline,2,opt_file,1,iiwrite)
cyt          read(46,*,end=110,err=110) freq1,freq2,fsrc,nfft,
cyt      .      th1,th2,nang
cyt          call check_val_r4(freq1,1.e-3,freq2,eline,27,nline,
cyt      .      'freq1',5,iierr)
cyt          call check_val_r4(freq2,freq1,fsrc/2.e0,eline,27,nline,
cyt      .      'freq2',5,iierr)
cyt          call check_val_r4(fsrc,1.e-3,1.e10,eline,27,nline,
cyt      .      'fsrc',4,iierr)
cyt          call check_val_i(nfft,32,131072,eline,27,nline,
cyt      .      'nfft',5,iierr)
cyt          call check_val_r4(th1,0.e0,90.e0,eline,27,nline,
cyt      .      'theta1',5,iierr)
cyt          call check_val_r4(th2,0.e0,90.e0,eline,27,nline,
cyt      .      'theta2',5,iierr)
cyt          call check_val_i(nang,1,10000,eline,27,nline,
cyt      .      'ntheta',6,iierr)
cyt          f_max=amax1(sngl(f_max),freq2)
cyt          f_min=amin1(sngl(f_min),freq1)
cyt       else
cyt          call star2(46,nline,2,opt_file,0,iiwrite)
cyt       endif
cyt c
cyt       if(iigeom .eq. 2) call arr_geom(iierr)
cyt c
cyt       if(iierr .eq. 1) then
cyt          print *,' '
cyt          print *,'Execution terminating.  Check input option file '//
cyt      .      'for error(s).'
cyt          stop
cyt       endif
      return
cyt c
cyt 110   print *,'Error opening or reading option file ',opt_file(1:lopt)
cyt       print *,'*() line number = ',nline
cyt       stop
cyt c
      end
cyt ccc
cyt       subroutine src_track(nline,eline,iierr)
cyt c
cyt       use Parms_com
cyt       use i_o_com
cyt       use gen_com
cyt       integer*4 nline,iierr,nrden,kr,j
cyt       real*4 r0,tlast,cosphi,sinphi,pierad,fac,xxx0,yyy0,x1x,y1x,
cyt      .   x2x,y2x,delx,dely,delr,delt,time0
cyt       character*64 eline
cyt       data pierad/0.01745329251994/
cyt c
cyt       call star2(46,nline,2,opt_file,nseg,iiwrite)
cyt       nsrc=0
cyt       do j=1,nseg
cyt          read(46,*,end=110,err=110) iitype(j),iicont(j),
cyt      .      vs(j),t1(j),t2(j),dt(j),cpa(j),phid(j),x2(j),y2(j)
cyt          call check_val_i(iitype(j),1,2,eline,27,nline,
cyt      .      'iitype',6,iierr)
cyt          call check_val_i(iicont(j),0,1,eline,27,nline,
cyt      .      'iicont',6,iierr)
cyt cxx      if(iimet .eq. 0) then
cyt cxx         vs(j)=.51480*vs(j)
cyt cxx         cpa(j)=1.8520*cpa(j)
cyt cxx         if(iitype(j) .eq. 2) phid(j)=1.8520*phid(j)
cyt cxx         x2(j)=1.8520*x2(j)
cyt cxx         y2(j)=1.8520*y2(j)
cyt cxx      endif
cyt c: For type=2 and vs not zero, set t2 according to dist traveled and velocity:
cyt          if(vs(j) .ne. 0 .and. iitype(j) .eq. 2) then
cyt             t2(j)=t1(j) + 1000.*sqrt((x2(j)-cpa(j))**2 +
cyt      .         (y2(j)-phid(j))**2)/(vs(j)*60.)
cyt          endif
cyt          if(dt(j) .lt. 0.) then
cyt             nrleg(j)=iabs(nint(dt(j)))
cyt             if(nrleg(j) - abs(dt(j)) .ne. 0.) then
cyt                print *,'NPT on source track must be integer: ',j,dt(j)
cyt                stop
cyt             endif
cyt          else
cyt             nrleg(j)=nint((t2(j) - t1(j))/dt(j)) + 1
cyt          endif
cyt          if((iicont(j) .eq. 0 .and. nrleg(j) .lt. 1) .or.
cyt      .      (iicont(j) .eq. 1 .and. nrleg(j) .lt. 2)) then
cyt             print *,'NPT for iicont=0 leg must be > 0, ',
cyt      .         'NPT for iicont=1 leg must be > 1: ',j,dt(j),nrleg(j)
cyt             stop
cyt          endif
cyt          nsrc=nsrc + nrleg(j)
cyt          if(iicont(j) .eq. 1) nsrc=nsrc - 1
cyt       enddo
cyt       iicont(1)=0
cyt       call mem_lim(nsrc,NRNGMAX,MLINE,LML,'nsrc',4,'NRNGMAX',7,1,1)
cyt c
cyt c: Compute source track positions:
cyt       r0=0.
cyt       tlast=0.
cyt       nsrc=0
cyt       do j=1,nseg
cyt          if(iitype(j) .eq. 1) then
cyt             cosphi=cos(phid(j)*pierad)
cyt             sinphi=sin(phid(j)*pierad)
cyt c: for vs=0, times are distances; for vs not 0, convert to min to km
cyt             fac=1.
cyt             if(vs(j) .ne. 0.) fac=vs(j)*60./1000.
cyt             if(iicont(j) .eq. 0) then
cyt c: xxx0,yyy0 are (x,y) source coordinates at cpa:
cyt c: Change phid to be in deg E of N, rather than from x- to y- axis,
cyt c: +cpa means a clockwise track, -cpa means a ccw track:
cyt                xxx0=-cpa(j)*cosphi
cyt                yyy0=cpa(j)*sinphi
cyt                x1x=xxx0 + fac*t1(j)*sinphi
cyt                y1x=yyy0 + fac*t1(j)*cosphi
cyt                x2x=xxx0 + fac*t2(j)*sinphi
cyt                y2x=yyy0 + fac*t2(j)*cosphi
cyt                time0=t1(j)
cyt             else
cyt c: begin at kr=2 for continuous legs so there is no overlap of points:
cyt                x1x=x2x
cyt                y1x=y2x
cyt c: Change phid to be in deg E of N, rather than from x- to y- axis:
cyt                x2x=x1x + fac*(t2(j)-t1(j))*sinphi
cyt                y2x=y1x + fac*(t2(j)-t1(j))*cosphi
cyt                time0=tlast
cyt             endif
cyt          else
cyt             if(iicont(j) .eq. 0) then
cyt                x1x=cpa(j)
cyt                y1x=phid(j)
cyt                time0=t1(j)
cyt             else
cyt                x1x=x2x
cyt                y1x=y2x
cyt                time0=tlast
cyt             endif
cyt             x2x=x2(j)
cyt             y2x=y2(j)
cyt          endif
cyt          nrden=max(1,nrleg(j) - 1)
cyt          delx=(x2x - x1x)/nrden
cyt          dely=(y2x - y1x)/nrden
cyt          delr=sqrt(delx**2 + dely**2)
cyt          delt=(t2(j) - t1(j))/nrden
cyt          do kr=iicont(j)+1,nrleg(j)
cyt             nsrc=nsrc + 1
cyt             xsrc(nsrc)=1000.*(x1x + (kr-1)*delx)
cyt             ysrc(nsrc)=1000.*(y1x + (kr-1)*dely)
cyt c: compute source track distance:
cyt cxx         rangx(nsrc)=r0 + 1000.*(kr-1)*delr
cyt c: t_src is the current time (or distance along track for vs=0):
cyt             t_src(nsrc)=time0 + (kr-1)*delt
cyt          enddo
cyt          tlast=t_src(nsrc)
cyt cxx      r0=rangx(nsrc)
cyt       enddo
cyt cxx   write(2,218)
cyt 218   format('### SOURCE TRACK')
cyt cxx   do j=1,nsrc
cyt cxx      write(2,220) .001*xsrc(j),.001*ysrc(j)
cyt 220      format(f8.3,2x,f8.3)
cyt cxx   enddo
cyt c
cyt       return
cyt 110   print *,'Error opening or reading option file ',opt_file(1:lopt)
cyt       print *,'*() line number = ',nline
cyt       stop
cyt c
cyt       end
cyt ccc
cyt       subroutine arr_geom(iierr)
cyt c
cyt c: Reads in the receiver array geometry.
cyt c
cyt       use Parms_com
cyt       use i_o_com
cyt       use gen_com
cyt       integer*4 iierr,nline,jz,jy,jx,jrec,iiarr,nx,ny,nz,j,nxy,nrd
cyt       real*4 zrj,zr1,yr,xr,dx,dy,dz
cyt       character*64 eline
cyt       data eline/'INVALID INPUT IN ARRAY FILE: '/
cyt c
cyt       write(2,95)
cyt 95    format(/'### RECEIVER ARRAY GEOMETRY FILE INFORMATION ###')
cyt       open(62,file=geom_file(1:lgeom),form='formatted',
cyt      .   status='old',err=99)
cyt       nline=0
cyt       call star2(62,nline,2,geom_file,1,iiwrite)
cyt       read(62,*) iiarr
cyt       call check_val_i(iiarr,1,2,eline,29,nline,'iiarr',5,iierr)
cyt       nrec=0
cyt       if(iiarr .eq. 1) then
cyt          call star2(62,nline,2,geom_file,1,iiwrite)
cyt          read(62,*) zr1,nx,ny,nz,dx,dy,dz
cyt          do 110 jz=1,nz
cyt             zrj=zr1 + (jz-1)*dz
cyt             do 110 jy=1,ny
cyt                yr=(jy-1)*dy
cyt                do 110 jx=1,nx
cyt                   xr=(jx-1)*dx
cyt                   nrec=nrec + 1
cyt                   xrec(nrec)=xr
cyt                   yrec(nrec)=yr
cyt                   zrec(nrec)=zrj
cyt 110      continue
cyt          call star2(62,nline,2,geom_file,0,iiwrite)
cyt          call star2(62,nline,2,geom_file,0,iiwrite)
cyt       else
cyt          call star2(62,nline,2,geom_file,0,iiwrite)
cyt          call star2(62,nline,2,geom_file,1,iiwrite)
cyt          read(62,*) nz,(zsr(j),j=1,nrd(nz))
cyt          call uni_space(nz,zsr,1.e0)
cyt          call star2(62,nline,2,geom_file,0,iiwrite)
cyt          do 112 jz=1,iabs(nz)
cyt             read(62,*) nxy,(xrec(j),yrec(j),j=nrec+1,nrec+nxy)
cyt             write(2,206) nxy,(xrec(j),yrec(j),j=nrec+1,nrec+1)
cyt 206         format(i8,f8.1,',',f8.1)
cyt             do j=nrec+2,nrec+nxy
cyt                write(2,207) xrec(j),yrec(j)
cyt 207            format(8x,f8.1,',',f8.1)
cyt             enddo
cyt             do jrec=nrec+1,nrec+nxy
cyt                zrec(jrec)=zsr(jz)
cyt             enddo
cyt             nrec=nrec + nxy
cyt 112      continue
cyt       endif
cyt c: Receiver label for TL plots for source track option:
cyt       do j=1,nrec
cyt          rec_lab(j)=j
cyt       enddo
cyt       close(62)
cyt c
cyt       return
cyt 99    print *,'Error opening array geometry file ',geom_file
cyt       stop
cyt       end
cyt ccc
cyt       subroutine star2(nf,nline,nfout,fname,necho,iiwrite)
cyt c
cyt c: Searches for the next line of an input file that begins
cyt c: with the key symbol '*', which indicates that the next
cyt c: line contains data.
cyt c
cyt       implicit none
cyt       integer*4 nf,nline,nfout,necho,iiwrite,j,lchfs
cyt       character*1 chstar
cyt       character*64 fname
cyt       character*80 chfs
cyt       data chstar/'*'/
cyt c
cyt       nline=nline + 1
cyt 10    read(nf,100,end=400,err=500) chfs(1:80)
cyt 100   format(a80)
cyt       if(chfs(1:1) .ne. chstar) goto 10
cyt       call lname80(chfs,lchfs)
cyt       if(iiwrite .ne. 0) then
cyt          write(nfout,110) chfs(1:lchfs)
cyt 110      format(a)
cyt          do j=1,necho
cyt             read(nf,102,end=390,err=390) chfs(1:80)
cyt 102         format(a80)
cyt             call lname80(chfs,lchfs)
cyt             write(nfout,110) chfs(1:lchfs)
cyt          enddo
cyt          do j=1,necho
cyt             backspace(nf)
cyt          enddo
cyt       endif
cyt c
cyt 390   return
cyt 400   continue
cyt       print *,'search for * reached end of file for line # ',nline,
cyt      .   '; file ',fname
cyt       stop
cyt 500   continue
cyt       print *,'search for * encountered end of file or error for '//
cyt      .   'line # ',nline,' in input file ',fname
cyt       stop
cyt       end
cyt ccc
cyt       subroutine check_val_i(val,val_lo,val_hi,eline,le,nline,
cyt      .   vname,lv,iierr)
cyt c
cyt c: Checks integer input val to see if it is in the range of allowable 
cyt c: values, val_lo to val_hi.
cyt c
cyt       implicit none
cyt       integer*4 val,val_lo,val_hi,nline,le,lv,iierr
cyt       character*64 eline,vname
cyt c
cyt       if(val .lt. val_lo .or. val .gt. val_hi) then
cyt          iierr=1
cyt          print *,' '
cyt          print *,eline(1:le),' LINE # ',nline
cyt          print *,'VARIABLE NAME = ',vname(1:lv),'; VALID RANGE = ',
cyt      .      val_lo,' ,',val_hi
cyt          print *,'ENTERED VALUE = ',val
cyt       endif
cyt c
cyt       return
cyt       end
cyt ccc
cyt      subroutine check_val_r4(val,val_lo,val_hi,eline,le,nline,
cyt     .   vname,lv,iierr)
cytc
cytc: Checks real*8 input val to see if it is in the range of allowable 
cytc: values, val_lo to val_hi.
cytc
cyt      implicit none
cyt      real*4 val,val_lo,val_hi
cyt      integer*4 nline,le,lv,iierr
cyt      character*64 eline,vname
cytc
cyt      if(val .lt. val_lo .or. val .gt. val_hi) then
cyt         iierr=1
cyt         print *,' '
cyt         print *,eline(1:le),' LINE # ',nline
cyt         print *,'VARIABLE NAME = ',vname(1:lv),'; VALID RANGE = ',
cyt     .      val_lo,' ,',val_hi
cyt         print *,'ENTERED VALUE = ',val
cyt      endif
cytc
cyt      return
cyt      end
cytccc
cyt      subroutine check_val_r8(val,val_lo,val_hi,eline,le,nline,
cyt     .   vname,lv,iierr)
cytc
cytc: Checks real*8 input val to see if it is in the range of allowable 
cytc: values, val_lo to val_hi.
cytc
cyt      implicit none
cyt      real*8 val,val_lo,val_hi
cyt      integer*4 nline,le,lv,iierr
cyt      character*64 eline,vname
cytc
cyt      if(val .lt. val_lo .or. val .gt. val_hi) then
cyt         iierr=1
cyt         print *,' '
cyt         print *,eline(1:le),' LINE # ',nline
cyt         print *,'VARIABLE NAME = ',vname(1:lv),'; VALID RANGE = ',
cyt     .      val_lo,' ,',val_hi
cyt         print *,'ENTERED VALUE = ',val
cyt      endif
cytc
cyt      return
cyt      end
ccc
      function nrd(n)
c
      implicit none
      integer*4 nrd,n
c
      if(n .ge. 0) then
         nrd=n
      else
         nrd=2
      endif
c
      return
      end
