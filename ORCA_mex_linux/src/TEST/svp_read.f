      subroutine svp_read(mex_in_uphalf_cp,mex_in_uphalf_cs,
     & mex_in_uphalf_rho,mex_in_uphalf_ap,mex_in_uphalf_as,
     & mex_in_nsvp, mex_in_ctol,
     & mex_in_wssp, mex_in_wrho,mex_in_walphs,
     & mex_in_nlayb,
     & mex_in_btm_env,
     & mex_in_lowhalf_cp,mex_in_lowhalf_cs,mex_in_lowhalf_rho,
     & mex_in_lowhalf_ap,mex_in_lowhalf_as,
     & mex_in_ntop,
     & mex_in_above_sea)
c
      use Parms_com
      use i_o_com
c
      real*8 mex_in_uphalf_cp,mex_in_uphalf_cs,mex_in_uphalf_rho,
     & mex_in_uphalf_ap,mex_in_uphalf_as
      integer*4 mex_in_total_layers, mex_in_nsvp
      real*8 mex_in_ctol
      real*8 mex_in_wrho,mex_in_walphs,mex_in_wssp(mex_in_nsvp,2)
      integer*4 mex_in_nlayb
      real*8 mex_in_btm_env(mex_in_nlayb,16)
      real*8 mex_in_lowhalf_cp,mex_in_lowhalf_cs,mex_in_lowhalf_rho,
     & mex_in_lowhalf_ap,mex_in_lowhalf_as
      integer*4 mex_in_ntop
      real*8 mex_in_above_sea(mex_in_ntop,16)
c
c: Local variables:
cyt      integer*4 nline,j,j1,j2,iiblug(-4:4),iierr,ndel
      integer*4 j,j1,j2,iiblug(-4:4),ndel
      real*8 zdel(5*NLMAX)
cyt      character*64 eline
cyt      data eline/'INVALID INPUT IN SVP FILE: '/
c: For ktb,ktt<0, read in two more values (p- and s-wave attn freq exponents):
      data iiblug/4,4,4,2,0,0,2,2,2/
c
cyt      if(iiwrite .ne. 0) write(2,95)
cyt95    format(/'### SVP FILE INFORMATION ###')
cyt      open(10,file=svp_file(1:lsvp),err=500) 
cyt      rewind(10)
c
cyt      iierr=0
cyt      nline=0
cyt      call star2(10,nline,2,svp_file,1,iiwrite)
cyt      read(10,*,end=510,err=508) svp_ver,svp_title
cyt      call check_val_r4(svp_ver,0.9e0,2.0e0,eline,27,nline,
cyt     .   'SVP version',11,iierr)
490   continue
c
c: Upper halfspace:
      j1=1
cyt      call star2(10,nline,2,svp_file,1,iiwrite)
cyt      read(10,*,end=510,err=510) (geot(2,j,j1),j=1,5)
      geot(2,1,j1) = mex_in_uphalf_cp
      geot(2,2,j1) = mex_in_uphalf_cs
      geot(2,3,j1) = mex_in_uphalf_rho
      geot(2,4,j1) = mex_in_uphalf_ap
      geot(2,5,j1) = mex_in_uphalf_as
cyt      
      if(geot(2,1,j1) .gt. 0.d0) then
c: c_hsp < 0 means use previous layer as halfspace:
         ht(j1)=1.d+20
         do j=1,5
            geot(1,j,j1)=geot(2,j,j1)
         enddo
cyt         call svp_check_val_lay(2,0.d0,geot(1,1,j1),'upper h-space',13,
cyt     .      eline,nline,iierr)
      endif
c
c: Sound speed profile in ocean:
cyt      call star2(10,nline,2,svp_file,1,iiwrite)
cyt      if(svp_ver .gt. 0.9d0) then
cyt         read(10,*,end=510,err=510) nsvp,ctol
      nsvp = mex_in_nsvp
      ctol = mex_in_ctol
cyt         call check_val_r8(ctol,0.d0,50.d0,eline,27,nline,'ctol',
cyt     .      4,iierr)
cyt      else
cyt         read(10,*,end=510,err=510) nsvp
cyt         ctol=0.d0
cyt      endif
cyt      call check_val_i(nsvp,2,5*NLMAX,eline,27,nline,'nsvp',4,iierr)
c
cyt      call star2(10,nline,2,svp_file,nsvp,iiwrite)
cyt      read(10,*,end=510,err=510) zsvp(1),csvp(1),rho_svp,alpha_svp
      zsvp(1) = mex_in_wssp(1,1)
      csvp(1) = mex_in_wssp(1,2)
      rho_svp = mex_in_wrho
      alpha_svp = mex_in_walphs
cyt      call check_val_r8(zsvp(1),0.d0,0.d0,eline,27,nline,
cyt     .   'zsvp(1) MUST BE 0',17,iierr)
cyt      call check_val_r8(csvp(1),1.d-10,1.d+10,eline,27,nline,
cyt     .   'csvp(1)',7,iierr)
cyt      call check_val_r8(rho_svp,1.d-100,1.d+100,eline,27,nline,
cyt     .   'rho_svp on first line',21,iierr)
      do j=2,nsvp
cyt         read(10,*,end=510,err=510) zsvp(j),csvp(j)
      zsvp(j) = mex_in_wssp(j,1)
      csvp(j) = mex_in_wssp(j,2)      
cyt         call check_val_r8(zsvp(j),zsvp(j-1),1.d+10,eline,27,nline,
cyt     .      'zsvp(j)',7,iierr)
cyt         call check_val_r8(csvp(j),1.d-10,1.d+10,eline,27,nline,
cyt     .      'csvp(j)',7,iierr)
      enddo
c
c: Check if the number of layers in ocean SVP can be reduced:
      ndel=0
      if(ctol .gt. 0.d0) then
         call svp_fit(nsvp,zsvp,csvp,ctol,ndel,zdel)
      endif
c
c: Bottom layering:
cyt      call star2(10,nline,2,svp_file,1,iiwrite)
cyt      read(10,*,end=510,err=510) nlayb
      nlayb = mex_in_nlayb
cyt      call check_val_i(nlayb+1,1,NLMAX,eline,27,nline,'nlayb+1',
cyt     .   7,iierr)
cyt      call star2(10,nline,2,svp_file,nlayb,iiwrite)
      do j=1,nlayb
cyt         read(10,*,end=510,err=510) ktb(j),hb(j),((geob(j1,j2,j),
cyt     .      j1=1,2),j2=1,5),(bpb(j1,j),j1=1,iiblug(ktb(j)))
         ktb(j) = int(mex_in_btm_env(j,1),4)
         hb(j) = mex_in_btm_env(j,2)
         do j2 = 1,5
            do j1 = 1,2
               geob(j1,j2,j) = mex_in_btm_env(j,2+(j2-1)*2+j1)
            enddo
         enddo
         do j1 = 1,iiblug(ktb(j))
            bpb(j1,j) = mex_in_btm_env(j,12+j1)
         enddo
cyt         
         if(j .eq. 1) then
c: Check for negative h, meaning two-way travel time, and negative c,
c: meaning csed/cwater ratio:
            if(geob(1,1,1) .lt. 0.d0) then
               cs_cw_rat=-geob(1,1,1)
               geob(1,1,1)=cs_cw_rat*csvp(nsvp)
cyt      print *,'cb(1) = ',geob(1,1,1)
            endif
            if(hb(1) .lt. 0.d0) then
c: Nominal average gradient (since we don't know it for sure):
               gbar=0.75
               tau_2way=-hb(1)
               hb(1)=geob(1,1,1)*(exp(gbar*tau_2way/2.) - 1.)/gbar
cyt      print *,'hb(1) = ',hb(1)
            endif
         endif
cyt         call check_val_i(ktb(j),-4,4,eline,27,nline,'Profile Type',12,
cyt     .      iierr)
cyt         if(iabs(ktb(j)) .gt. 1) then
cyt            call check_val_r8(bpb(2,j),.1d0,20.d0,eline,27,nline,
cyt     .         'ctol for blug layer',19,iierr)
cyt         endif
cyt         call svp_check_val_lay(1,hb(j),geob(1,1,j),'bottom layer',12,
cyt     .      eline,nline,iierr)
cyt         call svp_check_val_lay(2,hb(j),geob(1,1,j),'bottom layer',12,
cyt     .      eline,nline,iierr)
         call zero_sh(geob(1,2,j),geob(1,5,j),j,'top   ','bottom')
         call zero_sh(geob(2,2,j),geob(2,5,j),j,'bottom','bottom')
      enddo
c
c: Lower halfspace:
      j1=nlayb+1
cyt      call star2(10,nline,2,svp_file,1,iiwrite)
cyt      read(10,*,end=510,err=510) (geob(1,j,j1),j=1,5)
      geob(1,1,j1) = mex_in_lowhalf_cp
      geob(1,2,j1) = mex_in_lowhalf_cs
      geob(1,3,j1) = mex_in_lowhalf_rho
      geob(1,4,j1) = mex_in_lowhalf_ap
      geob(1,5,j1) = mex_in_lowhalf_as
cyt
      if(geob(1,1,j1) .gt. 0.d0) then
c: c_hsp < 0 means use previous layer as halfspace:
c: Set thickness of halfspaces to large numbers (for use in zmx_init):
         hb(j1)=1.d+20
         do j=1,5
            geob(2,j,j1)=geob(1,j,j1)
         enddo
cyt         call svp_check_val_lay(1,0.d0,geob(1,1,j1),'lower h-space',
cyt     .      13,eline,nline,iierr)
         call zero_sh(geob(1,2,j1),geob(1,5,j1),j1,'top   ','bottom')
      endif
c
c: Layering above ocean:
c: NOTE: TOP LAYERS ASSUMED TO BE GIVEN FROM UPPER HALFSPACE TO OCEAN.
cyt      call star2(10,nline,2,svp_file,1,iiwrite)
cyt      read(10,*,end=510,err=510) nlayt
      nlayt = mex_in_ntop
cyt      call check_val_i(nlayt+1,1,NLMAX,eline,27,nline,'nlayt+1',
cyt     .   7,iierr)
cyt      call star2(10,nline,2,svp_file,nlayt,iiwrite)
      do j=2,nlayt+1
cyt         read(10,*,end=510,err=510) ktt(j),ht(j),((geot(j1,j2,j),
cyt     .      j1=1,2),j2=1,5),(bpt(j1,j),j1=1,iiblug(ktt(j)))
         ktt(j) = int(mex_in_above_sea(j-1,1),4)
         ht(j) = mex_in_above_sea(j-1,2)
         do j2 = 1,5
            do j1 = 1,2
               geot(j1,j2,j) = mex_in_above_sea(j-1,2+(j2-1)*2+j1)
            enddo
         enddo
         do j1 = 1,iiblug(ktt(j))
            bpt(j1,j) = mex_in_above_sea(j-1,12+j1)
         enddo
cyt         
cyt         call check_val_i(ktt(j),-4,4,eline,27,nline,'Profile Type',12,
cyt     .      iierr)
cyt         if(iabs(ktt(j)) .gt. 1) then
cyt            call check_val_r8(bpt(2,j),.1d0,20.d0,eline,27,nline,
cyt     .         'ctol for blug layer',19,iierr)
cyt         endif
cyt         call svp_check_val_lay(1,ht(j),geot(1,1,j),'top layer',9,
cyt     .      eline,nline,iierr)
cyt         call svp_check_val_lay(2,ht(j),geot(1,1,j),'top layer',9,
cyt     .      eline,nline,iierr)
         call zero_sh(geot(1,2,j),geot(1,5,j),j,'top   ','top   ')
         call zero_sh(geot(2,2,j),geot(2,5,j),j,'bottom','top   ')
      enddo
cyt      close(10)
c
      nlay=nsvp+nlayb+1+nlayt+1
cyt      call check_val_i(nlay,2,NLMAX,eline,27,nline,'nlay',4,iierr)
c
cyt      if(iierr .ne. 0) stop
c
cyt      if(ndel .gt. 0 .and. iiwrite .ne. 0) then
cyt         write(2,410) ndel,ndel+nsvp,(zdel(j),j=1,ndel)
cyt410      format(/'== CTOL ALLOWED DELETION OF',i4,' OUT OF',i4,
cyt     .      ' SVP LAYERS.'/'== DEPTHS DELETED = ',7(f7.2,1x)/
cyt     .      40(3x,9(1x,f7.2)/))
cyt      endif
c
      return
cyt500   print *,'Error opening SVP file ',svp_file
cyt      stop
cytc: Old SVP format with version number:
cyt508   svp_ver=0.9
cyt      backspace(10)
cyt      read(10,200,end=510,err=510) svp_title
cyt200   format(a)
cyt      goto 490
cyt 510   print *,'Endo or error reading SVP file ',svp_file,
cyt     .   ' at line ',nline
cyt      stop
      end
ccc
      subroutine zero_sh(cs,as,j,ch_tb1,ch_tb2)
c
      implicit none
      real*8 cs,as
      integer*4 j
      character*6 ch_tb1,ch_tb2
c
      if(cs .eq. 0.d0 .and. as .ne. 0.d0) then
cyt         write(2,125) ch_tb1,ch_tb2,j
cyt125      format('/*** SHEAR ATTENUATION SET TO 0 for 0 shear speed at ',
cyt     .         a6,' of ',a6,' layer # ',i3,'***'/)
         as=0.d0
      endif
c
      return
      end
ccc
      subroutine svp_fit(nsvp,zsvp,csvp,ctol,ndel,zdel)
c
      implicit none
      integer*4 nsvp,j1,j2,j,jj,ndel,nd
      real*8 zsvp(nsvp),csvp(nsvp),ctol,zdel(nsvp),c1,c1_2,c2,cfit,gfac
c
      j1=1
      c1=csvp(j1)
      c1_2=1/(c1*c1)
15    continue
      j2=j1 + 2
      if(j2 .gt. nsvp) return
20    c2=csvp(j2)
      if(c1 .ne. c2) then
         gfac=(1/(c2*c2) - c1_2)/(zsvp(j2) - zsvp(j1))
      else
         cfit=c1
      endif
      do j=j1+1,j2-1
         if(c1 .ne. c2) then
            cfit=1.d0/sqrt(c1_2 + gfac*(zsvp(j) - zsvp(j1)))
         endif
c: Check fit:
         if(abs(cfit-csvp(j)) .gt. ctol) then
c: Fit bad, so check if any points can be deleted:
            nd=j2-j1-2
            if(nd .gt. 0) then
               do jj=j1+1,j2-2
                  ndel=ndel + 1
                  zdel(ndel)=zsvp(jj)
               enddo
               do jj=j1+1,nsvp-nd
                  csvp(jj)=csvp(jj+nd)
                  zsvp(jj)=zsvp(jj+nd)
               enddo
               nsvp=nsvp - nd
               j2=j2 - nd
            endif
            if(j2 .eq. nsvp) return
            j1=j2-1
            c1=csvp(j1)
            c1_2=1/(c1*c1)
            goto 15
         endif
      enddo
      if(j2 .lt. nsvp) then
         j2=j2 + 1
         goto 20
      else
         nd=j2-j1-1
         do jj=j1+1,j2-1
            ndel=ndel + 1
            zdel(ndel)=zsvp(jj)
         enddo
         do jj=j1+1,nsvp-nd
            csvp(jj)=csvp(jj+nd)
            zsvp(jj)=zsvp(jj+nd)
         enddo
         nsvp=nsvp - nd
         return
      endif
c
      end
ccc
cyt      subroutine svp_check_val_lay(ii,h,geo,char_lay,nch,eline,nline,
cyt     .   iierr)
cytc
cyt      implicit none
cyt      real*8 h,geo(2,5)
cyt      integer*4 ii,nch,nline,iierr
cyt      character*64 char_lay,eline
cytc
cytc: Make negative h mean two-way travel time:
cyt      call check_val_r8(h,-10.0d0,1.5d4,eline,27,nline,
cyt     .   'h '//char_lay(1:nch),2+nch,iierr)
cytc: Make negative c mean sound speed ratio:
cyt      call check_val_r8(geo(ii,1),-10.d0,1.d+10,eline,27,nline,
cyt     .   'cp '//char_lay(1:nch),3+nch,iierr)
cyt      call check_val_r8(geo(ii,2),0.0d0,1.d+10,eline,27,nline,
cyt     .   'cs '//char_lay(1:nch),3+nch,iierr)
cyt      call check_val_r8(geo(ii,3),1.d-50,1.d+50,eline,27,nline,
cyt     .   'rho '//char_lay(1:nch),4+nch,iierr)
cyt      call check_val_r8(geo(ii,4),-200.d0,999.d0,eline,27,nline,
cyt     .   'ap '//char_lay(1:nch),3+nch,iierr)
cyt      call check_val_r8(geo(ii,5),-200.d0,200.d0,eline,27,nline,
cyt     .   'as '//char_lay(1:nch),3+nch,iierr)
cytc
cyt      return
cyt      end
