      subroutine r1r2_calc(k,r1r2,ndv,iiw)
c
c: Computes the product of the downlooking (rc1)  and uplooking (rc2)
c: reflection coefficients at the horizontal wavenumber k, and (if ndv>0) 
c: Also computes deriviates with respect to k (for ndv>=2) and with respect
c: to w (for ndv=3).
c: Output: r1r2(3,4), where 
c:    r1r2(1:3,1)=R1 and its two derivatives,
c:    r1r2(1:3,2)=R2 and its two derivatives,
c:    r1r2(1:3,3)=R1*R2 and its two derivatives,
c:    r1r2(1:3,4)=ln(R1*R2) and its two derivatives,
c
      use Parms_com
      use gen_com
      integer ndv,iiw,j
      complex*16 k,r1r2(3,4)
c
      nctot=nctot + 1
      xkh=k
      phtot=0.
      iicut=0
      call xkh_init(ndv)
      call rp_calc(1,r1r2(1,1),ndv,iiw)
      call rp_calc(2,r1r2(1,2),ndv,iiw)
c
      r1r2(1,3)=r1r2(1,1)*r1r2(1,2)
      r1r2(1,4)=cdlog(r1r2(1,3))
      do j=2,ndv
         r1r2(j,3)=r1r2(1,1)*r1r2(j,2) + r1r2(j,1)*r1r2(1,2)
         r1r2(j,4)=r1r2(j,3)/r1r2(1,3)
      enddo
      if(iicut .eq. 1) lncut=r1r2(1,4)
c
      return
      end
