      subroutine cub_root_new(c1,c2,c3,c4,xhit,kcub)
c
c: Finds the root, xhit, between 0 and 1 of the
c: polynomial f(x)=c1 + c2*x + c3*x**2 + c4*x**4.
c
      implicit none
      integer*4 kcub
      real*8 c1,c2,c3,c4,xhit,a1,a2,a3,a1sq,q,r,
     .   dd,rdd,rq,arg,th,pietth,piefth,one_third
      data pietth/2.09439510239320/,piefth/4.18879020478639/,
     .   one_third/0.33333333333333/
c
      kcub=1
      a1=c3/c4
      a2=c2/c4
      a3=c1/c4
      a1sq=a1*a1
      q=(3.d0*a2 - a1sq)/9.
      r=(9.d0*a1*a2 - 27.d0*a3 - 2.d0*a1*a1sq)/54.d0
      dd=q**3 + r**2
      if(dd .ge. 0.d0) then
         rdd=sqrt(dd)
         xhit=dsign(1.d0,r+rdd)*abs(r+rdd)**(one_third) +
     .      dsign(1.d0,r-rdd)*abs(r-rdd)**(one_third) - a1*one_third
      else
         rq=sqrt(-q)
         arg=r/rq**3
         if((arg .lt. -1.d0) .or. (arg .gt. 1.d0)) then
            xhit=0.5d0
            kcub=0
            return
         endif
         th=acos(arg)/3.d0
         xhit=2.d0*rq*cos(th) - a1*one_third
         if((xhit .lt. 0.d0) .or. (xhit .gt. 1.d0)) then
            xhit=2.d0*rq*cos(th + pietth) - a1*one_third
            if((xhit .lt. 0.d0) .or. (xhit .gt. 1.d0)) then 
               xhit=2.d0*rq*cos(th + piefth) - a1*one_third
            endif
         endif
      endif
      if((xhit .le. 0.d0) .or. (xhit .ge. 1.d0)) then
         xhit=.5d0
         kcub=0
      endif
c     if(kcub .eq. 0) print *,'kcub=0 in cubroot' 
c
      return
      end
