module scairy_com
implicit none
save
!     common /airy_complex/ eye,ei13,ei23,ei16,ei56,eim13,eim23,
!    .   eim16,eim56,det_bi,det_pos,det_neg
!     common /airy_real/ pie23,pie_x,pie_inv,sqrt3
!     complex*16 eye,ei13,ei23,ei16,ei56,eim13,eim23,eim16,eim56,
!    .   det_bi,det_pos,det_neg
!     real*8 pie23,pie_x,pie_inv,sqrt3
complex*16 :: eye,ei13,ei23,ei16,ei56,eim13,eim23,eim16,eim56,&
	det_bi,det_pos,det_neg
real*8 :: pie23,pie_x,pie_inv,sqrt3
end module scairy_com
