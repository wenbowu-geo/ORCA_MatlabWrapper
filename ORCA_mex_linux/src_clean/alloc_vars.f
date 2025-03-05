      subroutine alloc_vars
c
      use Parms_com
      use i_o_com
      use gen_com
c
      NTLMAX=max(abs(nsrc*nrec),abs(nreal*nimag),abs(nang*nfreq))
cc    print *,'NTLMAX: ',NTLMAX,nsrc,nrec,nreal,nimag
cyt       allocate(tl(NTLMAX))
cyt       allocate(tli(NTLMAX))
cyt       allocate(tlc(NTLMAX))
      NHDFMAX=max(NM_MAX,abs(nsrc*nrec),abs(nreal*nimag),n_env+10,
     .   abs(nang*nfreq))
      allocate(r4mat1(NHDFMAX))
      allocate(r4mat2(NHDFMAX))
c
      return
      end
