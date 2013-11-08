      subroutine getval(indf,ic,jc,kc, dimr, r, &
                       ijkc,m1c,m2c,m3c,m12c, osch, m, i1, val)
	implicit none

	integer indf,ic,jc,kc, dimr, ijkc,m1c,m2c, m3c, m12c, osch, m, i1
      DOUBLE PRECISION r(dimr), val(8,2*osch+1)

	integer ideb(8), ii, idec, ind
      integer dimd, ijkc0


      if(indf.eq.1) then
	    dimd=1
	    m=m1c
	    i1 = ic
      endif

	if(indf.eq.2) then
	    dimd=1
	    m=m1c
	    i1 = ic -1
      endif

      if(indf.eq.3) then
	    dimd=m1c
	    m=m2c
	    i1 = jc
      endif

      if(indf.eq.4) then
	    dimd=m1c
	    m=m2c
	    i1 = jc-1
      endif

      if(indf.eq.5) then
	    dimd=m12c
	    m=m3c
	    i1 = kc
      endif

      if(indf.eq.6) then
	    dimd=m12c
	    m=m3c
	    i1 = kc-1
      endif


	idec = osch*dimd
	ijkc0 = ijkc

	ideb(1)=ijkc0 - idec
	ideb(2)=ijkc0 +1 - idec
	ideb(3)=ijkc0 +m1c +1 - idec
	ideb(4)=ijkc0 +m1c - idec
	ideb(5)=ijkc0 + m12c - idec
	ideb(6)=ijkc0 +1 + m12c - idec
	ideb(7)=ijkc0 +m1c +1 + m12c - idec
	ideb(8)=ijkc0 +m1c + m12c - idec


	do ii=1,8
	    do ind=0,2*osch
              val(ii,ind+1) = r(ideb(ii) + ind*dimd)  
          enddo
      enddo


	end

