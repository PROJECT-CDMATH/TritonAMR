subroutine init_amr

	use  mod_hgsparam

	implicit none

	integer l

	irefine = 0
	lmax    = 0
	itgfr   = 10
	isf     = 1
	nga(0)  = 0
	gp(0)   = 0
	ri(0) = 1
	rj(0) = 1
	rk(0) = 1
	do l = 1, ldim
		tol(l)    = .8
		delt(l)   = .1
		ri(l) = 2
		rj(l) = 2
		rk(l) = 2
	enddo


	return
end
