subroutine PointerReshape21(x,p,nt)

	integer nt
	DOUBLE PRECISION, target:: x(nt)
	DOUBLE PRECISION, pointer:: p(:)

	p => x(1:nt)

end subroutine PointerReshape21


