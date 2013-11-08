module mod_PointerReshape
	interface intf
		subroutine PointerReshape21(x, p,nt)

		integer nt 
		DOUBLE PRECISION, target:: x(nt)
		DOUBLE PRECISION, pointer:: p(:)

		end subroutine PointerReshape21

	end interface intf

end  module mod_PointerReshape
