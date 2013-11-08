subroutine loc_amr(im,i0,j0,k0 , Nx, Ny, Nz, ib,ie, jb,je,kb,ke, Sloc, Samr)
	use  mod_hgsparam
	use  mod_hgsarray
        use  mod_discretisation

	implicit none

    integer im, Nx, Ny, Nz, N,i0,j0,k0
    DOUBLE PRECISION Samr(la), Sloc(Nx,Ny,Nz)

    integer ibmp,jbmp,kbmp, ii,jj,kk,i,j,k, ijk
    integer ib,ie, jb,je,kb,ke, iamr


    call unpack_g(im,i0,j0,k0,ijk,ibmp,jbmp,kbmp)

		do kk=kb,ke
		do jj=jb,je
		do ii=ib,ie
            i=ii-ib+1
            j=jj-jb+1
            k=kk-kb+1
			iamr = ijk+(i-1)*ibmp+(j-1)*jbmp + (k-1)*kbmp
			Samr(iamr) = Sloc(ii,jj,kk)
		enddo
		enddo
		enddo



end subroutine loc_amr
