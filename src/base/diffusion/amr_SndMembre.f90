subroutine amr_SndMembre

	use  mod_hgsparam
	use  mod_hgsarray
        use  mod_discretisation
	use  mod_advection
	use  mod_elliptique

	implicit none

	integer :: idimx,jdimx,kdimx, im, m, l, i, ii,k,j,ijk
	integer :: ibmp,jbmp,kbmp,iii,jj	     
	DOUBLE PRECISION  :: dz, XMoy, VOL, VAR

	idimx = imx(1) 
	jdimx = jmx(1) 
	if(dir(1).eq.3) kdimx = kmx(1) 
	if(dir(1).eq.2) kdimx = 1  



	im = 1
	call unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)
	XMoy = 0.
    do k=1,kdimx
	do j=1,jdimx
	do i=1,idimx
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp + (k-1)*kbmp
			XMoy = XMoy + coef_de_dilatation(iter)*q(ii,1)
	enddo
	enddo
	enddo
	XMoy = XMoy/(idimx*jdimx*kdimx)


	do l= 0, lmax
		call  l_amr_ghost_ff(l)
		isit(l) = 0
		call l_amr_ghost_fc(l)


		do m= 1, nga(l)

			im  = gp(l) + m
			call unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)
			idimx = imx(im) 
			jdimx = jmx(im) 
			ijdim = idimx*jdimx
			if(dir(1).eq.3) kdimx = kmx(im) 
			if(dir(1).eq.2) kdimx = 1  
		
			do k=1,kdimx
			do j=1,jdimx
			do i=1,idimx
				ii = ijk+(i-1)*ibmp+(j-1)*jbmp + (k-1)*kbmp
				jj = (k-1)*ijdim + (j-1)*idimx + i
				SM(ii) = coef_de_dilatation(iter)*q(ii,1) - XMoy
			enddo
			enddo
			enddo
	end do
end do
	

end subroutine
