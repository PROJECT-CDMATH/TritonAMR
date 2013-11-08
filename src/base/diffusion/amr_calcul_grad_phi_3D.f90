!**************************************************************************************
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 01/06/08                                    *
!                                                                                     *
!		initialisation par une solution analytique 2D                         *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine amr_linit_vitesse_2D(l)                             *
!                                                                                     *
!	l : numero de level           	                                              *
!**************************************************************************************

subroutine amr_calcul_grad_phi_3D

	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
	use  mod_advection
	use  mod_discretisation

 
	implicit none
!!!!!!!!!!!!! DEBUT DE DECLARATIONS DES VARIABLES GLOBALES !!!!!!!!!!!!!!!

	integer :: l
	
!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES GLOBALES !!!!!!!!!!!!!!!!!

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!

	integer m, im, ijk, ibmp, jbmp, kbmp, ijk0, ibmp0, jbmp0, kbmp0, imp

	integer i, j, k, ii, ii0, ne, isauv

	DOUBLE PRECISION rayon, alpha, beta, epaisseur, rayon_1, rayon_2, periode_de_rotation, omega,xcentre,ycentre

	DOUBLE PRECISION pi

	DOUBLE PRECISION x_limit_inf, y_limit_inf, x_limit_sup, y_limit_sup

!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	pi =4.*atan(1.)

	imp = 6
	q(:,2) = 0.
	q(:,3) = 0.


	do l= 0, lmax
		delx = dxglb/float(ri(l))
		dely = dyglb/float(rj(l))
		dely = dyglb/float(rk(l))
		isit(l+1) = 0
		call l_amr_ghost_fc(l+1)
		call  l_amr_ghost_ff(l)

	do m= 1, nga(l)

		im = gp(l) + m

		call unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)


		do k=1,kmx(im)
		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
			q(ii,2)= (PHI(ii+1)-PHI(ii-1))/(2.*delx)
		end do
		end do
		end do

		do k=1,kmx(im)
		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
			q(ii,3)= (PHI(ii+jbmp)-PHI(ii-jbmp))/(2.*dely)
		end do
		end do
		end do

		do k=1,kmx(im)
		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
			q(ii,4)= (PHI(ii+kbmp)-PHI(ii-kbmp))/(2.*delz)
		end do
		end do
		end do

		do k=1,kmx(im)
		do ne=2,neqt-1
		do j=1,jmx(im)
		do i=1,imx(im)
			q(ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp,ne) = &
			q(ijk+(i-1)*ibmp+(j-1)*jbmp,ne)
		enddo
		enddo
		enddo
		enddo	                      

!		call verifdom(im)

	end do
	end do



end subroutine
