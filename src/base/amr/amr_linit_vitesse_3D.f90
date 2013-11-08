!**************************************************************************************
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 01/06/08                                    *
!                                                                                     *
!		initialisation de la vitesse par une solution analytique 3D           *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine amr_linit_vitesse_3D(l)                             *
!                                                                                     *
!	l : numero de level           	                                              *
!**************************************************************************************

subroutine amr_linit_vitesse_3D(l)

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

	integer m, im, ijk, ibmp, jbmp, kbmp, ijk0, ibmp0, jbmp0, kbmp0

	integer i, j, k, ii, ii0, ne, imp

	DOUBLE PRECISION rayon, alpha, beta, epaisseur, rayon_1, rayon_2, periode_de_rotation, omega,xcentre,ycentre,zcentre

	DOUBLE PRECISION pi
 
!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	pi =4.*atan(1.)

	imp = 6

	delx = dxglb/float(ri(l))
	dely = dyglb/float(rj(l))
	delz = dzglb/float(rk(l))

	do m= 1, nga(l)

		im = gp(l) + m

		call unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)
		call unpack_v(im,1,1,1,ijk0,ibmp0,jbmp0,kbmp0)


!!!!!!!!!!!!!!!!! INITIALISATION DE LA VITESSE D'ADVECTION !!!!!!!!!!!!!!

		if(type_adv_init.eq.1) then
			! Advection parallelle a l axe x :
			goto 1111
		else if (type_adv_init.eq.2) then
			! Advection parallelle a l axe y :
			goto 2222
		else if (type_adv_init.eq.3) then
			! Advection parallelle a l axe z :
			goto 3333
		else if (type_adv_init.eq.4) then
			! Advection diagonale :
			goto 4444
		else if (type_adv_init.eq.5) then
			! :
			goto 5555
		else
			write(imp,*) 'Le type advection est incorrecte' 
			stop
		end if

1111 continue

! Advection parallelle a l axe x :
! ================================

		do k=1,kmx(im)
		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
			q(ii,2)=1.
			q(ii,3)=1.e-8
			q(ii,4)=1.e-8
		end do
		end do
		end do

		goto 9999


2222 continue

! Advection parallelle a l axe y :
! ================================

		do k=1,kmx(im)
		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
			q(ii,2)=1.e-8
			q(ii,3)=1.
			q(ii,4)=1.e-8
		end do
		end do
		end do
		
		goto 9999

3333 continue

! Advection parallelle a l axe z :
! ================================

		do k=1,kmx(im)
		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
			q(ii,2)=1.e-8
			q(ii,3)=1.e-8
			q(ii,4)=1.
		end do
		end do
		end do

		goto 9999

4444 continue

! Advection diagonale :
! =====================

		do k=1,kmx(im)
		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
			q(ii,2)=1.
			q(ii,3)=1.
			q(ii,4)=1.
		end do
		end do
		end do

		goto 9999

5555 continue

! Kothe_Rider :
! ==========
	
		do k=1,kmx(im)
		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp +(k-1)*kbmp
			ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0 +(k-1)*kbmp0
			q(ii,2)= (2.*sin(pi*(x(ii0)+delx/2.))**2)* &
				  sin(2.*pi*(y(ii0)+dely/2.))* & 	
				  sin(2.*pi*(z(ii0)+delz/2.))

			q(ii,3)= -sin(2.*pi*(x(ii0)+delx/2.))* &
				  (sin(pi*(y(ii0)+dely/2.))**2)* & 	
				  sin(2.*pi*(z(ii0)+delz/2.))

			q(ii,4)= -sin(2.*pi*(x(ii0)+delx/2.))* &
				  sin(2.*pi*(y(ii0)+dely/2.))* & 	
				  (sin(pi*(z(ii0)+delz/2.))**2)
		end do
		end do
		end do

		goto 9999


9999 continue
			  
	end do

end 
