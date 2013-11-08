!**************************************************************************************
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 01/06/08                                    *
!                                                                                     *
!		initialisation de la vitesse par une solution analytique 2D           *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine amr_linit_vitesse_2D(l)                             *
!                                                                                     *
!	l : numero de level           	                                              *
!**************************************************************************************

subroutine amr_linit_vitesse_2D(l)

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

	integer i, j, k, ii, ii0, ne

	DOUBLE PRECISION rayon, alpha, beta, epaisseur, rayon_1, rayon_2, periode_de_rotation, omega,xcentre,ycentre

	DOUBLE PRECISION pi

	DOUBLE PRECISION x_limit_inf, y_limit_inf, x_limit_sup, y_limit_sup

!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	pi =4.*atan(1.)

	imp = 6
	delx = dxglb/float(ri(l))
	dely = dyglb/float(rj(l))

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
			! Advection diagonale :
			goto 3333
		else if (type_adv_init.eq.4) then
			! Rotation :
			goto 4444
		else if (type_adv_init.eq.5) then
			! Rotation :
			goto 5555
		else if (type_adv_init.eq.6) then
			! Rotation :
			goto 6666
		else if (type_adv_init.eq.7) then
			! Rotation :
			goto 7777
		else if (type_adv_init.eq.8) then
			! Elliptique :
			goto 8888
		else
			write(imp,*) 'Le type advection est incorrecte' 
			stop
		end if

1111 continue

! Advection parallelle a l axe x :
! ================================

		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp
			q(ii,2)=1.
			q(ii,3)=1.e-8
		end do
		end do

		goto 9999

2222 continue

! Advection parallelle a l axe y :
! ================================

		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp
			q(ii,2)=1.e-8
			q(ii,3)=1.
		end do
		end do
			
		goto 9999

3333 continue


! Advection diagonale :
! =====================

		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp
			q(ii,2)=1.
			q(ii,3)=1.
		end do
		end do

		goto 9999

4444 continue

! Rotation :
! ==========

		periode_de_rotation=2.
		omega=2.*pi/periode_de_rotation

		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp
			ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
			q(ii,2)=-omega*(y(ii0)+dely/2.)
			q(ii,3)=omega*(x(ii0)+delx/2.)
		end do
		end do

		goto 9999

5555 continue

! KHOTE-RIDER :
! ==========
	
		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp
			ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
			q(ii,2)= (-2.*sin(pi*(x(ii0)+delx/2.))**2)* &
				  sin(pi*(y(ii0)+dely/2.))* & 	
				  cos(pi*(y(ii0)+dely/2.))
			q(ii,3)= (2.*sin(pi*(y(ii0)+dely/2.))**2)* &
				  sin(pi*(x(ii0)+delx/2.))* & 	
				  sin(pi*(x(ii0)+delx/2.))
		end do
		end do

		goto 9999

6666 continue

! ZALESAk II :
! ==========

		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp
			ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
			q(ii,2)=x(ii0)+delx/2.
			q(ii,3)=-(y(ii0)+dely/2.)
		end do
		end do

		go to 9999

7777 continue

! ZALESAK I  :
! ==========

		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp
			ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
			q(ii,2)=-10*(y(ii0)+dely/2.)
			q(ii,3)= 10*(x(ii0)+delx/2.)
		end do
		end do

		go to 9999

8888 continue

! ELLIPTIQUE  :
! ==========

		do j=1,jmx(im)
		do i=1,imx(im)
			ii = ijk+(i-1)*ibmp+(j-1)*jbmp
			q(ii,2)= 0.
			q(ii,3)= 0.
		end do
		end do

		go to 9999

!!!!!!!!!!!!!!!! FIN INITIALISATION DE LA VITESSE D'ADVECTION !!!!!!!!!!!!!!


9999 continue


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

	end do

end 
