!*********************************************************************************************
!                                                                                            *
! VERSION ORIGINALE PAR A.MEKKAS - J.RYAN: 20/04/08                                          *
!                                                                                            *
! DESCRIPTION:                                                                               *
!              Initialisation des conditions initiales par le calcul                         *
!                                                                                            *
! SYNTAXE SUBROUTINE:  subroutine mc_init_compute(motcle)                                    *
!                                                                                            *
! ENTREES:                                                                                   *
!                                                                                            *
! VARIABLES:                                                                                 *
!           mot          ----------> tableau de variables caracteres contenant les mots de   *
!                                    la ligne de donnee chdon                                *
!           imot         ----------> tableau contenant le nombre de lettre des variables     *
!                                    caracters mot                                           *
!           nmot         ----------> nombre de mots contenus dans la ligne de donnee         *
!                                                                                            *
!*********************************************************************************************

subroutine mc_init_advection_3D(motcle)

	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
	use  mod_discretisation

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

	character *80 motcle
	
!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!!!!!!!

	integer :: im, ibmp, jbmp, kbmp,ibmp0, jbmp0, kbmp0
 
	integer :: jt, lt, kt, jb, lb, kb, nb, mm

	integer :: i, j, k, ijk,ijk0, ii,ii0, ne, imp

	DOUBLE PRECISION rayon, alpha, beta, epaisseur, rayon_1, rayon_2, periode_de_rotation, omega,xcentre,ycentre

	DOUBLE PRECISION pi

	DOUBLE PRECISION x_limit_inf, y_limit_inf, x_limit_sup, y_limit_sup, angle, x_bis, y_bis

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!!!!!!!

	pi =4.*atan(1.)

	im = 1
	imp = 6

 	call unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)
	call unpack_v(im,1,1,1,ijk0,ibmp0,jbmp0,kbmp0)

	if(motcle.eq.'DIRECTION_X') then
		! Advection parallelle a l axe x :
		type_adv_init = 1
		goto 1111
	else if (motcle.eq.'DIRECTION_Y') then
		! Advection parallelle a l axe y :
		type_adv_init = 2
		goto 2222
	else if (motcle.eq.'DIRECTION_Z') then
		! Advection parallelle a l axe z :
		type_adv_init = 3
		goto 3333
	else if (motcle.eq.'DIRECTION_DIAG') then
		! Advection diagonale :
		type_adv_init = 4
		goto 4444
	else if (motcle.eq.'DIRECTION_KOTHE_RIDER') then
		! Rotation :
		type_adv_init = 5
		goto 5555
	else if (motcle.eq.'DIRECTION_DILATATION_POS') then
		! elliptique :
		type_adv_init = 6
		goto 6666
	else
		write(imp,*) 'mc_init_advection : La vitesse', motcle, 'est incorrecte' 
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
		q(ii,3)=0.
		q(ii,4)=0.
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
		q(ii,2)=0.
		q(ii,3)=1.
		q(ii,4)=0.
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
		q(ii,2)=0.
		q(ii,3)=0.
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

6666 continue
! ELLIPTIQUE :
! ==========

	do k=1,kmx(im)
	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp +(k-1)*kbmp
		q(ii,2)= 0.
		q(ii,3)= 0.
		q(ii,4)= 0.
	end do
	end do
	end do

	goto 9999

9999 continue


	return      

end subroutine mc_init_advection_3D
