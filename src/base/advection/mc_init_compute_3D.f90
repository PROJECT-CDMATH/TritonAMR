!*********************************************************************************************
!                                                                                            *
! VERSION ORIGINALE PAR A.MEKKAS - J.RYAN: 20/04/08                                          *
!                                                                                            *
! DESCRIPTION:                                                                               *
!              Initialisation des conditions initiales par le calcul                         *
!                                                                                            *
! SYNTAXE SUBROUTINE:  subroutine mc_init_compute(ch1,motcle1,ch2,motcle2)                   *
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

subroutine mc_init_compute_3D(ch1,motcle1,ch2,motcle2)

	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
	use  mod_discretisation

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

	character *80 ch1

	character *80 motcle1

	character *80 ch2

	character *80 motcle2
	
!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!!!!!!!

	integer :: im, iie, jje, kke, limin, limax, ljmin, ljmax, ibmp, jbmp, kbmp,ibmp0, jbmp0, kbmp0
 
	integer :: lkmax, lkmin, ifici, jt, lt, kt, jb, lb, kb, nb, mm

	integer :: JP, LP, KP, JPMU, JPMD, LPMU, LPMD, KPMU, KPMD, LPM3, LPM4

	integer :: i, j, k, ijk,ijk0, ii,ii0, ne, imp

	DOUBLE PRECISION rayon, alpha, beta, epaisseur, rayon_1
	DOUBLE PRECISION rayon_2, periode_de_rotation, omega,xcentre,ycentre,zcentre

	DOUBLE PRECISION pi

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!!!!!!!

	pi =4.*atan(1.)

	im = 1
	imp = 6

	call unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)
	call unpack_v(im,1,1,1,ijk0,ibmp0,jbmp0,kbmp0)


	if(ch1.ne.'bulle') then
		write(imp,*) 'Le mot cle',ch1,'est pas correcte'
		write(imp,*)
		write(imp,*)
		stop
	end if

	if(motcle1.eq.'carree') then
		! Initialisation avec une bulle carree
		type_sol_init = 1
		goto 111
	else if (motcle1.eq.'cercle') then
		! Initialisation avec une bulle cercle
		type_sol_init = 2
		goto 222
	else if (motcle1.eq.'plane') then
		type_sol_init = 3
		goto 333	
	else if (motcle1.eq.'cercle_centre') then
		! Initialisation avec une bulle cercle
		type_sol_init = 4
		goto 44444
!	else if (motcle1.eq.'deux_bulles') then
!		! Initialisation avec une bulle cercle
!		type_sol_init = 5
!		goto 55555
	else
		write(imp,*) 'Le type de bulle', motcle1, 'est incorrecte' 
		stop
	end if
				

!!!!!!!!!!!!!!!!! INITIALISATION DE LA FORME DE LA BULLE !!!!!!!!!!!!!!

111	continue

! Bulle carree :
! =================

	do k=1,kmx(im)
	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0+(k-1)*kbmp0
		q(ii,1)=0.
		if (z(ii0)+delz/2.<(zmax+(zmax+zmin)/2.)/2.) then
		if (z(ii0)+delz/2.>(zmin+(zmax+zmin)/2.)/2.) then
		if (y(ii0)+dely/2.<(ymax+(ymax+ymin)/2.)/2.) then
		if (y(ii0)+dely/2.>(ymin+(ymax+ymin)/2.)/2.) then
		if (x(ii0)+delx/2.<(xmax+(xmax+xmin)/2.)/2.) then
		if (x(ii0)+delx/2.>(xmin+(xmax+xmin)/2.)/2.) then
							q(ii,1)=1.
		end if
		end if
		end if
		end if
		end if
		end if
	enddo
	enddo
	enddo

	go to 10000

222 continue

! Bulle sphere :
! =================

	rayon=.15
	xcentre = 0.35
	ycentre = 0.35	
	zcentre = 0.35	



	do k=1,kmx(im)
	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0+(k-1)*kbmp0
		q(ii,1)=0.
		if (sqrt((x(ii0)+delx/2.-xcentre)**2+(y(ii0)+dely/2.-ycentre)**2+(z(ii0)+delz/2.-zcentre)**2)<rayon) then
					q(ii,1)=1.
		end if
	end do
	end do
	end do

	go to 10000

333 continue

! Bulle plane :
! =================

	do k=1,kmx(im)
	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0+(k-1)*kbmp0
		q(ii,1)=1.
		if (z(ii0)+delz/2.>(zmin+zmax)/2.) q(ii,1) = 0.	
	end do
	end do
	end do

	go to 10000


44444 continue

! Bulle sphere centre:
! =================

	rayon=.15
	xcentre = 0.5
	ycentre = 0.5	
	zcentre = 0.5	



	do k=1,kmx(im)
	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0+(k-1)*kbmp0
		q(ii,1)=0.
		if (sqrt((x(ii0)+delx/2.-xcentre)**2+(y(ii0)+dely/2.-ycentre)**2+(z(ii0)+delz/2.-zcentre)**2)<rayon) then
					q(ii,1)=1.
		end if
	end do
	end do
	end do

	go to 10000


!55555 continue

! Bulle sphere :
! =================

!	rayon_1=.25
!	rayon_2=.15
!	xcentre = 0.5
!	ycentre = 0.5	
!	zcentre = 0.5	

!	do k=1,kmx(im)
!	do j=1,jmx(im)
!	do i=1,imx(im)
!		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
!		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
!		q(ii,1)=0.
!		if (sqrt((x(ii0)+delx/2.-xcentre)**2+ &
!		(y(ii0)+dely/2.)**2+&
!		(z(ii0)+delz/2.)**2)<rayon_1) q(ii,1)=1.

!		if (sqrt((x(ii0)+delx/2.-xmax/4.)**2+ &
!		(y(ii0)+dely/2.-ymin/4.)**2+
!		(z(ii0)+delz/2.-zmin/4.)**2)<rayon_2) q(ii,1)=1.
!	end do
!	end do
!	end do



	go to 10000
			  	      
!!!!!!!!!!!!!!!!! FIN INITIALISATION DE LA FORME DE LA BULLE !!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!! INITIALISATION DE LA VITESSE D'ADVECTION !!!!!!!!!!!!!!

10000 continue
    

	if(ch2.ne.'advection') then
		write(imp,*) 'Le mot cle',ch2,'est pas correcte'
		write(imp,*)
		write(imp,*)
		stop
	end if

	if(motcle2.eq.'direction_x') then
		! Advection parallelle a l axe x :
		type_adv_init = 1
		goto 1111
	else if (motcle2.eq.'direction_y') then
		! Advection parallelle a l axe y :
		type_adv_init = 2
		goto 2222
	else if (motcle2.eq.'direction_z') then
		! Advection parallelle a l axe z :
		type_adv_init = 3
		goto 3333
	else if (motcle2.eq.'direction_diag') then
		! Advection diagonale :
		type_adv_init = 4
		goto 4444
	else if (motcle2.eq.'direction_Kothe_Rider') then
		! Rotation :
		type_adv_init = 5
		goto 5555
	else if (motcle2.eq.'direction_elliptique') then
		! elliptique :
		type_adv_init = 6
		goto 6666
	else
		write(imp,*) 'Le type de bulle', motcle2, 'est incorrecte' 
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



!!!!!!!!!!!!!!!! FIN INITIALISATION DE LA VITESSE D'ADVECTION !!!!!!!!!!!!!!


	return      

end subroutine mc_init_compute_3D
