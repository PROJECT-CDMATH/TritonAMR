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

subroutine mc_init_bulle_3D(motcle)

	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
	use  mod_discretisation

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

	character *80 motcle
	
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


	if(motcle.eq.'CUBE') then
		! Initialisation avec une bulle carree
		type_sol_init = 1
		goto 111
	else if (motcle.eq.'SPHERE_KOTHE') then
		! Initialisation avec une bulle cercle
		type_sol_init = 2
		goto 222
	else if (motcle.eq.'SPHERE_CENTRE') then
		! Initialisation avec une bulle cercle
		type_sol_init = 4
		goto 44444
	else
		write(imp,*) 'mc_init_bulle : Le type de bulle', motcle, 'est incorrecte' 
		stop
	end if
				

!!!!!!!!!!!!!!!!! INITIALISATION DE LA FORME DE LA BULLE !!!!!!!!!!!!!!

111	continue

! Bulle cube :
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


10000 continue



	return      

end subroutine mc_init_bulle_3D
