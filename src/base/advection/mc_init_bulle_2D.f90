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

subroutine mc_init_bulle_2D(motcle)

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


	if(motcle.eq.'CARREE') then
		! Initialisation avec une bulle carree
		type_sol_init = 1
		goto 111
	else if (motcle.eq.'CERCLE_KOTHE') then
		! Initialisation avec une bulle cercle
		type_sol_init = 2
		goto 222
	else if (motcle.eq.'POISSON') then
		! Initialisation avec une bulle poisson
		type_sol_init = 3
		goto 333
	else if (motcle.eq.'PAPILLON') then
		! Initialisation avec une bulle papillon
		type_sol_init = 4
		goto 444
	else if (motcle.eq.'BERNOUILLI') then
		! Initialisation lemniscate de Bernoulli :
		type_sol_init = 5
		goto 555
	else if (motcle.eq.'PIRIFORME') then
		! Initialisation quartique piriforme :
		type_sol_init = 6
		goto 666
	else if (motcle.eq.'multi-materiaux') then
		! Initialisation bulle spherique (pseudo) multi-materiaux :
		type_sol_init = 7
		goto 777
	else if (motcle.eq.'COURONNE') then
		! Initialisation bulle spherique en couronne :
		type_sol_init = 8
		goto 888
	else if (motcle.eq.'ZALESAK_II') then
		type_sol_init = 9
		goto 999	
	else if (motcle.eq.'ZALESAK_I') then
		type_sol_init = 10
		goto 1000	
	else if (motcle.eq.'PLANE') then
		type_sol_init = 11
		goto 11111	
	else if (motcle.eq.'PLANE_PERTURBEE') then
		type_sol_init = 22
		goto 22222	
	else if (motcle.eq.'CERCLE_CENTRE') then
		! Initialisation avec une bulle cercle
		type_sol_init = 33
		goto 33333
	else if (motcle.eq.'DEUX_BULLES') then
		! Initialisation avec une bulle cercle
		type_sol_init = 44
		goto 44444
	else if (motcle.eq.'ETOILE') then
		! Initialisation avec une bulle cercle
		type_sol_init = 99
		goto 99999
	else
		write(imp,*) 'LE TYPE DE BULLE ', motcle, 'EST INCORRECTE' 
		stop
	end if
				

! INITIALISATION DE LA FORME DE LA BULLE

44444	continue

	rayon_1=.25
	rayon_2=.15


	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=0.
		if (sqrt((x(ii0)+delx/2.-xmin/4.)**2+(y(ii0)+dely/2.)**2)<rayon_1) q(ii,1)=1.
		if (sqrt((x(ii0)+delx/2.-xmax/4.)**2+(y(ii0)+dely/2.-ymin/4.)**2)<rayon_2) q(ii,1)=1.
	end do
	end do

	goto 10000

111	continue

! Bulle carree :
! =================

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=0.
		if (y(ii0)+dely/2.<(ymax+(ymax+ymin)/2.)/2.) then
		if (y(ii0)+dely/2.>(ymin+(ymax+ymin)/2.)/2.) then
		if (x(ii0)+delx/2.<(xmax+(xmax+xmin)/2.)/2.) then
		if (x(ii0)+delx/2.>(xmin+(xmax+xmin)/2.)/2.) then
			q(ii,1)=1.
		end if
		end if
		end if
		end if
	enddo
	enddo

	go to 10000

222 continue

! Bulle cercle :
! =================

	rayon=.15
	xcentre = 0.5
	ycentre = 0.75	


	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=0.
		if (sqrt((x(ii0)+delx/2.-xcentre)**2+(y(ii0)+dely/2.-ycentre)**2)<rayon) q(ii,1)=1.
	end do
	end do

	go to 10000


33333 continue

! Bulle cercle centre [0,1][0,1] :
! =================

	rayon=.15
	xcentre = 0.5
	ycentre = 0.5	


	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=0.
		if (sqrt((x(ii0)+delx/2.-xcentre)**2+(y(ii0)+dely/2.-ycentre)**2)<rayon) q(ii,1)=1.
	end do
	end do

	go to 10000

333 continue

! Bulle poisson :
! ===============

	alpha=.25
	beta=2.*sqrt(2.)*alpha

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=0.
		if((((x(ii0)+delx/2.)**2-(y(ii0)+dely/2.)**2+alpha**2-beta**2/2.)**2 &
		  -(alpha**2-(y(ii0)+dely/2.)**2)*(beta**2/2.+2.*(x(ii0)+delx/2.))**2)<0.) q(ii,1)=1.
	end do
	end do

	goto 10000

444 continue

! Bulle papillon [-3,3]x[-3,3]:
! ================

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=0.
		if (((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2 &
		-4.*((x(ii0)+delx/2.)**2-(y(ii0)+dely/2.)**2)**2/((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2)**(3./2.)+4) &
		-((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2 &
		-3.*((x(ii0)+delx/2.)**2-(y(ii0)+dely/2.)**2)**2 &
		/((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2)**(3./2.)+2.)**2 > 0.) then
			q(ii,1)=1.
		end if
	end do
	end do

	goto 10000

555 continue

! Bulle bernoulli:
! ================

	alpha=.75

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		rayon=((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2)**2 &
                -alpha**2*((x(ii0)+delx/2.)**2-(y(ii0)+dely/2.)**2)
		q(ii,1)=0.
			if (rayon<0.) q(ii,1)=1.
	end do
	end do

	goto 10000

666 continue

! Bulle multi-materiaux :
! ================

	alpha=1.
	beta=1.

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
! Cas symetrique par rapport a y=0 :
		rayon=(x(ii)+alpha/2.)**4 &
		-alpha*(x(ii)+alpha/2.)**3 &
		+beta**2*y(ii)**2
   		q(ii,1)=0.
		if (rayon<0.) q(ii,1)=1.
	end do
	end do

! Bulle piriforme :
! ============

	alpha=1.
	beta=1.

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
! Cas symetrique par rapport a y=0 :
		rayon=((x(ii0)+delx/2.)+alpha/2.)**4 &
		-alpha*((x(ii0)+delx/2.)+alpha/2.)**3 &
		+beta**2*(y(ii0)+dely/2.)**2
		q(ii,1)=0.
		if (rayon<0.) q(ii,1)=1.

	end do
	end do

	goto 10000

777 continue

! Bulle spherique (pseudo) multi-materiaux :
! ==========================================

	epaisseur=4.
	rayon_1=.5-epaisseur*max(delx,dely)
	rayon_2=.5

	do j=1,jmx(im)+1
	do i=1,imx(im)+1
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=0.
		if (sqrt((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2)<rayon_1) q(ii,1)=1.
	end do
	end do

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=0.
		if (sqrt((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2)>=rayon_1.and. &
		    sqrt((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2)<rayon_2) q(ii,1)=0.5
	end do
	end do

	goto 10000


888 continue

! Bulle spherique en couronne :
! =============================

	epaisseur=1.
	rayon_1=.5-epaisseur*max(delx,dely)
	rayon_2=.5

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=0.
		if (sqrt((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2)>=rayon_1.and. &
		sqrt((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2)<rayon_2) q(ii,1)=1.
	end do
	end do

	goto 10000
			  	      
999	continue

! Bulle Zalesak_II :
! =================
	x_limit_inf = 0.1
	x_limit_sup = 0.2	

	y_limit_inf = 0.4
	y_limit_sup = 0.9

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=0.
		if (y(ii0)+dely<y_limit_sup) then
		if (y(ii0)+dely/2.>y_limit_inf) then
		if (x(ii0)+delx<x_limit_sup) then
		if (x(ii0)+delx/2.>x_limit_inf) then
			q(ii,1)=1.
		end if
		end if
		end if
		end if
	enddo
	enddo

	go to 10000

1000 continue

! Bulle ZALESAK I :
! =================

	rayon=.5
	xcentre = 0.
	ycentre = 0.	

	x_limit_inf = -0.05
	x_limit_sup = 0.05	

	y_limit_inf = -0.5
	y_limit_sup = 0.

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=0.
		if (sqrt((x(ii0)+delx/2.-xcentre)**2+(y(ii0)+dely/2.-ycentre)**2)<rayon) q(ii,1)=1.
		if( (x(ii0)+delx/2.)<x_limit_sup.and.(x(ii0)+delx/2.)>x_limit_inf.and.&
		(y(ii0)+dely/2.)<y_limit_sup.and.(y(ii0)+dely/2.)>y_limit_inf) q(ii,1)=0.

	end do
	end do

	go to 10000

11111 continue

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=0.
		if (y(ii0)+dely/2.<(ymin+ymax)/2.) q(ii,1) = 1.	
	end do
	end do

	go to 10000

22222 continue

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,1)=1.
		if (y(ii0)+dely/2.>(ymin+ymax)/2.+(ymax-ymin)/50.*sin(2.*pi*10.*(x(ii0)+delx/2.)/(xmax-xmin))) then 
			q(ii,1) = 0.	
		end if
	end do
	end do

	go to 10000


 99999	continue
! Bulle en etoile :
! =================

	angle=0.

	do j=1,jmx(im)
	do i=1,imx(im)

		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0

		x_bis=(x(ii0)+delx/2.)*cos(2*pi*angle/360.) &
			+(y(ii0)+dely/2.)*sin(2*pi*angle/360.)

		y_bis=-(x(ii0)+delx/2.)*sin(2*pi*angle/360.) &
			+(y(ii0)+dely/2.)*cos(2*pi*angle/360.)

		if (x_bis<(xmax+(xmax+xmin)/2.)/4.) then
		if (x_bis>(xmin+(xmax+xmin)/2.)/4.) then
		if (y_bis<(ymax+(ymax+ymin)/2.)/4.) then
		if (y_bis>(ymin+(ymax+ymin)/2.)/4.) then
			q(ii,1)=1.
		end if
		end if
		end if
		end if

	end do
	end do

	angle=45.

	do j=1,jmx(im)
	do i=1,imx(im)

		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0

		x_bis=(x(ii0)+delx/2.)*cos(2*pi*angle/360.) &
			+(y(ii0)+dely/2.)*sin(2*pi*angle/360.)

		y_bis=-(x(ii0)+delx/2.)*sin(2*pi*angle/360.) &
			+(y(ii0)+dely/2.)*cos(2*pi*angle/360.)

		if (x_bis<(xmax+(xmax+xmin)/2.)/4.) then
		if (x_bis>(xmin+(xmax+xmin)/2.)/4.) then
		if (y_bis<(ymax+(ymax+ymin)/2.)/4.) then
		if (y_bis>(ymin+(ymax+ymin)/2.)/4.) then
			q(ii,1)=1.
		end if
		end if
		end if
		end if

	end do
	end do

	goto 10000

!!!!!!!!!!!!!!!!! FIN INITIALISATION DE LA FORME DE LA BULLE !!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!! INITIALISATION DE LA VITESSE D'ADVECTION !!!!!!!!!!!!!!

10000 continue



!!!!!!!!!!!!!!!! FIN INITIALISATION DE LA VITESSE D'ADVECTION !!!!!!!!!!!!!!

	do k=1,kmx(im)
	do ne=1,1
	do j=1,jmx(im)
	do i=1,imx(im)
		q(ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp,ne) = &
		q(ijk+(i-1)*ibmp+(j-1)*jbmp,ne)
	enddo
	enddo
	enddo
	enddo	                      

	return      

end subroutine mc_init_bulle_2D
