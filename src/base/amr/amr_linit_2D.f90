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

subroutine amr_linit_2D(l)

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

	DOUBLE PRECISION x_limit_inf, y_limit_inf, x_limit_sup, y_limit_sup, angle, x_bis, y_bis

!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	pi =4.*atan(1.)

	imp = 6
	delx = dxglb/float(ri(l))
	dely = dyglb/float(rj(l))

	do m= 1, nga(l)

		im = gp(l) + m

		call unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)
		call unpack_v(im,1,1,1,ijk0,ibmp0,jbmp0,kbmp0)


		if(type_sol_init.eq.1) then
			! Initialisation avec une bulle carree
			goto 111
		else if (type_sol_init.eq.2) then
			! Initialisation avec une bulle cercle
			goto 222
		else if (type_sol_init.eq.3) then
			! Initialisation avec une bulle poisson
			goto 333
		else if (type_sol_init.eq.4) then
			! Initialisation avec une bulle papillon
			goto 444
		else if (type_sol_init.eq.5) then
			! Initialisation lemniscate de Bernoulli :
			goto 555
		else if (type_sol_init.eq.6) then
			! Initialisation quartique piriforme :
			goto 666
		else if (type_sol_init.eq.7) then
			! Initialisation bulle spherique (pseudo) multi-materiaux :
			goto 777
		else if (type_sol_init.eq.8) then
			! Initialisation bulle spherique en couronne :
			goto 888
		else if (type_sol_init.eq.9) then
			goto 999	
		else if (type_sol_init.eq.10) then
			goto 1000	
		else if (type_sol_init.eq.11) then
			goto 11111	
		else if (type_sol_init.eq.22) then
			goto 22222	
		else if (type_sol_init.eq.33) then
			! Initialisation avec une bulle cercle
			goto 33333
		else if (type_sol_init.eq.44) then
			! Initialisation avec deux bulles
			goto 44444
		else if (type_sol_init.eq.99) then
			! Initialisation avec deux bulles
			goto 99999
		else
			write(imp,*) 'Le type de bulle est incorrecte' 
			stop
		end if

!!!!!!!!!!!!!!!!! INITIALISATION DE LA FORME DE LA BULLE !!!!!!!!!!!!!!


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

! Bulle cercle :
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
			-(alpha**2-(y(ii0)+dely/2.)**2)*(beta**2/2.+2.*(x(ii0)+delx/2.))**2)<0.)q(ii,1)=1.
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
			-4.*((x(ii0)+delx/2.)**2-(y(ii0)+dely/2.)**2)**2/&
			((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2)**(3./2.)+4) &
			-((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2 &
			-3.*((x(ii0)+delx/2.)**2-(y(ii0)+dely/2.)**2)**2 &
			/((x(ii0)+delx/2.)**2+(y(ii0)+dely/2.)**2)**(3./2.)+2.)**2 > 0.) q(ii,1)=1.
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

! Initialisation quartique piriforme :
! ====================================

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

! Bulle cercle :
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
			q(ii,1)=1.
			if (y(ii0)+dely/2.>(ymin+ymax)/2.) q(ii,1) = 0.	
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



10000 continue


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

	end do

end 
