!**************************************************************************************
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 01/06/08                                    *
!                                                                                     *
!		initialisation par une solution analytique 3D                         *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine amr_linit_vitesse_3D(l)                             *
!                                                                                     *
!	l : numero de level           	                                              *
!**************************************************************************************


subroutine amr_linit_3D(l)

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

		if(type_sol_init.eq.1) then
			! Initialisation avec une bulle carree
			goto 111
		else if (type_sol_init.eq.2) then
			! Initialisation avec une bulle cercle
			goto 222
		else if (type_sol_init.eq.3) then
			! Initialisation avec une bulle cercle
			goto 333
		else if (type_sol_init.eq.4) then
			! Initialisation avec une bulle cercle
			goto 444
		else
			write(imp,*) 'amr_linit_bulle : Le type de bulle est incorrecte' 
			stop
		end if

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

! Bulle cercle :
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
			if (sqrt((x(ii0)+delx/2.-xcentre)**2+&
			(y(ii0)+dely/2.-ycentre)**2+(z(ii0)+delz/2.-zcentre)**2)<rayon) q(ii,1)=1.
		end do
		end do
		end do

		go to 10000

333 continue


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

444 continue

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

!!!!!!!!!!!!!!!!! FIN INITIALISATION DE LA FORME DE LA BULLE !!!!!!!!!!!!!!

10000 continue
			  
	end do

end 
