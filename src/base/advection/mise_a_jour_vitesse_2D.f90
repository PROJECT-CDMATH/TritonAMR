!*********************************************************************************************
!                                                                                            *
! VERSION ORIGINALE: 14/06/88                                                                *
!                                                                                            *
! VERSION MODIFIEE PAR N.HURE: 14/09/99                                                      *
!                                                                                            *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 16/04/08                                           *
!                                                                                            *
! SYNTAXE SUBROUTINE:   subroutine mise_a_jour_vitesse_2D(im,timep)                          *
!                                                                                            *
! DESCRIPTION:                                                                               *
!              Mise a jour de la vitesse en 2D                                               *
! ENTREES:                                                                                   *
!                                                                                            *
! VARIABLES:                                                                                 *
!           im    ----------> numero de patche                                               *
!           timep ----------> temps                                                          *
!                                                                                            *
!*********************************************************************************************

subroutine mise_a_jour_vitesse_2D(im,timep)

	use  mod_hgsparam
	use  mod_discretisation
	use  mod_hgsarray
	use  mod_advection

	implicit none


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

	DOUBLE PRECISION  :: timep
	integer :: im

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	DOUBLE PRECISION :: pi
	integer :: i, ne

	integer :: ijk, ibmp,jbmp,kbmp, ii, k, j

	integer :: ijk0, ii0, ibmp0, jbmp0, kbmp0


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!



! KHOTE-RIDER :
! ==========

	pi =4.*atan(1.)

	call unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)
	call unpack_v(im,1,1,1,ijk0,ibmp0,jbmp0,kbmp0)

	do j=1,jmx(im)
	do i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		ii0 = ijk0+(i-1)*ibmp0+(j-1)*jbmp0
		q(ii,2)= (-2.*sin(pi*(x(ii0)+delx/2.))**2)* &
				  sin(pi*(y(ii0)+dely/2.))* & 	
				  cos(pi*(y(ii0)+dely/2.)) * &
	              		  cos(2.*timep*pi/periode)		
		q(ii,3)= (2.*sin(pi*(y(ii0)+dely/2.))**2)* &
				  sin(pi*(x(ii0)+delx/2.))* & 	
				  cos(pi*(x(ii0)+delx/2.)) * &
				  cos(2.*timep*pi/periode)		
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

end subroutine
