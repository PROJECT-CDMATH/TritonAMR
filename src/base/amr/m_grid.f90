!************************************************************************
!                                                                       *
! VERSION ORIGINAL: 13/06/88                                            *
!                                                                       *
! VERSION MODIFIEE PAR N.HURE: 14/09/99                                 *
!                                                                       *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 20/04/08                      *
!                                                                       *
! DESCRIPTION:                                                          *
!              Discretisation du domaine de calcul                      *
!                                                                       *
!************************************************************************

subroutine m_grid(mot,imot,nmot)

	use mod_hgsparam
	use mod_discretisation

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

    character *80 mot(20)

    integer imot(20), nmot

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!
	
	integer i1, j1, k1

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	call reel(xmin,mot(2),imot(2))
	call reel(xmax,mot(3),imot(3))
	call reel(ymin,mot(4),imot(4))
	call reel(ymax,mot(5),imot(5))

	if(dir(1).eq.2) then	
		zmin = 0
		zmax = 1
	else if(dir(1).eq.3) then
		call reel(zmin,mot(6),imot(6))
		call reel(zmax,mot(7),imot(7))
	else
		write(6,*) 'dimension inconnue'
		stop		
	end if

	i1 = 1
	j1 = 1
	k1 = 1        

	call mc_cartGrid(i1,j1,k1)

	return      

end
