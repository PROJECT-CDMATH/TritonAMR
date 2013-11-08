!************************************************************************
!                                                                       *
! VERSION ORIGINAL: 14/06/88                                            *
!                                                                       *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 24/04/08                      *
!                                                                       *
! DESCRIPTION:                                                          *
!              Initialisation des dimensions du domaine de calcul       *
!                                                                       *
!************************************************************************

subroutine m_dim(mot,imot,nmot)

	use  mod_discretisation
	use  mod_hgsparam

	implicit none


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

    character *80 mot(20)

    integer imot(20), nmot

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	integer i, j, k	

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	if(mot(2)(1:2).eq.'2D') then	
		k = 3
		dir(1)=2
		dir(2)=1
		dir(3)=2
		dir(4)=0
	else	if(mot(2)(1:2).eq.'3D') then
		call entier(k,mot(5),imot(5))
		dir(1)=3
		dir(2)=1
		dir(3)=2
		dir(4)=3
	else
		write(6,*) 'dimension inconnue'
		stop		
	end if
	  	
	call entier(i,mot(3),imot(3))
	call entier(j,mot(4),imot(4))

	call init_hgs(i,j,k)


	imax(0) = i-1
	jmax(0) = j-1
	kmax(0) = k-1     

	nb_mailles_x = i-1
	nb_mailles_y = j-1
	nb_mailles_z = k-1

	call patch_sav(0,1,i-1,1,j-1,1,k-1)

	return

end
