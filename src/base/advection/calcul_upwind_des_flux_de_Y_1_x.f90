!********************************************************************************************
!                                                                                           *
! VERSION ORIGINALE PAR A.MEKKAS - S.DELLACHERIE : 01/04/08                                 *
!                                                                                           *
!    CALCUL DECENTRE AMONT DES FLUX DE Y_1 DANS LA DIRECTION X                              *
!                      (SCHEMA DE UPWIND)                                                   *
!                                                                                           *
! SYNTAXE SUBROUTINE:  subroutine  calcul_upwind_des_flux_de_Y_1_x                          *
!                                                                                           *
!											    *
!********************************************************************************************


subroutine calcul_upwind_des_flux_de_Y_1_x

  	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
	use  mod_advection
	use  mod_discretisation

	implicit none


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!

	DOUBLE PRECISION :: u_moyen_x

	integer :: i, i_x, i_y, i_z, i_z_b, i_z_e

!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	flux_local_x_1= 0.
	flux_local_x_2= 0.

	if(dir(1).eq.2) then 
		i_z_b =1
		i_z_e =1
	else if(dir(1).eq.3) then
		i_z_b =1+ighc
		i_z_e = KX-ighc
	end if
			
	do i_z=i_z_b,i_z_e
	do i_y=1+ighc,JX-ighc
	do i_x=1+ighc,IX-ighc

		i=IX*(i_y-1)+(IX*JX)*(i_z-1)+i_x
		u_moyen_x= U(i)

		if (u_moyen_x>0.) then

			flux_local_x_1(i)=u_moyen_x*YY(i-1)
			flux_local_x_2(i)=u_moyen_x*YY(i)
		else if (u_moyen_x<0.) then

			flux_local_x_1(i)=u_moyen_x*YY(i)
			flux_local_x_2(i)=u_moyen_x*YY(i+1)
		end if
	end do
	end do
	end do



end subroutine





