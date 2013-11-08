!********************************************************************************************
!                                                                                           *
! VERSION ORIGINALE PAR A.MEKKAS - S.DELLACHERIE : 15/06/08                                 *
!                                                                                           *
!    CALCUL DECENTRE AMONT DES FLUX DE Y_1 DANS LA DIRECTION Y                              *
!                      (SCHEMA DE UPWIND)                                                   *
!                                                                                           *
! SYNTAXE SUBROUTINE:  subroutine  calcul_upwind_des_flux_de_Y_1_y                          *
!                                                                                           *
!											    *
!********************************************************************************************


subroutine calcul_upwind_des_flux_de_Y_1_y

  	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
	use  mod_advection
	use  mod_discretisation

	implicit none


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!

	DOUBLE PRECISION :: u_moyen_y

	integer :: i, i_x, i_y, i_z, i_z_b, i_z_e


!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	flux_local_y_1= 0.
	flux_local_y_2= 0.

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
		u_moyen_y=V(i)

		if (u_moyen_y>0.) then

			flux_local_y_1(i)=u_moyen_y*YY(i-IX)
			flux_local_y_2(i)=u_moyen_y*YY(i)

		else if (u_moyen_y<0.) then
			flux_local_y_1(i)=u_moyen_y*YY(i)
			flux_local_y_2(i)=u_moyen_y*YY(i+IX)
		end if
	end do
	end do
	end do



end subroutine







