!********************************************************************************************
!                                                                                           *
! VERSION ORIGINALE PAR A.MEKKAS - S.DELLACHERIE : 15/06/08                                 *
!                                                                                           *
!    CALCUL DECENTRE AMONT DES FLUX DE Y_1 DANS LA DIRECTION Z                              *
!                      (SCHEMA DE UPWIND)                                                   *
!                                                                                           *
! SYNTAXE SUBROUTINE:  subroutine  calcul_upwind_des_flux_de_Y_1_z                          *
!                                                                                           *
!																					        *
!********************************************************************************************


subroutine calcul_upwind_des_flux_de_Y_1_z

  	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
    	use  mod_advection
    	use  mod_discretisation

	implicit none


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!

	DOUBLE PRECISION :: u_moyen_z

	integer :: i, i_x, i_y, i_z


!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	flux_local_z_1= 0.
	flux_local_z_2= 0.


			
	do i_z=1+ighc,KX-ighc
	do i_y=1+ighc,JX-ighc
	do i_x=1+ighc,IX-ighc

		i=IX*(i_y-1)+(IX*JX)*(i_z-1)+i_x
		u_moyen_z=W(i)

		if (u_moyen_z>0.) then

			flux_local_z_1(i)=u_moyen_z*YY(i-(IX*JX))
			flux_local_z_2(i)=u_moyen_z*YY(i)

		else if (u_moyen_z<0.) then
			flux_local_z_1(i)=u_moyen_z*YY(i)
			flux_local_z_2(i)=u_moyen_z*YY(i+(IX*JX))
		end if

	end do
	end do
	end do



end subroutine




















