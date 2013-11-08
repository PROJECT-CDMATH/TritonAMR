!********************************************************************************************
!                                                                                           *
! VERSION ORIGINALE PAR A.MEKKAS - S.DELLACHERIE : 01/04/08                                 *
!                                                                                           *
!    CALCUL DECENTRE AVAL SOUS CONTRAINTES DES FLUX DE Y_1 DANS LA DIRECTION Z              *
!                      (SCHEMA DE FREDERIC LAGOUTIERE)                                      *
!                                                                                           *
! SYNTAXE SUBROUTINE:  subroutine  calcul_decentre_aval_sous_contraintes_des_flux_de_Y_1_z  *
!                                                                                           *
!											    *
!********************************************************************************************


subroutine calcul_decentre_aval_sous_contraintes_des_flux_de_Y_1_z

 	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
	use  mod_advection
	use  mod_discretisation


	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!

	DOUBLE PRECISION :: u_moyen_z

	integer :: i, i_x, i_y, i_z

	DOUBLE PRECISION :: petit_b, grand_b, min_Y_1, max_Y_1


!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	flux_local_z_1 = 0.
	flux_local_z_2 = 0.

	do i_z=1+ighc,KX-ighc
	do i_y=1+ighc,JX-ighc
	do i_x=1+ighc,IX-ighc

		i=IX*(i_y-1)+(IX*JX)*(i_z-1)+i_x	      	
		u_moyen_z=W(i)

	      	if (u_moyen_z>0.) then

			min_Y_1=min(YY(i-(IX*JX)),YY(i-2*(IX*JX)))
			max_Y_1=max(YY(i-(IX*JX)),YY(i-2*(IX*JX)))
			petit_b=1./(u_moyen_z*dtr/delz)*(YY(i-(IX*JX))-max_Y_1)+max_Y_1
			grand_b=1./(u_moyen_z*dtr/delz)*(YY(i-(IX*JX))-min_Y_1)+min_Y_1

			flux_local_z_1(i)=u_moyen_z*YY(i)
		
			if (YY(i)<=petit_b) flux_local_z_1(i)=u_moyen_z*petit_b
			if (YY(i)>=grand_b) flux_local_z_1(i)=u_moyen_z*grand_b

			min_Y_1=min(YY(i),YY(i-(IX*JX)))
			max_Y_1=max(YY(i),YY(i-(IX*JX)))
			petit_b=1./(u_moyen_z*dtr/delz)*(YY(i)-max_Y_1)+max_Y_1
			grand_b=1./(u_moyen_z*dtr/delz)*(YY(i)-min_Y_1)+min_Y_1

			flux_local_z_2(i)=u_moyen_z*YY(i+(IX*JX))
		
			if (YY(i+(IX*JX))<=petit_b) flux_local_z_2(i)=u_moyen_z*petit_b
			if (YY(i+(IX*JX))>=grand_b) flux_local_z_2(i)=u_moyen_z*grand_b

		else if (u_moyen_z<0.) then

			min_Y_1=min(YY(i),YY(i+(IX*JX)))
			max_Y_1=max(YY(i),YY(i+(IX*JX)))
			petit_b=1./(-u_moyen_z*dtr/delz)*(YY(i)-max_Y_1)+max_Y_1
			grand_b=1./(-u_moyen_z*dtr/delz)*(YY(i)-min_Y_1)+min_Y_1

			flux_local_z_1(i)=u_moyen_z*YY(i-(IX*JX))
		
			if (YY(i-(IX*JX))<=petit_b) flux_local_z_1(i)=u_moyen_z*petit_b
			if (YY(i-(IX*JX))>=grand_b) flux_local_z_1(i)=u_moyen_z*grand_b


			min_Y_1=min(YY(i+(IX*JX)),YY(i+2*(IX*JX)))
			max_Y_1=max(YY(i+(IX*JX)),YY(i+2*(IX*JX)))
			petit_b=1./(-u_moyen_z*dtr/delz)*(YY(i+(IX*JX))-max_Y_1)+max_Y_1
			grand_b=1./(-u_moyen_z*dtr/delz)*(YY(i+(IX*JX))-min_Y_1)+min_Y_1

			flux_local_z_2(i)=u_moyen_z*YY(i)
		
			if (YY(i)<=petit_b) flux_local_z_2(i)=u_moyen_z*petit_b
			if (YY(i)>=grand_b) flux_local_z_2(i)=u_moyen_z*grand_b

	      end if

	end do	
	end do	
	end do

end subroutine
