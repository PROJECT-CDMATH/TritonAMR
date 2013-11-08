!********************************************************************************************
!                                                                                           *
! VERSION ORIGINALE PAR A.MEKKAS - S.DELLACHERIE : 01/04/08                                 *
!                                                                                           *
!    CALCUL DECENTRE AVAL SOUS CONTRAINTES DES FLUX DE Y_1 DANS LA DIRECTION Y              *
!                      (SCHEMA DE FREDERIC LAGOUTIERE)                                      *
!                                                                                           *
! SYNTAXE SUBROUTINE:  subroutine  calcul_decentre_aval_sous_contraintes_des_flux_de_Y_1_y  *
!                                                                                           *
!											    *
!********************************************************************************************


subroutine calcul_decentre_aval_sous_contraintes_des_flux_de_Y_1_y

 	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
	use  mod_advection
	use  mod_discretisation

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!

	DOUBLE PRECISION :: u_moyen_y

	integer :: i, i_x, i_y, i_z, i_z_b, i_z_e

	DOUBLE PRECISION :: petit_b, grand_b, min_Y_1, max_Y_1


!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	flux_local_y_1 = 0.
	flux_local_y_2 = 0.

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

			min_Y_1=min(YY(i-IX),YY(i-2*IX))
			max_Y_1=max(YY(i-IX),YY(i-2*IX))
			petit_b=1./(u_moyen_y*dtr/dely)*(YY(i-IX)-max_Y_1)+max_Y_1
			grand_b=1./(u_moyen_y*dtr/dely)*(YY(i-IX)-min_Y_1)+min_Y_1

			flux_local_y_1(i)=u_moyen_y*YY(i)
		
			if (YY(i)<=petit_b) flux_local_y_1(i)=u_moyen_y*petit_b
			if (YY(i)>=grand_b) flux_local_y_1(i)=u_moyen_y*grand_b


			min_Y_1=min(YY(i),YY(i-IX))
			max_Y_1=max(YY(i),YY(i-IX))
			petit_b=1./(u_moyen_y*dtr/dely)*(YY(i)-max_Y_1)+max_Y_1
			grand_b=1./(u_moyen_y*dtr/dely)*(YY(i)-min_Y_1)+min_Y_1

			flux_local_y_2(i)=u_moyen_y*YY(i+IX)
		
			if (YY(i+IX)<=petit_b) flux_local_y_2(i)=u_moyen_y*petit_b
			if (YY(i+IX)>=grand_b) flux_local_y_2(i)=u_moyen_y*grand_b


		else if (u_moyen_y<0.) then

			min_Y_1=min(YY(i),YY(i+IX))
			max_Y_1=max(YY(i),YY(i+IX))
			petit_b=1./(-u_moyen_y*dtr/dely)*(YY(i)-max_Y_1)+max_Y_1
			grand_b=1./(-u_moyen_y*dtr/dely)*(YY(i)-min_Y_1)+min_Y_1

			flux_local_y_1(i)=u_moyen_y*YY(i-IX)
		
			if (YY(i-IX)<=petit_b) flux_local_y_1(i)=u_moyen_y*petit_b
			if (YY(i-IX)>=grand_b) flux_local_y_1(i)=u_moyen_y*grand_b


			min_Y_1=min(YY(i+IX),YY(i+2*IX))
			max_Y_1=max(YY(i+IX),YY(i+2*IX))
			petit_b=1./(-u_moyen_y*dtr/dely)*(YY(i+IX)-max_Y_1)+max_Y_1
			grand_b=1./(-u_moyen_y*dtr/dely)*(YY(i+IX)-min_Y_1)+min_Y_1

			flux_local_y_2(i)=u_moyen_y*YY(i)
		
			if (YY(i)<=petit_b) flux_local_y_2(i)=u_moyen_y*petit_b
			if (YY(i)>=grand_b) flux_local_y_2(i)=u_moyen_y*grand_b

	      end if

	end do	
	end do	
	end do

end subroutine
