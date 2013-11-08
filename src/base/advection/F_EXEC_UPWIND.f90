!********************************************************************************************
!                                                                                           *
! VERSION ORIGINALE PAR A.MEKKAS - S.DELLACHERIE : 15/06/08                                 *
!                                                                                           *
!                               CALCUL DE YY-n+1                                            *
!                      (SCHEMA DE UPWIND)                                                   *
!                                                                                           *
! SYNTAXE SUBROUTINE:  subroutine  F_EXEC_UPWIND                                            *
!                                                                                           *
!											    *
!********************************************************************************************

subroutine F_EXEC_UPWIND

 	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
	use  mod_advection
	use  mod_discretisation


	implicit none


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	integer IT

	integer erreur, NB_TOT

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!     


	erreur=0

	NB_TOT = IX*JX*KX	

	allocate(flux_local_x_1(NB_TOT))
	allocate(flux_local_x_2(NB_TOT))
	allocate(flux_local_y_1(NB_TOT))
	allocate(flux_local_y_2(NB_TOT))
	if(dir(1).eq.3) then 
		allocate(flux_local_z_1(NB_TOT))
		allocate(flux_local_z_2(NB_TOT))
	end if	


! AVEC LE DECENTRAGE AVAL SOUS CONTRAINTES :
! ==========================================

	   if (directions_alternees==0) then
	   
           	call calcul_upwind_des_flux_de_Y_1_x

           	call calcul_upwind_des_flux_de_Y_1_y

           	if(dir(1).eq.3) call calcul_upwind_des_flux_de_Y_1_z

           	call solveur_advection_YY

	   else

		if(TYPE_DIR.eq.1) then

			call calcul_upwind_des_flux_de_Y_1_x  !m

			flux_local_y_1=0.
			flux_local_y_2=0.
			if(dir(1).eq.3) then
				flux_local_z_1=0.
				flux_local_z_2=0.
			end if 

			call solveur_advection_YY
   
 
		else if(TYPE_DIR.eq.2) then 

			call calcul_upwind_des_flux_de_Y_1_y

			flux_local_x_1=0.
			flux_local_x_2=0.
			if(dir(1).eq.3) then
				flux_local_z_1=0.
				flux_local_z_2=0.
			end if 

  	   		call solveur_advection_YY

		else if(TYPE_DIR.eq.3) then 
 
 			flux_local_x_1=0.
			flux_local_x_2=0.
			flux_local_y_1=0.
			flux_local_y_2=0.
 
			call calcul_upwind_des_flux_de_Y_1_z
 
 	   		call solveur_advection_YY
		else

		write(6,*) 'PROBLEME DE DIRECTION TYPE_DIR DANS F_EXEC_LAGOUTIERE'	
		stop

		end if

	end if

	deallocate(flux_local_x_1)
	deallocate(flux_local_x_2)
	deallocate(flux_local_y_1)
	deallocate(flux_local_y_2)
	if(dir(1).eq.3) then 
		deallocate(flux_local_z_1)
		deallocate(flux_local_z_2)
	end if	

end  subroutine F_EXEC_UPWIND
