!********************************************************************************************
!                                                                                           *
! VERSION ORIGINALE PAR A.MEKKAS - S.DELLACHERIE : 01/04/08                                 *
!                                                                                           *
!                         CALCUL YY A L'INSTANT N+1                                         *
!                      (SCHEMA DE FREDERIC LAGOUTIERE)                                      *
!                                                                                           *
! SYNTAXE SUBROUTINE:  SUBROUTINE  solveur_advection_3d_de_Y_1                              *
!                                                                                           *
!											    *
!********************************************************************************************

SUBROUTINE solveur_advection_YY

 	USE  mod_hgsparam
	USE  mod_hgsarray
	USE  mod_hgssave
	USE  mod_advection
	USE  mod_discretisation

	IMPLICIT NONE

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	INTEGER :: i, i_x, i_y, i_z, i_z_b, i_z_e
		
	DOUBLE PRECISION :: eps

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!     

	eps = 1.e-14

	IF(dir(1).eq.2) THEN 
		i_z_b =1
		i_z_e =1
	ELSE IF(dir(1).eq.3) THEN
		i_z_b =1+ighc
		i_z_e = KX-ighc
	END IF
			
	DO i_z=i_z_b,i_z_e
	DO i_y=1+ighc,JX-ighc
	DO i_x=1+ighc,IX-ighc

		i=IX*(i_y-1)+(IX*JX)*(i_z-1)+i_x

		IF(dir(1).eq.2) YY(i)=YY(i) &
		-dtr/delx*(flux_local_x_2(i)-flux_local_x_1(i)) &
		-dtr/dely*(flux_local_y_2(i)-flux_local_y_1(i))

		IF(dir(1).eq.3) YY(i)=YY(i) &
		-dtr/delx*(flux_local_x_2(i)-flux_local_x_1(i)) &
		-dtr/dely*(flux_local_y_2(i)-flux_local_y_1(i)) &
		-dtr/delz*(flux_local_z_2(i)-flux_local_z_1(i))

! 		IF(YY(i).gt.(1.+eps)) THEN
! 			write(6,*) 'DEPASSEMENT DE Y: Y > 1+eps avec eps=1.e-14'
! 			write(6,*) 'i,YY(i),iter',i,YY(i),iter
! 	                STOP
! 		END IF		
! 
! 
! 		IF(YY(i).lt.-eps) THEN
! 			write(6,*) 'DEPASSEMENT DE Y: Y < -eps avec eps=1.e-14'
! 			write(6,*) 'i,YY(i),iter',i,YY(i),iter
! 	                STOP
! 		END IF		

	END DO	
	END DO		
	END DO

END SUBROUTINE
