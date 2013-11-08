!********************************************************************************************
!                                                                                           *
! VERSION ORIGINALE PAR A.MEKKAS - S.DELLACHERIE : 01/04/08                                 *
!                                                                                           *
!                            CALCUL DU PAS DE TEMPS                                         *
!                                                                                           *
! SYNTAXE SUBROUTINE:  SUBROUTINE  calcul_du_pas_de_temps(im)                               *
!          im : numero du patch                                                             *
!											    *
!********************************************************************************************


SUBROUTINE calcul_du_pas_de_temps(im)

	USE  mod_discretisation
	USE  mod_advection
	USE  mod_hgsparam
	USE  mod_hgsarray
	USE  mod_user

	IMPLICIT NONE	


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

	INTEGER im

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!

	INTEGER i, j, k, ijk, ibmp, jbmp, kbmp, ii,l

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!!!

	IF(DELTA_T_OR_CFL.EQ.0) THEN
		WRITE(6,*) 'PROBLEME DE PAS DE TEMPS' 
		WRITE(6,*) 'PROBLEME amr_calcul_grad_phi_2D'
	END IF 
	IF(DELTA_T_OR_CFL.EQ.1) GOTO 555

    	l = lgrid(im)
	delx = dxglb/float(ri(l))
	dely = dyglb/float(rj(l))
	delz = dzglb/float(rk(l))


	CALL unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)

	norme_max_de_u_lagoutiere = 0.

	IF(dir(1).eq.3) GO TO 333
	IF(dir(1).eq.2) GO TO 222

222 CONTINUE
	DO j=1,jmx(im)
	DO i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp
		norme_max_de_u_lagoutiere = max(norme_max_de_u_lagoutiere,sqrt(q(ii,2)**2+q(ii,3)**2))
	END DO
	END DO
	delta_t_lagoutiere = cfl_lagoutiere*min(delx,dely)/norme_max_de_u_lagoutiere
	GO TO 444

333 CONTINUE
	DO k=1,kmx(im)
	DO j=1,jmx(im)
	DO i=1,imx(im)
		ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
		norme_max_de_u_lagoutiere = max(norme_max_de_u_lagoutiere,sqrt(q(ii,2)**2+q(ii,3)**2+q(ii,4)**2))
	END DO
	END DO
	END DO
	delta_t_lagoutiere = cfl_lagoutiere*min(delx,dely,delz)/norme_max_de_u_lagoutiere

444 CONTINUE


	dtglb = delta_t_lagoutiere

555 CONTINUE
	RETURN
		
END SUBROUTINE calcul_du_pas_de_temps
