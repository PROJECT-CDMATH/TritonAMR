!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE PAR N.HURE: 14/09/99                                              *
!                                                                                     *
! VERSION MODIFIEE PAR J.RYAN: 29/06/00                                               *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 05/05/08                                    *
!                                                                                     *
!							level computation							              *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine l_compute_uns_solver(l)                             *
!     l : level                                                                       *
!																					  *
!**************************************************************************************

SUBROUTINE INTERFACE_AMR_HYPERBOLIQUE(l)

	USE  mod_hgsparam
	USE  mod_hgsarray
	USE  mod_hgssave
	USE  mod_advection
	USE  mod_discretisation
	USE  mod_user

	IMPLICIT NONE

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

    	INTEGER l

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	INTEGER           :: iraf, maxraf, m, im, IT

	INTEGER           :: ijk, ibmp, jbmp, kbmp,k,j,ne,i

	DOUBLE PRECISION  :: dt, dxr, dyr, dzr, timep

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!     

	DO m = 1, nga(l)		

	im = gp(l) + m

        IF (l.eq.0) THEN 
		delx = dxglb
		dely = dyglb
		delz = dzglb
		dtr  = dtglb
		WRITE(6,*) 
		WRITE(6,'(a,e12.5,a,e12.5,a)') '----- Solution Time = ',time, &
			'   ---   (+Dt = ',dtr,') -----'
        ELSE
		maxraf = 1
		DO iraf = 1,l
			maxraf = maxraf*max_raf(iraf)
		END DO         
		dtr = dtglb/float(maxraf)
		delx = dxglb/float(ri(l))
		dely = dxglb/float(rj(l))
		delz = dxglb/float(rk(l))
        END IF

! 	IT = iter

	IF(mise_jour_adv.eq.1) THEN
	timep = time + isit(l)*dtr
	IF(dir(1).eq.3.AND.MISEAJOUR_USER.EQ.0) THEN
	CALL mise_a_jour_vitesse_3D(im,timep)
	END IF
	IF(dir(1).eq.3.AND.MISEAJOUR_USER.EQ.1) THEN
	CALL US_MISEAJOUR_ADVECTION(l,im,timep)
	END IF
	IF(dir(1).eq.2) THEN
		IF (MISEAJOUR_USER.EQ.0) CALL mise_a_jour_vitesse_2D(im,timep)	
		IF (MISEAJOUR_USER.EQ.1) CALL US_MISEAJOUR_ADVECTION(l,im,timep)	
		CALL unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)
	END IF
	END IF

		CALL solver_define(im)
		IF(isolver.eq.2) THEN 			
			CALL F_EXEC_LAGOUTIERE
		else IF(isolver.eq.1) THEN
			CALL F_EXEC_UPWIND
		else
			WRITE(6,*) 'schema inconnu'
			stop
		END IF
 	   	CALL solver_undefine	

		IF(dir(1).eq.2) THEN
			CALL unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)
			DO k=1,kmx(im)
			DO ne=1,1
			DO j=1,jmx(im)
			DO i=1,imx(im)
				q(ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp,ne) = &
				q(ijk+(i-1)*ibmp+(j-1)*jbmp,ne)
			END DO
			END DO
			END DO
			END DO 
		END IF
	END DO ! END m
     


	write(74,102) l, dtr

102   format('It ',i2,' Dt = ',e10.3)      
9999  format('+++ Compute level ',i2)      




	return

END


