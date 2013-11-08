SUBROUTINE calcul_volume_de_fluide_1(num)

	USE  mod_hgsparam
	USE  mod_hgsarray
	USE  mod_advection
	USE  mod_elliptique
    	USE  mod_discretisation

	IMPLICIT NONE

	INTEGER :: num, isauv, maxraf, iraf, ijkdeb
	INTEGER :: i, l, m, im, i_z_b, i_z_e, i_z, i_x, i_y



	DO l= 0, 0 !lmax

		IF (l.eq.0) THEN 
			delx = dxglb
			dely = dyglb
			delz = dzglb
		ELSE
			maxraf = 1
			DO iraf = 1,l
				maxraf = maxraf*max_raf(iraf)
			END DO         
			delx = dxglb/float(ri(l))
			dely = dxglb/float(rj(l))
			delz = dxglb/float(rk(l))
		END IF
		DO m= 1, nga(l)

			im  = gp(l) + m

			call solver_define(im)	
            		ijkdeb = hqptr(im) 
			
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

				i= IX*(i_y-1)+(IX*JX)*(i_z-1)+i_x
				volume_de_fluide_1(num) = volume_de_fluide_1(num) + YY(i)
			END DO
			END DO
			END DO
			call solver_undefine
		END DO
			IF(dir(1).eq.2) volume_de_fluide_1(num) = volume_de_fluide_1(num)*delx*dely
			IF(dir(1).eq.3) volume_de_fluide_1(num) = volume_de_fluide_1(num)*delx*dely*delz

		END DO

END SUBROUTINE



