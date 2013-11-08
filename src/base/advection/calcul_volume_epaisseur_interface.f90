SUBROUTINE calcul_volume_epaisseur_interface(num)

	USE  mod_hgsparam
	USE  mod_hgsarray
	USE  mod_advection
    	USE  mod_discretisation

	IMPLICIT NONE

	INTEGER :: num, isauv, maxraf, iraf
	INTEGER :: i, l, m, im, i_z_b, i_z_e, i_z, i_x, i_y
	DOUBLE PRECISION :: epaisseur_interface(0:lmax)
	DOUBLE PRECISION :: volume(0:lmax)
	CHARACTER cnum



	epaisseur_interface = 0.
	volume = 0.
	DO l= 0, lmax
		CALL  l_amr_ghost_ff(l)
		isauv = isit(l)
		isit(l) = 0
		CALL l_amr_ghost_fc(l)
		isit(l) = isauv
		IF (l.eq.0) THEN 
			delx = dxglb
			dely = dyglb
			delz = dzglb
		else
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

			CALL solver_define(im)	
			IF(dir(1).eq.2) THEN 
				i_z_b =1
				i_z_e =1
			else IF(dir(1).eq.3) THEN
				i_z_b =1+ighc
				i_z_e = KX-ighc
			END IF
			
			DO i_z=i_z_b,i_z_e
			DO i_y=1+ighc,JX-ighc
			DO i_x=1+ighc,IX-ighc

				i=IX*(i_y-1)+(IX*JX)*(i_z-1)+i_x	
				epaisseur_interface(l) = epaisseur_interface(l) + abs(YY(i)*(1.-YY(i)))
				volume(l) = volume(l) + YY(i)
			END DO
			END DO
			END DO
			CALL solver_undefine	
		END DO

			write(cnum,102) l
		

			open(unit=103,file='EPAISSEUR_INTERFACE.DATA' ,form='FORMATTED',position="APPEND",status='old')

		IF(dir(1).eq.2) THEN
			write(103,*) num,epaisseur_interface(l)*delx*dely
		END IF

		IF(dir(1).eq.3) THEN
			write(103,*) num,epaisseur_interface(l)*delx*dely*delz
		END IF
	END DO
		CLOSE(103)


102     FORMAT(i1.1)

END SUBROUTINE




