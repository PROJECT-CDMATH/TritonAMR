!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE PAR N.HURE: 14/04/00                                              *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 25/04/08                                    *
!                                                                                     *
!                   Gestion de la boucle en temps                                     *
!                                                                                     *
! SYNTAXE SUBROUTINE:  SUBROUTINE  m_run                                              *
!                                                                                     *
!										                                              *
!**************************************************************************************

SUBROUTINE m_run

 	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
	use  mod_advection
	use  mod_elliptique
	use  mod_discretisation
	use  mod_user

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!
	
	integer :: i, idir, Deb, k
	
	DOUBLE PRECISION  :: surface_du_domaine, constante, psi	

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!!


	Deb = 1
	ALLOCATE(volume_de_fluide_1(0:nitmax))
	ALLOCATE(volume_de_fluide_1_exact(0:nitmax))
	ALLOCATE(coef_de_dilatation(0:nitmax))
	ALLOCATE(Temps(0:nitmax))
	coef_de_dilatation = 0.
	volume_de_fluide_1 = 0.
	volume_de_fluide_1_exact = 0.

	IF(nitmax.eq.0) RETURN
	IF(methode.eq.1) THEN
		DO iter=iter_init,nitmax
			IF (ELL_YES == 1) THEN
			Temps(iter) = time
			IF(time.gt.max_temps.or.iter.eq.nitmax) THEN
				IF(dir(1).eq.2) surface_du_domaine = (xmax-xmin)*(ymax-ymin)
				IF(dir(1).eq.3) surface_du_domaine = (xmax-xmin)*(ymax-ymin)*(zmax-zmin)

				constante= volume_de_fluide_1(0)/(surface_du_domaine-volume_de_fluide_1(0))
				psi=0.
				volume_de_fluide_1_exact(0)=volume_de_fluide_1(0)/surface_du_domaine

				DO k=1,iter-1

					psi=psi+coef_de_dilatation(k)*(Temps(k)-Temps(k-1))

					volume_de_fluide_1_exact(k)= &
					exp(psi+log(constante))/(1.+exp(psi+log(constante)))
				END DO


				open(NFVOLDE,file='../RESULTATS/volume_de_fluide_1_discret_exact.plot')

				DO k=0,iter-1
				   write(NFVOLDE,*) Temps(k), &
				   volume_de_fluide_1(k)/surface_du_domaine,&
				   volume_de_fluide_1_exact(k)

				END DO
				CLOSE(NFVOLDE)


				DEALLOCATE(volume_de_fluide_1)
				DEALLOCATE(volume_de_fluide_1_exact)
				DEALLOCATE(Temps)

				RETURN
			END IF
			END IF
			write(*,*) 
			write(*,'(a)') '+------------------------------------------------+'
			write(*,'(a,i5,a)') '| TritonAMR Cycle number : ',iter,'                 |'
			write(*,'(a)') '+------------------------------------------------+'
			
			CALL amr_driv_uns_solver_meth_1

			IF (ELL_YES == 1) THEN
			CALL calcul_du_coefficient_de_dilatation(iter)
			CALL amr_driv_LDC
			CALL calcul_volume_de_fluide_1(iter)
			END IF
			time = time + dtglb
                        IF(time.gt.max_temps.and.Deb.eq.1) THEN
				dtglb = time - max_temps	
				time = max_temps
				Deb = -1
			END IF
                        ! AJOUT ANOUAR A VERIFIER
                        IF(time.EQ.max_temps) return
                    END DO 			
	

	ELSE IF(methode.eq.2) THEN
		DO iter=iter_init,nitmax
			IF (ELL_YES == 1) THEN
			Temps(iter) = time
			IF(time.gt.max_temps.or.iter.eq.nitmax) THEN
				IF(dir(1).eq.2) surface_du_domaine = (xmax-xmin)*(ymax-ymin)
				IF(dir(1).eq.3) surface_du_domaine = (xmax-xmin)*(ymax-ymin)*(zmax-zmin)

				constante= volume_de_fluide_1(0)/(surface_du_domaine-volume_de_fluide_1(0))
				psi=0.
				volume_de_fluide_1_exact(0)=volume_de_fluide_1(0)/surface_du_domaine

				DO k=1,iter-1

					psi=psi+coef_de_dilatation(k)*(Temps(k)-Temps(k-1))

					volume_de_fluide_1_exact(k)= &
					exp(psi+log(constante))/(1.+exp(psi+log(constante)))
				END DO


				open(NFVOLDE,file='../RESULTATS/volume_de_fluide_1_discret_exact.plot')

				DO k=0,iter-1


				   write(NFVOLDE,*) Temps(k), &
				   volume_de_fluide_1(k)/surface_du_DOmaine,&
				   volume_de_fluide_1_exact(k)

				END DO
				CLOSE(NFVOLDE)


				DEALLOCATE(volume_de_fluide_1)
				DEALLOCATE(volume_de_fluide_1_exact)
				DEALLOCATE(Temps)

				RETURN
			END IF
			END IF

			write(*,*) 
			write(*,'(a)') '+------------------------------------------------+'
			write(*,'(a,i5,a)') '| TritonAMR Cycle number : ',iter,'                       |'
			write(*,'(a)') '+------------------------------------------------+'
			

			DO idir=1,dir(1)	
				TYPE_DIR = dir(idir+1)
				CALL amr_driv_uns_solver_meth_2
			END DO

			IF (ELL_YES == 1) THEN
			CALL amr_driv_LDC
			CALL calcul_du_coefficient_de_dilatation(iter)
			CALL calcul_volume_de_fluide_1(iter)
			END IF
			time = time + dtglb
			IF(time.gt.max_temps.and.Deb.eq.1) THEN
				dtglb = time - max_temps	
				time = max_temps
				Deb = -1
			END IF
		END DO 			

	END IF



	IF (idebug.eq.1) THEN
	write(*,*) 
	write(*,'(a,i5,a,e12.5)') '+++ +++ Last iteration : ',iter-1, &
	'     @ T = ', time
	END IF

	RETURN

END SUBROUTINE m_run
