!**************************************************************************************
!                                                                                     *
! VERSION ORIGINAL PAR N.HURE : 00/04/2000                                            *
!                                                                                     *
! VERSION MODIFIEE PAR M.BORREL: 00/00/00                                             *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 24/04/08                                    *
!                                                                                     *
!                                                 				                      *
! DESCRIPTION:                                                                        *
!              Programme principal AMR monodomaine Capture d'interface 2D - 3D        *
!										                                              *
!              Le fichier de lecture est lu sous forme de caracteres qui              *
!              sont ensuite traduits                                                  *
!										                                              *
! VARIABLES:								                                	      *	
!           mot   ----------> tableau de variables caracteres contenant les mots de   *
!                             la ligne de donnee chdon                                *
!           imot  ----------> tableau contenant le nombre de lettre des variables     *
!                             caracters mot                                           *
!           nmot  ----------> nombre de mots contenus dans la ligne de donnee         *
!										                                              *
!           lec   ----------> indicatif fichier de lecture necessairement formatte    *
!                                                                                     *
!           chdon ----------> variable caractere correspondant a une ligne du fichier * 
!                             de donnee                                               *
!                                                                                     *
!**************************************************************************************

PROGRAM main

	USE  mod_hgsparam
	USE  mod_advection
	USE  mod_hgssave
	USE  mod_visu
	USE  mod_discretisation
	USE  mod_elliptique
	USE  mod_user
	IMPLICIT NONE
      

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!

	integer   :: nmot = 0, ialerte, itest = 1, ind, iter_reprise, METHELL=0

	integer   :: dbx_plot = 0, pb_def = 0, ibid = 0
	  
	character *80 chdon

	character *80 mot(20)

	integer imot(20)
	
	INTEGER nx, ny, nz, l, i

	DOUBLE PRECISION r
!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!


! INITIALISATION DES PARAMETRES GLOBALES

	INIT_BULLE_USER = 0
	INIT_ADVECTION_USER = 0
	DELTA_T_OR_CFL = 0
	MISEAJOUR_USER = 0
	CRITERE_RAF_USER = 0
	ELL_YES = 0
	iter_init = 0
	Sav_Rep = 0
	idebug = 0
	itamr = 1.e8
	itbuild = 1.e8
	itmg = 1.e8
	visu_champ_Y = 0 
	visu_champ_vitesse = 0 

	iter_paraview_Y = 1.e8 
	iter_paraview_vitesse = 1.e8 
       
         mise_jour_adv = 0 

	ILISTING = 1
	IVOLFRAC = 1
	IVITESSE = 0
        IEPAISSSEUR = 1

	CALL OUV_FILES_RES

! OUVERTURES DU FICHIER DE DONNEES

	open(NFJDD,file='TritonAMR.data',status='unknown')

! INITIALISATION DES PARAMETRES DE AMR
	call init_amr


! LECTURE D'UNE LIGNE DU FICHIER

100 read(99,'(a)') chdon
 	if (chdon(1:1).ne.'#'.and. chdon(1:1).ne.' ') then 
		write(NFSORTIE,*) chdon
		!--DECOMPOSITION DE LA LIGNE EN MOTS--!
		call part(chdon,mot,imot,nmot)
	else
		go to 100
	end if

! FIN DE LECTURE D'UNE LIGNE DU FICHIER

	ialerte = 1
 
! MOT CLE DEBUG
	if (mot(1)(1:5).eq.'DEBUG') then 
	        WRITE(NFSORTIE,*)  'Mode debug actif'
	        idebug  = 1
	        ialerte = 0
 	endif        

 

! MOT CLE DIM
	if(mot(1)(1:4).eq.'DISC') then
		WRITE(NFSORTIE,*) 'Initialisation des dimension'
		call m_dim(mot,imot,nmot)
		ialerte=0
	endif


! MOT CLE GRID
	if(mot(1)(1:4).eq.'GRID') then
		WRITE(NFSORTIE,*) 'Lecture de la grille'
		call m_grid(mot,imot,nmot)
		ialerte=0
	endif

! MOT CLE INIT
	IF (mot(1)(1:4).EQ.'INIT') THEN
		IF (mot(2)(1:5).EQ.'BULLE') THEN
		WRITE(NFSORTIE,*) 'CI FRACTION VOLUMIQUE'
		IF (mot(3)(1:4).EQ.'USER') THEN		
		ialerte = 0
		INIT_BULLE_USER = 1
		CALL US_INIT_BULLE(0)		
		ELSE
		ialerte = 0
		IF(dir(1).EQ.2) THEN	
 			CALL mc_init_bulle_2D(mot(3))
		ELSE IF (dir(1).EQ.3) THEN
 			CALL mc_init_bulle_3D(mot(3))
		END IF	
		END IF	
		END IF

		IF (mot(2)(1:9).EQ.'ADVECTION') THEN
		WRITE(NFSORTIE,*) 'CI VITESSE'
		IF (mot(3)(1:10).EQ.'ELLIPTIQUE') THEN
		ELL_YES = 1
		IF(dir(1).EQ.2) THEN	
		CALL mc_init_advection_2D(mot(4))
		ELSE IF (dir(1).EQ.3) THEN
 		CALL mc_init_advection_3D(mot(4))
		END IF	
		ELSE IF(mot(3)(1:10).EQ.'USER') THEN
		INIT_ADVECTION_USER = 1
		CALL US_INIT_ADVECTION(0)	
		ELSE
		IF(dir(1).EQ.2) THEN	
		CALL mc_init_advection_2D(mot(3))
		ELSE IF (dir(1).EQ.3) THEN
 		CALL mc_init_advection_3D(mot(3))
		END IF	
		END IF	
		ialerte = 0
		itest = 0	
		END IF
	END IF

! MOT CLE DELTA_T_OR_CFL
	if(mot(1)(1:14).eq.'DELTA_T_OR_CFL') then
		call entier(ind,mot(2),imot(2))
		if(ind == 2.and.itest == 0) then
			DELTA_T_OR_CFL = 2
			write(NFSORTIE,*) 'CFL IMPOSER'
			write(NFSORTIE,*)
		        call reel(cfl_lagoutiere,mot(3),imot(3))
			if(ELL_YES == 0) call calcul_du_pas_de_temps(1)
		else if(ind == 2.and.itest == 1.and.ELL_YES.EQ.0) then
			write(NFSORTIE,*) 'il faut initialiser dabord'
			write(NFSORTIE,*)
			stop
		else if(ind == 1) then
			DELTA_T_OR_CFL = 1
			write(NFSORTIE,*) 'DELTA T IMPOSE'
			write(NFSORTIE,*)
			call reel(dtglb,mot(3),imot(3))	
		else	
			write(NFSORTIE,*) 'Le parametre devant delta_t_Or_cfl n''est pas bon'
			stop
		end if	 		 		
		ialerte=0
	endif


! MOT CLE AMR
	if (mot(1)(1:3).eq.'AMR') then
		write(*,*) 'lecture_amr'		      
	if (mot(2)(1:4).eq.'LMAX') then
		ialerte = 0
		call entier(i,mot(3),imot(3))
		lmax = i
	end if

	if(mot(2)(1:4).eq.'METH')then
		ialerte=0
		call entier(methode,mot(3),imot(3))
	endif

	itamr   = 0
	itbuild = 999999999
	itmg    = 999999999


	if(mot(2)(1:6).eq.'GLOBAL')then
		ialerte=0
		irefine=1
	elseif(mot(2)(1:5).eq.'LOCAL')then
		ialerte=0
		irefine=0
	endif

	if(mot(2)(1:3).eq.'ISF')then
		ialerte=0
		call entier(i,mot(3),imot(3))
		isf=i
	endif


	if(mot(2)(1:5).eq.'ITGFR')then
		ialerte = 0
		call entier(i,mot(3),imot(3))
		itgfr = i
	endif

	if(mot(2)(1:3).eq.'TOL')then
		do l=0,lmax-1
			ialerte=0
			call reel(r,mot(3+l),imot(3+l))
			tol(l)=r
		enddo
	endif

	if(mot(2)(1:4).eq.'DELT')then
		do l=0,lmax-1
			ialerte=0
			call reel(r,mot(3+l),imot(3+l))
			delt(l)=r
		enddo
	endif

	if (mot(2)(1:2).eq.'RI') then
		do l = 1, lmax
			ialerte = 0
			call entier(i,mot(2+l),imot(2+l))
			ri(l) = i
		end do
	end if

	if (mot(2)(1:2).eq.'RJ') then
		do l = 1, lmax
			ialerte = 0
			call entier(i,mot(2+l),imot(2+l))
			rj(l) = i
		end do
	end if

	if (mot(2)(1:2).eq.'RK') then
		do l = 1, lmax
			ialerte = 0
			if(dir(1).eq.3) then 
				call entier(i,mot(2+l),imot(2+l))
				rk(l) = i
			else if(dir(1).eq.2) then
				rk(l) = 1
			else
				write(6,*) 'probleme rk'
				stop 
			end if
					
		end do
		call amr_navig
	end if

	if (mot(2)(1:6).eq.'DIMCUT') then
		ialerte=0
		call entier(ndimi,mot(3),imot(3))
		call entier(ndimj,mot(4),imot(4))
		if(dir(1).eq.2) ndimk = 8
		if(dir(1).eq.3) call entier(ndimk,mot(5),imot(5))
		
		nx = ndimi
		ny = ndimj
		nz = ndimk
		do l = 0, lmax-1
			nx = nx*ri(l)
			ny = ny*rj(l)
			nz = nz*rk(l)
		end do		
		
		allocate ( iflag(0:nx,0:ny,0:nz) )

	end if
	endif

! MOT CLE VISU
	if ( mot(1)(1:4).eq.'VISU') then
		if ( mot(2)(1:8) .eq.'PARAVIEW') then
			if(mot(3)(1:5) .eq.'CHAMP'.and.mot(4).eq.'Y') then
				visu_champ_Y = 1
				call entier(iter_paraview_Y,mot(5),imot(5))
			end if
			if(mot(3)(1:5) .eq.'CHAMP'.and.mot(4)(1:7).eq.'VITESSE') then
				visu_champ_vitesse = 1
				call entier(iter_paraview_vitesse,mot(5),imot(5))
			end if
		end if
		ialerte = 0        
	endif

! MOT CLE MISE_JOUR_ADV
	if ( mot(1)(1:19) .eq. 'MISEAJOUR_ADVECTION') then
		mise_jour_adv = 1
		IF (mot(2)(1:4).EQ.'USER') THEN
		MISEAJOUR_USER = 1
		ELSE
		if ( mot(2)(1:7) .eq. 'PERIODE') call reel(periode,mot(3),imot(3))
		ialerte = 0
		END IF
	END IF

! MOT CLE max_temps
	if ( mot(1)(1:9) .eq. 'MAX_TEMPS') then
		call reel(max_temps,mot(2),imot(2))
		ialerte = 0        
	endif

! MOT CLE max_temps
	if ( mot(1)(1:2) .eq. 'BC') then
		IF (mot(2)(1:4).EQ.'USER') THEN
		CALL US_BOUND_COND()
		ialerte = 0        
		ELSE		
		if ( mot(2)(1:10) .eq. 'PERIODIQUE') then
		CALL BOUND_COND()
		end if
		ialerte = 0        
		END IF
	endif

! MOT CLE RUN
	if ( mot(1)(1:3) .eq. 'RUN') then
		call entier(nitmax,mot(3),imot(3))
		call amr_verifs      
 		call m_run
		ialerte = 0        
	endif


! MOT SaveReprise
!   if(mot(1)(1:11).eq.'SaveReprise') then
! 	Sav_Rep = 1
!  	call entier(iter_save_reprise,mot(2),imot(2))
!     ialerte=0
!   endif


! MOT Reprise
!	if(mot(1)(1:7).eq.'Reprise') then
! 	    call entier(iter_reprise,mot(2),imot(2))
!		call mc_recover(iter_reprise)
!		iter_init = iter_reprise + 1 
!	ialerte=0
!	endif

! MOT CLE SCHEME
	if ( mot(1)(1:6) .eq. 'SCHEMA') then
		if(mot(2)(1:10) .eq. 'LAGOUTIERE') then
			ighc=2
			isolver=2
		end if

		if(mot(2)(1:6) .eq. 'UPWIND') then
			ighc=1
			isolver=1
		end if

		if(mot(3)(1:18) .eq. 'DIRECTION_ALTERNEE') then
			directions_alternees = 1
		else
			directions_alternees = 0
		end if

		ialerte = 0        
	endif


! MOT CLE ELLIPTIQUE
	IF (mot(1)(1:10).EQ.'ELLIPTIQUE') THEN
		IF(mot(2)(1:4).EQ.'METH') THEN
			IF(mot(3)(1:3).EQ.'LDC') THEN
				METHELL = 1;				
				ialerte = 0        
			END IF
		END IF		
	END IF


        IF (METHELL == 1) THEN 
	IF (mot(1)(1:3).EQ.'LDC') THEN
	WRITE(NFSORTIE,*) 'LECTURE PARAM LDC'

	IF ( mot(2)(1:7) .EQ. 'ITERMAX') THEN
		CALL entier(Itermax,mot(3),imot(3))
		ialerte = 0        
	END IF

	IF ( mot(2)(1:3) .EQ. 'TOL') THEN
		CALL reel(RTOL_LDC,mot(3),imot(3))
		ialerte = 0        
	END IF

	IF ( mot(2)(1:9) .EQ. 'GCITERMAX') THEN
		CALL entier(ITMAX,mot(3),imot(3))
		ialerte = 0        
	END IF

	IF ( mot(2)(1:5) .EQ. 'GCTOL') THEN
		CALL reel(RTOL,mot(3),imot(3))
		ialerte = 0        
	END IF

	END IF
	END IF

!!!!!!!!!!!!! MOT CLE END !!!!!!!!!!!!!!!
	if (mot(1)(1:3).eq.'END') go to 999
	
	if (ialerte.eq.1) then
		WRITE(NFSORTIE,721) mot(1)
		WRITE(NFSORTIE,*) ' cette instruction n''est pas correcte'
		WRITE(NFSORTIE,*)
	endif   

	go to 100


999	continue
	
	CALL FER_FILES_RES
! Fermeture finale de fichier animation.pvd


721 	format(1x,' ALERTE =====> ',a20)

	stop      
end      
