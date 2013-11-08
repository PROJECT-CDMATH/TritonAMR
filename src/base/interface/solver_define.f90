!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE PAR N.HURE: 00/00/00                                              *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 06/05/08                                    *
!                                                                                     *
!    Remplissage des tableaux (Y,U,V,W) a partir de la structure AMR (tableau q)      *
!                                                                                     *
! SYNTAXE SUBROUTINE:  SUBROUTINE solver_define(im)                                   *
!     im : numero du patch                                                            *
!										      *
!**************************************************************************************

SUBROUTINE solver_define(im)

 	USE  mod_hgsparam
	USE  mod_advection
	USE  mod_hgsarray
	USE  mod_PointerReshape
	USE  mod_discretisation

	IMPLICIT NONE

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

    	INTEGER im

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	INTEGER :: ijkdeb, nt 

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!     

	


	IX = imx(im)+2*ighc
	JX = jmx(im)+2*ighc
	
	IF(dir(1).eq.3) then 
		KX = kmx(im)+2*ighc
		ijkdeb = hqptr(im) + 1
	ELSE IF(dir(1).eq.2) then 
		KX = 1
		ijkdeb = hqptr(im) + 1 + ighc*(IX*JX)
	ELSE
		write(6,*) 'probleme direction solver_define'
		STOP
	END IF
	
	nt = IX*JX*KX

	CALL PointerReshape21(q(ijkdeb,1), YY,nt)
	CALL PointerReshape21(q(ijkdeb,2), U,nt)
	CALL PointerReshape21(q(ijkdeb,3), V,nt)
	CALL PointerReshape21(q(ijkdeb,4), W,nt)

      RETURN
END SUBROUTINE
