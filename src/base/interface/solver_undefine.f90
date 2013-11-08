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

SUBROUTINE solver_undefine

	USE  mod_advection

	IMPLICIT NONE


	NULLIFY(YY)
	NULLIFY(U)
	NULLIFY(V)
	NULLIFY(W)

      RETURN
END SUBROUTINE
