!*******************************************************************
!                      Code_TritonAMR version 1.0.0
!                      --------------------------
!
!
!*******************************************************************
                      SUBROUTINE US_INIT_ADVECTION &
!                     ************************
!-------------------------------------------------------------------
     ( L )
!-------------------------------------------------------------------
!*******************************************************************
! BUT FONCTION :
! --------------
! ROUTINE UTILISATEUR : INITIALISATION DES VARIABLES DE CALCUL
! Cette routine est appelee en debut de calcul (suite ou non)
! avant le debut de la boucle en temps
!*******************************************************************
! HISTORIQUE :
! ------------
! VERSION ORIGINAL: 12/07/88
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 20/04/08
!*******************************************************************
! DESCRIPTION:
! ------------
!  ________________________________________________________________
! |    NOM   |TYPE | MODE | MODULE  |              ROLE             |
! |__________|_____|______|_________|_______________________________|
! |  linkbk  |T(E) |  DM  |hgsparam |                               |
! |  iec     |T(E) |  D   |hgsparam |                               |
! |  jec     |T(E) |  D   |hgsparam |                               |
! |  kec     |T(E) |  D   |hgsparam |                               |
! |  ibc     |T(E) |  D   |hgsparam |                               |
! |  jbc     |T(E) |  D   |hgsparam |                               |
! |  kbc     |T(E) |  D   |hgsparam |                               |
! |  iktag   |T(E) |  DM  |hgsarray |                               |
! |  jktag   |T(E) |  DM  |hgsarray |                               |
! |  ijtag   |T(E) |  DM  |hgsarray |                               |
! |  hikptr  |T(E) |  D   |hgsparam |                               |
! |  hjkptr  |T(E) |  D   |hgsparam |                               |
! |  hijptr  |T(E) |  D   |hgsparam |                               |
! |__________|_____|______|_________|_______________________________|
!
! TYPE : E (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!             L (LOGIQUE)
! MODE : D (DONNEE), RES (RESULTAT), DM (DONNEE MODIFIEE),
!        TT (TABLEAU DE TRAVAIL)
!*******************************************************************
!     DONNEES EN MODULES
!*******************************************************************
	USE  mod_hgsparam
	USE  mod_hgsarray
	USE  module_parametres_discretisation
!*******************************************************************
	IMPLICIT NONE
!*******************************************************************
! VARIABLES GLOBALES :
! --------------------
	INTEGER L
!*******************************************************************
! VARIABLES LOCALES :
! -------------------
      	INTEGER   IM, M, I, J, K, II, II0, IJK, IJK0, NE
	INTEGER   IBMP, JBMP, KBMP, IBMP0, JBMP0, KBMP0 
	DOUBLE PRECISION RAYON_1, RAYON_2
!*******************************************************************


!*******************************************************************
!
! EXEMPLE D'INITIALISATION D'ADVECTION (EN DIAGONALE) :
! -----------------------------------------------------
! DEBUT CORPS ROUTINE :
!----------------------
!
	DO M= 1, nga(L)
		IM = gp(L) + M

		CALL unpack_g(IM,1,1,1,IJK,IBMP,JBMP,KBMP)
		CALL unpack_v(IM,1,1,1,IJK0,IBMP0,JBMP0,KBMP0)
	
		DO K=1,1
		DO J=1,jmx(IM)
		DO I=1,imx(IM)
			II  = IJK  + (I-1)*IBMP  + (J-1)*JBMP  + (K-1)*KBMP
			q(II,2)=1.
			q(II,3)=1.
		END DO
		END DO
		END DO

! TRAITEMENT SPECIFIQUE EN 2D (CODE DIDIE 3D)
!
		DO K=1,kmx(IM)
		DO NE=2,neqt-1
		DO J=1,jmx(IM)
		DO I=1,imx(IM)
			q(IJK+(I-1)*IBMP+(J-1)*JBMP+(K-1)*KBMP,NE) = &
			q(IJK+(I-1)*IBMP+(J-1)*JBMP,NE)
		END DO
		END DO
		END DO
		END DO
	END DO
!
!----
! FORMATS
!----
!
!----
! FIN
!----
!
      RETURN
END SUBROUTINE US_INIT_ADVECTION