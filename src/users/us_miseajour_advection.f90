!*******************************************************************
!                      Code_TritonAMR version 1.0
!                      --------------------------
!
!
!*******************************************************************
                      SUBROUTINE US_MISEAJOUR_ADVECTION &
!                     *********************************
!-------------------------------------------------------------------
     ( L, IM , TT)
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
! VERSION ORIGINALE: 14/06/88
! VERSION MODIFIEE PAR N.HURE: 14/09/99
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 16/04/08
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
	USE  mod_discretisation
!*******************************************************************
	IMPLICIT NONE
!*******************************************************************
! VARIABLES GLOBALES :
! --------------------
	INTEGER IM, L
	DOUBLE PRECISION TT
!*******************************************************************
! VARIABLES LOCALES :
! -------------------
!       	INTEGER   I, J, K, II, II0, IJK, IJK0, NE
! 	INTEGER   IBMP, JBMP, KBMP, IBMP0, JBMP0, KBMP0 
! 	DOUBLE PRECISION RAYON_1, RAYON_2, P, PI, DELTA_X, DELTA_Y
!*******************************************************************


!*******************************************************************
! -----------------------------------------
! DEBUT CORPS ROUTINE :
!----------------------

	WRITE(6,*) "IL FAUT DEFINIR LA MISE A JOUR DE LA VITESSE"
	WRITE(6,*) "VOIR LES EXEMPLES DANS LE REPERTOIRE EXEMPLES"
	WRITE(6,*) "VOIR AVEC ANOUAR MEKKAS"
	STOP
 
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
END SUBROUTINE US_MISEAJOUR_ADVECTION
