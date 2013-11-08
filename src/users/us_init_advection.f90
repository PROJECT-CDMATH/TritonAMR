!*******************************************************************
!                      Code_TritonAMR version 1.0
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
	USE  mod_discretisation
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
! -----------------------------------------------------
! DEBUT CORPS ROUTINE :
!----------------------
!
	WRITE(6,*) "IL FAUT DEFINIR LA VITESSE POUR L'EQUATION DE TRANSPORT"
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
END SUBROUTINE US_INIT_ADVECTION
