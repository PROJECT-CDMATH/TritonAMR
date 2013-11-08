!*******************************************************************
!                      Code_TritonAMR version 1.0
!                      --------------------------
!
!
!*******************************************************************
                      SUBROUTINE LEC_JDD &
!                     ******************
!-------------------------------------------------------------------
     ( * )
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
	INTEGER IM
!*******************************************************************
! VARIABLES LOCALES :
! -------------------
      	INTEGER   I, J, K, II, II0, IJK, IJK0
	INTEGER   IBMP, JBMP, KBMP, IBMP0, JBMP0, KBMP0 
	DOUBLE PRECISION RAYON_1, RAYON_2
!*******************************************************************


!*******************************************************************
!
! EXEMPLE D'INITIALISATION (DEUX BULLES 2D) :
! -----------------------------------------
! DEBUT CORPS ROUTINE :
!----------------------
!
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
END SUBROUTINE LEC_JDD
