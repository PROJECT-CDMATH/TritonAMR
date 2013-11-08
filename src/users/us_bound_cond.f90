!*******************************************************************
!                      Code_TritonAMR version 1.0
!                      --------------------------
!
!
!*******************************************************************
                      SUBROUTINE US_BOUND_COND &
!                     ************************
!-------------------------------------------------------------------
     ( * )
!-------------------------------------------------------------------
!*******************************************************************
! BUT FONCTION :
! --------------
! ROUTINE UTILISATEUR : TRAITEMENT DES CONDITIONS AUX LIMITES
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
!*******************************************************************
	IMPLICIT NONE
!*******************************************************************
! VARIABLES GLOBALES :
! --------------------
!
!*******************************************************************
! VARIABLES LOCALES :
! -------------------
      	INTEGER   IM, KBMP, IIS, IC, KC, JC, IND, JBMP
!*******************************************************************


!*******************************************************************
!
! EXEMPLE DE TRAITEMENT DE CONDITIONS AUX LIMITES PERIODIQUES :
! -------------------------------------------------------------
! DEBUT CORPS ROUTINE :
!----------------------
!
	WRITE(6,*) "IL FAUT DEFINIR LES CL POUR L'EQUATION DE TRANSPORT"
	WRITE(6,*) "VOIR LES EXEMPLES DANS LE REPERTOIRE EXEMPLES"
	WRITE(6,*) "VOIR AVEC ANOUAR MEKKAS"
	STOP

!
! FIN CORPS ROUTINE :
!--------------------

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

END SUBROUTINE US_BOUND_COND
