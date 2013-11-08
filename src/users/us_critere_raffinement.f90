!*******************************************************************
!                      Code_TritonAMR version 1.0
!                      --------------------------
!
!
!*******************************************************************
                      SUBROUTINE US_CRITERE_RAFFINEMENT &
!                     ************************
!-------------------------------------------------------------------
     ( L, N )
!-------------------------------------------------------------------
!*******************************************************************
! BUT FONCTION :
! --------------
! ROUTINE UTILISATEUR : DEFINITION DU CRITERE DE RAFFINEMENT
! Cette routine est appelee à une fréquence Imposée par
! l'utilisateur
!*******************************************************************
! HISTORIQUE :
! ------------
! VERSION ORIGINAL: 16/04/03
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 06/04/08
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
!
!
!
!*******************************************************************
!	IMPLICIT NONE
!*******************************************************************
! VARIABLES GLOBALES :
! --------------------
!
!
!*******************************************************************
! VARIABLES LOCALES :
! -------------------
!
!
!*******************************************************************

	WRITE(6,*) "IL FAUT DEFINIR LE CRITERE DE RAFFINEMENT"
	WRITE(6,*) "VOIR LES EXEMPLES DANS LE REPERTOIRE EXEMPLES"
	WRITE(6,*) "VOIR AVEC ANOUAR MEKKAS"
	STOP

END
