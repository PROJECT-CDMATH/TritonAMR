!*******************************************************************
!                      Code_TritonAMR version 1.0
!                      --------------------------
!
!
!*******************************************************************
                      SUBROUTINE FER_FILES_RES &
!                     ******************
!-------------------------------------------------------------------
     ( * )
!-------------------------------------------------------------------
!*******************************************************************
! BUT FONCTION :
! --------------
! ROUTINE UTILISATEUR : FERMETURE DES FICHIERS DES RESULTATS
!*******************************************************************
! HISTORIQUE :
! ------------
! VERSION ORIGINAL PAR A.MEKKAS : 20/06/2009 
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
	USE  mod_discretisation
!*******************************************************************
	IMPLICIT NONE
!*******************************************************************
! VARIABLES GLOBALES :
! --------------------

!*******************************************************************
! VARIABLES LOCALES :
! -------------------
 
!*******************************************************************


!*******************************************************************
!
! OUVERTURE DES FICHIERS DE RESULTATS :
! -----------------------------------------
! DEBUT CORPS ROUTINE :
!----------------------
!

	IF ( ILISTING == 1 ) THEN
		OPEN(NFLISTING,FILE="Listing")
		WRITE(NFLISTING,*) "FIN DU LISTING"
		CLOSE(NFLISTING)
	END IF

	IF ( IVOLFRAC == 1 ) THEN
		OPEN(NFANIMY,FILE="FILM_VOL_FRAC.pvd",POSITION="append")	
		WRITE(NFANIMY,"(A)",ADVANCE="no")"</Collection></VTKFile>"	
		CLOSE(NFANIMY)
	END IF

	IF ( IVITESSE == 1 ) THEN
		OPEN(NFANIMVIT,FILE="FILM_VITESSE.pvd",POSITION="append")	
		WRITE(NFANIMVIT,"(A)",ADVANCE="no")"</Collection></VTKFile>"		
		CLOSE(NFANIMVIT)
	END IF

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
END SUBROUTINE FER_FILES_RES
