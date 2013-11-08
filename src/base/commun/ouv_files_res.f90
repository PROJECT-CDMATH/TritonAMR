!*******************************************************************
!                      Code_TritonAMR version 1.0
!                      --------------------------
!
!
!*******************************************************************
                      SUBROUTINE OUV_FILES_RES &
!                     ******************
!-------------------------------------------------------------------
     ( * )
!-------------------------------------------------------------------
!*******************************************************************
! BUT FONCTION :
! --------------
! ROUTINE UTILISATEUR : OUVERTURE DES FICHIERS DES RESULTATS
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
		OPEN(NFLISTING,file="Listing")
		CLOSE(NFLISTING)
	END IF

	IF ( IVOLFRAC == 1 ) THEN
		OPEN(NFANIMY,file="FILM_VOL_FRAC.pvd")  
		WRITE(NFANIMY,"(A)",ADVANCE="no")"<?xml version=""1.0""?>"
		WRITE(NFANIMY,"(A)",ADVANCE="no")"<VTKFile type=""Collection"" version=""0.1"""
		WRITE(NFANIMY,"(A)",ADVANCE="no")" byte_order=""LittleEndian"" compressor=""vtkZLibDataCompressor"">"
		WRITE(NFANIMY,"(A)",ADVANCE="no")"<Collection>"
		CLOSE(NFANIMY)
	END IF

	IF ( IVITESSE == 1 ) THEN
		OPEN(NFANIMVIT,file="FILM_VITESSE.pvd") 
		WRITE(NFANIMVIT,"(A)",ADVANCE="no")"<?xml version=""1.0""?>"
		WRITE(NFANIMVIT,"(A)",ADVANCE="no")"<VTKFile type=""Collection"" version=""0.1"""
		WRITE(NFANIMVIT,"(A)",ADVANCE="no")" byte_order=""LittleEndian"" compressor=""vtkZLibDataCompressor"">"
		WRITE(NFANIMVIT,"(A)",ADVANCE="no")"<Collection>"
		CLOSE(NFANIMVIT)
	END IF

	IF ( IEPAISSSEUR == 1 ) THEN
		open(NFEPAISSEUR,file='EPAISSEUR_INTERFACE.DATA' ,form='formatted',status='unknown')
		CLOSE(NFEPAISSEUR)
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
END SUBROUTINE OUV_FILES_RES
