!*******************************************************************
!                      Code_TritonAMR version 1.0
!                      --------------------------
!
!
!*******************************************************************
                      SUBROUTINE BOUND_COND &
!                     ********************************
!-------------------------------------------------------------------
     ( * )
!-------------------------------------------------------------------
!*******************************************************************
! BUT FONCTION :
! --------------
! ROUTINE BASE : TRAITEMENT DES CONDITIONS AUX LIMITES PERIODIQUE 
! POUR L'EQUATION DE TRANSPORT DE LA FRACTION VOLUMIQUE
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
	linkbk(1) = -1000
	linkbk(2) = -1000
	linkbk(3) = -1000
	linkbk(4) = -1000
	linkbk(5) = -1000
	linkbk(6) = -1000
 
	IM = 1

	KBMP  = iec(IM)- ibc(IM)+ 2
	DO IIS=1,2
	DO IC = ibc(IM), iec(IM)
	DO KC = kbc(IM), kec(IM)
		IND = hikptr(IM) + (IC-ibc(IM)+1) + (KC-kbc(IM))*KBMP
		iktag(IND,IIS) =  1 
	END DO
	END DO
	END DO


	KBMP  = jec(IM)- jbc(IM)+ 2
	DO IIS=1,2
	DO JC = jbc(IM), jec(IM)
	DO KC = kbc(IM), kec(IM)
		ind = hjkptr(IM) + (JC-jbc(IM)+1) + (KC-kbc(IM))*KBMP
		jktag(ind,IIS) = 1 
	END DO
	END DO
	END DO


	JBMP = iec(IM)- ibc(IM)+ 2
	DO IIS=1,2
	DO IC = ibc(IM), iec(IM)
	DO JC = jbc(IM), jec(IM)
		ind = hijptr(IM) + (IC-ibc(IM)+1) + (JC-jbc(IM))*JBMP
		ijtag(ind,IIS) =  1
	END DO
	END DO
	END DO
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

END SUBROUTINE BOUND_COND
