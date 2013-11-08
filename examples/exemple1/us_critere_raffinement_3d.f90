!*******************************************************************
!                      Code_TritonAMR version 1.0.0
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
! ROUTINE UTILISATEUR : DEFINITION DU CRITERE DE RAFFINEMENT 3D
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
	USE  Mod_hgsparam 
	USE  Mod_hgsarray	
	USE  Mod_hgssave
!*******************************************************************
	IMPLICIT NONE
!*******************************************************************
! VARIABLES GLOBALES :
! --------------------
	INTEGER L, N
!*******************************************************************
! VARIABLES LOCALES :
! -------------------
	INTEGER M, IM, IJK, IBMP, JBMP, KBMP, IJK0, IJK1, i, j, k, ISENSOR1
	
	REAL*8 EPS, GRADX_Y, GRADY_Y, GRADZ_Y, GRAD_MAX_Y, SENSOR1
!*******************************************************************

	EPS = 1.e-20

	DO M = 1, nga(l)
		IM = gp(l) + M
		CALL unpack_g(IM,1,1,1,IJK,IBMP,JBMP,KBMP) 
		DO k = 1, kmx(IM)
			IJK1 = IJK
			DO j = 1,jmx(IM)
				IJK0 = IJK
				DO i = 1,imx(IM)
					GRADX_Y = q(IJK+IBMP,1)-q(IJK-IBMP,1)
					GRADY_Y = q(IJK+JBMP,1)-q(IJK-JBMP,1)
					GRADZ_Y = q(IJK+KBMP,1)-q(IJK-KBMP,1)
					GRAD_MAX_Y = Max(GRAD_MAX_Y,abs(GRADX_Y)+abs(GRADY_Y)+abs(GRADZ_Y))
					IJK = IJK + IBMP	    
				END DO
				IJK = IJK0 + JBMP
			END DO
			IJK = IJK1 + KBMP
		END DO
	END do

	DO M = 1, nga(l)
		IM= gp(l) + M
		CALL unpack_g(IM,1,1,1,IJK,IBMP,JBMP,KBMP)
		N  = 0	  
		DO k =1, kmx(IM)
			IJK1 = IJK
			DO j = 1,jmx(IM)
				IJK0 = IJK
				DO i = 1,imx(IM)
					GRADX_Y = q(IJK+IBMP,1)-q(IJK-IBMP,1)
					GRADY_Y = q(IJK+JBMP,1)-q(IJK-JBMP,1)
					GRADZ_Y = q(IJK+KBMP,1)-q(IJK-KBMP,1)

					SENSOR1 = (abs(GRADX_Y )+abs(GRADY_Y )+abs(GRADZ_Y ))/GRAD_MAX_Y
					ISENSOR1 = int(dim(SENSOR1,delt(l))/(SENSOR1-delt(l)-EPS))
					sensor(IJK) = ISENSOR1

					N = N + sensor(IJK)		
					IJK = IJK + IBMP
				END DO 
				IJK = IJK0 + JBMP
			END DO
			IJK = IJK1 + KBMP
		END DO	
	END DO

	write(6,*) 'Dans Niveau=',L,'Le nombre de cellules flagees=',N

	return
END
