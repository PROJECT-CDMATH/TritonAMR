!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE : 13/06/88                                                        *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 24/04/08                                    *
!                                                                                     *
!              Decomposition de chdon en nmot mots dans mot                           *
!                                                                                     *
! SYNTAXE SUBROUTINE:   subroutine part(chdon,mot,imot,nmot)                          *
!                                                                                     *
! VARIABLES:																		  *	
!           chdon ----------> variable caractere correspondant a une ligne du fichier * 
!                             de donnee                                               *
!           mot   ----------> tableau de variables caracteres contenant les mots de   *
!                             la ligne de donnee chdon                                *
!           imot  ----------> tableau contenant le nombre de lettre des variables     *
!                             caracters mot                                           *
!           nmot  ----------> nombre de mots contenus dans la ligne de donnee         *
!																					  *
!                                                                                     *
!**************************************************************************************

subroutine part(chdon,mot,imot,nmot)

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

    character *80 chdon

    character *80 mot(20)

    integer imot(20), nmot

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	integer :: i0, iblanc, ilettre, nl 

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	do i0=1,20
		mot(i0)='                                                     '
		imot(i0)=0
	end do

	nmot=0
	iblanc=1
	ilettre=1

	do nl=1,80
		if(chdon(nl:nl).ne.' ')then
			if(iblanc.eq.1)then
				iblanc=0
				nmot=nmot+1
				ilettre=1
				mot(nmot)(ilettre:ilettre)=chdon(nl:nl)
			else
				ilettre=ilettre+1
				mot(nmot)(ilettre:ilettre)=chdon(nl:nl)
			endif
		else
			if(iblanc.eq.0) then
				imot(nmot)=ilettre
				iblanc=1
			end if
		endif

	end do
				  
	return

end subroutine part

