!      subroutine reel(r,ch,ich)
!
!	implicit none
!
!
!     lecture d'un reel dans une variable caractere
!     version du 13/06/88
!      character *80 ch
!      character *2 chtemp
!      character *20 chtemp2
!      write(chtemp,'(i2)')ich
!      chtemp2='(e'//chtemp//'.0)'
!      read(ch,chtemp2)r
!      return
!      end


!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE : 13/06/88                                                        *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 24/04/08                                    *
!                                                                                     *
!              Lecture d'un reel dans une variable caractere                          *
!                                                                                     *
! SYNTAXE SUBROUTINE:   subroutine reel(r,ch,ich)                                     *
!                                                                                     *
! VARIABLES:																		  *	
!																					  *
!           ch    ----------> tableau de variables caracteres contenant un reel       *
!           ich   ----------> tableau contenant le nombre de lettre des variables     *
!                             caracters ch                                            *
!           r     ----------> reel, la conversion de la chaine ch                     *
!																					  *
!**************************************************************************************

subroutine reel(r,ch,ich)

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

    character *80 ch

    integer ich

    DOUBLE PRECISION r

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	character *2 chtemp

	character *20 chtemp2

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!


	write(chtemp,'(i2)')ich
	chtemp2='(e'//chtemp//'.0)'
	read(ch,chtemp2)r
	return

end subroutine reel
