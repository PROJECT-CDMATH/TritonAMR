! #######################################
! #                                     #
! # CALCUL DU COEFFICIENT DE DILATATION #
! #                                     #
! #######################################



	subroutine calcul_du_coefficient_de_dilatation(num)

	use mod_hgsparam
	use mod_discretisation
	use mod_elliptique
        use mod_advection
	implicit none


! ********************************************************************************
! ****************************************



! DECLARATION DES VARIABLES LOCALES :

	DOUBLE PRECISION :: amplitude_des_oscillations, periode_d_oscillation
	
	integer :: nombre_max_de_periodes

	integer :: k, num 

	DOUBLE PRECISION pi


! FIN DE LA DECLARATION DES VARIABLES.

		pi =4.*atan(1.)


		if (type_adv_init.eq.8) then
			! Elliptique :
			goto 8888
		else if (type_adv_init.eq.9) then
			! Elliptique :
			goto 9999
		else if (type_adv_init.eq.10) then
			! Elliptique :
			goto 1111
		else if (type_adv_init.eq.99) then
			! Elliptique :
			goto 99999
		else if (type_adv_init.eq.999) then
			! Elliptique :
			goto 12345
		else
			write(6,*) 'Le type advection est incorrecte' 
			stop
		end if

!	periode_d_oscillation=1.e9


! CALCUL DU FACTEUR DE DILATATION :
! =================================


! LOI POUR L ETUDE D OSCILLATIONS AVEC COEF. DE DILATATION = +/- 1 :
!	goto 333


! LOI POUR L ETUDE DE LA PSEUDO-RESONNANCE POUR LA BULLE EN ETOILE (fonction en cosinus) :
!	goto 444



 12345	continue

       if(time.lt.max_temps/2.0) then
	      coef_de_dilatation(num)=-1.
       else
	      coef_de_dilatation(num)=1.
	   end if
	     
	goto 10000

 8888	continue

        coef_de_dilatation(num)=1.

	goto 10000


 9999	continue

        coef_de_dilatation(num)=-1.

	goto 10000


 1111	continue


! Pour les oscillations de la cardioide :
	nombre_max_de_periodes=10
	amplitude_des_oscillations=1.0
	periode_d_oscillation=2.0

	do k=1,2*nombre_max_de_periodes

	   if (time==0.) coef_de_dilatation(num)=amplitude_des_oscillations

	if (time>(real(k)-1.)*periode_d_oscillation/2..and. &
     time<=real(k)*periode_d_oscillation/2.) then

	   coef_de_dilatation(num)=(-1.)**(k)

	end if

	end do

	goto 10000


99999	continue

	amplitude_des_oscillations=3.
	periode_d_oscillation=1.
	coef_de_dilatation(num)=amplitude_des_oscillations &
     *cos(2.*pi*time/periode_d_oscillation) &
     *cos(2.*pi*time/(periode_d_oscillation*sqrt(2.))) &
     *cos(2.*pi*time/(periode_d_oscillation*sqrt(7.)))
!     *5.*cos(2.*pi*time/periode_d_oscillation)
! Pour la resonnance sur la haute frequence :

	goto 10000


10000	continue



	end subroutine
