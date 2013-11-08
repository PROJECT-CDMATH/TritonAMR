!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE : 00/00/00                                                        *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 11/06/08                                    *
!                                                                                     *
!							                              *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine l_build(l)                                          *
!                                                                                     *
! VARIABLES:									      *
!     l : level                                                                       *
!**************************************************************************************

subroutine l_build(l)

	use  mod_hgsparam
	use  mod_hgsarray

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

	integer l

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!
     
!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!

	integer iper, n, im

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	write(6,300) l
	write(74,101) 'Ad ',l

	nga(l)= 0
	gp(l) = gp(l-1) + nga(l-1)
	call l_flag(l-1,n)

	if (n.eq.0) write(6,*) ' attention n=0'

	iper=(100*n)/(imax(l-1)*jmax(l-1)*kmax(l-1))
	call l_cluster(l-1)
	call l_connect(l-1)

	call l_metrics(l)

	if (nga(l).eq.0) write(6,*) ' attention aucune grille ...'

	write(6,350) nga(l)

	im=gp(l)+nga(l)+1

	write(6,360) hqptr(im),hikptr(im),hjkptr(im),hijptr(im)

101   format(a3,i2)
300   format('+++ Building level ',i4)
350   format(1x,' total patches in the level :',i4)
360   format(1x,' hgs memory : la=',i10,' lik=',i10,' ljk=',i10,' lij=',i10)

	return
end
