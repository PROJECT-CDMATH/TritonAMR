!c
!c       _______________________      amr      _______________________  
!c      |                     hierarchical grid system                | 
!c      |                                                             |
!c      |         save the amr tree structure and flowfield values    |
!c      |_____________________________________________________________|
!c

subroutine l_save_nsc(l)


 	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

    integer l

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	integer :: m, im, ii, ne, i, j, k, ijk, ibmp, jbmp, kbmp

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	if(nga(l).gt.0) then

!c  save amr tree :

		nga_s(l) = nga(l)
		gp_s (l) = gp (l)
		do m = 1, nga(l)
			im = gp(l) + m
			ibf_s(im)  = ibf(im)
			ief_s(im)  = ief(im)
			jbf_s(im)  = jbf(im)
			jef_s(im)  = jef(im)
			kbf_s(im)  = kbf(im)
			kef_s(im)  = kef(im)
			hqptr_s(im)= hqptr(im)
		end do

!c  save conservative variables :

		do m = 1, nga(l)   
			im = gp(l) + m 
			call unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)
			do ne=1,neqt
			do k=1,kmx(im)
			do j=1,jmx(im)
			do i=1,imx(im)
				ii = ijk+(i-1)*ibmp+(j-1)*jbmp+(k-1)*kbmp
				smv(ii,ne)  = q(ii,ne) 
			enddo
			enddo
			enddo
			enddo
		enddo
	end if
	return

end
