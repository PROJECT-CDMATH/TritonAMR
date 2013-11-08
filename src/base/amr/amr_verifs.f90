!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE PAR N.HURE: 13/06/00                                              *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 25/04/08                                    *
!                                                                                     *
!                   Verifications de l'AMR                                            *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine  amr_verifs                                         *
!                                                                                     *
! VARIABLES:									      *	
!										      *
!										      *
!**************************************************************************************


subroutine amr_verifs

	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
 
 	implicit none


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!

	integer idimtheo,jdimtheo,kdimtheo,pbdim  
		
!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!!


	if (idebug.eq.1) write(6,*) '+++ Debut subroutine amr_verifs...'            

!----------- Iterations : debut / fin adapatation & start MG -----------------


	itbuild = nitmax+1
	itmg    = nitmax+1
	if (idebug.eq.1) then
		write(6,*) 'nitmax  = ', nitmax
		write(6,*) 'itamr   = ', itamr
		write(6,*) 'itbuild = ', itbuild
		write(6,*) 'itmg    = ', itmg
	endif


!----------- Verification place memoire... -----------------
	if (lmax.ge.1) then

		idimtheo = 0
		jdimtheo = 0
		kdimtheo = 0  
		pbdim = 0            

		idimtheo = max(idimtheo, imax(lmax-1))
		jdimtheo = max(jdimtheo, jmax(lmax-1))
		kdimtheo = max(kdimtheo, kmax(lmax-1))

        if (idimtheo .gt. idim) pbdim = 1
        if (jdimtheo .gt. jdim) pbdim = 1  
        if (kdimtheo .gt. kdim) pbdim = 1 

        if (pbdim.eq.1) then
			write(6,*) 
			write(6,*) '!!!!! WARNING : iflag tabular !!!!!!!!!!!!!!!!!!'
			write(6,*) '-> If possible, use the following values :'
			write(6,*) '   idim = ',idimtheo
			write(6,*) '   jdim = ',jdimtheo
			write(6,*) '   kdim = ',kdimtheo
			write(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
			write(6,*)         
        endif

	endif

	if (idebug.eq.1) write(6,*) '+++ Fin subroutine amr_verifs ...'            

	return
end subroutine amr_verifs


