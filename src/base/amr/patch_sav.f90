!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE : 00/00/00                                                        *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 25/04/08                                    *
!                                                                                     *
!              defines a new patch at level l for storage                             *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine patch_sav(l,ib,ie,jb,je,kb,ke)                  *
!                                                                                     *
! VARIABLES:																		  *	
!																					  *
!																					  *
!**************************************************************************************

subroutine patch_sav(l,ib,ie,jb,je,kb,ke)

	use  mod_hgsparam
	use  mod_hgsarray
	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

	integer l,ib,ie,jb,je,kb,ke

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!

	integer im
		
!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	nga(l)    = nga(l)+ 1
	im        = gp(l) + nga(l) 

	if (im.gt.mdim) then
	 write (6,*) ' attention, mdim trop petit par rapport', &
			' au nombre de sous-maillages generes im=',im
	 write (6,*) ' modifier mdim dans le module hgsparam'
	 stop 14
	end if

	lgrid(im) = l

	ibf(im)     = (ib-1)*ri(l) + 1
	ief(im)     =     ie*ri(l)
	jbf(im)     = (jb-1)*rj(l) + 1
	jef(im)     =     je*rj(l)
	kbf(im)     = (kb-1)*rk(l) + 1
	kef(im)     =     ke*rk(l)
	imx(im)     = ief(im) - ibf(im) + 1
	jmx(im)     = jef(im) - jbf(im) + 1
	kmx(im)     = kef(im) - kbf(im) + 1
	ibc(im)     = ib
	iec(im)     = ie
	jbc(im)     = jb
	jec(im)     = je
	kbc(im)     = kb
	kec(im)     = ke
	hqptr(im+1) = hqptr(im)+(imx(im)+2*ighc)*(jmx(im)+2*ighc)*(kmx(im)+2*ighc)
	hikptr(im+1)= hikptr(im)+(ie-ib+2)*(ke-kb+2)
	hjkptr(im+1)= hjkptr(im)+(je-jb+2)*(ke-kb+2)
	hijptr(im+1)= hijptr(im)+(ie-ib+2)*(je-jb+2)

	if (hqptr(im+1).gt.la) then
	 write (6,*) ' attention, dimension la insuffisante pour', &
			' stocker le sous-maillage im=',im,hqptr(im+1)
	 write (6,*) 'la = ',la,' le modifier dans le module hgsarray'
	 stop 14
	end if
	if (hikptr(im+1).gt.lik) then
	 write (6,*) ' attention, dimension lik insuffisante pour', &
			' stocker le sous-maillage im=',im,hikptr(im+1)
	 write (6,*) ' modifier lik dans le module hgsarray'
	 stop 14
	end if
	if (hjkptr(im+1).gt.ljk) then
	 write (6,*) ' attention, dimension ljk insuffisante pour', &
			' stocker le sous-maillage im=',im,hjkptr(im+1)
	 write (6,*) ' modifier ljk dans le module hgsarray'
	 stop 14
	end if
	if (hijptr(im+1).gt.lij) then
	 write (6,*) ' attention, dimension lij insuffisante pour', &
			' stocker le sous-maillage im=',im,hijptr(im+1)
	 write (6,*) ' modifier lij dans le module hgsarray'
	 stop 14
	end if

	return

end subroutine patch_sav
