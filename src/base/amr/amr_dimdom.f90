!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE : 00/00/00                                                        *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 25/04/08                                    *
!                                                                                     *
!                       Calcul des dimensions du patch im                             *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine amr_dimdom(im,iie,jje,kke, &                        *
!                                            limin,limax,ljmin,ljmax,lkmin,lkmax)     *
!                                                                                     *
!**************************************************************************************

subroutine amr_dimdom(im,iie,jje,kke,limin,limax,ljmin,ljmax,lkmin,lkmax)

	use  mod_hgsparam
	use  mod_hgsarray

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DES VARIABLES GLOBALES !!!!!!!!!!!!!!!

	integer im,iie,jje,kke,limin,limax,ljmin,ljmax,lkmin,lkmax

!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES GLOBALES !!!!!!!!!!!!!!!!!

	limin=1-ighc
	limax=imx(im)+ighc

	ljmin=1-ighc
	ljmax=jmx(im)+ighc


	lkmin=1-ighc
	lkmax=kmx(im)+ighc

	iie = imx(im)
	jje = jmx(im)
	kke = kmx(im)

	return


end subroutine amr_dimdom
