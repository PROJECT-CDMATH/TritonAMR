!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE : 00/00/00                                                        *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 20/05/08                                    *
!                                                                                     *
!              envoie l'emplacement dans le tableau q 1D a partir de i, j, k          *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine unpack_s (im,i,j,k,ijk,ibmp,jbmp,kbmp)              *
!                                                                                     *
!**************************************************************************************


subroutine unpack_s (im,i,j,k,ijk,ibmp,jbmp,kbmp)

	use  mod_hgsparam 
	use  mod_hgssave

	implicit none


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

	integer im,i,j,k,ijk,ibmp,jbmp,kbmp

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!

	integer imxim, jmxim

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!!


	imxim= ief_s(im) - ibf_s(im) + 1  
	jmxim= jef_s(im) - jbf_s(im) + 1  
	ibmp = 1
	jbmp = imxim + 2*ighc
	kbmp =(imxim +2*ighc)*(jmxim +2*ighc)

	ijk  = hqptr_s(im) + (i+ighc) + (j-1+ighc)*jbmp + (k-1+ighc)*kbmp


	return

end
