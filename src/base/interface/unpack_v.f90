!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE : 00/00/00                                                        *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 20/05/08                                    *
!                                                                                     *
!              envoie l'emplacement dans le tableau q 1D a partir de i, j, k          *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine unpack_v (im,i,j,k,ijk,ibmp,jbmp,kbmp)              *
!                                                                                     *
!**************************************************************************************

subroutine unpack_v (im,i,j,k,ijk,ibmp,jbmp,kbmp)

	use  mod_hgsparam 

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

	integer im,i,j,k,ijk,ibmp,jbmp,kbmp

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!

	ibmp = 1
	jbmp = imx(im) + 1 + 2*ighc
	kbmp =(imx(im) + 1 +2*ighc)*(jmx(im) + 1 +2*ighc)
	ijk  = hqptr_v(im) + (i+ighc) + (j-1+ighc)*jbmp + (k-1+ighc)*kbmp

	return
end
