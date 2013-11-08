      module mod_elliptique


	DOUBLE PRECISION, pointer:: PHIPHI(:)

	DOUBLE PRECISION, allocatable :: volume_de_fluide_1(:)
	DOUBLE PRECISION, allocatable :: volume_de_fluide_1_exact(:)
	DOUBLE PRECISION, allocatable :: Temps(:)
	DOUBLE PRECISION, allocatable :: coef_de_dilatation(:)
	DOUBLE PRECISION :: RTOL_LDC, RTOL
	INTEGER :: Itermax,ITMAX
       end  module mod_elliptique
