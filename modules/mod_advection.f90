MODULE mod_advection

	INTEGER                        :: IX, JX, KX
	INTEGER                        :: mise_jour_adv
	INTEGER                        :: ific(2,3), icl(2,3,2)
	INTEGER                        :: decentrage_aval_sous_contraintes
	INTEGER                        :: directions_alternees
	INTEGER                        :: TYPE_DIR

	DOUBLE PRECISION               :: periode, max_temps
	DOUBLE PRECISION               :: norme_max_de_u_lagoutiere	
	DOUBLE PRECISION               :: delta_t_lagoutiere, dtr
	DOUBLE PRECISION               :: cfl_lagoutiere
	DOUBLE PRECISION               :: V0
	DOUBLE PRECISION , POINTER     :: YY(:)
	DOUBLE PRECISION , POINTER     :: U(:)
	DOUBLE PRECISION , POINTER     :: V(:)
	DOUBLE PRECISION , POINTER     :: W(:)

	DOUBLE PRECISION , allocatable :: flux_local_x_1(:), flux_local_x_2(:) 
	DOUBLE PRECISION , allocatable :: flux_local_y_1(:), flux_local_y_2(:) 
	DOUBLE PRECISION , allocatable :: flux_local_z_1(:), flux_local_z_2(:) 
	DOUBLE PRECISION , allocatable :: flux_de_Y_1_x(:), flux_de_Y_1_y(:), flux_de_Y_1_z(:)

END MODULE mod_advection
