MODULE mod_discretisation

	INTEGER :: nb_mailles_x, nb_mailles_y, nb_mailles_z
	INTEGER :: nb_maille_totale
	INTEGER :: type_sol_init, type_adv_init 	
	INTEGER :: dir(4)			  	
	INTEGER :: ILISTING, IVOLFRAC, IVITESSE, IEPAISSSEUR
	
	DOUBLE PRECISION :: xmin, xmax, ymin, ymax, zmin, zmax
	DOUBLE PRECISION :: delta_t_lagoutiere_0
	DOUBLE PRECISION :: DELX, DELY, DELZ, delta_x_y_min
	
	PARAMETER (NFSORTIE=6, NFLISTING=11, NFVOLDE=51)
	PARAMETER (NFANIMY=101, NFANIMVIT=102, NFJDD=99)		   
	PARAMETER (NFSOLYPVD=500, NFSOLVPVD=501)
	PARAMETER (NFSOLYVTR=600 ,NFSOLVVTR=601)
	PARAMETER (NFEPAISSEUR=103)

END MODULE mod_discretisation
