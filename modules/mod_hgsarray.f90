MODULE mod_hgsarray

	INTEGER la, lij, ljk, lik, neqt

	INTEGER, ALLOCATABLE, DIMENSION(:) ::  sensor
	INTEGER, ALLOCATABLE, DIMENSION(:,:) :: iktag, jktag, ijtag

	DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: x, y, z
	DOUBLE PRECISION, ALLOCATABLE, TARGET, DIMENSION(:,:) :: q

	DOUBLE PRECISION, ALLOCATABLE, TARGET, DIMENSION(:) :: SM

	DOUBLE PRECISION, ALLOCATABLE, TARGET, DIMENSION(:) :: PHI

	PARAMETER ( neqt=4 )

END MODULE mod_hgsarray
