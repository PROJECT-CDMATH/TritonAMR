subroutine init_hgs(in,jn,kn)
      
	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave

	implicit none

	integer in,jn,kn,ibdim


	ijdim=12000

	idim = in -1
	jdim = jn -1
	kdim = kn -1

    ibdim = 1

!         la = 9500000
          la = 25000000
	lij= 40000
	ljk=50000
	lik=50000


	allocate( x(la) )
	allocate( y(la) )
	allocate( z(la) )
	allocate( q(la, neqt) )
	allocate( SM(la) )
	allocate( PHI(la) )
	allocate( sensor(la) )
	allocate( iktag(lik,2) )
	allocate( jktag(ljk,2) )
	allocate( ijtag(lij,2) )

	
	allocate( smv(la,neqt) ) 

!	x = 0.
!	y = 0.
!	z = 0.
!	q = 0.
!	smv = 0.
!	sensor = 0
!	iktag = 0
!	jktag = 0
!	ijtag = 0
!	PHI = 0.
	return
	
end subroutine init_hgs

