!	FLUX_AMR
!
!*	Cartesian grid constructor
!*
!*!	Chrono:
!*!	14.09.1999 - N.HURE - Creation
!*
      subroutine mc_cartGrid(i1,j1,k1)
!
!	implicit none
!
	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_discretisation

	im = 1

	hqptr_v(im) = 0

	delx = (xmax - xmin)/(nb_mailles_x - i1 + 1)
	dely = (ymax - ymin)/(nb_mailles_y - j1 + 1)
	delz = (zmax - zmin)/(nb_mailles_z - k1 + 1)      

	do k = k1, nb_mailles_z+1
	do j = j1, nb_mailles_y+1
	do i = i1, nb_mailles_x+1
	        call unpack_v(im,i,j,k,ijk,ibmp,jbmp,kbmp)

		x(ijk) = xmin + (i-i1)*delx
		y(ijk) = ymin + (j-j1)*dely
		z(ijk) = zmin + (k-k1)*delz

	enddo ! i
	enddo ! j
	enddo ! k


!  Mise en variables globales
	dxglb = delx
	dyglb = dely
	dzglb = delz


	if (idebug.eq.1) then
		i = i1
		j = j1
		k = k1
		call unpack_v(im,i,j,k,ijk,ibmp,jbmp,kbmp)
		write(*,*) 'en (0,0,0) x = ', x(ijk)
		write(*,*) 'en (0,0,0) y = ', y(ijk)
		write(*,*) 'en (0,0,0) z = ', z(ijk)      
		i = nb_mailles_x+1
		j = nb_mailles_y+1
		k = nb_mailles_z+1
		call unpack_v(im,i,j,k,ijk,ibmp,jbmp,kbmp)
		write(*,*) 'en (ijkmax) x = ', x(ijk)
		write(*,*) 'en (ijkmax) y = ', y(ijk)
		write(*,*) 'en (ijkmax) z = ', z(ijk)
	endif


      return
end

