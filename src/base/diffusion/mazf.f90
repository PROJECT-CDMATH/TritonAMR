subroutine mazf(nx, ny, nz, v)


	  implicit none
	  	
	  integer nx, ny, nz
	  DOUBLE PRECISION v(nx, ny, nz)
      integer i,j,k, i1,i2,j1,j2, k1,k2


    if (nx.ne.1) then
           do k=1,nz
            do j=1,ny
                i1 = 1
                i2 = nx
                v(i1,j,k) = 0.
                v(i2,j,k) = 0.
             enddo
         enddo
    endif

    if(ny.ne.1) then
         do k=1,nz
            do i=1,nx
                j1 = 1
                j2 = ny
                v(i,j1,k) = 0.
                v(i,j2,k) = 0.
             enddo
         enddo
     endif


    if(nz.ne.1) then
         do j=1,ny
            do i=1,nx
                k1 = 1
                k2 = nz
                v(i,j,k1) = 0.
                v(i,j,k2) = 0.
             enddo
         enddo
    endif


end subroutine mazf