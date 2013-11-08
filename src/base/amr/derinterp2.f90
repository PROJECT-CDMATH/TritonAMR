      subroutine derinterp2(nfic0,indf,nfic, m,val,i1, &
			imintmp,jmintmp,kmintmp, &
			imaxtmp,jmaxtmp,kmaxtmp, &
			dx,dy,dz,rmax,gg)

	implicit none

	integer osch
	parameter(osch=2)
	integer nfic0,indf,nfic, m,i1,imintmp,jmintmp,kmintmp,imaxtmp,jmaxtmp,kmaxtmp
	integer rmax
	DOUBLE PRECISION val(8,2*osch+1), gg(rmax,rmax,rmax), dx,dy,dz


	DOUBLE PRECISION f(8), a(8) , c(osch+1,osch+1), xi,yj,zk, aaa
	integer i, j,k,j1(8),j2(8),j3(8),jj(8), ibs(2)


	data (c(1,i),i=1,3) / -3,  4,  -1/
	data (c(2,i),i=1,3) / -1  ,0  ,  1 /
	data (c(3,i),i=1,3) / 3, -4, 1 /
	
	data (j1(i),i=1,8) /1,4,5,8,2,3,6,7/
	data (j2(i),i=1,8) /1,2,5,6,4,3,8,7/
	data (j3(i),i=1,8) /1,2,3,4,5,6,7,8/

	integer ne, is(2), isens, ib0,jb0,kb0


	is(1) = 1
	is(2) = 2

!  quel schema
      if( (i1.gt.nfic+1) .and. (i1.lt.m-nfic-1)) then
           is(1) = 2
           is(2) = 2
	endif
	if( i1.eq.nfic+1) then
           is(1) = 1
           is(2) = 2     
      endif 
	if( i1.eq.m-nfic-1) then
           is(1) = 2
           is(2) = 3     
      endif 

	ib0=0
	jb0=0
	kb0=0
 ! quel ordre des points
      if( indf .eq.1 .or.indf.eq.2) then
          jj= j1
	    ib0=nfic0
	endif
      if( indf .eq.3 .or.indf.eq.4) then
          jj= j2
	    jb0=nfic0
	endif
      if( indf .eq.5 .or.indf.eq.6) then
          jj= j3 
	    kb0=nfic0
	endif

! deriv‹es
      ibs = 2*osch -is
	do i=1,4
	   f(jj(i)) = DOT_PRODUCT(c(is(1),:),val(jj(i),ibs(1):ibs(1)+osch))
	enddo 
	do i=5,8
	   f(jj(i)) = DOT_PRODUCT(c(is(2),:),val(jj(i),ibs(2):ibs(2)+osch))
	enddo 
!interpolation Q1	  
          a(1)=f(1)
          a(2)=f(2)-f(1)
          a(3)=f(4)-f(1)
          a(4)=f(5)-f(1)
          a(5)=f(3)+f(1)-f(2)-f(4)
          a(6)=f(8)+f(1)-f(4)-f(5)
          a(7)=f(6)+f(1)-f(2)-f(5)
          a(8)=-f(1)+f(2)-f(3)+f(4)+f(5)-f(6)+f(7)-f(8)

      
	if(indf.eq.1) then
	    isens=-1
	    do k=kmintmp+1,kmaxtmp+1    
          do j=jmintmp+1,jmaxtmp+1
          do i=imintmp+1,imaxtmp+1
              xi  = 1. - (i-1)*dx
              yj  = (j-1)*dy      
              zk  = (k-1)*dz
	        aaa= a(1)+xi*a(2)+yj*a(3)+zk*a(4)+ &
                     xi*yj*a(5)+yj*zk*a(6)+zk*xi*a(7)+ &
                     xi*yj*zk*a(8)
	        gg(i+ib0,j+jb0,k+kb0) = isens*aaa       ! magouille pour que la d‹termination 
                                                      ! des valeurs soient la meme pour toutes 
          enddo                                       ! les faces (isens)
	    enddo
	    enddo
	endif

	if(indf.eq.2) then
	    isens=1
	    do k=kmintmp+1,kmaxtmp+1    
          do j=jmintmp+1,jmaxtmp+1
          do i=imintmp+1,imaxtmp+1
              xi  = (i-1)*dx
              yj  = (j-1)*dy      
              zk  = (k-1)*dz
	        aaa= a(1)+xi*a(2)+yj*a(3)+zk*a(4)+ &
                     xi*yj*a(5)+yj*zk*a(6)+zk*xi*a(7)+ &
                     xi*yj*zk*a(8)
	        gg(i+ib0,j+jb0,k+kb0) = isens*aaa       ! magouille pour que la d‹termination 
                                                  ! des valeurs soient la meme pour toutes 
          enddo                                       ! les faces (isens)
	    enddo
	    enddo
	endif

	if(indf.eq.3) then
	    isens=-1
	    do k=kmintmp+1,kmaxtmp+1    
          do j=jmintmp+1,jmaxtmp+1
          do i=imintmp+1,imaxtmp+1
              xi  = (i-1)*dx
              yj  = 1. - (j-1)*dy      
              zk  = (k-1)*dz
	        aaa= a(1)+xi*a(2)+yj*a(3)+zk*a(4)+ &
                     xi*yj*a(5)+yj*zk*a(6)+zk*xi*a(7)+ &
                     xi*yj*zk*a(8)
	        gg(i+ib0,j+jb0,k+kb0) = isens*aaa       ! magouille pour que la d‹termination 
                                                      ! des valeurs soient la meme pour toutes 
          enddo                                       ! les faces (isens)
	    enddo
	    enddo
	endif

	if(indf.eq.4) then
	    isens=1
	    do k=kmintmp+1,kmaxtmp+1    
          do j=jmintmp+1,jmaxtmp+1
          do i=imintmp+1,imaxtmp+1
              xi  = (i-1)*dx
              yj  = (j-1)*dy      
              zk  = (k-1)*dz
	        aaa= a(1)+xi*a(2)+yj*a(3)+zk*a(4)+ &
                     xi*yj*a(5)+yj*zk*a(6)+zk*xi*a(7)+ &
                     xi*yj*zk*a(8)
	        gg(i+ib0,j+jb0,k+kb0) = isens*aaa       ! magouille pour que la d‹termination 
                                                  ! des valeurs soient la meme pour toutes 
          enddo                                       ! les faces (isens)
	    enddo
	    enddo
	endif

	if(indf.eq.5) then
	    isens=-1
	    do k=kmintmp+1,kmaxtmp+1    
          do j=jmintmp+1,jmaxtmp+1
          do i=imintmp+1,imaxtmp+1
              xi  = (i-1)*dx
              yj  = (j-1)*dy      
              zk  = 1. - (k-1)*dz
	        aaa= a(1)+xi*a(2)+yj*a(3)+zk*a(4)+ &
                     xi*yj*a(5)+yj*zk*a(6)+zk*xi*a(7)+ &
                     xi*yj*zk*a(8)
	        gg(i+ib0,j+jb0,k+kb0) = isens*aaa       ! magouille pour que la d‹termination 
                                                      ! des valeurs soient la meme pour toutes 
          enddo                                       ! les faces (isens)
	    enddo
	    enddo
	endif

	if(indf.eq.6) then
	    isens=1
	    do k=kmintmp+1,kmaxtmp+1    
          do j=jmintmp+1,jmaxtmp+1
          do i=imintmp+1,imaxtmp+1
              xi  = (i-1)*dx
              yj  = (j-1)*dy      
              zk  = (k-1)*dz
	        aaa= a(1)+xi*a(2)+yj*a(3)+zk*a(4)+ &
                     xi*yj*a(5)+yj*zk*a(6)+zk*xi*a(7)+ &
                     xi*yj*zk*a(8) 
	        gg(i+ib0,j+jb0,k+kb0) = isens*aaa       ! magouille pour que la d‹termination 
                                                  ! des valeurs soient la meme pour toutes 
          enddo                                       ! les faces (isens)
	    enddo
	    enddo
	endif
      
      return
      end
