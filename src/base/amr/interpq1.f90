      subroutine interpq1(c1,c2,ijkc,m1c,m12c,ijksc,m1f,m12f, &
           imintmp,jmintmp,kmintmp,ic,ri,rj,rk, &
           imaxtmp,jmaxtmp,kmaxtmp,dx,dy,dz, &
           x,y)
      implicit none
      integer ijkc,m1c,m12c,ijksc,m1f,m12f, &
            imintmp,jmintmp,kmintmp,imaxtmp,jmaxtmp,kmaxtmp, &
            ic,ri,rj,rk
      DOUBLE PRECISION dx,dy,dz,x(*),y(*),c1,c2
!c----------
      integer i,j,k,ii
      DOUBLE PRECISION a(8),f(8),xi,yj,zk
      
      f(1)=x(ijkc)
      f(2)=x(ijkc+1)
      f(3)=x(ijkc+m1c+1)
      f(4)=x(ijkc+m1c)
      f(5)=x(ijkc+m12c)
      f(6)=x(ijkc+m12c+1)
      f(7)=x(ijkc+m12c+m1c+1)
      f(8)=x(ijkc+m12c+m1c)
!c---- cas particuler ou la face fine est deja definie
      if(ic.eq.1) then
        f(2)=y(ijksc+ri)
        f(3)=y(ijksc+ri+rj*m1f)
        f(6)=y(ijksc+ri+rj*m1f+rk*m12f)
        f(7)=y(ijksc+ri+rk*m12f)
      endif
      if(ic.eq.2) then
        f(1)=y(ijksc)
        f(4)=y(ijksc+rj*m1f)
        f(8)=y(ijksc+rj*m1f+rk*m12f)
        f(5)=y(ijksc+rk*m12f)
      endif
      if(ic.eq.3) then
        f(4)=y(ijksc+rj*m1f)
        f(3)=y(ijksc+rj*m1f+ri)
        f(7)=y(ijksc+rj*m1f+ri+rk*m12f)
        f(8)=y(ijksc+rj*m1f+rk*m12f)
      endif
      if(ic.eq.4) then
        f(1)=y(ijksc)
        f(2)=y(ijksc+ri)
        f(6)=y(ijksc+ri+rk*m12f)
        f(5)=y(ijksc+rk*m12f)
      endif
      if(ic.eq.5) then
        f(5)=y(ijksc+rk*m12f)
        f(6)=y(ijksc+rk*m12f+ri)
        f(7)=y(ijksc+rk*m12f+ri+rj*m1f)
        f(8)=y(ijksc+rk*m12f+rj*m1f)
      endif
      if(ic.eq.6) then
        f(1)=y(ijksc)
        f(2)=y(ijksc+ri)
        f(3)=y(ijksc+ri+rj*m1f)
        f(4)=y(ijksc+rj*m1f)
      endif
!c-------------------------------------
      a(1)=f(1)
      a(2)=f(2)-f(1)
      a(3)=f(4)-f(1)
      a(4)=f(5)-f(1)
      a(5)=f(3)+f(1)-f(2)-f(4)
      a(6)=f(8)+f(1)-f(4)-f(5)
      a(7)=f(6)+f(1)-f(2)-f(5)
      a(8)=-f(1)+f(2)-f(3)+f(4)+f(5)-f(6)+f(7)-f(8)
      
!cg      do j=jmintmp,jmaxtmp
!cg      do i=imintmp,imaxtmp
!cg	do k=kmintmp,kmaxtmp      
!cg          xi  = i*dx
!cg          yj  = j*dy      
!cg          zk  = k*dz
!cg          ii=ijksc+i+j*m1f+k*m12f
!cg          y(ii)=c1*y(ii)+ c2*(a(1)+xi*a(2)+yj*a(3)+zk*a(4)+
!cg     &       xi*yj*a(5)+yj*zk*a(6)+zk*xi*a(7)+
!cg     &       xi*yj*zk*a(8))
!cg      enddo
!cg      enddo
!cg      enddo

      do k=kmintmp,kmaxtmp 
        do j=jmintmp,jmaxtmp
          do i=imintmp,imaxtmp  
            xi  = i*dx
            yj  = j*dy      
            zk  = k*dz
            ii=ijksc+i+j*m1f+k*m12f
            y(ii)=c1*y(ii)+ c2*(a(1)+xi*a(2)+yj*a(3)+zk*a(4)+ &
            xi*yj*a(5)+yj*zk*a(6)+zk*xi*a(7)+ &
            xi*yj*zk*a(8))
          enddo
        enddo
      enddo
            
      return
      end

