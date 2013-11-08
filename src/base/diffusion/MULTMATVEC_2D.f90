subroutine MULTMATVEC_2D(W,R,Nx,Ny,IND,l)

	use mod_hgsparam

	  implicit none
	  	
	  integer Nx,Ny,i_y,i_x	,i, IND, l	  	
	  DOUBLE PRECISION W(Nx*Ny),R(Nx*Ny)	
      DOUBLE PRECISION dx,dy,dz

      dx = dxglb/float(ri(l))
      dy = dyglb/float(rj(l))
      dz = dzglb/float(rk(l))

	R = 0.

	do i_y=2,Ny-1
	do i_x=2,Nx-1

	   i=Nx*(i_y-1)+i_x

	   R(i)= ((W(i+1)-W(i))-(W(i)-W(i-1)))/dx**2 &
       + ((W(i+Nx)-W(i))-(W(i)-W(i-Nx)))/dy**2

	end do
	end do



if(IND.eq.2) then

! CALCUL DU LAPLACIEN SUR LES NOEUDS FRONTIERES :

! Frontiere Y = y_min HORS COINS :
! ================================

	i_y=1

	do i_x=2,Nx-1

	   i=Nx*(i_y-1)+i_x

	   R(i)=( (W(i+1)-W(i)) - (W(i)-W(i-1)) )/dx**2 &
        + (W(i+Nx)-W(i))/dy**2

	end do


! Frontiere Y = y_max HORS COINS :
! ================================

	i_y=Ny

	do i_x=2,Nx-1

	   i=Nx*(i_y-1)+i_x

	   R(i)=((W(i+1)-W(i))-(W(i)-W(i-1)))/dx**2 &
         + (-(W(i)-W(i-Nx)))/dy**2


	end do


! Frontiere X = x_min HORS COINS :
! ================================

	i_x=1

	do i_y=2,Ny-1

	   i=Nx*(i_y-1)+i_x

	   R(i)=((W(i+1)-W(i)))/dx**2 &
       + ((W(i+Nx)-W(i))-(W(i)-W(i-Nx)))/dy**2

	end do


! Frontiere X = x_max HORS COINS :
! ================================

	i_x=Nx

	do i_y=2,Ny-1

	   i=Nx*(i_y-1)+i_x

	   R(i)= (-(W(i)-W(i-1)))/dx**2 &
       + ((W(i+Nx)-W(i))-(W(i)-W(i-Nx)))/dy**2

	end do



! MAILLES DE COIN :
! =================


	i=1

	   R(i)= ( W(i+1)-W(i) )/dx**2 &
       + ( W(i+Nx)-W(i) )/dy**2



	i=Nx

	   R(i)= (-(W(i)-W(i-1)))/dx**2 &
       + ((W(i+Nx)-W(i)))/dy**2



	i=Nx*Ny

	   R(i)= (-(W(i)-W(i-1)))/dx**2 &
       + (-(W(i)-W(i-Nx)))/dy**2



	i=Nx*Ny-Nx+1

	   R(i)= ((W(i+1)-W(i)))/dx**2 &
        + (-(W(i)-W(i-Nx)))/dy**2
 

end if





end subroutine MULTMATVEC_2D