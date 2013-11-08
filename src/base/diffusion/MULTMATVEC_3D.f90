subroutine MULTMATVEC_3D(W,R,Nx,Ny,Nz,IND,l)

	use mod_hgsparam

	  implicit none
	  	
	  integer Nx,Ny,Nz,i_z,i_y,i_x,i,IND, l	  	
	  DOUBLE PRECISION W(Nx*Ny*Nz),R(Nx*Ny*Nz)	
      DOUBLE PRECISION dx,dy,dz

      dx = dxglb/float(ri(l))
      dy = dyglb/float(rj(l))
      dz = dzglb/float(rk(l))

	R = 0.

	do i_z=2,Nz-1
	do i_y=2,Ny-1
	do i_x=2,Nx-1

	   i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= ((W(i+1)-W(i))-(W(i)-W(i-1)))/dx**2 &
       + ((W(i+Nx)-W(i))-(W(i)-W(i-Nx)))/dy**2 &
       + ((W(i+(Nx*Ny))-W(i))-(W(i)-W(i-(Nx*Ny))))/dz**2

	end do
	end do
	end do



if(IND.eq.2) then

! CALCUL DU LAPLACIEN SUR LES NOEUDS FRONTIERES :

! Frontiere Z = z_min HORS COINS :
! ================================

	i_z=1

	do i_y=2,Ny-1
	do i_x=2,Nx-1

	   i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)=( (W(i+1)-W(i)) - (W(i)-W(i-1)) )/dx**2 &
     + ((W(i+Nx)-W(i))-(W(i)-W(i-Nx)))/dy**2 &
		+ ( (W(i+(Nx*Ny))-W(i)) )/dz**2

	end do
	end do


! Frontiere Z = z_max HORS COINS :
! ================================

	i_z=Nz

	do i_z=2,Nz-1
	do i_x=2,Nx-1

	   i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)=( (W(i+1)-W(i)) - (W(i)-W(i-1)) )/dx**2 &
     + ( (W(i+Nx)-W(i))- (W(i)-W(i-Nx)) )/dy**2 &
		 + ( -(W(i)-W(i-(Nx*Ny))) )/dz**2


	end do
	end do

! Frontiere Y = y_min HORS COINS :
! ================================

	i_y=1

	do i_z=2,Nz-1
	do i_x=2,Nx-1

	   i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)=( (W(i+1)-W(i)) - (W(i)-W(i-1)) )/dx**2 &
        + (W(i+Nx)-W(i))/dy**2 &
		+ ( (W(i+(Nx*Ny))-W(i)) - (W(i)-W(i-(Nx*Ny))) )/dz**2

	end do
	end do



! Frontiere Y = y_max HORS COINS :
! ================================

	i_y=Ny

	do i_z=2,Nz-1
	do i_x=2,Nx-1

	   i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)=((W(i+1)-W(i))-(W(i)-W(i-1)))/dx**2 &
         + (-(W(i)-W(i-Nx)))/dy**2 &
		 + ( (W(i+(Nx*Ny))-W(i)) - (W(i)-W(i-(Nx*Ny))) )/dz**2


	end do
	end do


! Frontiere X = x_min HORS COINS :
! ================================

	i_x=1

	do i_z=2,Nz-1
	do i_y=2,Ny-1

	   i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)=((W(i+1)-W(i)))/dx**2 &
       + ((W(i+Nx)-W(i))-(W(i)-W(i-Nx)))/dy**2 &
		 + ( (W(i+(Nx*Ny))-W(i)) - (W(i)-W(i-(Nx*Ny))) )/dz**2

	end do
	end do


! Frontiere X = x_max HORS COINS :
! ================================

	i_x=Nx

	do i_z=2,Nz-1
	do i_y=2,Ny-1

	   i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= (-(W(i)-W(i-1)))/dx**2 &
       + ((W(i+Nx)-W(i))-(W(i)-W(i-Nx)))/dy**2 &
		 + ( (W(i+(Nx*Ny))-W(i)) - (W(i)-W(i-(Nx*Ny))) )/dz**2

	end do
	end do


	do i_z=2,Nz-1

! MAILLES DE COIN :
! =================
		i_x = 1
		i_y = 1
	    i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= ( W(i+1)-W(i) )/dx**2 &
       + ( W(i+Nx)-W(i) )/dy**2 &
       + ( (W(i+(Nx*Ny))-W(i))-(W(i)-W(i-(Nx*Ny))) )/dz**2


		i_x = Nx
		i_y = 1
	    i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= (-(W(i)-W(i-1)))/dx**2 &
       + ((W(i+Nx)-W(i)))/dy**2 &
       + ( (W(i+(Nx*Ny))-W(i))-(W(i)-W(i-(Nx*Ny))) )/dz**2


		i_x = Nx
		i_y = Ny
	    i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= (-(W(i)-W(i-1)))/dx**2 &
       + (-(W(i)-W(i-Nx)))/dy**2 &
       + ( (W(i+(Nx*Ny))-W(i))-(W(i)-W(i-(Nx*Ny))) )/dz**2

		i_x = 1
		i_y = Ny
	    i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= ((W(i+1)-W(i)))/dx**2 &
        + (-(W(i)-W(i-Nx)))/dy**2 &
       + ( (W(i+(Nx*Ny))-W(i))-(W(i)-W(i-(Nx*Ny))) )/dz**2
 
	end do

! MAILLES DE COIN :
! =================
		i_x = 1
		i_y = 1
		i_z = 1
	    i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= ( W(i+1)-W(i) )/dx**2 &
       + ( W(i+Nx)-W(i) )/dy**2 &
       + ( (W(i+(Nx*Ny))-W(i)))/dz**2


		i_x = Nx
		i_y = 1
		i_z = 1
	    i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= (-(W(i)-W(i-1)))/dx**2 &
       + ((W(i+Nx)-W(i)))/dy**2 &
       + ( (W(i+(Nx*Ny))-W(i)))/dz**2


		i_x = Nx
		i_y = Ny
		i_z = 1
	    i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= (-(W(i)-W(i-1)))/dx**2 &
       + (-(W(i)-W(i-Nx)))/dy**2 &
       + ( (W(i+(Nx*Ny))-W(i)))/dz**2

		i_x = 1
		i_y = Ny
		i_z = 1
	    i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= ((W(i+1)-W(i)))/dx**2 &
        + (-(W(i)-W(i-Nx)))/dy**2 &
       + ( (W(i+(Nx*Ny))-W(i)))/dz**2
 

! MAILLES DE COIN :
! =================
		i_x = 1
		i_y = 1
		i_z = Nz
	    i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= ( W(i+1)-W(i) )/dx**2 &
       + ( W(i+Nx)-W(i) )/dy**2 &
       + ( -(W(i)-W(i-(Nx*Ny))) )/dz**2


		i_x = Nx
		i_y = 1
		i_z = Nz
	    i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= (-(W(i)-W(i-1)))/dx**2 &
       + ((W(i+Nx)-W(i)))/dy**2 &
       + ( -(W(i)-W(i-(Nx*Ny))) )/dz**2


		i_x = Nx
		i_y = Ny
		i_z = Nz
	    i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= (-(W(i)-W(i-1)))/dx**2 &
       + (-(W(i)-W(i-Nx)))/dy**2 &
       + ( -(W(i)-W(i-(Nx*Ny))) )/dz**2

		i_x = 1
		i_y = Ny
		i_z = Nz
	    i=(Nx*Ny)*(i_z-1)+Nx*(i_y-1)+i_x

	   R(i)= ((W(i+1)-W(i)))/dx**2 &
        + (-(W(i)-W(i-Nx)))/dy**2 &
       + ( -(W(i)-W(i-(Nx*Ny))) )/dz**2

end if





end subroutine MULTMATVEC_3D