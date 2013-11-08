!**************************************************************************************
!                                                                                     *
! WERSION ORIGINALE PAR A.MEKKAS - J.RYAN: 06/06/08                                   *
!                                                                                     *
!				 reconstrWction de la solWtion aWx noeWds                             *
!                    a partir des WaleWrs aWx mailles                                 *
!                                                                                     *
! SYNTAXE SWBROWTINE:  s_Wertex(im,WW_nodes,idimx,jdimx,kdimx)                        *
!     im : nWmero dW patch                                                            *
!	  idimx, jdimx, kdimx : dimensions dW patch    					                  *
!	  WW_nodes : solWtions aWx noeWds             					                  *
!                                                                                     *
!**************************************************************************************

subroutine s_vertex_W(im,WW_nodes,idimx,jdimx,kdimx)

	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_advection
	use  mod_discretisation

	implicit none

!!!!!!!!!!!!! DEBWT DE DECLARATIONS DE WARIABLES GLOBALES !!!!!!!!!!!!!!!

    integer im, idimx, jdimx, kdimx

	DOUBLE PRECISION WW_nodes(idimx,jdimx,kdimx)
	
!!!!!!!!!!!!! FIN DE DECLARATIONS DE WARIABLES GLOBALES !!!!!!!!!!!!!!!!!


!!!!!!!!!!!!! DEBWT DE DECLARATIONS DE WARIABLES LOCALES !!!!!!!!!!!!!!!!

	integer :: n1, n2, i, j, k, ii, jj, kk, i1, i2, i3, i4, i5, i6, i7, i8 

	integer :: ibmp, jbmp, kbmp,ibmp0, jbmp0, kbmp0, ijk, ijk0

	DOUBLE PRECISION :: yi1,yi2,yi3,yi4

!!!!!!!!!!!!! FIN DE DECLARATIONS DE WARIABLES LOCALES !!!!!!!!!!!!!!!!     

	n1 = idimx - 1 + 2*ighc
	n2 = jdimx - 1 + 2*ighc

	WW_nodes = 0

	do k=1,kdimx
	do j=1,jdimx
	do i=1,idimx
		if(dir(1).eq.3) then
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i3)+W(i4)+W(i5)+W(i6)+W(i7)+W(i8))/8.      
		else if(dir(1).eq.2) then
			ii = ighc+i
			jj = ighc+j
			kk = 1
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i3)+W(i4))/4.      
		end if

	end do
	end do
	end do	

! Probleme des coins

	if(dir(1).eq.2) then

		k=1
		kk = 1

	call unpack_v(im,1,1,1,ijk0,ibmp0,jbmp0,kbmp0)
	  
	  	if(y(ijk0).eq.ymin) then	  	
		j=1
		do i=2,idimx-1
			ii = ighc+i
			jj = ighc+j
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			WW_nodes(i,j,k) = (W(i1)+W(i2))/2.      
		end do
		end if

	  	if(x(ijk0).eq.xmin) then	  	
	   i=1
       do j=2,jdimx-1
		ii = ighc+i
		jj = ighc+j
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
		WW_nodes(i,j,k) = (W(i1)+W(i3))/2.      
		end do
	end if



	  	if(y(ijk0+jmx(im)*jbmp0).eq.ymax) then	  	
	   j=jdimx
      do i=2,idimx-1
		ii = ighc+i
		jj = ighc+j
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
		WW_nodes(i,j,k) = (W(i3)+W(i4))/2.      
		end do
	end if

	  	if(x(ijk0+imx(im)*ibmp0).eq.xmax) then	  	
	   i=idimx
       do j=2,jdimx-1
		ii = ighc+i
		jj = ighc+j
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
		WW_nodes(i,j,k) = (W(i2)+W(i4))/2.      
		end do
	end if

	    i=1
		j=1
		ii = ighc+i
		jj = ighc+j
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
        yi4 = .5*(2.*W(i2) - W(i2+n1) + 2.*W(i3) -W(i3+1) )
!		WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i3)+yi4)/4.      
!		WW_nodes(i,j,k) = W(i1) - WW_nodes(i+1,j,k) - WW_nodes(i,j+1,k) - WW_nodes(i+1,j+1,k)
		WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i3))/3.      
!	  	if(y(ijk0).eq.ymin.and.x(ijk0).eq.xmin) WW_nodes(i,j,k) = W(i1)      

		i=idimx
		j=1
		ii = ighc+i
		jj = ighc+j
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
        yi3=.5*( 2*W(i1) -W(i1+n1)+ 2*W(i4) -W(i4- 1) )
!		WW_nodes(i,j,k) = (W(i1)+W(i2)+yi3+W(i4))/4.      

		WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i4))/3.      
!	  	if(y(ijk0).eq.ymin.and.x(ijk0+imx(im)*ibmp0).eq.xmax) &
!			WW_nodes(i,j,k) = W(i2)      

        i=1
        j=jdimx
		ii = ighc+i
		jj = ighc+j
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
        yi2 = .5*(2*W(i1)-W(i1+1) +2*W(i4) - W(i4-n1))
!		WW_nodes(i,j,k) =  (W(i1)+yi2+W(i3)+W(i4))/4.      
		WW_nodes(i,j,k) =  (W(i1)+W(i3)+W(i4))/3.      
!	  	if(x(ijk0).eq.xmin.and.y(ijk0+jmx(im)*jbmp0).eq.ymax) &
!			WW_nodes(i,j,k) = W(i3)      


		i=idimx
		j=jdimx
		ii = ighc+i
		jj = ighc+j
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
        yi1 = .5*(2*W(i2)-W(i2-1) +2*W(i3) - W(i3-n1))
!		WW_nodes(i,j,k) = (yi1+W(i2)+W(i3)+W(i4))/4.      
		WW_nodes(i,j,k) = (W(i2)+W(i3)+W(i4))/3.      
!	  	if(x(ijk0+imx(im)*ibmp0).eq.xmax.and.y(ijk0+jmx(im)*jbmp0).eq.ymax) &
!			WW_nodes(i,j,k) = W(i4)      
	end if


	if(dir(1).eq.3) then

		k = 1
		j = 1
		do i=2,idimx-1
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i3)+W(i4))/4.      
		end do


		k = 1
		i = 1
		do j=2,jdimx-1
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i3)+W(i4))/4.      
		end do

		k = 1
		j = jdimx
		do i=2,idimx-1
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i3)+W(i4))/4.      
		end do

		k = 1
		i = idimx
		do j=2,jdimx-1
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i3)+W(i4))/4.      
		end do

		k = kdimx
		j = 1
		do i=2,idimx-1
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i5)+W(i6)+W(i7)+W(i8))/4.      
		end do

		k = kdimx
		i = 1
		do j=2,jdimx-1
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i5)+W(i6)+W(i7)+W(i8))/4.      
		end do

		k = kdimx
		j = jdimx
		do i=2,idimx-1
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i5)+W(i6)+W(i7)+W(i8))/4.      
		end do

		k = kdimx
		i = idimx
		do j=2,jdimx-1
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i5)+W(i6)+W(i7)+W(i8))/4.      
		end do

		i=1
		j=1
		do k=2,kdimx-1
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i3)+W(i5)+W(i6)+W(i7))/6.      
		end do	


		i=idimx
		j=1
		do k=2,kdimx-1
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i4)+W(i5)+W(i6)+W(i8))/6.      
		end do	

		do k=2,kdimx-1
			i=1
			j=jdimx
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i1)+W(i3)+W(i4)+W(i5)+W(i7)+W(i8))/6.      
		end do	

		do k=2,kdimx-1
			i=idimx
			j=jdimx
			ii = ighc+i
			jj = ighc+j
			kk = ighc+k
			i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
			i2=i1-1
			i3=i1-n1
			i4=i3-1
			i5=i1-(n1*n2)
			i6=i2-(n1*n2)
			i7=i3-(n1*n2)
			i8=i4-(n1*n2)
			WW_nodes(i,j,k) = (W(i2)+W(i3)+W(i4)+W(i6)+W(i7)+W(i8))/6.      
		end do	

!!!!!!!!! OK COIN !!!!!!!!!!!!!!!
		k=1
		j=1
		i=1
		ii = ighc+i
		jj = ighc+j
		kk = ighc+k
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
		i5=i1-(n1*n2)
		i6=i2-(n1*n2)
		i7=i3-(n1*n2)
		i8=i4-(n1*n2)
		WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i3))/3.      

		k=1
		j=1
		i=idimx
		ii = ighc+i
		jj = ighc+j
		kk = ighc+k
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
		i5=i1-(n1*n2)
		i6=i2-(n1*n2)
		i7=i3-(n1*n2)
		i8=i4-(n1*n2)
		WW_nodes(i,j,k) = (W(i1)+W(i2)+W(i4))/3.      


		k=1
		j=jdimx
        i=1
		ii = ighc+i
		jj = ighc+j
		kk = ighc+k
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
		i5=i1-(n1*n2)
		i6=i2-(n1*n2)
		i7=i3-(n1*n2)
		i8=i4-(n1*n2)
		WW_nodes(i,j,k) = (W(i1)+W(i3)+W(i4))/3.      


		k=1
		j=jdimx
		i=idimx
		ii = ighc+i
		jj = ighc+j
		kk = ighc+k
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
		i5=i1-(n1*n2)
		i6=i2-(n1*n2)
		i7=i3-(n1*n2)
		i8=i4-(n1*n2)
		WW_nodes(i,j,k) = (W(i2)+W(i3)+W(i4))/3.      



		k=kdimx
		j=1
		i=1
		ii = ighc+i
		jj = ighc+j
		kk = ighc+k
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
		i5=i1-(n1*n2)
		i6=i2-(n1*n2)
		i7=i3-(n1*n2)
		i8=i4-(n1*n2)
		WW_nodes(i,j,k) = (W(i5)+W(i6)+W(i7))/3.      

		k=kdimx
		j=1
		i=idimx
		ii = ighc+i
		jj = ighc+j
		kk = ighc+k
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
		i5=i1-(n1*n2)
		i6=i2-(n1*n2)
		i7=i3-(n1*n2)
		i8=i4-(n1*n2)
		WW_nodes(i,j,k) = (W(i5)+W(i6)+W(i8))/3.      


		j=jdimx
		i=1
		ii = ighc+i
		jj = ighc+j
		kk = ighc+k
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
		i5=i1-(n1*n2)
		i6=i2-(n1*n2)
		i7=i3-(n1*n2)
		i8=i4-(n1*n2)
		WW_nodes(i,j,k) = (W(i5)+W(i7)+W(i8))/3.      


		j=jdimx
		i=idimx
		ii = ighc+i
		jj = ighc+j
		kk = ighc+k
		i1=n1*(jj-1)+(n1*n2)*(kk-1)+ii
		i2=i1-1
		i3=i1-n1
		i4=i3-1
		i5=i1-(n1*n2)
		i6=i2-(n1*n2)
		i7=i3-(n1*n2)
		i8=i4-(n1*n2)
		WW_nodes(i,j,k) = (W(i6)+W(i7)+W(i8))/3.      

	end if

	return
end

