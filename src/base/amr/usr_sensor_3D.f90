!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE PAR M.BORREL: 16/04/03                                            *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 06/04/08                                    *
!                                                                                     *
!	flager les cellules (critere gradient)                                        *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine usr_sensor_3D(l,n)                                  *
!     l : numero du patch                                                             *
!     n : nombre de cellules flagees       					      *
!**************************************************************************************

subroutine usr_sensor_3D(l,n)

	use  mod_hgsparam 
	use  mod_hgsarray	
	use  mod_hgssave

	implicit none


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

	integer l, n

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!

	integer :: m, im, ijk, ibmp, jbmp, kbmp, ijk0, ijk1, i, j, k, isensor1 
	
	DOUBLE PRECISION :: eps, gradx_YY, grady_YY, gradz_YY, grad_max_YY, sensor1

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!     

	eps = 1.e-20
      
	do m = 1, nga(l)
		im = gp(l) + m
		call unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp) 
		do k = 1, kmx(im)
			ijk1 = ijk
			do j = 1,jmx(im)	    
				ijk0 = ijk
				do i = 1,imx(im)
					gradx_YY = q(ijk+ibmp,1)-q(ijk-ibmp,1)
					grady_YY = q(ijk+jbmp,1)-q(ijk-jbmp,1)
					gradz_YY = q(ijk+kbmp,1)-q(ijk-kbmp,1)
					grad_max_YY = max(grad_max_YY,abs(gradx_YY)+abs(grady_YY)+abs(gradz_YY))
					ijk = ijk + ibmp	    
				end do
				ijk = ijk0 + jbmp
			end do
			ijk = ijk1 + kbmp
		end do
	end do

	do m = 1, nga(l)
		im= gp(l) + m
		call unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)
		n  = 0	  
		do k =1, kmx(im)
			ijk1 = ijk
			do j = 1,jmx(im)
				ijk0 = ijk
				do i = 1,imx(im)
					gradx_YY = q(ijk+ibmp,1)-q(ijk-ibmp,1)
					grady_YY = q(ijk+jbmp,1)-q(ijk-jbmp,1)
					gradz_YY = q(ijk+kbmp,1)-q(ijk-kbmp,1)

					sensor1 = (abs(gradx_YY )+abs(grady_YY )+abs(gradz_YY ))/grad_max_YY
					isensor1 = int(dim(sensor1,delt(l))/(sensor1-delt(l)-eps))
					sensor(ijk) = isensor1

					n = n + sensor(ijk)		
					ijk = ijk + ibmp
				end do 
				ijk = ijk0 + jbmp
			end do
			ijk = ijk1 + kbmp
		end do	
	end do

	write(6,*) 'Dans Niveau=',l,'Le nombre de cellules flagees=',n

	return
end
