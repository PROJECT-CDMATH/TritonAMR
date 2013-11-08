!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE PAR N.HURE: 10/04/00                                              *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 25/04/08                                    *
!                                                                                     *
!                   initialize amr algorithm                                          *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine amr_navig                                           *
!                                                                                     *
!										      *
!**************************************************************************************

subroutine amr_navig

	use  mod_hgsparam
	use  mod_hgsarray


	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!

	integer coef, l, i, niv, k, j, iend  
		
!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!


!------------------- Raffinement max par level --------------------------
	do l = 0, lmax
		max_raf(l) = 1
	end do

	do l = 1, lmax
		max_raf(l) = max(max_raf(l),ri(l),rj(l),rk(l))         
	end do
	
	if (idebug.eq.1) then
		write(6,*) 'ri : ',(ri(l), l=1, lmax)
		write(6,*) 'rj : ',(rj(l), l=1, lmax)
		write(6,*) 'rk : ',(rk(l), l=1, lmax)
	        do l = 1, lmax
			write(*,987) l,max_raf(l)
	        end do
	end if   

!--------------- Calcul indices maximaux / level ---------------

	do l = 1, lmax
		imax(l) = imax(l-1) * ri(l)
		jmax(l) = jmax(l-1) * rj(l)
		kmax(l) = kmax(l-1) * rk(l)

		if (idebug.eq.1) then
			write(*,*) 'l, imax : ',l,imax(l)
			write(*,*) 'l, jmax : ',l,jmax(l)
			write(*,*) 'l, kmax : ',l,kmax(l)    
			write(*,*)       
		end if
	end do          


!----------------- Calculate maxnav ----------------------------

	nesq(1) = max_raf(1) + 1
	do l = 2, lmax
		nesq(l) = 1
		do i = 1 , (l-1)
			nesq(l) = nesq(l) * max_raf(i)
		end do
		nesq(l) = nesq(l) * (max_raf(l) + 1)
	end do


	maxnav = 0
	do l = 1, lmax
		maxnav = maxnav + nesq(l)
	end do

	coef = 1
	do l = 1, lmax-1
        coef = coef*max_raf(l)
	end do
	maxnav = maxnav - coef   

!------------------- Construct navigator ---------------------------

	navigator(0) = 0
	navigator(maxnav+1) = 0

	niv = 1
	do i = 1,lmax
		niesq(i) = 0
	end do

	do k = 1,maxnav
	        navigator(k) = niv
	        niesq(niv) = niesq(niv) + 1

	        iend = 0
	        do j = niv,lmax
			if (niesq(niv).ne.(max_raf(niv) + 1)) iend = 1
	        end do

		if  (niv.eq.lmax) then     
			if (niesq(niv).eq.(max_raf(niv))) niv = niv-1
		else  
			if (iend.eq.1) then
				niv = niv + 1
				do j = niv , lmax
					niesq(j) = 0
				end do
			else
				niv = niv - 1
			end if
		end if	
	end do

!------- Construct corrector : coarse grid correction ----------------
!------------------------- = 0 for unsteady problem ------------------

	do k = 1,maxnav      
		corrector(k) = 0
	enddo

!------------ Construct ellipthor : forcing function -----------------
!------------------------- = 0 pour l'instant... ---------------------

	do k = 1,maxnav 
		ellipthor(k) = 0
	enddo

!------ Construct constructor : adaptation --------------
!-------------- a chaque descente pour l'instant --------

	do k = 1,maxnav
	        constructor(k) = 0 !!!! a remplir
	enddo

	if (idebug .eq. 1) then
		write(6,*) 
		write(6,*) 'Lmax   = ',lmax 
		write(6,*) 'maxnav = ',maxnav
		write(6,*) '+++ Navigator :'
		write(6,999) (navigator(k),k=0,maxnav+1)
	endif

200   format(1x,i5,5x,i5,5x,i5,5x,i5)
987   format('max_raf( ',i2,' ) = ',i3)
999	  format(50(i2)) 


      return
end 

