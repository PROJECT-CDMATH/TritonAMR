subroutine amr_driv_LDC

	use  mod_hgsparam
	use  mod_hgsarray
        use  mod_discretisation
	USE  mod_elliptique

	implicit none

	integer :: idimxc,jdimxc,kdimxc, im, m, l, NC, NF,ijdimc,ijdimf	     
	integer :: i,j,k,iT, ii, jj, ijkc, ibmpc, jbmpc,kbmpc
	integer :: ijkf, ibmpf, jbmpf,kbmpf,idimxf,jdimxf,kdimxf
	DOUBLE PRECISION, allocatable, dimension(:,:,:) :: R1, R2, S0, S1, SS,PHILOCC,PHILOCF 
	DOUBLE PRECISION XMoy, ERR, DNRM2, adim,a, xxx
	Integer IUNIT,ITOL,IERR,IITER
    	integer ib, ie, jb, je, kb, ke, direc, k0, k00
	integer MAX_IT_C, MAX_IT_F
	DOUBLE PRECISION MAX_ERR_C, MAX_ERR_F
!---------------------------------
!  Initialisation
!--------------------------------
	if(dir(1).eq.3) direc = 3
	if(dir(1).eq.2) direc = 2  

    
	idimxc = imx(1) 
	jdimxc = jmx(1) 
	ijdimc = idimxc*jdimxc
	if(dir(1).eq.3) kdimxc = kmx(1)
	if(dir(1).eq.2) kdimxc = 1  
	NC = idimxc*jdimxc*kdimxc

	allocate( R1(idimxc,jdimxc,kdimxc) )
	allocate( R2(idimxc,jdimxc,kdimxc) )
	allocate( S0(idimxc,jdimxc,kdimxc) )
	allocate( SS(idimxc,jdimxc,kdimxc) )
	allocate( PHILOCC(idimxc,jdimxc,kdimxc) )


	R1 = 0.
	R2 = 0.

!-----------------------------------
!  parametres pour la resol. de DCG
!--------------------------------	
	ITOL = 1
	IUNIT = 0

!---------------------------------
!  Calcul du  second membre iitial
!--------------------------------
   call amr_SndMembre
   
   call amr_loc(1,1,1,1, idimxc,jdimxc,kdimxc,1,idimxc, 1,jdimxc,1,kdimxc, SM, S0)

    adim =  DNRM2 ( NC, S0, 1)
    adim = 1./adim
!-------------------------------------------
! Debut iter du LDC
!-------------------------------------------
    MAX_IT_C  = 0
    MAX_ERR_C = 0.0 
    MAX_IT_F  = 0
    MAX_ERR_F = 0.0 
    do iT=1,Itermax

! Resolution sur le grossier avec gradient Conjuge modifie et avec correction: 
! Pour i=1 R1 = 0 et donc pas de correction
		
        call amr_loc(1,1,1,1, idimxc,jdimxc,kdimxc,1,idimxc, 1,jdimxc,1,kdimxc, PHI, PHILOCC)

        SS = S0 - R1


		XMoy = sum(PHILOCC)/NC
        PHILOCC = PHILOCC - XMoy 

		call DCG(NC,SS,PHILOCC,ITOL, RTOL, ITMAX, IITER, ERR, IERR, IUNIT,&
		imx(1),jmx(1),kmx(1),2,0,direc)
 
 	  if(  IITER .GT. MAX_IT_C ) then
		MAX_IT_C = IITER
		MAX_ERR_C = ERR
	  end if	
		

 	  if(  ERR .GT. RTOL) then
			write(6,*) 'PAS DE CONVERGENCE DCG GROSSIER'
			write(6,*) 'LE RESIDU = ', ERR
			write(6,*) 'Nombre It = ', IITER
			return
		end if

        call loc_amr(1,1,1,1, idimxc,jdimxc,kdimxc,1,idimxc, 1,jdimxc,1,kdimxc, PHILOCC, PHI)



		if(lmax.eq.0) goto 999
!------------------------------------------
!Valable pour lmax=1
!------------------------------------------
! Conditions aux limites pour le fin
		call amr_trans_elli(0,1)

! Resolution sur les patchs fins avec gradient Conjuge classique

		l= 1
		call  l_amr_ghost_fc_elli(l)
		call  l_amr_ghost_ff_elli(l)

		do m= 1, nga(l)

			im  = gp(l) + m	

			idimxf = imx(im) +2    !Dim du patchs + 2 points dirichlet
			jdimxf = jmx(im) +2
			ijdimf = idimxf*jdimxf
			if(dir(1).eq.3) then
                kdimxf = kmx(im)+2
                k0= 0
                kb=2
                ke=kdimxf-1

            endif
			if(dir(1).eq.2) then
                 kdimxf = 1  
                 k0 = 1
                 kb=1
                 ke=1
            endif
			NF = idimxf*jdimxf*kdimxf


			allocate( S1(idimxf,jdimxf,kdimxf) )
			allocate( PHILOCF(idimxf,jdimxf,kdimxf) )

            call amr_loc(im,0,0,k0, idimxf,jdimxf,kdimxf,1,idimxf, 1,jdimxf,1,kdimxf, SM, S1)
            call amr_loc(im,0,0,k0, idimxf,jdimxf,kdimxf,1,idimxf, 1,jdimxf,1,kdimxf, PHI, PHILOCF)

            call mazf(idimxf, jdimxf, kdimxf, S1)

		    call DCG(NF,S1,PHILOCF,ITOL, RTOL, ITMAX, IITER, ERR, IERR, IUNIT, &
                     idimxf,jdimxf,kdimxf,1,l,direc)

			if(  IITER .GT. MAX_IT_F ) then
				MAX_IT_F = IITER
				MAX_ERR_F = ERR
			end if	

			  if(  ERR .GT. RTOL) then
					write(6,*) 'PAS DE CONVERGENCE DCG FIN - PATCH ',im
					write(6,*) 'LE RESIDU = ', ERR
					write(6,*) 'Nombre It = ', IITER
					return
				end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   CALL MULTMATVEC_2D(PHILOCF, RR,idimxf,jdimxf,1,1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           call loc_amr(im,1,1,1, idimxf,jdimxf,kdimxf,2,idimxf-1, 2,jdimxf-1,kb,ke, PHILOCF, PHI)


		    deallocate( S1 )
		    deallocate( PHILOCF )

		end	do
		call amr_trans_elli(1,0)

! Calcul de residu restreint


        call amr_loc(1,1,1,1, idimxc,jdimxc,kdimxc,1,idimxc, 1,jdimxc,1,kdimxc, PHI, PHILOCC)

		if(dir(1).eq.2) CALL MULTMATVEC_2D(PHILOCC, R2,idimxc,jdimxc,2,0)
		if(dir(1).eq.3) CALL MULTMATVEC_3D(PHILOCC, R2,idimxc,jdimxc,kdimxc,2,0)
 
        R2 = S0 - R2

        l=1
        SS = 0.
		do m= 1, nga(l)
			im  = gp(l) + m	
            ib = ibc(im)+1 
            ie = iec(im)-1 
            jb = jbc(im)+1 
            je = jec(im)-1 
            kb = kbc(im)+1 
            ke = kec(im)-1
            if(dir(1).eq.2) then
                kb=1
                ke=1
            endif
            SS(ib:ie,jb:je,kb:ke) = R2(ib:ie,jb:je,kb:ke)
         enddo
         R2 = SS ;

        XMoy = sum(R2)/NC
        R2 = R2 - Xmoy



        SS = R2 - R1
        ERR = DNRM2 ( NC, SS, 1)*adim
        if(  ERR .LT. RTOL_LDC) goto 999

        R1 = R2 

		end do
    999 continue
!		call amr_plt_tecplot_CELL_PHI(iter)

	deallocate( R1 )
	deallocate( R2 )
	deallocate( S0 )
	deallocate( PHILOCC )

	WRITE(*,*) 
	WRITE(*,'(a)') '+------------------------------------------------+'
	WRITE(*,*) '| Le residu LDC : ', ERR,' Iteration LDC',iT,'    |'
	WRITE(*,'(a)') '+------------------------------------------------+'
	WRITE(*,'(a)') '+------------------------------------------------+'
	WRITE(*,*) '| Le max iteration GC Grossier : ', MAX_IT_C, '     |'
	WRITE(*,*) '| avec residu                  : ', MAX_ERR_C, '    |'
	WRITE(*,'(a)') '+------------------------------------------------+'
	WRITE(*,'(a)') '+------------------------------------------------+'
	WRITE(*,*) '| Le max iteration GC Fin : ', MAX_IT_F,      '     |'
	WRITE(*,*) '| avec un residu          : ', MAX_ERR_F,     '    |'
	WRITE(*,'(a)') '+------------------------------------------------+'
    if(  ERR .GT. RTOL_LDC) then
		write(6,*) 'PAS DE CONVERGENCE LDC'
		write(6,*) 'LE RESIDU = ', ERR
		write(6,*) 'Nombre It = ', iT
		return
	end if

	call amr_calcul_grad_phi_2D

end subroutine amr_driv_LDC
