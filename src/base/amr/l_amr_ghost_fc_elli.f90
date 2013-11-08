!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE : 00/00/00                                                        *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 21/05/08                                    *
!                                                                                     *
!	remplissage des cellules fictives qui se trouvent dans  le grossier           *     
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine l_amr_ghost_fc(l)                                   *
!                                                                                     *
! VARIABLES:									      *
!     l : level                                                                       *
!**************************************************************************************

subroutine l_amr_ghost_fc_elli(l)

 	use  mod_hgsparam
	use  mod_hgsarray

	implicit none

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

	integer l

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!
     
!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!

	integer m1c,m2c,m3c,m1f,m2f,m3f
	integer m, im, iie, jje, kke, limin, limax, ljmin, ljmax, lkmin, lkmax 
	integer ijk0, ibmp, jbmp, kbmp, ig, j, k, i, ijk, ijk1, ijk2, kbump, ic, kc
	integer ind, ierr, mc, ii, jj, kk, ijkc, ibmc, jbmc, kbmc, iif, kbmf
	integer jc, jbump, jjf, kkf, ijkf, jbmf, ibmf	

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	do m = 1, nga(l)	
		im = gp(l) + m
		call amr_dimdom(im,iie,jje,kke,limin,limax,ljmin,ljmax,lkmin,lkmax)

!---- calcul des dimensions du patch m avec points fictifs
		m1f=iie+2*ighc
		m2f=jje+2*ighc
		m3f=kke*2*ighc

		call unpack_v(im,1,1,1,ijk0,ibmp,jbmp,kbmp)

!---------------------- boundary treatment -------------------------

		do ig=1,ighc

	!     frontiere j=1
			j=1	 
			do k=1-ig,kke+ig+1 
			do i=1-ig,iie+ig+1 
				ijk= ijk0 + (i-1)*ibmp + (j-1)*jbmp + (k-1)*kbmp-ig*jbmp
				ijk1=ijk+jbmp
				ijk2=ijk+2*jbmp
				x(ijk)=2.*x(ijk1)-x(ijk2)
				y(ijk)=2.*y(ijk1)-y(ijk2)
				z(ijk)=2.*z(ijk1)-z(ijk2)
			end do
			end do
	         
	!     frontiere j=jje
			j=jje
			do k=1-ig,kke+ig+1 
			do i=1-ig,iie+ig+1 
			        ijk= ijk0 + (i-1)*ibmp + (j-1)*jbmp + (k-1)*kbmp+ig*jbmp
				ijk1=ijk-jbmp
				ijk2=ijk-2*jbmp
				x(ijk)=2.*x(ijk1)-x(ijk2)
				y(ijk)=2.*y(ijk1)-y(ijk2)
				z(ijk)=2.*z(ijk1)-z(ijk2)
			end do
			end do

	!     frontiere i=1
			i=1 
			do k=1-ig,kke+ig+1 
			do j=1-ig,jje+ig+1
		               ijk= ijk0 + (i-1)*ibmp + (j-1)*jbmp + (k-1)*kbmp-ig*ibmp
		               ijk1=ijk+ibmp
		               ijk2=ijk+2*ibmp
		               x(ijk)=2.*x(ijk1)-x(ijk2)
		               y(ijk)=2.*y(ijk1)-y(ijk2)
		               z(ijk)=2.*z(ijk1)-z(ijk2)
			end do
			end do

	!     frontiere i=iie
		        i=iie
			do k=1-ig,kke+ig +1
			do j=1-ig,jje+ig+1
				ijk= ijk0 + (i-1)*ibmp + (j-1)*jbmp + (k-1)*kbmp+ig*ibmp
				ijk1=ijk-ibmp
				ijk2=ijk-2*ibmp
				x(ijk)=2.*x(ijk1)-x(ijk2)
				y(ijk)=2.*y(ijk1)-y(ijk2)
				z(ijk)=2.*z(ijk1)-z(ijk2)
			end do
			end do

	!     frontiere k=1
		        k=1
			do i=1-ig,iie+ig+1
			do j=1-ig,jje+ig+1
				ijk= ijk0 + (i-1)*ibmp + (j-1)*jbmp + (k-1)*kbmp-ig*kbmp
				ijk1=ijk+kbmp
				ijk2=ijk+2*kbmp
				x(ijk)=2.*x(ijk1)-x(ijk2)
				y(ijk)=2.*y(ijk1)-y(ijk2)
				z(ijk)=2.*z(ijk1)-z(ijk2)
			end do
			end do
	        
	!     frontiere k=kke
			k=kke
			do i=1-ig,iie+ig+1
			do j=1-ig,jje+ig+1
				ijk= ijk0 + (i-1)*ibmp + (j-1)*jbmp + (k-1)*kbmp+ig*kbmp
				ijk1=ijk-kbmp
				ijk2=ijk-2*kbmp
				x(ijk)=2.*x(ijk1)-x(ijk2)
				y(ijk)=2.*y(ijk1)-y(ijk2)
				z(ijk)=2.*z(ijk1)-z(ijk2)
			end do
			end do

		enddo !end ig 

!  connectivity information for the ik-faces 1 and 2:

		kbump= iec(im)-ibc(im)+2
		do k = kbc(im),kec(im)
		do i = ibc(im),iec(im)
			ic   = i - ibc(im)
			kc   = k - kbc(im)
			ind = hikptr(im) + (ic+1) + kc*kbump
			ierr = 1
			mc = iktag(ind,1)

			if (mc.gt.0) then
!  fine-coarse interface: (to improve)
!! Interpolation en temps... : 6 interfaces : 3 boucles : la premier : l 424-425
			if (l.eq.lgrid(mc)+1) then
				ierr = 1
				ii = i-ibf(mc)+1
				jj = jbc(im)-jbf(mc)
				kk = k-kbf(mc)+1

				if(jj.lt.1) then
				if(linkbk(3).eq.-1000) then
					  jj = jmx(mc)
				endif
				endif

				call unpack_g(mc,ii,jj,kk,ijkc,ibmc,jbmc,kbmc)

! calcul des dimensions du patch coarse sous-jacent
				m1c=jbmc
				m2c=kbmc/jbmc
				m3c=(hqptr(mc+1) - hqptr(mc))/kbmc

				iif = ic*ri(l) + 1
				jjf =1
				kkf = kc*rk(l) + 1
				call unpack_g(im,iif,jjf,kkf,ijkf,ibmf,jbmf,kbmf)
        
				call solver_vfin_cgradg_elli(isit(l),3,ri(l),rj(l),rk(l), &
					 hqptr(mc),ijkc,m1c,m2c,m3c, &
					 ijkf,m1f,m2f,m3f, &
					 la,la,1,ighc, &
					 PHI,PHI)
			end if 
			end if 

			mc = iktag(ind,2)

!     external interface:

			if (mc.gt.0) then

!  fine-coarse interface:
!! interpolatrion en temps...

			if (l.eq.lgrid(mc)+1) then
				ierr = 1
				ii = i-ibf(mc)+1
				jj = jec(im)-jbf(mc)+2
				kk = k-kbf(mc)+1

				if(jj.gt.jmx(mc)) then
				if(linkbk(4).eq.-1000) then
					  jj = 1
				endif
				endif

				call unpack_g(mc,ii,jj,kk,ijkc,ibmc,jbmc,kbmc)
! calcul des dimensions du patch coarse sous-jacent
				m1c=jbmc
				m2c=kbmc/jbmc
				m3c=(hqptr(mc+1) - hqptr(mc))/kbmc
				iif = ic*ri(l) + 1
				jjf =jje
				kkf = kc*rk(l) + 1
				call unpack_g(im,iif,jjf,kkf,ijkf,ibmf,jbmf,kbmf)
           
				call solver_vfin_cgradg_elli(isit(l),4,ri(l),rj(l),rk(l), &
					 hqptr(mc),ijkc,m1c,m2c,m3c, &
					 ijkf,m1f,m2f,m3f, &
					 la,la,1,ighc, &
					 PHI,PHI)

			end if 
			end if 

			if (ierr.eq.0) then
				write(6,*) iktag(ind,1)
				write(6,*) iktag(ind,2)
				write(6,*) jktag(ind,1)
				write(6,*) jktag(ind,2)
				write(6,*) mc
				write (*,*) ' 1-2 attention, ierr=0 dans define!'
				return
			end if 
		end do
		end do

!  connectivity information for the jk-faces 3 and 4:

		kbump = jec(im)-jbc(im)+2
		do j = jbc(im),jec(im)
		do k = kbc(im),kec(im)
			jc   = j - jbc(im)
			kc   = k - kbc(im)
			ind = hjkptr(im) + (jc+1) + kc*kbump
			ierr = 1
			mc = jktag(ind,1)

			if (mc.gt.0) then

!     fine-coarse interface:
!! interpolation en temps...
			if (l.eq.lgrid(mc)+1) then
				ierr = 1
  
				ii = ibc(im)-ibf(mc)
				jj = j-jbf(mc)+1
				kk = k-kbf(mc)+1


				if(ii.lt.1) then
				if(linkbk(1).eq.-1000) then
					  ii = imx(mc)
				endif
				endif

 
				call unpack_g(mc,ii,jj,kk,ijkc,ibmc,jbmc,kbmc)

! calcul des dimensions du patch coarse sous-jacent
				m1c=jbmc
				m2c=kbmc/jbmc
				m3c=(hqptr(mc+1) - hqptr(mc))/kbmc

				iif = 1   
				jjf = jc*rj(l) + 1
				kkf = kc*rk(l) + 1
				call unpack_g(im,iif,jjf,kkf,ijkf,ibmf,jbmf,kbmf)

				call solver_vfin_cgradg_elli(isit(l),1,ri(l),rj(l),rk(l), &
					 hqptr(mc),ijkc,m1c,m2c,m3c, &
					 ijkf,m1f,m2f,m3f, &
					 la,la,1,ighc, &
					 PHI,PHI)

			end if 
			end if 

			if (ierr.eq.0) then
				write(6,*) ' im,ind,mc',im,ind,mc
				write(6,*) ' linkbk(3)',linkbk(3)
				write(6,*) ' jktag(ind,1)',jktag(ind,1)
				write (*,*) ' 3 attention, ierr=0 dans define!'
				return
			end if 

			mc = jktag(ind,2)

			if (mc.gt.0) then

!     fine-coarse interface:
! interpolation en temps...

			if (l.eq.lgrid(mc)+1) then
				ierr = 1
				ii = iec(im)-ibf(mc)+2
				jj = j-jbf(mc)+1
				kk = k-kbf(mc)+1

				if(ii.gt.imx(mc)) then
				if(linkbk(2).eq.-1000) then
					  ii = 1
				endif
				endif


				call unpack_g(mc,ii,jj,kk,ijkc,ibmc,jbmc,kbmc) 

! calcul des dimensions du patch coarse sous-jacent
				m1c=jbmc
				m2c=kbmc/jbmc
				m3c=(hqptr(mc+1) - hqptr(mc))/kbmc

				iif = iie
				jjf = jc*rj(l) + 1
				kkf = kc*rk(l) + 1

				call unpack_g(im,iif,jjf,kkf,ijkf,ibmf,jbmf,kbmf)
          
				call solver_vfin_cgradg_elli(isit(l),2,ri(l),rj(l),rk(l), &
					 hqptr(mc),ijkc,m1c,m2c,m3c, &
					 ijkf,m1f,m2f,m3f, &
					 la,la,1,ighc, &
					 PHI,PHI)

			end if 
			end if 

			if (ierr.eq.0) then
				write(6,*) ' im,ind,mc',im,ind,mc
				write(6,*) ' linkbk(4)',linkbk(4)
				write(6,*) ' jktag(ind,2)',jktag(ind,2)
				write (6,*) ' 3-4 attention, ierr=0 dans define!'
				return
			end if 
		end do
		end do

!  connectivity information for the ij-faces 5 and 6:

		jbump= iec(im)-ibc(im)+2
		do j = jbc(im),jec(im)
		do i = ibc(im),iec(im)
			ic   = i - ibc(im)
			jc   = j - jbc(im)
			ind = hijptr(im) + (ic+1) + jc*jbump
			ierr = 1
			mc = ijtag(ind,1)

			if (mc.gt.0) then

!  fine-coarse interface: (to improve)
!! interpolation en temps...

			if (l.eq.lgrid(mc)+1) then
				ierr = 1
				ii = i-ibf(mc)+1
				jj = j-jbf(mc)+1
				kk = kbc(im)-kbf(mc)

 				if(kk.lt.1) then
				if(linkbk(5).eq.-1000) then
					  kk = kmx(mc)
				endif
				endif

				call unpack_g(mc,ii,jj,kk,ijkc,ibmc,jbmc,kbmc)

! calcul des dimensions du patch coarse sous-jacent
				m1c=jbmc
				m2c=kbmc/jbmc
				m3c=(hqptr(mc+1) - hqptr(mc))/kbmc

				iif = ic*ri(l) + 1
				jjf = jc*rj(l) + 1
				kkf = 1

				call unpack_g(im,iif,jjf,kkf,ijkf,ibmf,jbmf,kbmf)
          
				call solver_vfin_cgradg_elli(isit(l),5,ri(l),rj(l),rk(l), &
					 hqptr(mc),ijkc,m1c,m2c,m3c, &
					 ijkf,m1f,m2f,m3f, &
					 la,la,1,ighc, &
					 PHI,PHI)

			end if 
			end if 

			mc   = ijtag(ind,2)

			if (mc.gt.0) then

!     fine-coarse interface:
!! interpolation en temps...
			if (l.eq.lgrid(mc)+1) then
				ierr = 1
				ii = i-ibf(mc)+1
				jj = j-jbf(mc)+1
				kk = kec(im)-kbf(mc)+2

				if(kk.gt.kmx(mc)) then
				if(linkbk(6).eq.-1000) then
					  kk = 1
				endif
				endif

 
				call unpack_g(mc,ii,jj,kk,ijkc,ibmc,jbmc,kbmc)

!! calcul des dimensions du patch coarse sous-jacent
				m1c=jbmc
				m2c=kbmc/jbmc
				m3c=(hqptr(mc+1) - hqptr(mc))/kbmc

				iif = ic*ri(l) + 1
				jjf = jc*rj(l) + 1
				kkf = kke

				call unpack_g(im,iif,jjf,kkf,ijkf,ibmf,jbmf,kbmf)

				call solver_vfin_cgradg_elli(isit(l),6,ri(l),rj(l),rk(l), &
					 hqptr(mc),ijkc,m1c,m2c,m3c, &
					 ijkf,m1f,m2f,m3f, &
					 la,la,1,ighc, &
					 PHI,PHI)

			end if 
			end if 

			if (ierr.eq.0) then
				write(6,*) ijtag(ind,1)
				write(6,*) ijtag(ind,2)
				write(6,*) mc
				write (6,*) ' 5-6 attention, ierr=0 dans define!'
				return
			end if 
		end do
		end do

988  continue
	enddo

end
