!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE : 00/00/00                                                        *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 21/05/08                                    *
!                                                                                     *
!	remplissage des cellules fictives qui se trouvent dans  le meme niveau (fin)  *     
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine l_amr_ghost_ff(l)                                   *
!                                                                                     *
! VARIABLES:									      *
!     l : level                                                                       *
!**************************************************************************************

subroutine l_amr_ghost_ff_elli(l)

 	use  mod_hgsparam
	use  mod_hgsarray

	implicit none


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!

	integer l

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES GLOBALES !!!!!!!!!!!!!!!!!
     
!!!!!!!!!!!!! DEBUT DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!

	integer m, im, iie, jje, kke, limin, limax, ljmin, ljmax, lkmin, lkmax 
	integer ijk0, ibmp, jbmp, kbmp, ig, j, k, i, ijk, ijk1, ijk2, kbump, ic, kc
	integer ind, ierr, mc, ii, jj, kk, kr, kkf, ir, iif, jjf, ijks, ibmf,jbmf,kbmf
	integer iq, jc, jr, jbump

!!!!!!!!!!!!! FIN DE DECLARATIONS DE VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	do m = 1, nga(l)		
		im = gp(l) + m

		call amr_dimdom(im,iie,jje,kke,limin,limax,ljmin,ljmax,lkmin,lkmax)

!     boundary treatment:

		call unpack_v(im,1,1,1,ijk0,ibmp,jbmp,kbmp)

		do ig=1,ighc

!     frontiere j=1
			j=1
			do k=1-ig,kke+ig
			do i=1-ig,iie+ig
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
			do k=1-ig,kke+ig
			do i=1-ig,iie+ig
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
			do k=1-ig,kke+ig
			do j=1-ig,jje+ig
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
			do k=1-ig,kke+ig
			do j=1-ig,jje+ig
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
			do i=1-ig,iie+ig
			do j=1-ig,jje+ig
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
			do i=1-ig,iie+ig
			do j=1-ig,jje+ig
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

!     external interface: i.e conditions limites : traitees par le solveur

			if (mc.gt.0) then
!     fine-fine interface:
			if (l.eq.lgrid(mc)) then
				ierr = 1
				ii = (i-ibc(mc))*ri(l) + 1
				jj = jmx(mc)
				kk = (k-kbc(mc))*rk(l) + 1

				call unpack_g(mc,ii,jj,kk,ijk0,ibmp,jbmp,kbmp)

				do kr = 0,rk(l)-1
					kkf = kc*rk(l) + kr+1
					do ir = 0,ri(l)-1
						iif = ic*ri(l) + ir+1
						do ig=1,ighc
							jjf = 1-ig
							call unpack_g(im,iif,jjf,kkf,ijks,ibmf,jbmf,kbmf)                  
							ijk=ijk0+ir*ibmp+kr*kbmp+(1-ig)*jbmp
                  
								PHI(ijks)  = PHI(ijk)
						end do
					end do
				end do
			end if 
			end if 


			mc = iktag(ind,2)

!     external interface:

			if (mc.gt.0) then
!     fine-fine interface:
			if (l.eq.lgrid(mc)) then
				ierr = 1
				ii = (i-ibc(mc))*ri(l) + 1
				jj = 1
				kk = (k-kbc(mc))*rk(l) + 1
         
				call unpack_g(mc,ii,jj,kk,ijk0,ibmp,jbmp,kbmp)
				do kr = 0,rk(l)-1
					kkf = kc*rk(l) + kr +1
					do ir = 0,ri(l)-1
						iif = ic*ri(l) + ir +1                  
						do ig=1,ighc
							jjf = jje +ig
							call unpack_g(im,iif,jjf,kkf,ijks,ibmf,jbmf,kbmf)
							ijk=ijk0+ir*ibmp+kr*kbmp+(ig-1)*jbmp

								PHI(ijks)  = PHI(ijk)
						end do
					end do   
				end do         
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
		do k = kbc(im),kec(im)
		do j = jbc(im),jec(im)
			jc   = j - jbc(im)
			kc   = k - kbc(im)
			ind = hjkptr(im) + (jc+1) + kc*kbump
			ierr = 1
			mc = jktag(ind,1)

!     external interface:

			if (mc.gt.0) then
!     fine-fine interface:
			if (l.eq.lgrid(mc)) then
				ierr = 1
				ii = imx(mc)
				jj = (j-jbc(mc))*rj(l) + 1
				kk = (k-kbc(mc))*rk(l) + 1

				call unpack_g(mc,ii,jj,kk,ijk0,ibmp,jbmp,kbmp)

				do kr = 0,rk(l)-1
					kkf = kc*rk(l) + kr+1
					do jr = 0,rj(l)-1
						jjf =jc*rj(l) + jr+1
						do ig=1,ighc
							iif=1-ig
							call unpack_g(im,iif,jjf,kkf,ijks,ibmf,jbmf,kbmf)
							ijk=ijk0+(1-ig)*ibmp+kr*kbmp+jr*jbmp

								PHI(ijks)  = PHI(ijk)

						end do
					end do
				end do

			end if 
			end if 

			mc = jktag(ind,2)

!     external interface:

			if (mc.gt.0) then
!     fine-fine interface:
			if (l.eq.lgrid(mc)) then
				ierr = 1
				ii = 1
				jj = (j-jbc(mc))*rj(l) + 1
				kk = (k-kbc(mc))*rk(l) + 1

				call unpack_g(mc,ii,jj,kk,ijk0,ibmp,jbmp,kbmp)

				do kr = 0,rk(l)-1
					kkf = kc*rk(l) + kr +1
					do jr = 0,rj(l)-1
						jjf = jc*rj(l) + jr+1
						do ig=1,ighc
							iif=iie+ig
							call unpack_g(im,iif,jjf,kkf,ijks,ibmf,jbmf,kbmf)
							ijk=ijk0+(ig-1)*ibmp+kr*kbmp+jr*jbmp
                        
								PHI(ijks)  = PHI(ijk)
						end do
					end do   
				end do         

			end if 
			end if 

			if (ierr.eq.0) then
				write(6,*) iktag(ind,1)
				write(6,*) iktag(ind,2)
				write(6,*) jktag(ind,1)
				write(6,*) jktag(ind,2)
				write(6,*) mc
				write (*,*) ' 3-4 attention, ierr=0 dans define!'
				return 
			end if 

		end do
		end do

!  connectivity information for the ij-faces 5 and 6:
!
		jbump= iec(im)-ibc(im)+2
		do j = jbc(im),jec(im)
		do i = ibc(im),iec(im)
			ic   = i - ibc(im)
			jc   = j - jbc(im)
			ind = hijptr(im) + (ic+1) + jc*jbump
			ierr = 1
			mc = ijtag(ind,1)

!     external interface:

			if (mc.gt.0) then
!     fine-fine interface:
			if (l.eq.lgrid(mc)) then
				ierr = 1
				ii = (i-ibc(mc))*ri(l) + 1
				jj = (j-jbc(mc))*rj(l) + 1
				kk = kmx(mc)

				call unpack_g(mc,ii,jj,kk,ijk0,ibmp,jbmp,kbmp)

				do jr = 0,rj(l)-1
					jjf =jc*rj(l) + jr+1
					do ir = 0,ri(l)-1
						iif = ic*ri(l) + ir+1
						do ig=1,ighc
							kkf = 1-ig
							call unpack_g(im,iif,jjf,kkf,ijks,ibmf,jbmf,kbmf)
							ijk=ijk0+ir*ibmp+(1-ig)*kbmp+jr*jbmp

								PHI(ijks)  = PHI(ijk)
						end do
					end do
				end do

			end if 
			end if 

			mc   = ijtag(ind,2)

!     external interface:
			if (mc.gt.0) then
!     fine-fine interface:
			if (l.eq.lgrid(mc)) then
				ierr = 1
				ii = (i-ibc(mc))*ri(l) + 1
				jj = (j-jbc(mc))*rj(l) + 1
				kk = 1

				call unpack_g(mc,ii,jj,kk,ijk0,ibmp,jbmp,kbmp)

				do jr = 0,rj(l)-1
					jjf = jc*rj(l) + jr+1
					do ir = 0,ri(l)-1
						iif = ic*ri(l) + ir +1
						do ig=1,ighc
							kkf = kke +ig
							call unpack_g(im,iif,jjf,kkf,ijks,ibmf,jbmf,kbmf)
							ijk=ijk0+ir*ibmp+(ig-1)*kbmp+jr*jbmp

								PHI(ijks)  = PHI(ijk)
						end do
					end do   
				end do         

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

	end do
      
      return
 998  format(1x,' define - nf4: ',12i5)
 999  format(1x,' define - im,1,nif,i,ib: ',5i5)
 997  format(1x,' x,y face1 :',13f6.3)
 996  format(1x,' x,y face3 :',13f6.3)
 995  format(1x,' x,y face4 :',13f6.3)
 994  format(1x,' x,y face2 :',13f6.3)

      return

end
