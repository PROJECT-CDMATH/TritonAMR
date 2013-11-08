!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE : 00/00/00                                                        *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 15/05/08                                    *
!                                                                                     *
!                   transfers the data solution from l-grid to l1-grid                *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine amr_trans(l,l1)                                     *
!                                                                                     *
! VARIABLES:				sol					      *	
!		l,l1 : levels   						      *
!										      *
!**************************************************************************************

!subroutine ave(a,b,res) 
!
!	implicit none
!
!	DOUBLE PRECISION res,a,b
!	res = sign(1.,a)*max(0.0,min(abs(a),sign(1.,a)*b))
!	return
!end

subroutine amr_trans(l,l1)

	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave


	implicit none


!!!!!!!!!!!!! DEBUT DE DECLARATIONS DES VARIABLES GLOBALES !!!!!!!!!!!!!!!

	integer l, l1  

!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES GLOBALES !!!!!!!!!!!!!!!!!

!!!!!!!!!!!!! DEBUT DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!

	integer mf, imf, mc, imc, ie, ib, je, jb, ke, kb, ic, jc, kc
	integer i, j, k, ijk, ijkc,ibm,jbm,kbm,ne, ijkf, ibmp, jbmp, kbmp
	DOUBLE PRECISION slopei(neqt), slopej(neqt), slopek(neqt)
	integer kr, ijk1, ir, ijk0, jr,rijk, m, im, ms, ims
	integer ibms, jbms, kbms, ijk1s, ijk0s, ijks 
	DOUBLE PRECISION xi, yj, zk
	DOUBLE PRECISION :: eps
	DOUBLE PRECISION ave
	
!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	eps = 0.



	if (idebug.eq.1) then
		write(*,99) '+++ AMR : transfer hyperbolique ',l,' -> ',l1
	endif

!-------------------- Up from l to l+1 --------------------------------
	if (l1.eq.l+1) then
		write(74,100) 'Pr ',l,' -> ',l1

		do mf = 1, nga(l1)
			imf = gp(l1) + mf
			do mc = 1, nga(l)
				imc = gp(l) + mc

!       find the intersection:

				ie  = min(ief(imc),iec(imf))
				ib  = max(ibf(imc),ibc(imf))
				je  = min(jef(imc),jec(imf))
				jb  = max(jbf(imc),jbc(imf))
				ke  = min(kef(imc),kec(imf))
				kb  = max(kbf(imc),kbc(imf))

!       transfer in case of non empty intersection:

				if (ie.ge.ib .and. je.ge.jb .and. ke.ge.kb) then
					do ic = ib, ie
					do jc = jb, je
					do kc = kb, ke
						i = ic - ibf(imc) + 1
						j = jc - jbf(imc) + 1
						k = kc - kbf(imc) + 1
						call unpack_g(imc,i,j,k,ijkc,ibm,jbm,kbm)
						do ne = 1,neqt
						  slopei(ne)=ave(q(ijkc+ibm,ne)-q(ijkc,ne),q(ijkc,ne)-q(ijkc-ibm,ne))
						  slopej(ne)=ave(q(ijkc+jbm,ne)-q(ijkc,ne),q(ijkc,ne)-q(ijkc-jbm,ne))
						  slopek(ne)=ave(q(ijkc+kbm,ne)-q(ijkc,ne),q(ijkc,ne)-q(ijkc-kbm,ne))
						enddo
						i = (ic-1)*ri(l1)+1-ibf(imf)+1
						j = (jc-1)*rj(l1)+1-jbf(imf)+1
						k = (kc-1)*rk(l1)+1-kbf(imf)+1
						call unpack_g(imf,i,j,k,ijkf,ibmp,jbmp,kbmp)
						do kr= 1, rk(l1)
							ijk1 = ijkf
							do ir= 1, ri(l1)
								ijk0 = ijkf
								do jr= 1, rj(l1)
									xi  = (float(ir)-.5)/float(ri(l))-.5
									yj  = (float(jr)-.5)/float(rj(l))-.5
									zk  = (float(kr)-.5)/float(rk(l))-.5
									q(ijkf,1)=q(ijkc,1) !+xi*slopei(1)+yj*slopej(1)+zk*slopek(1)
									q(ijkf,2)=q(ijkc,2) !+xi*slopei(2)+yj*slopej(2)+zk*slopek(2)
									q(ijkf,3)=q(ijkc,3) !+xi*slopei(3)+yj*slopej(3)+zk*slopek(3)
									q(ijkf,4)=q(ijkc,4) !+xi*slopei(4)+yj*slopej(4)+zk*slopek(4)
									ijkf = ijkf + jbmp
								end do
								ijkf = ijk0 + ibmp
							end do
							ijkf = ijk1 + kbmp
						end do
					end do
					end do
					end do
				end if

			end do
		end do
	end if



!-------------------- Down from l to l-1 -------------------------------
	if (l1.eq.l-1) then
		write(74,100) 'Re ',l,' -> ',l1
		do mc = 1, nga(l1)
	        imc = gp(l1) + mc
	        do mf = 1, nga(l)
				imf = gp(l) + mf

!       find the intersection:

				ie  = min(ief(imc),iec(imf))
				ib  = max(ibf(imc),ibc(imf))
				je  = min(jef(imc),jec(imf))
				jb  = max(jbf(imc),jbc(imf))
				ke  = min(kef(imc),kec(imf))
				kb  = max(kbf(imc),kbc(imf))

!       transfer in case of non empty intersection:

				if (ie.ge.ib .and. je.ge.jb .and. ke.ge.kb) then
					do ic = ib, ie
					do jc = jb, je
					do kc = kb, ke
						i = ic - ibf(imc) + 1
						j = jc - jbf(imc) + 1
						k = kc - kbf(imc) + 1
						call unpack_g(imc,i,j,k,ijkc,ibm,jbm,kbm)
						i = (ic-1)*ri(l)+1-ibf(imf)+1
						j = (jc-1)*rj(l)+1-jbf(imf)+1
						k = (kc-1)*rk(l)+1-kbf(imf)+1
						call unpack_g(imf,i,j,k,ijkf,ibmp,jbmp,kbmp)

						do ne=1,neqt
							q(ijkc,ne) = 0.0
						enddo

						rijk = 0.
						do kr = 1,rk(l)
							ijk1 = ijkf
							do ir = 1,ri(l)
								ijk0 = ijkf
								do jr = 1,rj(l)
									q(ijkc,1) = q(ijkc,1) + q(ijkf,1)
									q(ijkc,2) = q(ijkc,2) + q(ijkf,2)
									q(ijkc,3) = q(ijkc,3) + q(ijkf,3)
									q(ijkc,4) = q(ijkc,4) + q(ijkf,4)

									rijk = rijk + 1.
									ijkf = ijkf + jbmp
								end do
								ijkf = ijk0 + ibmp
							end do
							ijkf = ijk1 + kbmp
						end do

						q(ijkc,1) = q(ijkc,1) / rijk
						q(ijkc,2) = q(ijkc,2) / rijk
						q(ijkc,3) = q(ijkc,3) / rijk
						q(ijkc,4) = q(ijkc,4) / rijk
					end do
					end do
					end do
				end if
			end do
		end do
	end if

!------------------ Shovel the values from the previous amr structure --------------

	if (l1.eq.l) then
		write(74,100) 'Up ',l,' -> ',l1
		do m = 1, nga(l)
	        im = gp(l) + m
			do ms = 1, nga_s(l)
				ims = gp_s(l) + ms

!       find the intersection:

				ie  = min(ief_s(ims),ief(im))
				ib  = max(ibf_s(ims),ibf(im))
				je  = min(jef_s(ims),jef(im))
				jb  = max(jbf_s(ims),jbf(im))
				ke  = min(kef_s(ims),kef(im))
				kb  = max(kbf_s(ims),kbf(im))

!       shovel in case of non empty intersection:

				if (ie.ge.ib .and. je.ge.jb .and. ke.ge.kb) then
					i = ib - ibf(im) + 1
					j = jb - jbf(im) + 1
					k = kb - kbf(im) + 1
					call unpack_g(im,i,j,k,ijk,ibmp,jbmp,kbmp)
					i = ib - ibf_s(ims) + 1
					j = jb - jbf_s(ims) + 1
					k = kb - kbf_s(ims) + 1
					call unpack_s(ims,i,j,k,ijks,ibms,jbms,kbms)
					do k = kb, ke
						ijk1 = ijk
						ijk1s= ijks
						do i = ib, ie
							ijk0 = ijk
							ijk0s= ijks
							do j = jb, je
								q(ijk,1) = smv(ijks,1)
								q(ijk,2) = smv(ijks,2)
								q(ijk,3) = smv(ijks,3)
								q(ijk,4) = smv(ijks,4)
								ijk = ijk + jbmp
								ijks= ijks+ jbms
							end do
							ijk = ijk0 + ibmp
							ijks= ijk0s+ ibms
						end do
						ijk = ijk1 + kbmp
						ijks= ijk1s+ kbms
					end do
			  end if
			end do
		end do
	end if

99    format(a30,i2,a4,i2)     
100   format(a3,i2,a4,i2)

	return
end
