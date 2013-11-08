!c       ________________________      amr     _______________________  
!c      |                     hierarchical grid system                | 
!c      |                                                             |
!c      |           calculate mesh co-ordinates for grid level l      |
!c      |                   en "dur" : wall grid plane k=1            |
!c      |_____________________________________________________________|
!c
subroutine l_metrics(l)
!*
!*	implicit none
!*
	use  mod_hgsparam
	use  mod_hgsarray

	epm = 1.e-12

	do mc = 1, nga(l-1)
		imc = gp(l-1) + mc
		do m = 1, nga(l)
			im = gp(l) + m
			hqptr_v(im) = hqptr_v(im-1)+((imx(im-1)+1+2*ighc)*(jmx(im-1)+1+2*ighc)*(kmx(im-1)+1+2*ighc))
			ib = max(ibc(im),ibf(imc))
			ie = min(iec(im),ief(imc))
			jb = max(jbc(im),jbf(imc))
			je = min(jec(im),jef(imc))
			kb = max(kbc(im),kbf(imc))
			ke = min(kec(im),kef(imc))
			if ( ib.le.ie .and. jb.le.je .and. kb.le.ke) then
				do in = ib, ie
				do jn = jb, je
				do kn = kb, ke
					ic = in - ibf(imc) + 1
					jc = jn - jbf(imc) + 1
					kc = kn - kbf(imc) + 1
					call unpack_v(imc,ic,jc,kc,ijkc,ibmc,jbmc,kbmc)
					i = (in - ibc(im))*ri(l) + 1
					j = (jn - jbc(im))*rj(l) + 1
					k = (kn - kbc(im))*rk(l) + 1
					call unpack_v(im,i,j,k,ijk,ibmp,jbmp,kbmp)
					x1 = x(ijkc)
					y1 = y(ijkc)
					z1 = z(ijkc)
					x2 = x(ijkc+ibmc)
					y2 = y(ijkc+ibmc)
					z2 = z(ijkc+ibmc)
					x3 = x(ijkc+ibmc+jbmc)
					y3 = y(ijkc+ibmc+jbmc)
					z3 = z(ijkc+ibmc+jbmc)
					x4 = x(ijkc+jbmc)
					y4 = y(ijkc+jbmc)
					z4 = z(ijkc+jbmc)
					x5 = x(ijkc+kbmc)
					y5 = y(ijkc+kbmc)
					z5 = z(ijkc+kbmc)
					x6 = x(ijkc+kbmc+ibmc)
					y6 = y(ijkc+kbmc+ibmc)
					z6 = z(ijkc+kbmc+ibmc)
					x7 = x(ijkc+kbmc+ibmc+jbmc)
					y7 = y(ijkc+kbmc+ibmc+jbmc)
					z7 = z(ijkc+kbmc+ibmc+jbmc)
					x8 = x(ijkc+kbmc+jbmc)
					y8 = y(ijkc+kbmc+jbmc)
					z8 = z(ijkc+kbmc+jbmc)
					ijkp = ijk

					do kr = 0, rk(l)
						ijk1 = ijk
						do ir = 0, ri(l)
							ijk0 = ijk
							do jr = 0, rj(l)
								ci = float(ir) / float(ri(l))
								cj = float(jr) / float(rj(l))
								ck = float(kr) / float(rk(l))

								x(ijk)=x1 + (x2-x1)*ci +  (x4-x1)*cj +  (x5-x1)*ck + &
							   (x3-x2-x4+x1)*ci*cj + (x6-x2-x5+x1)*ci*ck + &
							   (x8-x4-x5+x1)*cj*ck + (x7+x2-x3+x4+x5-x6-x8-x1)*ci*cj*ck
								y(ijk)=y1 + (y2-y1)*ci +  (y4-y1)*cj +  (y5-y1)*ck + &
							   (y3-y2-y4+y1)*ci*cj + (y6-y2-y5+y1)*ci*ck + &
							   (y8-y4-y5+y1)*cj*ck + (y7+y2-y3+y4+y5-y6-y8-y1)*ci*cj*ck
								z(ijk)=z1 + (z2-z1)*ci +  (z4-z1)*cj +  (z5-z1)*ck + &
							   (z3-z2-z4+z1)*ci*cj + (z6-z2-z5+z1)*ci*ck + &
							   (z8-z4-z5+z1)*cj*ck + (z7+z2-z3+z4+z5-z6-z8-z1)*ci*cj*ck

								ijk = ijk + jbmp
							end do
							ijk = ijk0 + ibmp
						end do
						ijk = ijk1 + kbmp
					end do
				end do
				end do
				end do
			end if
		end do
	end do

	return

end
