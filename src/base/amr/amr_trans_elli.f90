!       _______________________      amr      _______________________  
!      |                     hierarchical grid system                | 
!      |                                                             |
!      |     transfers the data solution from l-grid to l1-grid      | 
!      |                          3D version                         |
!      |_____________________________________________________________|
!
      subroutine amr_trans_elli(l,l1)
!
!	implicit none
!
	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
!      
! =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!
      DOUBLE PRECISION slopei,slopej,slopek
!      ave(a,b)=sign(1.,a)*max(0.0,min(abs(a),sign(1.,a)*b))
!
	if (idebug.eq.1) then
!        write(*,99) '+++ AMR : transfer ',l,' -> ',l1
      endif
!
!! ===== Up from l to l+1 =============================================
!
      if (l1.eq.l+1) then
	write(74,100) 'Pr ',l,' -> ',l1
!! --------------------------------------------------------------------      
!
       do mf = 1, nga(l1)
        imf = gp(l1) + mf
!
        do mc = 1, nga(l)
          imc = gp(l) + mc
!
!       find the intersection:
!
          ie  = min(ief(imc),iec(imf))
          ib  = max(ibf(imc),ibc(imf))
          je  = min(jef(imc),jec(imf))
          jb  = max(jbf(imc),jbc(imf))
          ke  = min(kef(imc),kec(imf))
          kb  = max(kbf(imc),kbc(imf))
!
!       transfer in case of non empty intersection:
!
          if (ie.ge.ib .and. je.ge.jb .and. ke.ge.kb) then
! 
!                   do kc = max(1,kb-1), min(ke+1,kef(imc))
!                   do ic = max(1,ib-1), min(ie+1,ief(imc))
!                   do jc = max(1,jb-1), min(je+1,jef(imc))
                    do kc = kb,ke
                    do ic = ib,ie
                    do jc = jb,je

              i = ic - ibf(imc) + 1
              j = jc - jbf(imc) + 1
              k = kc - kbf(imc) + 1
              call unpack_g(imc,i,j,k,ijkc,ibm,jbm,kbm)
              slopei=ave(PHI(ijkc+ibm)-PHI(ijkc),                    &
     &                       PHI(ijkc)-PHI(ijkc-ibm))
              slopej=ave(PHI(ijkc+jbm)-PHI(ijkc),                   &
     &                       PHI(ijkc)-PHI(ijkc-jbm))
              slopek=ave(PHI(ijkc+kbm)-PHI(ijkc),                   &
     &                       PHI(ijkc)-PHI(ijkc-kbm))
              i = (ic-1)*ri(l1)+1-ibf(imf)+1
              j = (jc-1)*rj(l1)+1-jbf(imf)+1
              k = (kc-1)*rk(l1)+1-kbf(imf)+1
              call unpack_g(imf,i,j,k,ijkf,ibmp,jbmp,kbmp)
              do kr= 1, rk(l1)
              ijk1 = ijkf
              do ir= 1, ri(l1)
              ijk0 = ijkf
              do jr= 1, rj(l1)
              xi  = (float(ir)-.5)/float( ri(l1) )-.5
              yj  = (float(jr)-.5)/float(rj(l1))-.5
              zk  = (float(kr)-.5)/float(rk(l1))-.5
              PHI(ijkf)=PHI(ijkc)+xi*slopei+yj*slopej+zk*slopek
!              PHI(ijkf)=PHI(ijkc)
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
!
 11     end do
 10    end do

!
      end if
!
!-------------------- Down from l to l-1 -------------------------------
	if (l1.eq.l-1) then
!		write(74,100) 'Re ',l,' -> ',l1
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
 !                   do kc = max(1,kb-1), min(ke+1,kef(imc))
 !                   do ic = max(1,ib-1), min(ie+1,ief(imc))
 !                   do jc = max(1,jb-1), min(je+1,jef(imc))
                    do kc = kb,ke
                    do ic = ib,ie
                    do jc = jb,je
						i = ic - ibf(imc) + 1
						j = jc - jbf(imc) + 1
						k = kc - kbf(imc) + 1
						call unpack_g(imc,i,j,k,ijkc,ibm,jbm,kbm)
						i = (ic-1)*ri(l)+1-ibf(imf)+1
						j = (jc-1)*rj(l)+1-jbf(imf)+1
						k = (kc-1)*rk(l)+1-kbf(imf)+1
						call unpack_g(imf,i,j,k,ijkf,ibmp,jbmp,kbmp)

						PHI(ijkc) = 0.0

						rijk = 0.
						do kr = 1,rk(l)
							ijk1 = ijkf
							do ir = 1,ri(l)
								ijk0 = ijkf
								do jr = 1,rj(l)
									PHI(ijkc) = PHI(ijkc) + PHI(ijkf)

									rijk = rijk + 1.
									ijkf = ijkf + jbmp
								end do
								ijkf = ijk0 + ibmp
							end do
							ijkf = ijk1 + kbmp
						end do

						PHI(ijkc) = PHI(ijkc) / rijk
					end do
					end do
					end do
				end if
			end do
		end do
	end if

99    format(a18,i2,a4,i2)     
100   format(a3,i2,a4,i2)
	return
      end
! ===== amr_trans.f ===== last line ===================================
