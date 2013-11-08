subroutine l_flag(l,n)

	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
	use  mod_discretisation

!	implicit none


	integer ii, jj, kk

	kle_blflag = 0
	kle_traj = 0
	kle_lp1 = 0

! Raffinement global : AMR

	if (irefine.eq.1) then
		n = imax(l)*jmax(l)*kmax(l)
		do k = 1, kmax(l)
		do j = 1, jmax(l)
		do i = 1, imax(l)
			iflag(i,j,k) = 1
		end do
		end do
		end do
		return
	endif

! Raffinement local : AMR

	n = 0                 
	iflag = 0

1000 continue

! Sensors on level l

	call  l_amr_ghost_ff(l)
	isauv = isit(l)
	isit(l) = 0
	call l_amr_ghost_fc(l)
	isit(l) = isauv
	if(dir(1).eq.2) call usr_sensor_2D(l,nbsensor)
	if(dir(1).eq.3) call usr_sensor_3D(l,nbsensor)

! Parcourt des patches
	im0 = gp(l)
	do m = 1, nga(l)
		im= gp(l) + m
	        call unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)
	        do k = kbf(im), kef(im)
			ijk1 = ijk
			do j = jbf(im), jef(im)
				ijk0 = ijk
				do i = ibf(im), ief(im)
					if (sensor(ijk).eq.1) then
						! Safety flags 3D
						! cas non periodique
						if(l.ge.1) then
						do ii = max(1,i-isf), min(i+isf,imax(l))
						do jj = max(1,j-isf), min(j+isf,jmax(l))
						do kk = max(1,k-isf), min(k+isf,kmax(l))
						if(iflag(ii,jj,kk).eq.0) n = n+1
						iflag(ii,jj,kk) = 1
						enddo
						enddo
						enddo
						end if
						! cas non periodique a optimiser
						if(l.eq.0) then
						do ib = i-isf, i+isf
						ii = ib 
						if(ib.lt.1) ii = ii + imx(im)
						if(ib.gt.imx(im)) ii = ii - imx(im)
						do jb = j-isf, j+isf
						jj = jb 
						if(jb.lt.1) jj = jj + jmx(im)
						if(jb.gt.jmx(im)) jj = jj - jmx(im)
						do kb = k-isf, k+isf
						kk = kb 
						if(kb.lt.1) kk = kk + kmx(im)
						if(kb.gt.kmx(im)) kk = kk - kmx(im)
						if(iflag(ii,jj,kk).eq.0) n = n+1
						iflag(ii,jj,kk) = 1
						enddo
						enddo
						enddo
						end if

					end if
					ijk = ijk + ibmp
				end do
				ijk = ijk0 + jbmp
			end do
			ijk = ijk1 + kbmp
		end do

! Force le raffinement jusqu'a la frontiere si elle est proche

		i1 = ibf(im)
		i2 = ief(im)
		j1 = jbf(im)
		j2 = jef(im)
		k1 = kbf(im)
		k2 = kef(im)

! I direction
		do k = k1,k2
		do j = j1,j2
!               do k = k1+1,k2-1
!               do j = j1+1,j2-1
		
			nif = int((j - (j1-1))/rj(l)) + 1
			if (mod((j - (j1-1)),rj(l)).eq.0) nif = nif-1
			ind = hjkptr(im) + nif
! I min limit

			if ((jktag(ind,1).ge.im0).or.(jktag(ind,1).lt.0)) then
			if (iflag(i1+1,j,k).eq.1) then
			if(iflag(i1,j,k).ne.1) n = n+1
			    iflag(i1,j,k) = 1
			end if
			end if
! I max limit

			if ((jktag(ind,2).ge.im0).or.(jktag(ind,2).lt.0)) then
			if (iflag(i2-1,j,k).eq.1) then
			if(iflag(i2,j,k).ne.1) n = n+1
			    iflag(i2,j,k) = 1
			end if
			end if 
		end do ! j
		end do ! k

! J direction
		do k = k1,k2
		do i = i1,i2
!               do k = k1+1,k2-1
!               do i = i1+1,i2-1
			nif = int((i - (i1-1))/ri(l)) + 1
			if (mod((i - (i1-1)),ri(l)).eq.0) nif = nif - 1
			ind = hikptr(im) + nif
! J min limit
			if ((iktag(ind,1).ge.im0).or.(iktag(ind,1).lt.0)) then
			if (iflag(i,j1+1,k).eq.1) then
			if(iflag(i,j1,k).ne.1) n = n+1
			    iflag(i,j1,k) = 1
			end if
			end if
! J max limit

			if ((iktag(ind,2).ge.im0).or.(iktag(ind,2).lt.0)) then
			if (iflag(i,j2-1,k).eq.1) then
			if(iflag(i,j2,k).ne.1) n = n+1
				iflag(i,j2,k) = 1
			end if
			end if
		end do ! i
		end do ! k

! K direction
		do j = j1,j2
		do i = i1,i2
			nif = int((i - (i1-1))/ri(l)) + 1
			if (mod((i - (i1-1)),ri(l)).eq.0) nif = nif - 1
			ind = hikptr(im) + nif

! K min limit

			if ((ijtag(ind,1).ge.im0).or.(ijtag(ind,1).lt.0)) then
			if (iflag(i,j,k1+1).eq.1) then
			if(iflag(i,j,k1).ne.1) n = n+1
			    iflag(i,j,k1) = 1
			end if
			end if

! K max limit

			if ((ijtag(ind,2).ge.im0).or.(ijtag(ind,2).lt.0)) then
			if (iflag(i,j,k2-1).eq.1) then
			if(iflag(i,j,k2).ne.1) n = n+1
				iflag(i,j,k2) = 1
			end if
			end if
		end do ! i
		end do ! j

! End : raffinement jusqu'a la frontiere

	end do ! m : parcourt des patches


!c------------------------------------------------
!c   following the flags through the flow
!c------------------------------------------------

      if(kle_traj .eq. 1) then
      if(l.ne.0) then
      do m = 1, nga(l)
         im = gp(l) + m
         klckmx = max(imx(im),jmx(im),kmx(im))
         call unpack_g(im,2,2,2,ijk,ibmp,jbmp,kbmp)
!a
         call unpack_v(im,2,2,2,ijk0,ibmp0,jbmp0,kbmp0)
!a

         kk   = kbf(im)
         i0   = ibf(im)
         do k =2, kmx(im)-1
           ijk1 = ijk
           ii   = i0
           j0   = jbf(im)
           do i =2, imx(im)-1
             jj   = j0
             ijk0 = ijk
             do j = 2, jmx(im)-1
              ijk = ijk + kbmp
              if(iflag(i,j,k).eq.1) then
              klck= 0
              in  = ii
              jn  = jj
              kn  = kk
              ijkc=ijk
!a 100          dx  = abs(x(ijkc+ibmp)-x(ijkc))
!a              dy  = abs(y(ijkc+jbmp)-y(ijkc))
!a              dz  = abs(z(ijkc+kbmp)-z(ijkc))
100           dx  = abs(x(ijk0+ibmp0)-x(ijk0))
              dy  = abs(y(ijk0+jbmp0)-y(ijk0))
              dz  = abs(z(ijk0+kbmp0)-z(ijk0))

              ui  = q(ijkc,2)
              vj  = q(ijkc,3)
              wk  = q(ijkc,4)
      if(abs(vj/ui).le.dy/dx.and.abs(wk/ui).le.dz/dx) then
        if(ui.gt.0) then
          in = in+1
          if(iflag(in,jn,kn).ne.1) n=n+1
          iflag(in,jn,kn) = 1
        elseif(ui.lt.0)then
          in = in-1
          if(iflag(in,jn,kn).ne.1) n=n+1
          iflag(in,jn,kn) = 1
        endif
      endif
      if(abs(ui/wk).le.dx/dz.and.abs(vj/wk).le.dy/dz) then
        if(wk.gt.0) then
          kn = kn+1
          if(iflag(in,jn,kn).ne.1) n=n+1
          iflag(in,jn,kn) = 1
        elseif(wk.lt.0) then
          kn = kn-1
          if(iflag(in,jn,kn).ne.1) n=n+1
          iflag(in,jn,kn) = 1
        endif
      endif
      if(abs(ui/vj).le.dx/dy.and.abs(wk/vj).le.dz/dy) then
        if(vj.gt.0) then
          jn = jn+1
          if(iflag(in,jn,kn).ne.1) n=n+1
          iflag(in,jn,kn) = 1
          iflag(in+1,jn,kn) = 1
          iflag(in,jn,kn+1) = 1
          iflag(in+1,jn,kn+1) = 1
        elseif(vj.lt.0) then
          jn = jn-1
          if(iflag(in,jn,kn).ne.1) n=n+1
          iflag(in,jn,kn) = 1
        endif
      endif
              klck=klck+1
              call unpack_g(im,in,jn,kn,ijkc,ibmp,jbmp,kbmp)
              if((in.ne.i.or.jn.ne.j.or.kn.ne.k).and. &
                in.lt.imx(im).and.jn.lt.jmx(im).and. &
                kn.lt.kmx(im).and.klck.lt.klckmx) then
!a             write(6,*) 'trajectoire i,j,k',in,jn,kn
              goto 100
              endif
              endif
              jj  = jj + 1
              ijk = ijk  + jbmp
              end do
           ii  = ii + 1
           ijk = ijk0 + ibmp
           end do
         kk  = kk + 1
         ijk = ijk1 + kbmp
         end do
      end do
      endif
      endif



      return
      end
