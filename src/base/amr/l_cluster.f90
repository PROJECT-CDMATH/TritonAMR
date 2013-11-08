!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE : 00/00/00                                                        *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 01/07/08                                    *
!                                                                                     *
!		Algorithme de grouping/clustering                                     *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine l_cluster(l)                                        *
!                                                                                     *
! VARIABLES:									      *
!     l : level                                                                       *
!**************************************************************************************

subroutine l_cluster(l)

	use  mod_hgsparam

!	implicit none


	external prnest

	parameter ( lstack=50000 )
	integer sptr, gptr, stack(lstack), gstack(0:lstack)

	write(6,*) 'coucou, Cluster',l



	ierr = 0
	gstack(0) = 0
	sptr  = 0
	gptr  = 0
	inest = 1 

!c     initialization of the domains

	ib  = 1
	jb  = 1
	kb  = 1
	ie  = imax(l)
	je  = jmax(l)
	ke  = kmax(l)

500     continue

!c   find the smallest rectangular patch of the flagged cells

	n   = 0
	iib = ie
	jjb = je
	kkb = ke
	iie = ib
	jje = jb
	kke = kb
	do i=ib, ie
	do j=jb, je
	do k=kb, ke
	if (iflag(i,j,k).eq.1) then
		n = n+1
		iib = min(iib,i)
		iie = max(iie,i)
		jjb = min(jjb,j)
		jje = max(jje,j)
		kkb = min(kkb,k)
		kke = max(kke,k)
	end if
	end do
	end do
	end do

!c   is the sub-division acceptable ?

	li = iie-iib+1
	lj = jje-jjb+1
	lk = kke-kkb+1
	lir= li*ri(l+1)
	ljr= lj*rj(l+1)
	lkr= lk*rk(l+1)

!	if ((li.ge.1).and.(lj.ge.1).and.(lk.ge.1).and.(n.gt.0)) then
!		inest = prnest(l,iib,iie,jjb,jje,kkb,kke)
	if ((li.ge.2).and.(lj.ge.2).and.(lk.ge.2).and.(n.gt.0)) then
		inest = prnest(l,iib,iie,jjb,jje,kkb,kke)
		if (((float(n)/float(li*lj*lk).ge.tol(l)).or. &
			((li.le.3).and.(lj.le.3).and.(lk.le.3))).and.(inest.eq.1)) then
			gstack(gptr+1)=iib
			gstack(gptr+2)=iie
			gstack(gptr+3)=jjb
			gstack(gptr+4)=jje
			gstack(gptr+5)=kkb
			gstack(gptr+6)=kke
			gstack(0) = gstack(0) + 1
			gptr = gptr + 6
         else
         if ((li.ge.lj).and.(li.ge.lk)) then
	       is2 = int((iie+iib)/2)
	       if (li.ge.15) then
	          if (mod(l+1,2).eq.1) then
	           ibord =  2
	          else
	           ibord = -2
	          endif 
               else
		   ibord = 0
               endif
!		   ibord = 0
               stack(sptr+1) = iib
               stack(sptr+2) = is2 + ibord
               stack(sptr+3) = jjb
               stack(sptr+4) = jje
               stack(sptr+5) = kkb
               stack(sptr+6) = kke
               stack(sptr+7) = is2 + ibord +1
               stack(sptr+8) = iie
               stack(sptr+9) = jjb
               stack(sptr+10)= jje
               stack(sptr+11)= kkb
               stack(sptr+12)= kke
         else
!c        if ((lj.ge.li).and.(lj.ge.lk).and.(lj.gt.3)) then
         if ((lj.ge.li).and.(lj.ge.lk)) then
	       js2 = int((jje+jjb)/2)
	       if (lj.ge.15) then
	          if (mod(l+1,2).eq.1) then
	           ibord =  2
	          else
	           ibord = -2
	          endif 
               else
!		   ibord = 0
               endif
		   ibord = 0
               stack(sptr+1) = iib
               stack(sptr+2) = iie
               stack(sptr+3) = jjb
               stack(sptr+4) = js2 + ibord
               stack(sptr+5) = kkb
               stack(sptr+6) = kke
               stack(sptr+7) = iib
               stack(sptr+8) = iie
               stack(sptr+9) = js2 + ibord +1
               stack(sptr+10)= jje
               stack(sptr+11)= kkb
               stack(sptr+12)= kke
         else
         if ((lk.ge.li).and.(lk.ge.lj)) then
	       ks2 = int((kke+kkb)/2)
	       if (lk.ge.15) then
	          if (mod(l+1,2).eq.1) then
	           ibord =  2
	          else
	           ibord = -2
	          endif 
               else
!		   ibord = 0
               endif
		   ibord = 0
               stack(sptr+1) = iib
               stack(sptr+2) = iie
               stack(sptr+3) = jjb
               stack(sptr+4) = jje
               stack(sptr+5) = kkb
               stack(sptr+6) = ks2 + ibord
               stack(sptr+7) = iib
               stack(sptr+8) = iie
               stack(sptr+9) = jjb
               stack(sptr+10)= jje
               stack(sptr+11)= ks2 + ibord +1
               stack(sptr+12)= kke
         end if
         end if
         end if
            sptr = sptr+12
            if (sptr.gt.lstack) then
                write (*,*) ' attention, dimension lstack insuffisante'
                stop 3
            end if
         end if
      end if


!c
!c     recursively iterates to the end of the stack
!c
      if (sptr.gt.0) then
         sptr = sptr-6
         ib   = stack(sptr+1)
         ie   = stack(sptr+2)
         jb   = stack(sptr+3)
         je   = stack(sptr+4)
         kb   = stack(sptr+5)
         ke   = stack(sptr+6)
         goto 500
      end if
!c
!c      patch merging
       call patch_mrg(gstack)
!c
!c      patch cutting
 100   ii1 = gstack(0)
	   call patch_cut(gstack,l,lstack)
       if (ii1.ne.gstack(0)) goto 100
!c
!c     save the grids for amr 
!c

      gptr = 0
      do igr = 1, gstack(0)
       iib =  gstack(gptr+1)
       iie =  gstack(gptr+2)
       jjb =  gstack(gptr+3)
       jje =  gstack(gptr+4)
       kkb =  gstack(gptr+5)
       kke =  gstack(gptr+6)
       call patch_sav(l+1,iib,iie,jjb,jje,kkb,kke)
       if(ierr.eq.1) return
       gptr = gptr + 6
      end do

      return
      end
