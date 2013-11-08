!
!       _______________________      amr      _______________________  
!      |                     hierarchical grid system                | 
!      |                                                             |
!      |   checks the connectivity information for the l sub-meshes  | 
!      |                         3D version                          |
!      |_____________________________________________________________|
!
!
      function prnest(l,ibb,iee,jbb,jee,kbb,kee)
!
!	implicit none
!
	use  mod_hgsparam
	use  mod_hgsarray
      prnest = 0     


!---- rajout JR       
      im0 = gp(l) + 1
      do m= 1, nga(l)
           im = gp(l) + m
               i1 = ibf(im)
               i2 = ief(im)
               j1 = jbf(im)
               j2 = jef(im)
               k1 = kbf(im)
               k2 = kef(im)
               if ((ibb.gt.i1).and.(iee.lt.i2).and. &
                 (jbb.gt.j1).and.(jee.lt.j2).and. &
                 (kbb.gt.k1).and.(kee.lt.k2))    then
     	                prnest =1
	                goto 1000
	         endif
	enddo
  10  continue

!
      do i = ibb,iee
      do j = jbb,jee
      do k = kbb,kee
       im0 = gp(l) + 1
       do m= 1, nga(l)
       im = gp(l) + m
       i1 = ibf(im)
       i2 = ief(im)
       j1 = jbf(im)
       j2 = jef(im)
       k1 = kbf(im)
       k2 = kef(im)
!
       if ((i.gt.i1).and.(i.lt.i2).and.(j.gt.j1).and.(j.lt.j2) &
           .and.(k.gt.k1).and.(k.lt.k2)) goto 400
!
!      west face
!
       if ((i.eq.i1) &
           .and.(j.ge.j1).and.(j.le.j2) &
           .and.(k.ge.k1).and.(k.le.k2)) then
       
       kbump= jec(im)-jbc(im)+2
!      nj = int((j - (j1-1))/rj(l)) + 1
!      nk = int((k - (k1-1))/rk(l)) + 1
!      if (mod((j - (j1-1)),rj(l)).eq.0) nj = nj - 1
!      if (mod((k - (k1-1)),rk(l)).eq.0) nk = nk - 1
       nj = int((j - j1)/rj(l)) + 1
       nk = int((k - k1)/rk(l))
       ind= hjkptr(im) + nj + nk*kbump
!
!  a verifier...
       if ((jktag(ind,1).lt.im0).and.(jktag(ind,1).gt.0)) then
       prnest = 0 
       goto 500
       else
       prnest = 1
       if (j.eq.j1) goto 700
       if (j.eq.j2) goto 750
       if (k.eq.k1) goto 800
       if (k.eq.k2) goto 850
       goto 400
       endif
!
       endif
!
!      east face
!
       if ((i.eq.i2) &
           .and.(j.ge.j1).and.(j.le.j2) &
           .and.(k.ge.k1).and.(k.le.k2)) then
!
       kbump= jec(im)-jbc(im)+2
!      nj = int((j - (j1-1))/rj(l)) + 1
!      nk = int((k - (k1-1))/rk(l)) + 1
!      if (mod((j - (j1-1)),rj(l)).eq.0) nj = nj - 1
!      if (mod((k - (k1-1)),rk(l)).eq.0) nk = nk - 1
       nj = int((j - j1)/rj(l)) + 1
       nk = int((k - k1)/rk(l))
       ind= hjkptr(im) + nj + nk*kbump
!      if(l.eq.1.and.1.eq.3.and.jbb.eq.1.and.jee.eq.4) write(6,*)
!    & 'Coucou, prnest : ind,jktag=',ind,jktag(ind,2),im0
! 
!  a verifier...
       if ((jktag(ind,2).lt.im0).and.(jktag(ind,2).gt.0)) then
       prnest = 0
       goto 500
       else 
       prnest = 1
       if (j.eq.j1) goto 700
       if (j.eq.j2) goto 750
       if (k.eq.k1) goto 800
       if (k.eq.k2) goto 850
       goto 400
       endif
!
       endif
!
!      south face
!
 700   continue
       if ((i.ge.i1).and.(i.le.i2).and.(k.ge.k1).and.(k.le.k2) &
           .and.(j.eq.j1)) then
!
       kbump= iec(im)-ibc(im)+2
       ni = int((i - i1)/ri(l)) + 1
       nk = int((k - k1)/rk(l))
       ind= hikptr(im) + ni + nk*kbump
! 
!  a verifier...
       if ((iktag(ind,1).lt.im0).and.(iktag(ind,1).gt.0)) then
       prnest = 0
       goto 500
       else 
       prnest = 1
       if (k.eq.k1) goto 800
       if (k.eq.k2) goto 850
       goto 400
       endif
!
       endif
!	
!      north face
!
 750   continue
       if ((i.ge.i1).and.(i.le.i2).and.(k.ge.k1).and.(k.le.k2) &
           .and.(j.eq.j2)) then	
!
       kbump= iec(im)-ibc(im)+2
       ni = int((i - i1)/ri(l)) + 1
       nk = int((k - k1)/rk(l))
       ind= hikptr(im) + ni + nk*kbump
! 
!  a verifier...
       if ((iktag(ind,2).lt.im0).and.(iktag(ind,2).gt.0)) then
       prnest = 0
       goto 500
       else 
       prnest = 1
       if (k.eq.k1) goto 800
       if (k.eq.k2) goto 850
       goto 400
       endif
!
       endif
!	
!      bottom face
!
 800   continue
       if ((i.ge.i1).and.(i.le.i2).and.(j.ge.j1).and.(j.le.j2) &
           .and.(k.eq.k1)) then
!
       jbump= iec(im)-ibc(im)+2
       ni = int((i - (i1-1))/ri(l)) + 1
       nj = int((j - (j1-1))/rj(l)) + 1
       if (mod((i - (i1-1)),ri(l)).eq.0) ni = ni - 1
       if (mod((j - (j1-1)),rj(l)).eq.0) nj = nj - 1
       ni = int((i - i1)/ri(l)) + 1
       nj = int((j - j1)/rj(l))
       ind= hijptr(im) + ni + nj*jbump
! 
!  a verifier...
       if ((ijtag(ind,1).lt.im0).and.(ijtag(ind,1).gt.0)) then
       prnest = 0
       goto 500
       else 
       prnest = 1
       goto 400
       endif
!
       endif
!	
!      upper face
!
 850   continue
       if ((i.ge.i1).and.(i.le.i2).and.(j.ge.j1).and.(j.le.j2) &
           .and.(k.eq.k2)) then	
!
       ind0 = hijptr(im)
       jbump= iec(im)-ibc(im)+2
       ni = int((i - i1)/ri(l)) + 1
       nj = int((j - j1)/rj(l))
       ind= hijptr(im) + ni + nj*jbump
! 
!  a verifier...
       if ((ijtag(ind,2).lt.im0).and.(ijtag(ind,2).gt.0)) then
       prnest = 0
       goto 500
       else 
       prnest = 1
       goto 400
       endif
!
       endif
!	
       end do
!
!     you lose the game
      prnest = 0 
      goto 500
!
!     you win
!
 400  continue
      end do 
      end do
      end do
 500  continue
!
 1000 continue
      return
      end
