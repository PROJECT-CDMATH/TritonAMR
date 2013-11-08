!        ____________________________________________________________ 
!       |                  patch merging for amr                     |
!       |                        3d version                          |
!       |____________________________________________________________|
!
        subroutine patch_mrg(bstack)
!
!
!	implicit none
!
        integer bstack(0:*)
	integer b1ptr,b2ptr,bptr
!
700     continue
!       
	if (bstack(0).eq.1) goto 600
!
	do ig1 = 1 , bstack(0)
!
          b1ptr = 6 * (ig1-1)
	  b2ptr = 6 * (ig1-1) + 6
!
	  do ig2 = ig1 + 1, bstack(0)
!
!      merging along the i-direction :
!
	  if ((bstack(b1ptr+3).eq.bstack(b2ptr+3)).and. &
             (bstack(b1ptr+4).eq.bstack(b2ptr+4)).and. &
             (bstack(b1ptr+5).eq.bstack(b2ptr+5)).and. &
             (bstack(b1ptr+6).eq.bstack(b2ptr+6)) ) then
!
	    if ((bstack(b1ptr+1)-1).eq.bstack(b2ptr+2)) then
	      bstack(b1ptr+1) = bstack(b2ptr+1)
	      goto 500
	    endif
	    if ((bstack(b1ptr+2)+1).eq.bstack(b2ptr+1)) then 
	      bstack(b1ptr+2) = bstack(b2ptr+2)
	      goto 500
	    endif
          endif
!
!      merging along the j-direction :
!
	  if ((bstack(b1ptr+1).eq.bstack(b2ptr+1)).and. &
             (bstack(b1ptr+2).eq.bstack(b2ptr+2)).and. &
             (bstack(b1ptr+5).eq.bstack(b2ptr+5)).and. &
             (bstack(b1ptr+6).eq.bstack(b2ptr+6)) ) then
!
	    if ((bstack(b1ptr+3)-1).eq.bstack(b2ptr+4)) then
	      bstack(b1ptr+3) = bstack(b2ptr+3)
	      goto 500
	    endif
	    if ((bstack(b1ptr+4)+1).eq.bstack(b2ptr+3)) then
	      bstack(b1ptr+4) = bstack(b2ptr+4)
	      goto 500
            endif
	  endif
!
!      merging along the k-direction :
!
	  if ((bstack(b1ptr+1).eq.bstack(b2ptr+1)).and. &
             (bstack(b1ptr+2).eq.bstack(b2ptr+2)).and. &
             (bstack(b1ptr+3).eq.bstack(b2ptr+3)).and. &
             (bstack(b1ptr+4).eq.bstack(b2ptr+4)) ) then

	    if ((bstack(b1ptr+5)-1).eq.bstack(b2ptr+6)) then
	      bstack(b1ptr+5) = bstack(b2ptr+5)
	      goto 500
	    endif
	    if ((bstack(b1ptr+6)+1).eq.bstack(b2ptr+5)) then
	      bstack(b1ptr+6) = bstack(b2ptr+6)
	      goto 500
            endif
	  endif
!
          b2ptr = b2ptr + 6
!
          end do
!
        end do
!
	goto 600
!
500     continue
!
!       dynamic table bstack
!
	if (ig2.ne.bstack(0)) then 
	  bptr = 6 * (bstack(0)-1)
	  bstack(b2ptr+1) = bstack(bptr+1)
	  bstack(b2ptr+2) = bstack(bptr+2)
	  bstack(b2ptr+3) = bstack(bptr+3)
	  bstack(b2ptr+4) = bstack(bptr+4)
	  bstack(b2ptr+5) = bstack(bptr+5)
	  bstack(b2ptr+6) = bstack(bptr+6)
        endif
!
	bstack(0) = bstack(0) - 1
!
        goto 700
!
600     continue
!
        return
        end
