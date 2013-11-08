!        __________________       amr / 3D     ______________________  
!       |        patch cutting for amr according solver limits       |
!       |                        3D version                          |
!       |____________________________________________________________|
!
subroutine patch_cut(bstack,l,lstack)

	use  mod_hgsparam

!	implicit none


	integer bstack(0:lstack)
	integer bptr,rmaxi,rmaxj,rmaxk,bpmax

	ific=ighc

	ig = 0
	bptr=0
	bpmax=bstack(0)*6

	do i = 1, bstack(0)
		nmaxi=ri(l+1)*(bstack(bptr+2)-bstack(bptr+1)+1)+2*ific
		nmaxj=rj(l+1)*(bstack(bptr+4)-bstack(bptr+3)+1)+2*ific
		nmaxk=rk(l+1)*(bstack(bptr+6)-bstack(bptr+5)+1)+2*ific

        if (nmaxi.ge.ndimi) then
          bstack(bpmax+2)=bstack(bptr+2)
          bstack(bptr+2)=int((bstack(bptr+2)+bstack(bptr+1)+1)/2) 
          bstack(bpmax+1)=bstack(bptr+2)+1
          bstack(bpmax+3)=bstack(bptr+3)
          bstack(bpmax+4)=bstack(bptr+4)
          bstack(bpmax+5)=bstack(bptr+5)
          bstack(bpmax+6)=bstack(bptr+6)
          bpmax=bpmax+6
          ig = ig + 1
        elseif (nmaxj.ge.ndimj) then
          bstack(bpmax+4)=bstack(bptr+4)
          bstack(bptr+4)=int((bstack(bptr+4)+bstack(bptr+3)+1)/2) 
          bstack(bpmax+1)=bstack(bptr+1)
          bstack(bpmax+2)=bstack(bptr+2)
          bstack(bpmax+3)=bstack(bptr+4)+1
          bstack(bpmax+5)=bstack(bptr+5)
          bstack(bpmax+6)=bstack(bptr+6)
          bpmax=bpmax+6
          ig = ig + 1
        elseif (nmaxk.ge.ndimk) then
          bstack(bpmax+6)=bstack(bptr+6)
          bstack(bptr+6)=int((bstack(bptr+6)+bstack(bptr+5)+1)/2) 
          bstack(bpmax+1)=bstack(bptr+1)
          bstack(bpmax+2)=bstack(bptr+2)
          bstack(bpmax+3)=bstack(bptr+3)
          bstack(bpmax+4)=bstack(bptr+4)
          bstack(bpmax+5)=bstack(bptr+6)+1
          bpmax=bpmax+6
          ig = ig + 1
        endif

        bptr=bptr+6

	end do
    bstack(0)=bstack(0)+ig

    return
end
