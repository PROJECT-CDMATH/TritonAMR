      subroutine solver_vfin_cgradg(isit,indf,ri,rj,rk, &
         ijkdeb,ijk,m1c,m2c,m3c, &
         ijks0,m1f,m2f,m3f, &
         ndimc,ndimf,neqt,nfic, &
         r,s)

      implicit none
	
      integer rmax, lmax, osch  , osch2 ! osch ordre du schema en espace
      parameter(rmax=20, lmax=20)

      integer isit, indf,ri,rj,rk, &
        ijk,m1c,m2c,m3c,ijkdeb, &
        ijks,m1f,m2f,m3f, ijks0, &
        ndimc,ndimf,neqt, nfic
	integer ijfmax, ijkfmax, ijk0, ijk1, ii, nne
      DOUBLE PRECISION  r(ndimc,neqt),s(ndimf,neqt)
!-----------------------
      DOUBLE PRECISION c1,c2,p,den,dx,dy,dz, xi,yj,zk, slopei,slopej, slopek

	integer m12c, m12f , rrm, ne,  i1, m
	integer ifmax(lmax), jfmax(lmax), kfmax(lmax)

      integer   i,j,k,mmc,ifc, &
         dijk,ijks1,dijks,ijkc,ijksc,rijk
     integer ib1,ib2,ib3

     DOUBLE PRECISION ave

!     DOUBLE PRECISION ave,a,b

!      ave(a,b)=sign(1.,a)*max(0.0,min(abs(a),sign(1.,a)*b))

 !  verif sur les dimensions
	rrm=max( ri,rj,rk,nfic)
	if(rrm.gt.rmax) then
		print *, 'dans solver_vfin_gradgr, dimension de rmax insuffisante, &
     +         rmax=', rmax,' max(ri,rj,rk,nfic)=',rrm
	    stop
	endif

! pour la consistance en temps  isist = 0 en debut de cycle sinon > ou = 1 et inferieur au degre de
! raffinement   
      if(isit.eq.0) then
	   c1=0.
	   c2=1.
	else
	   p=max(ri,rj,rk)
         den=1./(p-isit+1)
	   c1=den*(p-isit)
	   c2=den
	endif

! qq parametres en plus
    m12c=m1c*m2c 
	m12f=m1f*m2f

    dx=.5/ri
	dy=.5/rj
	dz=.5/rk

    ib1 = 1
    ib2 = 1
    ib3 = 1
	ifmax=ri
	jfmax=rj
	kfmax=rk

! quelles mailles pour les points fictifs
      mmc=1

!   indf = numero de la face
      if(indf.eq.1) then
            mmc=(nfic-1)/ri+1
            dijk=-1
            dijks=-1
            rijk=ri
            ib1 = -1
	      ifmax(mmc) = nfic-(mmc-1)*ri
      endif
      if(indf.eq.2) then
            mmc=(nfic-1)/ri+1
            dijk=1
            dijks=1
            rijk=ri
	      ifmax(mmc) = nfic-(mmc-1)*ri
      endif
      if(indf.eq.3) then
            mmc=(nfic-1)/rj+1
            dijk=-m1c
            dijks=-m1f
            rijk=rj
            ib2 = -1
	      jfmax(mmc) = nfic-(mmc-1)*rj
      endif
      if(indf.eq.4) then
            mmc=(nfic-1)/rj+1
            dijk=m1c
            dijks=m1f
            rijk=rj
	      jfmax(mmc) = nfic-(mmc-1)*rj
      endif
      if(indf.eq.5) then
            mmc=(nfic-1)/(rk+1)+1
            dijk=-m12c
            dijks=-m12f
            rijk=rk
            ib3 = -1
	      kfmax(mmc) = nfic-(mmc-1)*rk
      endif
      if(indf.eq.6) then
            mmc=(nfic-1)/rk+1
            dijk=m12c
            dijks=m12f
            rijk=rk
	      kfmax(mmc) = nfic-(mmc-1)*rk
      endif


! Points fictifs déterminés cellule par cellule
	do ne=1,1 !neqt

	  ijks = ijks0 + dijks 

	 do ifc=1,mmc

! Calcul des gradient grossiers locaux
        ijkc=ijk+(ifc-1)*dijk 
!
        slopei=ave(r(ijkc+1,ne)-r(ijkc,ne),   &
                       r(ijkc,ne)-r(ijkc-1,ne))
        slopej=ave(r(ijkc+m1c,ne)-r(ijkc,ne),   &
                        r(ijkc,ne)-r(ijkc-m1c,ne))
        slopek=ave(r(ijkc+m12c,ne)-r(ijkc,ne),   &
                        r(ijkc,ne)-r(ijkc-m12c,ne))

	  
! calcul des valeurs fines fictives 

        do k=1, kfmax(ifc)
        do j=1 ,jfmax(ifc)
        do i=1, ifmax(ifc)
          xi = -.5 + (2*i-1)*dx
          yj = -.5 + (2*j-1)*dy
          zk = -.5 + (2*k-1)*dz
	     ijks1 = ijks + ib1*(i-1) + ib2*(j-1)*m1f + ib3*(k-1)*m12f
	      s(ijks1,ne)=c1*s(ijks1,ne) +      &
!              & c2* r(ijkc,ne)
               & c2* (r(ijkc,ne) + ib1*xi*slopei+ib2*yj*slopej+ib3*zk*slopek )
        enddo	  
        enddo	  
        enddo	

        ijks = ijks + rijk*dijks


     enddo  ! Boucle sur ifc  
    enddo    ! boucle en neqt



      return
      end
