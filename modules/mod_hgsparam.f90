module mod_hgsparam

!*!	Modifs:
!*!	10.04.2000 - NH     - Parametre (ins)tationnaire : iuns
!*!	08.03.2000 - NH     - Reporte dans solver.inc
!*!	14.09.1999 - N.HURE - Suivi pas de temps global
!*


!     ldim        = valeur maximum de lmax
!     lmax        = maximum grid level
!     navigator   = pilot for amr
!     constructor = indicator of construction 
!     corrector   = indicator of multigrid correction
!     ellipthor   = indicator of noyage 
!     maxnav      = number of processes in navigator
!     ndimi       = number of nodes in the i-direction (for patch cutting)
!     ndimj       = number of nodes in the j-direction (for patch cutting)
!     ndimk       = number of nodes in the k-direction (for patch cutting)

!     ------- for each grid gl (l=0,lmax):   ----    
!
!     nga(l)    = number of meshes contained by gl
!     gp(l)     = grid index pointer [ =gp(l-1)+nga(l-1) ]
!     ri(l) = spatial refinement in i direction
!     rj(l) = spatial refinement in j direction
!     rk(l) = spatial refinement in k direction
!     imax(l)   = dimension in i direction [ =imax(l-1)*ri(l) ]
!     jmax(l)   = dimension in j direction [ =jmax(l-1)*rj(l) ]
!     kmax(l)   = dimension in k direction [ =kmax(l-1)*rk(l) ]
!     linkbk(6) = block interface connection (cf nbi)
!     grdtime(l)= time at level l
!     ------- for each mesh glm: ----    

!     im        = index of mesh glm (=gp(l)+m)
!     lgrid(im) = grid level
!     imx(im)   = width  (=ief-ibf+1)
!     jmx(im)   = height (=jef-jbf+1)
!     kmx(im)   = depth  (=kef-kbf+1)
!     ibf(im)   = extend of mesh glk using gl co-ordinates
!     ief(im)
!     jbf(im)
!     jef(im)
!     kbf(im)
!     kef(im)
!     ibc(im)   = extend of mesh glk using gl-1 co-ordinates
!     iec(im)
!     jbc(im)
!     jec(im)
!     kbc(im)
!     kec(im)
!     hqptr(im) = pointer to the start of cell data block
!     hikptr(im)=                         ik-interface data block         
!     hjkptr(im)=                         jk-interface data block         
!     hijptr(im)=                         ij-interface data block         
!
!     ------- for adapting the grid gl: ----    
!
!     idim      = imax(lmax)
!     jdim      = jmax(lmax)
!     kdim      = kmax(lmax)
!     isf       = number of safety flags in each grid direction
!     iflag(i,j,k)= flag for refinement
!     itamr     = starting iteration with amr
!     itmg      = starting iteration with mg
!     itbuild   = end iteration for regridding the levels
!     irgfr     = iteration gridding frequency
!     advtime   = dt current for each grid
!     tol(l)    = tolerance parameter for mesh sub-dividing l = 0,lmax
!     delt(l)   = threshold 1 for flagging at level l


	INTEGER ldim, mdim, maxi
	PARAMETER(ldim=3, mdim=1000, maxi=1000)
	INTEGER constructor(0:maxi),corrector(0:maxi)
	INTEGER ellipthor(0:maxi)
	INTEGER navigator(0:maxi),nesq(ldim),niesq(ldim)
	INTEGER nga(0:ldim), gp(0:ldim), linkbk(6)
	INTEGER ri(0:ldim),rj(0:ldim),rk(0:ldim)
	INTEGER max_raf(0:ldim)
	INTEGER imax(0:ldim),jmax(0:ldim)
	INTEGER kmax(0:ldim)
	INTEGER lgrid(mdim),imx(mdim),jmx(mdim),kmx(mdim)
	INTEGER ibf(mdim),ief(mdim),ibc(mdim),iec(mdim)
	INTEGER jbf(mdim),jef(mdim),jbc(mdim),jec(mdim)
	INTEGER kbf(mdim),kef(mdim),kbc(mdim),kec(mdim)
	INTEGER hqptr(mdim),hikptr(mdim),hjkptr(mdim),hijptr(mdim),hqptr_v(mdim)
	INTEGER, allocatable,dimension(:,:,:) :: iflag 
	INTEGER idim, jdim, kdim
	INTEGER ijdim
	INTEGER ndimi, ndimj, ndimk
	INTEGER idebug, dbxplot
	INTEGER iter,nitmax, itamr, itbuild, itmg
	INTEGER lmax, irefine   
	INTEGER maxnav
	INTEGER itgfr,isf,advtime
	INTEGER isolver
	INTEGER ighc
	INTEGER :: isit(0:ldim)

	DOUBLE PRECISION timemax, time, time0, dtglb, dxglb, dyglb,dzglb 
	DOUBLE PRECISION grdtime(0:ldim)
	DOUBLE PRECISION tol(0:ldim),delt(0:ldim)
	
	INTEGER :: methode

end module mod_hgsparam
