! #################################
! #                               #
! # SORTIE PARAVIEW DES RESULTATS #
! #                               #
! ################################# 


	SUBROUTINE amr_plt_paraview_NODE(t,IT)

	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_advection
        use  mod_discretisation
	use  mod_visu

	implicit none



! ********************************************************************************
! ****************************************

! DECLARATION DES VARIABLES GLOBALES :

	DOUBLE PRECISION :: t

	integer :: IT
! DECLARATION DES VARIABLES LOCALES :

	integer :: i,j,k,l,erreur,ii,iii, isauv

	character*4 num

	character*3 nim, nl

	character*20 numt

	integer :: WRITER
	
	integer :: success

	DOUBLE PRECISION, allocatable :: XXX(:)

	DOUBLE PRECISION, allocatable :: YYY(:)

	DOUBLE PRECISION, allocatable :: ZZZ(:)

	DOUBLE PRECISION, allocatable :: YY_nodes_1D(:),UU_nodes_1D(:),VV_nodes_1D(:),WW_nodes_1D(:)

        DOUBLE PRECISION,allocatable :: YY_nodes(:,:,:),UU_nodes(:,:,:),VV_nodes(:,:,:),WW_nodes(:,:,:)

	integer :: extent(6)

	integer :: ijk,ibmp,jbmp,kbmp,im,m,ipcf,nptl

	integer :: ijk0,ibmp0,jbmp0,kbmp0,kkk

	integer :: idimx,jdimx,kdimx,iie,jje,kke

	character*4 cnum_Y, cnum_vitesse

	integer :: num_Y, num_vitesse, ilmax

!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	num_Y = -1
	num_vitesse = -1

	IF(IT.eq.0) THEN 
		num_Y = 0
	ELSE
	
		IF(mod(IT,iter_paraview_Y).eq.0) num_Y =IT/iter_paraview_Y
		IF(mod(IT,iter_paraview_vitesse).eq.0)  num_vitesse =IT/iter_paraview_vitesse
	END IF
	
	IF(num_Y.eq.0) THEN 
	cnum_Y='0000'
	OPEN(NFSOLYPVD,FILE='../RESULTATS/Sol'//cnum_Y//'_Y.pvd')
		
	WRITE(NFSOLYPVD,"(A)",ADVANCE="no")"<?xml version=""1.0""?>"
	WRITE(NFSOLYPVD,"(A)",ADVANCE="no")"<VTKFile type=""Collection"" version=""0.1"""
	WRITE(NFSOLYPVD,"(A)",ADVANCE="no")" byte_order=""LittleEndian"" compressor=""vtkZLibDataCompressor"">"
	WRITE(NFSOLYPVD,"(A)",ADVANCE="no")"<Collection>"
	ELSE IF(num_Y.gt.0) THEN
	WRITE(cnum_Y,102) num_Y
	OPEN(NFSOLYPVD,FILE='../RESULTATS/Sol'//cnum_Y//'_Y.pvd')
		
	WRITE(NFSOLYPVD,"(A)",ADVANCE="no")"<?xml version=""1.0""?>"
	WRITE(NFSOLYPVD,"(A)",ADVANCE="no")"<VTKFile type=""Collection"" version=""0.1"""
	WRITE(NFSOLYPVD,"(A)",ADVANCE="no")" byte_order=""LittleEndian"" compressor=""vtkZLibDataCompressor"">"
	WRITE(NFSOLYPVD,"(A)",ADVANCE="no")"<Collection>"
	END IF

	IF(num_vitesse.eq.0) THEN 
	cnum_vitesse='0000'
	OPEN(NFSOLVPVD,FILE='../RESULTATS/Sol'//cnum_vitesse//'_vitesse.pvd')
		
	WRITE(NFSOLVPVD,"(A)",ADVANCE="no")"<?xml version=""1.0""?>"
	WRITE(NFSOLVPVD,"(A)",ADVANCE="no")"<VTKFile type=""Collection"" version=""0.1"""
	WRITE(NFSOLVPVD,"(A)",ADVANCE="no")"byte_order=""LittleEndian"" compressor=""vtkZLibDataCompressor"">"
	WRITE(NFSOLVPVD,"(A)",ADVANCE="no")"<Collection>"
	ELSE IF(num_vitesse.gt.0) THEN
	WRITE(cnum_vitesse,102) num_vitesse
	OPEN(NFSOLVPVD,FILE='../RESULTATS/Sol'//cnum_vitesse//'_vitesse.pvd')
		
	WRITE(NFSOLVPVD,"(A)",ADVANCE="no")"<?xml version=""1.0""?>"
	WRITE(NFSOLVPVD,"(A)",ADVANCE="no")"<VTKFile type=""Collection"" version=""0.1"""
	WRITE(NFSOLVPVD,"(A)",ADVANCE="no")"byte_order=""LittleEndian"" compressor=""vtkZLibDataCompressor"">"
	WRITE(NFSOLVPVD,"(A)",ADVANCE="no")"<Collection>"
	END IF

	WRITE(numt,103) t

	IF(dir(1).eq.3) ilmax = 1	
	IF(dir(1).eq.2) ilmax = 0	

      DO l= 0, lmax
      ipcf=0
      nptl=0

	CALL  l_amr_ghost_ff(l)
	isauv = isit(l)
	isit(l) = 0
	CALL l_amr_ghost_fc(l)
	isit(l) = isauv

     DO m= 1, nga(l)

      im  = gp(l) + m

	  CALL solver_define(im)	
	
		idimx = imx(im)+1 
		jdimx = jmx(im)+1 
		IF(dir(1).eq.3) kdimx = kmx(im)+1 
		IF(dir(1).eq.2) kdimx = 1 

	IF(num_Y.gt.-1) THEN
		ALLOCATE( YY_nodes(idimx,jdimx,kdimx),YY_nodes_1D(idimx*jdimx*kdimx))	
		CALL s_vertex_YY(im,YY_nodes,idimx,jdimx,kdimx)
	END IF

	IF(num_vitesse.gt.-1) THEN 
		ALLOCATE( UU_nodes(idimx,jdimx,kdimx),UU_nodes_1D(idimx*jdimx*kdimx))	
		CALL s_vertex_U(im,UU_nodes,idimx,jdimx,kdimx)
		ALLOCATE( VV_nodes(idimx,jdimx,kdimx),VV_nodes_1D(idimx*jdimx*kdimx))	
		CALL s_vertex_V(im,VV_nodes,idimx,jdimx,kdimx)
		IF(dir(1).eq.3) THEN
		ALLOCATE( WW_nodes(idimx,jdimx,kdimx),WW_nodes_1D(idimx*jdimx*kdimx))	
		CALL s_vertex_W(im,WW_nodes,idimx,jdimx,kdimx)
		END IF
	END IF

	ALLOCATE( XXX(idimx),&
		  YYY(jdimx),&
		  ZZZ(kdimx),&
		  stat=erreur)	


	IF (erreur.ne.0) THEN
 	  WRITE(NFSORTIE,*) "ALLOCATION IMPOSSIBLE"
 	  STOP
	END IF

!c     calcul du pourcentage de raffinement:
      ipcf = ipcf + imx(im)*jmx(im)*kmx(im)

        CALL unpack_v(im,1,1,1,ijk0,ibmp0,jbmp0,kbmp0)
        CALL unpack_g(im,1,1,1,ijk,ibmp,jbmp,kbmp)


	DO i=1,idimx
	     XXX(i) = x(ijk0+(i-1))
	END DO


	DO j=1,jdimx
		YYY(j) = y(ijk0+(j-1)*jbmp0)
	END DO

	IF(dir(1).eq.3) THEN 
	DO k=1,kdimx
	     ZZZ(k) = z(ijk0+(k-1)*kbmp0)	
	END DO
	END IF

	IF(dir(1).eq.2) ZZZ(1)=0. 

	IF(dir(1).eq.2) kkk = 1 
	IF(dir(1).eq.3) kkk = kmx(im) 

	IF(num_Y.gt.-1) THEN
	DO k=1,kdimx
 	DO j=1,jdimx
 	DO i=1,idimx
 		iii=idimx*(j-1)+(idimx*jdimx)*(k-1)+i
		YY_nodes_1D(iii) = YY_nodes(i,j,k)
 	END DO
 	END DO
 	END DO
	END IF


	IF(num_vitesse.gt.-1) THEN 
	DO k=1,kdimx
 	DO j=1,jdimx
 	DO i=1,idimx
 		iii=idimx*(j-1)+(idimx*jdimx)*(k-1)+i
		UU_nodes_1D(iii) = UU_nodes(i,j,k)
		VV_nodes_1D(iii) = VV_nodes(i,j,k)
		IF(dir(1).eq.3) WW_nodes_1D(iii) = WW_nodes(i,j,k)
 	END DO
 	END DO
 	END DO
	END IF

	extent(1) = 0
	extent(2) = idimx-1
	extent(3) = 0
	extent(4) = jdimx-1
	extent(5) = 0
	extent(6) = kdimx-1
	
	WRITE(nim,105) im
	WRITE(nl,105) l


	IF(num_Y.gt.-1) THEN

	CALL vtkXMLWriterF_New(WRITER)

	!0: binary, 1: ascii
	
	CALL vtkXMLWriterF_SetDataObjectType(WRITER, 3 , 0)
	CALL vtkXMLWriterF_SetExtent(WRITER,extent)
	CALL vtkXMLWriterF_SetFileName(WRITER,'../RESULTATS/TritonAmr'//cnum_Y//'_'//nim//'_Y.vtr')

	CALL vtkXMLWriterF_SetCoordinates(WRITER, 0, 11, XXX,idimx)
	CALL vtkXMLWriterF_SetCoordinates(WRITER, 1, 11, YYY,jdimx)
	CALL vtkXMLWriterF_SetCoordinates(WRITER, 2, 11, ZZZ,kdimx)
	CALL vtkXMLWriterF_SetPointData(WRITER, 'champ', 11, & 	
				       YY_nodes_1D,idimx*jdimx*kdimx, 1, 'SCALARS')
	CALL vtkXMLWriterF_Write(WRITER, success)
	CALL vtkXMLWriterF_Delete(WRITER)

      	open(NFANIMY,FILE="../RESULTATS/FILM_VOL_FRAC.pvd",position="APPEND")
  
        WRITE(NFSOLYPVD,"(A)",ADVANCE="no") '<DataSet timestep="'//numt//'" group="advection" part="0"'  
	WRITE(NFSOLYPVD,"(A)",ADVANCE="no") ' file="TritonAmr'//cnum_Y//'_'//nim//'_Y.vtr"/>'

        WRITE(NFANIMY,"(A)",ADVANCE="no") '<DataSet timestep="'//numt//'" group="advection" part="0"' 
	WRITE(NFANIMY,"(A)",ADVANCE="no") ' file="TritonAmr'//cnum_Y//'_'//nim//'_Y.vtr"/>'

	DEALLOCATE(YY_nodes,YY_nodes_1D)

	CLOSE(NFANIMY)

	END IF

	NULLIFY(YY)
	NULLIFY(U)
	NULLIFY(V)
	NULLIFY(W)

	DEALLOCATE(XXX,YYY,ZZZ)
1000 CONTINUE	
      END DO

     nptl = nptl + imax(l)*jmax(l)*kmax(l)
      WRITE(NFSORTIE,*) ipcf
	
      ipcf = (100*ipcf)/max(nptl,1)
      WRITE(NFSORTIE,200) l,nga(l),ipcf
      WRITE(NFSORTIE,*) ipcf,nptl

     END DO

! ********************************************************************************
! ****************************************

! Fermeture finale de fichier animation_3D.pvd

	IF(num_Y.gt.-1) THEN 
		WRITE(NFSOLYPVD,"(A)",ADVANCE="no")"</Collection></VTKFile>"
		
		CLOSE(NFSOLYPVD)
	END IF
! ****************************************
! ********************************************************************************


105	        FORMAT(i3.3)
102	        FORMAT(i4.4)
103	        FORMAT(F20.10)
100     FORMAT(i5)
110     FORMAT(a)
120     FORMAT(a,i5,a,a,i5,a,a,i5,a,a,i5,a,a)
130     FORMAT(4e20.10) 
200     FORMAT(1x,' niveau',i2,' : nombre de grilles =',i4, &
              ' , taux de remplissage =',i4)

	END SUBROUTINE




