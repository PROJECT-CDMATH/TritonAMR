!**************************************************************************************
!                                                                                     *
! VERSION ORIGINALE PAR N.HURE: 10/04/00                                              *
!                                                                                     *
! VERSION MODIFIEE PAR A.MEKKAS - J.RYAN: 25/04/08                                    *
!                                                                                     *
!		amr driver		                                              *
!                                                                                     *
! SYNTAXE SUBROUTINE:  subroutine amr_driv_uns_solver_meth_2                          *
!                                                                                     *
!									              *
!**************************************************************************************

subroutine amr_driv_uns_solver_meth_2

 	use  mod_hgsparam
	use  mod_hgsarray
	use  mod_hgssave
	use  mod_advection
	use  mod_discretisation
	use  mod_visu
	USE  mod_elliptique
	USE  mod_user
  
	implicit none

      
!!!!!!!!!!!!! DEBUT DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!

	integer lpr, l, ln, lmax_exist, isave, nfic, k,  ll, num  
	character*3 cnum
	character*6 chfich

!!!!!!!!!!!!! FIN DE DECLARATIONS DES VARIABLES LOCALES !!!!!!!!!!!!!!!!!

	if(iter.eq.1.or.iter.le.itamr) then
		lmax_exist  = 0
	else
		lmax_exist  = 1
	endif
	isit=0


! --- for level 0 ------------------------
	if (iter.lt.itamr.or.lmax.eq.0) then
		if(TYPE_DIR.eq.1.and.iter.eq.0) call calcul_volume_epaisseur_interface(0)
		if(TYPE_DIR.eq.1.and.iter.eq.0) call amr_plt_paraview_NODE(0.,0)
		if(iter.eq.0.AND.ELL_YES.EQ.1) goto 222
		isit(0) = 0
		call l_amr_ghost_ff(0)
		call INTERFACE_AMR_HYPERBOLIQUE(0)
	else
! --- for level 1 ------------------------
		if (iter.eq.itamr) then
			if(TYPE_DIR.eq.1) call l_build(1)

			call amr_trans(0,1)
			call amr_trans(1,1)
			IF(INIT_BULLE_USER.EQ.0) THEN
			if(TYPE_DIR.eq.1.and.iter.eq.0.and.dir(1).eq.2) call amr_linit_2D(1)
			if(TYPE_DIR.eq.1.and.iter.eq.0.and.dir(1).eq.3) call amr_linit_3D(1)
			ELSE IF(INIT_BULLE_USER.EQ.1) THEN
			IF(TYPE_DIR.eq.1.AND.iter.EQ.0)	CALL US_INIT_BULLE(1)
			END IF
			call l_amr_ghost_fc(1)
			if(TYPE_DIR.eq.1.and.iter.eq.0) call amr_plt_paraview_NODE(0.,0)
			if(iter.eq.0.AND.ELL_YES.EQ.1) goto 222
			isit(0) = 0
			call l_amr_ghost_ff(0)
			call INTERFACE_AMR_HYPERBOLIQUE(0)

		else
			if (mod(iter,itgfr).eq.0)  then
				if(TYPE_DIR.eq.1) call l_build(1)
				call amr_trans(0,1)
				call amr_trans(1,1)
			end if
			IF(INIT_ADVECTION_USER.EQ.0.AND.ELL_YES.EQ.0) THEN
			if(dir(1).eq.2.and.type_adv_init.le.7) call amr_linit_vitesse_2D(1)
			if(dir(1).eq.3.and.mise_jour_adv.ne.1.and.type_adv_init.le.7) call amr_linit_vitesse_3D(1)
			ELSE IF(INIT_ADVECTION_USER.EQ.1.AND.ELL_YES.EQ.0) THEN
			IF(mise_jour_adv.ne.1) CALL US_INIT_BULLE(1)
			END IF
			call l_amr_ghost_fc(1)      
			isit(0) = 0
			call l_amr_ghost_ff(0)
			call INTERFACE_AMR_HYPERBOLIQUE(0)
		end if

! --- for the other levels ---------------
		do k = 1, maxnav
			lpr = navigator(k-1)
			l  = navigator(k)
			ln = navigator(k+1)
			if (l.eq.lmax) lmax_exist = 1

!                  o
!               o   o

			if ((l.lt.lpr).and.(l.lt.ln)) then
				if (iter.le.itbuild .and. mod(iter,itgfr).eq.0) then
					call l_save_nsc(l+1)	
					if(TYPE_DIR.eq.1) call l_build(l+1)
					call amr_trans(l,l+1)
					call amr_trans(l+1,l+1)
					IF(INIT_ADVECTION_USER.EQ.0.AND.ELL_YES.EQ.0) THEN
					if(dir(1).eq.2.and.type_adv_init.le.7) call amr_linit_vitesse_2D(l+1)
					if(dir(1).eq.3.and.mise_jour_adv.ne.1.and.type_adv_init.le.7) call amr_linit_vitesse_3D(l+1)
					ELSE IF(INIT_ADVECTION_USER.EQ.1.AND.ELL_YES.EQ.0) THEN
					CALL US_INIT_ADVECTION(l+1)
					END IF
				end if
				call l_amr_ghost_fc(l+1)
				isit(0) = 0
				call l_amr_ghost_ff(l)
				call INTERFACE_AMR_HYPERBOLIQUE(l)
				isit(l)= isit(l) + 1
				goto 10
			endif

!                o   o
!                  o

			if ((lpr.lt.l).and.(ln.lt.l)) then
				call INTERFACE_AMR_HYPERBOLIQUE(l)
				call amr_trans(l,l-1)
				isit(l)=0
				goto 10
			endif

!                o
!                  o
!                    o

			if ((lpr.lt.l).and.(l.lt.ln)) then
				if (iter.le.itbuild .and.((mod(iter,itgfr).eq.0).or.(iter.eq.itamr))) then
					if (lmax_exist.eq.1) call l_save_nsc(l+1)
					if(TYPE_DIR.eq.1) call l_build(l+1)
					call amr_trans(l,l+1)
					call amr_trans(l+1,l+1)
					IF(INIT_ADVECTION_USER.EQ.0.AND.ELL_YES.EQ.0) THEN
					if(dir(1).eq.2.and.type_adv_init.le.7) call amr_linit_vitesse_2D(l+1)
					if(dir(1).eq.3.and.mise_jour_adv.ne.1.and.type_adv_init.le.7) call amr_linit_vitesse_3D(l+1)
					ELSE IF(INIT_ADVECTION_USER.EQ.1.AND.ELL_YES.EQ.0) THEN
					CALL US_INIT_ADVECTION(l+1)
					END IF
				endif
				call l_amr_ghost_fc(l+1)
				isit(0) = 0
				call l_amr_ghost_ff(l)
				call INTERFACE_AMR_HYPERBOLIQUE(l)
				isit(l) = isit(l)+1
				goto 10
			endif

!                    o
!                  o
!                o

			if ((ln.lt.l).and.(l.lt.lpr)) then
				call amr_trans(l,l-1)
				isit(l)=0
				goto 10
			endif

!              l = 0  t_n -------------- t_n+1
!              l = 1  t_n -----t_n/maxraf-----t_n/maxraf 

			if ((ln.eq.l).and.(l.eq.lpr)) then
				call l_amr_ghost_fc(l)
				isit(0) = 0
				call l_amr_ghost_ff(l)
				call INTERFACE_AMR_HYPERBOLIQUE(l)
				isit(l) = isit(l)+1
				goto 10
			endif

!              l = 0  t_n -------------- t_n+1
!              l = 1  ------t_n/maxraf---- t_n+1

			if ((lpr.eq.l).and.(ln.lt.l)) then
				call l_amr_ghost_fc(l)
				isit(0) = 0
				call l_amr_ghost_ff(l)
				call INTERFACE_AMR_HYPERBOLIQUE(l)
				call l_save_nsc(l)   
				call amr_trans(l,l-1)
				isit(l)=0
				goto 10
			endif

!              l = 0  tn -------------- tn+1
!              l = 1  tn  

			if ((lpr.lt.l).and.(ln.eq.l)) then
				isit(0) = 0
				call l_amr_ghost_ff(l)
				call INTERFACE_AMR_HYPERBOLIQUE(l)
				isit(l) = isit(l)+1
				goto 10
			endif          

			10  continue
		end do ! k

		do ll = 1,lmax
			call l_save_nsc(ll)
		end do

	end if ! level 0


 	    if(time.eq.max_temps) iter_paraview_Y = 1
		if((iter.eq.nitmax.and.iter.ne.0).or.(mod(iter,iter_paraview_Y).eq.0.and.iter.ne.0.or. &
			mod(iter,iter_paraview_vitesse).eq.0.and.iter.ne.0)) call amr_plt_paraview_NODE(time,iter)


	call calcul_volume_epaisseur_interface(iter)

!	if(mod(iter,iter_save_reprise).eq.0.and.Sav_Rep.eq.1) then
!	  call mc_savhgs(iter)
!	endif

1000 continue


104   format('+++ AMR cycle number ',i6,'   pas de temps(s)= ',e13.6,'   temps physique(s)= ',e13.6)     

222 continue	
	return

end


