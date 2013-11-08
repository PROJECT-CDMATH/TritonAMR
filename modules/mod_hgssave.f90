      module mod_hgssave
!c       _______________________      amr      _______________________  
!c      |                 hierarchical grid system savings            | 
!c      |_____________________________________________________________|
!c
!c
      use  mod_hgsparam

      integer iter_save_reprise, iter_init,Sav_Rep
	  integer nga_s(0:ldim), gp_s(0:ldim)
      integer ibf_s(mdim),ief_s(mdim),jbf_s(mdim),jef_s(mdim)
      integer ibc_s(mdim),iec_s(mdim),jbc_s(mdim),jec_s(mdim)
      integer kbf_s(mdim),kef_s(mdim),kbc_s(mdim),kec_s(mdim)
      integer hqptr_s(mdim)

      DOUBLE PRECISION, allocatable, dimension(:,:) :: smv

      end module mod_hgssave
