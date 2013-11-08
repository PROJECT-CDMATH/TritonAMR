!c
!c       _______________________      amr      _______________________  
!c      |                     hierarchical grid system                | 
!c      |                                                             |
!c      |          checks the connectivity information for level l    | 
!c      |                          3d version                         |
!c      |_____________________________________________________________|
!c
      subroutine l_connect (l)
!*
!*	implicit none
!*
	use  mod_hgsparam
	use  mod_hgsarray


       do m= 1, nga(l+1)
      im = gp(l+1) + m
!c
         kbmp  = iec(im)- ibc(im)+2
         do ic = ibc(im), iec(im)
         do kc = kbc(im), kec(im)
         ind = hikptr(im) + (ic-ibc(im)+1) + (kc-kbc(im))*kbmp
!c
!c------------------------
!c  south face
!c------------------------
          jc = jbc(im)
	    jcm1= jc-1
	    if(jc.eq.1.and.linkbk(1).eq.-1000) jcm1=jc

          if (jc.eq.1.and.linkbk(1).ne.-1000) then
              iktag(ind,1) = min(linkbk(1),0)
          else
!c
!c     fine-coarse connectivity information:
              do mc = 1, nga(l)
              imc = gp(l) + mc
                  if(ic.ge.ibf(imc) .and.   ic.le.ief(imc) .and. &
                     jcm1.ge.jbf(imc) .and. jcm1.le.jef(imc) .and. &
                     kc.ge.kbf(imc) .and.   kc.le.kef(imc)) then
                          iktag(ind,1) = imc
                  end if
              end do
!c
!c     fine-fine connectivity information:
              !  cas periodique
              if(jc.eq.1.and.linkbk(1).eq.-1000) then
                  do mc = 1, nga(l+1)
                      imf = gp(l+1) + mc
                      if(ic.ge.ibc(imf)  .and. ic.le.iec(imf) .and. &
                         jec(imf).eq.jmax(l).and. &
                         kc.ge.kbc(imf)  .and. kc.le.kec(imf)) then
                                  iktag(ind,1) = imf
                      end if
                  end do        ! fin periodique
              else
                  do mc = 1, nga(l+1)
                      if (m.ne.mc) then
                      imf = gp(l+1) + mc
                          if(ic.ge.ibc(imf)  .and. ic.le.iec(imf) .and. &
                         jc.eq.jec(imf)+1.and. &
                         kc.ge.kbc(imf)  .and. kc.le.kec(imf)) then
                              iktag(ind,1) = imf
                          end if
                      end if
                  end do     ! boucle mc
              endif

          end if
!c
!c----------------------------------------
!c  north face
!c----------------------------------------
          jc = jec(im)
	    jcp1=jc+1
		if(jc.eq.jmax(l).and.linkbk(2).eq.-1000) jcp1=jc

          if (jc.eq.jmax(l).and.linkbk(2).ne.-1000) then
              iktag(ind,2) = min(linkbk(2),0) 
          else
!c
!c     fine-coarse connectivity information:
              do mc = 1, nga(l)
                  imc = gp(l) + mc
                  if(ic.ge.ibf(imc) .and.   ic.le.ief(imc) .and. &
                     jcp1.ge.jbf(imc) .and. jcp1.le.jef(imc) .and. &
                     kc.ge.kbf(imc) .and.   kc.le.kef(imc)) then
                      iktag(ind,2) = imc
                  end if
              end do
!c
!c     fine-fine connectivity information:
              !  cas periodique
              if(jc.eq.jmax(l).and.linkbk(2).eq.-1000) then
                  do mc = 1, nga(l+1)
                      imf = gp(l+1) + mc
                      if(ic.ge.ibc(imf)  .and. ic.le.iec(imf) .and. &
                         jbc(imf).eq.1.and. &
                         kc.ge.kbc(imf)  .and. kc.le.kec(imf)) then
                          iktag(ind,2) = imf
                      end if
                  end do    !  end cas periodique
              else
                  do mc = 1, nga(l+1)
                      if (m.ne.mc) then
                          imf = gp(l+1) + mc
                          if(ic.ge.ibc(imf)  .and. ic.le.iec(imf) .and. &
                         jc.eq.jbc(imf)-1.and. &
                         kc.ge.kbc(imf)  .and. kc.le.kec(imf)) then
                              iktag(ind,2) = imf
                          end if
                      end if
                  end do
              end if

	    endif
!c
         end do
         end do
!c---------------------------- end j faces
!c---------------------------- end j faces
!c---------------------------- end j faces

         kbmp  = jec(im)- jbc(im)+2
         do jc = jbc(im), jec(im)
         do kc = kbc(im), kec(im)
          ind = hjkptr(im) + (jc-jbc(im)+1) + (kc-kbc(im))*kbmp
!c
!c--------------------------------------------------------
!c  west face
!c--------------------------------------------------------
          ic = ibc(im)
 	    icm1= ic-1
	    
!c  ***********************************************************	    
	    if(ic.eq.1.and.linkbk(3).eq.-1000) icm1=ic

          if (ic.eq.1.and.linkbk(3).ne.-1000) then
              jktag(ind,1) = min(linkbk(3),0)
          else
!c
!c     fine-coarse connectivity information:
              do mc = 1, nga(l)
              imc = gp(l) + mc
                  if(icm1.ge.ibf(imc) .and. icm1.le.ief(imc) .and. &
                     jc.ge.jbf(imc) .and.   jc.le.jef(imc) .and. &
                     kc.ge.kbf(imc) .and.   kc.le.kef(imc)) then
                          jktag(ind,1) = imc
                  end if
              end do
!c
!c     fine-fine connectivity information:
              !  cas periodique
              if(ic.eq.1.and.linkbk(3).eq.-1000) then
                  do mc = 1, nga(l+1)
                      imf = gp(l+1) + mc
                      if(iec(imf).eq.imax(l).and. &
                         jc.ge.jbc(imf)  .and. jc.le.jec(imf) .and. &
                         kc.ge.kbc(imf)  .and. kc.le.kec(imf)) then
                              jktag(ind,1) = imf
                      end if
                  end do
              else
                do mc = 1, nga(l+1)
                  if (m.ne.mc) then
                      imf = gp(l+1) + mc
                      if(ic.eq.iec(imf)+1.and. &
                     jc.ge.jbc(imf)  .and. jc.le.jec(imf) .and. &
                     kc.ge.kbc(imf)  .and. kc.le.kec(imf)) then
                          jktag(ind,1) = imf
                      end if
                  end if
                end do
             end if
	    endif
!c
!c-----------------------------------------------------
!c  east face
!c-----------------------------------------------------
          ic = iec(im)
	    icp1= ic+1
!c *******************************************************************	    
 		if(ic.eq.imax(l).and.linkbk(4).eq.-1000) icp1=ic

          if (ic.eq.imax(l).and.linkbk(4).ne.-1000) then
              jktag(ind,2) = min(linkbk(4),0)
          else
!c
!c     fine-coarse connectivity information:
	    
	     do mc = 1, nga(l)
                  imc = gp(l) + mc
                  if(icp1.ge.ibf(imc) .and. icp1.le.ief(imc) .and. &
                 jc.ge.jbf(imc) .and.   jc.le.jef(imc) .and. &
                 kc.ge.kbf(imc) .and.   kc.le.kef(imc)) then
                      jktag(ind,2) = imc
                  end if
              end do

!c
!c     fine-fine connectivity information:
              !  cas periodique
              if(ic.eq.imax(l).and.linkbk(4).eq.-1000) then
                  do mc = 1, nga(l+1)
                      imf = gp(l+1) + mc
                      if(ibc(imf).eq.1 .and. &
                         jc.ge.jbc(imf)   .and. jc.le.jec(imf) .and. &
                         kc.ge.kbc(imf)   .and. kc.le.kec(imf)) then
                              jktag(ind,2) = imf
                       end if
                  end do    !  end cas periodique
              else
                  do mc = 1, nga(l+1)
                      if (m.ne.mc) then
                          imf = gp(l+1) + mc
                          if(ic.eq.ibc(imf)-1 .and. &
                          jc.ge.jbc(imf)   .and. jc.le.jec(imf) .and. &
                          kc.ge.kbc(imf)   .and. kc.le.kec(imf)) then
                                      jktag(ind,2) = imf
                          end if
                      end if
                  end do
              end if
	   endif
!c
         end do
         end do
!c
!c---------------------------- end i faces
!c---------------------------- end i faces
!c---------------------------- end i faces

         jbmp  = iec(im)- ibc(im)+2
         do ic = ibc(im), iec(im)
         do jc = jbc(im), jec(im)
         ind = hijptr(im) + (ic-ibc(im)+1) + (jc-jbc(im))*jbmp
!c
!c  bottom face
!c----------------------------------------------------------
          kc = kbc(im)
	    kcm1= kc-1
	    if(kc.eq.1.and.linkbk(5).eq.-1000) kcm1=kc

          if (kc.eq.1.and.linkbk(5).ne.-1000) then
              ijtag(ind,1) = min(linkbk(5),0)
          else
!c
!c     fine-coarse connectivity information:
              do mc = 1, nga(l)
                  imc = gp(l) + mc
                  if(ic.ge.ibf(imc) .and.   ic.le.ief(imc) .and. &
                     jc.ge.jbf(imc) .and.   jc.le.jef(imc) .and. &
                     kcm1.ge.kbf(imc) .and. kcm1.le.kef(imc)) then
                              ijtag(ind,1) = imc
                  end if
              end do
!c
!c     fine-fine connectivity information:
              !  cas periodique
              if(kc.eq.1.and.linkbk(5).eq.-1000) then
                  do mc = 1, nga(l+1)
                      imf = gp(l+1) + mc
                      if(ic.ge.ibc(imf) .and. ic.le.iec(imf) .and. &
                         jc.ge.jbc(imf) .and. jc.le.jec(imf) .and. &
                          kec(imf).eq.kmax(l) ) then
                                  ijtag(ind,1) = imf
                      end if
                  end do
              else
                  do mc = 1, nga(l+1)
                      if (m.ne.mc) then
                          imf = gp(l+1) + mc
                          if(ic.ge.ibc(imf) .and. ic.le.iec(imf) .and. &
                          jc.ge.jbc(imf) .and. jc.le.jec(imf) .and. &
                           kc.eq.kec(imf)+1) then
                                  ijtag(ind,1) = imf
                          end if
                      end if
                  end do
	        endif
          end if
!c
!c  upper face
!c-----------------------------------------------------------
          kc = kec(im)
	    kcp1= kc + 1
	    if(kc.eq.kmax(l).and.linkbk(6).eq.-1000) kcp1=kc
          if (kc.eq.kmax(l).and.linkbk(6).ne.-1000) then
              ijtag(ind,2) = min(linkbk(6),0)
          else
!c
!c     fine-coarse connectivity information:
              do mc = 1, nga(l)
                  imc = gp(l) + mc
                  if(ic.ge.ibf(imc) .and.   ic.le.ief(imc) .and. &
                     jc.ge.jbf(imc) .and.   jc.le.jef(imc) .and. &
                  kcp1.ge.kbf(imc) .and. kcp1.le.kef(imc)) then
                      ijtag(ind,2) = imc
                   end if
               end do
!c
!c     fine-fine connectivity information:
              !  cas periodique
              if(kc.eq.kmax(l).and.linkbk(6).eq.-1000) then
                  do mc = 1, nga(l+1)
                      if (m.ne.mc) then
                          imf = gp(l+1) + mc
                          if(ic.ge.ibc(imf) .and. ic.le.iec(imf) .and. &
                         jc.ge.jbc(imf) .and. jc.le.jec(imf) .and. &
                         kbc(imf).eq.1) then
                                  ijtag(ind,2) = imf
                          end if
                       end if
                  end do
              else
                  do mc = 1, nga(l+1)
                      if (m.ne.mc) then
                          imf = gp(l+1) + mc
                          if(ic.ge.ibc(imf) .and. ic.le.iec(imf) .and. &
                         jc.ge.jbc(imf) .and. jc.le.jec(imf) .and. &
                         kc.eq.kbc(imf)-1) then
                                  ijtag(ind,2) = imf
                          end if
                      end if
                  end do
              end if
!c
          endif
         
         end do
         end do
!c----------------------------fin des faces
      end do
!c
      return
      end
