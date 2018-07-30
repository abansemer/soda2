PRO soda2_particlesort, pop, xtemp, d, istop, inewbuffer, lun_pbp, ncdf_offset, ncdf_id
   ;Sorts particle-by-particle data into the various accumulation arrays.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.


   x=xtemp[0:istop]  ;concatenate the particle structure
   op=*pop
   numparticles=istop+1
   rejection_flag=bytarr(numparticles)

   truetime=x.probetime + median(x.bufftime-x.reftime)
   difftime=[0,truetime[1:*]-truetime]>0
  
   ;interarrival=difftime   ;/(x.missed+1)   ;Tried this correction, but it gives strange shape to intspec. Don't use.
   interarrival=difftime

   ;Cluster analysis
   cluster=bytarr(numparticles)
   FOR i=1L,numparticles-1 DO BEGIN
      IF interarrival[i] lt (*pop).clusterthresh THEN BEGIN
         clustercount=(cluster[i-1]+1)>2
         cluster[i-(cluster[i-1]):i]=clustercount
         ;FOR j=i-(cluster[i-1]+1),i DO cluster[j]=clustercount  ;Increment all data in this cluster
      ENDIF
   ENDFOR

   deadtime=interarrival*x.overloadflag               ;Deadtime=interarrival when the flag is set
   timeindex=long((truetime-op.starttime)/op.rate)    ;Index each particle into right time period
   s=sort(timeindex)                                  ;Sort them (should be mostly sorted already)
   u=[-1, uniq(timeindex[s])]                         ;Give last index of every unique timeindex

   IF ((op.probetype eq '2DC') or (op.probetype eq '2DP')) THEN BEGIN
      deadtimes=(truetime[inewbuffer[1:*]]-truetime[inewbuffer[1:*]-1])>0 ;Gap between last particle of one buffer and first of next
      ideadtime=timeindex[inewbuffer[1:*]]
      IF (*pop).format eq 'RAF' THEN deadtimes=0  ;Timelines are often off by a factor of 2 in this format, should be no deadtime, ignore it.
      
      ;Loop through all the unique indices found
      FOR i=1L,n_elements(u)-1 DO BEGIN
         w=where(ideadtime eq i,ni)
         IF ni gt 0 THEN d.deadtime[i]=total(deadtimes[w]) < op.rate
      ENDFOR
   ENDIF
    
   ;Loop through all the unique indices found
   FOR i=1L,n_elements(u)-1 DO BEGIN
      indstart=(u[i-1]+1)
      indstop=(u[i])
      iparticles=s[indstart:indstop]               ;Index to each particle in this step
      itime=timeindex[iparticles[0]]               ;Time index of this step
      
      IF (itime ge 0) and (itime lt d.numrecords) THEN BEGIN                     ;Make sure in time range  
         ;Find TAS
         d.tas[itime]=mean(x[iparticles].tas)     
         
         ;Get interarrival spectrum
         FOR j=0L,n_elements(iparticles)-1 DO BEGIN
            intbin=max(where(d.intendbins le interarrival[iparticles[j]],nw)) 
            IF (nw gt 0)  and (intbin lt d.numintbins) THEN d.intspec_all[itime,intbin]=d.intspec_all[itime,intbin]+1
         ENDFOR
                  
         ;Find cutoff time based on interarrival time, must have 100 particles
         IF ((*pop).inttime_reject eq 1) and (total(d.intspec_all[itime,*]) gt 100) THEN BEGIN
            dummy=max(d.intspec_all[itime,*],imax)       ;Use the maximum of the poisson dist as the first guess for fit.
            poisson_firstguess=d.intmidbins(imax)/5.0    ;Divide it by 5 for a more reliable fit.
            pf=fit_poisson(d.intspec_all[itime,*],d.intmidbins,poisson_firstguess,plot=0)
            d.poisson_fac[itime,*]=pf.a                  ;Save data for output structure
            d.intcutoff[itime]=1/pf.a[1]*0.05            ;Use the large peak divided by 20
            d.corr_fac[itime]=(1.0/(2*exp(-d.intcutoff[itime]*pf.a[1])-1))
         ENDIF 
      
         ;Accumulate deadtime and missed particles
         d.deadtime[itime]=d.deadtime[itime]+total(deadtime[iparticles])
         d.count_missed[itime]=d.count_missed[itime]+total(x[iparticles].missed)
         d.missed_hist[itime,*]=d.missed_hist[itime,*]+histogram([x[iparticles].missed],min=0,max=49)
         
         ;Reject particles and build size and area ratio spectra
         FOR j=0L,n_elements(iparticles)-1 DO BEGIN
            nextparticleindex=(iparticles[j]+1) < (numparticles-1)
            reject=soda2_reject(x[iparticles[j]], interarrival[iparticles[j]], interarrival[nextparticleindex], d.intcutoff[itime], cluster[iparticles[j]], pop)
            rejection_flag[iparticles[j]] =  reject
            IF reject eq 0 THEN BEGIN   
               CASE (*pop).smethod OF
                  'xsize':size2use=x[iparticles[j]].xsize
                  'ysize':size2use=x[iparticles[j]].ysize
                  'areasize':size2use=x[iparticles[j]].areasize
                  ELSE:size2use=x[iparticles[j]].size
               ENDCASE        
               sizebin=max(where(op.endbins lt size2use),nws)
               IF size2use eq op.endbins[0] THEN sizebin=0  ;Special case where size=first endbin
               arbin=max(where(op.arendbins lt (x[iparticles[j]].ar<0.99>0.01)),nwa)
               asprbin=max(where(op.arendbins lt (x[iparticles[j]].aspr<0.99>0.01)),nwasp)
               obin=(floor(x[iparticles[j]].orientation + 90) / 10) < 17  ;Orientation bin every 10 degrees
               d.spec2d[itime, sizebin, arbin]=d.spec2d[itime, sizebin, arbin]+1
               d.spec2d_aspr[itime, sizebin, asprbin]=d.spec2d_aspr[itime, sizebin, asprbin]+1
               d.spec2d_orientation[itime, sizebin, obin]=d.spec2d_orientation[itime, sizebin, obin] + 1
               d.count_accepted[itime]=d.count_accepted[itime]+1
               d.hist3d[sizebin, arbin, asprbin]=d.hist3d[sizebin, arbin, asprbin] + 1
               
               intbin=max(where(d.intendbins le interarrival[iparticles[j]],nw)) 
               IF (nw gt 0)  and (intbin lt d.numintbins) THEN d.intspec_accepted[itime,intbin]=d.intspec_accepted[itime,intbin]+1
               
               zdbin=max(where(d.zdendbins le x[iparticles[j]].zd,nzd)) 
               d.zdspec[sizebin,zdbin]=d.zdspec[sizebin,zdbin]+1
            
               ;Temporary code for JPL oblateness test
               coeff_a=0.3 & coeff_b=1.48 & coeff_c=0.35  & coeff_d=0.55  ;From Marty's powerpoint slides
               arr=x[iparticles[j]].ar
               asr=x[iparticles[j]].aspr
               IF (arr gt coeff_d) THEN d.spec1d_spherical[itime,sizebin]=d.spec1d_spherical[itime,sizebin]+1
               IF (arr gt coeff_c) and (arr le coeff_d) THEN d.spec1d_mediumprolate[itime,sizebin]=d.spec1d_mediumprolate[itime,sizebin]+1
               IF (arr lt coeff_c) THEN BEGIN
                  IF asr gt (coeff_a+coeff_b*arr) THEN $
                     d.spec1d_oblate[itime,sizebin]=d.spec1d_oblate[itime,sizebin]+1 ELSE $
                        d.spec1d_maximumprolate[itime,sizebin]=d.spec1d_maximumprolate[itime,sizebin]+1
               ENDIF
               ;End JPL
            ENDIF ELSE BEGIN
               d.count_rejected[itime,reject]=d.count_rejected[itime,reject]+1
            ENDELSE
         ENDFOR
      END
   END
   
   ;Print particles if flagged
   IF op.ncdfparticlefile eq 1 THEN BEGIN   ;NetCDF version, now default
      varid=ncdf_varid(ncdf_id,'time')
      ncdf_varput,ncdf_id,varid,truetime[0:istop],count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'probetime')
      ncdf_varput,ncdf_id,varid,x[0:istop].probetime,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'buffertime')
      ncdf_varput,ncdf_id,varid,x[0:istop].bufftime,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'ipt')
      ncdf_varput,ncdf_id,varid,interarrival[0:istop],count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'diam')
      ncdf_varput,ncdf_id,varid,x[0:istop].size,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'xsize')
      ncdf_varput,ncdf_id,varid,x[0:istop].xsize,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'ysize')
      ncdf_varput,ncdf_id,varid,x[0:istop].ysize,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'areasize')
      ncdf_varput,ncdf_id,varid,x[0:istop].areasize,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'arearatio')
      ncdf_varput,ncdf_id,varid,x[0:istop].ar,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'aspectratio')
      ncdf_varput,ncdf_id,varid,x[0:istop].aspr,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'area')
      ncdf_varput,ncdf_id,varid,x[0:istop].area,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'perimeterarea')
      ncdf_varput,ncdf_id,varid,x[0:istop].perimeterarea,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'allin')
      ncdf_varput,ncdf_id,varid,x[0:istop].allin,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'centerin')
      ncdf_varput,ncdf_id,varid,x[0:istop].centerin,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'edgetouch')
      ncdf_varput,ncdf_id,varid,x[0:istop].edge_touch,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'zd')
      ncdf_varput,ncdf_id,varid,x[0:istop].zd,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'missed')
      ncdf_varput,ncdf_id,varid,x[0:istop].missed,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'orientation')
      ncdf_varput,ncdf_id,varid,x[0:istop].orientation,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'overload')
      ncdf_varput,ncdf_id,varid,x[0:istop].overloadflag,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'particle_counter')
      ncdf_varput,ncdf_id,varid,x[0:istop].particle_count,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'rejection_flag')
      ncdf_varput,ncdf_id,varid,rejection_flag,count=numparticles,offset=ncdf_offset
   ENDIF
   IF op.particlefile eq 1 THEN BEGIN   ;ASCII version
      FOR i=0L,numparticles-1 DO printf, lun_pbp, truetime[i], x[i].probetime, x[i].bufftime, interarrival[i], x[i].size, x[i].xsize, x[i].ysize, x[i].ar, $
          x[i].aspr, x[i].orientation, x[i].allin, x[i].overloadflag, x[i].missed, x[i].particle_count, form='(3f13.5,e13.5,3f12.3,2f6.2,f8.1,2i3,i7,i7)'
   ENDIF


END