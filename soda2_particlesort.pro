PRO soda2_particlesort, pop, xtemp, d, istop, inewbuffer, lun_pbp, ncdf_offset, ncdf_id, pbpprops, $
   image, image_offset, reprocessed_time=reprocessed_time
   ;Sorts particle-by-particle data into the various accumulation arrays.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.


   x=xtemp[0:istop]  ;concatenate the particle structure
   op=*pop
   numparticles=istop+1
   rejectionflag=bytarr(numparticles)

   truetime=x.probetime + median(x.buffertime-x.reftime)
   difftime=[0,truetime[1:*]-truetime]>0

   ;interarrival=difftime   ;/(x.missed+1)   ;Tried this correction, but it gives strange shape to intspec. Don't use.
   interarrival=difftime

   ;SPEC probes (esp 2DS) have lots of timing issues, e.g. buffertime often repeats many times before update,
   ;rawtime rollovers, floating frequency, etc.  Inttime computed in soda2_processbuffer is better, so use that
   ;instead.
   IF op.format eq 'SPEC' THEN BEGIN
      interarrival=x.inttime > 0
      ;Buffertime is not exact, but close enough with accuracy usually <0.1seconds.
      ;Considering rawtime gets far too complicated, with repeating buffer times, floating clock speed, delta-time mismatches, etc.
      ;May also consider interpolation between buffertimes, but must be sure they are monotonic first.
      ;Can now get better time by reprocessing entire file from PBP, which will replace buffertime with improved timing
      ;See reprocess loop in soda2_process_2d.pro and spec_newtime.pro
      IF n_elements(reprocessed_time) ne 0 THEN truetime=reprocessed_time ELSE truetime=x.buffertime
   ENDIF
   IF op.format eq 'SEA' THEN BEGIN
      ;Better computed at buffer level
      interarrival=x.inttime > 0
      ;Need to do this because probetime can be reset during flight (power cycle).
      ;There are still some mismatches between buffer elapsed time and probe elapsed time, so
      ;errors in truetime still appear.  Might just use buffertime in future.
      truetime=x.probetime - x.reftime + x.buffertime

      ;Look for errors in interarrival time, which can zero out large blocks of time in the deadtime/missed loop
      it2 = [0,truetime[1:*]-truetime] > 0             ;Secondary interarrival calculation
      bad = where(abs(interarrival-it2) gt 0.1, nbad)  ;Allow errors up to 0.1 seconds
      IF nbad gt 0 THEN interarrival[bad] = 0          ;Set bad interarrival times to zero
   ENDIF
   IF op.format eq 'TXT' THEN BEGIN
      interarrival=x.inttime > 0
      truetime=x.probetime ;Should be perfect
   ENDIF

   ;Cluster analysis
   cluster=bytarr(numparticles)
   FOR i=1L,numparticles-1 DO BEGIN
      IF interarrival[i] lt (*pop).clusterthresh THEN BEGIN
         clustercount=(cluster[i-1]+1)>2
         cluster[i-(cluster[i-1]):i]=clustercount
         ;FOR j=i-(cluster[i-1]+1),i DO cluster[j]=clustercount  ;Increment all data in this cluster
      ENDIF
   ENDFOR

   ;Time
   deadtime=interarrival*x.overloadflag               ;Deadtime=interarrival when the flag is set
   timeindex=long((truetime-op.starttime)/op.rate)    ;Index each particle into right time period
   dtimeindex = timeindex[1:*]-timeindex              ;For error detection
   IF min(dtimeindex) lt 0 THEN print, 'Time indexing error, non-monotonic times present'

   ;Compute deadtime based on gaps between buffers for 2DC
   IF (op.probetype eq '2DC') THEN BEGIN
      ;Compute gap between last particle of one buffer and first of next
      deadtimes=(truetime[inewbuffer[1:*]]-truetime[inewbuffer[1:*]-1])>0
      ideadtime=timeindex[inewbuffer[1:*]]
      IF (*pop).format eq 'RAF' THEN deadtimes=0  ;Timelines are often off by a factor of 2 in this format, should be no deadtime, ignore it.

      ;Apply deadtime to each time index
      FOR itime=min(timeindex), max(timeindex) DO BEGIN
         w=where(ideadtime eq itime, ni)
         IF ni gt 0 THEN d.deadtime[itime]=total(deadtimes[w]) < op.rate
      ENDFOR
   ENDIF

   ;Loop through all time indices and sort/analyze particles
   FOR itime=(min(timeindex)>0), max(timeindex) DO BEGIN
      iparticles=where(timeindex eq itime, nparticles)
      IF (itime ge 0) and (itime lt d.numrecords) and (nparticles gt 0) THEN BEGIN      ;Make sure in time range
         ;Find TAS
         d.tas[itime]=mean(x[iparticles].probetas)

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
         IF (op.format eq 'SPEC') or ((op.probetype eq 'CIP') and (op.format eq 'SEA')) THEN BEGIN
            ;Use this method for SPEC probes, applying deadtime backward rather than forward
            FOR j=0L,n_elements(iparticles)-1 DO BEGIN
               IF x[iparticles[j]].overloadflag THEN BEGIN
                  deadtimestart = truetime[iparticles[j]] - interarrival[iparticles[j]]
                  deadtimestop = truetime[iparticles[j]]
                  timeindexstart = long((deadtimestart-op.starttime)/op.rate) > 0
                  timeindexstop = long((deadtimestop-op.starttime)/op.rate)  ;Index when overload ends
                  IF timeindexstart eq timeindexstop THEN BEGIN
                     d.deadtime[timeindexstart] += interarrival[iparticles[j]]
                  ENDIF ELSE BEGIN
                     ;print, timeindexstart, timeindexstop
                     ;IF timeindexstop-timeindexstart gt 100 then stop
                     ;Portion in start index
                     d.deadtime[timeindexstart] += (d.time[timeindexstart]+op.rate - deadtimestart)
                     ;Portion in stop index
                     d.deadtime[timeindexstop] += (deadtimestop-d.time[timeindexstop])
                     ;Portion in intermediate indices will be have full deadtime.  Decided to remove this, since
                     ;it marked cloud-free time as 'dead'.
                     ;IF timeindexstop-timeindexstart gt 1 THEN d.deadtime[timeindexstart+1:timeindexstop-1] = op.rate
                  ENDELSE
               ENDIF
            ENDFOR
         ENDIF ELSE BEGIN
            d.deadtime[itime]=d.deadtime[itime]+total(deadtime[iparticles])
         ENDELSE

         ;Missed particles
         d.count_missed[itime]=d.count_missed[itime]+total(x[iparticles].missed > 0)
         d.missed_hist[itime,*]=d.missed_hist[itime,*]+histogram([x[iparticles].missed],min=0,max=49)

         ;Reject particles and build size and area ratio spectra
         FOR j=0L,n_elements(iparticles)-1 DO BEGIN
            nextparticleindex=(iparticles[j]+1) < (numparticles-1)
            CASE (*pop).smethod OF
               'xsize':binningsize=x[iparticles[j]].xsize
               'ysize':binningsize=x[iparticles[j]].ysize
               'areasize':binningsize=x[iparticles[j]].areasize
               'xextent':binningsize=x[iparticles[j]].xextent
               'oned':binningsize=x[iparticles[j]].oned
               'twod':binningsize=x[iparticles[j]].twod
               ELSE:binningsize=x[iparticles[j]].diam
            ENDCASE
            ;Apply poisson-spot correction
            IF ((*pop).apply_psc eq 1) or ((*pop).water eq 1) THEN BEGIN
               binningsize /= x[iparticles[j]].sizecorrection
            ENDIF
            IF ((*pop).water eq 1) THEN binningar=x[iparticles[j]].arearatiofilled ELSE binningar=x[iparticles[j]].arearatio
            reject=soda2_reject(x[iparticles[j]], interarrival[iparticles[j]], interarrival[nextparticleindex], d.intcutoff[itime], cluster[iparticles[j]], binningsize, pop)
            rejectionflag[iparticles[j]] = reject
            IF reject eq 0 THEN BEGIN
               sizebin=max(where(op.endbins le binningsize),nws)
               IF binningsize eq op.endbins[0] THEN sizebin=0  ;Special case where size=first endbin
               arbin=max(where(op.arendbins lt (binningar<0.99>0.01)),nwa)
               asprbin=max(where(op.arendbins lt (x[iparticles[j]].aspectratio<0.99>0.01)),nwasp)
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
            ENDIF ELSE BEGIN
               ireject=where(reject and [1,2,4,8,16,32,64])  ;Gives a list of rejection reasons
               d.count_rejected[itime,ireject[0]]++  ;Increment for each reason
               d.total_count_rejected[itime]++       ;Increment for any reasons
            ENDELSE
         ENDFOR
      END
   END

   ;Print particles if flagged, netCDF version is now default
   IF op.ncdfparticlefile ge 1 THEN BEGIN
      varid=ncdf_varid(ncdf_id,'time')
      ncdf_varput,ncdf_id,varid,truetime[0:istop],count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'probetime')
      ncdf_varput,ncdf_id,varid,x[0:istop].probetime,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'buffertime')
      ncdf_varput,ncdf_id,varid,x[0:istop].buffertime,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'rawtime')
      ncdf_varput,ncdf_id,varid,x[0:istop].rawtime,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'reftime')
      ncdf_varput,ncdf_id,varid,x[0:istop].reftime,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'inttime')
      ncdf_varput,ncdf_id,varid,interarrival[0:istop],count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'diam')
      ncdf_varput,ncdf_id,varid,x[0:istop].diam,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'xsize')
      ncdf_varput,ncdf_id,varid,x[0:istop].xsize,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'ysize')
      ncdf_varput,ncdf_id,varid,x[0:istop].ysize,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'xextent')
      ncdf_varput,ncdf_id,varid,x[0:istop].xextent,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'oned')
      ncdf_varput,ncdf_id,varid,x[0:istop].oned,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'twod')
      ncdf_varput,ncdf_id,varid,x[0:istop].twod,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'areasize')
      ncdf_varput,ncdf_id,varid,x[0:istop].areasize,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'arearatio')
      ncdf_varput,ncdf_id,varid,x[0:istop].arearatio,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'arearatiofilled')
      ncdf_varput,ncdf_id,varid,x[0:istop].arearatiofilled,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'aspectratio')
      ncdf_varput,ncdf_id,varid,x[0:istop].aspectratio,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'area')
      ncdf_varput,ncdf_id,varid,x[0:istop].area,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'areafilled')
      ncdf_varput,ncdf_id,varid,x[0:istop].areafilled,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'perimeterarea')
      ncdf_varput,ncdf_id,varid,x[0:istop].perimeterarea,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'area75')
      ncdf_varput,ncdf_id,varid,x[0:istop].area75,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'allin')
      ncdf_varput,ncdf_id,varid,x[0:istop].allin,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'centerin')
      ncdf_varput,ncdf_id,varid,x[0:istop].centerin,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'dofflag')
      ncdf_varput,ncdf_id,varid,x[0:istop].dofflag,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'edgetouch')
      ncdf_varput,ncdf_id,varid,x[0:istop].edgetouch,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'xpos')
      ncdf_varput,ncdf_id,varid,x[0:istop].xpos,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'ypos')
      ncdf_varput,ncdf_id,varid,x[0:istop].ypos,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'numregions')
      ncdf_varput,ncdf_id,varid,x[0:istop].numregions,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'zd')
      ncdf_varput,ncdf_id,varid,x[0:istop].zd,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'sizecorrection')
      ncdf_varput,ncdf_id,varid,x[0:istop].sizecorrection,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'missed')
      ncdf_varput,ncdf_id,varid,x[0:istop].missed,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'probetas')
      ncdf_varput,ncdf_id,varid,x[0:istop].probetas,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'aircrafttas')
      ncdf_varput,ncdf_id,varid,x[0:istop].aircrafttas,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'orientation')
      ncdf_varput,ncdf_id,varid,x[0:istop].orientation,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'overloadflag')
      ncdf_varput,ncdf_id,varid,x[0:istop].overloadflag,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'particlecounter')
      ncdf_varput,ncdf_id,varid,x[0:istop].particlecounter,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'rejectionflag')
      ncdf_varput,ncdf_id,varid,rejectionflag,count=numparticles,offset=ncdf_offset
   ENDIF

   IF op.ncdfparticlefile eq 2 THEN BEGIN
      varid=ncdf_varid(ncdf_id,'starty')
      ncdf_varput,ncdf_id,varid,x[0:istop].startline,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'stopy')
      ncdf_varput,ncdf_id,varid,x[0:istop].stopline,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'startx')  ;Just fill with zeros.  For compatibility with CPI pbp images.
      ncdf_varput,ncdf_id,varid,intarr(numparticles),count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'stopx')   ;Just fill with array width.  For compatibility with CPI pbp images.
      ncdf_varput,ncdf_id,varid,intarr(numparticles)+(size(image, /dim))[0]-1,count=numparticles,offset=ncdf_offset
      varid=ncdf_varid(ncdf_id,'image')
      print, 'Writing Images...'
      ncdf_varput, ncdf_id, varid, image, count=size(image, /dim), offset=[0, image_offset]
   ENDIF

   ;ASCII particle file
   IF op.particlefile eq 1 THEN BEGIN
      tags = tag_names(x)
      propnames = strupcase(pbpprops[0,*])
      propformat = pbpprops[4,*]
      ;Write data
      FOR i = 0L,numparticles-1 DO BEGIN
         ;Print each property individually.  Slow, but maintains flexibility when new properties are added.
         FOR j = 0, n_elements(propnames)-1 DO BEGIN
            w = where(tags eq propnames[j], nw)
            IF j lt (n_elements(propnames)-1) THEN suffix=',",",$' ELSE suffix=''
            IF nw eq 1 THEN BEGIN
               printf, lun_pbp, x[i].(w), format='('+propformat[j]+suffix+')'
            ENDIF ELSE BEGIN
               ;Special cases for data not in 'x' array of structures
               IF propnames[j] eq 'REJECTIONFLAG' THEN printf, lun_pbp, rejectionflag[i], format='(i0'+suffix+')'
               IF propnames[j] eq 'TIME' THEN printf, lun_pbp, truetime[i], format='(f0.5'+suffix+')'
            ENDELSE
         ENDFOR
      ENDFOR
   ENDIF

END
