PRO soda2_process_2d, op, textwidgetid=textwidgetid
   ;PRO to make 'spectra' files for a 2D probe, and save them
   ;in IDL's native binary format.  This version places particles
   ;individually in the appropriate time period, rather than the 
   ;entire buffer.  Limited to probes with absolute time, i.e. 
   ;CIP and Fast2D probes.
   ;Aaron Bansemer, NCAR, 2009.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   
   IF n_elements(textwidgetid) eq 0 THEN textwidgetid=0
   
   ;The current expected structure of op is:
   ;op={fn:fn, date:date, starttime:hms2sfm(starttime), stoptime:hms2sfm(stoptime), format:format, $
   ;subformat:subformat, probetype:probetype, res:res, endbins:endbins, arendbins:arendbins, rate:rate, $
   ;smethod:smethod, pth:pth, particlefile:0, inttime_reject:inttime_reject, reconstruct:1, stuckbits:0, water:water,$
   ;fixedtas:fixedtas, outdir:outdir, project:project, timeoffset:timeoffset, armwidth:armwidth, $
   ;numdiodes:numdiodes, greythresh:greythresh}
  
;presolve_all
;profiler,/reset
;profiler  
;profiler,/system
  
   
   soda2_update_op,op
   pop=ptr_new(op)      ;make a pointer to this for all other programs, these are constants
   ;Keep miscellaneous stuff here, things that change during processing
   misc={f2d_remainder:ulon64arr(512), f2d_remainder_slices:0, yres:op.yres, lastbufftime:0D, $
         nimages:0, imagepointers:lon64arr(500), lastclock:0d, lastparticlecount:0L, maxsfm:0D}    
   pmisc=ptr_new(misc)  ;a different pointer, for stuff that changes 
   
   ;====================================================================================================
   ;====================================================================================================
   ;Initialize variables.  
   ;====================================================================================================
   ;====================================================================================================
   numrecords=long((op.stoptime-op.starttime)/op.rate + 1)   ;Number of records that will be saved
   numbins=n_elements(op.endbins)-1 
   numarbins=n_elements(op.arendbins)-1 
   
   numintbins=40 
   iminpower=-7      ;lowest bin exponent
   imaxpower=1       ;highest bin exponent
   intendbins=10^((findgen(numintbins+1)/numintbins)*(imaxpower-iminpower)+iminpower)        ;log-spaced endbins from iminpower to maxpower
   intmidbins=10^(((findgen(numintbins)+0.5)/numintbins) *(imaxpower-iminpower)+iminpower)   ;midbins
   
   numzdbins=163 
   
   d={numrecords:numrecords,$
   time:op.starttime + op.rate*dindgen(numrecords) ,$    ;This is the start time for each record
   
   ;PSD
   numbins:numbins ,$
   numarbins:numarbins ,$
   count_accepted:lonarr(numrecords) ,$
   count_rejected:lonarr(numrecords,7) ,$
   count_missed:lonarr(numrecords) ,$
   missed_hist:fltarr(numrecords,50),$
   spec2d:fltarr(numrecords, numbins, numarbins) ,$
   spec2d_aspr:fltarr(numrecords, numbins, numarbins) ,$
   spec2d_orientation:fltarr(numrecords, numbins, 18) ,$
   ;JPL temporary
   spec1d_spherical:fltarr(numrecords, numbins) ,$
   spec1d_mediumprolate:fltarr(numrecords, numbins) ,$
   spec1d_oblate:fltarr(numrecords, numbins) ,$
   spec1d_maximumprolate:fltarr(numrecords, numbins) ,$
 
   ;Interarrival
   numintbins:numintbins ,$
   iminpower:iminpower  ,$    ;lowest bin exponent
   imaxpower:imaxpower   ,$    ;highest bin exponent
   intendbins:intendbins ,$
   intmidbins:intmidbins ,$
   intspec_all:fltarr(numrecords,numintbins) ,$       ;interarrival time spectra for all (rejected too) particles
   intspec_accepted:fltarr(numrecords,numintbins) ,$  ;interarrival time spectra for accepted particles
     
   ;Zd (from Korolev corrections)
   numzdbins:numzdbins ,$
   zdendbins:findgen(numzdbins)*0.05-0.025 ,$
   zdmidbins:findgen(numzdbins)*0.05 ,$
   zdspec:lonarr(numbins,numzdbins) ,$
     
   ;Misc
   tas:fltarr(numrecords) ,$
   poisson_fac:fltarr(numrecords,3) ,$ ;the coefficients into the double poisson fit
   corr_fac:fltarr(numrecords)+1 ,$    ;Poisson correction factor
   intcutoff:fltarr(numrecords), $   ;+intendbins[0]
   deadtime:fltarr(numrecords), $
   hist3d:fltarr(numbins,numarbins,numarbins) $  ;Histogram of area ratio, axis ratio, and size
   }
   ;Not needed in structure
   numbuffsaccepted=intarr(numrecords)
   numbuffsrejected=intarr(numrecords)
   dhist=lonarr(numrecords,op.numdiodes)

   ;Set up the particle structure.  
   num2process=10000000L ;Limit to reduce memory consumption
   basestruct={bufftime:0d, probetime:0d, reftime:0d, size:0.0, xsize:0.0, ysize:0.0, areasize:0.0, ar:0.0, aspr:0.0, area:0.0, $
               allin:0b, centerin:0b, edge_touch:0b, tas:0s, zd:0.0, missed:0.0, overloadflag:0b, orientation:0.0, $
               perimeterarea:0.0, dof:0b, particle_count:0L}
   x=replicate(basestruct, num2process)
 
   
   ;====================================================================================================
   ;====================================================================================================
   ;Get tas from PTH file, if applicable
   ;====================================================================================================
   ;====================================================================================================
   got_pth=0
   IF file_test(op.pth) THEN BEGIN
      suffix=(strsplit(op.pth,'.',/extract))[-1]
      ;IDL sav files      
      IF (suffix eq 'dat') or (suffix eq 'sav') THEN BEGIN      
         restore,op.pth
         IF total(d.time - data.time) ne 0 THEN stop,'PTH time does not match.'
         pth_tas=data.tas
         got_pth=1
      ENDIF
      ;ASCII or CSV files, assumes time and tas in first two columns
      IF (suffix eq 'txt') or (suffix eq 'csv') THEN BEGIN
         pth_tas=fltarr(numrecords)
         v=''
         openr,lun,op.pth,/get_lun
         on_ioerror, bad  ;Use to suppress type conversion errors
         REPEAT BEGIN
            readf,lun,v
            fields=float(strsplit(v, '[ ,' + STRING(9B) + ']+', /regex, /extract))
            i=(round(fields[0])-op.starttime)/op.rate ;find index for each variable
            ;Fill TAS array, don't bother with averaging.  Note use of i:*, which makes sure gaps are filled in for hirate data.
            IF (i ge 0) and (i lt numrecords) and (fields[1] gt 0) and (fields[1] lt 500) THEN pth_tas[i:*]=fields[1]
            bad:dummy=0
         ENDREP UNTIL eof(lun)
         on_ioerror, null
         free_lun,lun
         got_pth=1
      ENDIF
   ENDIF ELSE BEGIN
      pth_tas=fltarr(numrecords)
      IF op.pth ne '' THEN BEGIN  ;Extra warning if a filename was actually entered
         infoline='TAS file does not exist.  Use default air speed instead?'
         IF textwidgetid ne 0 THEN BEGIN 
            dummy=dialog_message(infoline,dialog_parent=textwidgetid,/question)
            IF dummy eq 'No' THEN return
         ENDIF ELSE BEGIN
            print,'TAS file '+op.pth+' does not exist. Stopping.'
            stop
         ENDELSE
      ENDIF ELSE print,'No TAS file entered, using default values'
   ENDELSE
      
   ;====================================================================================================
   ;====================================================================================================
   ;Build index of buffers for all files.  
   ;====================================================================================================
   ;====================================================================================================
   
   
   firstfile=1
   FOR i=0,n_elements(op.fn)-1 DO BEGIN
      y=soda2_buildindex(op.fn[i], pop)      
      IF y.error eq 0 THEN BEGIN
         IF firstfile THEN BEGIN
            ;Create arrays
            bufftime=y.bufftime
            buffdate=y.date
            buffpoint=y.pointer
            bufffile=bytarr(y.count)+i
         ENDIF ELSE BEGIN
            ;Concatenate arrays if there is more than one file
            IF op.format eq 'SPEC' THEN stop, 'Multiple files not supported with SPEC format, need to concatenate raw files first'
            bufftime=[bufftime, y.bufftime]
            buffdate=[buffdate, y.date]
            buffpoint=[buffpoint,y.pointer]
            bufffile=[bufffile,bytarr(y.count)+i]
         ENDELSE
         firstfile=0
      ENDIF ELSE IF y.error eq 1 THEN stop,'Error on build index, check probe ID set correctly.'
   ENDFOR
   
   ;Set up particlefile
   lun_pbp=2
   ncdf_offset=0L
   ncdf_id=0L
   IF op.ncdfparticlefile eq 1 THEN BEGIN
      fn_ncdf=soda2_filename(op,op.shortname,extension='.pbp.nc')
      file_delete, fn_ncdf, /quiet ;The 'clobber' switch does not work on ncdf_create with netcdf4
      ncdf_id=ncdf_create(fn_ncdf, /netcdf4_format)
      ;Define the x-dimension, should be used in all variables
      xdimid=ncdf_dimdef(ncdf_id,'Time',/unlimited)
      
      ;These are for ncplot compatibility
      opnames=tag_names(op)
      flightdate=strmid(op.date,0,2)+'/'+strmid(op.date,2,2)+'/'+strmid(op.date,4,4)
      
      tb='0000'+strtrim(string(sfm2hms(op.starttime)),2)
      te='0000'+strtrim(string(sfm2hms(op.stoptime)),2)
      starttimestr=strmid(tb,5,2,/r)+':'+strmid(tb,3,2,/r)+':'+strmid(tb,1,2,/r)
      stoptimestr=strmid(te,5,2,/r)+':'+strmid(te,3,2,/r)+':'+strmid(te,1,2,/r)
      intervalstr=starttimestr+'-'+stoptimestr
      
      ;Create global attributes
      ncdf_attput,ncdf_id,'Source','SODA-2 OAP Processing Software',/global
      ncdf_attput,ncdf_id,'FlightDate',flightdate[0],/global
      ncdf_attput,ncdf_id,'DateProcessed',systime(),/global
      ncdf_attput,ncdf_id,'TimeInterval',intervalstr,/global
      opnames=tag_names(op)                  
      FOR i=0,n_elements(opnames)-1 DO BEGIN
         IF size(op.(i), /type) eq 7 THEN BEGIN  ;Look for strings, must be handled differently
            IF string(op.(i)[0]) eq '' THEN attdata='none' ELSE attdata=op.(i)[0] ;To avoid a ncdf error (empty string)
            ncdf_attput,ncdf_id,opnames[i],attdata,/global  ;Only put first element for string
         ENDIF ELSE ncdf_attput,ncdf_id,opnames[i],op.(i),/global   ;Non-strings, all elements      
      ENDFOR
      
      ;List of variables to include in netCDF file: [varname, longname, unit, datatype]
      ncdfprops=[['time', 'UTC Time', 'seconds', 'double'],$
                 ['probetime', 'Unadjusted Probe Particle Time', 'seconds', 'double'],$
                 ['buffertime', 'Buffer Time', 'seconds', 'double'],$
                 ['ipt', 'Interarrival Time', 'seconds', 'float'],$
                 ['diam', 'Particle Diameter from Circle Fit', 'microns', 'float'],$
                 ['xsize', 'X-size (across array)', 'microns', 'float'],$
                 ['ysize', 'Y-size (along airflow)', 'microns', 'float'],$
                 ['areasize', 'Equivalent Area Size', 'microns', 'float'],$
                 ['arearatio', 'Area Ratio', 'unitless', 'float'],$
                 ['aspectratio', 'Aspect Ratio', 'unitless', 'float'],$
                 ['area', 'Number of Pixels Shaded', 'pixels', 'short' ],$
                 ['perimeterarea', 'Number of Perimeter Pixels Shaded', 'pixels', 'short'],$
                 ['allin', 'All-in Flag', 'unitless', 'byte'],$
                 ['centerin', 'Center-in Flag', 'unitless', 'byte'],$
                 ['dof_flag', 'Depth of Field Flag from Probe', 'unitless', 'byte'],$
                 ['edgetouch', 'Edge Touch (1=left, 2=right, 3=both)', 'unitless', 'byte'],$
                 ['zd', 'Z Position from Korolev Correction', 'microns', 'float'],$
                 ['missed', 'Missed Particle Count', 'number', 'short'],$
                 ['overload', 'Overload Flag', 'boolean', 'byte'],$
                 ['particle_counter', 'Particle Counter', 'number', 'long'],$
                 ['orientation', 'Particle Orientation Relative to Array Axis', 'degrees', 'float'],$
                 ['rejection_flag', 'Particle Rejection Code', 'unitless', 'byte']]
       
      tagnames=ncdfprops[0,*]
      FOR i=0,n_elements(tagnames)-1 DO BEGIN
         CASE ncdfprops[3,i] OF
            'float': varid = ncdf_vardef(ncdf_id,tagnames[i], xdimid, /float, gzip=5)
            'double': varid = ncdf_vardef(ncdf_id,tagnames[i], xdimid, /double, gzip=5)
            'byte': varid = ncdf_vardef(ncdf_id,tagnames[i], xdimid, /byte, gzip=5)
            'short': varid = ncdf_vardef(ncdf_id,tagnames[i], xdimid, /short, gzip=5)
            'long': varid = ncdf_vardef(ncdf_id,tagnames[i], xdimid, /long, gzip=5)
         ENDCASE              
         ncdf_attput,ncdf_id,varid,'longname',ncdfprops[1,i]
         ncdf_attput,ncdf_id,varid,'units',ncdfprops[2,i]
      ENDFOR
      ncdf_control,ncdf_id,/endef                ;put in data mode 
   ENDIF
   
   IF op.particlefile eq 1 THEN BEGIN
      fn_pbp=soda2_filename(op,op.shortname,extension='.pbp')
      close, lun_pbp
      openw, lun_pbp, fn_pbp
      ;printf,lun_pbp, ['Time(UTC)', 'IPT(s)', 'Diam(um)', 'XSize(um)', 'YSize(um)', 'AreaRatio', 'AspectRatio', 'Allin(bool)', $
      ;                'Missed', 'Overload', 'Orientation'], format='(99a14)'
      printf, lun_pbp, 'Source: SODA-2 OAP Processing Software'
      printf, lun_pbp, 'Flight Date: ', strmid(op.date,0,2)+'/'+strmid(op.date,2,2)+'/'+strmid(op.date,4,4)
      printf, lun_pbp, 'Data output for each column: '
      printf, lun_pbp, '  1: Particle time [seconds from midnight]'
      printf, lun_pbp, '  2: Raw unadjusted probe particle time [seconds from midnight]'
      printf, lun_pbp, '  3: Buffer time [seconds from midnight]'
      printf, lun_pbp, '  4: Interparticle time [seconds]'
      printf, lun_pbp, '  5: Particle diameter from circle sizing [microns]'
      printf, lun_pbp, '  6: X-size (across array) [microns]'
      printf, lun_pbp, '  7: Y-size (along airflow) [microns]'
      printf, lun_pbp, '  8: Area ratio [unitless]'
      printf, lun_pbp, '  9: Aspect ratio [unitless]'
      printf, lun_pbp, '  10: Particle orientation relative to array axis [degrees]'
      printf, lun_pbp, '  11: All-in flag [boolean]'
      printf, lun_pbp, '  12: Overload flag [boolean]'
      printf, lun_pbp, '  13: Missed particles [number]'
      printf, lun_pbp, '  14: Particle counter [number]'
      printf, lun_pbp, '-------------------------------------------'
   ENDIF
    
   ;====================================================================================================
   ;====================================================================================================
   ;Process buffers that fall into the specified time period.  Write individual
   ;particle data to a structure.
   ;====================================================================================================
   ;====================================================================================================
   
   ;Make sure buffers are sorted
   numbuffs=n_elements(bufftime)
   startdate=julday(strmid(op.date,0,2), strmid(op.date,2,2), strmid(op.date,4,4))
   IF abs(buffdate[0]-startdate) gt 5 THEN BEGIN
      ;Some probes do not have the date right, just use the first one in this case
      caldat,startdate,usermo,userday,useryear
      caldat,buffdate[0],buffmo,buffday,buffyear
      startdate=buffdate[0]
      print,usermo,userday,useryear,buffmo,buffday,buffyear,format='(i3,i3,i5,i5,i3,i5)'
      print,'Probe date stamps do not match user date, continuing...'
   ENDIF   
   bufftime=bufftime+86400d*(buffdate-startdate[0])  ;Midnight crossings  
   s=sort(bufftime)   
   bufftime=bufftime[s]
   buffpoint=buffpoint[s]
   bufffile=bufffile[s]
   buffindex=long((bufftime-op.starttime)/op.rate)  ;keep these for output only
   imagepointers=0  ;Used for SPEC probes only, pointers to each image in a buffer
   
   firstbuff=max(where(bufftime lt op.starttime)) > 0
   lastbuff=min(where(bufftime gt op.stoptime,nlb))
   IF nlb eq 0 THEN lastbuff=numbuffs-1
   IF (lastbuff-firstbuff) le 0 THEN BEGIN
      print,'No buffers in specified time range'    
      return
   ENDIF
   currentfile=-1
   
   ;lastbuffertime=0   
   lastpercentcomplete=0
   istop=-1L
   inewbuffer=lonarr(lastbuff-firstbuff+1)
   FOR i=firstbuff,lastbuff DO BEGIN
      ;Open new file if needed
      IF currentfile ne bufffile[i] THEN BEGIN
         close,1
         openr,1,op.fn[bufffile[i]]
         currentfile=bufffile[i]
      ENDIF
      
      ;Read in buffer
      point_lun,1,buffpoint[i]
      b=soda2_read2dbuffer(1,pop)
      b.time=bufftime[i]   ;In case time changed due to midnight crossing
      timeindex=long((b.time-op.starttime)/op.rate)>0<(numrecords-1)   ;Index each buffer into right time period
      
      ;Commented out b/c time indexing should account for offsets in truetime/reftime
      ;To-do....
      ;IF b.overload gt 0 THEN BEGIN
      ;   timeindex=long((b.time-op.starttime)/op.rate)
      ;   IF (timeindex ge 0) and (timeindex lt numrecords) THEN deadtime[timeindex]=deadtime[timeindex]+b.overload
      ;ENDIF
      
      ;Update miscellaneous
      ;*************Update misc.yres here, not yet implemented
      
      ;Process
      IF (*pop).format eq 'SPEC' THEN BEGIN
         (*pmisc).nimages=y.numimages[i]
         IF (*pmisc).nimages gt 0 THEN $
            (*pmisc).imagepointers=i*y.buffsize + y.imagep[(y.firstp[i]):(y.firstp[i]+y.numimages[i]-1)]
      ENDIF    
      p=soda2_processbuffer(b,pop,pmisc)
      dhist[timeindex,*]=dhist[timeindex,*]+p.dhist

      IF p.rejectbuffer eq 0 THEN BEGIN
        numbuffsaccepted[timeindex]=numbuffsaccepted[timeindex]+1
        ;Write data to structure
        n=n_elements(p.size)
        istart=istop+1
        inewbuffer[i-firstbuff]=istart  ;Save these start positions
        istop=istop+n
        
        x[istart:istop].bufftime=b.time
        x[istart:istop].probetime=p.probetime
        x[istart:istop].reftime=p.reftime
        x[istart:istop].size=p.size
        x[istart:istop].xsize=p.xsize
        x[istart:istop].ysize=p.ysize
        x[istart:istop].areasize=p.areasize
        x[istart:istop].ar=p.ar
        x[istart:istop].aspr=p.aspr
        x[istart:istop].area=p.area_orig 
        x[istart:istop].perimeterarea=p.perimeterarea 
        x[istart:istop].allin=p.allin
        x[istart:istop].centerin=p.centerin
        x[istart:istop].edge_touch=p.edge_touch
        x[istart:istop].tas=b.tas
        x[istart:istop].zd=p.zd
        x[istart:istop].missed=p.missed
        x[istart:istop].particle_count=p.particle_count
        x[istart:istop].overloadflag=p.overloadflag
        x[istart:istop].dof=p.dof
        x[istart:istop].orientation=p.orientation
   
        ;Feedback to user
        percentcomplete=fix(float(i-firstbuff)/(lastbuff-firstbuff)*100)
        IF percentcomplete ne lastpercentcomplete THEN BEGIN
            infoline=strtrim(string(percentcomplete))+'%'
            IF textwidgetid ne 0 THEN widget_control,textwidgetid,set_value=infoline,/append ELSE print,infoline
        ENDIF
        lastpercentcomplete=percentcomplete
      ENDIF ELSE BEGIN
         numbuffsrejected[timeindex]=numbuffsrejected[timeindex]+1
      ENDELSE
      IF (istop+500) gt num2process THEN BEGIN
         ;Memory limit reached, process particles and reset arrays
         soda2_particlesort, pop, x, d, istop, inewbuffer, lun_pbp, ncdf_offset, ncdf_id
         ncdf_offset=ncdf_offset + istop + 1
         istop=-1L         
      ENDIF
      
   ENDFOR
         
   IF istop lt 0 THEN return
   infoline='Sorting Particles...'
   IF textwidgetid ne 0 THEN widget_control,textwidgetid,set_value=infoline,/append ELSE print,infoline
   soda2_particlesort, pop, x, d, istop, inewbuffer, lun_pbp, ncdf_offset, ncdf_id
   close,1


   ;====================================================================================================
   ;====================================================================================================   
   ;Compute concentration, save data
   ;====================================================================================================
   ;====================================================================================================

   spec1d=total(d.spec2d,3)

   numbins=n_elements(op.endbins)-1
   midbins=(float(op.endbins[0:numbins-1])+op.endbins[1:numbins])/2.0
   binwidth=op.endbins[1:numbins]-op.endbins[0:numbins-1]
   sa=fltarr(numbins)
   FOR i=0,numbins-1 DO sa[i]=soda2_samplearea(midbins[i], op.res, op.armwidth, op.numdiodes, op.reconstruct, op.smethod, op.wavelength, centerin=op.centerin)

   ;Assume probe is always active, minus deadtime
   IF op.ignoredeadtime eq 1 THEN activetime=fltarr(numrecords)+op.rate ELSE activetime=(fltarr(numrecords)+op.rate-d.deadtime)>0 
   IF (got_pth eq 1) THEN d.tas=pth_tas
   sv=sa*d.tas*activetime
   
   conc1d=fltarr(numrecords, numbins)  ;size spectra
   
   ;JPL temp
   conc1d_spherical=fltarr(numrecords, numbins)
   conc1d_mediumprolate=fltarr(numrecords, numbins)
   conc1d_oblate=fltarr(numrecords, numbins)
   conc1d_maximumprolate=fltarr(numrecords, numbins) 
   
   ;Orientation
   orientation_index=fltarr(numrecords, numbins)
   FOR i=0L,numrecords-1 DO BEGIN
      spec1d[i,*]=spec1d[i,*]*(d.corr_fac[i] > 1.0)          ;Make the correction
      d.spec2d[i,*,*]=d.spec2d[i,*,*]*(d.corr_fac[i] > 1.0) 
      d.spec2d_aspr[i,*,*]=d.spec2d_aspr[i,*,*]*(d.corr_fac[i] > 1.0)
      IF d.tas[i]*activetime[i] gt 0 THEN BEGIN
         conc1d[i,*]=spec1d[i,*]/(sa*d.tas[i]*activetime[i])/(binwidth/1.0e6) 
         ;JPL temp
         conc1d_spherical[i,*]=d.spec1d_spherical[i,*]/(sa*d.tas[i]*activetime[i])/(binwidth/1.0e6) 
         conc1d_mediumprolate[i,*]=d.spec1d_mediumprolate[i,*]/(sa*d.tas[i]*activetime[i])/(binwidth/1.0e6) 
         conc1d_oblate[i,*]=d.spec1d_oblate[i,*]/(sa*d.tas[i]*activetime[i])/(binwidth/1.0e6) 
         conc1d_maximumprolate[i,*]=d.spec1d_maximumprolate[i,*]/(sa*d.tas[i]*activetime[i])/(binwidth/1.0e6) 
         ;Orientation index computation from histograms
         FOR j=0,numbins-1 DO BEGIN
            omax=max(d.spec2d_orientation[i,j,*], imax)
            totspec=total(d.spec2d_orientation[i,j,*],/nan)
            IF totspec gt 50 THEN orientation_index[i,j]=float(omax)/totspec
         ENDFOR
      ENDIF
   ENDFOR

   data={op:op, time:d.time, tas:d.tas, midbins:midbins, activetime:activetime, Date_Processed:systime(), sa:sa, $
         intspec_all:d.intspec_all, intspec_accepted:d.intspec_accepted, intendbins:intendbins, intmidbins:intmidbins,$  
         count_rejected:d.count_rejected,count_accepted:d.count_accepted, count_missed:d.count_missed, $
         missed_hist:d.missed_hist, conc1d:conc1d, spec1d:spec1d, spec2d:d.spec2d, spec2d_aspr:d.spec2d_aspr,$
         corr_fac:d.corr_fac, poisson_fac:d.poisson_fac, intcutoff:d.intcutoff, zdspec:d.zdspec, zdendbins:d.zdendbins, zdmidbins:d.zdmidbins,$
         pointer:buffpoint, ind:buffindex, currentfile:bufffile, numbuffsaccepted:numbuffsaccepted, numbuffsrejected:numbuffsrejected, dhist:dhist,$
         hist3d:d.hist3d, spec2d_orientation:d.spec2d_orientation, orientation_index:orientation_index}
         ;, conc1d_spherical:conc1d_spherical, conc1d_mediumprolate:conc1d_mediumprolate, conc1d_oblate:conc1d_oblate, conc1d_maximumprolate:conc1d_maximumprolate }
     
   ;Close pointers and luns
   ptr_free, pop, pmisc
   IF op.particlefile eq 1 THEN close,lun_pbp
   IF op.ncdfparticlefile eq 1 THEN ncdf_close,ncdf_id
   
   ;Save data and display notifications
   fn_out=soda2_filename(op,op.shortname)
   IF op.savfile eq 1 THEN BEGIN
      save,file=fn_out,data,/compress
      infoline='Saved file '+fn_out
      IF textwidgetid ne 0 THEN dummy=dialog_message(infoline,dialog_parent=textwidgetid,/info) ELSE print,infoline
   ENDIF
   
   IF op.particlefile eq 1 THEN BEGIN
      infoline='Saved file '+fn_pbp
      IF textwidgetid ne 0 THEN dummy=dialog_message(infoline,dialog_parent=textwidgetid,/info) ELSE print,infoline
   ENDIF
   
   IF op.ncdfparticlefile eq 1 THEN BEGIN
      infoline='Saved file '+fn_ncdf
      IF textwidgetid ne 0 THEN dummy=dialog_message(infoline,dialog_parent=textwidgetid,/info) ELSE print,infoline
   ENDIF

;profiler,/report  
END


