PRO soda2_process_2d, op, textwidgetid=textwidgetid, fn_pbp=fn_pbp, profile=profile
   ;PRO to make 'spectra' files for a 2D probe, and save them
   ;in IDL's native binary format.  This version places particles
   ;individually in the appropriate time period, rather than the
   ;entire buffer.  Limited to probes with absolute time, i.e.
   ;CIP and Fast2D probes.
   ;Aaron Bansemer, NCAR, 2009.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.


   ;Needed when running from GUI
   IF n_elements(textwidgetid) eq 0 THEN textwidgetid=0
   IF n_elements(profile) eq 0 THEN profile=0
   IF profile THEN BEGIN
      resolve_all
      profiler, /reset
      profiler
      profiler, /system
   ENDIF

   ;The current expected structure of op is:
   ;op={fn:fn, date:date, starttime:hms2sfm(starttime), stoptime:hms2sfm(stoptime), format:format, $
   ;subformat:subformat, probetype:probetype, res:res, endbins:endbins, arendbins:arendbins, rate:rate, $
   ;smethod:smethod, pth:pth, particlefile:0, inttime_reject:inttime_reject, reconstruct:1, stuckbits:0, water:water,$
   ;fixedtas:fixedtas, outdir:outdir, project:project, timeoffset:timeoffset, armwidth:armwidth, $
   ;numdiodes:numdiodes, greythresh:greythresh}
   soda2_update_op, op

   ;Option to reprocess data using an already-generated pbp file to save time
   IF n_elements(fn_pbp) eq 0 THEN reprocess=0 ELSE BEGIN
      reprocess=1
      IF file_test(fn_pbp) eq 0 THEN stop, 'PBP file not found.'
      op.ncdfparticlefile=0    ;Don't rewrite particle files since they may overwrite while in process
      op.particlefile=0
   ENDELSE

   pop=ptr_new(op)      ;make a pointer to this for all other programs, these are constants
   ;Keep miscellaneous stuff here, things that change during processing
   misc={f2d_remainder:ulon64arr(512), f2d_remainder_slices:0, lastbufftime:0D, aircrafttas:0.0, probetas:0.0,$
         nimages:0, imagepointers:lon64arr(500), hkpointers:lon64arr(500), lastclock:0d, lastparticlecount:0L, maxsfm:0D, $
         lastdhist:lonarr(op.numdiodes)}
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
   total_count_rejected:lonarr(numrecords),$
   count_missed:lonarr(numrecords) ,$
   missed_hist:fltarr(numrecords,50),$
   spec2d:fltarr(numrecords, numbins, numarbins) ,$
   spec2d_aspr:fltarr(numrecords, numbins, numarbins) ,$
   spec2d_orientation:fltarr(numrecords, numbins, 18) ,$

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
   activetime_sea=dblarr(numrecords)
   date=soda2_parsedate(op.date)
   maxslices=10000000   ;Maximum image length to keep in local memory before writing to netCDF
   image=bytarr(op.numdiodes, maxslices)
   image_offset_local=0ULL
   image_offset=0ULL
   slices_written=0ULL

   ;Set up the particle structure.
   num2process=10000000L ;Limit to reduce memory consumption
   basestruct={buffertime:0d, probetime:0d, reftime:0d, rawtime:0d, inttime:0d, diam:0.0, xsize:0.0, ysize:0.0, xextent:0.0,$
               areasize:0.0, arearatio:0.0, arearatiofilled:0.0, aspectratio:0.0, area:0.0, areafilled:0.0, xpos:0.0, ypos:0.0,$
               allin:0b, centerin:0b, edgetouch:0b, probetas:0.0, aircrafttas:0.0, zd:0.0, sizecorrection:0.0, missed:0.0, $
               overloadflag:0b, orientation:0.0, perimeterarea:0.0, dofflag:0b, particlecounter:0L, oned:0.0, twod:0.0, $
               area75:0.0, numregions:0b, diodegaps:0s, startline:0ull, stopline:0ull}
   x=replicate(basestruct, num2process)


   ;====================================================================================================
   ;====================================================================================================
   ;Get tas from PTH file, if applicable
   ;====================================================================================================
   ;====================================================================================================
   got_pth=0
   pth_tas=fltarr(numrecords)
   IF file_test(op.pth) THEN BEGIN
      suffix=(strsplit(op.pth,'.',/extract))[-1]
      ;IDL sav files
      IF (suffix eq 'dat') or (suffix eq 'sav') THEN BEGIN
         restore, op.pth
         IF total(d.time - data.time) ne 0 THEN BEGIN
            print, 'PTH time does not match, using nearest point.'
            good=where((data.tas gt 0) and (data.tas lt 400) and (data.time ge op.starttime) and (data.time le op.stoptime), ngood)
            IF ngood eq 0 THEN stop, 'Time mismatch for TAS file.'
            itas=(round(data.time[good])-op.starttime)/op.rate ;find index for each variable
            ;Fill TAS array, don't bother with averaging.  Note use of i:*, which makes sure gaps are filled in for hirate data.
            FOR i=0, n_elements(good)-1 DO pth_tas[itas[i]:*] = data.tas[good[i]]
         ENDIF ELSE BEGIN
            pth_tas=data.tas
         ENDELSE
         got_pth=1
         pthdat = data
      ENDIF
      ;ASCII or CSV files, assumes time and tas in first two columns
      IF (suffix eq 'txt') or (suffix eq 'csv') THEN BEGIN
         v=''
         openr,lun,op.pth,/get_lun
         on_ioerror, bad  ;Use to suppress type conversion errors
         REPEAT BEGIN
            readf,lun,v
            fields=float(strsplit(v, '[ ,' + STRING(9B) + ']+', /regex, /extract))
            i=(round(fields[0])-op.starttime)/op.rate > 0 ;find index for each variable
            ;Fill TAS array, don't bother with averaging.  Note use of i:*, which makes sure gaps are filled in for hirate data.
            IF (n_elements(fields) gt 1) && (i ge 0) and (i lt numrecords) and (fields[1] gt 0) and (fields[1] lt 500) THEN pth_tas[i:*]=fields[1]
            bad:dummy=0
         ENDREP UNTIL eof(lun)
         on_ioerror, null
         free_lun,lun
         got_pth=1
      ENDIF
      ;NetCDF (NCAR-style for now)
      IF (suffix eq 'nc') THEN BEGIN
         restorenc, op.pth, varlist=['TIME','TASX']
         ;Very basic error check
         good=where((data.tasx gt 0) and (data.tasx lt 400) and (data.time ge op.starttime) and (data.time le op.stoptime))
         itas=(round(data.time[good])-op.starttime)/op.rate ;find index for each variable
         ;Fill TAS array, don't bother with averaging.  Note use of i:*, which makes sure gaps are filled in for hirate data.
         FOR i=0,n_elements(good)-1 DO pth_tas[itas[i]:*] = data.tasx[good[i]]
         got_pth=1
      ENDIF
   ENDIF ELSE BEGIN
      IF op.fixedtas gt 0 THEN BEGIN
         pth_tas+=op.fixedtas
         got_pth=1
      ENDIF

      IF op.pth ne '' THEN BEGIN  ;Extra warning if a filename was actually entered
         infoline='TAS file does not exist.  Use default air speed instead?'
         IF textwidgetid ne 0 THEN BEGIN
            dummy=dialog_message(infoline,dialog_parent=textwidgetid,/question)
            IF dummy eq 'No' THEN return
         ENDIF ELSE BEGIN
            print,'TAS file '+op.pth+' does not exist. Stopping.'
            stop
         ENDELSE
      ENDIF ELSE print,'No TAS file entered, using default or fixed values'
   ENDELSE


   ;====================================================================================================
   ;====================================================================================================
   ;Set up PBP files
   ;====================================================================================================
   ;====================================================================================================
   lun_pbp=2
   ncdf_offset=0L
   ncdf_id=0L

   ;List of variables to include in netCDF file: [varname, longname, unit, datatype, ascii_format]
   pbpprops=[['time', 'UTC time', 'seconds', 'double', 'f0.5'],$
              ['probetime', 'Unadjusted probe particle time', 'seconds', 'double', 'f0.5'],$
              ['buffertime', 'Buffer time', 'seconds', 'double', 'f0.5'],$
              ['rawtime', 'Raw time', 'slices or seconds', 'double', 'f0.5'],$
              ['reftime', 'Reference time for buffer matching', 'seconds', 'double', 'f0.5'],$
              ['inttime', 'Interarrival time from previous particle', 'seconds', 'float', 'e0.5'],$
              ['diam', 'Particle diameter from circle fit. No Poisson spot size corrections applied', 'microns', 'float', 'f0.1'],$
              ['xsize', 'X-size (across array). No Poisson spot size corrections applied', 'microns', 'float', 'f0.1'],$
              ['ysize', 'Y-size (along airflow). No Poisson spot size corrections applied', 'microns', 'float', 'f0.1'],$
              ['xextent', 'Maximum x-size (across array) for a single slice. No Poisson spot size corrections applied', 'microns', 'float', 'f0.1'],$
              ['oned', '1-D emulation size. Number of latched pixels. No Poisson spot size corrections applied', 'microns', 'float', 'f0.1'],$
              ['twod', '2-D emulation size. Maximum number of shaded pixels on a single slice. No Poisson spot size corrections applied', 'microns', 'float', 'f0.1'],$
              ['areasize', 'Equivalent area size. No Poisson spot size corrections applied', 'microns', 'float', 'f0.1'],$
              ['arearatio', 'Area ratio', 'unitless', 'float', 'f0.2'],$
              ['arearatiofilled', 'Area ratio with particle voids filled', 'unitless', 'float', 'f0.2'],$
              ['aspectratio', 'Aspect ratio', 'unitless', 'float', 'f0.2'],$
              ['area', 'Number of shaded pixels', 'pixels', 'short', 'i0'],$
              ['areafilled', 'Number of shaded pixels including voids', 'pixels', 'short', 'i0'],$
              ['perimeterarea', 'Number of shaded pixels on particle perimeter', 'pixels', 'short', 'i0'],$
              ['area75', 'Number of shaded pixels at the 75% (or grey level-3) shading', 'pixels', 'short', 'i0'],$
              ['xpos', 'X-position of particle center (across array)', 'pixels', 'float', 'f0.2'],$
              ['ypos', 'Y-position of particle center (along airflow)', 'pixels', 'float', 'f0.2'],$
              ['allin', 'All-in flag (1=all-in)', 'unitless', 'byte','i0'],$
              ['centerin', 'Center-in flag (1=center-in)', 'unitless', 'byte','i0'],$
              ['dofflag', 'Depth of field flag from probe, if available (1=accepted)', 'unitless', 'byte','i0'],$
              ['edgetouch', 'Edge touch (1=left 2=right 3=both)', 'unitless', 'byte','i0'],$
              ['sizecorrection', 'Size correction factor from Korolev 2007 (D_edge/D0). Use to adjust sizes in this file if necessary', 'unitless', 'float', 'f0.2'],$
              ['zd', 'Z position from Korolev correction', 'microns', 'float', 'f0.2'],$
              ['missed', 'Missed particle count', 'number', 'float', 'i0'],$
              ['probetas', 'True air speed for probe clock', 'm/s', 'float', 'f0.2'],$
              ['aircrafttas', 'True air speed for aircraft (if available)', 'm/s', 'float', 'f0.2'],$
              ['overloadflag', 'Overload flag', 'boolean', 'byte', 'i0'],$
              ['particlecounter', 'Particle counter', 'number', 'long', 'i0'],$
              ['orientation', 'Particle orientation relative to array axis', 'degrees', 'float', 'f0.2'],$
              ['rejectionflag', 'Particle rejection code (see soda2_reject.pro for codes)', 'unitless', 'byte', 'i0'],$
              ['numregions', 'Number of connected regions (blobs) in the particle image, if the KEEP_LARGEST option is enabled', 'count', 'byte', 'i0'],$
              ['diodegaps', 'Number of unshaded diodes between the first and last shaded diodes', 'count', 'short', 'i0']]

   ;NetCDF PBP setup
   IF op.ncdfparticlefile ge 1 THEN BEGIN
      fn_ncdf=soda2_filename(op,op.shortname,extension='.pbp.nc')
      file_delete, fn_ncdf, /quiet ;The 'clobber' switch does not work on ncdf_create with netcdf4
      ncdf_id=ncdf_create(fn_ncdf, /netcdf4_format)

      ;Define the x-dimension, should be used in all variables
      xdimid=ncdf_dimdef(ncdf_id,'Time',/unlimited)

      ;These are for ncplot compatibility
      opnames=tag_names(op)
      flightdate=date.month+'/'+date.day+'/'+date.year

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

      tagnames=pbpprops[0,*]
      FOR i=0,n_elements(tagnames)-1 DO BEGIN
         CASE pbpprops[3,i] OF
            'float': varid = ncdf_vardef(ncdf_id,tagnames[i], xdimid, /float, gzip=5)
            'double': varid = ncdf_vardef(ncdf_id,tagnames[i], xdimid, /double, gzip=5)
            'byte': varid = ncdf_vardef(ncdf_id,tagnames[i], xdimid, /byte, gzip=5)
            'short': varid = ncdf_vardef(ncdf_id,tagnames[i], xdimid, /short, gzip=5)
            'long': varid = ncdf_vardef(ncdf_id,tagnames[i], xdimid, /long, gzip=5)
         ENDCASE
         ncdf_attput,ncdf_id,varid,'longname',pbpprops[1,i]
         ncdf_attput,ncdf_id,varid,'units',pbpprops[2,i]
      ENDFOR

      ;Add a few more variables and dimensions if saving images
      IF op.ncdfparticlefile eq 2 THEN BEGIN
         diodedimid = ncdf_dimdef(ncdf_id, 'Diode', /unlimited)  ;make both dimensions for the image unlimited, or errors result
         slicedimid = ncdf_dimdef(ncdf_id, 'Slice', /unlimited)
         ;Start/stop slice indexes
         varid = ncdf_vardef(ncdf_id, 'starty', xdimid, /ulong, gzip=5)
         ncdf_attput, ncdf_id,varid, 'longname', 'Index of start slice (y-position) in image array'
         ncdf_attput, ncdf_id,varid, 'units', 'unitless'
         varid = ncdf_vardef(ncdf_id, 'stopy', xdimid, /ulong, gzip=5)
         ncdf_attput, ncdf_id, varid, 'longname', 'Index of stop slice (y-position) in image array'
         ncdf_attput, ncdf_id, varid, 'units', 'unitless'
         varid = ncdf_vardef(ncdf_id, 'startx', xdimid, /ulong, gzip=5)
         ncdf_attput, ncdf_id,varid, 'longname', 'Index of start diode (x-position) in image array'
         ncdf_attput, ncdf_id,varid, 'units', 'unitless'
         varid = ncdf_vardef(ncdf_id, 'stopx', xdimid, /ulong, gzip=5)
         ncdf_attput, ncdf_id, varid, 'longname', 'Index of stop diode (x-position) in image array'
         ncdf_attput, ncdf_id, varid, 'units', 'unitless'
         ;Image array
         imageid = ncdf_vardef(ncdf_id, 'image', [diodedimid, slicedimid], /ubyte, gzip=8, chunk=[op.numdiodes, 1024])
         ncdf_attput, ncdf_id, imageid, 'longname', 'Particle images'
         ncdf_attput, ncdf_id, imageid, 'units', 'unitless'
      END

      ;Add a few more variables if lat, lon, alt, t are available
      IF (n_elements(pthdat) gt 0) THEN BEGIN
         IF (total(tag_names(pthdat) eq 'LAT') eq 1) THEN BEGIN
            varid = ncdf_vardef(ncdf_id, 'lat', xdimid, /double, gzip=5)
            ncdf_attput, ncdf_id,varid, 'longname', 'Latitude'
            ncdf_attput, ncdf_id,varid, 'units', 'degrees'
         ENDIF
         IF (total(tag_names(pthdat) eq 'LON') eq 1) THEN BEGIN
            varid = ncdf_vardef(ncdf_id, 'lon', xdimid, /double, gzip=5)
            ncdf_attput, ncdf_id, varid, 'longname', 'Longitude'
            ncdf_attput, ncdf_id, varid, 'units', 'degrees'
         ENDIF
            IF (total(tag_names(pthdat) eq 'GALT') eq 1) THEN BEGIN
            varid = ncdf_vardef(ncdf_id, 'alt', xdimid, /double, gzip=5)
            ncdf_attput, ncdf_id, varid, 'longname', 'GPS altitude'
            ncdf_attput, ncdf_id, varid, 'units', 'meters'
         ENDIF
            IF (total(tag_names(pthdat) eq 'T') eq 1) THEN BEGIN
            varid = ncdf_vardef(ncdf_id, 't', xdimid, /double, gzip=5)
            ncdf_attput, ncdf_id, varid, 'longname', 'Temperature'
            ncdf_attput, ncdf_id, varid, 'units', 'degrees Celsius'
         ENDIF
      ENDIF

      ncdf_control,ncdf_id,/endef                ;put in data mode
   ENDIF

   ;ASCII/CSV PBP
   IF op.particlefile eq 1 THEN BEGIN
      fn_pbp=soda2_filename(op,op.shortname,extension='.pbp.csv')
      close, lun_pbp
      openw, lun_pbp, fn_pbp

      printf, lun_pbp, 'Source: SODA-2 OAP Processing Software'
      printf, lun_pbp, 'Flight Date (mm/dd/yyyy): ', date.month+'/'+date.day+'/'+date.year
      printf,lun_pbp, 'Date Processed: ' + systime()

      printf, lun_pbp, 'Variable Descriptions: '
      tagnames=pbpprops[0,*]

      ;Print long-form header
      FOR i=0, n_elements(tagnames)-1 DO printf, lun_pbp, string(i+1, format='(4i)')+'. '+pbpprops[0,i]+': '+pbpprops[1,i]+' ['+pbpprops[2,i]+']'

      ;Print short-form header
      printf, lun_pbp, '-------------------------------------------'
      printf,lun_pbp, pbpprops[0,*], format='(99(a0,:,","))'  ;The colon suppresses final comma
   ENDIF


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
      ENDIF
      IF y.error eq 1 THEN BEGIN
         infoline = 'Error building index, verify probe ID and/or SEA tags are correct.'
         IF textwidgetid ne 0 THEN BEGIN
            dummy=dialog_message(infoline,dialog_parent=textwidgetid,/info)
            return
         ENDIF ELSE stop,infoline
      ENDIF
   ENDFOR

   ;Get housekeeping data, if available
   house={op:op}
   IF op.format eq 'SPEC' THEN spec_process_hk, op, y=y, /nosav, data=house
   IF (op.format eq 'SEA') and (op.probetype eq '1D2D') THEN process_hk_1d2d, op, /nosav, data=house
   IF (op.format eq 'SEA') and (op.probetype eq 'CIP') and (n_elements(op.seatag) ge 2) THEN $
      process_cip1d_sea, op, /nowrite, data=house

   ;====================================================================================================
   ;====================================================================================================
   ;Process buffers that fall into the specified time period.  Write individual
   ;particle data to a structure.
   ;====================================================================================================
   ;====================================================================================================

   ;Make sure buffers are sorted
   numbuffs=n_elements(bufftime)
   startdate=julday(date.month, date.day, date.year)
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
   pbpstartindex=lonarr(numbuffs)

   firstbuff=max(where(bufftime lt op.starttime)) > 0
   lastbuff=min(where(bufftime gt op.stoptime,nlb))
   IF nlb eq 0 THEN lastbuff=numbuffs-1
   IF (lastbuff-firstbuff) le 0 THEN BEGIN
      print,'No buffers in specified time range'
      return
   ENDIF

   IF reprocess eq 0 THEN BEGIN

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

         ;Process
         IF (*pop).format eq 'SPEC' THEN BEGIN
            (*pmisc).nimages=y.numimages[i]
            IF (*pmisc).nimages gt 0 THEN BEGIN
               (*pmisc).imagepointers=i*y.buffsize + y.imagep[(y.firstp[i]):(y.firstp[i]+y.numimages[i]-1)]
               (*pmisc).hkpointers=y.hkp[y.ihk[(y.firstp[i]):(y.firstp[i]+y.numimages[i]-1)]]
               (*pmisc).probetas=interpol(house.tas, house.time, b.time)   ;Backup TAS when using separate HK file without pointers (3V/Hawkeye)
            ENDIF
         ENDIF

         ;Send current aircraft TAS to processbuffer for y-sizing on SPEC probes
         (*pmisc).aircrafttas=pth_tas[timeindex]

         ;Process the buffer
         p=soda2_processbuffer(b,pop,pmisc)

         ;Active/dead time computation with SEA buffers which contain both a start time and a stop time.
         ;Must be after soda2_processbuffer since time updates can happen there.
         IF (*pop).format eq 'SEA' THEN BEGIN
            timeindex_stoptime=long((b.stoptime-op.starttime)/op.rate)>0<(numrecords-1)

            ;Add activetime if buffer starts and stop in the same time period
            IF timeindex eq timeindex_stoptime THEN activetime_sea[timeindex] += (b.stoptime-b.time)

            ;If buffer brackets 2+ time periods:
            IF (timeindex lt timeindex_stoptime) and (timeindex_stoptime lt numrecords) THEN BEGIN
               activetime_sea[timeindex] += ((d.time[timeindex+1]-b.time) < op.rate)  ;(Start of next time period) - (buffer start)
               activetime_sea[timeindex_stoptime] += (b.stoptime - d.time[timeindex_stoptime]) ; (buffer stop)-(last time period start)
               IF (timeindex_stoptime - timeindex) gt 1 THEN activetime_sea[(timeindex+1):(timeindex_stoptime-1)]=op.rate ;Fill intervening buffers
            ENDIF
         ENDIF

         ;Compute activetime from missed particle counts instead.  This is primarily for older DMT probes when they
         ;have a reliable particle counter.  Newer greyscale and mono-grey probes probably won't work.  Should take
         ;care of time gaps between SEA buffers and gaps within the buffers during probe overload.  Initially
         ;used for HIWC 2022.
         IF (*pop).activetimefrommissed eq 1 THEN BEGIN
            stop  ;Under development
         ENDIF

         ;Update diode histogram
         dhist[timeindex,*]=dhist[timeindex,*]+p.dhist
         (*pmisc).lastdhist=dhist[(timeindex-1)>0,*]  ;Use previous time period for detecting streaks and fixing soda2_processbuffer

         ;Get particle metrics
         IF p.rejectbuffer eq 0 THEN BEGIN
            numbuffsaccepted[timeindex]=numbuffsaccepted[timeindex]+1
            ;Write data to structure
            n=n_elements(p.diam)
            istart=istop+1
            inewbuffer[i-firstbuff]=istart  ;Save these start positions
            pbpstartindex[i]=istart+ncdf_offset ;Save for final output
            istop=istop+n

            x[istart:istop].buffertime=b.time
            x[istart:istop].probetime=p.probetime
            x[istart:istop].reftime=p.reftime
            x[istart:istop].rawtime=p.rawtime
            x[istart:istop].inttime=p.inttime
            x[istart:istop].diam=p.diam
            x[istart:istop].xsize=p.xsize
            x[istart:istop].ysize=p.ysize
            x[istart:istop].xextent=p.xextent
            x[istart:istop].oned=p.oned
            x[istart:istop].twod=p.twod
            x[istart:istop].areasize=p.areasize
            x[istart:istop].arearatio=p.ar
            x[istart:istop].arearatiofilled=p.arfilled
            x[istart:istop].aspectratio=p.aspr
            x[istart:istop].area=p.area_orig
            x[istart:istop].areafilled=p.area_filled
            x[istart:istop].perimeterarea=p.perimeterarea
            x[istart:istop].area75=p.area75
            x[istart:istop].allin=p.allin
            x[istart:istop].centerin=p.centerin
            x[istart:istop].edgetouch=p.edgetouch
            x[istart:istop].probetas=p.clocktas
            x[istart:istop].aircrafttas=pth_tas[timeindex]
            x[istart:istop].zd=p.zd
            x[istart:istop].sizecorrection=p.sizecorrection
            x[istart:istop].xpos=p.xpos
            x[istart:istop].ypos=p.ypos
            x[istart:istop].numregions=p.numregions
            x[istart:istop].diodegaps=p.diodegaps
            x[istart:istop].missed=p.missed
            x[istart:istop].particlecounter=p.particlecounter
            x[istart:istop].overloadflag=p.overloadflag
            x[istart:istop].dofflag=p.dofflag
            x[istart:istop].orientation=p.orientation

            IF op.ncdfparticlefile eq 2 THEN BEGIN
               nslices = (size(p.bitimage, /dim))[1]   ;Recompute, can be different than p.nslices
               image[*, image_offset_local:image_offset_local+nslices-1] = p.bitimage
               x[istart:istop].startline = p.startline+image_offset
               x[istart:istop].stopline = p.stopline+image_offset
               image_offset += nslices        ;Full offset of full image array
               image_offset_local += nslices  ;Offset of current image array, written periodically to netCDF
            ENDIF

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
         IF ((istop+500) gt num2process) or ((image_offset_local+10000) gt maxslices) THEN BEGIN
            ;Memory limit reached, process particles and reset arrays
            soda2_particlesort, pop, x, d, istop, inewbuffer, lun_pbp, ncdf_offset, ncdf_id, pbpprops, $
               image[*, 0:image_offset_local-1], slices_written
            ncdf_offset=ncdf_offset + istop + 1
            istop=-1L
            slices_written += image_offset_local
            image_offset_local = 0
         ENDIF
      ENDFOR

      IF istop lt 0 THEN return
      infoline='Sorting Particles...'
      IF textwidgetid ne 0 THEN widget_control,textwidgetid,set_value=infoline,/append ELSE print,infoline
      soda2_particlesort, pop, x, d, istop, inewbuffer, lun_pbp, ncdf_offset, ncdf_id, pbpprops, $
         image[*, 0:image_offset_local-1], slices_written
      close,1


   ENDIF ELSE BEGIN  ;Reprocessing IF statement

      ;Reprocess straight from PBP netCDF file
      restorenc, fn_pbp, 'pbp', var='time'  ;Just to get number of particles
      infoline='Sorting Particles...'
      IF textwidgetid ne 0 THEN widget_control,textwidgetid,set_value=infoline,/append ELSE print,infoline

      ;2DS time recomputation, this is necessary to get good dead time estimates when rate is 1Hz or greater
      IF (*pop).format eq 'SPEC' THEN BEGIN
         infoline='Recomputing improved probe time...'
         IF textwidgetid ne 0 THEN widget_control,textwidgetid,set_value=infoline,/append ELSE print,infoline
         newtime=spec_newtime(fn_pbp)
         gotnt=1
      ENDIF ELSE gotnt=0

      FOR i = 0, n_elements(pbp.time)/num2process DO BEGIN  ;Don't apply -1 to i, last iteration does remainder particles
         x=soda2_restore_pbp(fn_pbp, offset=i*num2process, count=num2process)
         istop=n_elements(x.time)-1
         inewbuffer=0  ;Only for 2DC/2DP, ignore for now, should cause crash if tried

         ;Replace buffertime with newtime for SPEC probes
         ;IF gotnt eq 1 THEN x.buffertime=newtime[i*num2process:i*num2process+n_elements(x.buffertime)-1]
         IF (*pop).format eq 'SPEC' THEN reprocessed_time = newtime[i*num2process:i*num2process+n_elements(x.buffertime)-1]
         ncdf_offset = num2process*i
         soda2_particlesort, pop, x, d, istop, inewbuffer, lun_pbp, ncdf_offset, ncdf_id, pbpprops, $
            reprocessed_time=reprocessed_time

         percentcomplete=fix(float(i+1)*num2process / n_elements(pbp.time) * 100) < 100
         infoline=strtrim(string(percentcomplete))+'%'
         IF textwidgetid ne 0 THEN widget_control,textwidgetid,set_value=infoline,/append ELSE print,infoline
      ENDFOR

      IF (*pop).format eq 'SEA' THEN print, 'NOTE: Activetime from SEA buffers not computed.  Process from scratch if needed.'
   ENDELSE

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

   ;Compute the number of active diodes for HVPS-1 or probes with partially active arrays
   numactivediodes = op.numdiodes  ;Default to full range
   IF (op.dioderange[0] ne 0) or (op.dioderange[1] ne 0) THEN numactivediodes = op.dioderange[1] - op.dioderange[0] + 1

   ;Sample area, will use op.customdof if nonzero
   ;FOR i=0,numbins-1 DO sa[i]=soda2_samplearea(midbins[i], op.res, op.armwidth, numactivediodes, op.eawmethod, $
   ;   op.smethod, op.wavelength, op.dofconst, dof=op.customdof[i])
   sa=soda2_samplearea(midbins, op.res, op.armwidth, numactivediodes, op.eawmethod, $
      op.smethod, op.wavelength, op.dofconst, dof=op.customdof)

   ;Assume probe is always active, minus deadtime
   IF op.ignoredeadtime eq 1 THEN BEGIN
      activetime=fltarr(numrecords)+op.rate
   ENDIF ELSE BEGIN
      activetime=(fltarr(numrecords)+op.rate-d.deadtime) > 0 < (*pop).rate
      ;Using overload flag now for CIP/PIP deadtime in processbuffer and particlesort, not activetime_sea.
      IF ((*pop).format eq 'SEA') and ((*pop).probetype ne 'CIP') THEN activetime = activetime_sea < (*pop).rate
   ENDELSE

   ;Figure out which TAS to use for concentration and compute sample volume
   probetas=d.tas
   IF (got_pth eq 1) THEN aircrafttas=pth_tas ELSE aircrafttas=probetas
   IF max(aircrafttas) lt 10 THEN print, 'Check TAS, too low, likely not valid'
   sv=sa*aircrafttas*activetime

   ;Orientation
   orientation_index=fltarr(numrecords, numbins)

   ;Make count/concentration arrays
   conc1d=fltarr(numrecords, numbins)  ;size spectra
   FOR i=0L,numrecords-1 DO BEGIN
      spec1d[i,*]=spec1d[i,*]*(d.corr_fac[i] > 1.0)          ;Make the correction
      d.spec2d[i,*,*]=d.spec2d[i,*,*]*(d.corr_fac[i] > 1.0)
      d.spec2d_aspr[i,*,*]=d.spec2d_aspr[i,*,*]*(d.corr_fac[i] > 1.0)
      IF aircrafttas[i]*activetime[i] gt 0 THEN BEGIN
         conc1d[i,*]=spec1d[i,*]/(sa*aircrafttas[i]*activetime[i])/(binwidth/1.0e6)
         ;Orientation index computation from histograms
         FOR j=0,numbins-1 DO BEGIN
            omax=max(d.spec2d_orientation[i,j,*], imax)
            totspec=total(d.spec2d_orientation[i,j,*],/nan)
            IF totspec gt 50 THEN orientation_index[i,j]=float(omax)/totspec
         ENDFOR
      ENDIF
   ENDFOR

   currenttime=string(systime(/julian), format='(C(CYI, "/", CMOI2.2, "/", CDI2.2, " ", CHI2.2, ":", CMI2.2, ":", CSI2.2))')
   data={op:op, time:d.time, tas:aircrafttas, probetas:probetas, midbins:midbins, activetime:activetime, $
         Date_Processed:currenttime, sa:sa, intspec_all:d.intspec_all, intspec_accepted:d.intspec_accepted, $
         intendbins:intendbins, intmidbins:intmidbins, count_rejected:d.count_rejected, $
         total_count_rejected:d.total_count_rejected, count_accepted:d.count_accepted, count_missed:d.count_missed, $
         missed_hist:d.missed_hist, conc1d:conc1d, spec1d:spec1d, spec2d:d.spec2d, spec2d_aspr:d.spec2d_aspr, $
         corr_fac:d.corr_fac, poisson_fac:d.poisson_fac, intcutoff:d.intcutoff, zdspec:d.zdspec, $
         zdendbins:d.zdendbins, zdmidbins:d.zdmidbins, pointer:buffpoint, ind:buffindex, currentfile:bufffile, $
         numbuffsaccepted:numbuffsaccepted, numbuffsrejected:numbuffsrejected, dhist:dhist, hist3d:d.hist3d, $
         spec2d_orientation:d.spec2d_orientation, orientation_index:orientation_index, house:house, $
         pbpstartindex:pbpstartindex}

   ;Close pointers and luns
   ptr_free, pop, pmisc
   infoline=['Saved file:']
   IF op.particlefile eq 1 THEN BEGIN
      close,lun_pbp
      infoline=[infoline, fn_pbp]
   ENDIF
   IF op.ncdfparticlefile ge 1 THEN BEGIN
      ;Add in PTH data if available
      IF (n_elements(pthdat) gt 0) THEN BEGIN
         varid=ncdf_varid(ncdf_id,'time')
         ncdf_varget, ncdf_id, varid, ptime
         ;Check to make sure each tag exists first
         IF (total(tag_names(pthdat) eq 'LAT') eq 1) THEN BEGIN
            varid=ncdf_varid(ncdf_id,'lat')
            ncdf_varput, ncdf_id, varid, interpol(pthdat.lat, pthdat.time, ptime)
         ENDIF
         IF (total(tag_names(pthdat) eq 'LON') eq 1) THEN BEGIN
            varid=ncdf_varid(ncdf_id,'lon')
            ncdf_varput, ncdf_id, varid, interpol(pthdat.lon, pthdat.time, ptime)
         ENDIF
         IF (total(tag_names(pthdat) eq 'GALT') eq 1) THEN BEGIN
            varid=ncdf_varid(ncdf_id,'alt')
            ncdf_varput, ncdf_id, varid, interpol(pthdat.galt, pthdat.time, ptime)
         ENDIF
         IF (total(tag_names(pthdat) eq 'T') eq 1) THEN BEGIN
            varid=ncdf_varid(ncdf_id,'t')
            ncdf_varput, ncdf_id, varid, interpol(pthdat.t, pthdat.time, ptime)
         ENDIF
      ENDIF
      ncdf_close,ncdf_id
      infoline=[infoline, fn_ncdf]
   ENDIF
   IF op.asciipsdfile eq 1 THEN BEGIN
      fn_asciipsd=soda2_filename(op,op.shortname,extension='.txt')
      soda2_export_ascii, data, outfile=fn_asciipsd
      infoline=[infoline, fn_asciipsd]
   ENDIF

   ;Save data and display notification
   fn_out=soda2_filename(op,op.shortname)
   IF op.savfile eq 1 THEN BEGIN
      save,file=fn_out,data,/compress
      infoline=[infoline,fn_out]
      IF textwidgetid ne 0 THEN BEGIN
         dummy=dialog_message([infoline, '', 'Browse data?'],dialog_parent=textwidgetid,/question,/default_no)
         IF dummy eq 'Yes' THEN call_procedure, 'soda2_browse', fn_out
      ENDIF ELSE print,infoline
   ENDIF ELSE BEGIN  ;Notify if sav file not written (no option to browse)
      IF textwidgetid ne 0 THEN dummy=dialog_message(infoline,dialog_parent=textwidgetid,/info) ELSE print,infoline
   ENDELSE

IF profile THEN profiler,/report
END
