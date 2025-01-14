PRO soda2_export_ncdf, data, outfile=outfile, pthfile=pthfile, lite=lite, noskip=noskip
   ;PRO to export a netCDF file with the variables contained in a data structure.
   ;Similar to soda2_export_ncdf_raf, but follows a simpler format,
   ;   without the 'SPS' dimension, PSD padding, reversed dimensions, etc.
   ;Aaron Bansemer, NCAR, 8/2014
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.


   IF n_elements(lite) eq 0 THEN lite=0  ;Option to avoid writing 2D matrices
   IF n_elements(noskip) eq 0 THEN noskip=0 ;Option to write everything in the sav file, even with unknown attributes
   !quiet=1  ;Suppress annoying messages from ncdf routines

   ;-----------Create new file-----------------
   ;Create the file, erasing existing one if it exists
   id=ncdf_create(outfile[0],/clobber)

   tags=tag_names(data)
   opnames=tag_names(data.op)

   ;Add endbins to main structure to make code below easier
   IF (total(opnames eq 'ENDBINS') eq 1) and (total(tags eq 'ENDBINS') eq 0) THEN BEGIN
      data = create_struct(data, 'endbins', data.op.endbins)
      tags=tag_names(data)
   ENDIF

   ;Define the x-dimension, should be used in all variables
   xdimid=ncdf_dimdef(id,'Time',n_elements(data.time))
   IF total(opnames eq 'ENDBINS') THEN BEGIN
      ;This is for numbins
      name='Vector'+strtrim(string(n_elements(data.op.endbins)-1),2)
      ydimid_size=ncdf_dimdef(id,name,n_elements(data.op.endbins)-1)
      name='Vector'+strtrim(string(n_elements(data.op.endbins)),2)
      ydimid_sizeend=ncdf_dimdef(id,name,n_elements(data.op.endbins))
   END
   ;This is for interarrival
   IF total(tags eq 'INTMIDBINS') THEN BEGIN
      name='Vector'+strtrim(string(n_elements(data.intmidbins)),2)
      ydimid_int=ncdf_dimdef(id,name,n_elements(data.intmidbins))
   ENDIF
   IF total(tags eq 'INTENDBINS') THEN BEGIN
      name='Vector'+strtrim(string(n_elements(data.intendbins)),2)
      ydimid_intend=ncdf_dimdef(id,name,n_elements(data.intendbins))
   ENDIF
   ;This is for area ratio
   IF total(tags eq 'ARENDBINS') THEN BEGIN
      name='Vector'+strtrim(string(n_elements(data.op.arendbins)-1),2)
      ydimid_ar=ncdf_dimdef(id,name,n_elements(data.op.arendbins)-1)
   ENDIF
   ;These are for ncplot compatibility
   opnames=tag_names(data.op)
   ms=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
   date=soda2_parsedate(data.op.date)
   days1970=date.julday-julday(1,1,1970)
   flightdate=date.month+'/'+date.day+'/'+date.year

   tb='0000'+strtrim(string(sfm2hms(min(data.time))),2)
   te='0000'+strtrim(string(sfm2hms(max(data.time))),2)
   starttime=strmid(tb,5,2,/r)+':'+strmid(tb,3,2,/r)+':'+strmid(tb,1,2,/r)
   stoptime=strmid(te,5,2,/r)+':'+strmid(te,3,2,/r)+':'+strmid(te,1,2,/r)
   str=starttime+'-'+stoptime

   ;Create global attributes
   ncdf_attput,id,'Source','SODA-2 OAP Processing Software',/global
   ncdf_attput,id,'DateCreated',systime(),/global
   ncdf_attput,id,'FlightDate',flightdate[0],/global
   IF total(tags eq 'DATE_PROCESSED') THEN ncdf_attput,id,'DateProcessed',data.date_processed,/global
   ncdf_attput,id,'TimeInterval',str,/global
   FOR i=0,n_elements(opnames)-1 DO BEGIN
         IF string(data.op.(i)[0]) eq '' THEN data.op.(i)[0]='none' ;To avoid an ncdf error (empty string)
         ncdf_attput,id,opnames[i],data.op.(i)[0],/global
   ENDFOR

   ;Write time data
   attname=['long_name','units']
   attvalue=['Elapsed time','seconds since '+date.year+'-'+date.month+'-'+date.day+' '+starttime+' +0000']
   timeid=ncdf_vardef(id,'elapsed_time',xdimid,/double)
   FOR k=0,n_elements(attname)-1 DO ncdf_attput,id,timeid,attname[k],attvalue[k]

   attvalue=['Base time','seconds since 01/01/1970']
   baseid=ncdf_vardef(id,'base_time',/double)
   FOR k=0,n_elements(attname)-1 DO ncdf_attput,id,baseid,attname[k],attvalue[k]

   attvalue=['UTC time','seconds from midnight of start date']
   utcid=ncdf_vardef(id,'utc_time',xdimid,/double)
   FOR k=0,n_elements(attname)-1 DO ncdf_attput,id,utcid,attname[k],attvalue[k]

   ncdf_control,id,/endef                ;put in data mode
   ncdf_varput,id,utcid,data.time
   ncdf_varput,id,baseid,data.time[0]+days1970*86400l
   ncdf_varput,id,timeid,data.time-data.time[0]
   ncdf_control,id,/redef                ;return to define mode

   ;-------Write data to file, start with main structure tags------------------------
   FOR j=0,n_elements(tags)-1 DO BEGIN
      ;Write each variable
      skiptag=0
      dims=xdimid
      attname=['long_name','units']
      attvalue=['Unknown','Unknown']
      unitadjust=1.0
      currentdata=data.(j)
      tagname=tags[j]

      CASE tags[j] OF
         'SA':BEGIN
            attvalue={a1:'Sample Area', a2:'m2'}
            dims=ydimid_size
         END
         'COUNT_ACCEPTED':BEGIN
            attvalue={a1:'Count Accepted', a2:'#'}
         END
         'TAS':BEGIN
            attvalue={a1:'True Air speed used', a2:'m/s'}
         END
         'ACTIVETIME':BEGIN
            attvalue={a1:'Probe activity time', a2:'s'}
         END
         'CONC1D': BEGIN
            binwidth = data.op.endbins[1:*] - data.op.endbins
            attname=['long_name', 'units', 'Bin_endpoints', 'Bin_width', 'Bin_units']
            attvalue={a0:'Particle Concentration Per Size Bin, Normalized by Bin Width', a1:'#/m4', $
			         a2:data.op.endbins, a3:binwidth, a4:'micrometers'}
            dims=[xdimid, ydimid_size]
            tagname='CONCENTRATION'
         END
         'SPEC1D':BEGIN
            binwidth = data.op.endbins[1:*] - data.op.endbins
            attname=['long_name','units', 'Bin_endpoints', 'Bin_width', 'Bin_units']
            attvalue={a0:'Particle Count Per Size Bin', a1:'#', a2:data.op.endbins, a3:binwidth, a4:'micrometers'}
            dims=[xdimid, ydimid_size]
            tagname='COUNTS'
         END
         'INTSPEC_ALL':BEGIN
               binwidth = data.intendbins[1:*] - data.intendbins
               attname=['long_name', 'units', 'Bin_endpoints', 'Bin_width', 'Bin_units']
               attvalue={a0:'Particle Count Per Interarrival Bin, All Particles', a1:'#',$
                  a2:data.intendbins, a3:binwidth, a4:'seconds'}
               dims=[xdimid, ydimid_int]
         END
         'INTSPEC_ACCEPTED':BEGIN
               binwidth = data.intendbins[1:*] - data.intendbins
               attname=['long_name', 'units', 'Bin_endpoints', 'Bin_width', 'Bin_units']
               attvalue={a0:'Particle Count Per Interarrival Bin, Accepted Particles', a1:'#',$
                  a2:data.intendbins, a3:binwidth, a4:'seconds'}
               dims=[xdimid, ydimid_int]
         END
         'MIDBINS':BEGIN
            attvalue={a1:'Size Bin Mid-points', a2:'micrometers'}
            dims=ydimid_size
         END
         'ENDBINS':BEGIN
            attvalue={a1:'Size Bin End-points', a2:'micrometers'}
            dims=ydimid_sizeend
         END
         'INTMIDBINS':BEGIN
               attvalue={a1:'Interarrival Bin Mid-points', a2:'s'}
               dims=ydimid_int
         END
         'INTENDDBINS':BEGIN
               attvalue={a1:'Interarrival Bin End-points', a2:'s'}
               dims=ydimid_intend
         END
         ELSE:BEGIN
            skiptag=1
            ;Exception to write data without attributes, could be anything with same size as time
            IF (noskip eq 1) and (n_elements(currentdata) eq n_elements(data.time)) and (tags[j] ne 'TIME') THEN BEGIN
               attvalue={a1:'Unknown',a2:'Unknown'}
               skiptag=0
            ENDIF
         END
      ENDCASE

      IF (lite eq 1) and (n_elements(dims) gt 1) THEN skiptag=1
      IF not(skiptag) THEN BEGIN
         varid=ncdf_varid(id,tagname)  ;Check if this variable already exists
         IF varid eq -1 THEN varid=ncdf_vardef(id,tagname,dims,/float)
         FOR k=0,n_elements(attname)-1 DO BEGIN
            IF size(attvalue.(k), /TYPE) eq 2 THEN BEGIN
              ncdf_attput,id,varid,attname[k],attvalue.(k),/LONG
            ENDIF ELSE BEGIN
              ncdf_attput,id,varid,attname[k],attvalue.(k)
            ENDELSE
         ENDFOR

         ncdf_control,id,/endef                ;put in data mode
         ncdf_varput,id,varid,currentdata*unitadjust
         ncdf_control,id,/redef                ;return to define mode
      ENDIF
   ENDFOR

   ;--------------Sub-structure tags------------------------------
   IF total(opnames eq 'ENDBINS') && max(data.op.endbins) gt 100 THEN BEGIN  ;Make sure we have an OAP (not CDP, FSSP, etc.)
      bulkall=compute_bulk_simple(data.conc1d,data.op.endbins,binstart=0)
      i100=min(where(data.op.endbins ge 100))
      bulk100=compute_bulk_simple(data.conc1d,data.op.endbins,binstart=i100)
      i200=min(where(data.op.endbins ge 200))
      bulk200=compute_bulk_simple(data.conc1d,data.op.endbins,binstart=i200)
      IF total(tags eq 'SPEC2D_ASPR') THEN BEGIN
         armidbins=(data.op.arendbins+data.op.arendbins[1:*])/2.0
         meanar=compute_meanar(data.spec2d,armidbins)
         meanaspr=compute_meanar(data.spec2d_aspr,armidbins)
         area=compute_area(data)
         area100=compute_area(data, binstart=i100)
         area200=compute_area(data, binstart=i200)

         bulk=create_struct(bulkall, 'area', area.area, 'nt100', bulk100.nt, 'mnd100', bulk100.mnd, 'mvd100', bulk100.mvd, $
              'iwc100', bulk100.iwc, 'area100', area100.area, 'lwc100', bulk100.lwc, 'nt200', bulk200.nt, 'mnd200', bulk200.mnd, $
              'mvd200', bulk200.mvd, 'iwc200', bulk200.iwc, 'lwc200', bulk200.lwc, 'area200', area200.area, 'meanar', $
              meanar, 'meanaspr', meanaspr, 'asd', area.asd)
      ENDIF ELSE BEGIN
          bulk=create_struct(bulkall, 'nt100', bulk100.nt, 'mnd100', bulk100.mnd, 'mvd100', bulk100.mvd, $
               'iwc100', bulk100.iwc, 'lwc100', bulk100.lwc, 'nt200', bulk200.nt, 'mnd200', bulk200.mnd, $
               'mvd200', bulk200.mvd, 'iwc200', bulk200.iwc, 'lwc200', bulk200.lwc)
      ENDELSE
   ENDIF ELSE bulk=data  ;For CDP, FSSP, etc. where the bulk variables are already in the main structure

   tags=tag_names(bulk)
   FOR j=0,n_elements(tags)-1 DO BEGIN
      ;Write each variable
      skiptag=0
      dims=xdimid
      attvalue=['Unknown','Unknown']
      attname=['long_name','units']
      unitadjust=1.0
      currentdata=bulk.(j)
      tagname=tags[j]

      CASE tags[j] OF
         'LWC':BEGIN
             attvalue={a1:'Liquid Water Content',a2:'gram/m3'}
         END
         'LWC100':BEGIN
             attvalue={a1:'Liquid Water Content, Particles Larger than 100um in Diameter',a2:'gram/m3'}
         END
         'LWC200':BEGIN
             attvalue={a1:'Liquid Water Content, Particles Larger than 200um in Diameter',a2:'gram/m3'}
         END
         'IWC':BEGIN
             attname=['long_name','units','parameterization']
             attvalue={a1:'Ice Water Content',a2:'gram/m3',a3:'Brown and Francis 1995'}
         END
         'IWC100':BEGIN
             attname=['long_name','units','parameterization']
             attvalue={a1:'Ice Water Content, Particles Larger than 100um in Diameter',a2:'gram/m3',a3:'Brown and Francis 1995'}
         END
         'IWC200':BEGIN
             attname=['long_name','units','parameterization']
             attvalue={a1:'Ice Water Content, Particles Larger than 200um in Diameter',a2:'gram/m3',a3:'Brown and Francis 1995'}
         END
         'AREA':BEGIN
             attvalue={a1:'Projected Particle Area, All Particles',a2:'1/m'}
         END
         'AREA100':BEGIN
             attvalue={a1:'Projected Particle Area, Particles Larger than 100um in Diameter',a2:'1/m'}
         END
         'AREA200':BEGIN
             attvalue={a1:'Projected Particle Area, Particles Larger than 200um in Diameter',a2:'1/m'}
         END
         'MVD':BEGIN
            attvalue={a1:'Median Volume Diameter',a2:'micrometers'}
         END
         'MND':BEGIN
            attvalue={a1:'Mean Diameter',a2:'micrometers'}
         END
         'NT': BEGIN
            attvalue={a1:'Total Number Concentration, All Particles',a2:'#/m3'}
         END
         'MVD100':BEGIN
            attvalue={a1:'Median Volume Diameter, Particles Larger than 100um in Diameter',a2:'micrometers'}
         END
         'MND100':BEGIN
            attvalue={a1:'Mean Diameter, Particles Larger than 100um in Diameter',a2:'micrometers'}
         END
         'NT100': BEGIN
            attvalue={a1:'Total Number Concentration, Particles Larger than 100um in Diameter',a2:'#/m3'}
         END
         'MVD200':BEGIN
            attvalue={a1:'Median Volume Diameter, Particles Larger than 200um in Diameter',a2:'micrometers'}
         END
         'MND200':BEGIN
            attvalue={a1:'Mean Diameter, Particles Larger than 200um in Diameter',a2:'micrometers'}
         END
         'NT200': BEGIN
            attvalue={a1:'Total Number Concentration, Particles Larger than 200um in Diameter',a2:'#/m3'}
         END
         'MEANAR':BEGIN
            attname=['long_name','units','Bin_endpoints','Bin_units']
            attvalue={a0:'Mean Area Ratio Per Size Bin',a1:'unitless',$
                        a2:data.op.endbins,a3:'micrometers'}
            dims=[xdimid, ydimid_size]
            tagname='MEAN_AREARATIO'
         END
         'MEANASPR':BEGIN
            attname=['long_name','units','Bin_endpoints','Bin_units']
            attvalue={a0:'Mean Aspect Ratio Per Size Bin', a1:'unitless',$
                        a2:data.op.endbins, a3:'micrometers'}
            dims=[xdimid, ydimid_size]
            tagname='MEAN_ASPECTRATIO'
         END
         'ASD':BEGIN
            binwidth = data.intendbins[1:*] - data.intendbins
            attname=['long_name', 'units', 'Bin_endpoints', 'Bin_width', 'Bin_units']
            attvalue={a0:'Projected Area Per Size Bin, Not Normalized by Bin Width', a1:'1/m',$
                        a2:data.op.endbins, a3:binwidth, a4:'micrometers'}
            dims=[xdimid, ydimid_size]
            tagname='ASD'
         END
         'MSD':BEGIN
            binwidth = data.intendbins[1:*] - data.intendbins
            attname=['long_name', 'units', 'parameterization', 'Bin_endpoints', 'Bin_width', 'Bin_units']
            attvalue={a0:'Calculated Mass Per Size Bin, Not Normalized by Bin Width', a1:'g/m3', $
               a2:'Brown and Francis 1995', a3:data.op.endbins, a4:binwidth, a5:'micrometers'}
            dims=[xdimid, ydimid_size]
            tagname='MSD'
         END
      ELSE:skiptag=1
      ENDCASE

      IF (lite eq 1) and (n_elements(dims) gt 1) THEN skiptag=1
      IF not(skiptag) THEN BEGIN
         varid=ncdf_varid(id,tagname)  ;Check if this variable already exists
         IF varid eq -1 THEN varid=ncdf_vardef(id,tagname,dims,/float)
         FOR k=0,n_elements(attname)-1 DO BEGIN
            IF size(attvalue.(k), /TYPE) eq 2 THEN BEGIN
              ncdf_attput,id,varid,attname[k],attvalue.(k),/LONG
            ENDIF ELSE BEGIN
              ncdf_attput,id,varid,attname[k],attvalue.(k)
            ENDELSE
         ENDFOR

         ncdf_control,id,/endef                ;put in data mode
         ncdf_varput,id,varid,currentdata*unitadjust
         ncdf_control,id,/redef                ;return to define mode
      ENDIF
   ENDFOR

   IF n_elements(pthfile) ne 0 THEN BEGIN
      time=data.time
      restore,pthfile
      pth=data
      IF total(time) eq total(data.time) THEN BEGIN
         tags=tag_names(pth)
         FOR j=0,n_elements(tags)-1 DO BEGIN
            ;Write each variable
            skiptag=0
            dims=xdimid
            attvalue=['Unknown','Unknown']
            attname=['long_name','units']
            unitadjust=1.0
            currentdata=pth.(j)
            tagname=tags[j]

            CASE tags[j] OF
               'TAS':attvalue={a1:'True air speed',a2:'m/s'}
               'LAT':attvalue={a1:'Latitude',a2:'degrees'}
               'LON':attvalue={a1:'Longitude',a2:'degrees'}
               'PALT':attvalue={a1:'Pressure Altitude',a2:'m'}
               'P_ALT':attvalue={a1:'Pressure Altitude',a2:'m'}
               'GALT':attvalue={a1:'GPS/Geopotential Altitude',a2:'m'}
               'GPS_ALT':attvalue={a1:'GPS/Geopotential Altitude',a2:'m'}
               'W':attvalue={a1:'Vertical Wind',a2:'m/s'}
               'T': BEGIN
                  attvalue={a1:'Temperature',a2:'C'}
                  tagname='TEMP'  ;Make consistent
               END
               'TEMP':attvalue={a1:'Temperature',a2:'C'}
               'TD': BEGIN
                  attvalue={a1:'Dewpoint Temperature',a2:'C'}
                  tagname='DEWPOINT'  ;Make consistent
               END
               'DEW': BEGIN
                  attvalue={a1:'Dewpoint Temperature',a2:'C'}
                  tagname='DEWPOINT'  ;Make consistent
               END
               'TDEW': BEGIN
                  attvalue={a1:'Dewpoint Temperature',a2:'C'}
                  tagname='DEWPOINT'  ;Make consistent
               END
               'RH':attvalue={a1:'Relative Humidity',a2:'percent'}
               'PRES':attvalue={a1:'Pressure',a2:'mb'}
               ELSE:skiptag=1
            ENDCASE

            IF not(skiptag) THEN BEGIN
               varid=ncdf_varid(id,tagname)  ;Check if this variable already exists
               IF varid eq -1 THEN varid=ncdf_vardef(id,tagname,dims,/float)
               FOR k=0,n_elements(attname)-1 DO BEGIN
                  IF size(attvalue.(k), /TYPE) eq 2 THEN BEGIN
                  ncdf_attput,id,varid,attname[k],attvalue.(k),/LONG
                  ENDIF ELSE BEGIN
                  ncdf_attput,id,varid,attname[k],attvalue.(k)
                  ENDELSE
               ENDFOR

               ncdf_control,id,/endef                ;put in data mode
               ncdf_varput,id,varid,currentdata*unitadjust
               ncdf_control,id,/redef                ;return to define mode
            ENDIF
         ENDFOR

      ENDIF ELSE print,'PTH time mismatch, skipping.'
   ENDIF

   ;Close the file
   ncdf_close,id

END
