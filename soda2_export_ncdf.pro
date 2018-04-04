PRO soda2_export_ncdf, data, outfile=outfile, pthfile=pthfile, lite=lite
   ;PRO to export a netCDF file with the variables contained in a data structure.
   ;Similar to soda2_export_ncdf_raf, but follows a simpler format, 
   ;   without the 'SPS' dimension, PSD padding, reversed dimensions, etc.
   ;Aaron Bansemer, NCAR, 8/2014
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.


   IF n_elements(lite) eq 0 THEN lite=0  ;Option to avoid writing 2D matrices
   !quiet=1  ;Suppress annoying messages from ncdf routines
   
   ;-----------Create new file instead-----------------
   
   suffix=''   ;Suffix for netcdf tags, not used here
   ;Create the file
   id=ncdf_create(outfile[0],/clobber)
   
   
   ;Define the x-dimension, should be used in all variables
   xdimid=ncdf_dimdef(id,'Time',n_elements(data.time))
   ;This is for numbins
   name='Vector'+strtrim(string(n_elements(data.op.endbins)-1),2)
   ydimid_size=ncdf_dimdef(id,name,n_elements(data.op.endbins)-1)
   ;This is for endbins
   name='Vector'+strtrim(string(n_elements(data.op.endbins)),2)
   dimid_endbins=ncdf_dimdef(id,name,n_elements(data.op.endbins))
   ;This is for interarrival
   name='Vector'+strtrim(string(n_elements(data.intmidbins)),2)
   ydimid_int=ncdf_dimdef(id,name,n_elements(data.intmidbins))
   ;This is for area ratio
   name='Vector'+strtrim(string(n_elements(data.op.arendbins)-1),2)
   ydimid_ar=ncdf_dimdef(id,name,n_elements(data.op.arendbins)-1)
         
   ;These are for ncplot compatibility
   opnames=tag_names(data.op)
   ms=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
   ;month=string(where(ms eq strmid(data.op.date,0,2))+1,format='(i02)')
   month=strmid(data.op.date,0,2)
   day=strmid(data.op.date,2,2)
   year=strmid(data.op.date,4,4)
   days1970=julday(month,day,year)-julday(1,1,1970) 
   flightdate=month+'/'+day+'/'+year 
   
   tb='0000'+strtrim(string(sfm2hms(min(data.time))),2)
   te='0000'+strtrim(string(sfm2hms(max(data.time))),2)
   starttime=strmid(tb,5,2,/r)+':'+strmid(tb,3,2,/r)+':'+strmid(tb,1,2,/r)
   stoptime=strmid(te,5,2,/r)+':'+strmid(te,3,2,/r)+':'+strmid(te,1,2,/r)
   str=starttime+'-'+stoptime
   
   ;Create global attributes
   ncdf_attput,id,'Source','SODA-2 OAP Processing Software',/global
   ;ncdf_attput,id,'Conventions', 'NCAR-RAF/nimbus', /global
   ncdf_attput,id,'DateCreated',systime(),/global
   ncdf_attput,id,'FlightDate',flightdate[0],/global
   ncdf_attput,id,'DateProcessed',data.date_processed,/global
   ncdf_attput,id,'TimeInterval',str,/global
   opnames=tag_names(data.op)                  
   FOR i=0,n_elements(opnames)-1 DO BEGIN
         IF string(data.op.(i)[0]) eq '' THEN data.op.(i)[0]='none' ;To avoid an ncdf error (empty string)
         ncdf_attput,id,opnames[i],data.op.(i)[0],/global      
   ENDFOR
   
   
   
   attname=['long_name','units']
   attvalue=['Elapsed time','seconds since '+year+'-'+month+'-'+day+' '+starttime+' +0000']
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
   tags=tag_names(data)     
   
   FOR j=0,n_elements(tags)-1 DO BEGIN
      ;Write each variable
      skiptag=0
      dims=xdimid
      attname=['long_name','units']
      attvalue=['Unknown','Unknown']
      unitadjust=1.0
      currentdata=data.(j)
      tagname=tags[j]+suffix
         
      CASE tags[j] OF
         'SA':BEGIN
            attvalue={a1:'Sample Area',a2:'m2'}
            dims=ydimid_size
         END
         'COUNT_ACCEPTED':BEGIN
            attvalue={a1:'Count Accepted',a2:'#'}
         END
         'TAS':BEGIN
            attvalue={a1:'True Air speed used',a2:'m/s'}
         END
         'ACTIVETIME':BEGIN
            attvalue={a1:'Probe activity time',a2:'s'}
         END
         'CONC1D': BEGIN
            attname=['long_name','units','Bin_endpoints','Bin_units']
            attvalue={a0:'Particle Concentration Per Bin, Normalized by Bin Width',a1:'#/m4',$
			a2:data.op.endbins,a3:'micrometers'}
            dims=[xdimid, ydimid_size]
            ;Pad first bin with zeros transpose, and reform to 3-dimensions
            ;conc=fltarr(n_elements(data.time),n_elements(data.op.endbins))
            ;conc[*,1:*]=data.conc1d
            ;currentdata=reform(transpose(conc),n_elements(data.op.endbins),1,n_elements(data.time))
            tagname='CONCENTRATION'
         END
         'SPEC1D':BEGIN
            attname=['long_name','units','Bin_endpoints','Bin_units']
            attvalue={a0:'Particle Count Per Bin',a1:'#',$
                        a2:data.op.endbins,a3:'micrometers'}
            dims=[xdimid, ydimid_size]
            ;Pad first bin with zeros, transpose, and reform to 3-dimensions
            ;spec=fltarr(n_elements(data.time),n_elements(data.op.endbins))
            ;spec[*,1:*]=data.spec1d
            ;currentdata=reform(transpose(spec),n_elements(data.op.endbins),1,n_elements(data.time))
            tagname='COUNTS'
         END
         'INTSPEC_ALL':BEGIN
               attvalue={a1:'Particle Count Per Interarrival Bin, All Particles',a2:'#'}
               dims=[ydimid_int,xdimid]
               ;currentdata=transpose(currentdata)
         END
         'INTSPEC_ACCEPTED':BEGIN
               attvalue={a1:'Particle Count Per Interarrival Bin, Accepted Particles',a2:'#'}
               dims=[ydimid_int,xdimid]
               ;currentdata=transpose(currentdata)
         END
         'MIDBINS':BEGIN
            attvalue={a1:'Size Bin Mid-points',a2:'um'}
            dims=ydimid_size
         END
         'INTMIDBINS':BEGIN
               attvalue={a1:'Interarrival Bin Mid-points',a2:'s'}
               dims=ydimid_int
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
   
   ;--------------Sub-structure tags------------------------------
   bulkall=compute_bulk_simple(data.conc1d,data.op.endbins,binstart=0)
   i100=min(where(data.op.endbins ge 100))
   bulk100=compute_bulk_simple(data.conc1d,data.op.endbins,binstart=i100)
   armidbins=(data.op.arendbins+data.op.arendbins[1:*])/2.0
   meanar=compute_meanar(data.spec2d,armidbins)
   meanaspr=compute_meanar(data.spec2d_aspr,armidbins)
   area=compute_area(data)
   area100=compute_area(data, binstart=i100)
   bulk=create_struct(bulkall, 'nt100', bulk100.nt, 'mnd100', bulk100.mnd, 'mvd100', bulk100.mvd, $
        'iwc100', bulk100.iwc, 'area', area, 'area100', area100, 'lwc100', bulk100.lwc, 'meanar', $
        meanar, 'meanaspr', meanaspr)
   tags=tag_names(bulk)     
   FOR j=0,n_elements(tags)-1 DO BEGIN
      ;Write each variable
      skiptag=0
      dims=xdimid
      attvalue=['Unknown','Unknown']
      attname=['long_name','units']
      unitadjust=1.0
      currentdata=bulk.(j)
      tagname=tags[j]+suffix
         
      CASE tags[j] OF
         'LWC':BEGIN
             attvalue={a1:'Liquid Water Content',a2:'gram/m3'}
         END
         'LWC100':BEGIN
             attvalue={a1:'Liquid Water Content, Particles Larger than 100um in Diameter',a2:'gram/m3'}
         END
         'IWC':BEGIN
             attname=['long_name','units','parameterization']
             attvalue={a1:'Ice Water Content',a2:'gram/m3',a3:'Brown and Francis 1995'}
         END
         'IWC100':BEGIN
             attname=['long_name','units','parameterization']
             attvalue={a1:'Ice Water Content, Particles Larger than 100um in Diameter',a2:'gram/m3',a3:'Brown and Francis 1995'}
         END
         'AREA':BEGIN
             attvalue={a1:'Projected Particle Area, All Particles',a2:'1/m'}
         END
         'AREA100':BEGIN
             attvalue={a1:'Projected Particle Area, Particles Larger than 100um in Diameter',a2:'1/m'}
         END
         'MVD':BEGIN
            attvalue={a1:'Median Volume Diameter',a2:'um'}
         END
         'MND':BEGIN
            attvalue={a1:'Mean Diameter',a2:'um'}
         END
         'NT': BEGIN
            attvalue={a1:'Total Number Concentration, All Particles',a2:'#/m3'}
         END
         'MVD100':BEGIN
            attvalue={a1:'Median Volume Diameter, Particles Larger than 100um in Diameter',a2:'um'}
         END
         'MND100':BEGIN
            attvalue={a1:'Mean Diameter, Particles Larger than 100um in Diameter',a2:'um'}
         END
         'NT100': BEGIN
            attvalue={a1:'Total Number Concentration, Particles Larger than 100um in Diameter',a2:'#/m3'}
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
            attvalue={a0:'Mean Aspect Ratio Per Size Bin',a1:'unitless',$
                        a2:data.op.endbins,a3:'micrometers'}
            dims=[xdimid, ydimid_size]
            tagname='MEAN_ASPECTRATIO'
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
            tagname=tags[j]+suffix
               
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
