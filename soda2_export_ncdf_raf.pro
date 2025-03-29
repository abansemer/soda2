PRO soda2_export_ncdf_raf, data, outfile=outfile, normalize=normalize, suffix=suffix, legacybinning=legacybinning
   ;PRO to export a netCDF file with the variables contained in a data structure.
   ;Follow RAF guidelines/format
   ;Aaron Bansemer, NCAR, 9/2007
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(normalize) eq 0 THEN normalize = 1  ;Use bin width normalization
   IF n_elements(suffix) eq 0 THEN suffix = ''       ;Probe suffix including underscore ('_LWI')
   IF n_elements(legacybinning) eq 0 THEN legacybinning = 0  ;Use legacy-style bins, with zero-padded 0th bin

   !quiet = 1  ;Suppress annoying messages from ncdf routines

   ;Create the netCDF file
   id = ncdf_create(outfile[0], /clobber)

   ;Define the x-dimension, should be used in all variables
   xdimid = ncdf_dimdef(id,'Time', n_elements(data.time))
   ;Samples per second
   sdimid = ncdf_dimdef(id, 'sps1', (1.0/data.op.rate)>1)
   ;This is for numbins
   IF legacybinning THEN numbins = n_elements(data.op.endbins) ELSE numbins = n_elements(data.op.endbins)-1
   name = 'Vector'+strtrim(string(numbins), 2)
   ydimid_size = ncdf_dimdef(id, name, numbins)
   ;This is for interarrival
   name = 'Vector'+strtrim(string(n_elements(data.intmidbins)), 2)
   ydimid_int = ncdf_dimdef(id, name, n_elements(data.intmidbins))
   ;This is for area ratio
   name = 'Vector'+strtrim(string(n_elements(data.op.arendbins)-1), 2)
   ydimid_ar = ncdf_dimdef(id, name, n_elements(data.op.arendbins)-1)

   ;These are for ncplot compatibility
   opnames = tag_names(data.op)
   ms = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
   date = soda2_parsedate(data.op.date)
   days1970 = date.julday-julday(1, 1, 1970)
   flightdate = date.month+'/'+date.day+'/'+date.year

   tb = '0000'+strtrim(string(sfm2hms(min(data.time))), 2)
   te = '0000'+strtrim(string(sfm2hms(max(data.time))), 2)
   starttime = strmid(tb, 5, 2, /r)+':'+strmid(tb, 3, 2, /r)+':'+strmid(tb, 1, 2, /r)
   stoptime = strmid(te, 5, 2, /r)+':'+strmid(te, 3, 2, /r)+':'+strmid(te, 1, 2, /r)
   str = starttime+'-'+stoptime

   ;Create global attributes
   ncdf_attput, id, 'Source', 'SODA-2 OAP Processing Software', /global
   ncdf_attput, id, 'Conventions',  'NCAR-RAF/nimbus',  /global
   ncdf_attput, id, 'DateCreated', systime(), /global
   ncdf_attput, id, 'FlightDate', flightdate[0], /global
   ncdf_attput, id, 'DateProcessed', data.date_processed, /global
   ncdf_attput, id, 'TimeInterval', str, /global
   opnames = tag_names(data.op)
   FOR i = 0, n_elements(opnames)-1 DO BEGIN
      IF string(data.op.(i)[0]) eq '' THEN data.op.(i)[0] = 'none' ;To avoid an ncdf error (empty string)
      ncdf_attput, id, opnames[i], data.op.(i)[0], /global
   ENDFOR

   attname = ['long_name', 'units']
   attvalue = ['Time', 'seconds since '+date.year+'-'+date.month+'-'+date.day+' '+starttime+' +0000']
   timeid = ncdf_vardef(id, 'Time', xdimid, /long)
   FOR k = 0, n_elements(attname)-1 DO ncdf_attput, id, timeid, attname[k], attvalue[k]

   attvalue = ['Base time', 'seconds since 01/01/1970']
   baseid = ncdf_vardef(id, 'base_time', /long)
   FOR k = 0, n_elements(attname)-1 DO ncdf_attput, id, baseid, attname[k], attvalue[k]

   attvalue = ['UTC time', 'seconds from midnight of start date']
   utcid = ncdf_vardef(id, 'utc_time', xdimid, /long)
   FOR k = 0, n_elements(attname)-1 DO ncdf_attput, id, utcid, attname[k], attvalue[k]

   ncdf_control, id, /endef                ;put in data mode
   ncdf_varput, id, utcid, data.time
   ncdf_varput, id, baseid, data.time[0]+days1970*86400l
   ncdf_varput, id, timeid, data.time-data.time[0]
   ncdf_control, id, /redef                ;return to define mode


   ;-------Write data to file, start with main structure tags------------------------
   tags = tag_names(data)

   FOR j = 0, n_elements(tags)-1 DO BEGIN
      ;Write each variable
      skiptag = 0
      dims = xdimid
      attname = ['long_name', 'units']
      attvalue = ['Unknown', 'Unknown']
      unitadjust = 1.0
      currentdata = data.(j)
      tagname = tags[j]+suffix

      CASE tags[j] OF
         'SA':BEGIN
            attvalue = {a1:'Sample Area', a2:'m2'}
            dims = ydimid_size
            IF legacybinning eq 1 THEN currentdata = [0, data.sa] ELSE currentdata = data.sa
         END
         'COUNT_ACCEPTED':BEGIN
            attvalue = {a1:'Count Accepted', a2:'#'}
         END
         'TAS':BEGIN
            attvalue = {a1:'True Air speed used', a2:'m/s'}
         END
         'CLH':BEGIN
            attvalue = {a1:'CU Laser Hygrometer, Condensed Water Content', a2:'g/m3'}
         END
         'LAT':BEGIN
            attvalue = {a1:'Latitude', a2:'degrees'}
         END
         'LON':BEGIN
            attvalue = {a1:'Longitude', a2:'degrees'}
         END
         'GALT':BEGIN
            attvalue = {a1:'GPS Altitude', a2:'meters'}
         END
         'T':BEGIN
            attvalue = {a1:'Temperature', a2:'degrees Celsius'}
         END
         'PRES':BEGIN
            attvalue = {a1:'Pressure', a2:'hPa'}
         END
         'ACTIVETIME':BEGIN
            attvalue = {a1:'Probe activity time', a2:'s'}
         END
         'CDP_NT':BEGIN
            attvalue = {a1:'Total number concentration from the CDP', a2:'#/cm3'}
         END
         'CDP_LWC':BEGIN
            attvalue = {a1:'Total LWC derived from the CDP', a2:'g/m3'}
         END
         'CDP_MEAND':BEGIN
            attvalue = {a1:'Mean diameter from the CDP', a2:'microns'}
         END
         'CONCU':BEGIN
            attvalue = {a1:'UHSAS Total Concentration', a2:'#/cm3'}
         END
         'CONCU100':BEGIN
            attvalue = {a1:'UHSAS Concentration, 0.1 micron and bigger', a2:'#/cm3'}
         END
         'CONCU500':BEGIN
            attvalue = {a1:'UHSAS Concentration, 0.5 microns and bigger', a2:'#/cm3'}
         END
         'W':BEGIN
            attvalue = {a1:'GPS-Corrected Wind Vector, Vertical Gust Component', a2:'m/s'}
         END
         'V':BEGIN
            attvalue = {a1:'GPS-Corrected Wind Vector, North Component', a2:'m/s'}
         END
         'U':BEGIN
            attvalue = {a1:'GPS-Corrected Wind Vector, East Component', a2:'m/s'}
         END
         'WSC':BEGIN
            attvalue = {a1:'GPS-Corrected Horizontal Wind Speed', a2:'m/s'}
         END
         'CONC1D': BEGIN
            ;Use CellSizeNote attribute 'upper' for old-style with zero padding, 'lower' for new-style
            IF legacybinning eq 1 THEN BEGIN
               cellsizenote = 'CellSizes are upper bin limits as particle size'
               firstbin = 1
               lastbin = fix(n_elements(data.midbins))
            ENDIF ELSE BEGIN
               cellsizenote = 'CellSizes are lower bin limits as particle size'
               firstbin = 0
               lastbin = fix(n_elements(data.midbins)-1)
            ENDELSE

            attname = ['_FillValue', 'long_name', 'units', 'FirstBin', 'LastBin', 'CellSizes', 'CellSizeUnits', 'Category', 'CellSizeNote']
            IF normalize eq 1 THEN BEGIN
               ;SODA convention, in #/m4
               attvalue = {a0:-32767., a1:'Particle Concentration Per Channel, Normalized by Bin Width', a2:'#/m4',$
                  a3:firstbin, a4:lastbin, a5:data.op.endbins, a6:'micrometers', a7:'PMS Probe', $
                  a8:cellsizenote}
               conc1d = data.conc1d
            ENDIF

            IF normalize eq 0 THEN BEGIN
               ;RAF convention, in #/L
               attvalue = {a0:-32767., a1:'Particle Concentration Per Channel', a2:'#/L',$
                  a3:firstbin, a4:lastbin, a5:data.op.endbins, a6:'micrometers', a7:'PMS Probe', $
                  a8:cellsizenote}
               conc1d = unnormalize(data.conc1d, data.op.endbins) / 1e3  ;#/L
            ENDIF
            dims = [ydimid_size, sdimid, xdimid]

            ;Pad first bin with zeros transpose, and reform to 3-dimensions
            conc = fltarr(n_elements(data.time), numbins)
            IF legacybinning THEN conc[*,1:*] = conc1d ELSE conc[*,*] = conc1d
            currentdata = reform(transpose(conc), numbins, 1, n_elements(data.time))
            tagname = 'C' + data.op.probetype+suffix
         END
         'SPEC1D':BEGIN
            attname = ['_FillValue', 'long_name', 'units', 'Category']
            attvalue = {a0:-32767., a1:'Particle Count Per Size Bin', a2:'count', a3:'PMS Probe'}
            dims = [ydimid_size, sdimid, xdimid]

            ;Pad first bin with zeros, transpose, and reform to 3-dimensions
            spec = fltarr(n_elements(data.time), numbins)
            IF legacybinning THEN spec[*,1:*] = data.spec1d ELSE spec[*,*] = data.spec1d
            currentdata = reform(transpose(spec), numbins, 1, n_elements(data.time))
            tagname = 'A' + data.op.probetype+suffix
         END
         'INTSPEC_ALL':BEGIN
               attvalue = {a1:'Particle Count Per Interarrival Bin, All Particles', a2:'#'}
               dims = [ydimid_int, xdimid]
               currentdata = transpose(currentdata)
         END
         'INTSPEC_ACCEPTED':BEGIN
               attvalue = {a1:'Particle Count Per Interarrival Bin, Accepted Particles', a2:'#'}
               dims = [ydimid_int, xdimid]
               currentdata = transpose(currentdata)
         END
         'MIDBINS':BEGIN
            attvalue = {a1:'Size Bin Mid-points', a2:'um'}
            dims = ydimid_size
            IF legacybinning THEN currentdata = [0, data.midbins] ELSE currentdata = data.midbins
         END
         'INTMIDBINS':BEGIN
               attvalue = {a1:'Interarrival Bin Mid-points', a2:'s'}
               dims = ydimid_int
         END
         ELSE:skiptag = 1
      ENDCASE

      IF not(skiptag) THEN BEGIN
         varid = ncdf_varid(id, tagname)  ;Check if this variable already exists
         IF varid eq -1 THEN varid = ncdf_vardef(id, tagname, dims, /float)
         FOR k = 0, n_elements(attname)-1 DO BEGIN
            IF size(attvalue.(k), /TYPE) eq 2 THEN BEGIN
              ncdf_attput, id, varid, attname[k], attvalue.(k), /LONG
            ENDIF ELSE BEGIN
              ncdf_attput, id, varid, attname[k], attvalue.(k)
            ENDELSE
         ENDFOR

         ncdf_control, id, /endef                ;put in data mode
         ncdf_varput, id, varid, currentdata*unitadjust
         ncdf_control, id, /redef                ;return to define mode
      ENDIF
   ENDFOR

   ;--------------Sub-structure tags------------------------------
   bulkall = compute_bulk_simple(data.conc1d, data.op.endbins, binstart=0)
   i100 = min(where(data.op.endbins ge 100))
   bulk100 = compute_bulk_simple(data.conc1d, data.op.endbins, binstart=i100)
   area = compute_area(data)
   area100 = compute_area(data, binstart=i100)
   bulk = create_struct(bulkall, 'nt100', bulk100.nt, 'mnd100', bulk100.mnd, 'mvd100', bulk100.mvd, 'iwc100', bulk100.iwc, 'area', area.area, 'area100', area100.area)
   tags = tag_names(bulk)
   FOR j = 0, n_elements(tags)-1 DO BEGIN
      ;Write each variable
      skiptag = 0
      dims = xdimid
      attvalue = ['Unknown', 'Unknown']
      unitadjust = 1.0
      currentdata = bulk.(j)
      tagname = tags[j]+suffix

      CASE tags[j] OF
         'LWC':BEGIN
             attvalue = {a1:'Liquid Water Content', a2:'gram/m3'}
         END
         'IWC':BEGIN
             attvalue = {a1:'Ice Water Content, All Particles', a2:'gram/m3'}
         END
         'IWC100':BEGIN
             attvalue = {a1:'Ice Water Content, Particles Larger than 100um in Diameter', a2:'gram/m3'}
         END
         'AREA':BEGIN
             attvalue = {a1:'Projected Particle Area, All Particles', a2:'1/m'}
         END
         'AREA100':BEGIN
             attvalue = {a1:'Projected Particle Area, Particles Larger than 100um in Diameter', a2:'1/m'}
         END
         'MVD':BEGIN
            attvalue = {a1:'Median Volume Diameter', a2:'um'}
         END
         'MND':BEGIN
            attvalue = {a1:'Mean Diameter', a2:'um'}
         END
         'NT': BEGIN
            attvalue = {a1:'Total Number Concentration, All Particles', a2:'#/m3'}
         END
         'MVD100':BEGIN
            attvalue = {a1:'Median Volume Diameter, Particles Larger than 100um in Diameter', a2:'um'}
         END
         'MND100':BEGIN
            attvalue = {a1:'Mean Diameter, Particles Larger than 100um in Diameter', a2:'um'}
         END
         'NT100': BEGIN
            attvalue = {a1:'Total Number Concentration, Particles Larger than 100um in Diameter', a2:'#/m3'}
         END
      ELSE:skiptag = 1
      ENDCASE

      IF not(skiptag) THEN BEGIN
         varid = ncdf_varid(id, tagname)  ;Check if this variable already exists
         IF varid eq -1 THEN varid = ncdf_vardef(id, tagname, dims, /float)
         FOR k = 0, n_elements(attname)-1 DO BEGIN
            IF size(attvalue.(k), /TYPE) eq 2 THEN BEGIN
              ncdf_attput, id, varid, attname[k], attvalue.(k), /LONG
            ENDIF ELSE BEGIN
              ncdf_attput, id, varid, attname[k], attvalue.(k)
            ENDELSE
         ENDFOR

         ncdf_control, id, /endef                ;put in data mode
         ncdf_varput, id, varid, currentdata*unitadjust
         ncdf_control, id, /redef                ;return to define mode
      ENDIF
   ENDFOR

   ;Close the file
   ncdf_close, id
END
