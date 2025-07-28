PRO soda2_export_ascii, data, outfile=outfile, a=a, b=b, minsize=minsize, counts=counts
   ;PRO to export an ASCII file with the variables contained in a data structure.
   ;Aaron Bansemer, NCAR, 8/2014
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(counts) eq 0 THEN counts = 0  ;Write counts instead of concentration

   IF n_elements(minsize) eq 0 THEN BEGIN
      minsize = 100
      IF data.op.res le 15 THEN minsize = 50  ;Go down to 50um for 2D-S, 1D2D, CIP-15
   ENDIF

   ;Compute bulk
   iminsize = min(where(data.op.endbins ge minsize))
   minsizestring = string(data.op.endbins[iminsize], format='(f0.2)')  ;The minimum size actually used is on the bin edge
   bulk100 = compute_bulk_simple(data.conc1d, data.op.endbins, binstart=iminsize, acoeff=a, bcoeff=b)

   ;Check for housekeeping data to add to the file
   IF (total(tag_names(data) eq 'HOUSE') eq 1) && (n_elements(tag_names(data.house)) gt 1) THEN BEGIN
      ;1D2D Modes
      IF (data.op.probetype eq '1D2D') THEN BEGIN
         housetitles = ['1D2D Probe Mode (1: No Depth of Field Rejections, 2: N75 Count Rejection, 3: N75/N50 Ratio Rejection)', $
            '1D2D Edge-touch Rejection (0:Disabled, 1:Enabled)']
         houseshortnames = ['ProbeMode', 'EdgeReject']
         houseformats = ['i12', 'i12']  ;Print format for each house variable, use width of 12

         ;Initialize probemode as Mode1
         probemode = bytarr(n_elements(data.time))+1
         ;Mode2
         w2 = where((data.house.dofnumreject ne 0) and (data.house.dofpercent eq 0), n2)
         IF n2 ne 0 THEN probemode[w2] = 2
         ;Mode3, the actual N75/N50 ratio required is linked to the number in this variable. 4 = 50%.
         w3 = where(data.house.dofpercent ne 0, n3)
         IF n3 ne 0 THEN probemode[w3] = 3

         ;Compute edge rejection flag.
         ;Left and right edge rejection are usually turned on together, so just ignore that and make a boolean flag.
         edgereject = byte(data.house.leftreject+data.house.rightreject)<1

         ;Put housekeeping data in a single array
         housedata = [[probemode], [edgereject]]
      ENDIF
   ENDIF

   ;No housekeeping data, make null arrays instead
   IF n_elements(housedata) eq 0 THEN BEGIN
      housetitles = []
      houseshortnames = []
      housedata = []
      houseformats = []
   ENDIF

   ;Open output file
   IF counts eq 1 THEN BEGIN
      shortname = 'Count'
      longname = 'counts'
      description = 'CountXXX: Particle counts per size bin [#]'
   ENDIF ELSE BEGIN
      shortname = 'Conc'
      longname = 'concentration'
      description = 'ConcXXX: Particle concentration per size bin, normalized by bin width [#/m4]'
   ENDELSE
   IF n_elements(outfile) eq 0 THEN outfile = soda2_filename(data.op, data.op.shortname + '_' + strupcase(longname), ext='.txt', outdir='')
   openw, lun, outfile, /get_lun

   ;Write header
   tags = tag_names(data.op)
   IF counts eq 1 THEN printf, lun, 'Particle counts for the ', data.op.probetype, ' probe in ', data.op.project ELSE $
      printf, lun, 'Particle size distributions for the ', data.op.probetype, ' probe in ', data.op.project
   date = soda2_parsedate(data.op.date)
   IF date.order eq 'mdy' THEN printf, lun, 'Flight date (mm/dd/yyyy):   '+date.month+'/'+date.day+'/'+date.year
   IF date.order eq 'ymd' THEN printf, lun, 'Flight date (yyyy/mm/dd):   '+date.year+'/'+date.month+'/'+date.day
   printf, lun, 'Processed:   '+data.date_processed
   printf, lun, 'Raw data source:  '+data.op.fn
   printf, lun, 'Sizing method:  '+data.op.smethod
   IF total(tags eq 'EAWMETHOD') eq 1 THEN printf, lun, 'Partial particle method:  '+data.op.eawmethod
   IF total(tags eq 'RECONSTRUCT') eq 1 THEN printf, lun, 'Partial particle reconstruction:  '+(['off', 'on'])[data.op.reconstruct] ;SODA-1
   IF total(tags eq 'INTTIME_REJECT') eq 1 THEN printf, lun, 'Shattering correction:  '+(['off', 'on'])[data.op.inttime_reject]
   IF total(tags eq 'TIMEREJECT') eq 1 THEN printf, lun, 'Shattering correction:  ' + string(data.op.timereject)  ;SODA-1
   printf, lun, 'Liquid water processing:  '+(['off', 'on'])[data.op.water]
   printf, lun, ''
   printf, lun, 'Bin midpoints (microns):'
   printf, lun, data.midbins, format='(500f9.2)'
   printf, lun, 'Bin endpoints (microns):'
   printf, lun, data.op.endbins, format='(500f9.2)'
   IF total(tags eq 'DOFCONST') eq 1 THEN dofconst = data.op.dofconst ELSE dofconst = 0
   doftitle = 'Sample area using a depth of field constant of ' + string(dofconst, form='(f0.2)') + ' (m^2):'
   IF (total(tags eq 'CUSTOMDOF') eq 1) && (max(data.op.customdof) gt 0) THEN doftitle = 'Sample area (m^2) using a custom depth of field constant:'
   printf, lun, doftitle
   printf, lun, data.sa, format='(500e9.2)'
   printf, lun, ''
   printf, lun, 'Notes:'
   printf, lun, '   All bulk properties are computed using particles larger than '+minsizestring+' microns in size.'
   IF data.op.water eq 1 THEN BEGIN
      printf, lun, '   This file contains "round" particles only, with area ratio > 0.5 and diameter < 6mm.'
      printf, lun, '   The intent is to process liquid drops only, there may be substantial errors in LWC when ice particles are also present.'
      masstitle = 'LWC: Estimated Liquid Water Content [g/m3]'
      massshortname = 'LWC'
      mass = bulk100.lwc
      diamtitle = 'MVD: Median Volume Diameter [microns]'
      diamshortname = 'MVD'
      diam = bulk100.mvd
   ENDIF ELSE BEGIN
      printf, lun, '   Size distributions were processed using ice rejection criteria.'
      printf, lun, '   Mass-size parameterization coefficients: a='+string(a, format='(e10.2)')+', b='+string(b, format='(f4.2)')
      masstitle = 'IWC: Estimated Ice Water Content [g/m3]'
      massshortname = 'IWC'
      mass = bulk100.iwc
      diamtitle = 'MMD: Median Mass-weighted Diameter [microns]'
      diamshortname = 'MMD'
      diam = bulk100.dmedianmass
   ENDELSE
   printf, lun, ''
   titles = ['Time: Time at start of interval [UTC seconds]', $
            'Nt: Total Concentration for Particles with D>'+minsizestring+'um [#/m3]', $
            masstitle, $
            diamtitle, $
            housetitles, $
            description]
   shortnames = ['Time', 'Nt', massshortname, diamshortname, houseshortnames, $
      replicate(shortname, n_elements(data.midbins))+string(findgen(n_elements(data.midbins))+1, format='(i03)')]

   ;Header variable list
   printf, lun, 'Variables:'
   FOR i = 0, n_elements(titles)-1 DO printf, lun, '   '+titles[i]
   printf, lun, ''
   printf, lun, shortnames, format='(a6, 500a12)'
   printf, lun, '----------------------------------------------------'

   ;Check for Nan and Inf, set to zero
   bad = where(finite(bulk100.nt) eq 0, nbad)
   IF nbad gt 0 THEN bulk100.nt[bad] = 0
   bad = where(finite(mass) eq 0, nbad)
   IF nbad gt 0 THEN mass[bad] = 0
   bad = where(finite(diam) eq 0, nbad)
   IF nbad gt 0 THEN diam[bad] = 0
   bad = where(finite(data.conc1d) eq 0, nbad)
   IF nbad gt 0 THEN data.conc1d[bad] = 0

   ;Write data
   IF counts eq 1 THEN BEGIN
      data2write = data.spec1d
      format = '(500i12)'
   ENDIF ELSE BEGIN
      data2write = data.conc1d
      format = '(500e12.2)'
   ENDELSE

   FOR i = 0, n_elements(data.time)-1 DO BEGIN
      printf, lun, data.time[i], bulk100.nt[i], mass[i], diam[i], form='(i6, 3e12.2, $)'
      FOR j = 0, n_elements(houseformats)-1 DO printf, lun, housedata[i,j], form='('+houseformats[j]+', $)'
      printf, lun, transpose(data2write[i,*]), format=format
   ENDFOR

   ;Close the file
   free_lun, lun
END
