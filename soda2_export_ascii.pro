PRO soda2_export_ascii, data, outfile=outfile, a=a, b=b
   ;PRO to export an ASCII file with the variables contained in a data structure.
   ;Aaron Bansemer, NCAR, 8/2014
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   openw,lun,outfile,/get_lun   
   i100=min(where(data.op.endbins ge 100))
   bulk100=compute_bulk_simple(data.conc1d,data.op.endbins,binstart=i100,acoeff=a,bcoeff=b)

   ;Write header
   printf,lun,'Particle size distributions for the ',data.op.probetype,' probe in ',data.op.project
   printf,lun,'Flight date (mm/dd/yy):   '+strmid(data.op.date,0,2)+'/'+strmid(data.op.date,2,2)+'/'+strmid(data.op.date,6,2)
   printf,lun,'Processed:   '+   data.date_processed
   printf,lun,'Raw data source:  '+data.op.fn
   printf,lun,''
   printf,lun,'Bin midpoints (microns):'
   printf,lun,data.midbins,format='(35f9.2)'
   printf,lun,'Bin endpoints (microns):'
   printf,lun,data.op.endbins,format='(36f9.2)'
   printf,lun,''
   printf,lun,'Time is UTC in seconds from midnight.'
   printf,lun,'Nt is total concentration of particles with diameter greater than 100 microns.'
   IF data.op.water eq 1 THEN BEGIN
      printf,lun,'This file contains "round" particles only, with area ratio > 0.5 and diameter < 6mm.'
      printf,lun,'The intent is to process liquid drops only, there may be substantial errors in LWC when ice particles are also present.' 
      masstitle='Estimated Liquid Water Content [g/m3]'
      massshortname='LWC'
      mass=bulk100.lwc
      diamtitle='Median Volume Diameter [microns]'
      diamshortname='MVD'
      diam=bulk100.mvd
   ENDIF ELSE BEGIN
      printf,lun,'Size distributions were processed using ice rejection criteria.'
      printf,lun,'Mass-size parameterization coefficients: a='+string(a,format='(e8.2)')+', b='+string(b,format='(f4.2)')
      masstitle='Estimated Ice Water Content [g/m3]'
      massshortname='IWC'
      mass=bulk100.iwc
      diamtitle='Median Mass-weighted Diameter [microns]'
      diamshortname='MMD'
      diam=bulk100.dmedianmass
   ENDELSE
   printf,lun,''
   titles=['Time at start of interval [UTC seconds]',$
           'Total Concentration for Particles with D>100um [#/m3]',$
           masstitle,$
           diamtitle,$
           'Concentration per size bin, normalized by bin width [#/m4]']
   shortnames=['Time','Nt',massshortname,diamshortname,replicate('Conc',n_elements(data.midbins))+string(findgen(n_elements(data.midbins))+1,format='(i03)')]

   printf,lun,'Variables:'
   FOR i=0,n_elements(titles)-1 DO printf,lun,'   '+titles[i]
   printf,lun,''
   printf,lun,shortnames,format='(a6, 200a10)'
   printf,lun,'----------------------------------------------------'

   FOR i=0,n_elements(data.time)-1 DO BEGIN
      printf,lun,data.time[i],bulk100.nt[i],mass[i],diam[i],transpose(data.conc1d[i,*]),form='(i6, 2e10.2,200e10.2)' 
   ENDFOR
   
   ;Close the file
   free_lun,lun
   
END
