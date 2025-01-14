FUNCTION compute_area, data, binstart=binstart, asd=asd
   ;FUNCTION to compute total area
   ;Input is a SODA dat file, containing spec2d
   ;AB 2/2015
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(binstart) eq 0 THEN binstart = 0

   s = size(data.spec2d)

   area = fltarr(s[1])

   endbins = float(data.op.endbins)
   numbins = n_elements(endbins)-1
   midbins = (endbins[0:numbins-1]+endbins[1:numbins])/2.0
   binwidth = float(endbins[1:numbins]-endbins[0:numbins-1])
   armidbins = (data.op.arendbins + data.op.arendbins[1:*])/2.0

   ;Build area array
   agrid = fltarr(s[2], s[3])
   sagrid = fltarr(s[2], s[3])  ;Sample area array
   FOR i = 0,s[2]-1 DO BEGIN
      agrid[i,*] = !pi/4 * (midbins[i]/1.0e6)^2 * armidbins   ;meters^2
      sagrid[i,*] = data.sa[i]
   ENDFOR

   ;Compute ASD and area
   asd = fltarr(s[1], s[2])     ;Area size distribution
   FOR i = 0, s[1]-1 DO BEGIN   ;Time
      svgrid = sagrid * data.tas[i] * data.activetime[i]
      IF data.tas[i] gt 0 THEN BEGIN
         ;area[i] = total(agrid[binstart:*,*] * data.spec2d[i,binstart:*,*]/svgrid[binstart:*,*], /nan)
         fullgrid = agrid[binstart:*,*] * data.spec2d[i,binstart:*,*]/svgrid[binstart:*,*]
         asd[i,binstart:*] = total(fullgrid, 2, /nan)
         area[i] = total(fullgrid, /nan)
      ENDIF
   ENDFOR

   return, {area:area, asd:asd, unit:'1/m'}
END
