FUNCTION meandiam, conc_raw, midbins, nan=nan, minimum=minimum
   ;FUNCTION to compute the mean number diameter from a size distribution (or time x size array)
   ;conc should be un-normalized (m^-3).  Use unnormalize.pro if needed.
   ;nan keyword uses NaN for null values rather than 0.
   ;minimum is the minimum concentration (#/m3) required to return a value.  Otherwise will be null.
   ;Aaron Bansemer, Feb 2007
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   IF n_elements(nan) eq 0 THEN nan=0
   IF nan eq 1 THEN nullvalue=!values.f_nan ELSE nullvalue=0
   IF n_elements(minimum) eq 0 THEN minimum=0
      
   n=n_elements(midbins)
   z=fltarr(n)
   s=size(conc_raw)  
   meand=fltarr(s[1])
   FOR i=0L,s[1]-1 DO BEGIN
      meand[i]=total(conc_raw[i,*]*midbins)/(total(conc_raw[i,*]) > 0.0001)  ;0.0001 just to avoid divide by zero errors
      IF total(conc_raw[i,*]) eq 0 THEN meand[i]=nullvalue
   ENDFOR

   IF minimum ne 0 THEN BEGIN
      w=where(total(conc_raw,2) lt minimum)
      IF w[0] ne -1 THEN meand[w]=nullvalue
   ENDIF

   return,meand
END
   
