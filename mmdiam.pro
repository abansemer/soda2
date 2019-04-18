FUNCTION mmdiam, iwc_spec, endbins, nan=nan
   ;FUNCTION to compute the median mass diameter from a mass distribution (or time x size array)
   ;iwc_spec should be un-normalized (g/m3).  Use unnormalize.pro if needed.
   ;nan keyword uses NaN for null values rather than 0.
   ;Aaron Bansemer, Oct 2018
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   IF n_elements(nan) eq 0 THEN nan=0
   IF nan eq 1 THEN nullvalue=!values.f_nan ELSE nullvalue=0
   
   n=n_elements(endbins)-1
   z=fltarr(n)
   s=size(iwc_spec)  
   IF s[0] eq 1 THEN BEGIN  ;One dimensional array, reform into 2D array 
      ntimes=1
      iwc_spec=reform(iwc_spec, 1, n_elements(iwc_spec)) 
   ENDIF
   IF s[0] eq 2 THEN ntimes=s[1]  ;2D array
   
   mmd=fltarr(ntimes)
   FOR i=0L,ntimes-1 DO BEGIN
      tmass=total(iwc_spec[i,*])
      FOR j=0,n-1 DO z[j]=total(iwc_spec[i,0:j])  ;cumulative mass      
      
      ;MMD interpolation is a little tricky: assume the high edge of each bin is where the value in iwc_spec
      ;   is realized.  So interpolation is done on the high edge, endbins[1:*].  This can be 
      ;   double-checked by printing out: for q=0,n-1 do print,endbins[q],endbins[q+1],z[q],tmass/2.0
      ;mmd[i]=interpol(endbins[1:*], z, tmass/2.0)
      
      ;Pad z with a leading zero to allow interpolation in the first bin and adjust endbins index accordingly
      mmd[i]=interpol(endbins, [0,z], tmass/2.0)

      IF tmass eq 0 THEN mmd[i]=nullvalue
   ENDFOR
   return,mmd
END
   
