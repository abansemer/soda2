FUNCTION mvdiam, conc_raw, endbins, nan=nan
   ;FUNCTION to compute the median volume diameter from a size distribution (or time x size array)
   ;conc should be un-normalized (m^-3).  Use unnormalize.pro if needed.
   ;The interpolate keyword gives an interpolated MVD, instead
   ;  of one centered on a particular bin size.
   ;nan keyword uses NaN for null values rather than 0.
   ;Aaron Bansemer, Oct 2006
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   IF n_elements(nan) eq 0 THEN nan=0
   IF nan eq 1 THEN nullvalue=!values.f_nan ELSE nullvalue=0
   
   midbins=(endbins[1:*] + endbins)/2.0
   n=n_elements(midbins)
   z=fltarr(n)
   s=size(conc_raw)  
   mvd=fltarr(s[1])
   FOR i=0L,s[1]-1 DO BEGIN
      volume=(!pi/6)*midbins^3*conc_raw[i,*]  ;volume in each bin
      tvolume=total(volume)
      
      FOR j=0,n-1 DO z[j]=total(volume[0:j])  ;cumulative volume
      ;MVD interpolation is a little tricky: assume the high edge of each bin is where the value in z
      ;   is realized.  So interpolation is done on the high edge, endbins[1:*].  This can be 
      ;   double-checked by printing out: for q=0,n-1 do print,endbins[q],endbins[q+1],z[q],tvolume/2.0
      mvd[i]=interpol(endbins[1:*], z, tvolume/2.0)
      
      ;Old way.  Tested against new way above, only minimal changes.
      ;w=min(where(z ge 0.5*tvolume)) > 0
      ;mvd[i]=midbins[w]
      
      ;f=(0.5*tvolume-z[w-1])/(z[w]-z[w-1])
      ;mvd[i]=((midbins[w]+midbins[(w+1)<(n-1)])/2 - (midbins[w-1]+midbins[w])/2 )*f + (midbins[w-1]+midbins[w])/2 
   
      IF tvolume eq 0 THEN mvd[i]=nullvalue
   ENDFOR
   return,mvd
END
   
