FUNCTION compute_meanar, spec2d, armidbins
   ;Computes mean area ratio for each time period from a SODA spec2d array
   ;AB 7/2013
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
 
   s=size(spec2d,/dim)
   meanar=fltarr(s[0],s[1]) ;Intialize output array
   
   ;Make a 2D array of area ratio to multiply with spec2d
   ;ar_array=fltarr(s[1],s[2])
   ;FOR i=0,s[2]-1 DO array[*,i]=armidbins[i]
   
   ;Loop through all times
   FOR i=0L,s[0]-1 DO BEGIN
      ;Loop through all sizes      
      FOR j=0,s[1]-1 DO BEGIN
         IF total(spec2d[i,j,*],/nan) gt 0 THEN meanar[i,j]=total(spec2d[i,j,*]*armidbins,/nan)/total(spec2d[i,j,*],/nan)
      ENDFOR
   ENDFOR
   return,meanar
END