FUNCTION spec_decompress, cimage, overload
   ;Function to decompress a 2DS/HVPS3 raw image
   ;and return bitimage and timelines
   ;AB 10/2011 
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
  
   
   n=n_elements(cimage)
   timeslice=(cimage and '8000'x)/2L^15    ;Bit 15 indicates time slice (not image)
   startslice=(cimage and '4000'x)/2L^14   ;Bit 14 indicates start of new slice
   numclear=(cimage and '7F'x)             ;Bits 0-6
   numshaded=(cimage and '3F80'x)/128      ;Bits 7-13
   
   numslices=total(startslice[0:(n-1)])+3     ;***  +3 IS TEMPORARY TEST
   bitimage=bytarr(128,numslices)
   
   ;Catch an error in 3VCPI data.  Sometimes the timeslice bit (15) is not set
   ;  properly, leading to lots of streaks in the data.
   ;IF (numshaded[n-1] gt (numshaded[(n-2)>0]+4)) THEN timeslice[n-1]=1 ;Last shaded must be less than previous+4
   
   islice=0
   idiode=0
   skip=overload*2   ;Skip the last two words if the overload is set

   FOR i=0,n-1-skip DO BEGIN
      IF not(timeslice[i]) THEN BEGIN ;Ignore timing/overflow words
         IF startslice[i] THEN BEGIN  ;New slice, reset diode index
            islice=islice+1
            idiode=numclear[i]
         ENDIF ELSE idiode=idiode+numclear[i] ;Advance diode index
         IF numshaded[i] gt 0 THEN BEGIN
            IF cimage[i] eq '7fff'x THEN BEGIN
               ;Check for special case '7fff'x = clear line
               bitimage[*,islice]=0 
               idiode=0
            ENDIF ELSE BEGIN
               IF idiode+numshaded[i]-1 lt 128 THEN BEGIN  ;Check for indexes above 128                 
                  goodbit=1  ;HVPS3 in MC3E had persistent artifacts with 32-diode strips at certain positions
                  ;There is a weird error in some HVPS data (e.g. MC3E) with 32 shaded diodes starting on diodes of 0, 32, 64...
                  IF (numshaded[i] eq 32) and ((idiode mod 32) eq 0) THEN BEGIN
                     IF islice eq 0 THEN goodbit=0   ;Can't happen on first slice, would be difficult naturally
                     ;Check the previous slice, skip if less than 4 shaded along same diode posisition
                     IF (islice gt 0) THEN IF (total(bitimage[idiode:idiode+numshaded[i]-1,islice-1]) lt 4) THEN goodbit=0
                  ENDIF
                  IF goodbit THEN bitimage[idiode:idiode+numshaded[i]-1,islice]=1
                  idiode=idiode+numshaded[i]
               ENDIF
            ENDELSE
         ENDIF ELSE BEGIN
            ;Check for special case '4000'x = solid line
            IF cimage[i] eq '4000'x THEN bitimage[*,islice]=1 
         ENDELSE
      ENDIF
   ENDFOR

   return,bitimage
END