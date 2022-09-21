FUNCTION fixstuckbits, image, h=h
   ;FUNCTION to work around bits that are stuck 'on'.
   ;This was created for the PIP in the TCSP field project.
   ;Other probes may require tweaking.
   ;x is the structure that comes out of decompress_dmt.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(h) eq 0 THEN h=total(image,2)
   
   ;Make a padded version of h with 5 extra elements on each side, since median filter can't edge_truncate
   pad=7  ;Be sure to use an odd number, otherwise edge is weird
   h2=lonarr(n_elements(h)+pad*2)
   h2[pad:n_elements(h)+pad-1] = h
   hf=median(h2, pad)  ;Median filter good for impulse noise, using pad-point filter
   diff=abs(h2-hf)
   ;Using sqrt(median(h)) to adjust threshold based on the significance of the histogram
   stuck=where(diff gt 10*sqrt(median(h))) - pad

   ;Display stuck bit IDs, use in soda2_browse
   ;cgplot,h
   ;cgoplot,hf[pad:*],color='green'
   ;cgoplot,stuck,h[stuck],/psym,color='red'
    
   ;Old way
   ;stuck=where((h gt 3*mean(h)) or (h lt 0.1*mean(h)))  ;Stuck bits are on more than 3 times the median or off 1/10th... 3 is a guess for now.
   
   IF stuck[0] eq -1 THEN return,image  ;No stuck bits found
   image[stuck,*]=0
   newimage=image

   ;If bit is on to the left of the stuck one, then turn it on.
   ;This results in a 50/50 chance of particle-edge bits being turned on, and 100% chance of internal bits
   for i=0,n_elements(stuck)-1 do begin
      IF stuck[i] eq 0 THEN newimage[stuck[i],*]=image[stuck[i]+1,*] $ ;look on the right if bit 0 is stuck
        ELSE newimage[stuck[i],*]=image[stuck[i]-1,*]                  ;otherwise look left
   endfor
   return, newimage
end


