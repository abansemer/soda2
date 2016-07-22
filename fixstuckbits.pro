FUNCTION fixstuckbits, image
   ;FUNCTION to work around bits that are stuck 'on'.
   ;This was created for the PIP in the TCSP field project.
   ;Other probes may require tweaking.
   ;x is the structure that comes out of decompress_dmt.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   h=total(image,2)
   stuck=where((h gt 3*mean(h)) or (h lt 0.1*mean(h)))  ;Stuck bits are on more than 3 times the mean or off 1/10th... 3 is a guess for now.
   IF stuck[0] eq -1 THEN return,image  ;No stuck bits found
   image[stuck,*]=0

   ;if bit is on to the left of the stuck one, then turn it on.
   ;This results in a 50/50 chance of particle-edge bits being turned on, and 100% chance of internal bits
   for i=0,n_elements(stuck)-1 do begin
      IF stuck[i] eq 0 THEN image[stuck[i],*]=image[stuck[i]+1,*] $ ;look on the right if bit 0 is stuck
        ELSE image[stuck[i],*]=image[stuck[i]-1,*]                  ;otherwise look left
   endfor
   return,image
end


