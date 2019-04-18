FUNCTION fixraking, image, modetype
   ;FUNCTION to work around raked images, where every other diode is undersampled.
   ;This was created for the PIP in the HIWC RADAR II field project.
   ;Other probes may require tweaking.
   ;"Modetype" is a joint parameter indicating how to make the fix:
   ;  An odd number [1,3,5...] will fix odd diodes, even number [2,4,6] will fix even diodes.
   ;  [1,2] will use copy mode, [3,4] convolution, [5,6] and higher different convolutions.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF modetype eq 0 THEN return, image         ;No fix
   
   mode = fix(modetype) mod 2
   type = fix(modetype+1) / 2
   ;IF n_elements(mode) eq 0 THEN mode=1
   ;IF n_elements(type) eq 0 THEN type='copy'
   
   newimage = image
   ndiodes = (size(image,/dim))[0]
   ieven = indgen(ndiodes/2)*2
   iodd = ieven+1
   
;    create_rake=0
;    IF create_rake eq 1 THEN BEGIN
;       ;Create eroded image to forward-simulate the raking, do this for algorithm testing only
;       k=[[0,1,0],[1,1,1],[0,1,0]]
;       erodedimage = erode(erode(image,k),k)  ;Double erode
;       IF mode eq 1 THEN newimage[iodd,*] = erodedimage[iodd,*]
;       IF mode eq 2 THEN newimage[ieven,*] = erodedimage[ieven,*]
;       ;print,image
;       ;print,total(image)
;       ;print,newimage
;       ;print,total(newimage)
;    ENDIF
   
   ;Copy mode
   IF type eq 1 THEN BEGIN
      IF mode eq 1 THEN newimage[iodd,*] = image[ieven,*]
      IF mode eq 2 THEN newimage[ieven,*] = image[iodd,*]
      ;Mode 3 auto-detect not yet implemented
   ENDIF
   
   ;Convolution mode
   IF type ge 2 THEN BEGIN
      ;Use a convolution kernel to more accurately reconstruct bad diodes
      
      IF type eq 2 THEN BEGIN
         ;This seems to perform best based on testing with good data
         kernel = [[0.7,1,0.7],[1,0,1],[0.7,1,0.7]]
         thresh = 2
      ENDIF
      IF type eq 3 THEN BEGIN
         ;This seems to give the best looking images
         kernel = [[1,1,1],[1,0,1],[1,1,1]] 
         thresh = 4
      ENDIF
      
      cimage = convol(float(newimage), kernel, /edge_mirror)
      
      IF mode eq 1 THEN cimage[ieven,*] = 0  ;Mask these off to ignore during next 'where' statement, these diodes not modified
      IF mode eq 2 THEN cimage[iodd,*] = 0  ;Mask these off to ignore during next 'where' statement, these diodes not modified

      good = where(cimage ge thresh)
      newimage[good] = 1  ;Note that this leaves the original shadowed diodes in place too (only adds new pixels)
   ENDIF
   
   return,newimage
end


