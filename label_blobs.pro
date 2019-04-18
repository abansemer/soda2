FUNCTION label_blobs, img, _EXTRA=e, dilate=dilate
   ;function to avoid error where label_region sets edges to zero
   ;using the dilate keyword will label on a dilated version of
   ;   the image (useful for messy images)
   ;If dilate=1 then will be dilated 1 pixel in each direction,
   ;   dilate=2 then 2 pixels in each direction, etc.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   IF n_elements(dilate) eq 0 THEN dilate=0
   img2 = 1b < img                 ;make sure its a binary array
   s = [size(img2,/dim), 1]        ;adding a 1 to array fixes 1-d issue
   
   temp = bytarr(s[0]+2,s[1]+2)    ;recreate array with extra 'edge' on each side
   temp[1:s[0],1:s[1]] = img2
   
   IF dilate ne 0 THEN BEGIN
      kerneldim = (dilate*2+1) < (s[1]+2)  ;Can't exceed temp's dimensions
      kernel = bytarr(kerneldim, kerneldim)+1
      dilated = label_region(dilate(temp, kernel, _EXTRA=e))
      labels = temp*dilated
   ENDIF ELSE BEGIN
      labels = label_region(temp, _EXTRA=e) ;make labels
   ENDELSE
   
   return, byte(labels[1:s[0],1:s[1]])          ;crop array back to original size
END