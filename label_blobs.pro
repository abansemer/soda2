FUNCTION label_blobs, img, _EXTRA=e, dilatelength=dilatelength, integer=integer
   ;function to avoid error where label_region sets edges to zero
   ;using the dilatelength keyword will label on a dilated version of
   ;   the image (useful for messy images)
   ;If dilatelength=1 then will be dilated 1 pixel in each direction,
   ;   dilatelength=2 then 2 pixels in each direction, etc.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(dilatelength) eq 0 THEN dilatelength=0
   IF n_elements(integer) eq 0 THEN integer=0

   img2 = 1b < img                 ;make sure its a binary array
   s = [size(img2,/dim), 1]        ;adding a 1 to array fixes 1-d issue

   pad = 1 > dilatelength                  ;Need more padding when dilation is indicated
   temp = bytarr(s[0]+2*pad,s[1]+2*pad)    ;recreate array with extra 'edge' on each side
   temp[pad:s[0]+pad-1, pad:s[1]+pad-1] = img2

   IF dilatelength ne 0 THEN BEGIN
      kerneldim = (dilatelength*2+1) < (s[1]+2)  ;Can't exceed temp's dimensions
      kernel = bytarr(kerneldim, kerneldim)+1
      dilatedimage = label_region(dilate(temp, kernel, _EXTRA=e))
      labels = temp*dilatedimage
   ENDIF ELSE BEGIN
      labels = label_region(temp, _EXTRA=e)   ;Make labels, automatically converts to integer
   ENDELSE

   IF integer eq 1 THEN return, labels[pad:s[0]+pad-1, pad:s[1]+pad-1] ;Crop labels back to original size
   return, byte(labels[pad:s[0]+pad-1, pad:s[1]+pad-1])          ;Convert to byte and crop array back to original size
END
