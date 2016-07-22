FUNCTION label_blobs, img, _EXTRA=e, dilate=dilate
   ;function to avoid error where label_region sets edges to zero
   ;using the dilate keyword will label on a dilated version of
   ;   the image (useful for messy images)
   ;If dilate=1 then will be dilated 1 pixel in each direction,
   ;   dilate=2 then 2 pixels in each direction, etc.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   IF n_elements(dilate) eq 0 THEN dilate=0
   img2=1<img                   ;make sure its a binary array
   s=size(img2)
   IF s[0] eq 1 THEN s=[0,s]   ;avoid an error where 1-D arrays have different size parameters
   
   temp=bytarr(s[1]+2,s[2]+2)  ;recreate array with extra 'edge' on each side
   temp[1:s[1],1:s[2]]=img2     ;
   
   IF dilate ne 0 THEN BEGIN
      bb=label_region(dilate(temp,bytarr(dilate*2+1,dilate*2+1)+1), _EXTRA=e)
      aa=bytarr(s[1]+2,s[2]+2)
      shaded=where(temp eq 1)
      aa[shaded]=bb[shaded]
   ENDIF ELSE BEGIN
      aa=label_region(temp, _EXTRA=e) ;make labels
   ENDELSE
   
   aa=aa[1:s[1],1:s[2]]          ;crop array back to original size
   return,aa
END