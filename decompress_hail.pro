FUNCTION decompress_hail, cimage
   ;Based on SDSMT get_img_hail.pro.
   ;Has some skipped slices and apparently misplaced occluded pixels, but
   ;  confirmed to reproduce image from SDSMT.
   ;AB 1/2023

   ;Find all flags prior to decompression
   image = bytarr(64, 2000)
   newslice = ishft(cimage, -14) and '01'X
   num_occluded = ishft(cimage, -7) and '007F'X
   num_clear = cimage and '003F'X

   ;This was in the original code.  Minimum num_occluded is usually 2, this reduces all num_occluded by 1.
   w = where(num_occluded gt 1, nw)
   IF nw ge 1 THEN num_occluded[w]--

   ;Start decompression, similar to HVPS
   islice = 0
   idiode = 0
   FOR i = 0, n_elements(cimage)-1 DO BEGIN
      ;Reset at start of next slice
      IF newslice[i] THEN BEGIN
         islice++
         idiode = 0
      ENDIF
      idiode += num_clear[i]  ;Advance clear position
      shadowstart = (idiode-num_occluded[i]+1) > 0    ;Note: idiode is the -ending- shadow position, not the start
      shadowstop = idiode
      IF (num_occluded[i] gt 0) and (shadowstop lt 64) THEN image[shadowstart:shadowstop, islice] = 1
   ENDFOR
   return, image[*, 0:islice+1]
end
