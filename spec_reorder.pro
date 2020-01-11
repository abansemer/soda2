PRO spec_reorder, fn
   ;Reorder the data in some SPEC Hawkeye data.  There is an error in some Hawkeye
   ;data circa 2019 where the checksum value comes after the timestamp, rather than
   ;after the data buffer.  This will reorder the file to the normal format.
   ;Made for GSFC Hawkeye, IMPACTS 2020.
   ;Input is array of base Hawkeye2DS files.  Reordered files will have suffix '_reordered'
   ;AB 1/2020
   
   ;Format should be:
   ;buffer={header:uintarr(8), image:bytarr(4096), checksum:0us}
   ;Format in files with the error:
   buffer={header:uintarr(8), checksum:0us, image:bytarr(4096)}
   close, 1, 2
   FOR i=0, n_elements(fn)-1 DO BEGIN
      openr, 1, fn[i]
      openw, 2, fn[i]+'_reordered'
      REPEAT BEGIN
         readu, 1, buffer
         ;Note: might be writing wrong checksum here, but does not matter for SODA
         writeu, 2, buffer.header, buffer.image, buffer.checksum
      ENDREP UNTIL eof(1)
      close, 1
      close, 2
   ENDFOR
END