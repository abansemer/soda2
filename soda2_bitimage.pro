FUNCTION soda2_bitimage, fn, pointer, pop, pmisc, divider=divider
   ;Return an image of a buffer from a SODA2 file.
   ;Send in file, pointer location, and pointer to the 'op' structure.
   ;AB 2015
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(divider) eq 0 THEN divider=0   ;Make a visual divider between particles

   openr, lun, fn, /get_lun
   point_lun, lun, pointer
   b=soda2_read2dbuffer(lun, pop)

   ;SPEC Probes, need to build buffer from frames
   IF (*pop).format eq 'SPEC' THEN BEGIN
      ind=where((*pmisc).pointer eq pointer)
      framep=spec_absolutepointers(ind, pmisc)
      ;Build a 'buffer' of up to 2000 slices
      maxslices=2000
      bitimage=bytarr(128,maxslices)
      c=0  
      imsize=0    
      WHILE (c lt framep.n) and (imsize lt maxslices) DO BEGIN   
         IF (*pop).probetype eq '3VCPI' THEN im=tvcpi_read_frame(lun,framep.ap[c],(*pop).probeid) ELSE $
            im=spec_read_frame(lun,framep.ap[c],(*pop).probeid)
         slices=n_elements(im.image)/128
         IF imsize+slices lt maxslices THEN bitimage[0:127,imsize:imsize+slices-1]=im.image
         
         ;Add a divider
         IF (divider eq 1) and (imsize+slices lt (maxslices-1)) and (total(im.image) gt 0) THEN BEGIN
            bitimage[0:127,imsize+slices]=(indgen(128) mod 4 / 3)*2
            imsize=imsize+1
         ENDIF
         
         imsize=imsize+slices
         c=c+1
      ENDWHILE
      bitimage=bitimage[0:127, 0:((imsize < maxslices)-1 > 0)]
      rejectbuffer=0
           
   ;All other probes
   ENDIF ELSE BEGIN
      IF n_elements(b.image) gt 1 THEN BEGIN
         p=soda2_processbuffer(b, pop, pmisc)
         rejectbuffer=p.rejectbuffer
         bitimage=p.bitimage
      ENDIF ELSE return, {time:b.time, bitimage:0b, rejectbuffer:1, error:1}
   ENDELSE
   
   free_lun,lun
   ;Adjust to color table for mono probes
   IF rejectbuffer eq 0 THEN BEGIN
      IF (*pop).probetype ne 'CIPG' THEN bitimage=bitimage*2  
   ENDIF
   
   return,{time:b.time, bitimage:bitimage, rejectbuffer:rejectbuffer, error:0}

END