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
         CASE (*pop).subformat OF
            0: im=spec_read_frame(lun,framep.ap[c],(*pop).probeid)
            1: im=spec_read_frame(lun,framep.ap[c], (*pop).probeid, version=1)
            2: im=hvps4_read_frame(lun,framep.ap[c],(*pop).probeid)
         ENDCASE
         slices=n_elements(im.image)/128
         IF (imsize+slices lt maxslices) THEN bitimage[0:127,imsize:imsize+slices-1]=im.image * 2  ;*2 for color table

         ;Add a divider
         IF (divider ne 0) and (imsize+slices lt (maxslices-1)) and (total(im.image) gt 0) THEN BEGIN
            IF divider eq 1 THEN bitimage[0:127,imsize+slices-1]=(indgen(128) mod 4 / 3)*2   ;Dotted line
            IF divider eq -1 THEN bitimage[*,imsize+slices-1] = 254b                         ;White line in soda2_browse
         ENDIF

         ;Increment size if there is a valid particle
         IF (max(im.image) gt 0) THEN imsize=imsize+slices
         c=c+1
      ENDWHILE
      bitimage=bitimage[0:127, 0:((imsize < maxslices)-1 > 0)]
      rejectbuffer=0
      IF (*pop).stuckbits eq 1 THEN bitimage=fixstuckbits(bitimage, h=(*pmisc).lastdhist)

   ;All other probes
   ENDIF ELSE BEGIN
      IF n_elements(b.image) gt 1 THEN BEGIN
         p=soda2_processbuffer(b, pop, pmisc)
         rejectbuffer=p.rejectbuffer
         bitimage=p.bitimage

         ;Adjust mono probe for color table
         IF (max(bitimage) lt 3) THEN bitimage=bitimage*2b

         ;Add a divider
         IF (divider eq -1) THEN bitimage[*, (p.startline-1)>0] = 254b  ;makes a white line in soda2_browse.
         IF (divider eq 1) THEN bitimage[*, (p.startline-1)>0] = 255b   ;makes a black line

         ;Stretch HVPS1
         IF (*pop).probetype eq 'HVPS1' and (n_elements(bitimage) gt 256) THEN BEGIN
            s = size(bitimage, /dim)
            stretch = (*pop).yres/(*pop).res
            bitimage=congrid(bitimage,s[0],s[1]*stretch)
         ENDIF

         ;Compress Hail Spectrometer
         IF (*pop).probetype eq 'HAIL' and (n_elements(bitimage) gt 256) THEN BEGIN
            s = size(bitimage, /dim)
            ;Crop a slice if there are an odd number of slices so rebin works
            bitimage = bitimage[*, 0:s[1]/2*2 - 1]
            bitimage = (bitimage < 1) * 2  ;Remove blob indexes for display, they are kept for PBP files
            stretch = (*pop).yres/(*pop).res
            bitimage=congrid(bitimage,s[0],s[1]*stretch)
         ENDIF

      ENDIF ELSE return, {time:b.time, bitimage:0b, rejectbuffer:1, error:1}
   ENDELSE

   free_lun,lun
   return,{time:b.time, bitimage:bitimage, rejectbuffer:rejectbuffer, error:0}

END
