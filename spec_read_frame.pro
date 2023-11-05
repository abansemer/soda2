FUNCTION spec_read_frame, lun, fpoint, id
   ;FUNCTION to read in an HVPS3/2DS frame
   ;Send lun and pointer to frame start
   ;'id' is 'H'=Horizontal, 'V'=Vertical
   ;AB 5/2011
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   point_lun,lun,fpoint

   x = spec_readint(lun,3)  ;Get size of frame
   nh = x[1] and 'fff'x
   nv = x[2] and 'fff'x

   ;Decode some flags
   missingtwh = ishft(x[1] and '1000'x,-12)  ;Missing time words
   missingtwv = ishft(x[2] and '1000'x,-12)
   fifoh = ishft(x[1] and '4000'x,-14)       ;Empty FIFO
   fifov = ishft(x[2] and '4000'x,-14)
   overloadh = ishft(x[1] and '8000'x,-15)   ;Last 2 words are overload times
   overloadv = ishft(x[2] and '8000'x,-15)

   ;Read the rest of the frame
   buff = spec_readint(lun,2+nh+nv)
   particlecount = buff[0]
   numslices = buff[1]
   image = bytarr(128,1)
   time = 0UL
   timetrunc = 0UL
   error = 1
   imageraw = 0b
   overload = 0
   gotframe = 0

   ;Horizontal data
   IF (nh ge 2) and (id eq 'H') THEN BEGIN
      IF (nh ge 3) THEN imageraw = buff[2:nh-1] ELSE imageraw = 0s    ;Skip last two words (otherwise nh+1)
      counter = ulong(buff[nh:nh+1])           ;Last two words is a counter
      overload = overloadh
      missingtw = missingtwh
      nn = nh
      gotframe = 1
   ENDIF

   ;Vertical data
   IF (nv ge 2) and (id eq 'V') THEN BEGIN
      IF (nv ge 3) THEN imageraw = buff[nh+2:nh+nv-1] ELSE imageraw = 0s  ;Skip last two words (otherwise nv+1)
      counter = ulong(buff[nh+nv:nh+nv+1])     ;Last two words is a counter
      overload = overloadv
      missingtw = missingtwv
      nn = nv
      gotframe = 1
   ENDIF

   ;Particle time and image
   IF gotframe THEN BEGIN
      ;As of 10/2011 HVPS3, the first time word (bits 16-31) does not always increase monotonically.
      ;Have decided to skip this time word entirely for now
      time = ishft(counter[0],16)+counter[1]   ;Assemble timeword
      timetrunc = counter[1]         ;Skip counter[0] until fixed, rollovers taken care of in processbuffer

      ;Check for incomplete particle.  Call this function recursively to get the next part
      IF missingtw THEN BEGIN
         ;Sometimes there are timeword-only buffers where error==1, skip over these
         REPEAT BEGIN
            point_lun, -lun, cpoint
            q = spec_read_frame(lun, cpoint, id)
         ENDREP UNTIL q.error eq 0
         ;Concatenate
         imageraw = [imageraw, q.imageraw]
         ;Get time and overload status from the next particle, which has the timewords
         time = q.time
         timetrunc = q.timetrunc
         overload = q.overload
      ENDIF

      ;Decompress
      image = spec_decompress(imageraw, overload)

      ;Check for a timeword with no overload.  This is usually an error where H or V flag appears to be backwards
      error = 0
      IF (overload eq 0) and (nn eq 2) THEN error = 1
   ENDIF

   return, {image:image, imageraw:imageraw, time:time, timetrunc:timetrunc, error:error, overload:overload, $
      particlecount:particlecount}
END
