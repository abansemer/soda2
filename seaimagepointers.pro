FUNCTION seaimagepointers, lun, tag
   ;Find the pointers to the next image or data buffer in an SEA file
   ;Copied from read2dseabuffer and readseabuffer_caps and consolidated here.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   q = fstat(lun)
   lastpointer = q.cur_ptr ; get current pointer in to file
   IF q.size-q.cur_ptr le 5000 THEN return, {starttime:999999, image:0, eof:1}

   ;Some older instruments don't have tas and/or elapsed tags, figure that out here.
   tas_tagexists=0
   elapsed_tagexists=0
   IF (n_elements(tag) ge 2) && (tag[1] gt 0) THEN tas_tagexists = 1
   IF (n_elements(tag) eq 3) && (tag[2] gt 0) THEN elapsed_tagexists = 1

   ;Initialization
   imagepoint = 0
   timepoint = 0
   elapsedpoint = 0
   taspoint = 0
   gotdata = 0         ;flag to test if 2d records found
   numberbytes = 4098  ;Start with a high default so logic below works

   REPEAT BEGIN      ;Read until and image buffer is found
      maxbytes = 0L
      REPEAT BEGIN   ;Read through data directories until a 'next' tag is found
         buf = readdatadir(lun)
         maxbytes = maxbytes > (long(buf.numberbytes) + long(buf.dataoffset))  ;Look for pointers that exceed limit of 65535

         ;Time
         IF buf.tagnumber eq 0 and ((buf.numberbytes eq 36) or (buf.numberbytes eq 18)) THEN BEGIN
            timepoint = lastpointer+buf.dataoffset
            timebuf = buf  ;Save for troubleshooting
         ENDIF

         ;Image
         IF buf.tagnumber eq tag[0] THEN BEGIN
            imagepoint = lastpointer+buf.dataoffset
            numberbytes = buf.numberbytes
            imagebuf = buf ;Save for troubleshooting
         ENDIF

         ;TAS
         IF (tas_tagexists eq 1) && (buf.tagnumber eq tag[1]) THEN taspoint = lastpointer+buf.dataoffset

         ;Elapsed Time
         IF (elapsed_tagexists eq 1) && (buf.tagnumber eq tag[2]) THEN elapsedpoint = lastpointer+buf.dataoffset

         ;Next
         IF buf.tagnumber eq 999 THEN BEGIN
            ;print, buf.dataoffset, totalbytes+minoffset
            contbuf = buf ;Save for troubleshooting
            ;Sometimes the 'parameter1' does not work when the previous buffer is longer than uint limit of 65536.
            ;This takes care of that situation, by adding this buffer's offset to the previous (data) buffer offset.
            dataoffset = long(buf.dataoffset)                ;Usual case
            IF maxbytes ge 65535 THEN dataoffset = maxbytes  ;Overflow case, can have errors so don't always use
         ENDIF

         lastbuf = buf

         ;Check for file overruns, 16 bytes is for the next datadir
         IF (lastpointer + maxbytes + 16) ge q.size THEN return, {starttime:999999, image:0, eof:1}
      ENDREP UNTIL (buf.tagnumber eq 999)  ;Found next tag

      ;Check if no 2d data in this buffer, or size is too small
      IF (imagepoint eq 0) or (timepoint eq 0) or (numberbytes lt 100) THEN BEGIN
         ;Find next buffer location, with the new overrun modification
         lastpointer = lastpointer + dataoffset + buf.parameter1*65536l
         ;Move to next buffer
         point_lun, lun, lastpointer
      ENDIF ELSE BEGIN
         gotdata = 1
         ;Save position so know where to start the next read
         nextpointer = lastpointer + dataoffset + buf.parameter1*65536l
      ENDELSE
   ENDREP UNTIL gotdata  ;Found image data

   ;print,'****************', q.cur_ptr
   return, {timepoint:timepoint, imagepoint:imagepoint, numberbytes:numberbytes, cur_ptr:q.cur_ptr, $
            elapsedpoint:elapsedpoint, taspoint:taspoint, nextpointer:nextpointer, eof:0}
END
