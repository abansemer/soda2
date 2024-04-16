FUNCTION decompress_hvps, cimage
   ;Function to decompress a 4096-byte compressed image
   ;that is recorded by the HVPS probe.
   ;cimage is the compressed image
   ;NOTE: this follows the 'updated' timing format (circa 1999, not in HVPS1 operators manual)
   ;NOTE: clean buffers should be less than 1000 slices, in future could reject
   ;      bad buffers if there are more than 1000 slices.
   ;AB, January 2002

   IF cimage[0] eq 'CAAA'x THEN BEGIN   ; *** indicates a diagnostic buffer, do not process as image ***
      mask=dec2bin(cimage[2:17]) ;get the mask bits
      mask=mask(16:31,*)  ; truncate leading zeros (should only be 16 bits on each line)
      ;print,'MASK BUFFER'
      return, {mask:1, maskbits:reform(mask,256,1), particle_index:0, image:0, error:0}
   ENDIF

   slice_time=double(4.0e-6) ; the length of time that each slice takes (clock max is 250kHz)
                     ; we will assume that the clock is maxxed out (tas must be less than 50 m/s otherwise)
   image=bytarr(256,10000)  ; this will hold the decompressed image
   time=lonarr(10000)   ; hold the time (number of blank slices since last particle) for each particle
   overflow=0L   ; holds the accumulated overflow time for this buffer
   slice=0L  ; 'y' position in the image array
   diode=0   ; 'x' position in the image array
   particle_num=0L  ;  counts the number of particles in the image (based on finding time words)
   particle_index=lonarr(10000)  ; holds the 'y' index of the last slice of each particle
   i=-1  ; position (in words) within the compressed image

   ;Ignore all data until the start of the first slice is found
   REPEAT i=i+1 UNTIL (((cimage[i] and '8000'x) eq 0) and ((cimage[i] and '4000'x) ne 0)) or (i eq 2047)

   ;Main loop through the compressed image words
   WHILE i lt 2047 DO BEGIN  ; There are 2048 words in the data record
      IF (cimage[i] and '8000'x) ne 0 THEN BEGIN  ; indicates this is a timing or overflow word
         bits1=cimage[i] and '3fff'x    ;word1 holds bits 14-27 of the counter
         bits2=cimage[i+1] and '3fff'x  ;word2 holds bits 0-13 of the counter
         counter=ishft(long(bits1),14)+long(bits2)  ;put the bits together to get the count
         IF (cimage[i] and '4000'x) ne 0 THEN BEGIN  ; this is overflow time
            overflow=overflow+counter
            ;print,counter,i
         ENDIF ELSE BEGIN  ; number of blank slices count (occurs after each particle)
            slice++     ;Add an extra slice for spacing between particles
            time[particle_num]=counter
            particle_index[particle_num]=slice
            particle_num=particle_num+1
         ENDELSE
         i=i+2
      ENDIF ELSE BEGIN   ; indicates this is an image word
         num_clear=(cimage[i] and '007f'x)  ; bits 0-6
         num_shaded=ishft((cimage[i] and '3f80'x),-7)  ; bits 7-13
         IF (cimage[i] and '4000'x) ne 0 THEN startslice=1 ELSE startslice=0
         IF startslice THEN BEGIN  ; start of a slice
            slice++    ;move pointers to the beginning of the next slice
            diode=0
            IF (num_shaded eq 0) and (num_clear eq 0) THEN BEGIN
               diode=127
            ENDIF ELSE BEGIN
               IF num_shaded gt 0 THEN image[diode+num_clear:diode+num_clear+num_shaded-1,slice]=1
               diode=diode+num_clear+num_shaded-1
            ENDELSE
         ENDIF ELSE BEGIN  ; continuation of a slice
            ;Check for bad buffer, will be rejected in process_buffer with zero particle count
            IF (diode+num_clear+num_shaded gt 256) or (diode+num_clear+num_shaded lt 0) THEN return, {particle_index:0, mask:0, error:1}
            IF num_shaded gt 0 THEN image[diode+num_clear:diode+num_clear+num_shaded-1,slice]=1
            diode=diode+num_clear+num_shaded-1
         ENDELSE
         i=i+1
      ENDELSE
   ENDWHILE
   particle_num=particle_num>1  ;to avoid an indexing error in bad buffers
   overflow=overflow*slice_time ; convert to seconds
   rawtime=time[0:particle_num-1]
   time=time[0:particle_num-1]*slice_time  ;truncate to size and convert to seconds
   totaltime=total(time) + slice*slice_time       ;add in the time for non-blank slices for a true total of active probe time
   image=image[*,0:slice]  ;truncate to size
   particle_index=particle_index[0:particle_num-1]

   return, {image:image, time:time, particle_index:particle_index, overflow:overflow, totaltime:totaltime, mask:0,$
            rawtime:rawtime, error:0}
END
