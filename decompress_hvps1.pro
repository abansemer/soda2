FUNCTION decompress_hvps1, cimage
   ;Function to decompress a 2048-byte compressed image recorded by the orginal SDSMT HVPS-1 (a.k.a. HVPS-C with charge
   ;   plates)
   ;cimage is the compressed image
   ;This follows code provided by SDSMT, get_img_only.pro
   ;AB, January 2023

   ;stop
   np         = 1000               ;most buffer images are less than 1000 pixels;
   nl         = 256                ;number of elements in the HVPS sensor array
                                   ;some of the top and bottom elements are masked out
   num_slices = 2048               ;number of 2 byte slices in an HVPS image buffer
   ;period     = 6.                 ;period of a slice in um (if clock frequency is 166kHz)
                                   ;true for T28 CO 98 data but maybe not for other data
                                   ;generally the clock speed should vary with TAS to
                                   ;give a horizontal resolution of 400 um
                                   ;period if used to calculate the time of a slice
   ;period = 0.0004 / tas          ;if clock speed is variable
   period     = 4.D                ;In the HVPS-C the slices are based on a 4 usec clock

   max_num_particles = 1000
   tot_num_slices = 2100
   init_time = 0D

   tot_np = np

   num_particles 	= 0

   time_between_particles = fltarr(max_num_particles)
   particle_start  = intarr(max_num_particles)
   particle_end    = intarr(max_num_particles)
   particle_bottom = intarr(max_num_particles)
   particle_top    = intarr(max_num_particles)
   particle_area 	= intarr(max_num_particles)
   particle_time   = fltarr(max_num_particles)
   charge_plates	= fltarr(1000,8,6)

   particle_start(*) = 20000

   img = intarr(tot_np,nl)
   img(*,*) = -1
   time_img = fltarr(tot_np,nl)		;necessary anymore?

   particle_time(0) = init_time
   ix = 0l
   i = 0
   data = cimage

   while i lt n_elements(data)-1 do begin ;(tot_num_slices - 48) do begin
   	if(data(i) eq 'CAAA'X) then begin
   		;print,'Mask: ',i
   		i = (i+1) < (n_elements(data)-1)  ;i + 1024		;save some time skipping through mask buffer
   	endif

   	if data(i) eq '3FF0'X and data(i+1) eq '3FF1'X then begin
   		;print,format = '(2Z8)',i,data(i),data(i+1)
   		inter_part_cycles = ishft(long(data(i+2)),16) or long(data(i+3))
   		;print,'inter_part_cycles: ',inter_part_cycles

   		for l=0,5 do charge_plates(num_particles,*,l) = data(i+4+l*8:i+4+(l+1)*8-1)
   		iy = 0
   		num_cycles = 0
   		i = i + 52
   		sw = 0
   		while sw eq 0 and i lt (tot_num_slices-1) do begin
   			if((data(i) and '4000'X) gt 0) then begin
   				bit2 = 'S'
   				ix = ix + 1l
   				iy = 0
   				num_cycles = num_cycles + 1.
   			endif else bit2 = 'C'
   			num_clear = data(i) and '7F'X
   			plus = data(i) and '80'X
   			num_occluded = ishft(data(i),-7) and '7F'X
   			if bit2 eq 'S' and num_clear eq 0 then num_clear = 128
   			particle_area(num_particles) = particle_area(num_particles) + num_occluded
   			iy = iy + num_clear + num_occluded

   			if (iy gt 15 and iy lt 240 and ix lt tot_np and num_occluded ne 0) then begin
   				img(ix,iy-num_occluded+1:iy) = num_particles
   			endif

   			i = i + 1
   			if (data(i) and 'FFFF'X) eq '3FF0'X and (data(i+1) and 'FFFF'X) eq '3FF1'X then sw = 1
   			if((data(i) and 'FFFF'X) eq 'CAAA'X) then sw = 1
   		endwhile				;end of a particle

   		time_between_particles(num_particles) = double(inter_part_cycles) * period * 1.e-6		;seconds

   		particle_end(num_particles) = ix + 1
   		particle_end(num_particles) = particle_end(num_particles) > particle_start(num_particles)

   		num_particles = num_particles + 1
   		if charge_sw eq 1 then particle_start(num_particles) = ix + 15 else particle_start(num_particles) = ix
   		s1 = particle_start(num_particles-1)
   		s2 = particle_end(num_particles-1)
   		;time_between_particles(num_particles-1) = time_between_particles(num_particles-1) + ((s2-s1) * period * 1.e-6)
   		particle_time(num_particles-1) = total(time_between_particles(0:num_particles-1)) / 3600. + init_time	;hours
   		;print,'s1,s2: ',s1,s2
   		tem = total(img(0>s1<(tot_np-1):s1>s2<(tot_np-1),*),1)
   		ind = where(tem gt 0,cnt)
   		if cnt ne 0 then begin
   			particle_bottom(num_particles-1) = ind(0)
   			particle_top(num_particles-1) = ind(cnt-1)

   		endif else begin
   			particle_bottom(num_particles-1) = 0
   			particle_top(num_particles-1) = 0
   		endelse
   	endif else i = i + 1		;if not a end/start slice then increment slice counter
   endwhile			;i < 2047, end of buffer

   return, {image:img, time:particle_time[0:num_particles], totaltime:total(time_between_particles[0:num_particles]), $
      inttime:time_between_particles[0:num_particles], mask:0}



   ;Original SODA HVPS decompression code below, save for now.
   IF cimage[0] eq 'CAAA'x THEN BEGIN   ; *** indicates a diagnostic buffer, do not process as image ***
      mask=dec2bin(cimage[2:17]) ;get the mask bits
      mask=mask(16:31,*)  ; truncate leading zeros (should only be 16 bits on each line)
      print,'MASK BUFFER'
      return,{mask:1,maskbits:reform(mask,256,1),particle_index:0,image:0}
   ENDIF

   slice_time=double(4.0e-6) ; the length of time that each slice takes (clock max is 250kHz)
                     ; we will assume that the clock is maxxed out (tas must be less than 50 m/s otherwise)
   image=bytarr(256,2100)  ; this will hold the decompressed image
   time=lonarr(1000)   ; hold the time (number of blank slices since last particle) for each particle
   overflow=0l   ; holds the accumulated overflow time for this buffer
   slice=0  ; 'y' position in the image array
   diode=0   ; 'x' position in the image array
   particle_num=0  ;  counts the number of particles in the image (based on finding time words)
   particle_index=intarr(1000)  ; holds the 'y' index of the last slice of each particle
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
            slice=slice+1  ;move pointers to the beginning of the next slice
            diode=0
            IF (num_shaded eq 0) and (num_clear eq 0) THEN diode=127 ELSE BEGIN
               IF num_shaded gt 0 THEN image[diode+num_clear:diode+num_clear+num_shaded-1,slice]=1
               diode=diode+num_clear+num_shaded-1
            ENDELSE
         ENDIF ELSE BEGIN  ; continuation of a slice
            IF (diode+num_clear+num_shaded gt 256) or (diode+num_clear+num_shaded lt 0) THEN return,{particle_index:0} ;bad buffer, will be rejected in process_buffer with zero particle count
            IF num_shaded gt 0 THEN image[diode+num_clear:diode+num_clear+num_shaded-1,slice]=1
            diode=diode+num_clear+num_shaded-1
         ENDELSE
         i=i+1
      ENDELSE
   ENDWHILE
   particle_num=particle_num>1  ;to avoid an indexing error in bad buffers
   overflow=overflow*slice_time ; convert to seconds
   time=time[0:particle_num-1]*slice_time  ;truncate to size and convert to seconds
   totaltime=total(time) + slice*slice_time       ;add in the time for non-blank slices for a true total of active probe time
   image=image[*,0:slice]  ;truncate to size
   particle_index=particle_index[0:particle_num-1]

   return, {image:image,time:time,particle_index:particle_index,overflow:overflow,totaltime:totaltime,mask:0}
END
