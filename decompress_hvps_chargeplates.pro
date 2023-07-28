function decompress_hvps_chargeplates,data
   ;Code copied from SDSMT library, adapted for use in SODA.

   nl         = 256                ;number of elements in the HVPS sensor array
                                   ;some of the top and bottom elements are masked out
   ;period     = 6.                 ;period of a slice in um (if clock frequency is 166kHz)
                                   ;true for T28 CO 98 data but maybe not for other data
                                   ;generally the clock speed should vary with TAS to
                                   ;give a horizontal resolution of 400 um
                                   ;period if used to calculate the time of a slice
   period     = 4.D                ;In the HVPS-C the slices are based on a 4 usec clock

   tot_num_slices = 2048           ;number of 2 byte slices in an HVPS image buffer
   tot_np = 2000                   ;most buffer images are less than 1000 pixels;
   num_particles 	= 0

   max_num_particles = 1000
   time_between_particles = fltarr(max_num_particles)
   particle_start  = intarr(max_num_particles)
   particle_area 	= intarr(max_num_particles)
   particle_time   = fltarr(max_num_particles)
   rawtime  = lonarr(max_num_particles)

   ;--------
   img = intarr(tot_np,nl)

   particle_time(0) = 0 ;init_time
   ix = 0l
   i = 0

   while i le (tot_num_slices - 4) do begin
   	if(data(i) eq 'CAAA'X) then begin
   		;print,'Mask: ',i
   		i = i + 1024		;save some time skipping through mask buffer
   	endif

   	if data(i) eq '3FF0'X and data(i+1) eq '3FF1'X then begin
   		inter_part_cycles = ishft(long(data(i+2)),8) or long(data(i+3))
   		iy = 0
   		num_cycles = 0
   		i = i + 52  ;Charge plate skip?

   		sw = 0
   		while sw eq 0 and i lt (tot_num_slices-2) do begin
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
   				img(ix,iy-num_occluded+1:iy) = 1  ;num_particles
   			endif

   			i = i + 1
   			if (data(i) and 'FFFF'X) eq '3FF0'X and (data(i+1) and 'FFFF'X) eq '3FF1'X then sw = 1
   			if((data(i) and 'FFFF'X) eq 'CAAA'X) then sw = 1
   		endwhile				;end of a particle

   		time_between_particles(num_particles) = double(inter_part_cycles) * period * 1.e-6		;seconds
         particle_time(num_particles) = total(time_between_particles(0:num_particles))
         rawtime(num_particles) = inter_part_cycles

   		num_particles = num_particles + 1
         particle_start(num_particles) = ix
      endif else i = i + 1		;if not a end/start slice then increment slice counter
   endwhile			;i < 2047, end of buffer

   particle_end = particle_start[1:(num_particles>1)]  ;This is what soda2_processbuffer needs
   return, {image:transpose(img[0:ix+1,*]), area:particle_area[0:num_particles-1], $
      time:time_between_particles[0:num_particles-1], particle_index:particle_end, $
      rawtime:rawtime[0:num_particles-1], totaltime:total(time_between_particles), mask:0, error:0}
end
