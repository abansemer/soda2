FUNCTION decompress_dmt, cimage
   ;Function to decompress a 4096-byte compressed image that is
   ;recorded from the DMT 2d probes.
   ;cimage is the compressed image. The uncompressed image, and
   ;the indexes of the slices that contain sync patterns, are returned
   ;in a structure. The time slices will be the sync indexes+1, and
   ;can be decoded using decode_header_slice.
   ;Aaron Bansemer, October 2001
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   image=bytarr(8,5000) ; this stores the decompressed image in byte form
   rlehb=0B  ; run-length encoding header byte
   ipos=0 ; where we are in the decompressed image
   cipos=0 ; where we are in the compressed image
   
   IF n_elements(cimage) le 1 THEN $
      return, {image:image, bitimage:0, sync_ind:0, time_elap:0, time_sfm:0, particle_count:0, slice_count:0}

   ;Advance to the first sync slice (a count byte followed by 8 bytes of '170') before beginning decompression, 
   ; otherwise weird stuff happens.  Checking cipos+1:cipos+8 to retain the leading count byte.
   sync=bytarr(8)+170  ;Sync is 8 bytes of 170
   WHILE total((cimage[cipos+1:cipos+8] eq sync)) ne 8 DO BEGIN
      cipos=cipos+1    
      ;Check for no syncs
      IF cipos ge 4000 THEN return, {image:image, bitimage:0, sync_ind:0, time_elap:0, time_sfm:0, particle_count:0, slice_count:0}  
   ENDWHILE

   WHILE cipos lt 4095 and ipos lt 30000 DO BEGIN
      zeroes=0 & ones=0 & dummy=0
      rlehb=cimage[cipos]
      cipos=cipos+1
      if (rlehb and 128) eq 128 then zeroes=1  ;There are 'count' ones to follow
      if (rlehb and 64) eq 64 then ones=1      ;There are 'count' zeroes to follow
      if (rlehb and 32) eq 32 then dummy=1     ;This is a dummy byte, skip it
      count=rlehb and 31  ;Number of zeroes, ones, or existing bytes to follow
      IF zeroes THEN BEGIN           ;Add in zeroes
         image[ipos:ipos+count]=0B
         ipos=ipos+count+1
      ENDIF
      
      IF ones THEN BEGIN             ;Add in ones
         image[ipos:ipos+count]=255B
         ipos=ipos+count+1
      ENDIF
      
      IF (zeroes eq 0) and (ones eq 0) and (dummy eq 0) THEN BEGIN    ;Just keeps the following bytes
         ;There is an infrequent error where cipos+count is too big
         IF cipos+count le 4095 THEN image[ipos:ipos+count]=cimage[cipos:cipos+count]
         ipos=ipos+count+1
         cipos=cipos+count+1
      ENDIF
      
   ENDWHILE
   image=image[0:ipos-1] ; truncate image to size
   
   ;-------Start the image at a sync slice *PROBABLY UNNECESSARY due to prior advancement-------
   i=-1l
   REPEAT BEGIN
      i=i+1
      IF i eq ipos-7 THEN return,{particle_count:0,bitimage:bytarr(64,1000),sync_ind:0, time_elap:0, time_sfm:0, slice_count:0}  ;indicates a bad buffer, exit now
   ENDREP UNTIL (image[i] eq 170) and (image[i+1] eq 170) and (image[i+7] eq 170) and (total(image[i:i+7]) eq 1360)
   image=image[i:*]
   ;----------------------------------------------------
   
   ;-------Find the rest of the sync slices-------------
   s=size(image)
   slices=s[1]/8
   image=image[0:slices*8-1] ; this just cuts off partial slices at the end of a buffer
   sync_count=0         ;number of sync slices found
   sync_ind=intarr(500) ;to store the indices of the sync slices
   time=dblarr(500)     ;stores the timeline (in seconds) of each particle
   time_total=dblarr(500) ;the raw time (not elapsed in the buffer)
   particle_count=lonarr(500)  ;stores the particle counter of each particle
   dof=bytarr(500)  ;stores the dof flag of each particle
   ;slice_count=intarr(500)  ; number of slices for each particle
   FOR i=0,slices-2 DO BEGIN   ; 'slices-2' to make sure we get a time slice with each sync slice
      ss=8*i & se=ss+7   ;slice start and end indices
      IF (image[ss] eq 170) and (image[ss+1] eq 170) and (image[se] eq 170) and (total(image[ss:se]) eq 1360) THEN BEGIN
         sync_ind[sync_count]=i
         ;Decode the time slice, which comes right after the sync slice
         params=decode_header_slice(image[ss+8:se+8])
         particle_count[sync_count]=params.particle_count
         dof[sync_count]=params.dof
         ;slice_count[sync_count]=params.slice_count
         
         ;Get the time for each particle, starting each buffer at 0 seconds         
         IF sync_count eq 0 THEN starttime=params.time_sfm
         time[sync_count]=(params.time_sfm-starttime)
         time_total[sync_count]=params.time_sfm
         sync_count=sync_count+1               
      ENDIF
   ENDFOR
   sync_ind=sync_ind[0:((sync_count>1)-1)] ; truncate the array to size (>1 since sync_count is sometimes 0 in bad buffers)
   particle_count=particle_count[0:sync_count>1-1]
   dof=dof[0:sync_count>1-1]
   time=time[0:sync_count>1-1]
   time_total=time_total[0:sync_count>1-1]
   ;slice_count=slice_count[0:sync_count>1-1]
   ;sometimes the recorded slice_count is off, which screws up processbuffer
   slice_count=0
   IF sync_count gt 2 THEN slice_count=sync_ind[1:sync_count-1]-sync_ind[0:sync_count-2]-1   
   ;-----------------------------------------------------
   
   bitimage=reform(reverse(dec2bin8(not(image)),1),64,slices)  ; change to 64 x #Slices binary image 

   ;This is for running on Windows PCs to avoid a crash
   IF !version.os_family eq 'Windows' THEN wait,0.01  
   return, {image:image, bitimage:bitimage, sync_ind:sync_ind, time_elap:time, time_sfm:time_total, particle_count:particle_count, slice_count:slice_count, dof:dof}
   
END
