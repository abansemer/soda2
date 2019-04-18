FUNCTION decompress_dmt3, cimage
   ;This version meant to be more robust. Find particle sync lines first, then decompress.

   ;Function to decompress a 4096-byte compressed image that is
   ;recorded from the DMT 2d probes.
   ;cimage is the compressed image. The uncompressed image, and
   ;the indexes of the slices that contain sync patterns, are returned
   ;in a structure. The time slices will be the sync indexes+1, and
   ;can be decoded using decode_header_slice.
   ;Aaron Bansemer, October 2001
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   image=bytarr(8,5000)+255 ; this stores the decompressed image in byte form, default is blank (255)
   rlehb=0B  ; run-length encoding header byte
   ipos=0 ; where we are in the decompressed image
   lastgoodipos=0
   cipos=0 ; where we are in the compressed image
   nullbuffer = {image:image, bitimage:0, sync_ind:0, time_elap:0, time_sfm:0, $
                 particle_count:0, slice_count:0, dof:0, remainder:0b, header_slice_count:999}
    
   nbytes=n_elements(cimage)
   IF nbytes le 1 THEN return, nullbuffer

   ;Search for particle sync slices before doing anything else
   sync=bytarr(8)+170  ;Sync is 8 bytes of 170
   c=0
   syncpos=intarr(1000)  ;Particle start position in cimage
   FOR cipos=0, nbytes-8-1 DO BEGIN
      ;Find all syncs.  Checking cipos+1:cipos+8 to retain the leading count byte, which must be in range 7-31.
      IF (total((cimage[cipos+1:cipos+8] eq sync)) eq 8) and ((cimage[cipos] ge 7) and (cimage[cipos] le 31)) THEN BEGIN
         syncpos[c]=cipos
         c++
      ENDIF
   ENDFOR
   
   IF c lt 2 THEN return, nullbuffer
   
   cipos=0
   FOR i=0,c-1 DO BEGIN  ;-1 since c was incremented once extra above, -1 again so final incomplete particle not counted
      IF (ipos mod 8) ne 0 THEN BEGIN
         ipos=ipos + 8-(ipos mod 8)
      ENDIF
      IF cipos gt syncpos[i] THEN BEGIN 
         ;This indicates a decompression error.  Revert back to previous position and erase data from last particle.
         ipos=lastgoodipos    
         image[ipos:*] = 255    ;reset written data from previous particle.
      ENDIF
      ;print,'*'
      lastgoodipos = ipos  ;Keep track of this so can revert back to this position when corrupt data encountered
      FOR cipos=syncpos[i], syncpos[i+1]-1 DO BEGIN
         zeroes=0 & ones=0 & dummy=0
         rlehb=cimage[cipos]
         if (rlehb and 128b) eq 128 then zeroes=1  ;There are 'count' ones to follow
         if (rlehb and 64b) eq 64 then ones=1      ;There are 'count' zeroes to follow
         if (rlehb and 32b) eq 32 then dummy=1     ;This is a dummy byte, skip it
         count=rlehb and 31b  ;Number of zeroes, ones, or existing bytes to follow.  Actual number to add is count+1 per manual.
         ;print,rlehb,dec2bin8(rlehb), 'z=',zeroes, 'o=',ones, 'd=',dummy , 'c=',count+1,format='(i5, 8i4, a6,i3,a6,i3,a6,i3,a6,i3)'
         
         IF (zeroes eq 1) and (ones eq 0) and (dummy eq 0) THEN BEGIN           ;Add in zeroes
            image[ipos:ipos+count]=0B
            ipos=ipos+count+1
         ENDIF
         
         IF (ones eq 1) and (zeroes eq 0) and (dummy eq 0) THEN BEGIN           ;Add in ones
            image[ipos:ipos+count]=255B
            ipos=ipos+count+1
         ENDIF
         
         IF (zeroes eq 0) and (ones eq 0) and (dummy eq 0) THEN BEGIN    ;Just keeps the following bytes
            ;There is an infrequent error where cipos+count is too big, runs to next buffer
            IF cipos+count+1 lt nbytes THEN BEGIN
               ;Realign each new particle at the edge 
               ;IF count gt 2 && total(cimage[cipos:cipos+2] eq byte([170,170,170])) eq 3 then ipos=ipos + 8-(ipos mod 8)
               ;Stuff image with cimage bytes
               image[ipos:ipos+count]=cimage[cipos+1:cipos+count+1]
               ipos=ipos+count+1
               cipos=cipos+count+1
            ENDIF ;ELSE stop
         ENDIF    
      ENDFOR
   ENDFOR
;bitimage=reform(reverse(dec2bin8(not(image)),1),64,5000)
;stop   
   image=image[0:ipos-1] ; truncate image to size
   
   ;-------Find the rest of the sync slices, some are not always detected above-------------
   s=size(image)
   slices=s[1]/8
   image=image[0:slices*8-1] ; this just cuts off partial slices at the end of a buffer
   sync_count=0         ;number of sync slices found
   sync_ind=intarr(500) ;to store the indices of the sync slices
   time=dblarr(500)     ;stores the timeline (in seconds) of each particle
   time_total=dblarr(500) ;the raw time (not elapsed in the buffer)
   particle_count=lonarr(500)  ;stores the particle counter of each particle
   dof=bytarr(500)  ;stores the particle counter of each particle
   header_slice_count=intarr(500)  ; number of slices for each particle
   FOR i=0,slices-2 DO BEGIN   ; 'slices-2' to make sure we get a time slice with each sync slice
      ss=8*i & se=ss+7   ;slice start and end indices
      IF (image[ss] eq 170) and (image[ss+1] eq 170) and (image[se] eq 170) and (total(image[ss:se]) eq 1360) THEN BEGIN
         sync_ind[sync_count]=i
         ;Decode the time slice, which comes right after the sync slice
         params=decode_header_slice(image[ss+8:se+8])
         particle_count[sync_count]=params.particle_count
         dof[sync_count]=params.dof
         header_slice_count[sync_count]=params.slice_count
         
         ;Get the time for each particle, starting each buffer at 0 seconds         
         IF sync_count eq 0 THEN starttime=params.time_sfm
         time[sync_count]=(params.time_sfm-starttime)
         time_total[sync_count]=params.time_sfm
         sync_count=sync_count+1               
      ENDIF
   ENDFOR
   
   ;Bad buffer check
   IF sync_count le 2 THEN return, nullbuffer
 
   sync_ind=sync_ind[0:((sync_count>1)-1)] ; truncate the array to size (>1 since sync_count is sometimes 0 in bad buffers)
   particle_count=particle_count[0:sync_count>1-1]
   dof=dof[0:sync_count>1-1]
   time=time[0:sync_count>1-1]
   time_total=time_total[0:sync_count>1-1]
   header_slice_count=header_slice_count[0:sync_count>1-1]
   ;Sometimes the recorded slice_count is off, which screws up processbuffer.  Compute here.
   slice_stop = [sync_ind[1:*], fix(slices-1)] 
   slice_count = slice_stop - sync_ind - 1   ;Not counting sync/time slices
   ;-----------------------------------------------------

   bitimage=reform(reverse(dec2bin8(not(image)),1),64,slices)  ; change to 64 x #Slices binary image 

   ;This is for running on Windows PCs to avoid a crash
   IF !version.os_family eq 'Windows' THEN wait,0.01  
   
   remainder=cimage[syncpos[c-1]:nbytes-1]   ;Save this to add to next buffer, if desired for bridging.
   
   return, {image:image, bitimage:bitimage, sync_ind:sync_ind, time_elap:time, time_sfm:time_total, $
            particle_count:particle_count, slice_count:slice_count, dof:dof, remainder:remainder, $
            header_slice_count:header_slice_count}
   
END
