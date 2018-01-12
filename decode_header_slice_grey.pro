FUNCTION decode_header_slice_grey, header
   ;Function to return slice count, time counter, depth of field flag,
   ;and particle number from a particle header in DMT grey probes.
   ;The header should come in as an array of 64 bytes.
   ;Parameters are returned in a structure.
   ;As of October 2001, the slice count doesn't seem to be correct.
   ;Aaron Bansemer, 02/08
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   ;Raw header is in 'grey' mode... 64 values from 0 to 3.  Convert to 128-bit slice:
   ind=indgen(64)
   fullslice=bytarr(128)
   
   orig=header   
   ;Sometimes the header slice is misaligned, fix it here 
   IF header[0] ne 0 THEN header=shift(header,-1)
   IF header[0] ne 0 THEN header=shift(header,-1)  ;Try again
     
   fullslice[ind*2+1]=(header and 2b)/2
   fullslice[ind*2]=header and 1b

   powerof2=2L^indgen(15)
   ;From 128-bit slice:
   tas=total(fullslice[56:63]*powerof2)
   particle_count=total(fullslice[64:79]*powerof2)
   counter=total(fullslice[80:82]*powerof2) ; this is in 125 ns increments (8MHz for all probes)
   microsecond=total(fullslice[83:92]*powerof2)
   millisecond=total(fullslice[93:102]*powerof2)
   second=total(fullslice[103:108]*powerof2)
   minute=total(fullslice[109:114]*powerof2)
   hour=total(fullslice[115:119]*powerof2)
   slice_count=total(fullslice[120:127]*powerof2)
   
   ;print,hour,minute,second,millisecond,microsecond,counter,particle_count,slice_count
   time=hour*10000d + minute*100d + second + millisecond/1000d + microsecond/1000000d + counter*125d / 1.0e9
   time_sfm=hour*3600d + minute*60d + second + millisecond/1000d + microsecond/1000000d + counter*125d / 1.0e9
   out={particle_count:particle_count, slice_count:slice_count, time:time, time_sfm:time_sfm, tas:tas}

   return, out
END
