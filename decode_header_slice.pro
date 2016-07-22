FUNCTION decode_header_slice, header
   ;Function to return slice count, time counter, depth of field flag,
   ;and particle number from a particle header in DMT 2d probes.
   ;The header should come in as an array of eight bytes.
   ;Parameters are returned in a structure.
   ;As of October 2001, the slice count doesn't seem to be correct.
   ;Aaron Bansemer, October 2001
   ;11-12-2001, rearranged bytes ahead of time for clarity
   ;11-21-2001, corrected data for new format from Darren O'Connor, using 2dimageclock.ppt document.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   ;For some reason, the header is in reverse order
   header=reverse(header)
   
   ;This is easier to decode if changed to binary
   bslice=dec2bin8(header)
   
   ;Particle count is last two bytes, rolls over at 65535
   particle_count=bin2dec(bslice[48:63])
   
   ;Slice count comes from first seven bits
   slice_count=bin2dec(bslice[0:6])
   
   ;Depth of field flag - is one if diode shadowed to 1/3 or less
   dof=bslice[7]
   
   ;Here are the bits for the time counter (16 MHz, or 62.5 ns each count)
   hour=bin2dec(bslice[8:12])
   minute=bin2dec(bslice[13:18])
   second=bin2dec(bslice[19:24])
   millisecond=bin2dec(bslice[25:34])
   counter=bin2dec(bslice[35:47])  ; this is in 125 ns increments (8MHz for all probes)
   
   time=hour*10000d + minute*100d + second + millisecond/1000d + counter*125d / 1.0e9
   time_sfm=hour*3600d + minute*60d + second + millisecond/1000d + counter*125d / 1.0e9

   out={particle_count:particle_count, slice_count:slice_count, dof:dof, time:time, time_sfm:time_sfm}
   return, out
END
