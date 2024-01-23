FUNCTION readseabuffer_caps, lun, tag=tag, probetype=probetype
   ;Function to read in a CAS DMT buffer from an SEA data file.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF tag[0] eq 0 THEN stop, 'Enter correct tag numbers into probeversion.pro or soda2_probespecs.pro'

   ;Get pointers to next time and image
   x = seaimagepointers(lun, tag)
   IF x.eof eq 1 THEN return, {starttime:999999, image:0, eof:1}

   ;Read in time
   point_lun, lun, x.timepoint
   time = intarr(9)
   readu, lun, time  ;First is start time
   year = time[0]
   month = time[1]
   day = time[2]
   hhmmss = time[3]*10000d + time[4]*100d + time[5] + time[6]/double(time[7])

   ;Next is stop time
   readu, lun, time
   hhmmss_stop = time[3]*10000d + time[4]*100d + time[5] + time[6]/double(time[7])  ;see SEA manual

   ;Read image
   ;ALWAYS read image last so file pointer is in correct position for next read
   IF probetype eq 'CIP1D' THEN image = {header:0s, bytecount:0s, oversizereject:0s, count:intarr(62), $
         dofreject:0s, endreject:0s, housekeeping:intarr(16), particlecounter:0us, secmsec:0s, hourmin:0s, $
         hostsynccounter:02, resetflag:0s, checksum:0s, trailer:0s}
   IF probetype eq 'CAS' THEN image = {header:0s, bytecount:0s, transit:0l, sum:0l, fifofull:0s, reset:0s, $
         foverflow:0s, boverflow:0s, interarrival:intarr(64), housekeeping:intarr(31), fcount:intarr(30),$
         bcount:intarr(30), checksum:0s, trailer:0s}

   point_lun, lun, x.imagepoint
   IF ((probetype ne 'CIP1D') and (probetype ne 'CAS')) THEN image = bytarr(x.numberbytes)  ;Shortened buffers occur in some projects
   readu, lun, image

   ;Avoid an error when the time is unreasonably large or small, usually early in the file
   IF hhmmss gt 320000 THEN hhmmss = 0d
   probetime = 0
   IF probetype eq 'CIP1D' THEN probetime = ishft(image.hourmin and 1984,-6)*10000d + (image.hourmin and 63)*100d + $
       ishft(image.secmsec and 64512,-10) + (image.secmsec and 1023)/1000d

   ;Reposition file pointer and return
   point_lun, lun, x.nextpointer
   return, {starttime:hhmmss, stoptime:hhmmss_stop, year:year, month:month, day:day, image:image, $
         difftime:sfm(hhmmss_stop)-sfm(hhmmss), eof:0, pointer:x.cur_ptr, tas:100, probetime:probetime, $
         imagepoint:x.imagepoint, numberbytes:x.numberbytes, timepoint:x.timepoint}
END
