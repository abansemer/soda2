FUNCTION read2dseabuffer, lun, res=res, tags=tags, probetype=probetype
   ;Function to read in a 2d buffer from an SEA data file.
   ;res is used for computing TAS, should be in microns.
   ;You can check ctltbl.txt to figure out what res the software was using.
   ;The 'tags' input parameter should be a 3-element array for the
   ;   image, TASfactors, and elapsed time, respectively.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(res) eq 0 THEN res=25
   IF n_elements(tags) eq 0 THEN tags=[5000,5001,5002]  ; standard tags if not specified
   IF tags[0] eq 0 THEN tags=[5000,5001,5002]

   ;Get pointers to next time and image
   x = seaimagepointers(lun, tags)
   IF x.eof eq 1 THEN return, {starttime:999999, image:0, eof:1}

   ;Read in the start time
   time = intarr(9)
   point_lun, lun, x.timepoint
   readu, lun, time
   year = time[0]
   month = time[1]
   day = time[2]
   hhmmss = time[3]*10000d + time[4]*100d + time[5] + time[6]/double(time[7])

   ;Stop time follows (usually, but not in some old M200 datasets)
   readu, lun, time
   hhmmss_stop = time[3]*10000d + time[4]*100d + time[5] + time[6]/double(time[7])

   ;Elapsed time
   IF x.elapsedpoint gt 0 THEN BEGIN
      elapsedtime=0ul
      point_lun, lun, x.elapsedpoint
      readu, lun, elapsedtime
      elapsedtime = elapsedtime*double(25e-6)
   ENDIF ELSE elapsedtime = 0.0

   ;True air speed
   IF x.taspoint gt 0 THEN BEGIN
      tas = [0us, 0us]        ;TAS factors
      point_lun, lun, x.taspoint
      readu, lun, tas
      tas2 = tas[0]*100.0*res*1.0e-6/ (tas[1]*0.002)  ;see SEA manual TAS factors (type 6) and equation in type 8
   ENDIF ELSE tas2 = 100.0

   ;Initialize image array and read
   ;ALWAYS read image last so file pointer is in correct position for next read
   bytesperslice = 4
   IF (probetype eq 'HVPS1') or (probetype eq 'HAIL') THEN bytesperslice=2
   IF (probetype eq '1D2D') THEN bytesperslice = 8  ;Documentation says 12288 bytes (1536 ull), but really 1024ull for now
   IF bytesperslice eq 2 THEN image = uintarr(x.numberbytes/bytesperslice)  ;Shortened buffers occur in some projects
   IF bytesperslice eq 4 THEN image = ulonarr(x.numberbytes/bytesperslice)  ;Shortened buffers occur in some projects
   IF bytesperslice eq 8 THEN image = ulon64arr(x.numberbytes/bytesperslice) ;1D2D with 64 pixels
   point_lun, lun, x.imagepoint
   readu, lun, image

   ;Reposition file pointer and return
   point_lun, lun, x.nextpointer
   return, {starttime:hhmmss, stoptime:hhmmss_stop, year:year, month:month, day:day, image:image, $
            elapsedtime:elapsedtime, tas:tas2, eof:0, pointer:x.cur_ptr, imagepoint:x.imagepoint, $
            numberbytes:x.numberbytes, timepoint:x.timepoint}
END
