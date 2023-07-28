function read2dseabuffer,lun,res=res,tags=tags,probetype=probetype
   ;Function to read in a 2d buffer from an SEA data file.
   ;res is used for computing TAS, should be in microns.
   ;You can check ctltbl.txt to figure out what res the software was using.
   ;The 'tags' input parameter should be a 3-element array for the
   ;   image, TASfactors, and elapsed time, respectively.
   ;Aaron Bansemer, 10-1-2000
   ;updated for flexible tag numbers 12-21-2001
   ;fixed several timing errors 2-2002
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(res) eq 0 THEN res=25
   IF n_elements(tags) eq 0 THEN tags=[5000,5001,5002]  ; standard tags if not specified
   IF tags[0] eq 0 THEN tags=[5000,5001,5002]

   ;Some older instruments don't have tas and/or elapsed tags, figure that out here.
   tas_tagexists=0
   elapsed_tagexists=0
   IF (n_elements(tags) ge 2) && (tags[1] gt 0) THEN tas_tagexists=1
   IF (n_elements(tags) eq 3) && (tags[2] gt 0) THEN elapsed_tagexists=1

   ;File info
   q=fstat(lun)
   lastpointer=q.cur_ptr ; get current pointer in to file
   IF q.size-q.cur_ptr le 5000 THEN return, {starttime:999999,image:0,eof:1}  ;Nearing end of file

   ;Variable initialization
   imagepoint=0
   gotdata=0             ;flag to test if 2d records found
   i=0l

   ;Find location of next image/time/elapsed/tas buffers
   REPEAT BEGIN
      REPEAT BEGIN   ;read through all the data directories
         buf=readdatadir(lun)
         IF buf.tagnumber eq 0 and ((buf.numberbytes eq 36) or (buf.numberbytes eq 18)) THEN timepoint=lastpointer+buf.dataoffset  ;found a time tag
         IF buf.tagnumber eq tags[0] THEN BEGIN
            imagepoint=lastpointer+buf.dataoffset
            numberbytes=buf.numberbytes
         ENDIF
         IF (tas_tagexists eq 1) &&  (buf.tagnumber eq tags[1]) THEN taspoint=lastpointer+buf.dataoffset
         IF (elapsed_tagexists eq 1) && (buf.tagnumber eq tags[2]) THEN elapsedpoint=lastpointer+buf.dataoffset
         i=i+1
      ENDREP UNTIL (buf.tagnumber eq 999) or (lastpointer+i*16 gt q.size)
      IF imagepoint eq 0 THEN BEGIN              ;no 2d data in this buffer
         lastpointer=lastpointer+buf.dataoffset+buf.parameter1*65536l ;find next buffer location
         point_lun,lun,lastpointer               ;move to next buffer
      ENDIF ELSE gotdata=1
      IF (lastpointer+i*16 gt q.size) THEN return,{starttime:999999,image:0,eof:1}
   ENDREP UNTIL gotdata  ; repeat until one of the sea buffers contains 2d data

   IF n_elements(imagepoint) eq 0 THEN BEGIN
      print, 'No data found, check tag numbers in read2dseabuffer.pro'
      stop
   ENDIF

   ;Read in the start time
   time=intarr(9)
   point_lun,lun,timepoint
   readu,lun,time
   year=time[0]
   month=time[1]
   day=time[2]
   hhmmss=time[3]*10000d + time[4]*100d + time[5] + time[6]/double(time[7])

   ;Stop time follows (usually, but not in some old M200 datasets)
   readu,lun,time
   hhmmss_stop=time[3]*10000d + time[4]*100d + time[5] + time[6]/double(time[7])

   ;Initialize image array and read
   bytesperslice=4
   IF (probetype eq 'HVPS1') or (probetype eq 'HAIL') THEN bytesperslice=2
   IF (probetype eq '1D2D') THEN bytesperslice=8  ;Documentation says 12288 bytes (1536 ull), but really 1024ull for now
   IF bytesperslice eq 2 THEN image=uintarr(numberbytes/bytesperslice)  ;Shortened buffers occur in some projects
   IF bytesperslice eq 4 THEN image=ulonarr(numberbytes/bytesperslice)  ;Shortened buffers occur in some projects
   IF bytesperslice eq 8 THEN image=ulon64arr(numberbytes/bytesperslice) ;1D2D with 64 pixels
   point_lun,lun,imagepoint
   readu,lun,image

   ;Elapsed time
   IF (elapsed_tagexists eq 1) THEN BEGIN
      elapsedtime=0ul
      point_lun,lun,elapsedpoint
      readu,lun,elapsedtime
      elapsedtime=elapsedtime*double(25e-6)
   ENDIF ELSE elapsedtime=0.0

   ;True air speed
   IF (tas_tagexists eq 1) THEN BEGIN
      tas=[0us, 0us]        ;TAS factors
      point_lun,lun,taspoint
      readu,lun,tas
      tas2=tas[0]*100.0*res*1.0e-6/ (tas[1]*0.002)  ;see SEA manual TAS factors (type 6) and equation in type 8
   ENDIF ELSE tas2=100.0

   ;Prepare for next read
   nextbuffer=buf.dataoffset+lastpointer+buf.parameter1*65536l   ; the 999 tag points to the start of next buffer
   point_lun,lun,nextbuffer ; position the file pointer at the start of next buffer

   return, {starttime:hhmmss, stoptime:hhmmss_stop, year:year, month:month, day:day, image:image, $
            elapsedtime:elapsedtime, tas:tas2, eof:0, pointer:q.cur_ptr, imagepoint:imagepoint, numberbytes:numberbytes}
END
