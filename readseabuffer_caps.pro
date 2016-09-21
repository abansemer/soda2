function readseabuffer_caps,lun,sun=sun,tag=tag,probetype=probetype
   ;Function to read in a CAS DMT buffer from an SEA data file.
   ;Aaron Bansemer, 2/2007
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   ;IF n_elements(probetype) eq 0 THEN probetype='CIP10'
   IF n_elements(sun) eq 0 THEN sun=0 ; assume running on non-sun, for byte swapping
   IF n_elements(tag) eq 0 and probetype eq 'CAS' THEN tag=31000 ;Default tag for the WMI SEA system
   IF n_elements(tag) eq 0 and probetype eq 'CIP1D' THEN tag=32000 ;Default tag for the WMI SEA system
   IF n_elements(tag) eq 0 and probetype eq 'CIP10' THEN tag=33000 ;Default tag for the WMI SEA system
   IF n_elements(tag) eq 0 and probetype eq 'CIP7' THEN tag=33000 ;Default tag for the WMI SEA system
   IF n_elements(tag) eq 0 and probetype eq 'CIP' THEN tag=33000 ;Default tag for the WMI SEA system
   IF tag[0] eq 0 THEN stop,'Enter correct tag numbers into probeversion.pro.'
    
   q=fstat(lun)
   lastpointer=q.cur_ptr ; get current pointer in to file
   IF q.size-q.cur_ptr le 5000 THEN return,{starttime:999999,image:0,eof:1}
   time=bytarr(18)
   
   image=bytarr(4096)   ;Default raw image
   IF probetype eq 'CIP1D' THEN image={header:0s,bytecount:0s,oversizereject:0s,count:intarr(62),dofreject:0s,endreject:0s,$
          housekeeping:intarr(16),particlecounter:0s,secmsec:0s,hourmin:0s,hostsynccounter:02,$
          resetflag:0s,checksum:0s,trailer:0s}
   IF probetype eq 'CAS' THEN image={header:0s,bytecount:0s,transit:0l,sum:0l,fifofull:0s,reset:0s,foverflow:0s,$
          boverflow:0s,interarrival:intarr(64),housekeeping:intarr(31),fcount:intarr(30),$
          bcount:intarr(30),checksum:0s,trailer:0s}
   
   imagepoint=0
   timepoint=0
   gotdata=0             ;flag to test if 2d records found
   i=0l
  
   REPEAT BEGIN
      REPEAT BEGIN   ;read through all the data directories
         buf=readdatadir(lun, sun=sun) 
         IF buf.tagnumber eq 0 and buf.numberbytes eq 36 THEN timepoint=lastpointer+buf.dataoffset  ;found a time tag
         IF buf.tagnumber eq tag[0]  THEN imagepoint=lastpointer+buf.dataoffset
         i=i+1
      ENDREP UNTIL (buf.tagnumber eq 999) or (lastpointer+i*16 gt q.size)
      IF imagepoint eq 0 or timepoint eq 0 THEN BEGIN                  ;no 2d data in this buffer
         lastpointer=lastpointer+buf.dataoffset+buf.parameter1*65536l  ;find next buffer location, with the new overrun modification
         point_lun,lun,lastpointer                                     ;move to next buffer
      ENDIF ELSE gotdata=1
      IF (lastpointer+i*16 gt q.size) THEN return,{starttime:999999,image:0,eof:1}   
   ENDREP UNTIL gotdata  ; repeat until one of the sea buffers contains 2d data
   
   point_lun,lun,timepoint   ;READ IN THE DATA
   readu,lun,time  ;First is start time
   year=time[0]+ishft(fix(time[1]),8)
   month=time[2]+ishft(time[3],8)
   day=time[4]+ishft(time[5],8)
   IF time[13] ne 0 then stop
   hhmmss=(time[6]+ishft(time[7],8))*10000d + (time[8]+ishft(time[9],8))*100d + (time[10]+ishft(time[11],8)) + $
      (time[12]+ishft(time[13],8))/double(time[14]+ishft(time[15],8))  ;see SEA manual
  
   readu,lun,time  ;Next is stop time
   hhmmss_stop=(time[6]+ishft(time[7],8))*10000d + (time[8]+ishft(time[9],8))*100d + (time[10]+ishft(time[11],8)) + $
      (time[12]+ishft(time[13],8))/double(time[14]+ishft(time[15],8))  ;see SEA manual

   point_lun,lun,imagepoint
   readu,lun,image
   
   nextbuffer=buf.dataoffset+lastpointer+buf.parameter1*65536l   ; the 999 tag points to the start of next buffer
   point_lun,lun,nextbuffer ; position the file pointer at the start of next buffer
   IF hhmmss gt 320000 THEN hhmmss=0d   ;avoid an error when the time is unreasonably large or small, usually early in the file
   probetime=0
   IF probetype eq 'CIP1D' THEN probetime=ishft(image.hourmin and 1984,-6)*10000d + (image.hourmin and 63)*100d + $
       ishft(image.secmsec and 64512,-10) + (image.secmsec and 1023)/1000d
   return,{starttime:hhmmss, stoptime:hhmmss_stop, image:image, difftime:0, eof:0, pointer:q.cur_ptr, tas:100, probetime:probetime,$
           year:year, month:month, day:day, imagepoint:imagepoint}
END
          
