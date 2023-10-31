function readseabuffer_caps,lun,tag=tag,probetype=probetype
   ;Function to read in a CAS DMT buffer from an SEA data file.
   ;Aaron Bansemer, 2/2007
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF tag[0] eq 0 THEN stop, 'Enter correct tag numbers into probeversion.pro or soda2_probespecs.pro'

   q=fstat(lun)
   lastpointer=q.cur_ptr ; get current pointer in to file
   IF q.size-q.cur_ptr le 5000 THEN return,{starttime:999999,image:0,eof:1}
   time=intarr(9)

   IF probetype eq 'CIP1D' THEN image={header:0s,bytecount:0s,oversizereject:0s,count:intarr(62),dofreject:0s,endreject:0s,$
          housekeeping:intarr(16),particlecounter:0us,secmsec:0s,hourmin:0s,hostsynccounter:02,$
          resetflag:0s,checksum:0s,trailer:0s}
   IF probetype eq 'CAS' THEN image={header:0s,bytecount:0s,transit:0l,sum:0l,fifofull:0s,reset:0s,foverflow:0s,$
          boverflow:0s,interarrival:intarr(64),housekeeping:intarr(31),fcount:intarr(30),$
          bcount:intarr(30),checksum:0s,trailer:0s}

   imagepoint=0
   timepoint=0
   gotdata=0             ;flag to test if 2d records found
   i=0l
   numberbytes=4098  ;Start with a high default so logic below works

   REPEAT BEGIN
      REPEAT BEGIN   ;read through all the data directories
         buf=readdatadir(lun)
         IF buf.tagnumber eq 0 and buf.numberbytes eq 36 THEN BEGIN
            timepoint=lastpointer+buf.dataoffset  ;found a time tag
            timebuf=buf  ;Save for troubleshooting
         ENDIF
         IF buf.tagnumber eq tag[0]  THEN BEGIN
            imagepoint=lastpointer+buf.dataoffset
            numberbytes=buf.numberbytes
            imagebuf=buf ;Save for troubleshooting
         ENDIF
         IF buf.tagnumber eq 999 THEN BEGIN
            contbuf=buf ;Save for troubleshooting
            ;Sometimes the 'parameter1' does not work when the previous buffer is longer than uint limit of 65536.
            ;This takes care of that situation, by adding this buffer's offset to the previous (data) buffer offset.
            dataoffset999 = long(buf.dataoffset)              ;Usual case
            IF buf.dataoffset lt lastbuf.dataoffset THEN $    ;Overflow case
               dataoffset999 = long(lastbuf.dataoffset)+long(lastbuf.numberbytes)
         ENDIF
         lastbuf=buf
         i=i+1
      ENDREP UNTIL (buf.tagnumber eq 999) or (lastpointer+i*16 gt q.size)
      IF (imagepoint eq 0) or (timepoint eq 0) or (numberbytes lt 100) THEN BEGIN  ;no 2d data in this buffer, or too small
         lastpointer=lastpointer+dataoffset999+buf.parameter1*65536l  ;find next buffer location, with the new overrun modification
         point_lun,lun,lastpointer                                     ;move to next buffer
      ENDIF ELSE gotdata=1
      IF (lastpointer+i*16 gt q.size) THEN return,{starttime:999999,image:0,eof:1}
   ENDREP UNTIL gotdata  ; repeat until one of the sea buffers contains 2d data

   ;Read in time
   point_lun,lun,timepoint
   readu,lun,time  ;First is start time
   year=time[0]
   month=time[1]
   day=time[2]
   hhmmss=time[3]*10000d + time[4]*100d + time[5] + time[6]/double(time[7])

   ;Next is stop time
   readu,lun,time
   hhmmss_stop = time[3]*10000d + time[4]*100d + time[5] + time[6]/double(time[7])  ;see SEA manual

   ;Read image
   point_lun,lun,imagepoint
   IF ((probetype ne 'CIP1D') and (probetype ne 'CAS')) THEN image=bytarr(numberbytes)  ;Shortened buffers occur in some projects
   readu,lun,image

   ;Avoid an error when the time is unreasonably large or small, usually early in the file
   IF hhmmss gt 320000 THEN hhmmss=0d
   probetime=0
   IF probetype eq 'CIP1D' THEN probetime=ishft(image.hourmin and 1984,-6)*10000d + (image.hourmin and 63)*100d + $
       ishft(image.secmsec and 64512,-10) + (image.secmsec and 1023)/1000d

   ;Return
   return,{starttime:hhmmss, stoptime:hhmmss_stop, year:year, month:month, day:day, image:image, $
         difftime:sfm(hhmmss_stop)-sfm(hhmmss), eof:0,  pointer:q.cur_ptr, tas:100, probetime:probetime, $
         imagepoint:imagepoint, numberbytes:numberbytes, timepoint:timepoint}
END
