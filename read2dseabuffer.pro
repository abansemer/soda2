function read2dseabuffer,lun,res=res,sun=sun,tags=tags,probetype=probetype
   ;Function to read in a 2d buffer from an SEA data file.
   ;res is used for computing TAS, should be in microns.
   ;You can check ctltbl.txt to figure out what res the software was using.
   ;The 'tags' input parameter should be a 3-element array for the
   ;   image, TASfactors, and elapsed time, respectively. 
   ;Aaron Bansemer, 10-1-2000
   ;updated for flexible tag numbers 12-21-2001
   ;fixed several timing errors 2-2002
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   forward_function readdatadir
   IF n_elements(res) eq 0 THEN begin & res=25 & print,'Using 25um for SEA tas.' & endif
   IF n_elements(sun) eq 0 THEN sun=0 ; assume running on non-sun, for byte swapping
   IF n_elements(tags) eq 0 THEN tags=[5000,5001,5002]  ; standard tags if not specified
   IF tags[0] eq 0 THEN tags=[5000,5001,5002]
      
   ;point_lun,-(lun),lastpointer ; get current pointer in to file
   q=fstat(lun)
   lastpointer=q.cur_ptr ; get current pointer in to file
   IF q.size-q.cur_ptr le 5000 THEN return,{starttime:999999,image:0,eof:1}
   ;time=bytarr(18)
   time=intarr(9)
   image=ulonarr(1024)
   IF probetype eq 'HVPS' THEN image=uintarr(2048)
   elapsedtime=0ul
   tas=[0us, 0us] ;TAS factors  ;0ul
   imagepoint=0
   gotdata=0             ;flag to test if 2d records found
   i=0l
   REPEAT BEGIN
      REPEAT BEGIN   ;read through all the data directories
         buf=readdatadir(lun, sun=sun) 
         IF buf.tagnumber eq 0       THEN timepoint=lastpointer+buf.dataoffset  ;found a time tag
         IF buf.tagnumber eq tags[0] THEN imagepoint=lastpointer+buf.dataoffset
         IF buf.tagnumber eq tags[1] THEN taspoint=lastpointer+buf.dataoffset
         IF buf.tagnumber eq tags[2] THEN elapsedpoint=lastpointer+buf.dataoffset
         i=i+1
      ENDREP UNTIL (buf.tagnumber eq 999) or (lastpointer+i*16 gt q.size)
      IF imagepoint eq 0 THEN BEGIN              ;no 2d data in this buffer
         lastpointer=lastpointer+buf.dataoffset+buf.parameter1*65536l ;find next buffer location
         point_lun,lun,lastpointer               ;move to next buffer
      ENDIF ELSE gotdata=1
      IF (lastpointer+i*16 gt q.size) THEN return,{starttime:999999,image:0,eof:1}
   ENDREP UNTIL gotdata  ; repeat until one of the sea buffers contains 2d data
   
   IF n_elements(elapsedpoint) eq 0 THEN BEGIN
      print, 'No data found, check tag numbers in read2dseabuffer.pro'
      stop
   ENDIF
      
   point_lun,lun,timepoint   ;READ IN THE DATA
   readu,lun,time
   ;IF sun THEN time=swap_endian(time)
   ;hhmmss=(time[6]+ishft(time[7],8))*10000d + (time[8]+ishft(time[9],8))*100d + (time[10]+ishft(time[11],8)) + $
   ;   (time[12]+ishft(time[13],8))/100d  ;see SEA manual
   hhmmss=time[3]*10000d + time[4]*100d + time[5] + time[6]/100d
   readu,lun,time
   hhmmss_stop=time[3]*10000d + time[4]*100d + time[5] + time[6]/100d
   
   point_lun,lun,imagepoint
   readu,lun,image
   ;IF sun THEN image=swap_endian(image)
   
   point_lun,lun,elapsedpoint
   readu,lun,elapsedtime
   ;IF sun THEN elapsedtime=swap_endian(elapsedtime)
   elapsedtime=elapsedtime*double(25e-6)
   
   point_lun,lun,taspoint
   readu,lun,tas
   ;IF sun THEN tas=swap_endian(tas)
   tas2=tas[0]*100.0*res*1.0e-6/ (tas[1]*0.002)  ;see SEA manual TAS factors (type 6) and equation in type 8

   nextbuffer=buf.dataoffset+lastpointer+buf.parameter1*65536l   ; the 999 tag points to the start of next buffer
   point_lun,lun,nextbuffer ; position the file pointer at the start of next buffer
   return,{starttime:hhmmss, stoptime:hhmmss_stop, year:time[0], month:time[1], day:time[2], image:image,difftime:elapsedtime,tas:tas2,eof:0,pointer:q.cur_ptr}
END
          
