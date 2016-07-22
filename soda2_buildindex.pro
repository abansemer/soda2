FUNCTION soda2_buildindex, fn, pop
   ;PRO to build an index of buffer pointers and time of a raw 2D file.
   ;pop is a pointer to structure with tags=[fn, probetype, format, subformat, tags]
   ;AB 4/2009
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   
   IF (*pop).format eq 'SPEC' THEN BEGIN
      ;Wrote a separate program for this completely different format
      close,1
      openr,1,fn
      data=spec_index(1)
      close,1
      ;Not calling read2dbuffer, so take care of timeoffset here
      data.bufftime=data.bufftime+(*pop).timeoffset 
      return, data
   ENDIF
   
   
   ;Test for .gz extension
   compress=0
   IF strcmp(strmid(fn,strlen(fn)-3,3), '.gz') THEN compress=1
   close,1
   openr,1,fn,compress=compress
   fs=fstat(1)
   IF fs.size eq 0 THEN BEGIN   ;Return error code for empty files
      close,1
      return, {error:2}
   ENDIF
   
   c=0L
   pointer=lonarr(1000000)
   bufftime=dblarr(1000000)
   date=lonarr(1000000)
           
   ;Read the buffers, record start time and pointer
   laststart=0
   REPEAT BEGIN
      x=soda2_read2dbuffer(1, pop)
      ;print,c,x.time
      IF (x.eof eq 0) and (x.time gt 0) and (x.time lt 200000) THEN BEGIN
         pointer[c]=x.pointer
         bufftime[c]=x.time
         date[c]=x.date
         c=c+1
      ENDIF
   ENDREP UNTIL (x.eof eq 1)
   close,1

   errortest=0
   IF c lt 2 THEN errortest=1
   IF errortest THEN return, {error:1}
             
   pointer=pointer[0:c-1]
   bufftime=bufftime[0:c-1]
   date=date[0:c-1]
                    
   data={bufftime:bufftime, date:date, pointer:pointer, count:c, error:0}
   return, data
END
