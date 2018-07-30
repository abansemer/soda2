FUNCTION spec_read_frame, lun, bpoint, id
   ;FUNCTION to read in an HVPS3/2DS frame
   ;Send lun, pointer to buffer start, and pointer to frame start
   ;  with respect to integer image array
   ;'id' is 'H'=Horizontal, 'V'=Vertical
   ;AB 5/2011
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   
   point_lun,lun,bpoint
   x=spec_readint(lun,3)  ;Get size of frame
   nh=x[1] and 'fff'x
   nv=x[2] and 'fff'x

   ;Decode some flags
   missingtwh=ishft(x[1] and '1000'x,-12)  ;Missing time words
   missingtwv=ishft(x[2] and '1000'x,-12)
   fifoh=ishft(x[1] and '4000'x,-14)       ;Empty FIFO
   fifov=ishft(x[2] and '4000'x,-14)
   overloadh=ishft(x[1] and '8000'x,-15)   ;Last 2 words are overload times
   overloadv=ishft(x[2] and '8000'x,-15)
   
   ;Read the rest of the frame
   buff=spec_readint(lun,2+nh+nv)  
   
   particlecount=buff[0]
   numslices=buff[1]
   himage=0
   vimage=0
   image=bytarr(128,1)
   htime=0UL
   vtime=0UL
   time=0L
   timefull=0L
   error=1
   overload=0

   ;*NOTE* As of 10/2011 HVPS3, the first time word (bits 16-31) does not always 
   ;     increase monotonically.  Seems to be a problem somewhere... firmware?
   ;Have decided to skip this time word entirely for now
   
   IF (nh ge 2) and (id eq 'H') THEN BEGIN
      IF (nh ge 3) THEN himageraw=buff[2:nh-1] ELSE himageraw=0s    ;Skip last two words (otherwise nh+1)
      hcounter=ulong(buff[nh:nh+1])           ;Last two words is a counter
      timefull=ishft(hcounter[0],16)+hcounter[1] ;Assemble timeword
      time=hcounter[1]  ;Skip hcounter[0] until fixed, rollovers taken care of in processbuffer
      image=spec_decompress(himageraw,overloadh)   
      error=0
      IF missingtwh THEN error=1
      overload=overloadh 
   ENDIF
   
   IF (nv ge 2) and (id eq 'V') THEN BEGIN 
      IF (nv ge 3) THEN vimageraw=buff[nh+2:nh+nv-1] ELSE vimageraw=0s  ;Skip last two words (otherwise nv+1)
      vcounter=ulong(buff[nh+nv:nh+nv+1])     ;Last two words is a counter
      timefull=ishft(vcounter[0],16)+vcounter[1]  ;Assemble timeword
      time=vcounter[1]  ;Skip vcounter[0] until fixed, rollovers taken care of in processbuffer
      image=spec_decompress(vimageraw,overloadv)   
      error=0
      IF missingtwv THEN error=1
      overload=overloadv
   ENDIF

   ;return,{himage:himage, vimage:vimage, nh:nh, nv:nv, htime:htime, vtime:vtime}
   return,{image:image, time:timefull, timetrunc:time, error:error, overload:overload, particlecount:particlecount}
END 
   
   
   
   
   