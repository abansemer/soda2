FUNCTION hvps4_read_frame, lun, bpoint, id
   ;FUNCTION to read in an HVPS-4 frame, slightly different than the 2DS/HVPS-3 and 3VCPI/Hawkeye.
   ;Send lun, pointer to buffer start, and pointer to frame start
   ;  with respect to integer image array
   ;'id' is 'H50', 'H150', 'V50', or 'V150'
   ;AB 8/2022
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.



   point_lun,lun,bpoint
   x=spec_readint(lun,3)  ;Get size of frame
   nh=x[1] and 'fff'x
   nv=x[2] and 'fff'x

   ;Decode some flags
   missingtwh=ishft(x[1] and '1000'x,-12)  ;Missing time words
   missingtwv=ishft(x[2] and '1000'x,-12)
   arrayhid=ishft(x[1] and '4000'x,-14)      ;1=50um, 0=150um
   arrayvid=ishft(x[2] and '4000'x,-14)      ;1=50um, 0=150um
   overloadh=ishft(x[1] and '8000'x,-15)   ;Last 2 words are overload times
   overloadv=ishft(x[2] and '8000'x,-15)

   ;Read the rest of the frame (2+nh+nv found by trial and error)
   buff=spec_readint(lun,2+nh+nv)

   particlecount=buff[0]
   numslices=buff[1]
   himage=0
   vimage=0
   image=bytarr(128,1)
   htime=0UL
   vtime=0UL
   time=0L
   error=1
   overload=0

   ;Use these to make sure only particles for desired array are read, otherwise throws error=1 in output structure
   array=id.substring(0,0)
   res=fix(id.substring(1,3))
   IF arrayhid eq 1 THEN arrayhres=50 ELSE arrayhres=150
   IF arrayvid eq 1 THEN arrayvres=50 ELSE arrayvres=150

   IF (nh gt 3) and (array eq 'H') and (arrayhres eq res) THEN BEGIN
      nh=nh-1
      himageraw=buff[2:nh-1]                  ;Skip last two words (otherwise nh+1)
      hcounter=ulong(buff[nh:nh+2])           ;Last three words for the counter
      time=ishft(hcounter[2],32)+ishft(hcounter[1],16)+hcounter[0]  ;Assemble timeword
      image=spec_decompress(himageraw,overloadh,version=1)
      error=0
      IF missingtwh THEN error=1
      overload=overloadh
   ENDIF

   IF (nv gt 3) and (array eq 'V') and (arrayvres eq res) THEN BEGIN
      nv=nv-1
      vimageraw=buff[2:nv-1]                  ;Skip last two words (otherwise nv+1)
      vcounter=ulong(buff[nv:nv+2])     ;Last three words for the counter
      time=ishft(vcounter[2],32)+ishft(vcounter[1],16)+vcounter[0]  ;Assemble timeword
      image=spec_decompress(vimageraw,overloadv,version=1)
      error=0
      IF missingtwv THEN error=1
      overload=overloadv   ;Not sure about overloads yet, the documentation seems to have typos (HVPS4_Packet_format_Data_and_HK.xls)
   ENDIF

   return,{image:image, time:time, error:error, overload:overload, particlecount:particlecount}
END
