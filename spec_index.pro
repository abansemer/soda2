FUNCTION spec_index, lun, lite=lite, minimagesize=minimagesize, pointerstart=pointerstart, $
         pointerstop=pointerstop, absolutepointer=absolutepointer 
   ;FUNCTION to return the file pointers to image, housekeeping,
   ;and mask buffers in 2DS/HVPS3 data.
   ;Full indexing is working but is very slow and requires lot of
   ;         memory, approx 20% of raw file size...
   ;"Lite" option only returns indexes to the time headers. 
   ;Set pointerstart/pointerstop to begin/end at position other than start of file
   ;AB 10/2011
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   IF n_elements(lite) eq 0 THEN lite=0
   IF n_elements(minimagesize) eq 0 THEN minimagesize=2  ;Minumum number of words required in a compressed image
   IF n_elements(pointerstart) eq 0 THEN pointerstart=0ULL
   IF n_elements(buffsize) eq 0 THEN buffsize=4114ULL
   IF n_elements(absolutepointer) eq 0 THEN absolutepointer=0
   
   f=fstat(lun)
   IF n_elements(pointerstop) eq 0 THEN pointerstop=f.size
   bytes2read=pointerstop-pointerstart
   
   ;Find the buffer size from the position of 'year' data
   point_lun,lun,0
   x=intarr(10000<(f.size/2))
   readu,lun,x
   w=where(x eq x[0])   ;Year equals first year
   buffsize=w[1]*2ULL
   
   ;buffsize=91;4114ULL   ;As of 4/2011 there is a buffer time header every 4114 bytes
   header={year:0s, month:0s, dummy:0s, day:0s, hour:0s, minute:0s, second:0s, millisecond:0s}
   headersize=n_tags(header,/data_length)   ;This should be the first buffer
   ndataints=(buffsize-headersize-2)/2 ;length of data in short ints, note:there appears to be a dummy int at the end, ignore
   
   thisbuffer=uintarr(ndataints)
   nextbuffer=uintarr(ndataints)
   
   point_lun,lun,pointerstart
   readu,lun,header
   firstday=header.day    ;To check for midnight crossings
   readu,lun,thisbuffer   ;Get the first buffer
   point_lun,lun,pointerstart
   
   numbuffs=bytes2read/buffsize
   bufftime=dblarr(numbuffs)
   buffpoint=lindgen(numbuffs)*buffsize
   firstp=lonarr(numbuffs)-1  ;Index of first image in each buffer
   numimages=intarr(numbuffs) ;Number in each buffer
   date=lonarr(numbuffs)

   id=0s
   numpointers=bytes2read/10
   imagep=intarr(numpointers)   ;Image pointers (offset from buffer start to save memory)
   IF absolutepointer eq 1 THEN imagep=lon64arr(numpointers)   ;Image pointers (full)
   ibuffer=lonarr(numpointers)  ;Index of nearest buffer header to each image
   ihk=lonarr(numpointers)      ;Index of nearest housekeeping header to each image
   imagec=0L                    ;Image counter
   maskp=lon64arr(1000)         ;Mask pointers
   maskc=0L                     ;Mask counter
   hkp=lon64arr(100000)         ;Housekeeping pointers
   hkc=0L                       ;Housekeeping counter
   j=0
   
   ;Get all the time buffers
    FOR i=0L,numbuffs-2 DO BEGIN
        IF (header.year lt 2005) or (header.year gt 2030) THEN stop, 'File misaligned'
        ;bufftime[i]=(header.day ne firstday)*86400D + header.hour*3600D + header.minute*60D + header.second + header.millisecond/1000D
        ;Will take care of midnight crossings elsewhere
        bufftime[i]=header.hour*3600D + header.minute*60D + header.second + header.millisecond/1000D
        date[i]=julday(header.month,header.day,header.year)
        
        point_lun,lun,((i+1)*buffsize)
        readu,lun,header
        readu,lun,nextbuffer
        b=[thisbuffer,nextbuffer]

        REPEAT BEGIN
           CASE b[j] OF
            12883:BEGIN
               nh=b[j+1] and 'fff'x
               nv=b[j+2] and 'fff'x
               IF (nh ge minimagesize) or (nv ge minimagesize) THEN BEGIN  ;Ignore small particles
                  imagep[imagec]=headersize+j*2    ;i*buffsize+ removed... just using offset now
                  ibuffer[imagec]=i     ;Index of nearest buffer header
                  ihk[imagec]=(hkc-1)>0 ;Index of nearest housekeeping header
                  IF absolutepointer eq 1 THEN imagep[imagec]=i*buffsize+headersize+j*2    ;full pointer
                  IF firstp[i] eq -1 THEN firstp[i]=imagec  ;Index of first image in each buffer
                  numimages[i]=numimages[i]+1
                  imagec=imagec+1  ;Total count
               ENDIF
               j=j+5+nh+nv               
            END
            19787:BEGIN
               maskp[maskc]=i*buffsize+headersize+j*2
               maskc=maskc+1
               j=j+23             
            END
            18507:BEGIN
               hkp[hkc]=i*buffsize+headersize+j*2
               hkc=hkc+1
               j=j+53
           END
            20044:BEGIN
               j=ndataints
            END
            0:j=j+1
            48813:j=j+1
            ELSE:BEGIN ;Advance to next j if nothing found
               ;print,'unknown tag',b[j],' ',string(byte((b[j] and 'ff'x))),string(byte((ishft(b[j],-8) and 'ff'x)))
               j=j+1
            END
           ENDCASE        
        ENDREP UNTIL j ge ndataints
        j=j-ndataints
        IF j ge ndataints THEN j=0  ;Take care of rare error in noisy data where j gets way too big, resets at start of next buffer
        thisbuffer=nextbuffer
        
   ENDFOR
   ;for the last header
   bufftime[numbuffs-1]=header.hour*3600D + header.minute*60D + header.second + header.millisecond/1000D
   date[numbuffs-1]=julday(header.month,header.day,header.year)
  
   ;firstp=[1,lastimage+1]  ;Shift the index to the first particle in each buffer
   IF lite eq 1 THEN return,{bufftime:bufftime, buffpoint:buffpoint, firstp:firstp}   
   imagep=temporary(imagep[0:(imagec-1)>0])
   ibuffer=temporary(ibuffer[0:(imagec-1)>0])
   ihk=temporary(ihk[0:(imagec-1)>0])
   hkp=hkp[0:(hkc-1)>0]
   maskp=maskp[0:(maskc-1)>0]
   ;print,'allocated: ',numpointers,' image: ',imagec,' house: ',hkc,' mask: ',maskc
   return,{date:date, bufftime:bufftime, pointer:buffpoint, firstp:firstp, imagep:imagep, $
           hkp:hkp, maskp:maskp, count:numbuffs, numimages:numimages, buffsize:buffsize, $
           ibuffer:ibuffer, ihk:ihk, error:0}
   
END
 
