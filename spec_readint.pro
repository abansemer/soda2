FUNCTION spec_readint,lun,length,buffsize=buffsize
   ;FUNCTION to read the next 'length' integers from an HVPS/2DS
   ;file.  Main purpose of this is to skip over intervening buffer headers.
   ;There is also a 2-byte buffer trailer.  Format={header:intarr(8), image:bytarr(4096), footer:0s}
   ;AB 5/2011
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   
   IF n_elements(buffsize) eq 0 THEN buffsize=4114
   headersize=9ULL  ;integer length
   point_lun,-lun,pointer
   m1=pointer/buffsize
   m2=(pointer+length*2ULL)/buffsize
   crossings=m2-m1
   ;IF crossings gt 0 THEN print,'crossings: ',crossings,pointer,pointer mod buffsize,buffsize-(pointer mod buffsize),length
   CASE crossings OF
      0:BEGIN
         x=uintarr(length)
         readu,lun,x
      END
      1:BEGIN
         x2=uintarr(length+headersize)
         readu,lun,x2
         div=(buffsize-(pointer mod buffsize))/2 - 1 ;-1 for footer
         IF div gt 0 THEN x=[x2[0:div-1],x2[div+headersize:length+headersize-1]] ELSE $
               x=x2[div+headersize:length+headersize-1]
      END
      ELSE:BEGIN
         print,'Frame spans 2+ buffers, unsupported so far'
         x=uintarr(length+crossings*headersize) 
         readu,lun,x
         x[*]=0  ;Just return a bunch of zeros
      END
   ENDCASE
   
   ;Take care of case where current pointer is at the end of a buffer, set it to start of next one
   point_lun,-lun,pointer
   IF (pointer mod buffsize) eq (buffsize-2) THEN point_lun,lun,pointer+headersize*2
   return,x
END
   