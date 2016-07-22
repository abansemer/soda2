FUNCTION spec_absolutepointers,buffnum,p
   ;Returns the absolute image frame pointers from a specific 4114 byte SPEC buffer.
   ;p is the pointer to the index structure returned by spec_index.pro
   ;buffnum is the buffer number of desired pointers. 
   ;AB 12/2011
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   n=(*p).numimages[buffnum]
   nt=total(n)
   IF nt eq 0 THEN return, {ap:0, n:0}
   ap=ulon64arr(nt)
   c=0L
   FOR i=0,n_elements(buffnum)-1 DO BEGIN
      IF n[i] gt 0 THEN BEGIN
         istart=(*p).firstp[buffnum[i]] 
         istop=(*p).firstp[buffnum[i]]+(*p).numimages[buffnum[i]]-1 
         ap[c:c+n[i]-1]=(*p).pointer[buffnum[i]]+(*p).imagep[istart:istop]
         c=c+n[i]
      ENDIF
   ENDFOR
   return,{ap:ap, n:nt}
END
   