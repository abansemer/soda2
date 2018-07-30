FUNCTION decompress_dmt_grey, cimage
   ;Function to decompress a DMT grey-probe buffer.  Should be a series of bytes.
   ;AB, 2/2008
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   im=bytarr(64,3000)
   next=0l
   pairs=bytarr(4)
   repcount=0
   blankdetect=0
   partcount=0
   timeline=lonarr(300)
   badbuffer={particle_count:0,bitimage:bytarr(64,1000),sync_ind:0, time_elap:0, time_sfm:0, slice_count:0, error:1}
   FOR i=0,n_elements(cimage)-1 DO BEGIN
      IF cimage[i] eq 255 THEN blankdetect=1
      IF cimage[i] ge 128 THEN BEGIN
         ;This is a "count" byte
         n=cimage[i] and 127b   
         im[next:next+n-1]=pairs[3]
         repcount=repcount+n
      ENDIF ELSE BEGIN
         repcount=0
         n=0
         IF blankdetect THEN BEGIN
            next=next-(next mod 64)  ;align on an edge boundary, it sometimes drifts...
            timeline[partcount]=next
            partcount=partcount+1
         ENDIF
         blankdetect=0
         pairs=[(cimage[i] and 64b)/64, (cimage[i] and 48b)/16, (cimage[i] and 12b)/4, cimage[i] and 3b]
         ;This is a data byte
         CASE 1 OF
            pairs[0] eq 1: BEGIN
               im[next]=pairs[1]
               im[next+1]=pairs[2]
               im[next+2]=pairs[3]
               n=3
            END
            pairs[1] eq 1: BEGIN
               im[next]=pairs[2]
               im[next+1]=pairs[3]
               n=2
            END
            pairs[2] eq 1 :BEGIN
               im[next]=pairs[3]
               n=1
            END 
            ELSE:dummy=0
         ENDCASE
      ENDELSE
      next=next+n
   ENDFOR
 
   IF partcount lt 3 THEN return,badbuffer  ;indicates a bad buffer, exit now
   itemp1=timeline[0]                ;start index, begins with the first timeline
   itemp2=timeline[partcount-1]-1+64 ;stop index, this includes the last timeline
   temp=im[itemp1:itemp2]
   bitimage=reform(temp,64,n_elements(temp)/64)
   
   ;sync_ind=(timeline[0:partcount-1]-timeline[0])/64
   ;Sync lines were often missed, causing lots of 1000um-sized artifacts.  Here is a new method of detecting them:
   leftsidetotal=total(bitimage[0:27,*],1)
   lst_shift=[84,leftsidetotal]
   sync_ind=where((leftsidetotal lt 7) and (lst_shift eq 84))  ;Look for blank line followed by solid '3's on left side   
   partcount=n_elements(sync_ind)
   IF partcount lt 3 THEN return,badbuffer  ;indicates a bad buffer, exit now

   time=dblarr(partcount)       ;stores the timeline (in seconds) of each particle
   time_elap=dblarr(partcount)  ;elapsed time from start of buffer
   particle_count=lonarr(partcount)  ;stores the particle counter of each particle
   slice_count=lonarr(partcount)  ;slice count
   tas=intarr(partcount)        ;tas
   
   FOR i=0,partcount-1 DO BEGIN
      s=decode_header_slice_grey(bitimage[*,sync_ind[i]])
      IF i eq 0 THEN starttime=s.time_sfm
      time_elap[i]=(s.time_sfm-starttime)
      time[i]=s.time_sfm
      particle_count[i]=s.particle_count
      slice_count[i]=s.slice_count
      tas[i]=s.tas
;print,particle_count[i],particle_count[i]-particle_count[(i-1)>0]   
   ENDFOR 
   return, {bitimage:bitimage, sync_ind:sync_ind, time_elap:time_elap, time_sfm:time, particle_count:particle_count, slice_count:slice_count, tas:tas}
END
   
