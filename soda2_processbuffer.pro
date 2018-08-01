FUNCTION soda2_processbuffer, buffer, pop, pmisc
   ;FUNCTION to  make any necessary corrections to a buffer,
   ;patches, etc., and then size all particles and return as an array to
   ;the main program.  
   ;smethod is the sizing method, 'xsize', 'fastcircle', 'maxsize', or 'ysize'
   ;Also checks for rejected particles.
   ;Works for a variety of probe types.
   ;timereject will reject particles with interarrival times less than some predetermined number (to avoid broken particles)
   ;stuckbits turns on stuck bit checking for DMT probes only (for now)
   ;water adjusts the rejection criteria to accept only water
   ;Updated 5/05: have decided to keep time associated with rejected particles, added timereject option
   ;10/05 Added stuckbits option
   ;Aaron Bansemer, NCAR, 2009
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   
   ;Define the structure to return for bad buffers   
   nullbuffer= {size:0,probetime:0,reftime:0,ar:0, aspr:0, rejectbuffer:1,bitimage:0,$
                allin:0,streak:0,zd:0,dhist:0,nslices:0,missed:0,overloadflag:0,dof:0b,particle_count:0L}
      
   CASE 1 OF
      ;-----------------------------------------------------------------------------------------------      
      ;-----------------------------------------------------------------------------------------------      
      ;-----------------------------------------------------------------------------------------------      

      
      (((*pop).probetype eq 'F2DC') or ((*pop).probetype eq 'F2DP')): BEGIN
          
          ;Fast 2D probes split images across buffers.  Concatenate leftover slices from previous buffer.
          IF (*pmisc).f2d_remainder_slices gt 0 THEN BEGIN
             remainder=(*pmisc).f2d_remainder[0:(*pmisc).f2d_remainder_slices-1]
             image=[remainder,buffer.image]
          ENDIF ELSE image=buffer.image
          bufflength=n_elements(image)    

          sync_ind=[where((image and 'FFFFFF0000000000'x) eq ulong64('AAAAAA0000000000'x))] 
          num_images=n_elements(sync_ind)
          IF num_images lt 3 THEN BEGIN
             (*pmisc).f2d_remainder_slices=0
             return, nullbuffer  ; This is a bad buffer
          ENDIF
          timelines = image[sync_ind] and '000000FFFFFFFFFF'x  ;The timeline refers to how long AFTER the last particle this one arrived.
          image[sync_ind]='ffffffffffffffff'X             ;replace the non-image lines with blanks (blanks bits are 1, for now)     
          bitimage=dec2bin64(not(image))     
          startline=[0,sync_ind+1]
          stopline=(sync_ind-1) > 0
          stopline=stopline>startline            ;Avoid occasional error with bad sync lines 
                  
          ;IF (*pmisc).lastbufftime eq 0 THEN $  ;For the first buffer only, when lastbufftime not available
          ;   truetime=buffer.time - (timelines[num_images-1] - timelines)/double(12.0e6) $
          ;ELSE truetime=(*pmisc).lastbufftime + (timelines - timelines[0])/double(12.0e6)          
          ;time_sfm=(timelines[num_images-1] - timelines)/double(12.0e6)
          time_sfm=timelines/double(12.0e6)
          reftime=timelines[num_images-1]/double(12.0e6) ;This is the time that -should- match the buffer time
        
          restore_slice=0   ;These probes do not skip the first slice
          missed=0       
          particle_count=intarr(num_images)  ;No counter
          dof=bytarr(num_images)+1  ;Dofs are automatically rejected by requiring timeline AAAAAA.  There is a flipped bit in there if want to use them.
          
          ;Set up remainder for the next buffer
          nremainder=bufflength-max(sync_ind)
          (*pmisc).f2d_remainder_slices=nremainder
          (*pmisc).f2d_remainder[0:nremainder-1]=image[(max(sync_ind)):(bufflength-1)]
          (*pmisc).lastbufftime=buffer.time
       END
      
       (((*pop).probetype eq '2DC') or ((*pop).probetype eq '2DP')): BEGIN
 
          image=buffer.image
          IF n_elements(image) lt 1024 THEN return, nullbuffer   ;This happens with partial buffers at eof's
          sync_ind=where(((image[1:1023] and 'FF000000'X) eq '55000000'X) and ((image[0:1022] and 'FF000000'X) eq '55000000'X),num_images)+1
          
          IF num_images lt 3 THEN BEGIN  ;Try to fix bad timelines and try again
              image=fix2dimage(image)
              sync_ind=where(((image[1:1023] and 'FF000000'X) eq '55000000'X) and ((image[0:1022] and 'FF000000'X) eq '55000000'X),num_images)+1
          ENDIF
          
          IF num_images lt 3 THEN return, nullbuffer  ; Unfixable, this is a bad buffer
          time_ind=sync_ind-1
          sync_ind=[0,sync_ind]    ;Use the first slice as a sync_ind

          timelines=image[time_ind] and '00ffffff'X ;The timeline refers to how long AFTER the last particle this one arrived.     
          
          extralines=where((image and 'ff000000'x) eq '55000000'x)  ;[time_ind, sync_ind]  Changed to clear up some stray timelines that sneak in
          image[extralines]='ffffffff'X             ;replace the non-image lines with blanks (blanks bits are 1, for now)
         
          freq=(*pop).res/(1.0e6*buffer.tas)  ; the time interval of each tick in a timeline
          parttime=(timelines+2)*freq  ; particle interarrival time for each particle, not including transit time.  +2 is for sync and time slices.
          elaptime=dblarr(num_images)
          elaptime[0]=parttime[0]
          FOR i=1,num_images-1 DO elaptime[i]=elaptime[i-1]+parttime[i]
          time_sfm=buffer.time + elaptime - elaptime[num_images-1]   ;Count BACKWARD from the buffer time to get actual time
          reftime=buffer.time + elaptime[num_images-1]  ;This is the time that -should- match the buffer time
     
          bitimage=dec2bin(not(image))     
          startline=sync_ind < 1023
          stopline=time_ind < 1023
          restore_slice=1
          missed=0
          particle_count=intarr(num_images)  ;No counter
          dof=bytarr(num_images)+1  ;No dof flag, assume all are good
       END
       
       (((*pop).probetype eq 'CIP') or ((*pop).probetype eq 'PIP')): BEGIN   
          x=decompress_dmt(buffer.image)
          num_images=n_elements(x.particle_count)-1  ; the number of particles in the buffer
          IF num_images lt 3 THEN return, nullbuffer  ; This is a bad buffer
          startline=x.sync_ind+1     ;This will skip particle fragments starting a buffer
          stopline=x.sync_ind[1:*]
          
          ;NOTE!  The first particle's time seems to match best with PREVIOUS buffer time.  
          ;If clocks drift there can be problems.  They are not accounted for here yet.          
          ;truetime=x.time_sfm+((*pop).timeoffset)
          ;truetime=buffer.time + (x.time_sfm[0:num_images-1] - x.time_sfm[num_images]) + (*pop).timeoffset
          ;print,buffer.time, min(truetime), max(truetime), num_images, max(x.particle_count)-min(x.particle_count)
          time_sfm=x.time_sfm[0:num_images-1]
          reftime=x.time_sfm[num_images]  ;This is the time that -should- match the buffer time
          bitimage=x.bitimage
          bitimage[0:63,x.sync_ind]=0   ;eliminate sync lines
          bitimage[0:63,x.sync_ind+1]=0 ;eliminate time lines         
          restore_slice=0
          
          previouscount=[(*pmisc).lastparticlecount, x.particle_count]
          diffcount=x.particle_count-previouscount
          missed=diffcount[0:num_images-1]-1
          ;The num_images-1 is tricky here since -1 also taken above. The last timeline is a partial image, so really taking num_timelines-2.
          (*pmisc).lastparticlecount=x.particle_count[num_images-1] 
          particle_count=x.particle_count[0:num_images-1]
          dof=x.dof[0:num_images-1]

          ;Assign the first particle in each buffer as an overload for SEA files.  
          ;This is to account for deadtime between buffers in SEA data throttling.
          ;Commented out for now, comparisons with SODA-1 were only so-so.
          ;overload=bytarr(num_images)
          ;IF ((*pop).format eq 'SEA') THEN overload[0]=1 
       END
      
       ((*pop).probetype eq 'CIPG'): BEGIN   
          x=decompress_dmt_grey(buffer.image)
          IF n_elements(x.time_sfm) lt 4 THEN return, nullbuffer  ; This is a bad buffer
          
          bitimage=(3-x.bitimage)    ;make background 0, 1,2,3 grey levels
          bitimage[*,x.sync_ind]=0   ;eliminate sync lines
          startline=x.sync_ind+1     ;This will skip particle fragments starting a buffer
          stopline=x.sync_ind[1:*]-1
          ;time_sfm=x.time_sfm
          ;particle_count=x.particle_count

          ;Remove pixel noise
          IF ((*pop).juelichfilter eq 1) THEN pixel_noise_filter, bitimage
                    
          ;Trying a new way to filter bad particles.  There are lots of them.
          ;Also account for missed/skipped images too, also LOTS of them.
          previouscount=[(*pmisc).lastparticlecount, x.particle_count]
          diffcount=x.particle_count-previouscount
          previoustime=[(*pmisc).lastclock, x.time_sfm]
          difftime=x.time_sfm-previoustime
          deadtime=difftime-difftime/diffcount
          greymax=intarr(n_elements(x.time_sfm))
          ;Eliminate particles that don't have at least 1 level-2 pixel.  Iterating to n-2 will automatically eliminate final incomplete image.
          FOR i=0, n_elements(x.time_sfm)-2 DO greymax[i]=max(bitimage[*,startline[i]:stopline[i]])
          
          good=where((diffcount gt 0) and (diffcount lt 50) and (difftime gt 0) and $
                     (x.slice_count ne 0) and (x.slice_count lt 150) and (greymax ge 2),num_images)
          IF num_images lt 4 THEN return, nullbuffer  ; This is a bad buffer after num_images updated
          startline=startline[good]
          stopline=stopline[good]
          time_sfm=x.time_sfm[good]
          particle_count=x.particle_count[good]
          dof=bytarr(num_images)+1  ;No dof flag, assume all are good
          deadtime=deadtime[good]
          missed=diffcount[good]-1
          (*pmisc).lastparticlecount=particle_count[num_images-1]
          (*pmisc).lastclock=time_sfm[num_images-1]
                 
          reftime=time_sfm[num_images-1]  ;This is the time that -should- match the buffer time          
          restore_slice=0      
       END

       (((*pop).probetype eq 'HVPS3') or ((*pop).probetype eq '2DS') or ((*pop).probetype eq '3VCPI')): BEGIN      
          ;Decompress the images in this buffer
          num_images=(*pmisc).nimages
          IF num_images eq 0 THEN return, nullbuffer
          bitimage=bytarr(128,1)
          startline=intarr(num_images)
          stopline=intarr(num_images)
          inttime=dblarr(num_images)
          overload=bytarr(num_images)
          particle_count=intarr(num_images)
          slicecount=1  ;dummy first slice
          freq=double((*pop).res/(1.0e6*buffer.tas))  ; the time interval of each tick in a timeline
          c=0   ;keep an actual count since there are some bad particles in there that will be skipped
          lastclock=(*pmisc).lastclock
          FOR i=0,num_images-1 DO BEGIN
            IF ((*pop).probetype eq '3VCPI') THEN x=tvcpi_read_frame(1,(*pmisc).imagepointers[i],(*pop).probeid) ELSE $
               x=spec_read_frame(1,(*pmisc).imagepointers[i],(*pop).probeid)
            IF x.error eq 0 THEN BEGIN
               bitimage=[[bitimage],[x.image]]
               startline[c]=slicecount
               slicecount=slicecount+(size(x.image,/dim))[1]
               stopline[c]=slicecount-1
               overload[c]=x.overload
               particle_count[c]=x.particlecount
               IF ((*pop).probetype eq 'HVPS3') THEN x.time=x.timetrunc  ;HVPS has some timing errors, use truncated version.
               ;Check for clock rollovers over at 65536 (every 0.1 seconds at 100 m/s)
               IF x.time lt lastclock THEN inttime[c]=((x.time+65536)-lastclock)*freq ELSE $ 
                    inttime[c]=(x.time-lastclock)*freq
               lastclock=x.time           
               c=c+1
            ENDIF
          ENDFOR
          (*pmisc).lastclock=lastclock       ;Save for next buffer    
          num_images=c   ;update for sizing below
          overload=overload[0:c-1]  ;update size
          particle_count=particle_count[0:c-1]
          
          IF num_images eq 0 THEN return, nullbuffer
          
          ;This is the time that -should- match the buffer time
          ;There tends to be ~4 buffers in a row with the same time stamp.
          ;This check increments from the last time encountered instead of the buffer's stamp.
          IF buffer.time eq (*pmisc).lastbufftime THEN reftime=(*pmisc).maxsfm ELSE reftime=buffer.time 
          time_sfm=dblarr(c)
          
          ;Count backward from buffer time to get time_sfm
          ;This seems to be the correct method.  Use line below to test, 
          ;noting that elapsed particle time in the current buffer is closely
          ;matched to the difference between current buffer time and last buffer time:
          ;print, (*pmisc).lastbufftime, buffer.time, buffer.time-(*pmisc).lastbufftime, totalelapsed
          elaptime=0d
          totalelapsed=total(inttime)
          FOR i=0,c-1 DO BEGIN
             elaptime=elaptime+inttime[i]
             time_sfm[i]=reftime - (totalelapsed - elaptime)
          ENDFOR
          
          ;Counting forward, ignore this unless new info
          ;time_sfm[0]=reftime
          ;FOR i=1,c-1 DO time_sfm[i]=time_sfm[i-1]+inttime[i] 
          
          restore_slice=0
          missed=0
          ;particle_count=intarr(num_images)  ;No counter
          (*pmisc).maxsfm=max(time_sfm)
          (*pmisc).lastbufftime=buffer.time   
           dof=bytarr(num_images)+1  ;No dof flag, assume all are good
       END
      
       ELSE: PRINT, 'Probe type not available'
   ENDCASE 
   
   ;Code common to all probes   
   IF (*pop).stuckbits THEN bitimage=fixstuckbits(bitimage)
   diodetotal=total(bitimage,2)  ;Make sure bitimage is free of time/sync lines
   IF max(diodetotal)/mean(diodetotal) gt 3 THEN streak=1 ELSE streak=0

   size=fltarr(num_images)
   xsize=fltarr(num_images)
   ysize=fltarr(num_images)
   areasize=fltarr(num_images)
   allin=bytarr(num_images)
   centerin=bytarr(num_images)
   edge_touch=bytarr(num_images)
   area_ratio=fltarr(num_images)
   aspr=fltarr(num_images)
   orientation=fltarr(num_images)
   area_orig=fltarr(num_images)
   perimeterarea=fltarr(num_images)  ;Number of pixels on the border, for water detection
   zd=fltarr(num_images)
   nsep=intarr(num_images)
   dhist=intarr((*pop).numdiodes)
   
   ;Boolean flag for SPEC probes indicates empty particle with overload time, set to 0 if not already defined.
   IF n_elements(overload) gt 0 THEN overloadflag=overload ELSE overloadflag=bytarr(num_images) 
   nslices=0  ;Number of image slices (no timelines, sync, etc)
   FOR i=0,num_images-1 DO BEGIN         
      roi=bitimage[*,startline[i]:stopline[i]]  ;extract a single particle from the buffer image
         
      ;Adjust the particle for grey probes to the right threshold
      IF (*pop).greythresh gt 0 THEN BEGIN
         thresh=(*pop).greythresh-1     ;makes next line easier
         roi=((roi > thresh)-thresh)<1  ;turn all pixels above threshold to 1
      ENDIF
      
      IF restore_slice THEN roi=soda2_backfill(roi)
        
      roilen=stopline[i]-startline[i]+1
      nslices=nslices+roilen
      
      ;Option to keep the largest particle when multiple particles exist in an roi.
      ;If enabled, keep in mind this will reduce rejections due to low area ratio.
      IF ((*pop).keeplargest eq 1) THEN BEGIN
         blobs=label_blobs(roi, dilate=2)  ;Neighborhood of 2 allowed
         nblobs=max(blobs)
         IF nblobs gt 1 THEN BEGIN     
            blobhist=histogram(blobs, min=1)  ;min=1 skips white space
            dummy=max(blobhist, ilargest)
            good=where(blobs eq (ilargest+1)) ;add 1 here to get index right
            roi2=roi*0            ;This is a little convoluted since we need to keep original roi in case of grey levels
            roi2[good]=roi[good]  ;Transfer the good particle to roi2
            roi=roi2
         ENDIF
      ENDIF
     
      IF roilen eq 1 THEN roihist=roi ELSE roihist=total(roi,2)   ;Keep this for housekeeping
      dhist=dhist+roihist
 
      ;Find the number of unshaded diodes within the boundary of each particle.  For detecting double particles.
      w=where(roihist gt 0, nw)
      xspan=max(w)-min(w)+1
      nsep[i]=xspan-nw
      
      ;Water processing
      zeed=0
      area_orig[i]=total(roi)
      IF ((*pop).water eq 1) and (*pop).smethod ne 'areasize' THEN BEGIN
         roi=fillholes(roi)     ;fills in poisson spots for liquid water drops, change roi so soda2_findsize get right area ratio, +40% processing time
         ps_correction = poisson_spot_correct(area_orig[i], total(roi), zd=zeed) ; as in Korolev 2007 
      ENDIF ELSE ps_correction=1.0
         
      part=soda2_findsize(roi,(*pop).res,(*pmisc).yres)
      size[i]=part.diam/ps_correction
      xsize[i]=part.xsize/ps_correction
      ysize[i]=part.ysize/ps_correction
      areasize[i]=part.areasize
      area_ratio[i]=part.ar
      aspr[i]=part.aspr
      allin[i]=part.allin   
      centerin[i]=part.centerin
      edge_touch[i]=part.edge_touch 
      zd[i]=zeed
      orientation[i]=part.orientation
      perimeterarea[i]=part.perimeterarea
   ENDFOR   ;image loop end

   return,{size:size,xsize:xsize,ysize:ysize,areasize:areasize,probetime:time_sfm,reftime:reftime,ar:area_ratio,aspr:aspr,rejectbuffer:0,bitimage:bitimage,$
           allin:allin,centerin:centerin,streak:streak,zd:zd,dhist:dhist,nslices:nslices,missed:missed,nsep:nsep,overloadflag:overloadflag,dof:dof,$
           orientation:orientation, area_orig:area_orig, perimeterarea:perimeterarea, particle_count:particle_count, edge_touch:edge_touch}  
END
