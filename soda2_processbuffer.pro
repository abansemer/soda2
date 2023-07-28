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
   nullbuffer= {diam:0,probetime:0,reftime:0,ar:0, rawtime:0, aspr:0, rejectbuffer:1,bitimage:0,$
                allin:0,streak:0,zd:0,dhist:0,nslices:0,missed:0,overloadflag:0,dofflag:0b,particlecounter:0L, $
                inttime:0d, clocktas:0.0}

   CASE 1 OF
      ;-----------------------------------------------------------------------------------------------
      ;-----------------------------------------------------------------------------------------------
      ;-----------------------------------------------------------------------------------------------


      (((*pop).probetype eq 'F2DC') or ((*pop).probetype eq 'F2DC_v2')): BEGIN

          ;Fast 2D probes split images across buffers.  Concatenate leftover slices from previous buffer.
          IF (*pmisc).f2d_remainder_slices gt 0 THEN BEGIN
             remainder=(*pmisc).f2d_remainder[0:(*pmisc).f2d_remainder_slices-1]
             image=[remainder,buffer.image]
          ENDIF ELSE image=buffer.image
          bufflength=n_elements(image)

          ;Only compare the first two bytes for sync, this allows dof flag and also works for F2DC_v2
          sync_ind=[where((image and 'FFFF000000000000'x) eq ulong64('AAAA000000000000'x))]
          num_images=n_elements(sync_ind)
          IF num_images lt 3 THEN BEGIN
             (*pmisc).f2d_remainder_slices=0
             return, nullbuffer  ; This is a bad buffer
          ENDIF

          IF (*pop).probetype eq 'F2DC_v2' THEN BEGIN
             timelines = image[sync_ind] and '00000FFFFFFFFFFF'x   ;SPICULE 2021, changed from '00007FFFFFFFFFFF' to '00000FFFFFFFFFF'
             dof = ((image[sync_ind] and '0000100000000000'x)) < 1 ;changed from '0000800000000000'x to '0000100000000000'x
          ENDIF ELSE BEGIN
             timelines = image[sync_ind] and '000000FFFFFFFFFF'x  ;The timeline refers to how long AFTER the last particle this one arrived.
             dof = ((image[sync_ind] and '0000010000000000'x)) < 1
          ENDELSE
          dof = 1-dof  ;set so 1=accepted

          image[sync_ind]='ffffffffffffffff'X             ;replace the non-image lines with blanks (blanks bits are 1, for now)
          bitimage=dec2bin64(not(image))
          startline=[0,sync_ind+1]
          stopline=(sync_ind-1) > 0
          stopline=stopline>startline            ;Avoid occasional error with bad sync lines

          ;IF (*pmisc).lastbufftime eq 0 THEN $  ;For the first buffer only, when lastbufftime not available
          ;   truetime=buffer.time - (timelines[num_images-1] - timelines)/double(12.0e6) $
          ;ELSE truetime=(*pmisc).lastbufftime + (timelines - timelines[0])/double(12.0e6)
          ;time_sfm=(timelines[num_images-1] - timelines)/double(12.0e6)
          clockHz=12.0e6
          IF (*pop).probetype eq 'F2DC_v2' THEN clockHz=1e8/3d ;was 33.0e6, now 33.33333e6, found by trial and error SPICULE
          time_sfm=timelines/double(clockHz)
          reftime=timelines[num_images-1]/double(clockHz) ;This is the time that -should- match the buffer time
          rawtime=timelines

          restore_slice=0   ;These probes do not skip the first slice
          missed=0
          particle_count=intarr(num_images)  ;No counter
          ;dof=bytarr(num_images)+1  ;Dofs are automatically rejected by requiring timeline AAAAAA.  There is a flipped bit in there if want to use them.

          ;Set up remainder for the next buffer
          nremainder=bufflength-max(sync_ind)
          (*pmisc).f2d_remainder_slices=nremainder
          (*pmisc).f2d_remainder[0:nremainder-1]=image[(max(sync_ind)):(bufflength-1)]
          (*pmisc).lastbufftime=buffer.time
          stretch=fltarr(num_images)+1.0  ;Not implemented yet for this probe, assume no stretch
          inttime=dblarr(num_images)   ;Not yet implemented for this probe
          clocktas=fltarr(num_images)+buffer.tas
       END

       ((*pop).probetype eq '2DC'): BEGIN
          image=buffer.image
          buffer.stoptime = buffer.time   ;Not reliable in old datasets, set to same as starttime
          IF n_elements(image) lt 1024 THEN return, nullbuffer   ;This happens with partial buffers at eof's
          sync_ind=where(((image[1:1023] and 'FF000000'X) eq '55000000'X) and ((image[0:1022] and 'FF000000'X) eq '55000000'X),num_images)+1

          IF num_images lt 3 THEN BEGIN  ;Try to fix bad timelines and try again
              image=fix2dimage(image)
              sync_ind=where(((image[1:1023] and 'FF000000'X) eq '55000000'X) and $
                 ((image[0:1022] and 'FF000000'X) eq '55000000'X) and $
                 (image[0:1022] ne '55000000'x), num_images) + 1
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
          rawtime=timelines

          bitimage=dec2bin(not(image))
          startline=sync_ind < 1023
          stopline=time_ind < 1023
          restore_slice=1
          missed=0
          particle_count=intarr(num_images)  ;No counter
          dof=bytarr(num_images)+1  ;No dof flag, assume all are good
          stretch=fltarr(num_images)+1.0  ;Not implemented yet for this probe, assume no stretch
          previoustime=[(*pmisc).lastclock, time_sfm]
          inttime=time_sfm-previoustime
          (*pmisc).lastclock=time_sfm[num_images-1]       ;Save for next buffer
          clocktas=fltarr(num_images)+buffer.tas
          IF (*pop).format eq 'SEA' THEN BEGIN
             buffer.stoptime = buffer.time + (time_sfm[-1] - time_sfm[0])
             reftime=time_sfm[0]   ;SEA buffer time is the actual start time (tested)
          ENDIF
       END

       ((*pop).probetype eq 'CIP'): BEGIN
          x=decompress_dmt3(buffer.image)

          ;Detect good particles.  Best way I've found is to compare slice count from
          ;header line to slice count from sync detection.  Allow error up to 1.
          good1 = where((abs(x.slice_count - x.header_slice_count) le 1) and (x.slice_count gt 0), ngood1)
          IF ngood1 lt 3 THEN return, nullbuffer

          ;Good particle filter #2, look for large time excursions (100+ sec) on initial good particles.
          mediantime = median(x.time_sfm[good1])
          good2 = where(abs(x.time_sfm[good1] - mediantime) lt 100, ngood2)
          IF ngood2 lt 3 THEN return, nullbuffer

          ;Combine filters
          good3 = good1[good2]

          ;Good particle filter #3, particle_counter and time must be monotonically increasing.  Final diffcount is below.
          previouscount=[(*pmisc).lastparticlecount, x.particle_count[good3]]
          diffcount=uint(x.particle_count[good3])-uint(previouscount)   ;uint automatically takes care of rollovers at 65535
          previoustime=[(*pmisc).lastclock, x.time_sfm[good3]]
          inttime=x.time_sfm[good3]-previoustime
          ;Due to rollovers diffcount will always be positive number.  Bad ones will be ~65000.
          ;Pattern is different if counter error is high vs. low.  Just check for neighbor too and reject
          ;   if either one is >10000, this takes care of both conditions.
          IF (*pop).strictcounter eq 1 THEN BEGIN
             ;Use the counter to reject particles.  However it is often filled with noisy numbers so option to turn it off
             good4 = where((diffcount lt 10000) and ([diffcount[1:*], 0us] lt 10000) and (inttime ge 0) and ([inttime[1:*], 0] ge 0), ngood4)
          ENDIF ELSE BEGIN
             ;Less strict version, doesn't care about particle counter, just interarrival time
             good4 = where((inttime ge 0) and ([inttime[1:*], 0] ge 0), ngood4)
          ENDELSE
          IF ngood4 lt 3 THEN return, nullbuffer

          ;Combine filters
          good = good3[good4]
          num_images = n_elements(good)

          startline=x.sync_ind[good] + 2   ;Skip over sync and time
          stopline=startline+x.slice_count[good]-1   ;

          ;NOTE!  The first particle's time seems to match best with PREVIOUS buffer time.
          ;If clocks drift there can be problems.  They are not accounted for here yet.
          ;truetime=x.time_sfm+((*pop).timeoffset)
          ;truetime=buffer.time + (x.time_sfm[0:num_images-1] - x.time_sfm[num_images]) + (*pop).timeoffset
          ;print,buffer.time, min(truetime), max(truetime), num_images, max(x.particle_count)-min(x.particle_count)
          time_sfm=x.time_sfm[good]
          reftime=time_sfm[num_images-1]  ;This is the time that -should- match the buffer time
          rawtime=time_sfm                ;No difference for CIP/PIP
          dof=x.dof[good]
          particle_count=x.particle_count[good]
          bitimage=x.bitimage
          bitimage[0:63,x.sync_ind]=0   ;eliminate sync lines
          bitimage[0:63,x.sync_ind+1]=0 ;eliminate time lines
          restore_slice=0

          previouscount=[(*pmisc).lastparticlecount, particle_count]
          diffcount=uint(particle_count)-uint(previouscount)   ;uint automatically takes care of rollovers at 65535
          missed=diffcount-1
          (*pmisc).lastparticlecount=particle_count[num_images-1]

          previoustime=[(*pmisc).lastclock, time_sfm]
          inttime=rawtime-previoustime
          (*pmisc).lastclock=time_sfm[num_images-1]       ;Save for next buffer

          overload=byte(missed<1)         ;Flag these particles for computing dead time
          stretch=fltarr(num_images)+1.0  ;Not implemented yet for this probe, assume no stretch
          clocktas=fltarr(num_images)+buffer.tas

          ;Improve the SEA buffer stoptime, since the start/stop times in the particle headers are more accurate
          ;Added for HIWC PIP 2022 with heavy overloads between buffers
          IF (*pop).format eq 'SEA' THEN BEGIN
             buffer.stoptime = buffer.time + (time_sfm[-1] - time_sfm[0])
             reftime=time_sfm[0]   ;SEA buffer time is the actual start time (tested)
          ENDIF
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
          rollover=where(diffcount lt -10000, nrollovers)  ;Rollovers happen at 32767, have to adjust manually since not at uint limit
          IF nrollovers gt 0 THEN diffcount[rollover]+=32767
          previoustime=[(*pmisc).lastclock, x.time_sfm]
          difftime=x.time_sfm-previoustime
          greymax=intarr(n_elements(x.time_sfm))
          ;Eliminate particles that don't have at least 1 level-2 pixel.  Iterating to n-2 will automatically eliminate final incomplete image.
          FOR i=0, n_elements(x.time_sfm)-2 DO greymax[i]=max(bitimage[*,startline[i]:stopline[i]])
          IF (*pop).format eq 'SEA' THEN maxdiff = 5000 ELSE maxdiff = 50  ;Allow a greater counter diff for SEA due to missing buffers

          good=where((diffcount gt 0) and (diffcount lt maxdiff) and (difftime gt 0) and $
                     (x.slice_count ne 0) and (x.slice_count lt 150) and $
                     (greymax ge (*pop).greythresh) and (x.time_sfm gt 0),num_images)
          IF num_images lt 4 THEN return, nullbuffer  ; This is a bad buffer after num_images updated
          startline=startline[good]
          stopline=stopline[good]
          time_sfm=x.time_sfm[good]
          rawtime=time_sfm  ;No difference
          ;Reform previous time to use only good particles
          previoustime=[(*pmisc).lastclock, x.time_sfm[good]]
          inttime=x.time_sfm[good]-previoustime
          particle_count=x.particle_count[good]
          dof=(greymax[good]-2)>0  ;Should only flag particles with a level-3 pixel
          ;Implement Mode3 here?  Need to get N75 and N50 first, maybe better later

          ;Need to reform previouscount now to only use good particles
          previouscount=[(*pmisc).lastparticlecount, x.particle_count[good]]
          diffcount=x.particle_count[good]-previouscount
          rollover=where(diffcount lt -10000, nrollovers)  ;Rollovers happen at 32767, have to adjust manually since not at uint limit
          IF nrollovers gt 0 THEN diffcount[rollover]+=32767
          missed=diffcount-1
          overload=byte(missed<1)         ;Flag these particles for computing dead time
          (*pmisc).lastparticlecount=particle_count[num_images-1]
          (*pmisc).lastclock=time_sfm[num_images-1]

          reftime=time_sfm[num_images-1]  ;This is the time that -should- match the buffer time
          restore_slice=0
          stretch=fltarr(num_images)+1.0  ;Not implemented yet for this probe, assume no stretch
          ;inttime=dblarr(num_images)   ;Not yet implemented for this probe
          clocktas=fltarr(num_images)+buffer.tas
       END

       (((*pop).probetype eq 'HVPS3') or ((*pop).probetype eq '2DS') or ((*pop).probetype eq '3VCPI') or ((*pop).probetype eq 'HVPS4')): BEGIN
          ;Decompress the images in this buffer
          num_images=(*pmisc).nimages
          IF num_images eq 0 THEN return, nullbuffer
          bitimage=bytarr(128,1)
          startline=intarr(num_images)
          stopline=intarr(num_images)
          inttime=dblarr(num_images)
          overload=bytarr(num_images)
          particle_count=uintarr(num_images)
          rawtime=ulonarr(num_images)       ;Slice counter straight from the data
          clocktas=fltarr(num_images)       ;From the HK data, to get actual clocking speed, important for computing dead time
          stretch=fltarr(num_images)+1.0    ;Stretching factor for yres
          slicecount=1  ;dummy first slice
          ;freq=double((*pop).res/(1.0e6*buffer.tas))  ; the time interval of each tick in a timeline
          c=0   ;keep an actual count since there are some bad particles in there that will be skipped
          lastclock=(*pmisc).lastclock
          lasthkpointer=-1   ;Keep track of HK pointers to avoid reading the same one over and over
          FOR i=0,num_images-1 DO BEGIN
            ;First read the associated housekeeping buffer to get TAS
            IF lasthkpointer ne (*pmisc).hkpointers[i] THEN hk=spec_read_hk(1,(*pmisc).hkpointers[i])
            lasthkpointer=(*pmisc).hkpointers[i]  ;Keep track of last one
            ;HK buffers for 3VCPI/Hawkeye/HVPS4 does not contain TAS, need external house data which has been placed into *pmisc
            IF ((*pop).subformat ne 0) THEN hk.tas=(*pmisc).probetas

            ;Read images
            CASE (*pop).subformat OF
               0: x=spec_read_frame(1,(*pmisc).imagepointers[i],(*pop).probeid)
               1: x=tvcpi_read_frame(1,(*pmisc).imagepointers[i],(*pop).probeid)  ;Also Fast2DS
               2: x=hvps4_read_frame(1,(*pmisc).imagepointers[i],(*pop).probeid)
            ENDCASE
            IF x.error eq 0 THEN BEGIN
               bitimage=[[bitimage],[x.image]]
               startline[c]=slicecount
               slicecount=slicecount+(size(x.image,/dim))[1]
               stopline[c]=slicecount-1
               overload[c]=x.overload
               particle_count[c]=x.particlecount
               rawtime[c]=x.time
               clocktas[c]=hk.tas
               ;Compute stretching factor, keep between reasonable values 0.1 to 10
               IF ((*pmisc).aircrafttas gt 0) and (hk.tas gt 0) and ((*pop).stretchcorrect eq 1) THEN $
                  stretch[c]=((*pmisc).aircrafttas/hk.tas) > 0.1 < 10.0
               freq=double((*pop).res/(1.0e6*clocktas[c]))  ; the time interval of each tick in a timeline
               ;Special case for Hawkeye, horizontal 50um array uses the 10um clock on the V-array, not a 50um clock
               IF ((*pop).probetype eq '3VCPI') and ((*pop).res eq 50) THEN freq=double(10.0/(1.0e6*clocktas[c]))
               ;Special case for HVPS-4, horizontal 150um array uses the 50um clock, not a 150um clock
               IF ((*pop).probetype eq 'HVPS4') and ((*pop).res eq 150) THEN freq=double(50.0/(1.0e6*clocktas[c]))
               ;Special case for HVPS-3
               IF ((*pop).probetype eq 'HVPS3') THEN BEGIN
                  x.time=x.timetrunc  ;HVPS has some timing errors, use truncated version.
                  rollovervalue=65536
               ENDIF ELSE rollovervalue=4294967295d
               ;Check for clock rollovers over at 65536 (every 0.1 seconds at 100 m/s)
               IF x.time lt lastclock THEN inttime[c]=((x.time+rollovervalue)-lastclock)*freq ELSE $
                    inttime[c]=(x.time-lastclock)*freq
               lastclock=x.time
               c=c+1
            ENDIF
          ENDFOR
          (*pmisc).lastclock=lastclock       ;Save for next buffer
          num_images=c   ;update for sizing below
          overload=overload[0:c-1]  ;update size
          particle_count=particle_count[0:c-1]
          rawtime=rawtime[0:c-1]
          stretch=stretch[0:c-1]
          inttime=inttime[0:c-1]
          clocktas=clocktas[0:c-1]

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

          ;Count forward for subsequent buffers.
          ;Still has problems since buffer times are not consistent and smooth.
          ;This doesn't really matter for now, will overwrite in soda2_particlesort.
          IF buffer.time eq (*pmisc).lastbufftime THEN BEGIN
             time_sfm[0]=(*pmisc).maxsfm
             FOR i=1,c-1 DO time_sfm[i]=time_sfm[i-1]+inttime[i]
          ENDIF

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

       ((*pop).probetype eq 'TXT'): BEGIN
          ;For processing simulations without a native format
          num_images=n_elements(buffer.particletime)
          time_sfm=buffer.particletime
          rawtime=time_sfm
          reftime=buffer.time
          bitimage=buffer.image
          startline=buffer.startslice
          stopline=buffer.startslice+buffer.nslices-1
          particle_count=intarr(num_images)  ;No counter
          dof=bytarr(num_images)+1  ;Assume all are good to start
          ;Figure out dofreject for greyscale or 1D2D criteria
          IF (*pop).dofreject ne 0 THEN BEGIN
             FOR i=0,num_images-1 DO BEGIN
                dummy=where(bitimage[*, startline[i]:stopline[i]] ge 2, n50)
                dummy=where(bitimage[*, startline[i]:stopline[i]] eq 3, n75)
                IF (*pop).dofreject eq 1 THEN IF n75 eq 0 THEN dof[i]=0  ;No level-3 pixels
                IF (*pop).dofreject eq 2 THEN IF float(n75)/n50 lt 0.5 THEN dof[i]=0  ;"Mode3" for SEA-1D2D

                ;Set greythresh if it is not already, can be missed easily in the TXT data
                IF (n50 gt 0) and ((*pop).greythresh eq 0) THEN (*pop).greythresh=1
             ENDFOR
          ENDIF
          stretch=fltarr(num_images)+1.0  ;Not implemented yet for this probe, assume no stretch
          inttime=dblarr(num_images)   ;Not yet implemented for this probe
          clocktas=fltarr(num_images)+buffer.tas
          restore_slice=0  ;May want to change for certain tests
          missed=0
       END

       ((*pop).probetype eq '1D2D'): BEGIN
          ;Make sure last slice is full alternating on/off pixels
          IF buffer.image[-1] ne ulong64('AAAAAAAAAAAAAAAA'x) THEN print, 'Buffer may be misaligned'

          ;Particle headers use 3 slices, should come in triplicate
          ;First conditional matches '55'x pattern.  Second conditional is to avoid error where '55'x exists with blank slice afterward.
          sync_ind = where(((buffer.image and 'FF00000000000000'x) eq ulong64('5500000000000000'x)) and (buffer.image ne '55FFFFFFFFFFFFFF'x), nsync)
          IF (nsync mod 3) ne 0 THEN return, nullbuffer    ;Uneven number of sync slices, can happen when a particle makes the '55'x pattern
          IF nsync lt 15 THEN return, nullbuffer  ;Reject buffers that are filled with noise and have few timelines
          num_timelines = nsync/3

          ;Decode time and particle information
          ;First index of each triplet, contains the time of the particle
          i = indgen(num_timelines) * 3
          timecounter = (buffer.image[sync_ind[i]] and '0000FFFFFFFFFFFF'x)

          ;Second timeline contains probe settings, should usually be the same for all particles
          secondtimeline = buffer.image[sync_ind[i+1]]
          largereject = ishft(secondtimeline and '00FFF00000000000'x, -44)  ;Maximum number of slices, otherwise reject
          smallreject = ishft(secondtimeline and '00000FFF00000000'x, -32)  ;Minimum number of slices, otherwise reject
          dofpercent  = ishft(secondtimeline and '000000003C000000'x, -26)  ;Indicates which dof% setting is used, see manual
          leftreject  = ishft(secondtimeline and '0000000000FFF000'x, -12)  ;Number of slices required on left edge to reject
          rightreject = secondtimeline and '0000000000000FFF'x    ;Number of slices required on right edge to reject

          ;Third timeline contains number of pixels at 50 and 75 percent shading, as well as the dofnumreject setting
          thirdtimeline = buffer.image[sync_ind[i+2]]
          dofnumreject = ishft(thirdtimeline and '00FFF00000000000'x, -44)  ;Number of pixels required at 75% to accept
          pixels75     = ishft(thirdtimeline and '000003FFFF000000'x, -24)
          pixels50     = thirdtimeline and '000000000003FFFF'x

          ;Get particle times
          time = timecounter * double(1e-8)  ;Clock is straight 100MHz from last 48 bits
          bufferstarttime = time[0]   ;By definition first two timelines contain buffer start/stop
          bufferstoptime = time[1]
          num_images = num_timelines-2
          time_sfm = time[2:*]        ;Particles start on third timeline
          rawtime = timecounter[2:*]
          inttime = time_sfm - [bufferstarttime, time_sfm]  ;Use buffer start time as the first time to use (may not be exactly right)
          reftime = time[-1]    ;The time that should match the starttime on the SEA buffer

          ;Update the SEA buffer stoptime, since the start/stop times in the particle headers are more accurate
          buffer.stoptime = buffer.time + (time[-1] - time[0])

          ;Make image
          image = buffer.image
          image[sync_ind] = 'FFFFFFFFFFFFFFFF'x    ;Replace timelines with blank(1) data
          bitimage = dec2bin64(not(image))

          ;Start/stop indexes of image, starting with third timeline
          ;Note: The timelines in 1D2D come AFTER the particle itself, so will always start with line 6
          startline = [6, (sync_ind[i[2:num_timelines-2]]+3)]   ;Don't use the last timeline as a startline, since only empty space after
          stopline  = sync_ind[i[2:num_timelines-1]]-1

          ;Misc
          restore_slice = 0
          missed = 0
          particle_count = intarr(num_images) ;No counter
          dof = pixels75[2:*] < 1             ;Set flag to 'accept' if at least one 75% pixel, flag must be 0 or 1
          IF (*pop).dofreject eq 2 THEN BEGIN
             ;Stricter DoF for N75/N50 > 0.5, aka Mode3.  This forces Mode3 even if data recorded in Mode1 or Mode2.
             n75 = pixels75[2:*]
             n50 = pixels50[2:*]  ;this can sometimes be slightly different than computed area, but ignore for now
             dof = bytarr(num_images)+1
             FOR i=0,num_images-1 DO IF float(n75[i])/n50[i] lt 0.5 THEN dof[i]=0
          ENDIF

          stretch = fltarr(num_images)+1.0    ;Not implemented yet for this probe, assume no stretch
          clocktas = fltarr(num_images)+buffer.tas
       END

       ((*pop).probetype eq 'HVPS1'): BEGIN
          ;Adapted from SODA-1 code for the SDSMT HVPS
          ;There are at least 3 different raw formats, use year to distinguish
          year = fix(strmid((*pop).date, 4, 4))
          CASE 1 OF
             year le 1999: x=decompress_hvps(buffer.image)  ;Not yet tested, see get_img_1995hvps.pro in SDSMT code base.  Also hvps95_buff_times.pro for outer buffer.
             year eq 2000: x=decompress_hvps_chargeplates(buffer.image)
             year ge 2001: x=decompress_hvps(buffer.image)
          ENDCASE

          IF (x.mask eq 1) || (x.error eq 1) || (n_elements(x.time) lt 5) || (n_elements(x.image) lt 1000) THEN return, nullbuffer

          ;Take care of an error in 1995 data where the timestamps are not lined up right.  This ensures accurate
          ;separation of particles.  Use Diagnostic Plots below to test.
          IF year eq 1995 THEN x.particle_index--

          ;Particle separation
          num_images = n_elements(x.time)
          num_slices = fix((size(x.image))[2])
          startline = [0, x.particle_index[0:num_images-2]+1]
          stopline = x.particle_index < (num_slices-1) > startline
          bitimage = x.image
          inttime = x.time

          ;Highlight non-imaged parts of the array if dioderange is active
          oddslices = indgen(num_slices/2) * 2  ;Dotted line
          fp = (*pop).dioderange[0]   ;First/last pixels for ROI
          lp = (*pop).dioderange[1]
          IF fp gt 0 THEN bitimage[fp-1, oddslices] = 2
          IF lp lt (*pop).numdiodes-1 THEN bitimage[lp+1, oddslices] = 2

          ;Misc
          restore_slice = 0
          missed = 0
          particle_count = intarr(num_images) ;No counter
          ;Clock speed is 250kHz, *2 for half res on HVPS
          ;This doesn't work on 2003 data since TAS is not correctly recorded.  Just setting it to 400 for now
          ;yresfromtas = buffer.tas/250000.0 * 1.0e6 * 2
          yresfromtas = (*pop).yres  ;Results in no TAS dependency
          stretch = fltarr(num_images) + yresfromtas/(*pop).yres  ;Should work no matter what is given in *pop.yres
          clocktas = fltarr(num_images) + buffer.tas
          time_sfm = buffer.time + total(inttime, /cumulative)
          reftime = buffer.time
          rawtime = x.rawtime
          dof=bytarr(num_images)+1  ;No dof flag, assume all are good
          IF (*pop).format eq 'SEA' THEN BEGIN
             buffer.stoptime = buffer.time + x.totaltime
             reftime=time_sfm[0]   ;SEA buffer time is the actual start time (tested)
          ENDIF


          ;Diagnostic plots, mainly to check particle separation which varies from year to year
          IF 1 eq 2 THEN BEGIN
             colorimage = bitimage
             FOR i = 0, num_images-1 do begin
                roi=bitimage[*,startline[i]:stopline[i]]  ;extract a single particle from the buffer image
                color = ((i mod 4) + 1)*50
                colorimage[*,startline[i]:stopline[i]] *= color
             ENDFOR
             tv, bytarr(1200,1200)
             sss=size(colorimage, /dim)
             tv, congrid(colorimage, sss[0]*2, sss[1]*2)
             tv, x.image*80 + 20, 600, 0
             stop
          ENDIF
          ;IF max(x.area) gt 10 then stop
       END

       ((*pop).probetype eq 'HAIL'): BEGIN
          x = decompress_hail(buffer.image)

          ;The images tend to have lots of breaks in them, try a few strategies to pull particles together
          ;xgfb appears to work best on Flt729.  Try others.
          ;xb = label_blobs(x, /int)                            ;No correction
          ;xbd = label_blobs(x, dilate=2, /int)                 ;Blob with dilation, works well
          xgf = (convol(fix(x), [[0,1,0], [0,2,0], [0,1,0]], /edge_truncate) - 1) > 0 < 1  ;Horizontal gap fill, then label
          xgfb = label_blobs(xgf, dilate=1, /int)

          ;Diagnostics
          ;erase
          ;tv, x*100+10
          ;tv, xb*20+10, 100, 0
          ;tv, xbd*20+10, 200, 0
          ;tv, xgfb*20+10, 300, 0
          ;wait, 0.2

          ;Find dividers between particles.  This is tricky
          blobimage = xgfb
          bitimage = xgfb < 1
          num_images = max(blobimage)
          startline = intarr(num_images)  ;All zero, separation done below by blobs
          stopline = intarr(num_images)   ;All zero, separation done below by blobs
          elapsedtime = buffer.time - (*pmisc).lastbufftime

          ;Spread time out evenly between last buffer time and current buffer time
          time_sfm = findgen(num_images)/num_images*elapsedtime + (*pmisc).lastbufftime
          rawtime = fltarr(num_images)   ;All zero, no data
          inttime = fltarr(num_images) + elapsedtime/num_images  ;Assume all evenly distributed in time
          reftime = buffer.time    ;UNTESTED for Hail.  The time that should match the starttime on the SEA buffer
          (*pmisc).lastbufftime=buffer.time

          ;Misc
          restore_slice = 0
          missed = 0
          dof=bytarr(num_images)+1  ;No dof flag, assume all are good
          particle_count = intarr(num_images) ;No counter
          stretch = fltarr(num_images)+1.0    ;Not implemented yet for this probe, assume no stretch
          clocktas = fltarr(num_images) + buffer.tas
          probetas = fltarr(num_images) + buffer.tas

       END

       ELSE: stop, 'Probe type not available'
   ENDCASE

   ;Code common to all probes
   sbhist=(*pmisc).lastdhist  ;Use the last time period's dhist for stuck bit checking
   bitimage_orig=bitimage
   IF (*pop).stuckbits THEN bitimage=fixstuckbits(bitimage, h=sbhist)  ;sbhist should be undefined except for SPEC probes, defaulting to current buffer
   IF (*pop).rakefix gt 0 THEN bitimage=fixraking(bitimage, (*pop).rakefix)
   diodetotal=total(bitimage,2)  ;Make sure bitimage is free of time/sync lines
   IF max(diodetotal)/mean(diodetotal) gt 3 THEN streak=1 ELSE streak=0

   diam=fltarr(num_images)
   xsize=fltarr(num_images)
   ysize=fltarr(num_images)
   xextent=fltarr(num_images)
   areasize=fltarr(num_images)
   oned=fltarr(num_images)
   twod=fltarr(num_images)
   allin=bytarr(num_images)
   centerin=bytarr(num_images)
   edgetouch=bytarr(num_images)
   arearatio=fltarr(num_images)
   arearatiofilled=fltarr(num_images)
   aspr=fltarr(num_images)
   orientation=fltarr(num_images)
   area_orig=fltarr(num_images)
   area_filled=fltarr(num_images)
   perimeterarea=fltarr(num_images)  ;Number of pixels on the border, for water detection
   area75=fltarr(num_images)
   IF ((*pop).probetype eq '1D2D') THEN area75=pixels75[2:*]  ;Use directly from probe since not available in images
   zd=fltarr(num_images)
   sizecorrection=fltarr(num_images) + 1.0
   xpos=fltarr(num_images)
   ypos=fltarr(num_images)
   nsep=intarr(num_images)
   dhist=intarr((*pop).numdiodes)

   ;Boolean flag for SPEC probes indicates empty particle with overload time, set to 0 if not already defined.
   IF n_elements(overload) gt 0 THEN overloadflag=overload ELSE overloadflag=bytarr(num_images)
   nslices=0  ;Number of image slices (no timelines, sync, etc)
   fp = (*pop).dioderange[0]   ;First/last pixels for ROI
   lp = (*pop).dioderange[1]
   IF lp eq 0 THEN lp = (*pop).numdiodes-1
   FOR i=0,num_images-1 DO BEGIN
      roi=bitimage[fp:lp, startline[i]:stopline[i]]  ;extract a single particle from the buffer image
      roi_orig=bitimage_orig[fp:lp, startline[i]:stopline[i]]  ;the original bitimage preserving stuck bits
      IF ((*pop).probetype eq 'HAIL') THEN BEGIN
         ;Hail spectrometer has no timelines, only blobs.  Handle that here by separating indexed blobs.
         roi = byte(blobimage*0)
         w = where(blobimage eq i+1)
         roi[w] = 1
         roi_orig = roi
      ENDIF

      ;Adjust the particle for grey probes to the right threshold
      IF (*pop).greythresh gt 0 THEN BEGIN
         ;Quickly compute area75 on level 3
         thresh=2
         roi75=((roi > thresh)-thresh)<1
         area75[i]=total(roi75)

         ;Get final roi for sizing
         thresh=(*pop).greythresh-1     ;makes next line easier
         roi=((roi > thresh)-thresh)<1  ;turn all pixels above threshold to 1
         roi_orig=((roi_orig > thresh)-thresh)<1  ;same for version preserving stuck bits
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

      IF roilen eq 1 THEN roihist=roi_orig ELSE roihist=total(roi_orig,2)   ;For housekeeping, keeping stuck bits using roi_orig
      dhist[fp:lp] += roihist

      ;Find the number of unshaded diodes within the boundary of each particle.  For detecting double particles.
      w=where(roihist gt 0, nw)
      xspan=max(w)-min(w)+1
      nsep[i]=xspan-nw

      ;Water processing
      zeed=0
      area_orig[i]=total(roi)
      roifilled=fillholes(roi)     ;fills in poisson spots for liquid water drops, +40% processing time
      area_filled[i]=total(roifilled)
      sizecorrection[i] = poisson_spot_correct(area_orig[i], area_filled[i], zd=zeed) ; as in Korolev 2007

      ;Find particle metrics and fill arrays
      part=soda2_findsize(roi,(*pop).res, (*pmisc).yres * stretch[i], voidarea=area_filled[i]-area_orig[i])
      diam[i]=part.diam
      xsize[i]=part.xsize
      ysize[i]=part.ysize
      xextent[i]=part.xextent
      oned[i]=part.oned
      twod[i]=part.twod
      areasize[i]=part.areasize
      arearatio[i]=part.ar
      arearatiofilled[i]=part.arfilled
      aspr[i]=part.aspr
      allin[i]=part.allin
      centerin[i]=part.centerin
      edgetouch[i]=part.edgetouch
      zd[i]=zeed
      xpos[i]=part.c[0]
      ypos[i]=part.c[1]
      orientation[i]=part.orientation
      perimeterarea[i]=part.perimeterarea
   ENDFOR   ;image loop end

   return,{diam:diam, xsize:xsize, ysize:ysize, xextent:xextent, areasize:areasize, oned:oned, twod:twod, $
           probetime:time_sfm, reftime:reftime, rawtime:rawtime, ar:arearatio, arfilled:arearatiofilled, aspr:aspr, $
           rejectbuffer:0, bitimage:bitimage, allin:allin, centerin:centerin, streak:streak, zd:zd, $
           sizecorrection:sizecorrection, dhist:dhist, nslices:nslices, missed:missed, nsep:nsep, $
           overloadflag:overloadflag, dofflag:dof, xpos:xpos, ypos:ypos, orientation:orientation, area_orig:area_orig, $
           area_filled:area_filled, perimeterarea:perimeterarea, area75:area75, particlecounter:particle_count, $
           edgetouch:edgetouch, inttime:inttime, clocktas:clocktas, startline:startline, stopline:stopline}
END
