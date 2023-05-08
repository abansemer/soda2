PRO soda2_imagedump, file, outdir=outdir, starttime=starttime, stoptime=stoptime, $
              all=all, skip=skip, showdividers=showdividers, maxwidth=maxwidth, nofile=nofile,$
              textwidgetid=textwidgetid, writejunk=writejunk, writeempty=writeempty, numcolumns=numcolumns,$
              rakefixtype=rakefixtype, hourly=hourly, datestyle=datestyle, naming_convention=naming_convention, $
              version=version
   ;Make a series of particle image png files from processed OAP data.
   ;Uses the SODA2 '.dat' files to find raw data locations and pointers.
   ;File: the processed SODA2 file for flight of interest
   ;Outdir: output directory for images
   ;Starttime and stoptime: in seconds from midnight, will do entire file if none specified
   ;All:  Option to output every single image instead of just first per time period.
   ;Skip:  Modifies 'all' option to do every 'skip' buffers
   ;Maxwidth: Maximum image width to display, only valid for 'all' option.
   ;A Bansemer, NCAR, 2015
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.


   IF n_elements(file) eq 0 THEN return   ;Just a simple return with no file, so it can be inserted in soda2.pro for packaging
   IF n_elements(all) eq 0 THEN all=0
   IF n_elements(showdividers) eq 0 THEN showdividers=0
   IF n_elements(outdir) eq 0 THEN outdir=''
   IF n_elements(maxwidth) eq 0 THEN maxwidth=1024
   IF n_elements(nofile) eq 0 THEN nofile=0
   IF n_elements(textwidgetid) eq 0 THEN textwidgetid=0
   IF n_elements(writejunk) eq 0 THEN writejunk=1
   IF n_elements(writeempty) eq 0 THEN writeempty=0
   IF n_elements(numcolumns) eq 0 THEN numcolumns=2
   IF n_elements(hourly) eq 0 THEN hourly=0
   IF n_elements(naming_convention) eq 0 THEN naming_convention='standard'
   IF n_elements(datestyle) eq 0 THEN datestyle=0  ;1 for YYYYMMDD, otherwise MMDDYYYY
   IF n_elements(version) eq 0 THEN version='v01'  ;GHRC default

   IF nofile eq 1 THEN data=file ELSE restore, file
   op=data.op
   soda2_update_op,op
   month=strmid(op.date,0,2)
   day=strmid(op.date,2,2)
   year=strmid(op.date,4,4)
   IF datestyle eq 1 THEN op.date=year+month+day

   ft=file_test(op.fn[0])
   IF (ft eq 0) THEN BEGIN
      rawdir=dialog_pickfile(/read,/directory,title='Can''t find '+op.fn[0]+', where is it?')
      IF file_test(rawdir+file_basename(op.fn[0])) eq 1 THEN op.fn=rawdir+file_basename(op.fn) ELSE stop,'No data found'
   ENDIF

   ;Need to index SPEC files for frame locations
   IF op.format eq 'SPEC' THEN BEGIN
      openr,lun,op.fn[0],/get_lun
      misc=spec_index(lun)
      pspec=ptr_new(ispec)
      free_lun, lun
   ENDIF ELSE misc={dummy:0}
   ;Keep miscellaneous stuff here, things that change during processing
   misc2={f2d_remainder:ulon64arr(512), f2d_remainder_slices:0, yres:op.res, lastbufftime:0.0, $
         nimages:0, imagepointers:lon64arr(500), lastclock:0d, lastparticlecount:0L, lastdhist:lonarr(op.numdiodes)}

   misc=create_struct(temporary(misc), misc2)  ;Join the SPEC and misc structures
   pmisc=ptr_new(misc)

   probename=op.shortname
   IF (op.probetype eq '2DS') or (op.probetype eq 'HVPS4') or (op.probetype eq '3VCPI') THEN $
      probename=op.shortname+'_'+op.probeid  ;Add 'H' or 'V' specifier
   IF (naming_convention eq 'GHRC') THEN BEGIN
      IF (op.probetype eq '2DS') THEN probename=op.shortname+op.probeid
      IF (op.probetype eq '3VCPI') THEN probename='Hawkeye2DS'+op.probeid
      IF (op.project eq 'IMPACTS') THEN probename+='-P3'  ;Add aircraft ID for both 2DS/HVPS
   ENDIF
   rate=fix(op.rate)

   ;Output images using a different rakefix than what is in the op structure (for HIWC PIP, mainly)
   IF n_elements(rakefixtype) ne 0 THEN op.rakefix=rakefixtype

   ;Set up blue, black, and white color table
   r=bytarr(6) & g=bytarr(6) & b=bytarr(6)
   r[0]=255 & g[0]=255 & b[0]=255
   r[1]=0 & g[1]=0 & b[1]=0
   r[2]=0 & g[2]=0 & b[2]=255
   r[3]=0 & g[3]=255 & b[3]=0
   r[4]=255 & g[4]=0 & b[4]=0
   r[5]=255 & g[5]=0 & b[5]=255

   ;Greyscale table for CIP-grey, extra white plus 3 blue shades
   IF op.probetype eq 'CIPG' THEN BEGIN
     r[2]=255 & g[2]=255 & b[2]=255
     r[3]=180 & g[3]=180 & b[3]=250
     r[4]=100 & g[4]=100 & b[4]=200
     r[5]=50 & g[5]=50 & b[5]=150
   ENDIF

   tvlct, rold, gold, bold, /get  ;Save current table
   tvlct,r,g,b

   ;General setup
   set_plot,'z'
   !p.charsize=1.0
   n=n_elements(data.time)

   wid=strtrim(string(long(op.res*op.numdiodes)),2)  ; buffer width (for header)

   ;Start/stop time setup
   IF n_elements(starttime) eq 0 THEN i=-1l ELSE i=max(where(data.time lt starttime[0]))
   IF all eq 0 THEN REPEAT i=i+1 UNTIL (sfm2hms(data.time[i]) mod 100) lt op.rate ;Start at the top of a minute
   starttime2=data.time[i>0]

   IF n_elements(stoptime) eq 0 THEN stoptime2=data.time[n-1] ELSE stoptime2=stoptime
   numframes=long(stoptime2-starttime2)/60 - 1

   ;Create setup for hourly files
   IF hourly ne 0 THEN BEGIN
      hourstart = fix(data.time[0]/3600)
      hourstop = fix(data.time[-1]/3600)
      numframes = hourstop-hourstart  ;+1 implicit in main FOR loop below
      rate = fix(hourly)  ;Defaults to one image per minute, but set hourly to another number for 5-minute, etc
      i = 0
      ;Refigure data.ind to put into minutes rather than 'rate' frequency
      indtime = data.op.starttime + data.ind*data.op.rate
      indminute = (indtime - hourstart*3600l)/60
      i = min(where(indminute eq 0))
   ENDIF


   ;----------------------------------------------------------------------
   ;--------------Main loop start-----------------------------------------
   ;----------------------------------------------------------------------
   numslices=700
   imwidth=numcolumns*(numslices+50)+50  ;1600
   headerheight=150 ;Pixels for header
   imheight=headerheight+(60.0/numcolumns)/rate*(op.numdiodes+7)+50 ;Total image height
   pop=ptr_new(op)

   IF all eq 0 THEN BEGIN
      emptyimage=bytarr(op.numdiodes,numslices)
      FOR minute=0,numframes DO BEGIN
         IF hourly eq 0 THEN imagetime=string(sfm2hms(data.time[i]),form='(i06)')   ;Normal minute files
         IF hourly ne 0 THEN imagetime=string(sfm2hms((hourstart+minute)*3600l),form='(i06)')  ;Minute here is really hours
         IF textwidgetid ne 0 THEN widget_control,textwidgetid,set_value=imagetime,/append ELSE print,imagetime
         gotimage=0                          ;flag to write only if there are some images.
         device,/close
         device,set_resolution=[imwidth,imheight]

         ;-------Write image header-------
         xyouts,20,imheight-30, op.date+' '+imagetime+'  Buffer width = '+wid+' microns.',/device,color=1
         xyouts,20,imheight-50, 'Project: '+op.project+'  Probe: '+probename+ '   Resolution: '+strtrim(string(op.res),2)+' microns',/device,color=1
         IF hourly ne 0 THEN xyouts,50,imheight-70, 'This image represents one hour of flight time, one panel every '+strtrim(string(rate),2)+' minutes.',/device,color=1 ELSE $
            xyouts,50,imheight-70, 'This image represents one minute of flight time, one panel every '+strtrim(string(rate),2)+' seconds.',/device,color=1
         xyouts,50,imheight-90, 'Many more images are not shown.  Contact PI for complete imagery.',/device,color=1
         ;xyouts,50,imheight-110,'Contacts: Andy Heymsfield (heyms1@ncar.ucar.edu ) or  Aaron Bansemer (bansemer@ucar.edu)',/device,color=1

         numaccepted = 0 ;Keep track of the number of accepted particles to avoid writing junk

         ;------ Loop for each sub-image -------
         FOR sec=0,59,rate DO BEGIN
            ind=where(data.ind eq i,buffcount)
            IF hourly ne 0 THEN ind=where(indminute eq minute*60l+sec, buffcount)  ;Minutes are really hours, seconds are really minutes

            IF buffcount gt 0 THEN BEGIN
               (*pmisc).lastdhist=data.dhist[(i-1)>0,*]
               buff=soda2_bitimage(op.fn[data.currentfile[ind[0]]], data.pointer[ind[0]], pop, pmisc, divider=showdividers)
               finalimage=buff.bitimage
               ;Make sure there are at least 2 slices, if not just use empty image
               IF n_elements(finalimage) gt op.numdiodes THEN gotimage=1 ELSE finalimage = emptyimage
            ENDIF ELSE finalimage = emptyimage

            ;-----Placement details----------
            xpos=(sec/rate mod numcolumns)*(numslices+50) + 50
            ypos=imheight-headerheight-20-op.numdiodes-(sec/rate/numcolumns)*(op.numdiodes+7)
            s=size(finalimage,/dim)
            im2=rotate(finalimage[*,0:(numslices-1)<(s[1]-1)],3)
            ;Place time and image
            xyouts,xpos-20,ypos+op.numdiodes/2,strtrim(string(sec),2),/device,color=1
            tv,bytarr(numslices+2,op.numdiodes+2)+1,xpos-1,ypos-1  ;Black outline
            tv,bytarr(numslices,op.numdiodes),xpos,ypos            ;White center
            IF op.probetype eq 'CIPG' THEN im2=im2+2    ;Shift for color table
            tv,im2,xpos,ypos

            IF hourly eq 0 THEN numaccepted += data.count_accepted[i]  ;Doesn't always work when hourly turned on
            i++
         ENDFOR

         ;-------Write the image---------
         pngfile=op.date+'_'+imagetime+'_'+probename+'.png'
         IF naming_convention eq 'GHRC' THEN BEGIN
            pngfile=op.project+'_'+probename+'_'+year+month+day+'-'+imagetime+'_images_'+version+'.png'
         ENDIF
         IF (writejunk eq 0) and (numaccepted eq 0) THEN gotimage = 0  ;Cull bad images
         IF (gotimage eq 1) or (writeempty eq 1) THEN write_png,outdir+pngfile,tvrd(),r,g,b
      ENDFOR

   ;----------------------------------------------------------------------
   ;----------------MAIN Program for 'ALL' images-------------------------
   ;----------------------------------------------------------------------
   ENDIF ELSE BEGIN ;Do all images instead
      IF n_elements(starttime) eq 0 THEN BEGIN
         framestart=0L
      ENDIF ELSE BEGIN
         istart=max(where(data.time lt starttime[0]))
         frames=max(where(data.ind lt istart))+1
         framestart=long(frames[0])>0L
      ENDELSE

      IF n_elements(stoptime) eq 0 THEN BEGIN
         framestop=n_elements(data.ind)-1
      ENDIF ELSE BEGIN
         istop=min(where(data.time gt stoptime[0],ni))
         IF ni eq 0 THEN istop = n-1
         frames=min(where(data.ind gt istop,nframes))-1
         framestop=frames[0]
         IF nframes eq 0 THEN framestop=n_elements(data.ind)-1
      ENDELSE

      IF n_elements(skip) eq 0 THEN skip=1

      panelcount=0
      npanels=20  ;Number of panels per image
      pad=10
      headerheight=50
      imheight=(op.numdiodes+pad)*npanels + headerheight+ 40
      imwidth=maxwidth+75
      maxind=n_elements(data.time)-1
      device,set_resolution=[imwidth,imheight]
      timecharsize=1.0
      IF op.numdiodes eq 64 THEN timecharsize=0.8
      remainder=0  ;flag for long buffers

      FOR i=framestart,framestop,skip DO BEGIN
         ;(*pmisc).lastdhist=data.dhist[(???)>0,*]  ;For stuck bits, not yet implemented, need to figure out the index for data.dhist
         buff=soda2_bitimage(op.fn[data.currentfile[i]], data.pointer[i], pop, pmisc, divider=showdividers)
         finalimage=buff.bitimage

         IF max(finalimage) gt 0 THEN BEGIN ;skip empty images
            im2=rotate(finalimage,3)
            s=size(im2,/dim)
            IF s[0] gt maxwidth THEN BEGIN
               remainder=1
               ;Skipping output of remainder for now, getting complicated...
               ;remainderimage=im2[maxwidth:*,*] ;Save rest of image
               im2=im2[0:maxwidth-1,*]   ;Crop current image
               IF max(finalimage) eq 0 THEN remainder=0  ;Ignore empty remainders
               remainderstring='+'+strtrim(string(s[0]-maxwidth),2)+' slices'
            ENDIF ELSE remainder=0
            xpos=30
            ypos=(op.numdiodes+pad)*(npanels-panelcount-1)+pad
            paneltime=string(sfm2hms(buff.time)+buff.time-long(buff.time),form='(f09.2)')  ;Convert to hhmmss, keeping fraction
            xyouts,xpos-10,ypos+op.numdiodes/2,paneltime,/device,color=1,orient=90,align=0.5,charsize=timecharsize
            IF remainder THEN xyouts,xpos+maxwidth+15,ypos+op.numdiodes/2,remainderstring,/device,color=1,orient=90,align=0.5,charsize=timecharsize
            tv,bytarr((s[0]<maxwidth)+2,s[1]+2)+1,xpos-1,ypos-1
            IF op.probetype eq 'CIPG' THEN im2=im2+2    ;Shift for color table
            tv,im2,xpos,ypos
            IF (panelcount eq 0) THEN imagetime=paneltime
            panelcount=panelcount+1
            IF ((panelcount mod npanels) eq 0) or (i ge framestop) THEN BEGIN
               ;-------Write image header-------
               xyouts,20,imheight-30, op.date+' '+imagetime+'  Buffer width = '+wid+' microns.',/device,color=1
               xyouts,20,imheight-50, 'Project: '+op.project+'  Probe: '+probename+ '   Resolution: '+strtrim(string(op.res),2)+' microns',/device,color=1
               ;xyouts,50,imheight-110,'Contacts: Andy Heymsfield (heyms1@ncar.ucar.edu ) or  Aaron Bansemer (bansemer@ucar.edu)',/device,color=1

               ;-------Write the image---------
               pngfile=op.date+'_'+imagetime+'_'+probename+'.png'
               IF naming_convention eq 'GHRC' THEN BEGIN
                  pngfile=op.project+'_'+probename+'_'+year+month+day+'-'+imagetime+'_images_'+version+'.png'
               ENDIF
               write_png,outdir+pngfile,tvrd(),r,g,b

               device,/close
               device,set_resolution=[imwidth,imheight]
               panelcount=0
               IF textwidgetid ne 0 THEN widget_control,textwidgetid,set_value=imagetime,/append ELSE print,imagetime
            ENDIF
         ENDIF
      ENDFOR
   ENDELSE

   IF op.format eq 'SPEC' THEN ptr_free,pspec,pmisc

   close,1
   device,/close
   tvlct, rold, gold, bold  ;Restore original color table
   IF !version.os_family eq 'Windows' THEN set_plot,'win' ELSE set_plot,'x'
END
