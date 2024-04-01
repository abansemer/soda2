PRO soda2_event, ev
   ;Event handler for SODA GUI
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

    uname=widget_info(ev.id,/uname)
    widget_control,widget_info(ev.top,find='base'),get_uvalue=pinfo
    CASE uname OF
        'loadfile': BEGIN ;===========================================================================
            a=dialog_pickfile(/read,filter=['*.dat'],path=(*pinfo).datpath,dialog_parent=widget_info(ev.top,find='process'))
            IF file_test(a) eq 0 THEN return
            (*pinfo).datpath=file_dirname(a)
            restore,a
            op=data.op
            soda2_update_op,op

            ;--------Fill in boxes
            widget_control,widget_info(ev.top,find='timeoffset'),set_value=op.timeoffset
            widget_control,widget_info(ev.top,find='starttime'),set_value=string(sfm2hms(op.starttime), format='(i06)')
            widget_control,widget_info(ev.top,find='stoptime'),set_value=string(sfm2hms(op.stoptime), format='(i06)')
            widget_control,widget_info(ev.top,find='rate'),set_value=op.rate
            widget_control,widget_info(ev.top,find='xres'),set_value=op.res
            widget_control,widget_info(ev.top,find='yres'),set_value=op.yres
            widget_control,widget_info(ev.top,find='dofconst'),set_value=op.dofconst
            widget_control,widget_info(ev.top,find='date'),set_value=op.date
            widget_control,widget_info(ev.top,find='project'),set_value=op.project
            widget_control,widget_info(ev.top,find='seatag'),set_value=string(op.seatag,form='(3(i0," "))')
            tagid=widget_info(ev.top,find='tagbase')  ;Toggle SEA tag sensitivity
            IF op.format eq 'SEA' THEN widget_control,tagid,sensitive=1 ELSE widget_control,tagid,sensitive=0


            ;--------TAS stuff
            widget_control,widget_info(ev.top,find='tas'),set_value=op.fixedtas
            widget_control,widget_info(ev.top,find='pthfile'),set_value=op.pth
            widget_control,widget_info(ev.top,find='tascheckbox'),set_value=op.stretchcorrect

            ;--------Checkboxes
            id=widget_info(ev.top,find='options')
            widget_control,id,get_uvalue=values
            checkboxarray = intarr(n_elements(values))  ;Initialize all options with zero
            IF total(where(tag_names(op) eq 'RECONSTRUCT')) ne -1 THEN IF op.reconstruct eq 0 THEN checkboxarray[where(values eq 'All-In')]=1
            IF total(where(tag_names(op) eq 'EAWMETHOD')) ne -1 THEN IF op.eawmethod eq 'allin' THEN checkboxarray[where(values eq 'All-In')]=1
            IF total(where(tag_names(op) eq 'INTTIME_REJECT')) ne -1 THEN IF op.inttime_reject eq 1 THEN checkboxarray[where(values eq 'Shatter Correct')]=1
            ;SODA-1 compatibility:
            IF total(where(tag_names(op) eq 'TIMEREJECT')) ne -1 THEN IF op.timereject eq 'variable' THEN checkboxarray[where(values eq 'Shatter Correct')]=1
            IF op.water eq 1 THEN checkboxarray[where(values eq 'Water Processing')]=1
            IF op.stuckbits eq 1 THEN checkboxarray[where(values eq 'Stuck Bit Correct')]=1
            IF op.juelichfilter eq 1 THEN checkboxarray[where(values eq 'Pixel Noise Filter')]=1
            IF op.keeplargest eq 1 THEN checkboxarray[where(values eq 'Largest Particle')]=1
            IF op.apply_psc eq 1 THEN checkboxarray[where(values eq 'Force PSC')]=1
            IF total(op.customdof) gt 0 THEN checkboxarray[where(values eq 'Custom DoF Curve')]=1
            widget_control,id,set_value=checkboxarray

            ;--------Output checkboxes
            checkboxarray=[0,0,0,0]
            id=widget_info(ev.top,find='outputflags')
            widget_control,id,get_uvalue=values
            IF op.savfile THEN checkboxarray[where(values eq 'IDL(sav)')]=1
            IF op.asciipsdfile THEN checkboxarray[where(values eq 'PSD(ASCII)')]=1
            IF op.ncdfparticlefile THEN checkboxarray[where(values eq 'ParticleFile(netCDF)')]=1
            IF op.particlefile THEN checkboxarray[where(values eq 'ParticleFile(CSV)')]=1
            widget_control,id,set_value=checkboxarray

            ;--------Filenames
            widget_control,widget_info(ev.top,find='filelist'),set_value=op.fn
            widget_control,widget_info(ev.top,find='outdir'),set_value=op.outdir
            widget_control,widget_info(ev.top,find='filetag'),set_value=op.filetag

            ;--------Bins
            widget_control,widget_info(ev.top,find='endbins'),set_value=string(op.endbins,form='(100(f0.1," "))')

            ;--------Size method
            id=widget_info(ev.top,find='sizemethod')
            widget_control,id,get_value=methods  ;See main routine below for values to compare with CASE here
            CASE op.smethod OF
               'fastcircle':w=where(strmid(methods,0,1) eq 'C')
               'xsize':w=where(strmid(methods,0,1) eq 'X')
               'ysize':w=where(strmid(methods,0,1) eq 'Y')
               'areasize':w=where(strmid(methods,0,1) eq 'A')
               'xextent':w=where(strmid(methods,0,1) eq 'L')
               'oned':w=where(strmid(methods,0,1) eq '1')
               'twod':w=where(strmid(methods,0,1) eq '2')
            ENDCASE
            w=w[0]  ;Can double up when old data loaded
            widget_control,id,set_combobox_select=w

            ;--------DoF Criteria
            id=widget_info(ev.top,find='dofcriteria')
            widget_control,id,get_value=criteria  ;See main routine below for values to compare with CASE here
            CASE op.dofreject OF
               0:w=where(criteria eq 'Off')
               1:w=where(criteria eq 'One Level3 Pixel')        ;Mode2 rejection N75 > 1
               2:w=where(criteria eq '50% Level3 Pixel Ratio')  ;Mode3 rejection N75/N50 > 0.5
            ENDCASE
            widget_control,id,set_combobox_select=w[0]

            ;--------Probe type
            id=widget_info(ev.top,find='probetype')
            p=soda2_probespecs()
            w=where((p.format eq op.format) and (p.subformat eq op.subformat) and (p.probeid eq op.probeid) $
               and (p.res eq op.res) and (p.armwidth eq op.armwidth) and (p.numdiodes eq op.numdiodes) $
               and (p.probetype eq op.probetype), nw)
            IF op.format eq 'TXT' THEN w=where(p.format eq 'TXT', nw)
            IF nw eq 0 THEN BEGIN
               dummy=dialog_message('Unknown probe type in .sav file.' + string(10B) + string(10B)+ $
                  'Select new type or add probe to soda2_probespecs.pro')
            ENDIF ELSE BEGIN
               widget_control,id,set_value=['None', p.probename]
               widget_control,id,set_combobox_select=w[0]+1 ;w[0] for duplicates, +1 for "None"
               widget_control,id,set_uvalue=p[w].probename
            ENDELSE
        END


        'addfile': BEGIN ;===========================================================================
            IF ev.value eq 0 THEN BEGIN ;Add a file series pressed
                a=dialog_pickfile(/read,/multiple,path=(*pinfo).rawpath,title='Use [Ctrl] or [Shift] to Select Multiple Files',dialog_parent=widget_info(ev.top,find='process'))

                widget_control,widget_info(ev.top,find='filelist'),set_value=a
                (*pinfo).rawpath=file_dirname(a[0])
                IF !version.os_family ne 'unix' THEN widget_control,widget_info(ev.top,find='outdir'),set_value=file_dirname(a[0])+path_sep()
            ENDIF
            IF ev.value eq 1 THEN BEGIN ;Clear files pressed
                widget_control,widget_info(ev.top,find='filelist'),set_value=''
            ENDIF
        END


        'findpthfile': BEGIN ;===========================================================================
            IF ev.value eq 0 THEN BEGIN ;Add a file pressed
               a=dialog_pickfile(/read,filter=['*.dat;*.sav;*.txt;*.csv'],title='Select flight data file',dialog_parent=widget_info(ev.top,find='process'),path=(*pinfo).rawpath)
               IF file_test(a) THEN BEGIN
                  widget_control,widget_info(ev.top,find='pthfile'),set_value=a
                  widget_control,widget_info(ev.top,find='tas'),sensitive=0
               ENDIF
            ENDIF
            IF ev.value eq 1 THEN BEGIN ;Clear files pressed
                widget_control,widget_info(ev.top,find='pthfile'),set_value=''
                widget_control,widget_info(ev.top,find='tas'),sensitive=1
            ENDIF

        END

        'pthfile': BEGIN ;==========================================================================
           widget_control,widget_info(ev.top,find='pthfile'),get_value=pthfile
           IF file_test(pthfile) THEN widget_control,widget_info(ev.top,find='tas'),sensitive=0 $
           ELSE widget_control,widget_info(ev.top,find='tas'),sensitive=1
        END

        'findoutdir': BEGIN ;===========================================================================
           a=dialog_pickfile(/read,/directory,get_path=a2,title='Select output directory',dialog_parent=widget_info(ev.top,find='process'))
           IF file_test(a) THEN widget_control,widget_info(ev.top,find='outdir'),set_value=a2
        END

        'defaultbins': BEGIN ;===========================================================================
            widget_control,widget_info(ev.top,find='xres'),get_value=xres  ;Get current x-res from GUI
            IF xres lt 50 THEN endbins=[25, 50, 100, 150, 200, 250, 300, 350, 400, 500, 600,700,800,900,1000,1200,1400,1600,1800,2000]
            IF xres eq 50 THEN endbins=[25, 75, 125, 175, 250, 350, 450, 550, 650, 800, 1000, 1200, 1400, 1600, 1800, 2000, 2200, 2600, 3000, 3400, 3800, 4200, 4600, 5000, 6000, 7000, 8000, 9000, 10000]
            IF xres lt 15 THEN endbins=[5,15,25,35,45,55,65,75,85,95,105,125,145,175,225,275,325,400,475,550,625,700,800,900,1000,1200,1400,1600,1800,2000]
            IF xres gt 50 THEN endbins=[200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2200, 2600, 3000, 3400, 3800, 4200, 4600, 5000,6000,7000,8000,9000,10000,15000,20000,25000,30000]
            widget_control,widget_info(ev.top,find='endbins'),set_value=string(endbins,form='(100(i0," "))')
        END

        'fullbins': BEGIN ;===========================================================================
            id=widget_info(ev.top,find='probetype')
            widget_control,id,get_uvalue=probename
            widget_control,widget_info(ev.top,find='filelist'),get_value=fn
            probe=soda2_probespecs(name=probename, fn=fn)
            widget_control,widget_info(ev.top,find='xres'),get_value=xres  ;Get current x-res from GUI
            endbins=findgen(probe.numdiodes+1)*xres + xres/2.0
            widget_control,widget_info(ev.top,find='endbins'),set_value=string(endbins,form='(200(f0.1," "))')
        END

        'fullbinsx2': BEGIN ;===========================================================================
            id=widget_info(ev.top,find='probetype')
            widget_control,id,get_uvalue=probename
            widget_control,widget_info(ev.top,find='filelist'),get_value=fn
            probe=soda2_probespecs(name=probename, fn=fn)
            widget_control,widget_info(ev.top,find='xres'),get_value=xres  ;Get current x-res from GUI
            endbins=findgen(probe.numdiodes*2 + 1)*xres + xres/2.0
            widget_control,widget_info(ev.top,find='endbins'),set_value=string(endbins,form='(500(f0.1," "))')
        END

        'custombins': BEGIN ;===========================================================================
            id=widget_info(ev.top,find='probetype')
            widget_control,id,get_uvalue=probename
            widget_control,widget_info(ev.top,find='filelist'),get_value=fn
            probe=soda2_probespecs(name=probename, fn=fn)
            widget_control,widget_info(ev.top,find='xres'),get_value=xres  ;Get current x-res from GUI
            endbins=soda2_custombins(dummy)
            endbins=findgen(probe.numdiodes*2 + 1)*xres + xres/2.0
            widget_control,widget_info(ev.top,find='endbins'),set_value=string(endbins,form='(500(f0.1," "))')
        END

        'autofill': BEGIN ;===========================================================================
            widget_control,widget_info(ev.top,find='filelist'),get_value=fn
            widget_control,widget_info(ev.top,find='timeoffset'),get_value=timeoffset
            starttime=9999999d
            stoptime=0d
            gottime=0
            FOR i=0, n_elements(fn)-1 DO BEGIN
               ss=soda2_startstop(fn[i])
               IF ss.err eq 0 THEN BEGIN
                  starttime= starttime < ss.starttime
                  stoptime= stoptime > ss.stoptime
                  gottime=1
               ENDIF
            ENDFOR
            IF gottime THEN BEGIN
               starttime+=timeoffset/86400d  ;Adjust for the time offset
               stoptime+=timeoffset/86400d
               caldat, starttime, month, startday, year, hour, minute, second
               datestring = string(month, startday, year, format='(i02,i02,i04)')
               starttimestring = string(hour, minute, second, format='(3i02)')
               caldat, stoptime, month, stopday, year, hour, minute, second
               IF stopday gt startday THEN hour+=24  ;Midnight crossing
               stoptimestring = string(hour, minute, second, format='(3i02)')
               widget_control,widget_info(ev.top,find='date'),set_value=datestring
               widget_control,widget_info(ev.top,find='starttime'),set_value=starttimestring
               widget_control,widget_info(ev.top,find='stoptime'),set_value=stoptimestring

               ;Check for time span longer than 24 hours, probably entered wrong files
               IF (stoptime-starttime) gt 1.0 THEN dummy=dialog_message('Warning: Time span longer than 24 hours',dialog_parent=widget_info(ev.top,find='process'))

               ;Pare down the probe list
               specs=soda2_probespecs()
               w=where(specs.format eq ss.format, nw)
               IF nw gt 0 THEN probenames=['None', specs[w].probename] ELSE probenames=['None', specs.probename]

               ;Update list
               id=widget_info(ev.top,find='probetype')
               widget_control,id,get_uvalue=currentprobename
               widget_control,id,set_value=probenames    ;Use the new list
               w=where(probenames eq currentprobename)   ;Check if current probe is already on the list
               IF w ne -1 THEN widget_control,id,set_combobox_select=w   ;If so, set it to current
            ENDIF ELSE BEGIN
               ;Reset the full probe list
               specs=soda2_probespecs()
               widget_control, widget_info(ev.top,find='probetype'),set_value=['None',specs.probename]
               widget_control, widget_info(ev.top,find='probetype'),set_combobox_select=0
            ENDELSE
        END

        'process':BEGIN ;===========================================================================
            ;Collect data from the GUI
            ;--------Boxes
            widget_control,widget_info(ev.top,find='rate'),get_value=rate
            widget_control,widget_info(ev.top,find='xres'),get_value=xres
            widget_control,widget_info(ev.top,find='yres'),get_value=yres
            widget_control,widget_info(ev.top,find='dofconst'),get_value=dofconst
            widget_control,widget_info(ev.top,find='tas'),get_value=fixedtas
            widget_control,widget_info(ev.top,find='project'),get_value=project
            widget_control,widget_info(ev.top,find='date'),get_value=date
            widget_control,widget_info(ev.top,find='starttime'),get_value=starttime
            widget_control,widget_info(ev.top,find='stoptime'),get_value=stoptime
            widget_control,widget_info(ev.top,find='timeoffset'),get_value=timeoffset
            widget_control,widget_info(ev.top,find='seatag'),get_value=tagstring
            seatag=long(strsplit(tagstring, '[ ,]+', /regex, /extract))

            IF stoptime lt starttime THEN BEGIN
               dummy=dialog_message('Stop time must be later than start time',dialog_parent=widget_info(ev.top,find='process'))
               return
            ENDIF

            ;--------Checkboxes
            id=widget_info(ev.top,find='options')
            widget_control,id,get_uvalue=values
            widget_control,id,get_value=iadv
            IF iadv[where(values eq 'Shatter Correct')] eq 1 THEN inttime_reject=1 ELSE inttime_reject=0
            ;textfile=iadv[where(values eq 'Create PBP file')]
            IF iadv[where(values eq 'All-In')] eq 1 THEN eawmethod='allin' ELSE eawmethod='centerin'
            IF iadv[where(values eq 'Water Processing')] eq 1 THEN water=1 ELSE water=0
            IF iadv[where(values eq 'Stuck Bit Correct')] eq 1 THEN stuckbits=1 ELSE stuckbits=0
            IF iadv[where(values eq 'Pixel Noise Filter')] eq 1 THEN juelichfilter=1 ELSE juelichfilter=0
            IF iadv[where(values eq 'Largest Particle')] eq 1 THEN keeplargest=1 ELSE keeplargest=0
            IF iadv[where(values eq 'Force PSC')] eq 1 THEN apply_psc=1 ELSE apply_psc=0
            IF iadv[where(values eq 'Custom DoF Curve')] eq 1 THEN customdof=1 ELSE customdof=0
            widget_control,widget_info(ev.top,find='tascheckbox'),get_value=stretchcorrect

            ;--------Size Method
            id=widget_info(ev.top,find='sizemethod')
            sizemethodstr=widget_info(id, /combobox_gettext)
            CASE strmid(sizemethodstr,0,1) OF
               'C':smethod='fastcircle'
               'X':smethod='xsize'
               'Y':smethod='ysize'
               'A':smethod='areasize'
               'L':smethod='xextent'
               '1':smethod='oned'
               '2':smethod='twod'
               ELSE:print,'Unknkown size method'
            ENDCASE

            ;--------DoF Criteria
            id=widget_info(ev.top,find='dofcriteria')
            dofcriteriastr=widget_info(id, /combobox_gettext)
            CASE dofcriteriastr OF
               'Off':dofreject=0
               'One Level3 Pixel':dofreject=1
               '50% Level3 Pixel Ratio':dofreject=2
            ENDCASE

            ;--------Output Flag Checkboxes
            id=widget_info(ev.top,find='outputflags')
            widget_control,id,get_uvalue=values
            widget_control,id,get_value=iadv
            IF iadv[where(values eq 'IDL(sav)')] eq 1 THEN savfile=1 ELSE savfile=0
            IF iadv[where(values eq 'PSD(ASCII)')] eq 1 THEN asciipsdfile=1 ELSE asciipsdfile=0
            IF iadv[where(values eq 'ParticleFile(netCDF)')] eq 1 THEN ncdfparticlefile=1 ELSE ncdfparticlefile=0
            IF iadv[where(values eq 'ParticleFile(CSV)')] eq 1 THEN particlefile=1 ELSE particlefile=0
            IF iadv[where(values eq 'Housekeeping(sav)')] eq 1 THEN housefile=1 ELSE housefile=0

            ;--------Probe Details
            id=widget_info(ev.top,find='probetype')
            probename=widget_info(id, /combobox_gettext)
            widget_control,widget_info(ev.top,find='filelist'),get_value=fn
            probe=soda2_probespecs(name=probename, fn=fn)
            IF probe.probename eq '' THEN BEGIN
               dummy=dialog_message('Enter a valid probe',dialog_parent=widget_info(ev.top,find='process'))
               return
            ENDIF
            ;--------Filenames
            widget_control,widget_info(ev.top,find='filelist'),get_value=fn
            widget_control,widget_info(ev.top,find='outdir'),get_value=outdir
            widget_control,widget_info(ev.top,find='filetag'),get_value=filetag
            widget_control,widget_info(ev.top,find='pthfile'),get_value=pthfile

            ;--------Filename checks
            IF total(file_test(fn)) ne n_elements(fn) THEN BEGIN
               dummy=dialog_message('Unknown raw filename(s)',dialog_parent=widget_info(ev.top,find='process'))
               return
            ENDIF
            IF probe.format eq 'SPEC' and n_elements(fn) gt 1 THEN BEGIN
               infoline=['Multiple SPEC files not supported.','Concatenate first with:',$
                         'cat base1 base2 > baseAll (Linux/Mac)',$
                         'copy /b base1+base2 baseAll (Windows)']
               dummy=dialog_message(infoline,dialog_parent=widget_info(ev.top,find='process'))
               return
            ENDIF

            ;--------Bin Sizes
            arendbins=[0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]

            ;--------Bins
            widget_control,widget_info(ev.top,find='endbins'),get_value=binstring
            endbins=float(strsplit(binstring, '[ ,]+', /regex, /extract))

            ;-------Bin size and option checks
            warn=0 & go='Yes'
            ;Bin/Resolution mismatch checks
            IF xres ge 100 and mean(endbins) lt 2000 THEN warn=1
            IF xres lt 100 and mean(endbins) ge 2000 THEN warn=1
            IF xres lt 25  and endbins[0] gt 20 THEN warn=1
            IF warn eq 1 THEN go=dialog_message('The bin sizes seem strange for this probe... Continue?',/question,dialog_parent=widget_info(ev.top,find='process'))
            IF go eq 'No' THEN return

            ;Resolution checks
            IF (xres lt 5) or (xres gt 1000) or (yres lt 5) or (yres gt 1000) THEN $
               go=dialog_message('X-res or Y-res invalid... Continue?',/question,dialog_parent=widget_info(ev.top,find='process'))
            IF go eq 'No' THEN return

            ;Bin checks
            dendbins=endbins[1:*]-endbins
            IF min(dendbins) le 0 THEN BEGIN
               dummy=dialog_message('Bin end-points must be increasing',dialog_parent=widget_info(ev.top,find='process'))
               return
            ENDIF

            ;DoF constant checks
            IF (dofreject eq 2) and (dofconst gt 2) THEN $
               go=dialog_message('DoF Const unusually high for Mode3... Continue?',/question,dialog_parent=widget_info(ev.top,find='process'))
            IF go eq 'No' THEN return

            IF (dofreject le 2) and (dofconst lt 2) THEN $
               go=dialog_message('DoF Const unusually low... Continue?',/question,dialog_parent=widget_info(ev.top,find='process'))
            IF go eq 'No' THEN return

            ;All-in check
            warn=0 & go='Yes'
            IF (total(smethod eq ['xsize','xextent','oned','twod']) gt 0) and (eawmethod ne 'allin') THEN warn=1
            IF warn THEN go=dialog_message('All-in recommended for this sizing method... Continue?',/question,dialog_parent=widget_info(ev.top,find='process'))
            IF go eq 'No' THEN return

            ;Custom DoF check - Create popup window
            IF (customdof eq  1) THEN BEGIN
               dofop={endbins:endbins, armwidth:probe.armwidth, res:xres, numdiodes:probe.numdiodes, eawmethod:eawmethod,$
                  wavelength:probe.wavelength, dofconst:dofconst, smethod:smethod}

               soda2_customdof, dofop, pinfo=pdof, groupleaderid=(*pinfo).groupleader
               IF (*pdof).proceedprocessing_flag eq 1 THEN BEGIN
                  endbins = (*pdof).newendbins[0:(*pdof).newnumbins]
                  customdofvalues = (*pdof).newdof[0:(*pdof).newnumbins-1]
                  ptr_free, pdof  ;This is not released in soda2_customdof
                  IF max(customdofvalues/1e4) gt probe.armwidth THEN BEGIN
                     go=dialog_message('DoF exceeds arm width... Continue?',/question,dialog_parent=widget_info(ev.top,find='process'))
                     IF go eq 'No' THEN return
                  ENDIF
               ENDIF ELSE BEGIN
                  ptr_free, pdof  ;This is not released in soda2_customdof
                  return
               ENDELSE
            ENDIF ELSE customdofvalues = fltarr(n_elements(endbins)-1)

            ;Can add bindistribution to this structure if desired
            op={fn:fn, date:date[0], starttime:hms2sfm(starttime[0]), stoptime:hms2sfm(stoptime[0]), format:probe.format, $
               subformat:probe.subformat, probetype:probe.probetype, res:xres, yres:yres, dofconst:dofconst, $
               endbins:endbins, arendbins:arendbins, rate:rate, smethod:smethod, pth:pthfile[0], asciipsdfile:asciipsdfile, $
               savfile:savfile, inttime_reject:inttime_reject, eawmethod:eawmethod, stuckbits:stuckbits, juelichfilter:juelichfilter, water:water,$
               fixedtas:fixedtas, outdir:outdir[0], filetag:filetag, project:project[0], timeoffset:timeoffset, armwidth:probe.armwidth, $
               numdiodes:probe.numdiodes, probeid:probe.probeid, shortname:probe.shortname, greythresh:probe.greythresh, $
               wavelength:probe.wavelength, seatag:seatag, ncdfparticlefile:ncdfparticlefile, particlefile:particlefile, $
               stretchcorrect:stretchcorrect[0], keeplargest:keeplargest, apply_psc:apply_psc, dofreject:dofreject, $
               dioderange:probe.dioderange, customdof:customdofvalues}

            ;Process housekeeping if flagged
            IF (housefile eq 1) and (probe.format eq 'SPEC') THEN BEGIN
               widget_control,widget_info(ev.top,find='process'),set_value='Housekeeping...'
               spec_process_hk, op, textwidgetid=widget_info(ev.top,find='process')
               widget_control,widget_info(ev.top,find='process'),set_value='BEGIN PROCESSING'
            ENDIF

            ;Process image data
            IF (savfile eq 1) or (asciipsdfile eq 1) or (ncdfparticlefile eq 1)  or (particlefile eq 1) THEN BEGIN
               widget_control,widget_info(ev.top,find='process'),set_value='Processing...'
               soda2_process_2d, op, textwidgetid=widget_info(ev.top,find='process')
               widget_control,widget_info(ev.top,find='process'),set_value='BEGIN PROCESSING'
            ENDIF
        END

        'probetype':BEGIN ;===========================================================================
            widget_control,ev.id,set_uvalue=ev.str    ;A workaround to be able to access current index with widget_control later on
            id=widget_info(ev.top,find='probetype')
            widget_control,id,get_uvalue=probename
            widget_control,widget_info(ev.top,find='filelist'),get_value=fn
            probe=soda2_probespecs(name=probename, fn=fn)
            widget_control,widget_info(ev.top,find='xres'),set_value=probe.res
            widget_control,widget_info(ev.top,find='yres'),set_value=probe.yres
            widget_control,widget_info(ev.top,find='dofconst'),set_value=probe.dofconst
            widget_control,widget_info(ev.top,find='seatag'),set_value=string(probe.seatag,form='(3(i0," "))')
            tagid=widget_info(ev.top,find='tagbase')  ;Toggle SEA tag sensitivity
            IF probe.format eq 'SEA' THEN widget_control,tagid,sensitive=1 ELSE widget_control,tagid,sensitive=0
         END

        'sizemethod': widget_control,ev.id,set_uvalue=ev.str   ;A workaround to be able to access current index with widget_control later on

        'browse': soda2_browse

        'merge': soda2_merge

        'export': soda2_export

        'quit': WIDGET_CONTROL, ev.TOP, /DESTROY

        ELSE: dummy=0
    ENDCASE
END











PRO soda2
   ;Main GUI for SODA-2
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

    IF !version.os_family eq 'windows' THEN default_font='Helvetica*fixed*12'
    IF !version.os_family eq 'unix' THEN default_font='-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1' ;use xlsfonts to see more
    widget_control, default_font=default_font

    device,get_screen_size=screen_size
    IF screen_size[1] lt 900 THEN compact=1 ELSE compact=0  ;For smaller screens

    ;----------Main widget setup-------------------------------------------
    base = WIDGET_BASE(COLUMN=1,title='SODA-2 Optical Array Probe Processor',MBar=menubarID)
    info={datpath:'', rawpath:'', groupleader:base}
    pinfo=ptr_new(info)
    fileID=widget_button(menubarID, value='Menu', /menu, uname='base', uvalue=pinfo)
    loadfile=widget_button(fileID, value='Load settings...',uname='loadfile')
    browseID=widget_button(fileID, value='Browse data...',uname='browse')
    mergeID=widget_button(fileID, value='Compare/Merge data...',uname='merge')
    exportID=widget_button(fileID, value='Export data...',uname='export')
    quitID=widget_button(fileID, value='Quit',uname='quit')


    ;-----------File names widget block------------------------------------
    subbase1=widget_base(base,column=1,frame=5)
    IF compact ne 1 THEN dummy=widget_label(subbase1,value='---OAP Data---',/align_left)
    subbase1a=widget_base(subbase1,row=1)
    addfile = cw_bgroup(subbase1a,['Add file...','Clear files'], uname='addfile',/row,label_left='Raw data file(s):')
    timeoffset=cw_field(subbase1a,/float, title='Clock Correction (s):',uname='timeoffset' , xsize=6, value=0.0)
    filelist= widget_text(subbase1,/scroll,uname='filelist',ysize=2,xsize=62,/editable) ;/editable


    ;------------TAS widget block ------------------------------------
    subbase3=widget_base(base,column=1,frame=5)
    IF compact ne 1 THEN dummy=widget_label(subbase3,value='---Aircraft TAS Data---',/align_left)
    subbase3a=widget_base(subbase3,row=1)
    subbase3b=widget_base(subbase3,row=1)
    addpthfile=cw_bgroup(subbase3a,['Select...','Clear'], uname='findpthfile',/row,label_left='TAS data (dat/ASCII):')
    pthfile=widget_text(subbase3a,uname='pthfile',/editable,xsize=50,/all_events)
    subbase3c=widget_base(subbase3,row=1)
    tas=cw_field(subbase3c,/int, title='or use fixed TAS of (m/s):',uname='tas', value='0', xsize=4)
    vals=['Apply stretch correction']
    tasadvanced=cw_bgroup(subbase3c,vals,uname='tascheckbox',/row,/nonexclusive,uval=vals,set_value=[0])


    ;-----------Probe options widget block----------------------------
    subbase2=widget_base(base,column=1,frame=5)
    IF compact ne 1 THEN dummy=widget_label(subbase2,value='---Probe Options---',/align_left)

    subbase2d=widget_base(subbase2,row=1)
    autofill=widget_button(subbase2d,   value='Auto-Fill',uname='autofill')
    projectname=cw_field(subbase2d,/string,title='Project Name',uname='project',xsize=10,value='NONE',/column)
    date=cw_field(subbase2d,/string,       title='Date (mmddyyyy)',uname='date',xsize=10,value='01012000',/column)
    starttime=cw_field(subbase2d,/string,  title='Start Time (hhmmss)',uname='starttime',value='000000',xsize=10,/column)
    stoptime=cw_field(subbase2d,/string,   title='Stop Time (hhmmss)',uname='stoptime',value='240000',xsize=10,/column)
    rate=cw_field(subbase2d,/float, title='Rate (s):', uname='rate', xsize=5, value=5.0, /column)

    subbase2b=widget_base(subbase2,row=1)
    specs=soda2_probespecs()
    dummy=widget_label(subbase2b,value='Probe:',/align_left)
    probetype=widget_combobox(subbase2b,value=['None',specs.probename],uname='probetype',uvalue='None')

    subbase2a=widget_base(subbase2,row=1)
    xres=cw_field(subbase2a,/float, title='X-res (um):',uname='xres' , xsize=5, value=0)
    yres=cw_field(subbase2a,/float, title='Y-res (um):',uname='yres' , xsize=5, value=0)
    dofconst=cw_field(subbase2a,/float, title='DoF Const:',uname='dofconst' , xsize=5, value=0)
    subbase2f=widget_base(subbase2a,row=1,sensitive=0,uname='tagbase')
    seatag=cw_field(subbase2f, title='SEA Tags:', uname='seatag' , xsize=10, value='0 0 0')


    ;---------Processing options-------------------------
    subbase5=widget_base(base,column=1,frame=5)
    IF compact ne 1 THEN dummy=widget_label(subbase5,value='---Processing Options---',/align_left)

    subbase5a=widget_base(subbase5,row=1)
    binstring=string([25, 50, 100, 150, 200, 250, 300, 350, 400, 500, 600,700,800,900,1000,1200,1400,1600,1800,2000],form='(100(i0," "))')
    endbins=cw_field(subbase5a, title='Bin edges (um):  ', uname='endbins', xsize=45, value=binstring)
    defaultbins=widget_button(subbase5a, value=' Default ',uname='defaultbins')
    fullbins=widget_button(subbase5a, value=' Full ',uname='fullbins')
    fullbinsx2=widget_button(subbase5a, value=' x2 ',uname='fullbinsx2')

    subbase5b=widget_base(subbase5,row=1)
    dummy=widget_label(subbase5b,value='Size Method:',/align_left)
    methodnames=['Circle fit','X-size (across array)','Y-size (with airflow)','Area equivalent','Lx (max slice width)', $
                 '1D emulation', '2D emulation']
    sizemethod=widget_combobox(subbase5b,value=methodnames,uname='sizemethod',uvalue=methodnames[0])
    dummy=widget_label(subbase5b,value='  DoF Criteria:',/align_left)
    dofcriterianames=['Off', 'One Level3 Pixel', '50% Level3 Pixel Ratio']
    dofcriteria=widget_combobox(subbase5b,value=dofcriterianames,uname='dofcriteria',uvalue=dofcriterianames[0])

    subbase5c=widget_base(subbase5,row=1)
    vals=['Shatter Correct','All-In','Water Processing','Stuck Bit Correct','Pixel Noise Filter','Largest Particle',$
      'Force PSC','Custom DoF Curve']
    advanced=cw_bgroup(subbase5c,vals,uname='options',row=2,/nonexclusive,uval=vals,set_value=[1,0,0,0,0,0,0,0])


    ;---------Output directory and process button-------------------------
    subbase4=widget_base(base,column=1,frame=5)
    IF compact ne 1 THEN dummy=widget_label(subbase4,value='---Output Options---',/align_left)

    subbase4a=widget_base(subbase4,row=1)
    vals=['IDL(sav)','PSD(ASCII)','ParticleFile(netCDF)','ParticleFile(CSV)','Housekeeping(sav)']
    outputflags=cw_bgroup(subbase4a,vals,uname='outputflags',/row,/nonexclusive,uval=vals,set_value=[1,0,0])

    subbase4b=widget_base(subbase4,row=1)
    cd,current=currentdir
    outdirID=cw_field(subbase4b,/string,  title='Output directory: ',uname='outdir',xsize=42,value=currentdir+path_sep())
    browse2=widget_button(subbase4b,value='Select...',uname='findoutdir')
    filetagID=cw_field(subbase4b, /string, title='Tag: ', uname='filetag', xsize=7)

    process = WIDGET_BUTTON(base, value='BEGIN PROCESSING', uname='process')

    soda2_imagedump  ;This is just to ensure imagedump is included with the 'package' command.
    WIDGET_CONTROL, base, /REALIZE
    XMANAGER, 'soda2', base, /no_block
END
