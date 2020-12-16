PRO soda2_event, ev
   ;Event handler for SODA GUI
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

    uname=widget_info(ev.id,/uname)
    widget_control,widget_info(ev.top,find='base'),get_uvalue=pinfo
    CASE uname OF
        'loadfile': BEGIN ;===========================================================================
            a=dialog_pickfile(/read,filter=['*.dat'],path=(*pinfo).datpath,dialog_parent=widget_info(ev.top,find='process'))
            IF file_test(a) THEN BEGIN
            (*pinfo).datpath=file_dirname(a)
            restore,a
            op=data.op
            soda2_update_op,op
            
            ;--------Fill in boxes
            widget_control,widget_info(ev.top,find='timeoffset'),set_value=op.timeoffset
            widget_control,widget_info(ev.top,find='starttime'),set_value=sfm2hms(data.op.starttime)
            widget_control,widget_info(ev.top,find='stoptime'),set_value=sfm2hms(data.op.stoptime)
            widget_control,widget_info(ev.top,find='rate'),set_value=data.op.rate
            widget_control,widget_info(ev.top,find='date'),set_value=data.op.date
            widget_control,widget_info(ev.top,find='project'),set_value=data.op.project
            
            ;--------TAS stuff
            widget_control,widget_info(ev.top,find='tas'),set_value=op.fixedtas
            widget_control,widget_info(ev.top,find='pthfile'),set_value=op.pth
            widget_control,widget_info(ev.top,find='tascheckbox'),set_value=op.stretchcorrect
             
            ;--------Checkboxes
            checkboxarray=[0,0,0,0,0,0,0,0]
            id=widget_info(ev.top,find='options')
            widget_control,id,get_uvalue=values
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
            IF op.dofreject eq 1 THEN checkboxarray[where(values eq 'DoF Reject')]=1
            widget_control,id,set_value=checkboxarray

            ;--------Output checkboxes
            checkboxarray=[0,0,0,0]
            id=widget_info(ev.top,find='outputflags')
            widget_control,id,get_uvalue=values
            IF op.savfile THEN checkboxarray[where(values eq 'IDL sav')]=1
            IF op.particlefile THEN checkboxarray[where(values eq 'Particle-by-Particle')]=1
            IF op.ncdfparticlefile THEN checkboxarray[where(values eq 'Particle-by-Particle(netCDF)')]=1
            widget_control,id,set_value=checkboxarray

            ;--------Filenames
            widget_control,widget_info(ev.top,find='filelist'),set_value=op.fn
            widget_control,widget_info(ev.top,find='outdir'),set_value=op.outdir
            
            ;--------Bins
            widget_control,widget_info(ev.top,find='endbins'),set_value=string(data.op.endbins,form='(100(i0," "))')

            ;--------Size method
            id=widget_info(ev.top,find='sizemethod')
            methods=['Circle fit','X-size (across array)','Y-size (with airflow)','Area equivalent','Lx (max slice width)',$
                     '1D emulation', '2D emulation']
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
            widget_control,id,set_value=[methods[w], methods]     
            widget_control,id,set_uvalue=methods[w]

            ;--------Probe type
            id=widget_info(ev.top,find='probetype')
            p=soda2_probespecs()
            w=where((p.format eq op.format) and (p.subformat eq op.subformat) and (p.probeid eq op.probeid) $
               and (p.res eq op.res) and (p.armwidth eq op.armwidth) and (p.numdiodes eq op.numdiodes), nw)
            IF nw eq 0 THEN dummy=dialog_message('Unknown probe type in .sav file.' + string(10B) + string(10B)+ $
                'Select new type or add probe to soda2_probespecs.pro') ELSE $
            w=w[0]  ;Just in case of duplicates (i.e. Fast-2DC 66%)
            widget_control,id,set_value=[p[w].probename, p.probename]     
            widget_control,id,set_uvalue=p[w].probename
            ENDIF
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
            id=widget_info(ev.top,find='probetype')
            widget_control,id,get_uvalue=probename
            probe=soda2_probespecs(name=probename)
            IF probe.res le 50 THEN endbins=[25, 50, 100, 150, 200, 250, 300, 350, 400, 500, 600,700,800,900,1000,1200,1400,1600,1800,2000]
            IF probe.res lt 15 THEN endbins=[5,15,25,35,45,55,65,75,85,95,105,125,145,175,225,275,325,400,475,550,625,700,800,900,1000,1200,1400,1600,1800,2000]
            IF probe.res gt 50 THEN endbins=[200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2200, 2600, 3000, 3400, 3800, 4200, 4600, 5000,6000,7000,8000,9000,10000,15000,20000,25000,30000]
            widget_control,widget_info(ev.top,find='endbins'),set_value=string(endbins,form='(100(i0," "))')
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
               IF nw gt 0 THEN probenames=specs[w].probename ELSE probenames=specs.probename
               
               id=widget_info(ev.top,find='probetype')  ;Get the current probe to avoid changing it
               widget_control,id,get_uvalue=currentprobename
               
               w=where(probenames eq currentprobename)
               IF w ne -1 THEN BEGIN  ;Keep the current probe
                  widget_control,id,set_value=[currentprobename, probenames]     
                  widget_control,id,set_uvalue=currentprobename
               ENDIF ELSE BEGIN   ;Don't keep current probe
                  widget_control,id,set_value=probenames     
                  widget_control,id,set_uvalue=probenames[0]
               ENDELSE
            ENDIF ELSE BEGIN
               ;Reset the full probe list
               specs=soda2_probespecs()
               widget_control, widget_info(ev.top,find='probetype'),set_value=specs.probename
               widget_control, widget_info(ev.top,find='probetype'),set_uvalue=specs.probename[0]
            ENDELSE
        END
      
        'process':BEGIN ;===========================================================================
            ;Collect data from the GUI
            ;--------Boxes
            widget_control,widget_info(ev.top,find='rate'),get_value=rate
            widget_control,widget_info(ev.top,find='tas'),get_value=fixedtas
            widget_control,widget_info(ev.top,find='project'),get_value=project
            widget_control,widget_info(ev.top,find='date'),get_value=date
            widget_control,widget_info(ev.top,find='starttime'),get_value=starttime
            widget_control,widget_info(ev.top,find='stoptime'),get_value=stoptime
            widget_control,widget_info(ev.top,find='timeoffset'),get_value=timeoffset
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
            IF iadv[where(values eq 'DoF Reject')] eq 1 THEN dofreject=1 ELSE dofreject=0
            widget_control,widget_info(ev.top,find='tascheckbox'),get_value=stretchcorrect
           
            ;--------Size Method
            id=widget_info(ev.top,find='sizemethod')
            widget_control,id,get_uvalue=sizemethodstr
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
                 
            ;--------Output Flag Checkboxes
            id=widget_info(ev.top,find='outputflags')
            widget_control,id,get_uvalue=values
            widget_control,id,get_value=iadv
            IF iadv[where(values eq 'IDL sav')] eq 1 THEN savfile=1 ELSE savfile=0
            IF iadv[where(values eq 'Particle-by-Particle')] eq 1 THEN particlefile=1 ELSE particlefile=0
            IF iadv[where(values eq 'Particle-by-Particle(netCDF)')] eq 1 THEN ncdfparticlefile=1 ELSE ncdfparticlefile=0
            IF iadv[where(values eq 'Housekeeping')] eq 1 THEN housefile=1 ELSE housefile=0

            ;--------Probe Details
            id=widget_info(ev.top,find='probetype')
            widget_control,id,get_uvalue=probename
            probe=soda2_probespecs(name=probename)
            
            ;--------Filenames
            widget_control,widget_info(ev.top,find='filelist'),get_value=fn
            widget_control,widget_info(ev.top,find='outdir'),get_value=outdir
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
            ;IF probe.res le 50 THEN endbins=[25,75,125,175,225,275,325,400,475,550,625,700,800,900,1000,1200,1400,1600,1800,2000]
            ;IF probe.res gt 50 THEN endbins=[200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2200, 2600, 3000, 3400, 3800, 4200, 4600, 5000,6000,7000,8000,9000,10000,15000,20000,25000,30000]       
            ;2D-S bins as special case
            ;IF probe.res eq 10 THEN endbins=[5,15,25,35,45,55,65,75,85,95,105,125,145,175,225,275,325,400,475,550,625,700,800,900,1000,1200,1400,1600,1800,2000]

            ;--------Bins
            widget_control,widget_info(ev.top,find='endbins'),get_value=binstring
            endbins=float(strsplit(binstring, '[ ,]+', /regex, /extract))
            
            ;-------Bin size checks
            warn=0 & go='Yes'
            IF probe.res ge 100 and mean(endbins) lt 2000 THEN warn=1
            IF probe.res lt 100 and mean(endbins) ge 2000 THEN warn=1
            IF probe.res lt 25  and endbins[0] gt 20 THEN warn=1
            IF warn THEN go=dialog_message('The bin sizes seem strange for this probe... Continue?',/question,dialog_parent=widget_info(ev.top,find='process'))
            IF go eq 'No' THEN return
            dendbins=endbins[1:*]-endbins
            IF min(dendbins) le 0 THEN BEGIN
               dummy=dialog_message('Bin end-points must be increasing',dialog_parent=widget_info(ev.top,find='process'))
               return
            ENDIF
            
            
            ;Can add bindistribution to this structure if desired
            op={fn:fn, date:date[0], starttime:hms2sfm(starttime[0]), stoptime:hms2sfm(stoptime[0]), format:probe.format, $
               subformat:probe.subformat, probetype:probe.probetype, res:probe.res, yres:probe.yres, dofconst:probe.dofconst, $
               endbins:endbins, arendbins:arendbins, rate:rate, smethod:smethod, pth:pthfile[0], particlefile:particlefile, $
               savfile:savfile, inttime_reject:inttime_reject, eawmethod:eawmethod, stuckbits:stuckbits, juelichfilter:juelichfilter, water:water,$
               fixedtas:fixedtas, outdir:outdir[0], project:project[0], timeoffset:timeoffset, armwidth:probe.armwidth, $
               numdiodes:probe.numdiodes, probeid:probe.probeid, shortname:probe.shortname, greythresh:probe.greythresh, $
               wavelength:probe.wavelength, seatag:probe.seatag, ncdfparticlefile:ncdfparticlefile, stretchcorrect:stretchcorrect[0],$
               keeplargest:keeplargest, apply_psc:apply_psc, dofreject:dofreject}

            ;Process housekeeping if flagged
            IF (housefile eq 1) and (probe.format eq 'SPEC') THEN BEGIN
               widget_control,widget_info(ev.top,find='process'),set_value='Housekeeping...'
               spec_process_hk, op, textwidgetid=widget_info(ev.top,find='process')
               widget_control,widget_info(ev.top,find='process'),set_value='BEGIN PROCESSING'
            ENDIF
            
            ;Process image data
            IF (savfile eq 1) or (particlefile eq 1) or (ncdfparticlefile eq 1) THEN BEGIN
               widget_control,widget_info(ev.top,find='process'),set_value='Processing...'
               soda2_process_2d, op, textwidgetid=widget_info(ev.top,find='process')
               widget_control,widget_info(ev.top,find='process'),set_value='BEGIN PROCESSING'
            ENDIF
        END
        
        'probetype': widget_control,ev.id,set_uvalue=ev.str    ;A workaround to be able to access current index with widget_control later on

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

    IF !version.os_family eq 'windows' THEN widget_control,default_font='Helvetica*fixed*12'
    IF !version.os_family eq 'unix' THEN widget_control,default_font='-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1' ;use xlsfonts to see more
    ;IF !version.os_family eq 'unix' THEN widget_control,default_font='-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1'
   device,get_screen_size=screen_size
   IF screen_size[1] lt 900 THEN compact=1 ELSE compact=0  ;For smaller screens
  
    ;----------Main widget setup-------------------------------------------
    base = WIDGET_BASE(COLUMN=1,title='SODA-2 Processing Software Version 1.0',MBar=menubarID)
    info={datpath:'', rawpath:''}
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
    addpthfile=cw_bgroup(subbase3a,['Select...','Clear'], uname='findpthfile',/row,label_left='Aircraft TAS data file (SODA or ASCII format):')
    pthfile=widget_text(subbase3,uname='pthfile',/editable,xsize=62,/all_events) 
    subbase3c=widget_base(subbase3,row=1)
    tas=cw_field(subbase3c,/int, title='or use fixed TAS of (m/s):',uname='tas', value='0', xsize=4)
    vals=['Apply stretch correction']
    tasadvanced=cw_bgroup(subbase3c,vals,uname='tascheckbox',/row,/nonexclusive,uval=vals,set_value=[0])


    ;-----------Processing options widget block----------------------------
    subbase2=widget_base(base,column=1,frame=5)
    IF compact ne 1 THEN dummy=widget_label(subbase2,value='---Processing Options---',/align_left)
    
    subbase2d=widget_base(subbase2,row=1)
    projectname=cw_field(subbase2d,/string,title='Project Name',uname='project',xsize=15,value='NONE',/column)
    date=cw_field(subbase2d,/string,       title='Date (mmddyyyy)',uname='date',xsize=15,value='01012000',/column)
    starttime=cw_field(subbase2d,/string,  title='Start Time (hhmmss)',uname='starttime',value='000000',xsize=15,/column)
    stoptime=cw_field(subbase2d,/string,   title='Stop Time (hhmmss)',uname='stoptime',value='240000',xsize=15,/column)
    defaultbins=widget_button(subbase2d,   value='Auto-Fill',uname='autofill')

    subbase2b=widget_base(subbase2,row=1)
    specs=soda2_probespecs()
    dummy=widget_label(subbase2b,value='Probe:',/align_left)
    probetype=widget_combobox(subbase2b,value=specs.probename,uname='probetype',uvalue=specs[0].probename)
    dummy=widget_label(subbase2b,value='  Sizing Method:',/align_left)
    methodnames=['Circle fit','X-size (across array)','Y-size (with airflow)','Area equivalent','Lx (max slice width)', $
                 '1D emulation', '2D emulation']
    sizemethod=widget_combobox(subbase2b,value=methodnames,uname='sizemethod',uvalue=methodnames[0])

    subbase2e=widget_base(subbase2,row=1)
    binstring=string([25, 50, 100, 150, 200, 250, 300, 350, 400, 500, 600,700,800,900,1000,1200,1400,1600,1800,2000],form='(100(i0," "))')
    endbins=cw_field(subbase2e, title='Bin end-points (um):  ', uname='endbins', xsize=52, value=binstring)
    defaultbins=widget_button(subbase2e, value=' Default ',uname='defaultbins')
    
    subbase2a=widget_base(subbase2,row=1)  
    rate=cw_field(subbase2a,/float, title='Averaging Time (s):',uname='rate' , xsize=6, value=5.0)
   
    subbase2c=widget_base(subbase2,row=1)
    vals=['Shatter Correct','All-In','Water Processing','Stuck Bit Correct','Pixel Noise Filter','Largest Particle','Force PSC','DoF Reject']
    advanced=cw_bgroup(subbase2c,vals,uname='options',row=2,/nonexclusive,uval=vals,set_value=[1,0,0,0,0,0,0,0])


    ;---------Output directory and process button-------------------------
    subbase4=widget_base(base,column=1,frame=5)
    IF compact ne 1 THEN dummy=widget_label(subbase4,value='---Output Options---',/align_left)
    
    subbase4a=widget_base(subbase4,row=1)
    vals=['IDL sav','Particle-by-Particle','Particle-by-Particle(netCDF)','Housekeeping']
    outputflags=cw_bgroup(subbase4a,vals,uname='outputflags',/row,/nonexclusive,uval=vals,set_value=[1,0,0])

    subbase4b=widget_base(subbase4,row=1)
    cd,current=currentdir   
    outdirID=cw_field(subbase4b,/string,  title='Output directory: ',uname='outdir',xsize=52,value=currentdir+path_sep())
    browse2=widget_button(subbase4b,value='Select...',uname='findoutdir')
    
    process = WIDGET_BUTTON(base, value='BEGIN PROCESSING', uname='process')

    soda2_imagedump  ;This is just to ensure imagedump is included with the 'package' command.
    WIDGET_CONTROL, base, /REALIZE
    XMANAGER, 'soda2', base, /no_block
END

