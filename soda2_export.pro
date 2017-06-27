PRO soda2_export_event, ev
   ;GUI for netCDF export routine
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   uname=widget_info(ev.id,/uname)

   CASE uname OF
 
        'addfile': BEGIN ;===========================================================================
            IF ev.value eq 0 THEN BEGIN ;Add a file pressed
                a=dialog_pickfile(/read,/multiple,path=defaultpath,filter='*.dat',title='Files to Export',dialog_parent=ev.id)
                IF file_test(a[0]) THEN BEGIN
                   widget_control,widget_info(ev.top,find='filelist'),get_value=fn
                   IF strlen(fn[0]) gt 2 THEN fn=[fn,a] ELSE fn=a
                   widget_control,widget_info(ev.top,find='filelist'),set_value=fn
                ENDIF
            ENDIF
            IF ev.value eq 1 THEN BEGIN ;Clear files pressed
                widget_control,widget_info(ev.top,find='filelist'),set_value=''
            ENDIF
        END


        'findoutdir': BEGIN ;===========================================================================
            a=dialog_pickfile(/read,/directory,get_path=a2,dialog_parent=ev.id)
            IF file_test(a) THEN widget_control,widget_info(ev.top,find='outdir'),set_value=a2
        END


        'process':BEGIN ;===========================================================================
            ;Collect data from the GUI

            ;--------Filenames
            widget_control,widget_info(ev.top,find='filelist'),get_value=fn
            widget_control,widget_info(ev.top,find='outdir'),get_value=outdir
            IF strmid(outdir, strlen(outdir)-1, 1) ne path_sep() THEN outdir=outdir+path_sep()
           
            widget_control,widget_info(ev.top,find='process'),set_value='Processing...'
            infoline=['Exported files:']
            FOR i=0,n_elements(fn)-1 DO BEGIN
               IF file_test(fn[i]) THEN BEGIN
                  restore,fn[i]
                  outfile=(str_sep(file_basename(fn[i]), '.dat'))[0] + '.nc'
                  soda2_export_ncdf,data,outfile=outdir + outfile
                  infoline=[infoline, outdir+outfile]
               ENDIF
            ENDFOR
            dummy=dialog_message(infoline,dialog_parent=ev.id,/info)
            widget_control,widget_info(ev.top,find='process'),set_value='EXPORT to netCDF'
        END

        'processimages':BEGIN ;===========================================================================
            ;Collect data from the GUI

            ;--------Filenames
            widget_control,widget_info(ev.top,find='filelist'),get_value=fn
            widget_control,widget_info(ev.top,find='outdir'),get_value=outdir
            IF strmid(outdir, strlen(outdir)-1, 1) ne path_sep() THEN outdir=outdir+path_sep()
            
            FOR i=0,n_elements(fn)-1 DO BEGIN
               IF file_test(fn[i]) THEN BEGIN
                  widget_control,widget_info(ev.top,find='processimages'),set_value='Indexing: '+file_basename(fn[i])
                  soda2_imagedump, fn[i], all=all, outdir=outdir, textwidgetid=widget_info(ev.top,find='processimages')
               ENDIF
            ENDFOR
            dummy=dialog_message('Image export complete',dialog_parent=ev.id,/info)
            widget_control,widget_info(ev.top,find='processimages'),set_value='WRITE IMAGES'
        END

        
        'quit': WIDGET_CONTROL, ev.TOP, /DESTROY

        ELSE: dummy=0
    ENDCASE
END


PRO soda2_export
    IF !version.os_family eq 'windows' THEN widget_control,default_font='Helvetica*fixed*12'
    IF !version.os_family eq 'unix' THEN widget_control,default_font='-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1' ;use xlsfonts to see more
    ;IF !version.os_family eq 'unix' THEN widget_control,default_font='-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1'
    ;widget_control,default_font='Courier*10'
    
    base = WIDGET_BASE(COLUMN=1,title='Export Data',MBar=menubarID)

    fileID=widget_button(menubarID, value='File', /menu)
    quitID=widget_button(fileID, value='Quit',uname='quit')

    helpID=widget_button(menubarID, value='Help', /menu,/align_right)


    subbase5=widget_base(base,column=1,/frame)
    addfile = cw_bgroup(subbase5,['Add a file...','Clear files'], uname='addfile',/row,label_left='SODA file(s) to export:')
    filelist= widget_text(subbase5,/scroll,uname='filelist',/editable,ysize=6)
	 subbase5a=widget_base(subbase5,row=1)
    cd,current=currentdir
    outdirID=cw_field(subbase5a,/string, title='Output directory: ', value=currentdir+path_sep(), uname='outdir', xsize=52)
    browse2=widget_button(subbase5a,value='Select...',uname='findoutdir')



    process = WIDGET_BUTTON(base, value='EXPORT to netCDF', uname='process')
    process = WIDGET_BUTTON(base, value='WRITE IMAGES', uname='processimages')
    WIDGET_CONTROL, base, /REALIZE
    XMANAGER, 'soda2_export', base, /no_block
END
