PRO soda2_browse_event, ev
   ;Event handler for SODA2 browsing GUI
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

    uname=widget_info(ev.id,/uname)
    ;Get pointers to the restored data
    widget_control,widget_info(ev.top,find='load'),get_uvalue=p1
    widget_control,widget_info(ev.top,find='properties'),get_uvalue=pop
    widget_control,widget_info(ev.top,find='base'),get_uvalue=pinfo
    widget_control,widget_info(ev.top,find='tab5'),get_uvalue=pmouse
    widget_control,widget_info(ev.top,find='tab2'),get_uvalue=pmisc
    widget_control,widget_info(ev.top,find='tab'),get_uvalue=screen
    screen_x=screen[0]
    screen_y=screen[1]
    
    CASE 1 OF
        ;====================================================================================================
        uname eq 'load':BEGIN
            IF ptr_valid(pinfo) THEN path=(*pinfo).outdir ELSE path=''
            fn=dialog_pickfile(/read,filter=['*.dat'],dialog_parent=widget_info(ev.top,find='tab'),path=path)
            IF (file_test(fn) eq 0) or (file_test(fn,/directory) eq 1) THEN return
            
            ;Get window IDs to store in pinfo
            widget_control,widget_info(ev.top,find='w1'),get_value=w1
            widget_control,widget_info(ev.top,find='w2'),get_value=w2
            widget_control,widget_info(ev.top,find='w3'),get_value=w3
            widget_control,widget_info(ev.top,find='w5'),get_value=w5
            widget_control,widget_info(ev.top,find='wt'),get_value=wt
            wid=[w1,w2,w3,w5]

            outdir=file_dirname(fn)+path_sep()           
            info={i:0L,i1:0L,i2:0L,b1i:-1L,fn:fn,gotfile:0,timeformat:1,wid:wid,wt:wt,$
               bmp:bytarr(float(screen_x),50),outdir:outdir,rawdir:'',show_correction:1,panelstart:0} ; Reset pinfo
            
            ;Restore files, set pointers to the restored data           
            restore,fn
            op=data.op
            soda2_update_op,op
            IF ptr_valid(p1) THEN ptr_free,p1   
            IF ptr_valid(pinfo) THEN ptr_free,pinfo
            IF ptr_valid(pop) THEN ptr_free,pop
            
            ;Make all bulk computations starting at 100um
            binstart=min(where(op.endbins ge 100))         
            bulk=compute_bulk_simple(data.conc1d,op.endbins,binstart=binstart)
            
            armidbins=(op.arendbins[1:*]+op.arendbins[1:*])/2.0           
            meanar=compute_meanar(data.spec2d,armidbins)
            binary=data.conc1d and (data.conc1d*0 + 1)  ;Declutter the meanar and conc1d data
            kernel=[[0,1,0],[1,1,1],[0,1,0]]
            binary=morph_open(binary,kernel)
            meanar=meanar*binary
            data.conc1d=data.conc1d*binary

            IF total(tag_names(data) eq 'COUNT_ALL') eq 1 THEN BEGIN
               ;SODA-1
               sodaversion=1
               count_rejected_total=data.count_all-data.count_accepted
               data=create_struct(data,bulk,'count_rejected_total',count_rejected_total,'meanar',meanar,$
                    'count_missed',data.time*0,'sodaversion',sodaversion)    
               soda1_plots=['Total Concentration','IWC','dBZ','Diameter',$
                   'Color Concentration','Color Interarrival', 'Color Interarrival Accepted','Color Area Ratio', $
                   'Particle Counts']

               widget_control,widget_info(ev.top,find='ts_type1'),set_value=soda1_plots
               widget_control,widget_info(ev.top,find='ts_type2'),set_value=soda1_plots
            ENDIF ELSE BEGIN
               ;SODA-2
               sodaversion=2
               count_rejected_total=long(total(data.count_rejected,2))
               data=create_struct(data,bulk,'count_rejected_total',count_rejected_total,'meanar',meanar,'sodaversion',sodaversion)
               ;Add in aspect ratio if it is in the dat file
               IF total(tag_names(data) eq 'SPEC2D_ASPR') ne 0 THEN BEGIN
                  meanaspr=(compute_meanar(data.spec2d_aspr, armidbins)) * binary  ;Uses same bins as area ratio
                  data=create_struct(data, 'meanaspr', meanaspr)
               ENDIF
            ENDELSE
            p1=ptr_new(data)
            pop=ptr_new(op)
    
            info.gotfile=1
            widget_control,widget_info(ev.top,find='load'),set_uvalue=p1
            widget_control,widget_info(ev.top,find='properties'),set_uvalue=pop
            
            ;Check to see if the raw data files are still in the same location
            ft=file_test(op.fn[0])
            IF (ft eq 0) and (sodaversion eq 2) THEN BEGIN
               info.rawdir=dialog_pickfile(/read,/directory,title='Can''t find '+op.fn[0]+', where is it?',dialog_parent=ev.top)
               IF file_test(info.rawdir+file_basename(op.fn[0])) eq 0 THEN info.rawdir=''  ;User selected the wrong directory
            ENDIF ELSE info.rawdir=file_dirname(op.fn[0])+path_sep()
            
            ;Index SPEC files if available and add to misc
            IF (op.format eq 'SPEC') and (file_test(info.rawdir+file_basename(op.fn[0])) eq 1) THEN BEGIN
               indexoption=dialog_message('Load images?  This may take a few minutes.',/question,dialog_parent=ev.top)
               IF indexoption eq 'Yes' THEN BEGIN
                  tabnum=widget_info(widget_info(ev.top,find='tab'),/tab_current)
                  wset,wid[tabnum]
                  tv,bytarr(1000,1000) + !p.background  ;clear window
                  xyouts,0.45,0.5,'Indexing Images...',/norm,charsize=2
                  openr,lun,info.rawdir+file_basename(op.fn[0]),/get_lun
                  misc=spec_index(lun)              
                  free_lun,lun
                  imagesfound=1
               ENDIF ELSE info.rawdir=''
            ENDIF
   
            ;Keep miscellaneous stuff here, things that change during processing
            misc2={f2d_remainder:ulon64arr(512), f2d_remainder_slices:0, yres:op.res, lastbufftime:0.0, $
                nimages:0, imagepointers:lon64arr(500), lastclock:0d, lastparticlecount:0L}    

            misc=create_struct(temporary(misc), misc2)  ;Join the SPEC and misc structures
            
            pmisc=ptr_new(misc)
            widget_control,widget_info(ev.top,find='tab2'),set_uvalue=pmisc
                                   
            ;Plot bin 2 in the time bar window
            wset,wt
            plot,smooth(data.conc1d[*,1],5,/nan),xsty=5,ysty=5,pos=[0,0,1,1],yr=[1e2,1e12],/yl
            info.bmp=tvrd()     ;The raw plot
            tsbmp=info.bmp
            tsbmp[info.i,*]=150 ;Add a time bar at the beginning
            tv,tsbmp
            
            info.i2=n_elements(data.time)-1
            pinfo=ptr_new(info)
            widget_control,widget_info(ev.top,find='base'),set_uvalue=pinfo
            
            ;Make the rest of the GUI sensitive
            widget_control,widget_info(ev.top,find='tab'),sensitive=1
            widget_control,widget_info(ev.top,find='time'),sensitive=1
            widget_control,widget_info(ev.top,find='timeformat'),sensitive=1
            widget_control,widget_info(ev.top,find='png'),sensitive=1
            widget_control,widget_info(ev.top,find='wt'),sensitive=1
            widget_control,widget_info(ev.top,find='properties'),sensitive=1
            widget_control,widget_info(ev.top,find='color_invert'),sensitive=1
            widget_control,widget_info(ev.top,find='massparammenu'),sensitive=1
            widget_control,widget_info(ev.top,find='filedisplay'),set_value=file_basename(fn)

            ;Establish mouse info
            mouse={down:0, pixid:0, wid:0, xsize:0, ysize:0, sx:0, sy:0, dx:0, dy:0}
            pmouse=ptr_new(mouse)
            widget_control,widget_info(ev.top,find='tab5'),set_uvalue=pmouse
            
                       
            ;Plot new data
            soda2_windowplot,ev.top,p1,pinfo,pop,pmisc
            
         END
        ;====================================================================================================
        (uname eq 'wt')  or (uname eq 'time') or (uname eq 'w1') or (uname eq 'w2') or (uname eq 'w3') or (uname eq 'w5'):BEGIN      
            tabnum=widget_info(widget_info(ev.top,find='tab'),/tab_current)
            (*pinfo).b1i=-1 ;Reset particle buffer count
            (*pinfo).panelstart=0 ;Reset image panel index
            
            advance=0
            i=(*pinfo).i
            i1=(*pinfo).i1
            i2=(*pinfo).i2
            update=0  ;Flag to enact updates
                        
            ;Mouse events draw selection boxes when in the time series tab
            IF  (tabnum eq 3) THEN BEGIN
               ;Button press
               IF ev.type eq 0 THEN BEGIN
                  (*pmouse).down=1
                  IF (uname eq 'w5') THEN wid=(*pinfo).wid[tabnum]
                  IF (uname eq 'wt') THEN wid=(*pinfo).wt
                  wset,wid
                  xsize = !D.X_VSize
                  ysize = !D.Y_VSize
                  Window, /Pixmap, /Free, XSize=xsize, YSize=ysize
                  pixID = !D.Window
                  Device, Copy=[0, 0, xsize, ysize, 0, 0, wid]
                  WSet, wid
                  (*pmouse).sx=ev.x
                  (*pmouse).sy=ev.y
                  (*pmouse).pixid=pixid
                  (*pmouse).wid=wid
                  (*pmouse).xsize=xsize
                  (*pmouse).ysize=ysize
               ENDIF
               ;Mouse motion
               IF (ev.type eq 2) and ((*pmouse).down eq 1) THEN BEGIN
                  dx=ev.x > 0 <(*pmouse).xsize
                  dy=ev.y > 0 <(*pmouse).ysize
                  sx=(*pmouse).sx
                  sy=(*pmouse).sy
                  wset,(*pmouse).wid
                  ; Erase the old box.
                  Device, Copy=[0, 0, (*pmouse).xsize, (*pmouse).ysize, 0, 0, (*pmouse).pixID]
                  ; Draw the new box.
                  PlotS, [sx, sx, dx, dx, sx], [sy, dy, dy, sy, sy], /Device, Color=250                  
               ENDIF
               ;Button Release
               IF ev.type eq 1 THEN BEGIN
                  (*pmouse).down=0
                  dx=ev.x > 0 <(*pmouse).xsize
                  dy=ev.y > 0 <(*pmouse).ysize 
                  sx=(*pmouse).sx
                  sy=(*pmouse).sy
                  
                  ; Erase the final box.
                  wset,(*pmouse).wid
                  Device, Copy=[ 0, 0, (*pmouse).xsize, (*pmouse).ysize, 0, 0, (*pmouse).pixID]
                  ; Delete the pixmap.
                  WDelete, (*pmouse).pixID
                  ; Order the box coordinates and return.
                  sx = Min([sx,dx], Max=dx)
                  IF (uname eq 'w5')  THEN BEGIN                              
                     xnorm_start=sx/float(screen_x)
                     xnorm_stop=dx/float(screen_x)
                     xnormplot_start=(xnorm_start-!x.window[0])/(!x.window[1]-!x.window[0])  ;Norm coord within the plot
                     xnormplot_stop=(xnorm_stop-!x.window[0])/(!x.window[1]-!x.window[0])  ;Norm coord within the plot
                     xnormplot_start=xnormplot_start>0<1 ;Enforce limits
                     xnormplot_stop=xnormplot_stop>0<1 ;Enforce limits
                     i1orig=i1
                     i1=xnormplot_start*(i2-i1orig)+i1orig
                     i2=xnormplot_stop*(i2-i1orig)+i1orig 
                  ENDIF
                  IF (uname eq 'wt')  THEN BEGIN
                     i1=sx/float(screen_x) * n_elements((*p1).time)
                     i2=dx/float(screen_x) * n_elements((*p1).time)
                  ENDIF
                  IF dx-sx gt 10 THEN update=1
               ENDIF
            ENDIF
            
            ;In other tabs just use release events
            IF (tabnum ne 3) and ((ev.release ne 0) or (ev.type eq 7)) THEN BEGIN
               IF (uname eq 'w1') or (uname eq 'w2') or (uname eq 'w3') THEN BEGIN               
                  ;Left click, middle click, or scroll down
                  IF (ev.release eq 1) or (ev.release eq 2) or (ev.release eq 16) THEN advance=1
                  ;Right click or scroll up
                  IF (ev.release eq 4) or (ev.release eq 8) THEN advance=-1           
                  IF (ev.type eq 7) THEN advance=-(ev.clicks)   ;For Windows compatibility    
               ENDIF
               IF uname eq 'time' THEN BEGIN
                  widget_control,widget_info(ev.top,find='time'),get_value=texttime
                  time=long(texttime)
                  IF (*pinfo).timeformat eq 1 THEN time=hms2sfm(time)
                  w=where((*p1).time eq time[0])
                  IF w[0] ne -1 THEN i=w[0]
               ENDIF
               IF uname eq 'wt' THEN BEGIN   ;Left click on the wt window moves to the click location
                  IF (ev.release eq 1) THEN i=ev.x/float(screen_x) * n_elements((*p1).time)
                  IF (ev.release eq 8) THEN advance =-1
                  IF (ev.release eq 16) THEN advance=1  
                  IF (ev.type eq 7) THEN advance=-(ev.clicks)   ;For Windows compatibility
               ENDIF
               update=1
            ENDIF
            
            ;Update the plot if flagged
            IF update THEN BEGIN
               i=i + advance > 0 < (n_elements((*p1).time)-1)         
   
               i1=i1 < (n_elements((*p1).time)-2) > 0
               i2=i2 > (i1+1) < (n_elements((*p1).time)-1)
               (*pinfo).i=i
               (*pinfo).i1=i1
               (*pinfo).i2=i2
               ;Update the time indicator
               IF (*pinfo).timeformat eq 1 THEN texttime=sfm2hms((*p1).time[i]) ELSE texttime=(*p1).time[i]
               widget_control,widget_info(ev.top,find='time'),set_value=strtrim(string(long(texttime)),2)
               
               ;Update the time plot
               tsbmp=(*pinfo).bmp
               it=((*p1).time[i]-(*p1).op.starttime)/((*p1).op.stoptime-(*p1).op.starttime)*(screen_x-1)
               it1=((*p1).time[i1]-(*p1).op.starttime)/((*p1).op.stoptime-(*p1).op.starttime)*(screen_x-1)
               it2=((*p1).time[i2]-(*p1).op.starttime)/((*p1).op.stoptime-(*p1).op.starttime)*(screen_x-1)
               tsbmp[it1,indgen(25)*2]=150
               tsbmp[it2,indgen(25)*2]=250
               tsbmp[it,*]=100
               tsbmp[(it+1)<(screen_x-1),*]=70
               tsbmp[(it-1)>0,*]=70
               wset,(*pinfo).wt
               tv,tsbmp
         
               ;Plot new data         
               soda2_windowplot,ev.top,p1,pinfo,pop,pmisc
            ENDIF
        END
        ;====================================================================================================
        uname eq 'tab': BEGIN
           ;The current tab has been changed
           soda2_windowplot,ev.top,p1,pinfo,pop,pmisc
        END
        ;====================================================================================================
        uname eq 'reset_range': BEGIN
           (*pinfo).i1=0
           (*pinfo).i2=n_elements((*p1).time)-1
            i=(*pinfo).i
            it=((*p1).time[i]-(*p1).op.starttime)/((*p1).op.stoptime-(*p1).op.starttime)*(screen_x-1)
            tsbmp=(*pinfo).bmp
            tsbmp[0,indgen(25)*2]=150
            tsbmp[screen_x-1,indgen(25)*2]=250
            tsbmp[it,*]=100
            tsbmp[(it+1)<(screen_x-1),*]=70
            tsbmp[(it-1)>0,*]=70
            wset,(*pinfo).wt
            tv,tsbmp
          soda2_windowplot,ev.top,p1,pinfo,pop,pmisc
        END
        ;====================================================================================================
        uname eq 'properties': BEGIN
           soda2_properties,p1,pop,ev.top
        END
        ;====================================================================================================
        uname eq 'quit': BEGIN
           IF ptr_valid(p1) THEN ptr_free,p1   
           IF ptr_valid(pinfo) THEN ptr_free,pinfo 
           WIDGET_CONTROL, ev.TOP, /DESTROY
        END
        ;====================================================================================================
        uname eq 'timeformat': BEGIN
           IF (*pinfo).timeformat eq 1 THEN BEGIN
              (*pinfo).timeformat=0 
              widget_control,widget_info(ev.top,find='timeformat'),set_value='SFM'
           ENDIF ELSE BEGIN
              (*pinfo).timeformat=1
              widget_control,widget_info(ev.top,find='timeformat'),set_value='HMS'
           ENDELSE
           ;Update time
           IF (*pinfo).timeformat eq 1 THEN texttime=sfm2hms((*p1).time[(*pinfo).i]) ELSE texttime=(*p1).time[(*pinfo).i]
           widget_control,widget_info(ev.top,find='time'),set_value=strtrim(string(long(texttime)),2)

        END
        ;====================================================================================================
        uname eq 'png': BEGIN
           tabnum=widget_info(widget_info(ev.top,find='tab'),/tab_current) 
           wset,(*pinfo).wid[tabnum]
           image=tvrd(/true)           
           
           ;set_plot,'z'
           ;device,set_resolution=[1000,1000]
           ;sid_windowplot,ev.top,p1,pinfo,/noset           
           ;image=tvrd() 
           ;set_plot,'x'  
           

           IF tabnum eq 0 THEN base='psd'
           IF tabnum eq 1 THEN base='particles'
           IF tabnum eq 2 THEN base='timing'
           IF tabnum eq 3 THEN BEGIN  ;Use the top plot to name it
              widget_control,widget_info(ev.top,find='ts_type1'),get_value=plottype
              id=widget_info(widget_info(ev.top,find='ts_type1'),/droplist_select)
              base=strlowcase(strcompress(plottype[id],/remove_all))
           ENDIF
           
           file=base+'_'+(*pop).date+'_'+(*pop).probetype+'_'+strtrim(string(sfm2hms((*p1).time[(*pinfo).i])),2)
           IF file_test((*pinfo).outdir,/write) eq 0 THEN $
               (*pinfo).outdir=dialog_pickfile(/read,/directory,title='Select output directory',dialog_parent=widget_info(ev.top,find='tab'))
           write_png,(*pinfo).outdir+file+'.png',image
           dummy=dialog_message('Wrote '+(*pinfo).outdir+file+'.png',dialog_parent=widget_info(ev.top,find='tab'),/info)
        END
        ;====================================================================================================
        (uname eq 'ts_type1') or (uname eq 'ts_type2') or (uname eq 'ts_units'): BEGIN
           ;The time series plot selection changed
           soda2_windowplot,ev.top,p1,pinfo,pop,pmisc
        END
        ;====================================================================================================
        (uname eq 'nextpanel'): BEGIN
           ;Show next panel of images
           (*pinfo).panelstart=(*pinfo).panelstart+1
           soda2_windowplot,ev.top,p1,pinfo,pop,pmisc
        END
        ;====================================================================================================
        (uname eq 'previouspanel'): BEGIN
           ;Show previous panel of images
           (*pinfo).panelstart=((*pinfo).panelstart-1) > 0
           soda2_windowplot,ev.top,p1,pinfo,pop,pmisc
        END
        ;====================================================================================================
        (strmid(uname,0,9) eq 'massparam'): BEGIN
           ;Adjust mass/size param
           ;Make all bulk computations starting at 100um
           CASE uname OF
              'massparam_bf':BEGIN
                 a=0.00294 & b=1.9
              END
              'massparam_crystal':BEGIN
                 a=0.0061 & b=2.05
              END
              'massparam_2012':BEGIN
                 a=0.00528 & b=2.1
              END
           ENDCASE
           binstart=min(where((*pop).endbins ge 100))         
           bulk=compute_bulk_simple((*p1).conc1d,(*pop).endbins,binstart=binstart,ac=a,bc=b)
           (*p1).iwc=bulk.iwc
           (*p1).dmass=bulk.dmass
           (*p1).dbz=bulk.dbz
           (*p1).mvd=bulk.mvd
           (*p1).mnd=bulk.mnd
           soda2_windowplot,ev.top,p1,pinfo,pop,pmisc
        END        
        ;====================================================================================================
        (uname eq 'color_invert'): BEGIN
           IF !p.background eq 255 THEN BEGIN
              !p.background=0
              !p.color=255
              !p.thick=1
           ENDIF ELSE BEGIN
              !p.background=255
              !p.color=0
              !p.thick=2
           ENDELSE         
           soda2_windowplot,ev.top,p1,pinfo,pop,pmisc
        END
        ;====================================================================================================
      
       ELSE: dummy=0
    ENDCASE
END



PRO soda2_browse_cleanup,tlb
    heap_gc  ;Ugly, but it works for now....    
END




PRO soda2_browse
   ;Main GUI for SODA-2 data browsing
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

    device,decompose=0,get_screen_size=screen_size     ;Set to 8-bit color
    
    IF !version.os_family eq 'windows' THEN widget_control,default_font='Helvetica*fixed*12'
    IF !version.os_family eq 'unix' THEN widget_control,default_font='-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1' ;use xlsfonts to see more
    ;IF !version.os_family eq 'unix' THEN widget_control,default_font='-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1'
    !except=0  ;Suppress floating point error messages
    
    ;Make sure GUI will fit on the screen
    screen_x=(screen_size[0]-50)  <900
    screen_y=(screen_size[1]-300) <900
    
    ;Main widget
    base = WIDGET_BASE(COLUMN=1,title='Browse Processed Data',MBar=menubarID)
    fileID=widget_button(menubarID, value='File', /menu,uname='base') ;uvalue=pinfo,
    loadID=widget_button(fileID, value='Load...',uname='load')
    propID=widget_button(fileID, value='Properties',uname='properties',sensitive=0)
    colorID=widget_button(fileID, value='Invert colors',uname='color_invert',sensitive=0)
    massparamID=widget_button(fileID, value='Change mass/size parameterization',uname='massparammenu',sensitive=0,/menu)
    mp1ID=widget_button(massparamID, value='Brown/Francis',uname='massparam_bf')
    mp2ID=widget_button(massparamID, value='CRYSTAL',uname='massparam_crystal')
    mp3ID=widget_button(massparamID, value='Heymsfield 2012',uname='massparam_2012')
    quitID=widget_button(fileID, value='Quit',uname='quit')

    
    tab=widget_tab(base,uname='tab',sensitive=0,uvalue=[screen_x, screen_y])
    
    ;Tab 1
    drawbase=widget_base(tab,row=1,title='Distributions',uname='tab1')
    plot1=widget_draw(drawbase,xsize=screen_x,ysize=screen_y,uname='w1',/button_events,/wheel_events)

    ;Tab 2
    drawbase2=widget_base(tab,column=1,title='Particles',uname='tab2')
    plot2=widget_draw(drawbase2,xsize=screen_x,ysize=screen_y,uname='w2',/button_events,/wheel_events)
    drawbase2b=widget_base(drawbase2,row=1,/base_align_center)
    previouspanel=widget_button(drawbase2b,uname='previouspanel',value='<--',/frame)
    panelcount=widget_label(drawbase2b,uname='panelcount',value=' Panel  1 of  1  ')
    nextpanel=widget_button(drawbase2b,uname='nextpanel',value='-->',/frame)
    
    ;Tab 3
    drawbase3=widget_base(tab,row=1,title='Timing/Diodes',uname='tab3')
    plot3=widget_draw(drawbase3,xsize=screen_x,ysize=screen_y-30,uname='w3',/button_events,/wheel_events)
       
    ;Tab 5
    drawbase5=widget_base(tab,column=1,title='Time Series',uname='tab5')
    plot5=widget_draw(drawbase5,xsize=screen_x,ysize=screen_y-30,uname='w5',/button_events,/motion_events)
    drawbase5b=widget_base(drawbase5,row=1)
    plottypes=['Total Concentration','IWC','dBZ','Diameter',$
        'Color Concentration','Color Interarrival', 'Color Interarrival Accepted','Color Diode Histogram',$
       'Color Area Ratio','Color Aspect Ratio','Color Orientation','Rejection Codes',$
       'Particle Counts']
    ts_type1=widget_droplist(drawbase5b,uname='ts_type1',value=plottypes,title='Plot 1',/frame)
    ts_type2=widget_droplist(drawbase5b,uname='ts_type2',value=plottypes,title='Plot 2',/frame)
    widget_control,ts_type2,set_droplist_select=1
    ts_units=widget_droplist(drawbase5b,uname='ts_units',value=['Hours','Seconds'],title='X-units',/frame)
    reset_range=widget_button(drawbase5b,uname='reset_range',value=' Reset Range ',/frame)
    
    
    
    ;Time series bar
    tsbase=widget_base(base,row=1)
    tsID=widget_draw(tsbase,xsize=screen_x,ysize=50,uname='wt',/button_events,/motion_events,/wheel_events,sensitive=0,frame=3)
    
    ;Time indicator and other info
    timebarbase=widget_base(base,row=1)
    timeID=widget_text(timebarbase,uname='time',value='',xsize=8,/editable,sensitive=0)
    timeformatID=widget_button(timebarbase,uname='timeformat',value=' HMS ',sensitive=0)
    pngID=widget_button(timebarbase,uname='png',value=' Create PNG ',sensitive=0)
    infoID=widget_label(timebarbase,uname='filedisplay',value='',xsize=300,ysize=25)
    
    loadct,39    ;A color table that works for Linux....
    tvlct,r,g,b,/get
    ;r=r/1.5 & g=g/1.5 & b=b/1.5
    r[1]=220 & g[1]=220 & b[1]=220  ;add a grey color
    r[2]=100 & g[2]=100 & b[2]=250  ;add three blue shades for images
    r[3]=000 & g[3]=000 & b[3]=200  ;add three blue shades for images
    r[4]=000 & g[4]=000 & b[4]=000  ;add three blue shades for images
    tvlct,r,g,b
    !p.background=255
    !p.color=0
    !p.charsize=1.5
    !p.thick=2
    
    WIDGET_CONTROL, base, /REALIZE
    XMANAGER, 'soda2_browse', base, cleanup='soda2_browse_cleanup', /no_block
END
