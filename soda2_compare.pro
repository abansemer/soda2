PRO soda2_compare_event, ev
   ;Event handler for SODA2 compare GUI
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   uname=widget_info(ev.id,/uname)
   ;Get pointers to the restored data
   widget_control,widget_info(ev.top,find='base'),get_uvalue=pinfo
   p1=(*pinfo).p1
    
   CASE 1 OF
      ;====================================================================================================
      (uname eq 'wt')  or (uname eq 'time') or (uname eq 'plot'):BEGIN      
         advance=0
         i=(*pinfo).i
         update=0  ;Flag to enact updates
         p1=(*pinfo).p1

         IF uname eq 'time' THEN BEGIN
            widget_control,widget_info(ev.top,find='time'),get_value=texttime
            time=long(texttime)
            IF (*pinfo).timeformat eq 1 THEN time=hms2sfm(time)
            w=where((*p1).time eq time[0])
            IF w[0] ne -1 THEN i=w[0]
            update=1
         ENDIF ELSE BEGIN
            ;In other tabs just use release events
            IF ((ev.release ne 0) or (ev.type eq 7)) THEN BEGIN
               IF (uname eq 'plot') THEN BEGIN               
                  ;Left click, middle click, or scroll down
                  IF (ev.release eq 1) or (ev.release eq 2) or (ev.release eq 16) THEN advance=1
                  ;Right click or scroll up
                  IF (ev.release eq 4) or (ev.release eq 8) THEN advance=-1           
                  IF (ev.type eq 7) THEN advance=-(ev.clicks)   ;For Windows compatibility    
               ENDIF
               IF uname eq 'wt' THEN BEGIN   ;Left click on the wt window moves to the click location
                  IF (ev.release eq 1) THEN i=ev.x/float((*pinfo).screen_x) * n_elements((*p1).time)
                  IF (ev.release eq 8) THEN advance =-1
                  IF (ev.release eq 16) THEN advance=1  
                  IF (ev.type eq 7) THEN advance=-(ev.clicks)   ;For Windows compatibility
               ENDIF
               update=1
            ENDIF
         ENDELSE
         ;Update the plot if flagged
         IF update THEN BEGIN
            i=i + advance > 0 < (n_elements((*p1).time)-1)         

            (*pinfo).i=i
            ;Update the time indicator
            IF (*pinfo).timeformat eq 1 THEN texttime=sfm2hms((*p1).time[i]) ELSE texttime=(*p1).time[i]
            widget_control,widget_info(ev.top,find='time'),set_value=strtrim(string(long(texttime)),2)
            
            ;Update the time plot
            tsbmp=(*pinfo).bmp
            it=((*p1).time[i]-(*p1).op.starttime)/((*p1).op.stoptime-(*p1).op.starttime)*((*pinfo).screen_x-1)
            tsbmp[it,*]=100
            tsbmp[(it+1)<((*pinfo).screen_x-1),*]=70
            tsbmp[(it-1)>0,*]=70
            wset,(*pinfo).wt
            tv,tsbmp
      
            ;Plot new data         
            soda2_compareplot, pinfo
         ENDIF
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
         image=tvrd(/true)           

         base='comparison'
         file=base+'_'+(*p1).op.date+'_'+strtrim(string(sfm2hms((*p1).time[(*pinfo).i]),format='(i06)'),2)
         IF file_test((*pinfo).outdir,/write) eq 0 THEN $
            (*pinfo).outdir=dialog_pickfile(/read,/directory,title='Select output directory',dialog_parent=widget_info(ev.top,find='tab'))
         write_png,(*pinfo).outdir+file+'.png',image
         dummy=dialog_message('Wrote '+(*pinfo).outdir+file+'.png',dialog_parent=widget_info(ev.top,find='tab'),/info)
      END
      ;====================================================================================================
      (uname eq 'ts_type1') or (uname eq 'ts_type2') or (uname eq 'ts_units'): BEGIN
         ;The time series plot selection changed
         soda2_compareplot,ev.top,p1,pinfo,pop,pmisc
      END
      ;====================================================================================================
      (strmid(uname,0,9) eq 'massparam'): BEGIN
         ;Adjust mass/size param
         ;Make all bulk computations starting at 100um
         ;Note: may be small differences from original due to declutter application
         p1=(*pinfo).p1
         p2=(*pinfo).p2
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
           'massparam_water':BEGIN
              a=!pi/6 & b=3.0
           END
         ENDCASE
         bulk=compute_bulk_simple((*p1).conc1d,(*p1).op.endbins,ac=a,bc=b,binstart=(*pinfo).binstart)
         (*p1).msdnorm=bulk.msdnorm
         (*p1).iwc=bulk.iwc    ;Update these, will be displayed on the plot
         (*p1).dmedianmass=bulk.dmedianmass

         bulk=compute_bulk_simple((*p2).conc1d,(*p2).op.endbins,ac=a,bc=b,binstart=(*pinfo).binstart)
         (*p2).msdnorm=bulk.msdnorm
         (*p2).iwc=bulk.iwc    ;Update these, will be displayed on the plot
         (*p2).dmedianmass=bulk.dmedianmass
         soda2_compareplot,pinfo
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
         soda2_compareplot, pinfo
      END
      ;====================================================================================================
      (uname eq 'options'): BEGIN
         widget_control,widget_info(ev.top,find='options'),get_value=vals
         (*pinfo).lognormalize=vals[0]
         (*pinfo).xlog=vals[1]
         (*pinfo).ylog=vals[2]         
         soda2_compareplot, pinfo
     END
      ELSE: dummy=0
   ENDCASE
END



PRO soda2_compare_cleanup,tlb
   heap_gc  ;Ugly, but it works for now....    
END

PRO soda2_compareplot, pinfo
   color1=80  ;Blue
   color2=250 ;Red
   !p.multi=[0,2,1]
   i=(*pinfo).i
   p1=(*pinfo).p1
   p2=(*pinfo).p2
   wset,(*pinfo).wid
   allbins=[(*p1).midbins,(*p2).midbins]
  
   ;Normalize plots if flagged
   IF (*pinfo).lognormalize eq 1 THEN BEGIN
      conc1=lognormalize((*p1).conc1d[i,*], (*p1).op.endbins)
      conc2=lognormalize((*p2).conc1d[i,*], (*p2).op.endbins)
      msd1=lognormalize((*p1).msdnorm[i,*], (*p1).op.endbins)
      msd2=lognormalize((*p2).msdnorm[i,*], (*p2).op.endbins)
      concunit='(#/m3/dlogD)'
      msdunit='(g/m3/dlogD)'
   ENDIF ELSE BEGIN
      conc1=(*p1).conc1d[i,*]
      conc2=(*p2).conc1d[i,*]
      msd1=(*p1).msdnorm[i,*]
      msd2=(*p2).msdnorm[i,*]
      concunit='(m!u-4!n)'
      msdunit='(g/m!u4!n)'
   ENDELSE
   
   ;Set axis ranges
   IF (*pinfo).xlog eq 1 THEN padfactor=2.0 ELSE padfactor=1.1
   xrange=[min(allbins)/padfactor, max(allbins)*padfactor]
   IF (*pinfo).ylog eq 1 THEN BEGIN
      yrangeconc=[1e2,1e12]
      yrangemsd=[1e0,1e4]
      IF (*pinfo).lognormalize eq 1 THEN BEGIN
         yrangeconc=[1e0,1e8]
         yrangemsd=[1e-4,1e1]
      ENDIF
   ENDIF ELSE BEGIN
      yrangeconc=[0, max([reform(conc1),reform(conc2)])*1.1]
      yrangemsd=[0, max([reform(msd1),reform(msd2)])*1.1]
   ENDELSE

   ;Left side plot
   plot,(*p1).midbins, conc1, xlog=(*pinfo).xlog, ylog=(*pinfo).ylog, /nodata, xtit='Diameter (microns)', ytit='Concentration '+concunit, xr=xrange, /xs, yr=yrangeconc
   oplot,(*p1).midbins, conc1, color=color1, thick=3
   oplot,(*p2).midbins, conc2, color=color2, thick=2
   oplot,[(*pinfo).crossover,(*pinfo).crossover], [1e-12, 1e12], line=1, thick=1
   legend_old,[(*p1).op2.shortname+(*p1).op2.probeid,(*p2).op2.shortname+(*p2).op2.probeid],line=0,box=0,color=[color1, color2],/top,/right,charsize=1.0,thick=2
   
   ;Right side plot
   plot,(*p1).midbins, msd1, xlog=(*pinfo).xlog, ylog=(*pinfo).ylog, /nodata, xtit='Diameter (microns)', ytit='Mass '+msdunit, xr=xrange, /xs, yr=yrangemsd
   oplot,(*p1).midbins, msd1,color=color1, thick=3
   oplot,(*p2).midbins, msd2, color=color2, thick=2
   oplot,[(*pinfo).crossover,(*pinfo).crossover], [1e-12, 1e12], line=1, thick=1
   legend_old,[string((*p1).iwc[i],format='(f0.2)')+' g/m3 '+string((*p1).dmedianmass[i],format='(f0.1)')+' um',$
               string((*p2).iwc[i],format='(f0.2)')+' g/m3 '+string((*p2).dmedianmass[i],format='(f0.1)')+' um'],$
               line=0,color=[color1, color2],thick=2,/top,/left,box=0,charsize=1.0
END



PRO soda2_compare, fn1, fn2, fn3=fn3, crossover=crossover, binstart=binstart
   ;Main GUI for SODA-2 data browsing
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   device,decompose=0,get_screen_size=screen_size   ;Set to 8-bit color
   device, retain=2   
   loadct,39    ;A color table that works for Linux....
   !p.background=255
   !p.color=0
   !p.charsize=1.5
   !p.thick=2

   IF !version.os_family eq 'windows' THEN widget_control,default_font='Helvetica*fixed*12'
   IF !version.os_family eq 'unix' THEN widget_control,default_font='-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1' ;use xlsfonts to see more
   !except=0  ;Suppress floating point error messages

   ;Make sure GUI will fit on the screen
   screen_x=(screen_size[0]-50)  <900
   screen_y=(screen_size[1]-300) <400

   ;Main widget
   base = WIDGET_BASE(COLUMN=1,title='Compare Processed Data',MBar=menubarID)
   fileID=widget_button(menubarID, value='File', /menu,uname='base') ;uvalue=pinfo,
   colorID=widget_button(fileID, value='Invert colors',uname='color_invert')
   massparamID=widget_button(fileID, value='Change mass/size parameterization',uname='massparammenu',/menu)
   mp1ID=widget_button(massparamID, value='Brown/Francis',uname='massparam_bf')
   mp2ID=widget_button(massparamID, value='CRYSTAL',uname='massparam_crystal')
   mp3ID=widget_button(massparamID, value='Heymsfield 2012',uname='massparam_2012')
   mp4ID=widget_button(massparamID, value='Water',uname='massparam_water')
   quitID=widget_button(fileID, value='Quit',uname='quit')

   ;Main plot
   plot1=widget_draw(base,xsize=screen_x,ysize=screen_y,uname='plot',/button_events,/wheel_events)

   ;Time series bar
   tsbase=widget_base(base,row=1)
   tsID=widget_draw(tsbase,xsize=screen_x,ysize=50,uname='wt',/button_events,/wheel_events,frame=3)

   ;Time indicator and other info
   timebarbase=widget_base(base,row=1)
   timeID=widget_text(timebarbase,uname='time',value='',xsize=8,/editable)
   timeformatID=widget_button(timebarbase,uname='timeformat',value=' HMS ')
   vals=['Log Normalization', 'Log X', 'Log Y']
   paramtype=cw_bgroup(timebarbase, vals, uname='options', /row, /nonexclusive, uval=vals, set_value=[1,1,1])
   pngID=widget_button(timebarbase,uname='png',value=' Create PNG ')

   ;Restore and organize data
   IF n_elements(binstart) eq 0 THEN binstart=1
   restore,fn1    
   a=0.00294 & b=1.9  ;Default to Brown and Francis
   IF data.op.water eq 1 THEN BEGIN & a=!pi/6 & b=3.0 & ENDIF  ;Unless processed with water
   bulk=compute_bulk_simple(data.conc1d,data.op.endbins,ac=a,bc=b,binstart=binstart)
   op2=data.op
   soda2_update_op, op2  ;For back compatibility
   data1=create_struct(data, bulk, 'op2', op2)
   restore,fn2   
   a=0.00294 & b=1.9  ;Default to Brown and Francis
   IF data.op.water eq 1 THEN BEGIN & a=!pi/6 & b=3.0 & ENDIF  ;Unless processed with water
   bulk=compute_bulk_simple(data.conc1d,data.op.endbins,ac=a,bc=b,binstart=binstart)
   op2=data.op
   soda2_update_op, op2  ;For back compatibility
   data2=create_struct(data, bulk, 'op2', op2)
   IF ptr_valid(p1) THEN ptr_free,p1   
   IF ptr_valid(p1) THEN ptr_free,p1   
   IF ptr_valid(pinfo) THEN ptr_free,pinfo
   p1=ptr_new(data1)
   p2=ptr_new(data2)
   IF total(data1.time) ne total(data2.time) THEN BEGIN
      dummy=dialog_message('Time mismatch.  Returning.',dialog_parent=base)
      return
   ENDIF
   
   ;Start GUI
   WIDGET_CONTROL, base, /REALIZE
   XMANAGER, 'soda2_compare', base, cleanup='soda2_compare_cleanup', /no_block

   ;Plot Nt in the time bar window
   widget_control,widget_info(base,find='wt'),get_value=wt
   wset,wt
   plot,smooth(alog10((data1.nt+data2.nt)/2.0),5,/nan),xsty=5,ysty=5,pos=[0,0,1,1],yr=[2,8],/yl,thick=2
   bitmap=tvrd()
   tsbmp=bitmap
   tsbmp[1,*]=100 ;Add a time bar at the beginning
   tsbmp[0,*]=70 
   tsbmp[2,*]=70 
   tv,tsbmp
   
   ;Establish mouse info
   mouse={down:0, pixid:0, wid:0, xsize:0, ysize:0, sx:0, sy:0, dx:0, dy:0}
   pmouse=ptr_new(mouse)

   ;Set up info
   outdir=file_dirname(fn1)+path_sep()           
   widget_control,widget_info(base,find='plot'),get_value=wid
   IF n_elements(crossover) eq 0 THEN crossover=0
   info={i:0L, timeformat:1, wid:wid, wt:wt, p1:p1, p2:p2, pmouse:pmouse, bmp:bitmap, $
         outdir:outdir, screen_x:screen_x, screen_y:screen_y, crossover:crossover, $
         lognormalize:1, xlog:1, ylog:1, binstart:binstart}
   pinfo=ptr_new(info)
   widget_control,widget_info(base,find='base'),set_uvalue=pinfo
   
   ;Plot new data
   soda2_compareplot, pinfo
END
