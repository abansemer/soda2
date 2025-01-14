PRO soda2_windowplot, topid, p1, pinfo, pop, pmisc, noset=noset
   ;Make various plots in the GUI windows
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   !p.multi=[0,1,1,0,0]
   tabnum=widget_info(widget_info(topid,find='tab'),/tab_current)
   IF keyword_set(noset) eq 0 THEN wset,(*pinfo).wid[tabnum]  ;Don't set window if we're in z-buffer for PNGs
   psave=!p
   !p.charsize=!d.y_size/600.0
   erase
   i=(*pinfo).i
   color1=80  ;blue
   color2=150 ;green
   color3=250 ;red
   color4=200 ;dark yellow
   color5=215 ;orange
   color6=110 ;light blue
   color7=45  ;purple
   color8=5   ;dark green (custom added in soda2_browse)

   sizerange=[min((*p1).midbins), max((*p1).midbins)]
   arrange=[0,1]
   armidbins=((*p1).op.arendbins[1:*]+(*p1).op.arendbins[1:*])/2.0

   CASE tabnum OF

      ;------------------------------------------------------
      ;---------------Distributions--------------------------
      ;------------------------------------------------------
      0:BEGIN
         ;---Normalize plots if needed
         IF (*pinfo).xlog eq 1 THEN BEGIN
            conc1=lognormalize((*p1).conc1d[i,*], (*p1).op.endbins)
            msd1=lognormalize((*p1).msdnorm[i,*], (*p1).op.endbins)
            concunit='(#/m3/dlogD)'
            msdunit='(g/m3/dlogD)'
         ENDIF ELSE BEGIN
            conc1=(*p1).conc1d[i,*]
            msd1=(*p1).msdnorm[i,*]
            concunit='(#/m3/m)'
            msdunit='(g/m3/m)'
         ENDELSE

         ;---Set axis ranges
         ;All plots X-range
         binwidth1 = (*p1).op.endbins[1] - (*p1).op.endbins[0]   ;Width of first bin
         IF (*pinfo).xlog eq 1 THEN xrange=[min((*p1).op.endbins)/1.1, max((*p1).op.endbins)*1.1] ELSE $
            xrange=[min((*p1).op.endbins)-binwidth1/2.0, max((*p1).op.endbins)+binwidth1/2.0]>0

         ;PSD Y-range
         IF (*pinfo).ylogpsd eq 1 THEN BEGIN
            yrangepsd=[1e4,1e14]
            IF (*pinfo).xlog eq 1 THEN yrangepsd=[1e2,1e10]  ;Lognormalized
         ENDIF ELSE yrangepsd=[0, max(conc1)*1.1]

         ;MSD Y-range
         IF (*pinfo).ylogmsd eq 1 THEN BEGIN
            yrangemsd=[1e1,1e5]
            IF (*pinfo).xlog eq 1 THEN yrangemsd=[1e-3,1e1]  ;Lognormalized
         ENDIF ELSE yrangemsd=[0, max(msd1)*1.1]

         ;Get tight axes if auto-range is enabled
         IF (*pinfo).autorange eq 1 THEN BEGIN
            wconc=where((conc1 gt 0) and (finite(conc1) eq 1), ng)
            IF ng gt 1 THEN BEGIN  ;Make sure there are at least two good points to begin with
               xrange=[min((*p1).op.endbins), (*p1).op.endbins[max(wconc)+1]+binwidth1/2.0]
               yrangepsd=[min(conc1[wconc]), max(conc1[wconc])]
               yrangemsd=[min(msd1[wconc]), max(msd1[wconc])]
               ;If ylog is enabled then make sure the min/max ranges differ
               IF ((*pinfo).ylogpsd eq 1) and (yrangepsd[0] eq yrangepsd[1]) THEN yrangepsd[0]=yrangepsd[0]*5
               IF ((*pinfo).ylogmsd eq 1) and (yrangemsd[0] eq yrangemsd[1]) THEN yrangemsd[0]=yrangemsd[0]*5
            ENDIF
         ENDIF

         ;---Get mean area/aspect/orientation ratio
         meanar=(*p1).meanar[i,*]
         bad=where(meanar eq 0, nbad)
         IF nbad gt 0 THEN meanar[bad]=!values.f_nan

         IF total(tag_names(*p1) eq 'MEANASPR') THEN meanaspr=(*p1).meanaspr[i,*] ELSE meanaspr=meanar*0
         bad=where(meanaspr eq 0, nbad)
         IF nbad gt 0 THEN meanaspr[bad]=!values.f_nan

         IF total(tag_names(*p1) eq 'ORIENTATION_INDEX') THEN meanoi=(*p1).orientation_index[i,*] ELSE meanoi=meanar*0
         bad=where(meanoi eq 0, nbad)
         IF nbad gt 0 THEN meanoi[bad]=!values.f_nan

         ;---Make stairstep lines
         stairconc = stairsteps(conc1, (*p1).op.endbins)
         stairmsd = stairsteps(msd1, (*p1).op.endbins)
         stairar = stairsteps(meanar, (*p1).op.endbins)
         stairaspr = stairsteps(meanaspr, (*p1).op.endbins)
         stairoi = stairsteps(meanoi, (*p1).op.endbins)
         imin = min(where(stairconc.x ge (*pinfo).minsize)) > 0

         ;---Left side plot (*p1).midbins, conc1
         plot, stairconc.x, stairconc.y, xlog=(*pinfo).xlog, ylog=(*pinfo).ylogpsd, /nodata, xtit='Diameter (microns)', $
            ytit='Concentration '+concunit, xr=xrange, /xs, yr=yrangepsd, position=[0.1,0.45,0.48,0.95], charsize=1
         oplot, stairconc.x, stairconc.y, color=color1, thick=1, line=3
         oplot, stairconc.x[imin:*], stairconc.y[imin:*], color=color1, thick=2

         ;---Right side plot, MSD and MMD
         plot, stairmsd.x, stairmsd.y, xlog=(*pinfo).xlog, ylog=(*pinfo).ylogmsd, /nodata, xtit='Diameter (microns)', $
            ytit='Mass '+msdunit, xr=xrange, /xs, yr=yrangemsd, position=[0.57,0.45,0.95,0.95], /noerase, charsize=1
         oplot, stairmsd.x, stairmsd.y, color=color5, thick=1, line=3
         oplot, stairmsd.x[imin:*], stairmsd.y[imin:*], color=color5, thick=2
         oplot, [(*p1).dmedianmass[i], (*p1).dmedianmass[i]], [1e-10, 1e10], line=2, thick=1

         ;---Area ratio plot
         plot, stairar.x, stairar.y, yr=[0,1.01], position=[0.57,0.07,0.95,0.35], charsize=1, /noerase,$
             xtit='Diameter (microns)', ytit='Particle Shape', /nodata, xr=xrange, /xs, xlog=(*pinfo).xlog, /ys
         oplot, stairoi.x, stairoi.y, color=color4, thick=1, line=3
         oplot, stairoi.x[imin:*], stairoi.y[imin:*], color=color4, thick=2
         oplot, stairaspr.x, stairaspr.y, color=color6, thick=1, line=3
         oplot, stairaspr.x[imin:*], stairaspr.y[imin:*], color=color6, thick=2
         oplot, stairar.x, stairar.y, color=color8, thick=1, line=3
         oplot, stairar.x[imin:*], stairar.y[imin:*], color=color8, thick=2
         legend_old, ['Aspect Ratio','Area Ratio','Orientation'], line=[0,0,0], thick=[2,2,2], $
            color=[color6,color8,color4], box=0, charsize=1.0, pos=[0.55, 0.38], /norm, /horizontal

         ;---Display numeric data
         savesize=!p.charsize  ;Make these a little easier to read
         !p.charsize=1.2
         x = 0.07  & dx = 0.1 & y = 0.10  & dy = 0.04 ;Bottom-left corner of display area
         xyouts,x,y+6*dy,'N!dT!n(#/L):',/norm & xyouts,x+dx,y+6*dy,string((*p1).nt[i]/1.0e3,form='(f0.1)'),/normal
         xyouts,x,y+5*dy,'IWC(g/m!u3!n):',/norm & xyouts,x+dx,y+5*dy,string((*p1).iwc[i],form='(f0.3)'),/normal
         xyouts,x,y+4*dy,'LWC(g/m!u3!n):',/norm & xyouts,x+dx,y+4*dy,string((*p1).lwc[i],form='(f0.3)'),/normal
         xyouts,x,y+3*dy,'dBZ:',/norm & xyouts,x+dx,y+3*dy,string((*p1).dbz[i],form='(f0.1)'),/normal
         xyouts,x,y+2*dy,'MeanD(um):',/norm & xyouts,x+dx,y+2*dy,string((*p1).mnd[i],form='(f0.1)'),/normal
         xyouts,x,y+1*dy,'MMD(um):',/norm & xyouts,x+dx,y+dy,string((*p1).dmedianmass[i],form='(f0.1)'),/normal
         xyouts,x,y+0*dy,'MVD(um):',/norm & xyouts,x+dx,y+0*dy,string((*p1).mvd[i],form='(f0.1)'),/normal
         xyouts,x+0.2,y+4*dy,'Accepted:',/norm & xyouts,x+0.2+dx,y+4*dy,string((*p1).count_accepted[i],form='(i0)'),/normal
         xyouts,x+0.2,y+3*dy,'Rejected:',/norm & xyouts,x+0.2+dx,y+3*dy,string((*p1).count_rejected_total[i],form='(i0)'),/normal
         xyouts,x+0.2,y+2*dy,'Missed:',/norm & xyouts,x+0.2+dx,y+2*dy,string((*p1).count_missed[i],form='(i0)'),/normal
         !p.charsize=savesize
      END

      ;------------------------------------------------------
      ;---------------Particles------------------------------
      ;------------------------------------------------------
      1:BEGIN
         ;Check for missing files
         IF (*pinfo).rawdir eq '' THEN BEGIN
            xyouts,0.5,0.5,'Raw data files not found.',align=0.5,/norm
            return
         ENDIF
         IF (*p1).sodaversion eq 1 THEN BEGIN
            xyouts,0.5,0.5,'SODA-1 processed data, no images available.',align=0.5,/norm
            return
         ENDIF

         ;Update raw directory
         fn=(*pinfo).rawdir + file_basename((*pop).fn)

         ;Buffer indexes for current time period
         ind=where((*p1).ind eq (*pinfo).i,buffcount)

         ;Update stuck bit reference
         (*pmisc).lastdhist=(*p1).dhist[((*pinfo).i-1)>0,*]

         ;Reset scroll bars to bottom-left
         drawid=widget_info(topid,find='w2')
         widget_control, drawid, set_draw_view=[0, 0]

         buffermargin=10
         panelwidth=(*pop).numdiodes+buffermargin
         num2plot=!d.x_size/fix(panelwidth)
         charsize=((*pop).numdiodes/50.0) < 1.0 > 0.8  ;Character size for the timestamp depends on buffer width

         ibuffer=0
         panelstart=(*pinfo).panelstart
         imagestart=(panelstart*num2plot)
         IF imagestart ge buffcount THEN BEGIN
            ;No new images
            (*pinfo).panelstart=(*pinfo).panelstart-1  ;Negate the update from the GUI
            panelstart=(*pinfo).panelstart >0
            imagestart=(panelstart*num2plot)
         ENDIF
         imagestop=((panelstart+1)*num2plot-1) < (buffcount-1)
         FOR i=imagestart, imagestop DO BEGIN
            ;Get bitimage.  A showdividers option of -1 shows white lines.  +1 for blue, +2 for black.
            b=soda2_bitimage(fn[(*p1).currentfile[ind[i]]], (*p1).pointer[ind[i]], pop, pmisc, divider=-(*pinfo).showdividers)
            ;Display buffers
            IF b.rejectbuffer eq 0 THEN BEGIN
               tv, b.bitimage+1b, panelwidth*ibuffer+buffermargin, 20
               xyouts, panelwidth*ibuffer+buffermargin, 10, string(b.time,format='(f8.2)'), /device, charsize=charsize
            ENDIF
            ibuffer=ibuffer+1
         ENDFOR
         ;Update panel counter
         numpanels=buffcount/num2plot
         IF buffcount mod num2plot ne 0 THEN numpanels=numpanels+1
         panelcountid=widget_info(topid,find='panelcount')
         widget_control,panelcountid,set_value='Panel '+strtrim(string(panelstart+1),2) + ' of ' + strtrim(string(floor(numpanels>1)),2)
      END


      ;------------------------------------------------------
      ;---------------Timing---------------------------------
      ;------------------------------------------------------
      2:BEGIN
         !p.multi=[0,1,2,0,0]
         tothist=total((*p1).intspec_all,1)
         plot,(*p1).intmidbins,(*p1).intspec_all[i,*],/xl,/xs,xtit='Interarrival Time (s)',ytit='Count',xr=[1e-7,1.0],/nodata
         oplot,(*p1).intmidbins,tothist/max(tothist)*!y.crange[1]*0.8,color=color3,line=2,thick=1
         oplot,(*p1).intmidbins,(*p1).intspec_all[i,*],color=color1
         oplot,(*p1).intmidbins,(*p1).intspec_accepted[i,*],color=color2
         legend_old,['All Particles','Accepted Particles','Entire Flight (Scaled)'],line=[0,0,2], $
            color=[color1,color2,color3],box=0,charsize=1.0

         IF (*p1).sodaversion eq 2 THEN BEGIN
            tothist=total((*p1).dhist,1)
            plot,findgen((*p1).op.numdiodes),(*p1).dhist[i,*],/xs,xtit='Diode Number',ytit='Shadow Count',/nodata
            oplot,findgen((*p1).op.numdiodes),(*p1).dhist[i,*],color=color1
            oplot,findgen((*p1).op.numdiodes),tothist/max(tothist)*!y.crange[1]*0.8,line=2,thick=1,color=color3
            legend_old,['This Time Period', 'Entire Flight (Scaled)'],line=[0,2],color=[color1,color3],box=0,charsize=1.0,/bottom
         ENDIF ELSE BEGIN
            plot,findgen(n_elements((*p1).dhist)),(*p1).dhist,/xs,xtit='Diode Number',ytit='Shadow Count',/nodata
            oplot,findgen(n_elements((*p1).dhist)),(*p1).dhist, color=color1
            legend_old,['Entire Flight (Scaled)'],line=[0],color=[color1],box=0,charsize=1.5,/bottom
         ENDELSE
      END

      ;------------------------------------------------------
      ;---------------Time Series----------------------------
      ;------------------------------------------------------
      3:BEGIN
         ;Get plot type
         widget_control,widget_info(topid,find='ts_type1'),get_value=plottype
         id=[0,0]
         id[0]=widget_info(widget_info(topid,find='ts_type1'),/droplist_select)
         id[1]=widget_info(widget_info(topid,find='ts_type2'),/droplist_select)
         xunit=widget_info(widget_info(topid,find='ts_units'),/droplist_select)

         ;Get start/stop times
         a=(*pinfo).i1
         b=(*pinfo).i2

         IF xunit eq 1 THEN BEGIN
            !x.title='Time (s)'
            x=(*p1).time[a:b]
            !x.tickformat='(i6)'
         ENDIF ELSE BEGIN
            !x.title='Time'
            x=((*p1).time[a:b]+43200)/86400d
            date_label=label_date(date_form='%H:%I:%S')
            !x.tickformat='label_date'
            !x.tickunits=['Seconds']
         ENDELSE
         !x.style=1
         !p.multi=[2,1,2,0,0]
         ymarginsave=!y.margin
         !y.margin=[4,4]

         ;For contour plots
         nlevels=30
         c_colors=findgen(nlevels)/nlevels * 220 + 30
         FOR p=0,1 DO BEGIN ;make both plots
            CASE plottype[id[p]] OF
               'IWC':BEGIN
                  plot,x,(*p1).iwc[a:b],ytitle='IWC (g/m!u3!n)',/nodata
                  oplot,x,(*p1).iwc[a:b],color=color1
               END
               'LWC':BEGIN
                  plot,x,(*p1).lwc[a:b],ytitle='LWC (g/m!u3!n)',/nodata
                  oplot,x,(*p1).lwc[a:b],color=color1
               END
               'Extinction':BEGIN
                  plot,x,(*p1).area[a:b]*2000,ytitle='Extinction (1/km)',/nodata
                  oplot,x,(*p1).area[a:b]*2000,color=color2
               END
              'dBZ':BEGIN
                  plot,x,(*p1).dbz[a:b],ytitle='Reflectivity (dBZ)',/nodata
                  oplot,x,(*p1).dbz[a:b],color=color1
               END
               'Total Concentration':BEGIN
                  y=(*p1).nt[a:b]/1.0e3
                  IF max(y) lt 10 THEN ylog=0 ELSE ylog=1
                  IF max(y) lt 10 THEN yr=[0, max(y)] ELSE yr=[0.1,max(y,/nan)>1]
                  plot,x,(*p1).nt[a:b]/1.0e3,ytitle='N!lt!n (#/L)',ylog=ylog,yr=yr,/nodata
                  oplot,x,(*p1).nt[a:b]/1.0e3,color=color1
               END
                'Diameter':BEGIN
                  plot,x,(*p1).mnd[a:b],ytitle='Diameter (um)',/yl,yr=sizerange,/ys,/nodata
                  oplot,x,(*p1).mnd[a:b],color=color1
                  oplot,x,(*p1).dmedianmass[a:b],color=color2
                  oplot,x,(*p1).mvd[a:b],color=color3
                  legend_old,['Mean','Median Mass','Median Volume'],line=0,thick=2,color=[color1,color2,color3],$
                     box=0,/top,/right,charsize=1
               END
                'Rejection Codes':BEGIN
                  IF (b-a) lt 50 THEN psym=0 ELSE psym=1   ;Plot symbols if there is a lot of data, otherwise lines
                  total_count=float((*p1).count_rejected_total[a:b]+(*p1).count_accepted[a:b])>1
                  plot,x,(*p1).count_rejected[a:b,1]/total_count*100,ytitle='Percent Rejected',yr=[0,105],/ys,psym=psym,symsize=0.5
                  colorchoices=[color1,color2,color3,color5,color4,color6]
                  FOR i=0,5 DO oplot,x,(*p1).count_rejected[a:b,i]/total_count*100,color=colorchoices[i],psym=psym,symsize=0.5
                  legend_old,['Area Ratio','Interarrival','Size Range','Edge Touch','Cluster','Water'],$
                      line=0,thick=2,color=colorchoices,charsize=1.0,box=0,/right
               END
               'Particle Counts':BEGIN
                  maxy=max([(*p1).count_missed[a:b],(*p1).count_accepted[a:b],(*p1).count_rejected_total[a:b]])
                  plot,x,(*p1).count_missed[a:b],ytitle='Particle Count',yr=[0,maxy]
                  oplot,x,(*p1).count_missed[a:b],color=color4
                  oplot,x,(*p1).count_accepted[a:b],color=color2
                  oplot,x,(*p1).count_rejected_total[a:b],color=color3
                  legend_old,['Accepted','Rejected','Missed'],line=0,color=[color2,color3,color4],box=0,charsize=1,/right
               END
               'Active Time':BEGIN
                  maxy=max((*p1).activetime[a:b]) < ((*p1).op.rate*2)
                  plot,x,(*p1).activetime[a:b],ytitle='Active Time',yr=[0,maxy*1.1],/ys,/nodata
                  oplot,x,(*p1).activetime[a:b],color=color1
                  oplot,x,fltarr(n_elements(x))+(*p1).op.rate,color=color3,line=2
                  legend_old,['Measured','Rate'],line=[0,2],color=[color1,color3],box=0,/bottom,/right,charsize=1
               END
              'TAS':BEGIN
                  maxy=max((*p1).tas[a:b])
                  IF total(tag_names(*p1) eq 'PROBETAS') THEN maxy=max([(*p1).tas[a:b], (*p1).probetas[a:b]])
                  plot,x,(*p1).tas[a:b],ytitle='True Air Speed (m/s)',yr=[0,maxy*1.1],/ys,/nodata
                  oplot,x,(*p1).tas[a:b],color=color1
                  IF total(tag_names(*p1) eq 'PROBETAS') THEN BEGIN
                     oplot, x,(*p1).probetas[a:b],color=color3,line=2
                     legend_old,['Aircraft','Probe'],line=[0,2],color=[color1,color3],box=0,/bottom,/right,charsize=1
                  ENDIF
               END
              'Diode Voltages':BEGIN
                  maxy=5
                  ss=size((*p1).house.volts, /dim)
                  colorarray=[color3, color5, color4, 0, color2, color1, color7]  ;Put these in a rainbow order array
                  IF ss[1] eq 3 THEN colorarray=[color1, color2, color3]  ;Make clearer when only 3
                  plot,x,(*p1).house.volts[a:b,0],ytitle='Diode Volts',yr=[0,maxy],/ys,/nodata
                  FOR i=0, ss[1]-1 DO oplot, x, (*p1).house.volts[a:b,i], color=colorarray[i]
                  legend_old,string((*p1).house.diodes, format='(i3)'),line=0,color=colorarray[0:ss[1]-1],box=1,$
                     /bottom,/right,/clear,charsize=1.0,thick=2
               END
              'Probe Temperature':BEGIN
                  ;Set for SPEC probes for now, may need to eventually adjust
                  colorarray=[color3, color5, color4, 0, color2, color1, color7]  ;Put these in a rainbow order array
                  temps2plot=[0,2,6,7,9,10,12]  ;More available for SPEC
                  IF (*pop).probetype eq 'CIP' THEN temps2plot=[0,1,2,3,4]
                  IF (*pop).probetype eq '3VCPI' THEN temps2plot=[0,1,2,3,4,5]
                  IF (*pop).probetype eq '1D2D' THEN temps2plot=[0]
                  miny=-20 & maxy=40
                  plot,x,(*p1).house.temp[a:b,0],ytitle='Temperature (C)',yr=[miny,maxy],/ys,/nodata
                  FOR i=0, n_elements(temps2plot)-1 DO oplot, x, (*p1).house.temp[a:b,temps2plot[i]], color=colorarray[i]
                  legend_old,(*p1).house.tempid[temps2plot],line=0,color=colorarray[0:n_elements(temps2plot)-1],box=1,$
                     /bottom,/right,/clear,charsize=1.0,thick=2
               END
               'Probe Settings':BEGIN
                  ;1D2D probe settings
                  IF (*pop).probetype eq '1D2D' THEN BEGIN
                     colorarray=[color7, color5, color4, color2, color1, color3]  ;Put these in a rainbow order array
                     plot,x,(*p1).house.leftreject[a:b],ytitle='Status',yr=[-0.1,1.2],/ys,/nodata,yticks=1, $
                        ytickv=[0,1],ytickname=['Off','On']
                     oplot,x,((*p1).house.smallreject[a:b]<1)+0.1,color=colorarray[0]
                     oplot,x,((*p1).house.largereject[a:b]<1)+0.08,color=colorarray[1]
                     oplot,x,(*p1).house.leftreject[a:b]+0.06,color=colorarray[2]
                     oplot,x,(*p1).house.rightreject[a:b]+0.04,color=colorarray[3]
                     oplot,x,((*p1).house.dofnumreject[a:b]<1)+0.02,color=colorarray[4]
                     oplot,x,((*p1).house.dofpercent[a:b]<1)+0.0,color=colorarray[5]
                     legend_old,['Small Reject','Large Reject','Left Reject','Right Reject','Num Reject','Ratio Reject'], $
                        line=0,color=colorarray[0:5],box=1,/bottom,/right,/clear,charsize=1.0,thick=2
                  ENDIF ELSE plot, x, x*0
               END
               'Color Concentration':BEGIN
                   conc=alog10((*p1).conc1d[a:b,*] > 1)
                   wc = where(((*p1).conc1d gt 0) and (finite((*p1).conc1d) eq 1))
                   medianlog = round(median(alog10((*p1).conc1d[wc])))
                   zrange = [medianlog-2, medianlog+4]  ;Six orders of mag, was [4,12]
                   IF (*pinfo).autorange eq 1 THEN BEGIN
                      h = histogram(conc, min=1, max=12)    ;min=1 to avoid zeros, added back in zrange assignment
                      wh = where(h/total(h) gt 0.01, nwh)   ;Get range that contains at least 1% of particles
                      IF nwh gt 1 THEN zrange = [min(wh)+1, max(wh)+2]
                   ENDIF
                   contour,conc,x,(*p1).midbins,/cell,nlevels=nlevels,ytitle='Diameter (um)',/yl,/ys,yr=sizerange, $
                      zr=zrange,/zs,c_colors=c_colors ;min_val=max(conc)-10,

                   barposx=[0.3,0.7]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[1.01,1.04]*(!y.window[1]-!y.window[0]) + !y.window[0]
                   bar=findgen(nlevels+1)/nlevels*(zrange[1]-zrange[0])+zrange[0]
                   xsave=!x  ;Needed so clicking in plot works later
                   contour,[[bar],[bar]],findgen(nlevels+1),[0,1],/cell,nlevels=nlevels,$
                      position=[barposx[0],barposy[0],barposx[1],barposy[1]],$
                      xtickname=string(zrange, form='(i2)') ,xstyle=5,ystyle=5,zr=zrange,/zs,/noerase,noclip=1,c_colors=c_colors
                   !x=xsave
                   xyouts, barposx, barposy[1]+0.01, /norm,align=0.5, $
                      ['10!u'+strtrim(string(zrange[0]),2)+'!n','10!u'+strtrim(string(zrange[1]),2)+'!n']
                   xyouts, mean(barposx), barposy[1]+0.01, 'Concentration (m!u-4!n)',/norm,align=0.5
               END
               'Color Area Ratio':BEGIN
                   conc=(*p1).meanar[a:b,*]
                   zrange=[0,0.8]
                   bartitle='Area Ratio'
                   IF (*pop).smethod eq 'fastcircle_aspectratio' THEN bartitle = 'Aspect Ratio'
                   contour,conc,x,(*p1).midbins,/cell,nlevels=nlevels,ytitle='Diameter (um)',/yl,/ys,yr=sizerange, $
                      zr=zrange,c_colors=c_colors

                   barposx=[0.3,0.7]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[1.01,1.04]*(!y.window[1]-!y.window[0]) + !y.window[0]
                   bar=findgen(nlevels+1)/nlevels*(zrange[1]-zrange[0])+zrange[0]
                   xsave=!x
                   contour,[[bar],[bar]],findgen(nlevels+1),[0,1],/cell,nlevels=nlevels,$
                      position=[barposx[0],barposy[0],barposx[1],barposy[1]],$
                      xtickname=['4','12'] ,xstyle=5,ystyle=5,zr=zrange,/noerase,noclip=0,c_colors=c_colors
                   !x=xsave
                   xyouts, barposx, barposy[1]+0.01, string(zrange, form='(f3.1)'),/norm,align=0.5
                   xyouts, mean(barposx), barposy[1]+0.01, bartitle ,/norm,align=0.5
               END
               'Color Aspect Ratio':BEGIN
                   conc=(*p1).meanaspr[a:b,*]
                   zrange=[0,0.9]
                   bartitle='Aspect Ratio'
                   contour,conc,x,(*p1).midbins,/cell,nlevels=nlevels,ytitle='Diameter (um)',/yl,/ys,yr=sizerange, $
                      zr=zrange,c_colors=c_colors

                   barposx=[0.3,0.7]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[1.01,1.04]*(!y.window[1]-!y.window[0]) + !y.window[0]
                   bar=findgen(nlevels+1)/nlevels*(zrange[1]-zrange[0])+zrange[0]
                   xsave=!x
                   contour,[[bar],[bar]],findgen(nlevels+1),[0,1],/cell,nlevels=nlevels,$
                      position=[barposx[0],barposy[0],barposx[1],barposy[1]],$
                      xtickname=['4','12'] ,xstyle=5,ystyle=5,zr=zrange,/noerase,noclip=0,c_colors=c_colors
                   !x=xsave
                   xyouts, barposx, barposy[1]+0.01, string(zrange, form='(f3.1)'),/norm,align=0.5
                   xyouts, mean(barposx), barposy[1]+0.01, bartitle ,/norm,align=0.5
               END
               'Color Mass Distribution':BEGIN
                   conc=(lognormalize((*p1).msdnorm[a:b,*],(*p1).op.endbins))
                   zrange=[0.001,max(conc)/1.5]
                   zlog=0
                   IF zlog eq 1 THEN BEGIN
                     conc=alog10(conc)
                     zrange=alog10(zrange)
                   ENDIF
                   contour,conc,x,(*p1).midbins,/cell,nlevels=nlevels,ytitle='Diameter (um)',/ys,/yl,yr=sizerange, $
                      zr=zrange,c_colors=c_colors ;min_val=max(conc)-10,

                   barposx=[0.3,0.7]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[1.01,1.04]*(!y.window[1]-!y.window[0]) + !y.window[0]
                   bar=findgen(nlevels+1)/nlevels*(zrange[1]-zrange[0])+zrange[0]
                   xsave=!x  ;Needed so clicking in plot works later
                   contour,[[bar],[bar]],findgen(nlevels+1),[0,1],/cell,nlevels=nlevels,$
                      position=[barposx[0],barposy[0],barposx[1],barposy[1]],$
                      xtickname=string(zrange, form='(i2)') ,xstyle=5,ystyle=5,zr=zrange,/noerase,noclip=1,c_colors=c_colors
                   !x=xsave
                   xyouts, barposx, barposy[1]+0.01, [string(zrange[0], form='(f4.1)'),string(zrange[1], $
                      form='(f4.1)')],/norm,align=0.5
                   xyouts, mean(barposx), barposy[1]+0.01, 'Mass (g/m!u3!n/dlogD)',/norm,align=0.5
               END
               'Color Orientation':BEGIN
                   conc=(*p1).orientation_index[a:b,*]
                   zrange=[0,0.4]
                   bartitle='Orientation Index'
                   contour,conc,x,(*p1).midbins,/cell,nlevels=nlevels,ytitle='Diameter (um)',/yl,/ys,yr=sizerange, $
                      zr=zrange,c_colors=c_colors

                   barposx=[0.3,0.7]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[1.01,1.04]*(!y.window[1]-!y.window[0]) + !y.window[0]
                   bar=findgen(nlevels+1)/nlevels*(zrange[1]-zrange[0])+zrange[0]
                   xsave=!x
                   contour,[[bar],[bar]],findgen(nlevels+1),[0,1],/cell,nlevels=nlevels,$
                      position=[barposx[0],barposy[0],barposx[1],barposy[1]],$
                      xtickname=['4','12'] ,xstyle=5,ystyle=5,zr=zrange,/noerase,noclip=0,c_colors=c_colors
                   !x=xsave
                   xyouts, barposx, barposy[1]+0.01, string(zrange, form='(f3.1)'),/norm,align=0.5
                   xyouts, mean(barposx), barposy[1]+0.01, bartitle ,/norm,align=0.5
               END
               'Color Interarrival':BEGIN
                   conc=(*p1).intspec_all[a:b,*]
                   zrange=[0,max(conc)]
                   intrange=[min((*p1).intmidbins),max((*p1).intmidbins)]
                   contour,conc,x,(*p1).intmidbins,/cell,nlevels=nlevels,ytitle='Interarrival Time (s)',/yl,/ys, $
                      yr=intrange,zr=zrange,c_colors=c_colors

                   barposx=[0.3,0.7]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[1.01,1.04]*(!y.window[1]-!y.window[0]) + !y.window[0]
                   bar=findgen(nlevels+1)/nlevels*(zrange[1]-zrange[0])+zrange[0]
                   xsave=!x
                   contour,[[bar],[bar]],findgen(nlevels+1),[0,1],/cell,nlevels=nlevels,$
                      position=[barposx[0],barposy[0],barposx[1],barposy[1]],$
                      xtickname=['4','12'] ,xstyle=5,ystyle=5,zr=zrange,/noerase,noclip=0,c_colors=c_colors
                   !x=xsave
                   xyouts, barposx, barposy[1]+0.01, string(zrange, form='(i5)'),/norm,align=0.5
                   xyouts, mean(barposx), barposy[1]+0.01, 'Particle Count',/norm,align=0.5
               END
               'Color Interarrival Accepted':BEGIN
                   conc=(*p1).intspec_accepted[a:b,*]
                   zrange=[0,max(conc)]
                   intrange=[min((*p1).intmidbins),max((*p1).intmidbins)]
                   contour,conc,x,(*p1).intmidbins,/cell,nlevels=nlevels,ytitle='Interarrival Time Accepted Particles (s)',$
                      /yl,/ys,yr=intrange,zr=zrange,c_colors=c_colors

                   barposx=[0.3,0.7]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[1.01,1.04]*(!y.window[1]-!y.window[0]) + !y.window[0]
                   bar=findgen(nlevels+1)/nlevels*(zrange[1]-zrange[0])+zrange[0]
                   xsave=!x
                   contour,[[bar],[bar]],findgen(nlevels+1),[0,1],/cell,nlevels=nlevels,$
                      position=[barposx[0],barposy[0],barposx[1],barposy[1]],$
                      xtickname=['4','12'] ,xstyle=5,ystyle=5,zr=zrange,/noerase,noclip=0,c_colors=c_colors
                   !x=xsave
                   xyouts, barposx, barposy[1]+0.01, string(zrange, form='(i5)'),/norm,align=0.5
                   xyouts, mean(barposx), barposy[1]+0.01, 'Particle Count',/norm,align=0.5
               END
               'Color Diode Histogram':BEGIN
                   conc=(*p1).dhist[a:b,*]
                   zrange=[0,max(conc)]
                   yaxis=findgen((*p1).op.numdiodes)
                   drange=[0,max(yaxis)]
                   contour,conc,x,yaxis,/cell,nlevels=nlevels,ytitle='Diode Number',/ys,yr=drange,zr=zrange,c_colors=c_colors

                   barposx=[0.3,0.7]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=([1.01,1.04])*(!y.window[1]-!y.window[0]) + !y.window[0]
                   bar=findgen(nlevels+1)/nlevels*(zrange[1]-zrange[0])+zrange[0]
                   xsave=!x
                   contour,[[bar],[bar]],findgen(nlevels+1),[0,1],/cell,nlevels=nlevels,$
                      position=[barposx[0],barposy[0],barposx[1],barposy[1]],$
                      xstyle=5,ystyle=5,zr=zrange,/noerase,noclip=0,c_colors=c_colors,$
                      xticks=1,yticks=1,xtickname=['x','x',' '],ytickname=[' ',' '],xtit=' '
                   !x=xsave
                   xyouts, barposx, barposy[1]+0.01, strtrim(string(zrange, form='(i5)'),2),/norm,align=0.5
                   xyouts, mean(barposx), barposy[1]+0.01, 'Diode Shadow Counts',/norm,align=0.5
               END
               ELSE:dummy=0
            ENDCASE
         ENDFOR
         !x.style=0 & !x.tickformat='' & !x.title='' & !x.tickunits='' & !x.ticks=0 & !p.multi=[0,1,1,0,0]  & !y.margin=ymarginsave
      END
   ENDCASE
   !p.charsize=psave.charsize
END
