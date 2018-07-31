PRO soda2_windowplot,topid,p1,pinfo,pop,pmisc,noset=noset
   ;Make various plots in the GUI windows
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   !p.multi=[0,1,1,0,0]
   tabnum=widget_info(widget_info(topid,find='tab'),/tab_current) 
   IF keyword_set(noset) eq 0 THEN wset,(*pinfo).wid[tabnum]  ;Don't set window if we're in z-buffer for PNGs
   erase
   i=(*pinfo).i
   color1=80  ;blue
   color2=150 ;green
   color3=250 ;red
   color4=190 ;yellow
   color5=215 ;orange
   color6=110 ;light blue
   
   sizerange=[min((*p1).midbins), max((*p1).midbins)]
   arrange=[0,1]
   armidbins=((*p1).op.arendbins[1:*]+(*p1).op.arendbins[1:*])/2.0
   !p.charsize=1.5
   
   CASE tabnum OF
      
      ;------------------------------------------------------
      ;---------------Distributions--------------------------
      ;------------------------------------------------------
      0:BEGIN 
         spec=(*p1).spec2d[i,*,*]
         IF max(spec) gt 10 THEN plotarray=alog10(spec)>1 ELSE plotarray=spec
         contour,plotarray,(*p1).midbins,armidbins,/cell,nlevels=40,position=[0.4,0.4,0.95,0.95],$
            /xl,/xs,/ys,xr=sizerange,yr=arrange,title='Image Counts'
         plot,(*p1).midbins,(*p1).conc1d[i,*],/xl,/yl,/xs,xr=sizerange,yr=[1e4,1e12],position=[0.4,0.07,0.95,0.35],/noerase,$
             xtit='Diameter (um)',ytit='Concentration (#/m!u4!n)',/nodata
         oplot,(*p1).midbins,(*p1).conc1d[i,*],color=color1
         plot,total((*p1).spec2d[i,*,*],2),armidbins,/ys,yr=arrange,position=[0.07,0.4,0.35,0.95],/noerase,$
             xtit='Count',ytit='Area Ratio',/nodata
         oplot,total((*p1).spec2d[i,*,*],2),armidbins,color=color1
    
         xyouts,0.07,0.30,'N!dT!n(#/L):',/norm & xyouts,0.2,0.30,strtrim(string((*p1).nt[i]/1.0e3,form='(f10.1)'),2),/normal         
         xyouts,0.07,0.27,'IWC(g/m!u3!n):',/norm & xyouts,0.2,0.27,strtrim(string((*p1).iwc[i],form='(f7.4)'),2),/normal        
         xyouts,0.07,0.24,'MVD(um):',/norm & xyouts,0.2,0.24,strtrim(string((*p1).mvd[i],form='(f7.1)'),2),/normal         
         xyouts,0.07,0.21,'MeanD(um):',/norm & xyouts,0.2,0.21,strtrim(string((*p1).mnd[i],form='(f7.1)'),2),/normal         
        ;xyouts,0.07,0.18,'Mean Af:',/norm & xyouts,0.2,0.18,strtrim(string((*p1).meanaf[i],form='(f5.1)'),2),/normal
         xyouts,0.07,0.12,'Accepted:',/norm & xyouts,0.2,0.12,strtrim(string((*p1).count_accepted[i]),2),/normal
         xyouts,0.07,0.09,'Rejected:',/norm & xyouts,0.2,0.09,strtrim(string((*p1).count_rejected_total[i]),2),/normal
         xyouts,0.07,0.06,'Missed:',/norm & xyouts,0.2,0.06,strtrim(string((*p1).count_missed[i]),2),/normal
         
         ;plot,(*p1).intmidbins,(*p1).intspec[i,*],/xl,/xs,position=[0.05,0.05,0.35,0.35],/noerase
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
         
         buffermargin=10
         panelwidth=(*pop).numdiodes+buffermargin
         num2plot=!d.x_size/fix(panelwidth)
         charsize=((*pop).numdiodes/50.0) < 1.4 > 0.8  ;Character size for the timestamp depends on buffer width
         
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
            b=soda2_bitimage(fn[(*p1).currentfile[ind[i]]], (*p1).pointer[ind[i]], pop, pmisc)
            IF b.rejectbuffer eq 0 THEN BEGIN
               tv,b.bitimage+1,panelwidth*ibuffer+buffermargin,20
               xyouts,panelwidth*ibuffer+buffermargin,10,string(b.time,format='(f8.2)'),/device,charsize=charsize
            ENDIF
            ibuffer=ibuffer+1
         ENDFOR
         
         ;Update panel counter
         numpanels=buffcount/num2plot
         IF buffcount mod num2plot ne 0 THEN numpanels=numpanels+1
         panelcountid=widget_info(topid,find='panelcount')
         widget_control,panelcountid,set_value='Panel '+strtrim(string(panelstart+1),2) + ' of ' + strtrim(string(floor(numpanels)),2)
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
         legend_old,['All Particles','Accepted Particles','Entire Flight (Scaled)'],line=[0,0,2],color=[color1,color2,color3],box=0,charsize=1.5
    
         IF (*p1).sodaversion eq 2 THEN BEGIN
            tothist=total((*p1).dhist,1)
            plot,findgen((*p1).op.numdiodes),(*p1).dhist[i,*],/xs,xtit='Diode Number',ytit='Shadow Count',/nodata
            oplot,findgen((*p1).op.numdiodes),(*p1).dhist[i,*],color=color1
            oplot,findgen((*p1).op.numdiodes),tothist/max(tothist)*!y.crange[1]*0.8,line=2,thick=1,color=color3
            legend_old,['This Time Period', 'Entire Flight (Scaled)'],line=[0,2],color=[color1,color3],box=0,charsize=1.5,/bottom
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
         
         ;For contour plots
         nlevels=30
         c_colors=findgen(nlevels)/nlevels * 220 + 30
         FOR p=0,1 DO BEGIN ;make both plots
            CASE plottype[id[p]] OF
               'IWC':BEGIN
                  plot,x,(*p1).iwc[a:b],ytitle='IWC (g/m!u3!n)',/nodata
                  oplot,x,(*p1).iwc[a:b],color=color1
                  ;legend_old,['All','Round','Non-Round'],line=0,color=[color1],box=0
               END
               'Extinction':BEGIN
                  plot,x,(*p1).area[a:b]*2000,ytitle='Extinction (1/km)',/nodata
                  oplot,x,(*p1).area[a:b]*2000,color=color2
               END
              'dBZ':BEGIN
                  plot,x,(*p1).dbz[a:b],ytitle='Reflectivity (dBZ)',/nodata
                  oplot,x,(*p1).dbz[a:b],color=color1
                  ;legend_old,['All','Round','Non-Round'],line=0,color=[color1],box=0
               END
               'Total Concentration':BEGIN
                  y=(*p1).nt[a:b]/1.0e3
                  IF max(y) lt 10 THEN ylog=0 ELSE ylog=1
                  IF max(y) lt 10 THEN yr=[0, max(y)] ELSE yr=[0.1,max(y,/nan)>1]
                  plot,x,(*p1).nt[a:b]/1.0e3,ytitle='N!lt!n (#/L)',ylog=ylog,yr=yr,/nodata
                  oplot,x,(*p1).nt[a:b]/1.0e3,color=color1
                  ;IF total(tag_names((*p1)) eq 'NT_ROUND') THEN oplot,x,(*p1).nt_round[a:b]/1.0e6,color=color2
                  ;IF total(tag_names((*p1)) eq 'NT_IRREG') THEN oplot,x,(*p1).nt_irreg[a:b]/1.0e6,color=color3
                  ;legend_old,['All','Round','Non-Round'],line=0,color=[color1,color2,color3]
               END
                'Diameter':BEGIN             
                  plot,x,(*p1).mnd[a:b],ytitle='Diameter (um)',/yl,yr=sizerange,/ys,/nodata
                  oplot,x,(*p1).mnd[a:b],color=color1
                  oplot,x,(*p1).dmass[a:b],color=color2
                  oplot,x,(*p1).mvd[a:b],color=color3
                  legend_old,['Mean','Mass Weighted','Median Volume'],line=0,color=[color1,color2,color3],box=0
               END
                'Rejection Codes':BEGIN
                  total_count=float((*p1).count_rejected_total[a:b]+(*p1).count_accepted[a:b])>1
                  plot,x,(*p1).count_rejected[a:b,1]/total_count*100,ytitle='Percent Rejected',yr=[0,105],/ys,psym=1,symsize=0.3
                  colorchoices=[color1,color2,color3,color4,color5,color6]
                  FOR i=1,6 DO oplot,x,(*p1).count_rejected[a:b,i]/total_count*100,color=colorchoices[i-1],psym=1,symsize=0.3
                  legend_old,['Area Ratio','Interarrival','Size Range','Edge Touch','Cluster','Centerin'],$
                      line=0,color=colorchoices,charsize=1.0,box=0
               END
               'Particle Counts':BEGIN
                  maxy=max([(*p1).count_missed[a:b],(*p1).count_accepted[a:b],(*p1).count_rejected_total[a:b]])
                  plot,x,(*p1).count_missed[a:b],ytitle='Particle Count',yr=[0,maxy]
                  oplot,x,(*p1).count_missed[a:b],color=color4
                  oplot,x,(*p1).count_accepted[a:b],color=color2
                  oplot,x,(*p1).count_rejected_total[a:b],color=color3
                  legend_old,['Accepted','Rejected','Missed'],line=0,color=[color2,color3,color4],box=0
               END
               'Color Concentration':BEGIN
                   conc=alog10((*p1).conc1d[a:b,*] > 1)
                   zrange=fix([3,max(alog10((*p1).conc1d),/nan)]) ;was [4,12]
                   contour,conc,x,(*p1).midbins,/cell,nlevels=nlevels,ytitle='Diameter (um)',/yl,/ys,yr=sizerange,zr=zrange,c_colors=c_colors ;min_val=max(conc)-10,
                   
                   barposx=[0.05,0.35]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[0.85,0.9]*(!y.window[1]-!y.window[0]) + !y.window[0]
                   bar=findgen(nlevels+1)/nlevels*(zrange[1]-zrange[0])+zrange[0]
                   xsave=!x  ;Needed so clicking in plot works later
                   contour,[[bar],[bar]],findgen(nlevels+1),[0,1],/cell,nlevels=nlevels,$
                      position=[barposx[0],barposy[0],barposx[1],barposy[1]],$
                      xtickname=string(zrange, form='(i2)') ,xstyle=5,ystyle=5,zr=zrange,/noerase,noclip=1,c_colors=c_colors
                   !x=xsave
                   xyouts, barposx, barposy[1]+0.01, ['10!u'+strtrim(string(zrange[0]),2)+'!n','10!u'+strtrim(string(zrange[1]),2)+'!n'],/norm,align=0.5
                   xyouts, mean(barposx), barposy[1]+0.01, 'Concentration (m!u-4!n)',/norm,align=0.5
               END
               'Color Area Ratio':BEGIN
                   conc=(*p1).meanar[a:b,*]
                   zrange=[0,0.8]
                   bartitle='Area Ratio'
                   IF (*pop).smethod eq 'fastcircle_aspectratio' THEN bartitle = 'Aspect Ratio'
                   contour,conc,x,(*p1).midbins,/cell,nlevels=nlevels,ytitle='Diameter (um)',/yl,/ys,yr=sizerange,zr=zrange,c_colors=c_colors
                   
                   barposx=[0.05,0.35]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[0.85,0.9]*(!y.window[1]-!y.window[0]) + !y.window[0]
;                    k=0.002 ;Border thickness
;                    k2=0.015  ;Thickness for wiping the background where text will be
;                    polyfill,[barposx[0]-k,barposx[1]+k,barposx[1]+k,barposx[0]-k],[barposy[0]-k,barposy[0]-k,barposy[1]+k,barposy[1]+k],/norm
;                    polyfill,[barposx[0]-k2,barposx[1]+k2,barposx[1]+k2,barposx[0]-k2],[barposy[1]+k,barposy[1]+k,barposy[1]+k2*2,barposy[1]+k2*2],/norm,color=255
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
                   contour,conc,x,(*p1).midbins,/cell,nlevels=nlevels,ytitle='Diameter (um)',/yl,/ys,yr=sizerange,zr=zrange,c_colors=c_colors
                   
                   barposx=[0.05,0.35]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[0.85,0.9]*(!y.window[1]-!y.window[0]) + !y.window[0]
                   bar=findgen(nlevels+1)/nlevels*(zrange[1]-zrange[0])+zrange[0]
                   xsave=!x
                   contour,[[bar],[bar]],findgen(nlevels+1),[0,1],/cell,nlevels=nlevels,$
                      position=[barposx[0],barposy[0],barposx[1],barposy[1]],$
                      xtickname=['4','12'] ,xstyle=5,ystyle=5,zr=zrange,/noerase,noclip=0,c_colors=c_colors
                   !x=xsave
                   xyouts, barposx, barposy[1]+0.01, string(zrange, form='(f3.1)'),/norm,align=0.5
                   xyouts, mean(barposx), barposy[1]+0.01, bartitle ,/norm,align=0.5
               END
               'Color Orientation':BEGIN
                   conc=(*p1).orientation_index[a:b,*]
                   zrange=[0,0.4]
                   bartitle='Orientation Index'
                   contour,conc,x,(*p1).midbins,/cell,nlevels=nlevels,ytitle='Diameter (um)',/yl,/ys,yr=sizerange,zr=zrange,c_colors=c_colors
                   
                   barposx=[0.05,0.35]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[0.85,0.9]*(!y.window[1]-!y.window[0]) + !y.window[0]
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
                   contour,conc,x,(*p1).intmidbins,/cell,nlevels=nlevels,ytitle='Interarrival Time (s)',/yl,/ys,yr=intrange,zr=zrange,c_colors=c_colors
                   
                   barposx=[0.05,0.35]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[0.85,0.9]*(!y.window[1]-!y.window[0]) + !y.window[0]
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
                   contour,conc,x,(*p1).intmidbins,/cell,nlevels=nlevels,ytitle='Interarrival Time Accepted Particles (s)',/yl,/ys,yr=intrange,zr=zrange,c_colors=c_colors
                   
                   barposx=[0.05,0.35]*(!x.window[1]-!x.window[0]) + !x.window[0]
                   barposy=[0.85,0.9]*(!y.window[1]-!y.window[0]) + !y.window[0]
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
                   barposy=([0.95,1.0])*(!y.window[1]-!y.window[0]) + !y.window[0]
                   bar=findgen(nlevels+1)/nlevels*(zrange[1]-zrange[0])+zrange[0]
                   xsave=!x
                   contour,[[bar],[bar]],findgen(nlevels+1),[0,1],/cell,nlevels=nlevels,$
                      position=[barposx[0],barposy[0],barposx[1],barposy[1]],$
                      xstyle=5,ystyle=5,zr=zrange,/noerase,noclip=0,c_colors=c_colors,$
                      xticks=1,yticks=1,xtickname=['x','x',' '],ytickname=[' ',' '],xtit=' '
                   !x=xsave
                   xyouts, barposx, barposy[1]+0.01, strtrim(string(zrange, form='(i5)'),2),/norm,align=0.5
                   xyouts, mean(barposx), barposy[1]+0.01, 'Diode Shadow Counts',/norm,align=0.5
                   ;plots,[barposx[0],barposx[1],barposx[1],barposx[0]],[barposy[0],barposy[0],barposy[1],barposy[1]],/norm
               END
               ELSE:dummy=0
            ENDCASE
         ENDFOR
         !x.style=0 & !x.tickformat='' & !x.title='' & !x.tickunits='' & !x.ticks=0 & !p.multi=[0,1,1,0,0]   
      END
   ENDCASE
END

