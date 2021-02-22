PRO merge_main, fn1, fn2=fn2, pthfile=pthfile, crossover=crossover, outdir=outdir, cgs=cgs, $
                  binstart=binstart, kcoeff=kcoeff, ncoeff=ncoeff, alpha=alpha, computeradar=computeradar,$
                  particle_aspect=particle_aspect, suffix=suffix, declutter=declutter, $
                  p1000=p1000, data=data, nosave=nosave, allspec=allspec, arvt1=arvt1, _extra=e
   ;pro to merge two probes at a specified crossover point, and compute
   ;the new bulk properties.
   ;Also returns gamma/exponential fit coeffiecients 
   ;fn1 and fn2 are the SODA dat files for probes 1 and 2
   ;pth is the dat file with pth data.
   ;Crossover is merge point in microns
   ;cgs flag puts fits in cgs units (default)
   ;Binstart is index of bin to start bulk compuations
   ;k,n,alpha for effective density
   ;Particle_aspect either 0.6 or 0.3 for T-matrix computations
   ;Suffix is an ID to add to output filename for organizational purposes
   ;Declutter runs a filter on data to eliminate spurious points in the concentration spectra
   ;p1000 sets pressure to 1000mb, rather than flight level pressure
   ;arvt1 sets area ratio to 1.0 for fall velocity computations
   ;'data' returns the final structure to the command line, can use with 'nosave' option.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   IF n_elements(cgs) eq 0 THEN cgs=1
   IF n_elements(outdir) eq 0 THEN outdir=''
   IF n_elements(fn2) eq 0 THEN fn2='' 
   IF n_elements(pthfile) eq 0 THEN pthfile=''
   IF n_elements(binstart) eq 0 THEN binstart=1     ;Default skip first bin
   IF n_elements(kcoeff) eq 0 THEN kcoeff=0.005615  ;Default to Brown and Francis
   IF n_elements(ncoeff) eq 0 THEN ncoeff=0.0
   IF n_elements(alpha) eq 0 THEN alpha=-1.1
   IF n_elements(suffix) eq 0 THEN suffix=''
   IF n_elements(declutter) eq 0 THEN declutter=0
   IF n_elements(particle_aspect) eq 0 THEN particle_aspect=0.6  ;For T-matrix
   IF n_elements(p1000) eq 0 THEN p1000=0  ;Set a constant pressure of 1000mb
   IF n_elements(arvt1) eq 0 THEN arvt1=0  ;Set area ratio to 1.0 for Vt computations
   IF n_elements(nosave) eq 0 THEN nosave=0
   IF n_elements(crossover) eq 0 THEN crossover=1000
   IF n_elements(allspec) eq 0 THEN allspec=0
   IF n_elements(computeradar) eq 0 THEN computeradar=1
   
   ;Restore primary probe
   IF file_test(fn1) eq 0 THEN stop,'File '+fn1+' not found. Stopping'  
   restore,fn1
   one=data
   n=n_elements(one.time)
   
   ;Restore PTH data and get TAS
   IF pthfile ne '' THEN BEGIN
      IF file_test(pthfile) eq 0 THEN stop,'File pthfile not found'
      restore,pthfile
      pth=data
      IF p1000 ne 0 THEN pth.pres[*]=1000.0
   ENDIF ELSE BEGIN
      pth={tas:one.tas, t:fltarr(n)-10.0, pres:fltarr(n)+1000.0}
      print,'--Warning--  Default TAS (or 2D recorded tas), temperature of -10C and pressure of 1000mb have been assumed.'
   ENDELSE

   ;Restore secondary (P) probe if found, to merge with fn1
   IF fn2 ne '' THEN BEGIN
      IF file_test(fn2) eq 0 THEN stop,'File fn2 not found'
      restore,fn2
      two=data
      m=merge_spectra(one, two, crossover=crossover, tas=pth.tas, declutter=declutter)
   ENDIF ELSE m=merge_spectra(one, crossover=crossover, tas=pth.tas)

   ;-------------------------------------------------------------------
   ;----------------Compute bulk parameters----------------------------
   ;-------------------------------------------------------------------

   ;Water or ice density limit
   densitylimit=0.91
   water=0
   IF (kcoeff gt 0.98) and (ncoeff eq 0) and (alpha eq 0) THEN BEGIN  ;>0.98 due to round off errors
      densitylimit=1.0
      water=1
      kcoeff=1.0  ;eliminate round off errors
   ENDIF
 
   ;Compute mass, area, reflectivity arrays
   mass=!pi/6 * ((kcoeff * (m.midbins/10000.)^(3+alpha) # (m.ar_midbins^ncoeff)) )
   massLWC=!pi/6 * (m.midbins/10000.)^3 # (m.ar_midbins^0)
   mass=mass < (massLWC*densitylimit)
   areagrid=!pi/4 * (m.midbins/1.0e6)^2 # (m.ar_midbins)
   diamgrid=(m.midbins) # (m.ar_midbins^0)
   dm6=(6/!pi)^2 * mass^2 * 1.e6
   dm6_water=(6/!pi)^2 * massLWC^2 * 1.e6

   ;Compute full 3D [time, size, ar] Vt array
   s=size(m.conc2d_raw, /dim)
   vt=fltarr(s)
   vtwater=fltarr(s)

   ;Compute terminal velocity
   FOR i=0, s[0]-1 DO BEGIN
      FOR j=0,s[1]-1 DO BEGIN        
         ar_vt=m.ar_midbins
         IF arvt1 eq 1 THEN ar_vt[*]=1.0    ;Force area ratio to unity for Vt calc
         v=compute_vt(m.midbins[j], mass[j,*], ar_vt, pth.t[i], pth.pres[i])
         vt[i,j,*]=v.vt
         vtwater[i,j,*]=v.vtwater        
      ENDFOR
   ENDFOR

   ;Compute Mie radar cross sections
   wavelength=[3.12, 2.20, 0.84, 0.319]   ;cm: X, Ku, Ka, W
   freqid=['X', 'Ku', 'Ka', 'W']
   qmie0={ze:0} & qmie1={ze:0} & qmie2={ze:0} & qmie3={ze:0} 
   IF computeradar eq 1 THEN BEGIN
      qmie0=mie(wavelength[0]*10.0, m.midbins/10000.0, m.ar_midbins, k=kcoeff, n=ncoeff, alpha=alpha)
      qmie1=mie(wavelength[1]*10.0, m.midbins/10000.0, m.ar_midbins, k=kcoeff, n=ncoeff, alpha=alpha)
      qmie2=mie(wavelength[2]*10.0, m.midbins/10000.0, m.ar_midbins, k=kcoeff, n=ncoeff, alpha=alpha)
      qmie3=mie(wavelength[3]*10.0, m.midbins/10000.0, m.ar_midbins, k=kcoeff, n=ncoeff, alpha=alpha)
   ENDIF
   
   ;T-matrix cross sections
   qtm0=fltarr(s[1], s[2]) &  qtm1=fltarr(s[1], s[2]) & qtm2=fltarr(s[1], s[2]) & qtm3=fltarr(s[1], s[2])
   IF (ncoeff eq 0) and (computeradar eq 1) THEN BEGIN  ;T-matrix does not support AR exponents, only straight mass-D
      q0=tmatrix(m.midbins/10000.0, freqband=freqid[0], a=kcoeff*!pi/6, b=3+alpha, particle_aspect=particle_aspect, _extra=e)
      q1=tmatrix(m.midbins/10000.0, freqband=freqid[1], a=kcoeff*!pi/6, b=3+alpha, particle_aspect=particle_aspect, _extra=e)
      q2=tmatrix(m.midbins/10000.0, freqband=freqid[2], a=kcoeff*!pi/6, b=3+alpha, particle_aspect=particle_aspect, _extra=e)
      q3=tmatrix(m.midbins/10000.0, freqband=freqid[3], a=kcoeff*!pi/6, b=3+alpha, particle_aspect=particle_aspect, _extra=e)
      FOR i=0,s[2]-1 DO BEGIN  ;Build the 2D matrix for each
         qtm0[*,i]=q0
         qtm1[*,i]=q1
         qtm2[*,i]=q2
         qtm3[*,i]=q3
      ENDFOR
   ENDIF
   
   ;Gamma and exponential fits, area ratio fits
   gam=gammafit(m.conc1d, m.endbins, cgs=cgs, lite=0, minsize=m.endbins[binstart])
   meanar=compute_meanar(m.conc2d_raw, m.ar_midbins)
   meanaspr=compute_meanar(m.conc2d_aspr_raw, m.ar_midbins)
   IF cgs THEN f=100d ELSE f=1d       ;a conversion factor for different units

   ;Initialize the output structure
   op={fn:[fn1,fn2], fn1:fn1, fn2:fn2, pth:pthfile, crossover:crossover, outdir:outdir, cgs:cgs, binstart:binstart,$
       kcoeff:kcoeff, ncoeff:ncoeff, alpha:alpha, particle_aspect:particle_aspect, declutter:declutter,$
       suffix:suffix, p1000:p1000, arvt1:arvt1, endbins:m.endbins, date:one.op.date, water:water, probetype:'MERGED',$
       project:one.op.project}
   units={conc1d:'#/m4', conc1d_raw:'#/m3', endbins:'microns', fits:'cgs', lwc:'g/m3', iwc:'g/m3', $
          area:'1/m', nt:'#/m3', rr:'mm/hr', vm:'cm/s', va:'cm/s', vz:'cm/s', dmass:'microns', $
          dmean:'microns', dvol:'microns', darea:'microns', dz:'microns', tas:'m/s', t:'C', pres:'mb'}
   linarray=fltarr(n)
   specarray=fltarr(n, n_elements(m.midbins))
   data={op:op, time:one.time, conc1d:m.conc1d, $
         conc1d_raw:m.conc1d_raw, endbins:m.endbins,$
         midbins:m.midbins, binwidth:m.binwidth,$
         meanar:meanar, meanaspr:meanaspr, efit:fltarr(n,2), pfit:fltarr(n,2),$
         fit:gam, lwc:linarray, nt:linarray,$
         rr:linarray, iwc:linarray, vm:linarray,$
         dbz:linarray, area:linarray,$
         va:linarray,  vz:linarray,$
         iwcspec:specarray, vmspec:specarray,$
         zspec:specarray, areaspec:specarray,$
         vaspec:specarray, vzspec:specarray, vtspec:specarray,$
         dbzmie0:linarray, dbzmie1:linarray,$
         dbzmie2:linarray, dbzmie3:linarray,$
         vzmie0:linarray, vzmie1:linarray,$
         vzmie2:linarray, vzmie3:linarray,$
         dbztm0:linarray, dbztm1:linarray, $
         dbztm2:linarray, dbztm3:linarray,$
         vztm0:linarray, vztm1:linarray,$
         vztm2:linarray, vztm3:linarray,$
         dmass:linarray, darea:linarray,$
         dmean:linarray, dvol:linarray,$
         dz:linarray,$
         wavelength:wavelength, freqid:freqid,$
         tas:pth.tas, t:pth.t, pres:pth.pres, $
         units:units, Date_Processed:systime()}
   IF allspec eq 1 THEN BEGIN  ;Add new spectra/variables here as needed
      data=create_struct(data, 'zmie0spec', specarray, 'zmie1spec', specarray, 'zmie2spec', specarray, $
                         'zmie3spec', specarray, 'rrspec', specarray, 'qmie0', qmie0.ze, 'qmie1', qmie1.ze,$
                         'qmie2', qmie2.ze, 'qmie3', qmie3.ze, 'qtm0', qtm0, 'qtm1', qtm1, 'qtm2', qtm2,$
                         'qtm3', qtm3)
   ENDIF   

   ;Loop through each time step, computing bulk parameters
   FOR i=0,n-1 DO BEGIN
      iconc=reform(m.conc2d_raw[i,*,*])  ;Concentration for this index
      ivt=vt[i,*,*]                      ;Vt for this index
      IF water eq 1 THEN ivt=vtwater[i,*,*]
      IF binstart gt 0 THEN iconc[0:binstart-1,*]=0  ;Zero out any data below binstart
      
      ;Compute a few size-by-AR arrays
      iwc2d =iconc * mass
      lwc2d =iconc * massLWC
      vm2d  =iconc * mass * ivt
      z2d   =iconc * dm6
      vz2d  =iconc * dm6 * ivt
      area2d=iconc * areagrid
      va2d  =iconc * areagrid * ivt
      vt2d  =iconc * ivt
      mie0  =iconc * qmie0.ze
      mie1  =iconc * qmie1.ze
      mie2  =iconc * qmie2.ze
      mie3  =iconc * qmie3.ze
      tm0   =iconc * qtm0
      tm1   =iconc * qtm1
      tm2   =iconc * qtm2
      tm3   =iconc * qtm3
      
      ;Fill structure
      data.iwc[i]=total(iwc2d)
      data.iwcspec[i,*]=total(iwc2d,2)
      data.lwc[i]=total(lwc2d)
      data.nt[i]=total(iconc)
      data.vm[i]=total(vm2d)/data.iwc[i]
      data.vmspec[i,*]=total(vm2d,2)/data.iwcspec[i,*]
      data.rr[i]=3.6e-2*total(vm2d)
      data.area[i]=total(area2d)
      data.areaspec[i,*]=total(area2d,2)
      data.va[i]=total(va2d)/data.area[i]
      data.vaspec[i,*]=total(va2d,2)/data.areaspec[i,*]
      data.dbz[i]=10*alog10(total(z2d))-7.2
      IF water eq 1 THEN data.dbz[i]=10*alog10(total(z2d))
      data.zspec[i,*]=total(z2d,2)
      data.vz[i]=total(vz2d)/total(z2d)
      data.vzspec[i,*]=total(vz2d,2)/data.zspec[i,*]
      data.dbzmie0[i]=10*alog10(total(mie0))
      data.dbzmie1[i]=10*alog10(total(mie1))
      data.dbzmie2[i]=10*alog10(total(mie2))
      data.dbzmie3[i]=10*alog10(total(mie3))
      data.dbztm0[i]=10*alog10(total(tm0))
      data.dbztm1[i]=10*alog10(total(tm1))
      data.dbztm2[i]=10*alog10(total(tm2))
      data.dbztm3[i]=10*alog10(total(tm3))
      data.vzmie0[i]=total(mie0*ivt)/total(mie0)
      data.vzmie1[i]=total(mie1*ivt)/total(mie1)
      data.vzmie2[i]=total(mie2*ivt)/total(mie2)
      data.vzmie3[i]=total(mie3*ivt)/total(mie3)
      data.vztm0[i]=total(tm0*ivt)/total(tm0)
      data.vztm1[i]=total(tm1*ivt)/total(tm1)
      data.vztm2[i]=total(tm2*ivt)/total(tm2)
      data.vztm3[i]=total(tm3*ivt)/total(tm3)
      data.dmass[i]=total(iwc2d*diamgrid)/data.iwc[i]
      data.darea[i]=total(area2d*diamgrid)/data.area[i]
      data.dmean[i]=total(iconc*diamgrid)/data.nt[i]
      data.dvol[i]=total(lwc2d*diamgrid)/data.lwc[i]
      data.dz[i]=total(z2d*diamgrid)/total(z2d)
      data.vtspec[i,*]=total(vt2d,2)/total(iconc,2)
      IF allspec eq 1 THEN BEGIN  ;Add extra spectra here as needed
         data.zmie0spec[i,*]=total(mie0,2)
         data.zmie1spec[i,*]=total(mie1,2)
         data.zmie2spec[i,*]=total(mie2,2)
         data.zmie3spec[i,*]=total(mie3,2)
         data.rrspec[i,*]=3.6e-2*total(vm2d,2)
      ENDIF

      ;Compute area ratio fits
      g=where(meanar[i,*] gt 0)
      IF g[0] ne -1 THEN BEGIN
         pl=fitpl(m.midbins[g]/(1.0e6/f), meanar[i,g], ccpl)
         ex=fitexp(m.midbins[g]/(1.0e6/f), meanar[i,g], ccex)
         ;Eliminate poor fits
         IF abs(ccpl) gt 0.5 THEN data.pfit[i,*]=pl
         IF abs(ccex) gt 0.5 THEN data.efit[i,*]=ex
      ENDIF
   ENDFOR

   fnout=outdir+one.op.date+'_'+strtrim(string(long(sfm2hms(one.time[0])),form='(i06)'),2)+'_BULK'+suffix+'.dat'
   IF nosave eq 0 THEN save, data, file=fnout, /compress
END   
   
   
