FUNCTION compute_bulk_simple, conc1d, endbins, binstart=binstart, acoeff=a, bcoeff=b, _extra=e
   ;Compute IWC from simple power law from for a single or to-be-merged spectra.
   ;conc1d in m^-4, endbins in um, iwc in g/m3
   ;AB 9/2009
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
    
   IF n_elements(a) eq 0 THEN a=0.00294  ;Use BF as default
   IF n_elements(b) eq 0 THEN b=1.9
   IF n_elements(binstart) eq 0 THEN binstart=0
   
   endbins=float(endbins)
   numbins=n_elements(endbins)-1
   midbins=(endbins[0:numbins-1]+endbins[1:numbins])/2.0
   binwidth=float(endbins[1:numbins]-endbins[0:numbins-1])
   
   ;Take care of NaN values
   bad=where(finite(conc1d) eq 0, nbad)
   IF nbad gt 0 THEN conc1d[bad]=0
   
   s=size(conc1d,/dim)
   IF n_elements(s) eq 1 THEN BEGIN  ;Have to reform if only one PSD is given
      conc1d=reform(conc1d,1,n_elements(conc1d))
      s=size(conc1d,/dim)
   ENDIF
   num=s[0]
   binstop=s[1]-1
   
   iwc=fltarr(num)
   lwc=fltarr(num)
   dmass=fltarr(num)
   dmassw=fltarr(num)
   dmassmelted=fltarr(num)
   z=fltarr(num)
   
   mass=(a*(midbins/1.0e4)^b)
   massLWC=!pi/6 * (midbins/1.0e4)^3 
   mass=mass<(massLWC*0.91)
   dm6=(6/!pi)^2 * mass^2 * 1.e6
   midbins_melted=(6/!pi*mass)^(1.0/3.0)*1e4
   
   FOR i=0L,num-1 DO BEGIN
      spec=conc1d[i,*]*binwidth/1.0e6
      iwc[i]=total((mass*spec)[binstart:binstop])
      lwc[i]=total((massLWC*spec)[binstart:binstop])
      z[i]=total((dm6*spec)[binstart:binstop])
      IF iwc[i] gt 0 THEN dmass[i]=total((mass*spec*midbins)[binstart:binstop])/iwc[i]  
      IF iwc[i] gt 0 THEN dmassmelted[i]=total((mass*spec*midbins_melted)[binstart:binstop])/iwc[i]  
      IF lwc[i] gt 0 THEN dmassw[i]=total((massLWC*spec*midbins)[binstart:binstop])/lwc[i]    
   ENDFOR
   
   u=unnormalize(conc1d,endbins)
   nt=total(u[*,binstart:binstop],2,/nan)
   mvd=mvdiam(u[*,binstart:binstop],midbins[binstart:binstop],/interp)
   mnd=meandiam(u[*,binstart:binstop],midbins[binstart:binstop])

   return, {iwc:iwc, lwc:lwc, dmass:dmass, dmassw:dmassw, dmassmelted:dmassmelted, dbz:10*(alog10(z))-7.2, dbzw:10*(alog10(z)), nt:nt, mvd:mvd, mnd:mnd} 
END
