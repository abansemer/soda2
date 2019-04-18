FUNCTION compute_bulk_simple, conc1d, endbins, binstart=binstart, acoeff=acoeff, bcoeff=bcoeff, _extra=e, mass=mass
   ;Compute IWC from simple power law from for a single or to-be-merged spectra.
   ;conc1d in m^-4, endbins in um, iwc in g/m3
   ;AB 9/2009
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
    
   IF n_elements(acoeff) eq 0 THEN acoeff=0.00294  ;Use BF as default
   IF n_elements(bcoeff) eq 0 THEN bcoeff=1.9
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
   dmedianmass=fltarr(num)
   z=fltarr(num)

   mass=(acoeff*(midbins/1.0e4)^bcoeff)
   massLWC=!pi/6 * (midbins/1.0e4)^3 
   mass=mass<(massLWC*0.91)
   dm6=(6/!pi)^2 * mass^2 * 1.e6
   midbins_melted=(6/!pi*mass)^(1.0/3.0)*1e4
   msd=conc1d*0.0
   
   FOR i=0L,num-1 DO BEGIN
      spec=conc1d[i,*]*binwidth/1.0e6
      iwc[i]=total((mass*spec)[binstart:binstop])
      lwc[i]=total((massLWC*spec)[binstart:binstop])
      z[i]=total((dm6*spec)[binstart:binstop])
      msd[i,*]=mass*spec
      IF iwc[i] gt 0 THEN dmass[i]=total((mass*spec*midbins)[binstart:binstop])/iwc[i]  
      IF iwc[i] gt 0 THEN dmassmelted[i]=total((mass*spec*midbins_melted)[binstart:binstop])/iwc[i]  
      IF iwc[i] gt 0 THEN dmedianmass[i]=mmdiam((mass*spec)[binstart:binstop], endbins[binstart:binstop+1])
      IF lwc[i] gt 0 THEN dmassw[i]=total((massLWC*spec*midbins)[binstart:binstop])/lwc[i]    
   ENDFOR
   
   u=unnormalize(conc1d,endbins)
   nt=total(u[*,binstart:binstop],2,/nan)
   mvd=mvdiam(u[*,binstart:binstop],midbins[binstart:binstop])
   mnd=meandiam(u[*,binstart:binstop],midbins[binstart:binstop])

   return, {iwc:iwc, lwc:lwc, dmass:dmass, dmedianmass:dmedianmass, dmassw:dmassw, dmassmelted:dmassmelted, $
            dbz:10*(alog10(z))-7.2, dbzw:10*(alog10(z)), nt:nt, mvd:mvd, mnd:mnd, msd:msd} 
END
