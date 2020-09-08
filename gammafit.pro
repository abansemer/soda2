FUNCTION gammafit, spectra_in, endbins_in, cgs=cgs, lite=lite, minsize=minsize
   ;Fits a gamma function to a particle size spectrum.
   ;Uses a moment-matching tecnique for the 1,2, and 6th moments.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
  
   ;'Spectra' should be one or more size spectra binned into size bins given in 'endbins', in m^-4.  
   ;Endbins should be in microns.   
   ;Set the 'cgs' parameter to 1 to output coefficients in cgs units.
   ;Minsize only uses bins above minsize
   
   IF n_elements(lite) eq 0 THEN lite=0
   IF n_elements(minsize) eq 0 THEN minsize=0
   IF (minsize ne 0) and (total(endbins_in eq minsize) eq 0) THEN print,'Minsize does not match an endbin boundary, some data will be unused.'

   spectra=double(spectra_in)  ;this routine needs high precision
   endbins=double(endbins_in)
   numbins=n_elements(endbins)-1

   k=size(spectra,/dim)
   IF k[0] eq numbins THEN trans=0 ELSE trans=1    ;check to see if spectra matrix needs to be transposed
   IF trans THEN BEGIN
      spectra=transpose(spectra)
      k=size(spectra,/dim)
   ENDIF
   s=size(endbins,/dim)                            ;check to see if endbins needs to be transposed
   IF n_elements(s) eq 2 THEN endbins=transpose(endbins)
   
   midbins=(endbins[1:numbins]+endbins[0:numbins-1])/2.0
   binwidth=endbins[1:numbins]-endbins[0:numbins-1]

   IF n_elements(k) eq 2 THEN numspectra=k[1] ELSE numspectra=1
   mu_gam=dblarr(numspectra)
   lam_gam=dblarr(numspectra)
   n0_gam=dblarr(numspectra)
   cc_gam=dblarr(numspectra)
   lam_exp=dblarr(numspectra)
   n0_exp=dblarr(numspectra)
   cc_exp=dblarr(numspectra)
   gam_spec=dblarr(numbins,numspectra)
   exp_spec=dblarr(numbins,numspectra)
   M0=dblarr(numspectra)
   M1=dblarr(numspectra)
   M2=dblarr(numspectra)
   M3=dblarr(numspectra)
   M4=dblarr(numspectra)
   M5=dblarr(numspectra)
   M6=dblarr(numspectra)
   nfitted_gam=intarr(numspectra)
   nfitted_exp=intarr(numspectra)
   IF n_elements(cgs) eq 0 THEN cgs=0
   IF cgs THEN f=100d ELSE f=1d       ;a conversion factor for different units
   
   FOR c=0L,numspectra-1 DO BEGIN
      nonzero=where((spectra[*,c] gt 0) and (finite(spectra[*,c]) eq 1) and (endbins_in ge minsize))
      IF n_elements(nonzero) ge 5 THEN BEGIN
         ;These used in fitting
         M1[c]=total((midbins[nonzero]/(1.0e6/f))^1*spectra[nonzero,c]/f^4*(binwidth[nonzero]/(1.0e6/f)))
         M2[c]=total((midbins[nonzero]/(1.0e6/f))^2*spectra[nonzero,c]/f^4*(binwidth[nonzero]/(1.0e6/f)))
         M6[c]=total((midbins[nonzero]/(1.0e6/f))^6*spectra[nonzero,c]/f^4*(binwidth[nonzero]/(1.0e6/f)))              
         
         ;These are not used, just for returning back to user 
         M0[c]=total((midbins[nonzero]/(1.0e6/f))^0*spectra[nonzero,c]/f^4*(binwidth[nonzero]/(1.0e6/f)))
         M3[c]=total((midbins[nonzero]/(1.0e6/f))^3*spectra[nonzero,c]/f^4*(binwidth[nonzero]/(1.0e6/f)))
         M4[c]=total((midbins[nonzero]/(1.0e6/f))^4*spectra[nonzero,c]/f^4*(binwidth[nonzero]/(1.0e6/f)))              
         M5[c]=total((midbins[nonzero]/(1.0e6/f))^5*spectra[nonzero,c]/f^4*(binwidth[nonzero]/(1.0e6/f)))              
         
         MM=M2[c]^5/M6[c]/M1[c]^4
         ttt=fz_roots([16-360*MM, 32-342*MM, 24-119*MM, 8-18*MM, 1-MM])  ; find the roots
         mu_gam[c]=double(ttt[3])  ; this is the only real root, so use it as mu
         lam_gam[c]=M1[c]*(mu_gam[c]+2)/M2[c]
         n0_gam[c]=M1[c]*lam_gam[c]^(mu_gam[c]+2)/gamma(mu_gam[c]+2)
         cc_gam[c]=correlate(alog10(n0_gam[c]*(midbins[nonzero]/(1.0e6/f))^mu_gam[c]*exp(-midbins[nonzero]/(1.0e6/f)*lam_gam[c])),alog10(spectra[nonzero,c]/f^4))
         gam_spec[*,c]=n0_gam[c]*(midbins/(1.0e6/f))^mu_gam[c]*exp(-(lam_gam[c])*midbins/(1.0e6/f))
         nfitted_gam[c]=n_elements(nonzero)
      ENDIF

      nonzero_exp=where((spectra[*,c] gt 0) and (endbins_in ge minsize))   
      IF n_elements(nonzero_exp) ge 5 THEN BEGIN
         temp=fitexp((midbins[nonzero_exp]/(1.0e6/f)), spectra[nonzero_exp,c]/f^4)
         lam_exp[c]=-temp[1] & n0_exp[c]=temp[0]
         exp_spec[*,c]=n0_exp[c] * exp(-lam_exp[c]*midbins/(1.0e6/f))
         cc_exp[c]=correlate(alog10(exp_spec[nonzero,c]),alog10(spectra[nonzero,c]))
         nfitted_exp[c]=n_elements(nonzero_exp)
      ENDIF 
   ENDFOR
   IF trans THEN gam_spec=transpose(gam_spec)  ;align gam_spec dimensions to match the input spectra
   IF trans THEN exp_spec=transpose(exp_spec)  ;align gam_spec dimensions to match the input spectra
   nT_gam=n0_gam*gamma(mu_gam+1)/(lam_gam^(mu_gam+1)) ;this is from Chandrasekar and Bringi, Jtech Sept 1987

   IF lite ne 0 THEN $
     return,{nT:nT_gam,lam:lam_gam, n0:n0_gam, mu:mu_gam, cc:cc_gam, midbins:midbins, lam_exp:lam_exp, n0_exp:n0_exp} $
   ELSE $
     return,{nT:nT_gam,lam:lam_gam, n0:n0_gam, mu:mu_gam, cc:cc_gam, cc_exp:cc_exp, spec:gam_spec, spec_exp:exp_spec, midbins:midbins, lam_exp:lam_exp, n0_exp:n0_exp, $
             m0:m0,m1:m1,m2:m2,m3:m3,m4:m4,m5:m5,m6:m6,nfitted_exp:nfitted_exp,nfitted_gam:nfitted_gam}

END
