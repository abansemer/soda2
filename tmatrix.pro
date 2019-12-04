FUNCTION tmatrix, dspec, freqband=freqband, a=a, b=b, filepath=filepath, particle_aspect=particle_aspect, oldway=oldway, quiet=quiet, xsect=xsect
   ;Return tmatrix values similar to the mie.pro program.
   ;Will read files sent by Sergey, April 2013.
   ;dspec is an array of size midbins in cm  **!!! DIAMETER !!!**
   ;AB 4/2013
   
   ;Defaults
   IF n_elements(freqband) eq 0 THEN freqband='Ka'
   IF n_elements(b) eq 0 THEN b=2.1
   IF n_elements(a) eq 0 THEN a=0.01
   IF n_elements(particle_aspect) eq 0 THEN particle_aspect=0.6
   IF n_elements(filepath) eq 0 THEN filepath='~/programs/tmatrix/'
   IF n_elements(oldway) eq 0 THEN oldway=0
   IF n_elements(quiet) eq 0 THEN quiet=0
   
   IF particle_aspect eq 0.3 THEN suffix='_0.3' ELSE suffix=''
   CASE freqband OF
      'Ka':fn=filepath+'t_matrix_Ka_band_various_a_and_b'+suffix+'.dat'
      'Ku':fn=filepath+'t_matrix_Ku_band_various_a_and_b'+suffix+'.dat'
      'X':fn=filepath+'t_matrix_X_band_various_a_and_b'+suffix+'.dat'
      'W':fn=filepath+'t_matrix_W_band_various_a_and_b'+suffix+'.dat'
      ELSE:stop,'Unknown radar band'
   ENDCASE 
   
   ;Read appropriate data file
   IF file_test(fn) eq 0 THEN BEGIN
      print,'Can not find '+fn+', returning 0.'
      return, dspec*0
   ENDIF
   openr,lun,fn,/get_lun
   s=' '
   REPEAT readf,lun,s UNTIL strmid(s,0,3) eq ' NF'   ;Skip header
   
   nblocks=110
   nfields=15
   nlines=192
   all=fltarr(nfields,nblocks*nlines)
   block=fltarr(nfields,nlines)
   FOR i=0,nblocks-1 DO BEGIN
      readf,lun,block
      IF i lt nblocks-1 THEN readf,lun,s  ;Another header
      all[*,(i*nlines):((i+1)*nlines-1)]=block
   ENDFOR
   free_lun,lun
  
   ;Rename important variables from the data block
   data={diam:all[0,*], xsect:all[5,*], particle_aspect:all[6,*], density:all[7,*], mass:all[8,*], a:all[13,*], b:all[14,*], freq:all[3,0]}
    
   ;Find best match for a and b coeffs
   ayes=data.a[uniq(data.a[sort(data.a)])]
   bees=data.b[uniq(data.b[sort(data.b)])]
   ;Check for out of range data
   IF ((a lt min(ayes)) or (a gt max(ayes)) or (b lt min(bees)) or (b gt max(bees))) THEN BEGIN
      IF quiet eq 0 THEN BEGIN
         print,'a or b coefficients out of range for T-matrix calculations, returning 0'
         print,'a-range:',min(ayes),max(ayes),', a=',a
         print,'b-range:',min(bees),max(bees),', b=',b
      ENDIF
      return, dspec*0
   ENDIF
   bhi=bees[min(where(bees ge b))]
   blo=bees[max(where(bees lt b))]
   ahi=ayes[min(where(ayes ge a))]
   alo=ayes[max(where(ayes lt a))]
   
   ;Find cross sections for all combinations of hi/lo a and b
   w=where((data.particle_aspect eq particle_aspect) and (data.a eq alo) and (data.b eq blo))
   xsect1=interpol(data.xsect[w], data.diam[w], dspec) > 0
   w=where((data.particle_aspect eq particle_aspect) and (data.a eq alo) and (data.b eq bhi))
   xsect2=interpol(data.xsect[w], data.diam[w], dspec) > 0
   w=where((data.particle_aspect eq particle_aspect) and (data.a eq ahi) and (data.b eq blo))
   xsect3=interpol(data.xsect[w], data.diam[w], dspec) > 0
   w=where((data.particle_aspect eq particle_aspect) and (data.a eq ahi) and (data.b eq bhi))
   xsect4=interpol(data.xsect[w], data.diam[w], dspec) > 0
   
   ;First interpolate for each a
   xsect_alo=xsect1 + (xsect2-xsect1)/(bhi-blo) * (b-blo)
   xsect_ahi=xsect3 + (xsect4-xsect3)/(bhi-blo) * (b-blo)
   
   ;Now interpolate again for b
   xsect=xsect_alo + (xsect_ahi-xsect_alo)/(ahi-alo) * (a-alo)


   ;Old way, just using nearest neighbor (no interpolation)
   ;Interpolation verified correct, old way no longer needed....
   IF oldway eq 1 THEN BEGIN
      dummy=min(abs(a-data.a),imin)
      a1=data.a[imin]
      dummy=min(abs(b-data.b),imin)
      b1=data.b[imin]
      IF a ne a1 THEN print,'a changed from ',a,' to ',a1
      IF b ne b1 THEN print,'b changed from ',b,' to ',b1
   
      ;Get the right block of data based on a, b, and aspect ratio
      w=where((data.particle_aspect eq particle_aspect) and (data.a eq a1) and (data.b eq b1))
      
      ;Get backscatter x-sect for each size, interpolated
      xsect=interpol(data.xsect[w], data.diam[w], dspec) > 0
   ENDIF
  
   k2=0.93                            ;For long wavelengths
   IF freqband eq 'Ka' THEN k2=0.88   ;According to Sergey Jan 2015 email
   IF freqband eq 'W'  THEN k2=0.69   ;According to Sergey Jan 2015 email
   wavel=3.0e8/(data.freq*1.0e9)*1000 ;mm
   ze=wavel^4*xsect*100/(!pi^5*k2)    ;mm^6
   return, ze
END
   
