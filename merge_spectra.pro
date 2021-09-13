FUNCTION merge_spectra, data1, data2, crossover=crossover, tas=tas, declutter=declutter
   ;FUNCTION to merge two SODA files at a specified crossover point,
   ;and return PSDs in a few different forms.
   ;Enter SODA structures for each probe in data1 and data2.
   ;Crossover point in microns.
   ;Will use a new TAS if specified.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(tas) eq 0 THEN tas=data1.tas
   IF n_elements(crossover) eq 0 THEN crossover=1000
   got_aspr = total(tag_names(data1) eq 'SPEC2D_ASPR')  ;Old SODA-1 files don't have ASPR

   ;Set initial concentration arrays
   n=n_elements(data1.time)
   s1=size(data1.spec2d,/dim)
   sagrid1=fltarr(s1[1], s1[2])     ;Sample area 2D array
   FOR i=0,s1[1]-1 DO sagrid1[i,*]=data1.sa[i]

   ;Compute unnormalized (raw) 2d spectra
   conc2d_raw = fltarr(s1)
   conc2d_aspr_raw = fltarr(s1)
   FOR i=0,s1[0]-1 DO conc2d_raw[i,*,*] = data1.spec2d[i,*,*]/(sagrid1*tas[i]*data1.activetime[i])
   IF got_aspr THEN FOR i=0,s1[0]-1 DO conc2d_aspr_raw[i,*,*] = data1.spec2d_aspr[i,*,*]/(sagrid1*tas[i]*data1.activetime[i])
   midbins=float(data1.midbins)
   endbins=float(data1.op.endbins)
   binwidth=float(data1.op.endbins[1:*])-data1.op.endbins
   ar_midbins=(data1.op.arendbins[1:*] + data1.op.arendbins)/2.0

   ;Secondary (P) probe, merge with fn1
   IF n_elements(data2) ne 0 THEN BEGIN
      IF n ne n_elements(data2.time) THEN stop, 'Time mismatch fn1 and fn2'
      IF data1.op.res gt data2.op.res THEN stop, 'fn2 must have lower resolution than fn1'
      IF total(data1.op.arendbins) ne total(data2.op.arendbins) THEN stop,'Area ratio bin mismatch.'

      s2=size(data2.spec2d,/dim)
      sagrid2=fltarr(s2[1], s2[2])     ;Sample area 2D array
      FOR i=0,s2[1]-1 DO sagrid2[i,*]=data2.sa[i]

      ;Compute unnormalized 2d spectra for probe 2
      conc2d2_raw = fltarr(s2)
      conc2d2_aspr_raw = fltarr(s2)
      FOR i=0,s2[0]-1 DO conc2d2_raw[i,*,*] = data2.spec2d[i,*,*]/(sagrid2*tas[i]*data2.activetime[i])
      IF got_aspr THEN FOR i=0,s2[0]-1 DO conc2d2_aspr_raw[i,*,*] = data2.spec2d_aspr[i,*,*]/(sagrid2*tas[i]*data2.activetime[i])

      ;Merge the 2D spectra
      stop1=max(where(data1.midbins lt crossover))
      start2=min(where(data2.midbins ge crossover))
      ;Warn if crossover does not land on a common endbin boundary
      IF total((data1.op.endbins eq crossover) + (data2.op.endbins eq crossover)) ne 2 THEN $
           print,'Uneven merge in merge_spectra.pro, merged endbins may have errors.'
      conc2d_raw=[[conc2d_raw[*,0:stop1,*]], [conc2d2_raw[*,start2:*,*]]]
      IF got_aspr THEN conc2d_aspr_raw=[[conc2d_aspr_raw[*,0:stop1,*]], [conc2d2_aspr_raw[*,start2:*,*]]]

      ;Merge bins, try not using endbins since they can be mismatched with the merge.
      midbins=float([data1.midbins[0:stop1], data2.midbins[start2:*]])
      endbins=float([data1.op.endbins[0:stop1], data2.op.endbins[start2:*]])
      binwidth2=float(data2.op.endbins[1:*])-data2.op.endbins
      binwidth=[binwidth[0:stop1], binwidth2[start2:*]]   ;Keep this for normalizing later
   ENDIF

   ;Get rid of NaN and set to zero
   bad=where(finite(conc2d_raw) eq 0, nbad)
   IF nbad gt 0 THEN conc2d_raw[bad]=0

   ;Declutter data if flagged, to get rid of noisy data especially in large sizes
   IF n_elements(declutter) eq 0 THEN declutter=0
   IF declutter ne 0 THEN BEGIN
      binary=conc2d_raw and (conc2d_raw*0 + 1)  ;convert to binary image
      kernel=[[0,1,0],[1,1,1],[0,1,0]]
      binary=morph_open(binary,kernel)
      conc2d_raw=conc2d_raw*binary
   ENDIF

   ;Compute 1D spectra
   conc1d_raw=total(conc2d_raw,3)
   conc1d=conc1d_raw*0
   FOR i=0,n-1 DO conc1d[i,*]=conc1d_raw[i,*]/(binwidth/1.0e6)

   return,{conc1d:conc1d, conc1d_raw:conc1d_raw, conc2d_raw:conc2d_raw, conc2d_aspr_raw:conc2d_aspr_raw, midbins:midbins, $
           endbins:endbins, binwidth:binwidth, ar_midbins:ar_midbins}
END
