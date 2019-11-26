FUNCTION princomp, x, y, corr
   ; calculates coefficients of "principle components" linear fit, where
   ; perpendicular deviations are minimized.   yfit = c[0] + c[1]*xfit

   c = dblarr(2)
   npts = n_elements(x)
   IF (n_elements(y) ne npts) THEN BEGIN
     print,'** Error: x and y arrays not the same length'
     stop
   ENDIF

   xmean = total(x)/double(npts)
   ymean = total(y)/double(npts)
   sxx = total((x-xmean)^2)
   syy = total((y-ymean)^2)
   sxy = total((x-xmean)*(y-ymean))
   xlam = 0.5*(sxx+syy) + 0.5*sqrt((sxx-syy)^2 + (4.*sxy^2))
   c[1] = (xlam - sxx)/sxy
   c[0] = ymean - c[1]*xmean
   corr = sxy / sqrt(sxx * syy)
   return, c
END                  

