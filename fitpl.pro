FUNCTION fitpl,x,y,cc,nozero=nozero
   ;function to return the power law coefficients, c, to the 
   ;function y=c[0]x^c[1]
   ;Uses the princomp function to make the fit.
   ;The nozero switch filters out data le 0 for proper fitting.
   
   IF n_elements(nozero) eq 0 THEN nozero=0
   IF nozero eq 1 THEN good=where((y gt 0) and (x gt 0),ngood) ELSE good=where(y ne -999999,ngood)
   IF ngood eq 0 THEN return,[0,0]
   
   c=princomp(alog(x[good]),alog(y[good]),cc)
   c[0]=exp(c[0])
   return,c   
END
