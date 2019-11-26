FUNCTION fitexp,x,y,cc
   ;function to return the exponential fit coefficients, c, to the 
   ;function y=c[0]*exp(c[1]*x)
   ;Uses the princomp function to make the fit.
   
   good=where(y gt 0, ngood) ;Can't take log of non-positive numbers
   IF ngood gt 2 THEN BEGIN
      c=princomp(x[good],alog(y[good]),cc)
      c[0]=exp(c[0])
   ENDIF ELSE BEGIN 
      c=[0,0]
      cc=0
   ENDELSE
   return,c   
END
