pro draw_circle, x,y,diam,_extra=e
  ;draws a circle on a plot
  ;print,x,y,diam
  ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

  num=100; number of sectors to draw

     a=diam[0]*cos(findgen(num+1)/num*2*!pi)/2 + x
     b=diam[0]*sin(findgen(num+1)/num*2*!pi)/2 + y
  
  plots,a,b,/data,_extra=e
end