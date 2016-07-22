function find_radius, p1,p2,p3
   ;finds the radius of a circle as
   ;defined by 3 points. p1,p2 and p3 should all
   ;have 2 elements: the x and y coordinates.
   ;Based on algorithm by Goldman, in Graphics Gems "Triangles"
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   d1=float(p3-p1)##transpose(p2-p1)
   d2=float(p3-p2)##transpose(p1-p2)
   d3=float(p1-p3)##transpose(p2-p3)
   
   c=(d2*d3+d3*d1+d1*d2)
   
   r=0.5*sqrt(  (d1+d2)*(d2+d3)*(d3+d1)/c )  
    
   ;Formula for coordinates of center is given in 
   ;reference, but is not computed here
   
   ;3-31-03  Here is the center algorithm
   c1=d2*d3
   c2=d3*d1
   c3=d1*d2
   cent=((c2[0]+c3[0])*p1 + (c3[0]+c1[0])*p2 + (c1[0]+c2[0])*p3)/(2*c[0])
 
   return,{r:r,center:cent}
end