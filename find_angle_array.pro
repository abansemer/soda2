FUNCTION find_angle_array,x1,y1,x2,y2,x3,y3
   ;find the angle w/ vertex x1,y1
   ;x1 and y1 can be an array of points
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF typename(x1) ne 'DOUBLE' THEN stop,'find_angle_array.pro needs double floating point values.'
   
   dx1=x2-x3
   dx2=x1-x2
   dx3=x1-x3
   
   dy1=y2-y3
   dy2=y1-y2
   dy3=y1-y3
   
   ;Find length of sides a, b, and c.  a is opposite x1,y1 points
   b2=dx2^2 + dy2^2
   c2=dx3^2 + dy3^2
   a2=dx1^2 + dy1^2
   
   ;Use law of cosines to get angle
   angle=acos((b2+c2-a2)/(2*sqrt(b2*c2)))
   
   return,angle
END