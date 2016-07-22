FUNCTION find_angle,p1,p2,p3
   ;find the angle w/ vertex p1
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   B=p2-p1
   C=p3-p1   
   angle=acos(total(B*C)/sqrt(total(B^2)*total(C^2)))
   return,angle
END