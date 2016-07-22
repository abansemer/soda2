function hms2sfm, hms
   ;Convert hhmmss to seconds from midnight
   ;works for double floats, just pass in hms as a double
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   hour=long(hms)/10000
   minute=(long(hms)-hour*10000l)/100
   second=(long(hms)-hour*10000l-minute*100l)
   fraction=double(hms)-long(hms)
   sfm=hour*3600l+minute*60l+second+fraction      
   return,sfm
end