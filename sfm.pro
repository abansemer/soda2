function sfm, hms
   ;Convert hhmmss to seconds from midnight
   ;works for double floats, just pass in hms as a double
   hour=long(hms)/10000
   minute=(long(hms)-hour*10000l)/100
   second=(long(hms)-hour*10000l-minute*100l)
   fraction=double(hms)-long(hms)
   sfm=hour*3600l+minute*60l+second+fraction      
   return,sfm
end