FUNCTION fix2dimage, image
   ;FUNCTION to fix 2d image buffers recorded by NCAR probes.
   ;The sync words and timelines are different on NCAR probes.
   ;This procedure detects those slices and replaces them with 
   ;the standard slices.
   ;Tested good for C130 SHEBA data, 9/2/2003 AB.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   test1=where(image eq 'ff800000'x or image eq 0 or image eq 'ff000000'x or image eq '2a000000'x, num)       ;find sync words (NCAR format)
   IF num lt 5 THEN return,image   ;bad buffer, return unchanged
   
   ;Get rid of adjacent timelines (a common error)
   diff=test1[1:num-1]-test1[0:num-2]
   good=where(diff gt 1)
   If good[0] ne -1 THEN test1=test1[good]
   
   test2=where(image[(test1>2)-2] eq 'ffffffff'x or test1 lt 2) ;look for blank lines 2 slices back
   IF test2[0] eq -1 THEN return,image   ;bad buffer, return unchanged
   
   timelines=test1[test2]
   
   img=image
   ;the first 'or' is to make all bad timelines alike (some are full bars all the way across the array)
   ;the xor adds in the sync pattern
   img[timelines]=(image[timelines] or 'ff000000'x) xor 'aa000000'X     ;add in the proper sync word
   img[timelines-1]=(image[timelines-1] or 'ff000000'x) xor 'aa000000'X 
   return,img
END
