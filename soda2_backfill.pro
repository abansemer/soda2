FUNCTION soda2_backfill, image
   ;FUNCTION to reconstruct the first line of a 2d particle
   ;(the first image line is not recorded)
   ;Uses an eroded version of the first recorded line as the real first line
   
   ;es is erosion structure, increasing length increases amount of erosion.
   ;This version increases es until at least half the first recorded line
   ;is eroded, then sets it as real first line.
   ;The IF statements in REPEAT loop set the direction of erosion to be towards
   ;the edge that is occluded, if an edge is occluded.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.


   s=size(image)
   if s[0] eq 1 then return,image  ; image too small
   if max(image) eq 0 then return, image ; zero-area image
   
   temp=image
   
   count=1
   orig=temp[*,1]
   origtot=total(temp[*,1])
   if origtot lt 2 then return, image  ;not enough pixels on first slice to erode 
   es=[1]
   
   
   REPEAT BEGIN
      es=[es,1]
      if orig[0] eq 1 then x0=0 else x0=count/2.0
      if orig[s[1]-1] eq 1 then x0=count
      temp[*,0]=erode(orig,es,x0)
      count=count+1
   ENDREP UNTIL total(temp[*,0]) lt origtot/2.0
      
   return,temp
END
