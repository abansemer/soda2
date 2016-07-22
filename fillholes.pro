FUNCTION fillholes, img
   ;FILL holes in a binary image
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   img=byte(img)
   
   s=size(img)
   IF (s[1] lt 4) or (s[2] lt 4) THEN return, img   ;can't do small particles
   reverse=byte(label_blobs(img xor 1b)) ; make a negative of the image & label resulting blobs
   
   edges=reverse
   f=[transpose(edges[0,*]), edges[*,0], transpose(edges[s[1]-1,*]), edges[*,s[2]-1]]
   f=f[sort(f)]              ;f is an array of values on the edge of the image
   edge_labels=f[uniq(f)]    ;these values touch the edge, so they will be removed
   
   FOR i=0,n_elements(edge_labels)-1 DO BEGIN       ;remove the blobs that touch an edge
      reverse[where(reverse eq edge_labels[i])]=0b
   ENDFOR
   
   newimg=img or (reverse<1b)      ;'add up' the original image and the remaining holes
   return,newimg
END
