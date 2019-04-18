FUNCTION soda2_findsize, img, xres, yres
   ;Returns the diameter of a binary image based on the size of a pixel in each dir (xres and yres)
   ;Aaron Bansemer, NCAR, 2009
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   ;Predefine output struct to ensure consistent data types
   out={diam:0.0, xsize:0.0, ysize:0.0, areasize:0.0, ar:0.0, aspr:0.0, allin:0b, c:fltarr(2), centerin:1b, $
            orientation:0.0, perimeterarea:0L, edgetouch:0b}
   
   area_original=total(img) * (yres/xres) ;area of particle
   IF area_original eq 0 THEN return, out ;zero area image, return
   s=size(img)
   ;ndims=size(img,/n_dim)
   IF s[0] eq 1 THEN BEGIN  ;Image must be a 2D array
      img=reform(img, s[1], 1)
      s=size(img)
   ENDIF
   
   ;xsize
   w=where(total(img,2) gt 0)   ;IF ndims gt 1 THEN  ELSE w=where(img gt 0)
   out.xsize=(max(w)-min(w)+1)*xres

   ;ysize
   w=where(total(img,1) gt 0)   ; ELSE w=0
   out.ysize=(max(w)-min(w)+1)*yres
       
   ;Fastcircle is used within aspect_ratio routine, so don't repeat here
   out.aspr=aspect_ratio(img, circle=circle, tas_adjust=yres/xres, orientation=orientation, makeplot=0, x_ind=x_ind, y_ind=y_ind)
   out.diam=(circle.diam+1)*xres  ;plus 1 is due to the fact we're looking a pixels with 0.5 shading threshold, not points
   out.c=circle.center
   out.orientation=orientation
   out.perimeterarea=circle.area 
   IF  (total(img[0,*])+total(img[s[1]-1,*]) ne 0) THEN out.allin=0b ELSE out.allin=1b
   
   ;Add in edgetouch to eventually replace 'allin', 0=allin, 1=left edge, 2=right edge, 3=both edges
   nshadowed=total(img,2)
   ileft=0
   iright=s[1]-1
   img2 = img+!values.f_nan   ;Make an image of NaNs for getting the y-extent on each diode
   IF (size(img2))[0] eq 1 THEN img2=reform(img2, s[1], 1)  ;IDL error where won't initialize 2D array
   img2[x_ind, y_ind]=y_ind          ;Fill image with the slice index of each shadowed pixel
   extent = max(img2, dim=2) - min(img2, dim=2)
   IF (nshadowed[ileft] gt 0) THEN BEGIN
      out.edgetouch += 1b
      ;This center-in method seems to work best so far, based on simulations. 
      ;Better than looking for circle center on/near edge.
      ;May want to move to max extent rather than max shadowed.
      ;IF nshadowed[ileft] ge max(nshadowed) THEN out.centerin=0b
      IF extent[ileft] ge max(extent) THEN out.centerin=0b
   ENDIF
   IF (nshadowed[iright] gt 0) THEN BEGIN
      out.edgetouch += 2b
      ;IF nshadowed[iright] ge max(nshadowed) THEN out.centerin=0b
      IF extent[iright] ge max(extent) THEN out.centerin=0b
   ENDIF
   
   r=out.diam/xres/2.0
   x=circle.center[0]
   ;out.centerin=1b
   ;IF (x eq 0) or (x eq s[1]-1) THEN out.centerin=0b
   
   ;Truncate the circle if the particle is not fully imaged
   IF out.allin eq 0 THEN BEGIN
      theta=acos((x/r) <1)           ;angle
      phi=acos(((s[1]-1-x)/r) <1)
      ; find area:       triangles(left)   triangles(right)         (remaining wedges)
      circle_area_imaged=x*r*sin(theta) + (s[1]-1-x)*r*sin(phi) + !pi*r^2*((!pi-phi-theta)/!pi)     
   ENDIF ELSE circle_area_imaged = !pi * r^2  
   out.ar=(area_original/circle_area_imaged) < 1.0

   out.areasize=out.diam*sqrt(out.ar)   ;Can show by derivation.  This also adjusts for partial images.
 
   return, out
END
