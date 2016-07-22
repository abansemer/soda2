FUNCTION soda2_findsize, img, pop, pmisc
   ;Returns the diameter of a binary image based on the size of a pixel in each dir (xres and yres)
   ;Aaron Bansemer, NCAR, 2009
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

      
   xres=float((*pop).res)
   yres=float((*pmisc).yres) 
     
   CASE 1 OF
      ;---------------------------------------------------------------------------------------
      (*pop).smethod eq 'xsize': BEGIN   ; size only by array width of particle
         s=size(img)
         ar=1  ; can't be computed reliably with this kind of sizing
      
         ;If particle touches an edge, set ar to zero so it will be rejected
         IF (total(img[0,*]) gt 0) or (total(img[s[1]-1,*]) gt 0) THEN ar=0
         
         allin=1b  ;flag to indicate whether the particle was totally within the array
         IF  (total(img[0,*])+total(img[s[1]-1,*]) ne 0) THEN allin=0b

         IF s[2] gt 1 THEN w=where(total(img,2) gt 0) ELSE w=where(img gt 0)
         diam=(max(w)-min(w)+1)*xres

         ;diam=max(total(img,1))*xres
         return, {diam:diam, ar:ar, aspr:0, allin:allin, centerin:1b, orientation:0}
      END
      ;---------------------------------------------------------------------------------------
      (*pop).smethod eq 'ysize': BEGIN  ; size by number of slices
         s=size(img)
         ar=1  ; can't be computed reliably with this kind of sizing
          ;IF s[2] gt 1 THEN diam=max(total(img,2))*yres ELSE diam=yres
        
         IF s[2] gt 1 THEN w=where(total(img,1) gt 0) ELSE w=0
         diam=(max(w)-min(w)+1)*yres
                
         allin=1b  ;flag to indicate whether the particle was totally within the array
         IF  (total(img[0,*])+total(img[s[1]-1,*]) ne 0) THEN allin=0b

         return, {diam:diam, ar:ar, aspr:0, allin:allin, centerin:1b, orientation:0}
      END
      ;---------------------------------------------------------------------------------------
      ((*pop).smethod eq 'fastcircle_old') or ((*pop).smethod eq 'areasize'): BEGIN  ;size by approximate circle fitting (fast)
         ;Reconstruction is automatic, although it will not yet extend particles
         ;beyond a size found by center-in sizing.
         ;IF xres ne yres THEN print,'Square pixels required for fastcircle sizing, using x-resolution only.'
         area_original=total(img) * (yres/xres) ;area of particle
         s=size(img)
         IF s[2] gt 2 THEN im=erode(img,[[0,1,0],[1,1,1],[0,1,0]]) xor img ELSE im=img   ;only use outline
   
         z=where(im eq 1, area)  ;these 3 lines get area and indices of each occluded element
         y_ind=z/s[1]            ;remember that y is along airflow, x along array
         x_ind=z mod s[1]        ;area here is area of perimeter, not full particle
   
         y_ind=y_ind * (yres/xres) ;Correct for TAS error in y-index
         temp=min_circle_fast(x_ind,y_ind)
         diam=(temp.diam+1)*xres  ;plus 1 is due to the fact we're looking a pixels with 0.5 shading threshold, not points

         r=diam/xres/2.0
         x=temp.center[0]
         centerin=1b
         IF (x eq 0) or (x eq s[1]-1) THEN centerin=0b
         theta=acos((x/r) <1)           ;angle
         phi=acos(((s[1]-1-x)/r) <1)
         ; find area:       triangles(left)   triangles(right)         (remaining wedges)
         circle_area_imaged=x*r*sin(theta) + (s[1]-1-x)*r*sin(phi) + !pi*r^2*((!pi-phi-theta)/!pi)
         ar=(area_original/circle_area_imaged) < 1.0

         ;if (phi+theta) gt 0 then allin=0 else allin=1
         IF  (total(img[0,*])+total(img[s[1]-1,*]) ne 0) THEN allin=0b ELSE allin=1b
         IF (*pop).smethod eq 'areasize' THEN BEGIN
            ;Adjust for partially imaged particles using the centerpoint of the circle fit.
            area_adjusted=area_original*((!pi*r^2)/circle_area_imaged)
            ;Note: original VOCALS testing with this (Feb 2013) did not have the 4/pi factor included, or area_adjusted.
            diam=sqrt(4.0/!pi*area_adjusted*xres*yres)            
         ENDIF
         return, {diam:diam, ar:ar, aspr:0, allin:allin, c:temp.center, centerin:centerin, orientation:0}  
      END 
      ;---------------------------------------------------------------------------------------
      (*pop).smethod eq 'fastcircle': BEGIN  ;size by approximate circle fitting (fast)
         
         ;IF xres ne yres THEN print,'Square pixels required for fastcircle sizing, using x-resolution only.'
         area_original=total(img) * (yres/xres) ;area of particle
         s=size(img)
              
         ;Fastcircle is used within aspect_ratio routine, so don't repeat here
         aspr=aspect_ratio(img, circle=circle, tas_adjust=yres/xres, orientation=orientation, makeplot=0)
         diam=(circle.diam+1)*xres  ;plus 1 is due to the fact we're looking a pixels with 0.5 shading threshold, not points

         r=diam/xres/2.0
         x=circle.center[0]
         centerin=1b
         IF (x eq 0) or (x eq s[1]-1) THEN centerin=0b
         theta=acos((x/r) <1)           ;angle
         phi=acos(((s[1]-1-x)/r) <1)
         ; find area:       triangles(left)   triangles(right)         (remaining wedges)
         circle_area_imaged=x*r*sin(theta) + (s[1]-1-x)*r*sin(phi) + !pi*r^2*((!pi-phi-theta)/!pi)
         ar=(area_original/circle_area_imaged) < 1.0

         ;if (phi+theta) gt 0 then allin=0 else allin=1
         IF  (total(img[0,*])+total(img[s[1]-1,*]) ne 0) THEN allin=0b ELSE allin=1b
         return, {diam:diam, ar:ar, aspr:aspr, allin:allin, c:circle.center, centerin:centerin, orientation:orientation}  
      END 
      ;--------------------------------------------------------------------------------------- 
      (*pop).smethod eq 'maxsize': BEGIN  ;----- This is the main routine for diagonal sizing --------
         ;***** WARNING, area ratio for reconstructed particles is wrong! *****
         area_original=total(img) ;area of particle
         s=size(img)
         IF (s[0] gt 1) and (s[1] gt 2) and (s[2] gt 2) THEN im=erode(img,[[0,1,0],[1,1,1],[0,1,0]]) xor img ELSE im=img   ;only use outline!!
   
         z=where(im eq 1, area)  ;these 3 lines get area and indices of each occluded element
         y_ind=z/s[1]            ;remember that y is along airflow, x along array
         x_ind=z mod s[1]        ;area here is area of perimeter, not full particle
   
         maxlng2 =0l   
         FOR i=0,area-1 DO BEGIN    ;search through for largest diam
            maxlng2 =max(((abs(x_ind(i)-x_ind(i:area-1))+1)*xres)^2+ $
            ((abs(y_ind(i)-y_ind(i:area-1))+1)*yres)^2) > maxlng2
         ENDFOR   
         diam = sqrt(maxlng2) > xres  ;set minimum size as x-res
         ar=(area_original*xres*yres)/(0.25*3.1416*diam^2);

         dr=0.0
         allin=1b  ;flag to indicate whether the particle was totally within the array
         IF  (total(img[0,*])+total(img[s[1]-1,*]) ne 0) THEN allin=0b
         IF (*pop).reconstruct and (allin eq 0) THEN BEGIN
            ;****DIAMETER****
            size_right = 0.0 & size_left=0.0
            x1=float((max(x_ind)-min(x_ind) + 1)*xres)  ; get size in x direction
            temp=where(x_ind eq 0)       ; get indices on left boundary
            IF temp[0] ne -1 THEN size_left=float((max(y_ind[temp])-min(y_ind[temp])+1)*yres)
            temp=where(x_ind eq s[1]-1)  ; get indices on right boundary
            IF temp[0] ne -1 THEN size_right=float((max(y_ind[temp])-min(y_ind[temp])+1)*yres)    
            y1=size_left < size_right    ; this follows convention from Heyms & Parrish
            y2=size_left > size_right
            ;Find diameter from Heyms & Parrish 1978, note x and y here opposite of paper
            dr=((x1+((y2^2-y1^2)/(4*x1)))^2+y1^2)^0.5 
            diam=diam>dr ;take the larger of the two values for diameter
      
            ;****AREA RATIO****
            IF y1 eq 0 THEN BEGIN    ; one side occluded
               theta2=2.0*asin(y2/diam)
               IF x1 gt diam/2 THEN theta2=2*!pi - theta2
               area_r=theta2/8*diam^2 - (y2*(diam/2-x1))/2                 
            ENDIF ELSE BEGIN ; both sides occluded
               theta2=2.0*asin(y2/diam)
               theta1=2.0*asin(y1/diam)
               h=sqrt(diam^2-(y1/2)^2)
               area_r=diam^2/8*(2*!pi-theta1-theta2) + y1*h/2 + y2/2*(x1-h)
            ENDELSE
            ar=area_original*xres*yres/area_r < 1.0 ; max ar is 1;
         ENDIF
         return, {diam:diam, ar:ar, aspr:0, allin:allin, centerin:1b, orientation:0}  ; return larger of two values
         ;---------------------------------------------------------------------------------------
      END
      ELSE: print,'Unknown sizing method specified in findsize.pro'
   ENDCASE 
END
