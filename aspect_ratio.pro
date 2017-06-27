FUNCTION aspect_ratio, img_in, tas_adjust=tas_adjust, makeplot=makeplot, circle=circle, orientation=orientation, coordinates=coordinates
   ;Find the aspect ratio of a particle
   ;'coordinates' flag means img_in is a set of coordinates in a [n,2] array rather than an image
   ;AB 3/2014
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(coordinates) eq 0 THEN coordinates=0

   IF coordinates eq 0 THEN BEGIN   
      img=byte(img_in)
      s=size(img)
      IF s[2] gt 2 THEN im=erode(img,[[0,1,0],[1,1,1],[0,1,0]]) xor img ELSE im=img   ;only use outline

      z=where(im eq 1, area)  ;these 3 lines get area and indices of each occluded element
      y_ind=z/s[1]            ;remember that y is along airflow, x along array
      x_ind=z mod s[1]        ;area here is area of perimeter, not full particle
   ENDIF ELSE BEGIN
      x_ind=img_in[*,0]
      y_ind=img_in[*,1]
      s=[2, max(x_ind), max(y_ind)]
   ENDELSE
   IF n_elements(tas_adjust) gt 0 THEN y_ind=y_ind*tas_adjust  ;Correct for TAS error in y-index
   circle=min_circle_fast(x_ind,y_ind)

   IF n_elements(circle.corners) eq 3 THEN BEGIN
      ;Need to find minimum angle when circle defined by 3 points, then bisect that angle
      ; to get a good axis for determining aspect ratio.
      P=circle.corners[0]
      Q=circle.corners[1]
      R=circle.corners[2]
      angP=find_angle([x_ind[P],y_ind[P]],[x_ind[R],y_ind[R]],[x_ind[Q],y_ind[Q]])
      angQ=find_angle([x_ind[Q],y_ind[Q]],[x_ind[R],y_ind[R]],[x_ind[P],y_ind[P]])
      angR=find_angle([x_ind[R],y_ind[R]],[x_ind[Q],y_ind[Q]],[x_ind[P],y_ind[P]])
      
      ;Find vertex of smallest angle. Use as a corner.
      dummy=min([angP, angQ, angR], vertex)   
      cx1=x_ind[circle.corners[vertex]]
      cy1=y_ind[circle.corners[vertex]]
      
      ;Use circle center as other 'corner'
      cx2=circle.center[0]
      cy2=circle.center[1]      
   ENDIF ELSE BEGIN  ;Min circle defined by two points, just use the corners
      cx1=x_ind[circle.corners[0]]
      cy1=y_ind[circle.corners[0]]
      cx2=x_ind[circle.corners[1]]
      cy2=y_ind[circle.corners[1]]
   ENDELSE
   
   ;Compute orientation, in degrees, range is -180 to +180
   orientation=atan(float(cy2-cy1), float(cx2-cx1)) * 180/!pi

   ;Get angle in format of -90 to +90 degrees with respect to x-axis (across array), since otherwise can be ambiguous
   ;Tricky math, but this works (tested using make_column.pro at all angles):
   orientation=((orientation - 360 - 90) mod 180) + 90

   ;Use line in form of Ax+By+C=0 
   ;Use this formula to compute A, B, and C: (y1 – y2)x + (x2 – x1)y + (x1y2 – x2y1) = 0
   ;Got from https://bobobobo.wordpress.com/2008/01/07/solving-linear-equations-ax-by-c-0/
   a=cy1-cy2
   b=cx2-cx1
   c=cx1*cy2 - cx2*cy1
   
   den=sqrt(a^2+b^2) > 1  ;Denominator in distance formula
   
   distmajor=(a*x_ind +  b*y_ind + c)/den
   
   ;Use width as sum of max distance from one side of major axis plus max distance from other side.
   ;  as in Korolev and Isaac 2003. 
   ;plus 1 is due to the fact we're looking a pixels with 0.5 shading threshold, not points
   aspr=(max(distmajor)-min(distmajor)+1)/(circle.diam+1) 
   
   IF n_elements(makeplot) eq 0 THEN makeplot=0
   IF makeplot THEN BEGIN
      window,0
      plot,x_ind,y_ind,psym=2,xr=[0,s[1]],yr=[0,s[2]],/iso,xtit='Pixels',ytit='Pixels'
      draw_circle,circle.center[0],circle.center[1],circle.diam
      oplot,x_ind[circle.corners],y_ind[circle.corners],psym=2,color=150
      oplot,[cx1,cx2],[cy1,cy2],psym=1,color=250
      x=findgen(s[1])
      y=(-a*x - c)/b
      oplot,x,y,color=100
      xyouts, 0.1, 0.1, 'Aspect Ratio: ' + string(aspr), /norm
      xyouts, 0.1, 0.2, 'Orientation (deg): ' + string(orientation), /norm
      xyouts, 0.1, 0.15, 'Diameter (pixels): ' + string(circle.diam), /norm
   ENDIF
   IF makeplot eq 2 THEN BEGIN
      z=where(img eq 1, area)  ;these 3 lines get area and indices of each occluded element
      y_ind2=z/s[1]            ;remember that y is along airflow, x along array
      x_ind2=z mod s[1]        ;area here is area of perimeter, not full particle
      p=plot(x_ind2,y_ind2,'+',xrange=[0,s[1]],yrange=[0,s[1]],/buffer)  ;Plot to buffer, remove if want display
      p.aspect_ratio=1
      e=ellipse(circle.center[0],circle.center[1],major=circle.diam/2.0+0.5,/data,$
                fill_transparency=80,fill_color='blue') 
      p2=plot(x_ind2,y_ind2,'ks',sym_filled=1,/overplot,sym_size=1.1)
      
      anchors=plot(x_ind[circle.corners],y_ind[circle.corners],'ks',sym_filled=1,/overplot,fill_color='red')
      ;line=plot(x_ind[circle.corners],y_ind[circle.corners],'r',/overplot,thick=2)  ;Temporarily remove line since ugly if 3 points
      ;p.save,'aspect_image.png'
   ENDIF


   return, aspr < 1.0   ;Eliminate round-off errors
END
