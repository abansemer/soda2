function min_circle_fast,xin,yin
   ;finds the minimum bounding circle for a set of points
   ;given by x and y.
   ;Based on algorithm in Graphics Gems II, article by Jon Rokne
   ;A faster version that eliminates the 'for' loops 6/2012
   ;Aaron Bansemer, NCAR
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   area=n_elements(xin)
   if area eq 1 then return,{diam:0.0, center:[xin[0],yin[0]], corners:[0,0], area:1}
   x=double(xin)
   y=double(yin)

   dummy=min(y,P) ; gets point with smallest y
   
   ;Find point 'Q', smallest angle with P and x-axis
   b=y-y[P]
   a=abs(x-x[P])
   angle=atan(float(b)/a)
   angle[P]=100  ;Ignore this point
   angX=min(angle, Q)
   ;Q=max(where(angle eq min(angle)))  ;Speedup, use point furthest away to start, ** CAUSES ERROR... WHY? **
   c=0L   ;A counter to prevent infinite loops

   REPEAT BEGIN
      doagain=0
      ;Find point 'R', which minimizes angle PRQ

      anglearray=find_angle_array(x,y,x[P],y[P],x[Q],y[Q])
      anglearray[[P,Q]]=100 ;Ignore these
      angR=min(anglearray, R)
      
      angQ=find_angle([x[Q],y[Q]],[x[R],y[R]],[x[P],y[P]])
      angP=!pi-angR-angQ

      ;Look for obtuse angles in P and Q vertices
      IF angP ge !pi/2 THEN BEGIN
         doagain=1
         P=R
      ENDIF
      IF angQ ge !pi/2 THEN BEGIN
         doagain=1
         Q=R
      ENDIF     
      ;Limit the number of iterations, usually ~10% of number of points, but
      ;can be infinite for a few weird particles.
      c=c+1
      IF c gt 10000 THEN doagain=0 
   ENDREP UNTIL not doagain
   
   IF angR ge !pi/2 THEN BEGIN
      ;Circle defined by 2 points
      radius=sqrt((x[P]-x[Q])^2+(y[P]-y[Q])^2)/2.0
      center=[(x[P]+x[Q])/2.0,(y[P]+y[Q])/2.0]
      circ={r:radius,center:center}
      corners=[P,Q]
   ENDIF ELSE BEGIN
      ;Circle defined by 3 points
      circ=find_radius([x[P],y[P]],[x[Q],y[Q]],[x[R],y[R]])
      corners=[P,Q,R]
   ENDELSE

   return,{diam:circ.r*2, center:circ.center, corners:corners, area:area}  ;RETURNS *DIAMETER*
END
   
   
   
