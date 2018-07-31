FUNCTION soda2_reject, a, inttime, nextinttime, cutoff, clustercount, pop
   ;FUNCTION to decide whether a SODA2 particle is accepted or rejected.
   ;a is the structure that comes soda2_readpbp.pro.  Interarrival time in 
   ;seconds.  Cutoff is for interarrival time.
   ;Aaron Bansemer, NCAR, 2009
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.


   ;Rejection criteria in increasing order of importance. Eventually change back to binary sum?
   reject=0               
   IF ((*pop).centerinrejection eq 1) and (a.centerin eq 0) THEN reject=6  ;Center-in rejection
   IF (a.ar lt 0.10) or (a.ar gt 1.0) THEN reject=1      ;Low area ratio
   IF (inttime lt cutoff) or (nextinttime lt cutoff) THEN reject=2                  ;Below interarrival
   IF (a.size lt min((*pop).endbins)) or (a.size gt max((*pop).endbins)) THEN reject=3     ;Out of size range
   IF ((*pop).reconstruct eq 0) and (a.allin eq 0) THEN reject=4
   IF ((*pop).clusterthresh gt 0) and (clustercount ge 2) THEN reject=5
   ;IF a.nsep ge 1 THEN reject=6  ;Look for gaps between two particles  ;Now done with 'keeplargest' option

   ;Water rejection
   IF (*pop).water eq 1 THEN BEGIN
      ;The 10um F2DC need different thresholds.  This could be cleaned up better in the future....
      IF ((*pop).probeid eq 'C6') THEN BEGIN
         IF (a.ar lt 0.30) or (a.ar lt 0.40 and a.size gt (*pop).res*10.0) THEN reject=1
      ENDIF ELSE BEGIN
         IF (a.ar lt 0.40) or (a.ar lt 0.50 and a.size gt (*pop).res*10.0) THEN reject=1
      ENDELSE
      IF a.size gt 6000 THEN reject=1     
   ENDIF
   
   ;Irregular-only (non-water) rejection
   IF (*pop).water eq -1 THEN BEGIN
      IF (a.ar ge 0.50) or (a.ar ge 0.40 and a.size le (*pop).res*10.0) THEN reject=1
   ENDIF
 
   return,reject
END
