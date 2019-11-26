FUNCTION soda2_reject, a, inttime, nextinttime, cutoff, clustercount, binningsize, pop
   ;FUNCTION to decide whether a SODA2 particle is accepted or rejected.
   ;a is the structure that comes soda2_readpbp.pro.  Interarrival time in 
   ;seconds.  Cutoff is for interarrival time.
   ;Aaron Bansemer, NCAR, 2009
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.


   ;Rejection criteria in increasing order of importance.  Binary sum keeps track of reasons.
   reject=0b
   IF (a.arearatio lt 0.10) or (a.arearatio gt 1.0) THEN reject+=1           ;Low area ratio
   IF (inttime lt cutoff) or (nextinttime lt cutoff) THEN reject+=2          ;Below interarrival
   IF (binningsize lt min((*pop).endbins)) or (binningsize gt max((*pop).endbins)) or (finite(binningsize) eq 0) THEN reject+=4     ;Out of size range
   IF ((*pop).eawmethod eq 'allin') and (a.allin eq 0) THEN reject+=8        ;All-in rejection
   IF ((*pop).eawmethod eq 'centerin') and (a.centerin eq 0) THEN reject+=8  ;Center-in rejection
   IF ((*pop).clusterthresh gt 0) and (clustercount ge 2) THEN reject+=16    ;Cluster rejection

   ;Water rejection
   IF (*pop).water eq 1 THEN BEGIN
      IF (a.arearatiofilled lt 0.40) or ((a.arearatiofilled lt 0.50) and (a.diam gt (*pop).res*10.0)) or (binningsize gt 6000) THEN reject+=32
   ENDIF
   
   ;Irregular-only (non-water) rejection
   IF (*pop).water eq -1 THEN BEGIN
      IF (a.arearatio ge 0.50) or (a.arearatio ge 0.40 and a.diam le (*pop).res*10.0) THEN reject+=64
   ENDIF
 
   return,reject
END
