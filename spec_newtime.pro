FUNCTION spec_newtime, fn, data=data
   ;Recompute time from SPEC data to better account for rollovers and deadtime
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(data) eq 0 THEN restorenc, fn, var=['rawtime','buffertime','probetime','probetas']

   IF max(data.rawtime) lt 2ULL^31 THEN stop, 'File does not contain full rawtime, just truncated.'

   ;Compute delta counter for computing inttime
   dcounter = [0, data.rawtime[1:*] - data.rawtime]
   dbuffer = [0, data.buffertime[1:*] - data.buffertime]

   ;Find rollovers, should happen every 5.5 minutes at 150 m/s
   ;Based on both negative dcounter or large gap in buffer time
   rollover = where((dcounter lt 0) or (dbuffer gt 200), nrollover)
   dcounter[rollover] += 2ULL^32    ;Add the rollover max value to get positive inttimes

   ;Convert dcounter to inttime based on probe's airspeed
   freq = double(data.global.yres/(1.0e6*data.probetas))  ;Time interval of each tick in a timeline
   inttime = dcounter * freq

   ;First guess is buffertime, should be no drift or rollovers
   itimematch = where(dbuffer gt 0) - 1   ;We want to match buffertime to particletime at the end of each buffer
   newtime = data.buffertime
   elaptime = data.buffertime*0d

   ;Figure the best time for each rollover period
   FOR i = 0, nrollover DO BEGIN   ;No -1 in the loop is intentional, since need to get particles after last rollover
      IF i eq 0 THEN istart = 0 ELSE istart = rollover[i-1]
      IF i eq nrollover THEN istop = n_elements(dcounter)-1 ELSE istop = rollover[i]-1
      elaptime[istart:istop] = total(inttime[istart:istop], /cumulative)

      matches = where((itimematch ge istart) and (itimematch le istop), nmatches)
      offset = median(data.buffertime[itimematch[matches]] - elaptime[[itimematch[matches]]] )
      newtime[istart:istop] = elaptime[istart:istop]+offset
   ENDFOR

   return, newtime
END
