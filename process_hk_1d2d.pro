PRO process_hk_1d2d, op, textwidgetid=textwidgetid, fn_out=fn_out, y=y, nosav=nosav, data=data
   ;Get 1D2D housekeeping data from an SEA file.  This information is embedded in the first several lines
   ;of the particle buffer.
   ;Add other housekeeping data as it becomes available.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(textwidgetid) eq 0 THEN textwidgetid = 0
   IF n_elements(nosav) eq 0 THEN nosav = 0
   soda2_update_op, op
   pop = ptr_new(op)

   ;Initialize variables
   numrecords = long((op.stoptime-op.starttime)/op.rate + 1)   ;Number of records that will be saved
   time = op.starttime + op.rate*dindgen(numrecords)     ;This is the start time for each record
   ;Placeholders for volts and temp needed for soda2_windowplot.pro plotting
   data = {op:op, time:time, largereject:fltarr(numrecords), smallreject:fltarr(numrecords), $
      dofpercent:fltarr(numrecords), leftreject:fltarr(numrecords), rightreject:fltarr(numrecords), $
      dofnumreject:fltarr(numrecords), temp:fltarr(numrecords,1), tempid:'Not Available', $
      volts:fltarr(numrecords,3), diodes:[0,31,63]}

   ;Get file information and process
   openr, lun, op.fn, /get_lun
   p = sea_tagpositions(lun, op.seatag[0])
   n = n_elements(p.datapointer)
   date = dblarr(n)
   hms = dblarr(n)
   time = intarr(9)
   timelines = ulon64arr(3)
   lastitime = -1
   FOR i = 0, n-1 DO BEGIN
      ;Time data
      point_lun, lun, p.timepointer[i]
      readu, lun, time
      date[i] = time[0]*10000d + time[1]*100d + time[2]
      hms[i] = time[3]*10000d + time[4]*100d + time[5] + time[6]/double(time[7])
      itime=long((sfm(hms[i])-op.starttime)/op.rate)    ;Index each particle into right time period

      ;Save data if a new time period is detected
      IF (itime ne lastitime) and (itime ge 0) and (itime lt numrecords) THEN BEGIN
         ;Particle data
         point_lun, lun, p.datapointer[i]
         readu, lun, timelines
         data.largereject[itime] = ishft(timelines[1] and '00FFF00000000000'x, -44)  ;Maximum number of slices, otherwise reject
         data.smallreject[itime] = ishft(timelines[1] and '00000FFF00000000'x, -32)  ;Minimum number of slices, otherwise reject
         data.dofpercent[itime]  = ishft(timelines[1] and '000000003C000000'x, -26)  ;Indicates which dof% setting is used, see manual
         data.leftreject[itime]  = ishft(timelines[1] and '0000000000FFF000'x, -12)  ;Number of slices required on left edge to reject
         data.rightreject[itime] = timelines[1] and '0000000000000FFF'x    ;Number of slices required on right edge to reject
         data.dofnumreject[itime] = ishft(timelines[2] and '00FFF00000000000'x, -44)  ;Number of pixels required at 75% to accept
      ENDIF
   lastitime = itime
   ENDFOR

   close,lun

   ;Save data
   IF nosav eq 1 THEN return  ;This applies when called from soda2_process_2d
   fn_out = soda2_filename(op, op.shortname+'_HOUSE')
   save, file=fn_out, data
   infoline='Saved file '+fn_out
   IF textwidgetid ne 0 THEN dummy=dialog_message(infoline,dialog_parent=textwidgetid,/info) ELSE print,infoline
END
