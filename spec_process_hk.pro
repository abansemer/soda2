PRO spec_process_hk, op, textwidgetid=textwidgetid, fn_out=fn_out, y=y, nosav=nosav, data=data
   ;PRO to create a housekeeping file of HVPS/2DS data
   ;Send it the same op structure as soda2
   ;AB 12/2011
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(textwidgetid) eq 0 THEN textwidgetid=0
   IF n_elements(nosav) eq 0 THEN nosav=0
   soda2_update_op,op
   pop=ptr_new(op)

   ;---Initialize variables---------------------------------------
   version = 1
   IF (op.subformat ne 0) THEN BEGIN  ;3VCPI/Hawkeye/HVPS4/Fast2DS
      version = 2
      fn = op.fn+'HK'   ;These use separate file for HK data
      IF file_test(fn) eq 0 THEN stop,'File: '+fn+' not found.'
      ;Version 2 needs a new index using HK file
      hkindex=spec_index_hk(fn)
      ;Not calling read2dbuffer, so take care of timeoffset here
      hkindex.bufftime=hkindex.bufftime+(*pop).timeoffset
   ENDIF ELSE BEGIN   ;Original 2DS/HVPS3
      fn = op.fn  ;Need to have separate fn variable maintain op.fn in process_2d
      ;Check if index already built and passed in as argument
      IF (n_elements(y) eq 0) THEN hkindex=soda2_buildindex(fn[0], pop) ELSE hkindex = y
   ENDELSE

   numrecords=long((op.stoptime-op.starttime)/op.rate + 1)   ;Number of records that will be saved
   time=op.starttime + op.rate*dindgen(numrecords)     ;This is the start time for each record
   numfields=49
   IF version eq 2 THEN numfields=83
   hk=fltarr(numrecords, numfields)
   hkraw=fltarr(numrecords, numfields)  ;Not adjusted with coefficients
   tas=fltarr(numrecords)
   nb=fltarr(numrecords)


   IF n_elements(fn) gt 1 THEN stop,'Multiple SPEC raw files not supported, concatenate first'
   startdate=(soda2_parsedate(op.date)).julday
   IF abs(hkindex.date[0]-startdate) gt 5 THEN BEGIN
      ;Some probes do not have the date right, just use the first one in this case
      startdate=hkindex.date[0]
      print,'Probe date stamps do not match user date, continuing...'
   ENDIF
   hkindex.bufftime=hkindex.bufftime+86400*(hkindex.date-startdate)  ;Midnight crossings

   lun=1
   close,lun
   openr,lun,fn[0]


   ;---Read data and stuff arrays------------------
   FOR i=0,n_elements(hkindex.hkp)-1 DO BEGIN
      ;There is no time stamp on the housekeeping buffers.  Use the 4114-byte buffer time
      ;stamp immediately preceding it.
      buffind=max(where(hkindex.pointer lt hkindex.hkp[i])) > 0
      bufftime=hkindex.bufftime[buffind]
      itime=long((bufftime-op.starttime)/op.rate)    ;Index each particle into right time period

      IF (itime ge 0) and (itime lt numrecords) THEN BEGIN
         h=spec_read_hk(lun,hkindex.hkp[i],hkindex.buffsize,version=version)
         hk[itime,*]=hk[itime,*]+h.x
         hkraw[itime,*]=hkraw[itime,*]+h.raw
         tas[itime]=tas[itime]+h.tas
         nb[itime]=nb[itime]+1
      ENDIF
   ENDFOR

   ;---Make averages------------------
   ;-------Original 2DS/HVPS------------
   IF version eq 1 THEN BEGIN
      FOR i=0,numrecords-1 DO BEGIN
         n=nb[i]>1  ;Avoid divide by zero
         hk[i,0:32]=hk[i,0:32]/n    ;Do not average counts
         hk[i,36:39]=hk[i,36:39]/n
         tas[i]=tas[i]/n
      ENDFOR

      ;---Only save the data that applies to this array---
      IF op.probeid eq 'V' THEN BEGIN
         volts=hk[*,[4,29,30,5,31,32,6]]
         overloads=hk[*,44]
         laservolts=hk[*,37]
         counts=hk[*,33]
      ENDIF ELSE BEGIN
         volts=hk[*,[1,25,26,2,27,28,3]]
         overloads=hk[*,43]
         laservolts=hk[*,36]
         counts=hk[*,34]
      ENDELSE

      ;---Compute some values for 2DS---
      canpressure =  hk[*,24]*68.9476
      power = hk[*,[7,8,22,23]]
      temperature = hk[*,9:21]
      tempid=['horiz arm tx', 'horiz arm rx', 'vert arm tx', 'vert arm rx', 'horiz tip tx', 'horiz tip rx', $
              'rear optical bridge', 'DSP board', 'forward vessel', 'horiz laser', 'vert laser', 'front plate', 'power supply']
   ENDIF

   ;-------3VCPI/Hawkeye/HVPS4------------
   IF version eq 2 THEN BEGIN
      FOR i=0,numrecords-1 DO BEGIN
         n=nb[i]>1  ;Avoid divide by zero
         hk[i,2:53]=hk[i,2:53]/n    ;Do not average counts
         tas[i]=tas[i]/n
      ENDFOR

      ;---Only save the data that applies to this array---
      f1=0.00244140625  ;Conversion factor for raw units

      CASE op.probeid OF
         'V':BEGIN
            volts=hkraw[*,[36:42]]*f1  ;PSD45 = Vertical
            overloads=hkraw[*,67]
            laservolts=hkraw[*,30]  ;Current actually
            counts=hkraw[*,58]
         END
         'H':BEGIN
            volts=hkraw[*,[44:50]]*f1  ;PDS90 = Horizontal
            overloads=hkraw[*,66]
            laservolts=hkraw[*,31]  ;Current actually
            counts=hkraw[*,57]
         END
         'V50':BEGIN
            volts=hkraw[*,[2:8]]*f1
            overloads=hkraw[*,67]
            laservolts=hkraw[*,30]  ;Current actually
            counts=hkraw[*,60]
         END
         'V150':BEGIN
            volts=hkraw[*,[36:42]]*f1
            overloads=hkraw[*,67]  ;None available, using V50
            laservolts=hkraw[*,30]  ;Current actually
            counts=hkraw[*,58]
         END
         'H50':BEGIN
            volts=hkraw[*,[24,25,26,27,30,31,32]]*f1
            overloads=hkraw[*,66]
            laservolts=hkraw[*,30]  ;Current actually
            counts=hkraw[*,59]
         END
         'H150':BEGIN
            volts=hkraw[*,[44:50]]*f1
            overloads=hkraw[*,66]  ;None available, using H50
            laservolts=hkraw[*,30]  ;Current actually
            counts=hkraw[*,57]
         END
      ENDCASE

      tempraw = hkraw[*,[2,5,7,13,14,17,19,20,21]]
      tempid = ['fwd sample tube','central sample tube','nose cone','vertical laser','horizontal laser',$
                'vertical optics', 'horizontal optics','vertical mirror','horizontal mirror']
      IF (op.probetype eq 'HVPS4') THEN BEGIN
         tempraw = hkraw[*,9:21]
         tempid=['horiz arm tx', 'horiz arm rx', 'vert arm tx', 'vert arm rx', 'horiz tip tx', 'horiz tip rx', $
                 'rear optical bridge', 'DSP board', 'forward vessel', 'horiz laser', 'vert laser', 'front plate', 'power supply']
      ENDIF
      rt = 6.5536e9 * (1.-double(tempraw)/65536.) / (5. * double(tempraw))   ;Copied from 3VCPIView
      temperature = -273.15 + (1.1117024e-3 + 237.02702e-6 * alog(rt) + 75.78814e-9 * (alog(rt))^3.)^(-1.)

      power = hkraw[*,34:35]
      canpressure = (hkraw[*,29] * 0.00572205 - 3.75) * 68.9476    ;in mb, copied from 3VCPIView, seems high
   ENDIF

   ;---Interpolate TAS over missing values---
   good=where(tas gt 0, ngood)
   firstgood=min(good)
   lastgood=max(good)
   index=lindgen(numrecords)
   bad=where((tas eq 0) and (index gt firstgood) and (index lt lastgood), nbad)
   IF nbad gt 0 THEN tas[bad]=interpol(tas[good], time[good], time[bad])

   ptr_free,pop
   close,lun


   ;---Save data-----------------
   diodes=[0,21,42,64,85,106,127]
   data={op:op, time:time, nb:nb, arrayid:op.probeid, tas:tas, diodes:diodes, volts:volts, canpressure:canpressure, $
         power:power, tempid:tempid, temp:temperature, laservolts:laservolts, overloads:overloads, counts:counts }

   IF nosav eq 1 THEN return  ;This applies when called from soda2_process_2d
   fn_out=soda2_filename(op,op.shortname+'_HOUSE')
   save,file=fn_out,data
   infoline='Saved file '+fn_out
   IF textwidgetid ne 0 THEN dummy=dialog_message(infoline,dialog_parent=textwidgetid,/info) ELSE print,infoline
END
