PRO process_cip1d_sea, op, nowrite=nowrite, data=data
   ;PRO to read in SEA CIP-1D data, and return it
   ;in a structure in IDL's native format.
   ;starttime and stoptime entered in sfm
   ;tas is an output variable
   ;the nowrite option will not write a file... this is if you're just getting TAS
   ;op should be a structure with the following tags:
   ;   starttime [sfm]
   ;   stoptime [sfm]
   ;   rate
   ;   date
   ;   fn  [mulitple is OK]
   ;All units in m, s, mb, C
   ;Aaron Bansemer, 02-2007

   IF n_elements(nowrite) eq 0 THEN nowrite = 0

   starttime = long(op.starttime) & stoptime = long(op.stoptime) & rate = long(op.rate)
   IF stoptime lt starttime THEN stoptime = stoptime+86400d
   num = (stoptime-starttime)/rate +1 ;number of records that will be saved

   IF (total(tag_names(op) eq 'SEATAG') gt 0) THEN BEGIN  ;SODA2
      tag = op.seatag[1]
      timeoffset = op.timeoffset
   ENDIF ELSE BEGIN  ;Old call from SODA1
      pversion = probeversion(op)
      timeoffset = pversion.timeoffset
   ENDELSE

   ;These are the endbins for the NAMMA project... not necessarily valid!
   ;res = 25
   endbins = op.res*(findgen(63)+1) + op.res/2.0
   numbins = n_elements(endbins)-1
   midbins = (endbins[0:numbins-1]+endbins[1:numbins])/2d
   binwidth = endbins[1:numbins]-endbins[0:numbins-1]

   spec = fltarr(num, numbins)  ;size spectra
   hk = fltarr(num,16)   ;housekeeping data
   nb = lonarr(num)  ;nb = num buffers for each record
   endreject = lonarr(num)
   oversizereject = lonarr(num)
   dofreject = lonarr(num)
   particlecounter = lonarr(num)
   time = starttime+rate*dindgen(num)  ; this is the start time for each record

   close,1
   filenum = 0
   openr,1,op.fn[0]

   ;-------Find first line of interest------------
   REPEAT BEGIN
      a = readseabuffer_caps(1,probetype='CIP1D',tag=tag)
      a.starttime = a.starttime+timeoffset
      IF a.eof eq 1 THEN BEGIN
         close,1
         filenum++
         openr,1,op.fn[filenum]
         print,'Opened: ',op.fn[filenum]
      ENDIF
   ENDREP UNTIL sfm(a.starttime) ge starttime

   ;-------Read in subsequent data----------------
   WHILE round(sfm(a.starttime)) le stoptime DO BEGIN
      i = (round(sfm(a.starttime))-starttime)/rate ;find index for each variable
      IF i ge 0 and i lt num THEN BEGIN ;This is for bad time stamps
         ;Check for bad data
         nb[i]++
         IF min(a.image.count) lt 0 THEN a.image.count[*] = 0  ;An error with large negative counts in MC3E
         spec[i,*] += a.image.count
         hk[i,*] += a.image.housekeeping
         endreject[i] += a.image.endreject
         oversizereject[i] += a.image.oversizereject
         dofreject[i] += a.image.dofreject
         particlecounter[i] += a.image.particlecounter
         IF a.eof eq 1 THEN BEGIN
            close,1
            filenum++
            openr,1,op.fn[filenum]
            print,'Opened: ',op.fn[filenum]
         ENDIF
      ENDIF
      a = readseabuffer_caps(1,probetype='CIP1D',tag=tag)
      a.starttime = a.starttime+timeoffset
      IF sfm(a.starttime) lt starttime THEN a.starttime = a.starttime+240000.0 ;midnight crossing
   ENDWHILE
   close,1

   nb = nb>1
   tas = fltarr(num)
   tas[*] = !values.f_nan
   FOR i = 0L,num-1 DO BEGIN
      hk[i,*] = hk[i,*]/nb[i]
      temp = (hk[i,13]-1024)*.048828125  ;got this from Darrel's Fortran code
      cp = 0.24 & cv = 0.171 & R = 0.0668557 & rcv = 1.0

      difprs =  68.9476 *5*hk[i,3]/4095
      totprs = 68.9476*15*hk[i,4]/4095
      Tm = temp+273.16
      IF (difprs gt 1  and  totprs gt 10) THEN BEGIN
         xmach = sqrt(2*(cv/R)*((difprs/totprs +1)^(R/cp) -1))
         Ta = Tm / (1 + (rcv*xmach^2) * (cp/cv- 1)/2)
         tas[i] = xmach * 20.06 * sqrt(Ta)
      ENDIF ELSE tas[i] = 0
   ENDFOR

   volts = hk[*,0:2]*20.0/4095   ;Conversion end/mid diode voltages, see manual
   diodes = [1, 64, 32]        ;Diode IDs compatible with spec_hk and soda2_browse.pro
   dsp_temp = ((hk[*,7]*10.0/4095 - 1.49)/5e-3)+25
   ambient_temp = (hk[*,13]-1024)*.048828125
   Rt = ((4095/hk[*,10:12]) - 1) * 15
   optional_temps = 1.0/((1.114e-3 + 236.43e-6 * alog(Rt) + 74.03e-9 * (alog(Rt))^3 )) - 273.3
   tempid = ['DSP', 'Ambient', 'Option1', 'Option2', 'Option3']
   alltemps = [[dsp_temp], [ambient_temp], [optional_temps]]


   data = {spec1d:spec, midbins:midbins, time:time, endbins:endbins,$
           binwidth:binwidth, tas_pitot:tas, pres:68.9476*15*hk[*,4]/4095, $
           diodes:diodes, volts:volts, $
           tempid:tempid, temp:alltemps, laser_current:hk[*,14]*0.19536, $
           laser_power:hk[*,15]*0.407, dofreject:dofreject, endreject:endreject,$
           oversizereject:oversizereject, particlecounter:particlecounter, op:op}

   IF nowrite eq 0 THEN save, file=soda2_filename(op, 'CIP1D'), data

END
