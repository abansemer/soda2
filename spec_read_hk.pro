FUNCTION spec_read_hk, lun, bpoint, buffsize, version=version
   ;FUNCTION to read in an HVPS3/2DS housekeeping frame
   ;Send lun, pointer to buffer start
   ;Will just extract diode voltages and TAS.  See manual for more available data.
   ;AB 5/2011
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(version) eq 0 THEN version=1

   IF version eq 1 THEN hklength=53   ;2DS/HVPS3
   IF version eq 2 THEN hklength=83   ;3VCPI/Hawkeye, usually with separate base*HK file

   point_lun,lun,bpoint
   x=spec_readint(lun,hklength,buffsize=buffsize,/signed)  ;Get size of frame

   f1=0.00244140625  ;Conversion factor for raw units

   IF version eq 1 THEN BEGIN
      c0=fltarr(49)  ;Coefficients from the manual
      c0[9:21]=1.6
      c0[24]=-3.846

      c1=fltarr(49)+1.0
      c1[1:6]=f1
      c1[7:8]=f1*2
      c1[9:21]=f1*10
      c1[22:23]=f1*2
      c1[24]=0.018356
      c1[25:32]=f1
      c1[36:37]=f1/2

      y=x[0:48]*c1+c0  ;apply all the coefficients

      ;TAS in floating point binary: mantissa bits 0-22, exponent 23-30, sign 31
      exponent=ishft(x[49] and '3f80'x,-7)+1  ;Only works for positive exponents
      mantissabits=[dec2bin8(x[49] and '7f'x), dec2bin16(x[50] and 'ffff'x)]
      mantissa=1+total(mantissabits/2^findgen(24))
      tas=mantissa*2^exponent

      time=ishft(ulong(x[51]),16)+x[52] ;Assemble timeword
   ENDIF

   IF version eq 2 THEN BEGIN
      ;Only doing a select few items, lots of CPI and other stuff don't care about
      ;Not using anything here except TAS and time, now recomputed from raw in spec_process_hk.pro
      y=float(x)  ;Start with straight copy

      ;Temperatures
      rt=6.5536e9 * (1-x[2:28]/65536d) / (5*x[2:28])
      y[2:28] = (-273.15) + (1.111e-3 + 237e-6 * alog(rt) + 75.8e-9*(alog(rt))^3)^(-1)

      ;Pressure
      y[29] = x[29]*5.72e-4  - 3.75   ;PSI

      ;Volts
      y[34:53] = x[34:53]*f1

      ;TAS in floating point binary: mantissa bits 0-22, exponent 23-30, sign 31
      exponent=ishft(x[75] and '3f80'x,-7)+1  ;Only works for positive exponents
      mantissabits=[dec2bin8(x[75] and '7f'x), dec2bin16(x[76] and 'ffff'x)]
      mantissa=1+total(mantissabits/2^findgen(24))
      tas=mantissa*2^exponent

      time=ishft(ulong(x[72]),32) + ishft(ulong(x[73]),16) + x[74] ;Assemble timeword
   ENDIF

   return,{time:time, tas:tas, x:y, raw:x}
END
