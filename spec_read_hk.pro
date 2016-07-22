FUNCTION spec_read_hk, lun, bpoint, buffsize
   ;FUNCTION to read in an HVPS3/2DS housekeeping frame
   ;Send lun, pointer to buffer start
   ;Will just extract diode voltages and TAS.  See manual for more available data.
   ;AB 5/2011
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   
   f1=0.00244140625  ;Conversion factor for raw units
   
   point_lun,lun,bpoint
   x=spec_readint(lun,53,buffsize=buffsize)  ;Get size of frame
   
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
   ;It appears that word 2 is always 0, ignore it (just higher precision stuff anyway)
   exponent=ishft(x[49] and '3f80'x,-7)+1  ;Only works for positive exponents
   mantissa=1+total(dec2bin8(x[49] and '7f'x)/2^findgen(8))
   tas=mantissa*2^exponent
   IF x[50] gt 0 then print,'found higher precision tas'
   ;print,exponent,mantissa,tas
   
   time=ishft(ulong(x[51]),16)+x[52] ;Assemble timeword
   
   return,{time:time, tas:tas, x:y}
END 
   
   
   
   
   