FUNCTION dec2bin64,in
   ;FUNCTION to convert binary to decimal, also works for arrays.
   ;Code copied from http://orpheus.nascom.nasa.gov/cds/#SOFTWARE
   ;by Pike ('93) and Thompson ('94)   
   ;Using unsigned long int for 2d processing
   
   out=bytarr(64,n_elements(in))
   for i=0,63 do out(63-i,*)=(in and 2ULL^i)/2ULL^i
   return,out
END
