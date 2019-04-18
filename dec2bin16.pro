   


FUNCTION dec2bin16,in
   ;FUNCTION to convert binary to decimal, also works for arrays.
   ;Code copied from http://orpheus.nascom.nasa.gov/cds/#SOFTWARE
   ;by Pike ('93) and Thompson ('94)   
   ;Using unsigned long int for 2d processing
   ;This version just returns an eight bit array.  A. Bansemer, 10-2001
   
   out=bytarr(16,n_elements(in))
   for i=0,15 do out(15-i,*)=(in and 2L^i)/2L^i
   return,out
END