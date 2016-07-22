FUNCTION dec2bin,in
   ;FUNCTION to convert binary to decimal, also works for arrays.
   ;Code copied from http://orpheus.nascom.nasa.gov/cds/#SOFTWARE
   ;by Pike ('93) and Thompson ('94)   
   ;Using unsigned long int for 2d processing
   
   out=bytarr(32,n_elements(in))
   for i=0,31 do out(31-i,*)=(in and 2UL^i)/2UL^i
   return,out
END