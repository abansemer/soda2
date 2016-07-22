FUNCTION bin2dec,inp
   ;FUNCTION to convert decimal to binary.
   ;Also works for 2d arrays, individual numbers are in rows.
   ;Code copied from http://orpheus.nascom.nasa.gov/cds/#SOFTWARE
   ;Originally by Pike, modified 10-31-2000 by Aaron Bansemer for 2d image use
   ;Using unsigned long int for 2d processing
   
   ;initialize output and find size of input
   s = size(inp,/dim)
   if n_elements(s) eq 1 then n=1 else n=s[1]
   if s[0] gt 32 then print, 'possible overflow in bin2dec'
   out = ulonarr(n)   

   ;  switch array around for convenience
   x = reverse(byte(inp),1)

   ;  calculate integer
   for i=0,s[0]-1 do out = out + ulong(x(i,*))*2UL^i

   return,out
END