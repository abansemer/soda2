PRO pixel_noise_filter, bitimage
   ;This is a filter developed by Armin Afchine at FZ Juelich to eliminate single-pixel noise in the image.
   ;It checks for single pixels that have no neighbors on either side. 
   ;July 2018

   ;Eliminates vertical stripes
   bitsize = size(bitimage, /DIMENSIONS)
   A = make_array([bitsize[0]+2, bitsize[1]])
   A[1:-2, *] = (bitimage gt 0) < 1
   ind = where((((A[0:-2,*]-A[1:-1,*])[0:-2,*] lt 0) and ((A[1:-1,*]-A[0:-2,*])[1:-1,*] lt 0)) eq 1)
   if min(ind) ge 0 then bitimage[ind] = byte(0)

   ;eliminates horizontal stripes
   bitsize = size(bitimage, /DIMENSIONS)
   A = make_array([bitsize[0], bitsize[1]+2])
   A[*,1:-2] = (bitimage gt 0) < 1
   ind = where((((A[*,0:-2]-A[*,1:-1])[*,0:-2] lt 0) and ((A[*,1:-1]-A[*,0:-2])[*,1:-1] lt 0)) eq 1)
   if min(ind) ge 0 then bitimage[ind] = byte(0)         
END