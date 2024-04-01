FUNCTION stairsteps, data, endbins
   ;Convert a PSD, MSD, or other distribution into an array for plotting in stair-step style.
   ;Returns a structure with new x and y points for plotting
   ;AB 4/2024

   ;Initialize data and set up even/odd indexing
   numbins = n_elements(endbins)-1
   out = {x:fltarr(numbins*2), y:fltarr(numbins*2)}
   ieven = indgen(numbins)*2
   iodd = ieven+1

   ;Fill arrays
   out.x[ieven] = endbins[0:numbins-1]
   out.x[iodd] = endbins[1:numbins]
   out.y[ieven] = data
   out.y[iodd] = data

   return, out
END
