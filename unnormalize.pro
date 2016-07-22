FUNCTION unnormalize, spectra, endbins
   ;FUNCTION to un-normalize a size spectrum or an array of spectra (time x size).
   ;spectra should be in m^-4, endbins in um.
   ;Returns new spectra in m^-3
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   spec_un=spectra*0.0
   s=size(spectra)
   n=n_elements(endbins)
   binwidth=endbins[1:n-1]-endbins[0:n-2]
 
   IF s[0] eq 1 THEN spec_un = spectra*binwidth/1.0e6 $
     ELSE FOR i=0L,s[1]-1 DO spec_un[i,*] = spectra[i,*]*binwidth/1.0e6
   
   return,spec_un
END
