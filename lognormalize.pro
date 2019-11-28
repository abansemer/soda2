FUNCTION lognormalize, spectra, endbins
   ;FUNCTION to log-normalize a size spectrum or an array of spectra (time x size).
   ;spectra should be in m^-4, endbins in um.
   ;Returns new spectra in dN/dlogD (meters)
   
   spec_log=spectra*0.0
   s=size(spectra)
   n=n_elements(endbins)
   binwidth=endbins[1:n-1]-endbins[0:n-2]
   logbinwidth=alog10(endbins[1:n-1])-alog10(endbins[0:n-2])
 
   IF s[0] eq 1 THEN spec_log = spectra*binwidth/1.0e6/logbinwidth $
     ELSE FOR i=0L,s[1]-1 DO spec_log[i,*] = spectra[i,*]*binwidth/1.0e6/logbinwidth
   
   return,spec_log
END
