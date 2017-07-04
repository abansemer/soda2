PRO restorenc, fn, structname, _extra=e
   ;Mimics the IDL 'restore' function, but for netCDF files
   ;See ncdf2sav.pro for formatting info.
   ;Inputs:
   ;fn [string] = netCDF filename
   ;structname [string] = Variable name to restore the file into.  Default is 'data'
   ;Extras can include 'varlist' and 'lite'
   ;AB 2016
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   ncdf2sav,fn,data,/nosave, _extra=e
   IF n_elements(structname) eq 0 THEN structname='data'
   IF n_elements(data) gt 0 THEN (Scope_VarFetch(structname,level=scope_level()-1,/enter)) = data ELSE print,'No such file ',fn
END
