FUNCTION compute_vt, diameter, mass, ar, temp, pres
   ;Compute fall velocity of a single particle given size and environmental parameters.
   ;Diameter in microns, mass in grams, temp in C, pressure in mb.
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.   
   
   TK = temp + 273.15
   rho = 0.348e-3 * pres/TK                 ;air density g/cm3
   anu = 0.043 / pres * (TK^2.5)/(TK+120.0) ;kinematic viscosity cm2/s
                  
   ;From Heymsfield and Westbrook JAS 2010
   delta0=8.0
   C0=0.35
   ;FYI eta=rho*anu
   X= (8*mass*980.0) / (!pi*(ar^0.5)*rho*anu^2)  ;See page 2478 (unnumbered equations)
   term1= 1 + (4*X^0.5)/(delta0^2*C0^0.5)
   Re= delta0^2/4.0 * (term1^0.5 - 1)^2
   vt=Re*anu/(diameter/1.0e4)
   
   ;For water, using Szyrmer and Zawadski
   vtwater=1690.0*(diameter/1.0e4)^0.6*(1000.0/pres)^0.5

   return,{vt:vt, vtwater:vtwater}
END
