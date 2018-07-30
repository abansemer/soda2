FUNCTION soda2_samplearea, diam, res, armwidth, numdiodes, reconstruct, smethod, wavelength, forcespec=forcespec, centerin=centerin, dof=dof, eff_wid=eff_wid
   ;FUNCTION to return the sample area for a variety of probes.
   ;The sample area unit should be meter^2 for all probes.
   ;Allin/reconstruct option, for computing sa when particle must be all-in.
   ;Input units:  diam[um], res[um], armwidth[cm], wavelength[m]
   ;Aaron Bansemer, NCAR, 2010
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(forcespec) eq 0 THEN forcespec=0
   IF n_elements(centerin) eq 0 THEN centerin=0
   
   diam=float(diam)  ;avoid errors if wrong data type is sent in
   res=float(res)
   
   ;DoF
   c=3.0  ;Knollenberg 1970
   
   ;From Lawson 2006, this is for SPEC probes
   IF (wavelength eq 0.785e-6) or (forcespec eq 1) THEN c=8.12 

   r2overlam=(diam/1e6/2.0)^2 / wavelength   ;m
   dof=2*c*r2overlam * 1.0e6  ;*2 is for the +/-.  Convert from m to microns. 
     
   prht=armwidth * 1.0e4  ;convert cm to microns
   dof=dof < prht  ;Limit dof to physical distance between arms. 
   
   ;Effective width.  Need to reconsider reconstruction for circle sizing, but OK based on sims (offsets error in c?).
   eff_wid=res*numdiodes+0.72*diam  ;Reconstruction, from eq 17 in Heymsfield & Parrish 1978. 
   IF centerin eq 1 THEN eff_wid=res*numdiodes    ;Assume center-in for this sizing
   IF reconstruct eq 0 THEN eff_wid=res*(numdiodes-1)-diam   ;All-in, from eq 6 in HP78
   
   sa=(dof*eff_wid * 1e-12) > 0  ;compute sa and convert to m^2, don't allow negative SA 
   return,sa
END
   
