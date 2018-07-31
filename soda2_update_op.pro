PRO soda2_update_op, op
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   ;Updates the 'op' structure if any new tags are added
   IF total(where(tag_names(op) eq 'GREYTHRESH')) eq -1 THEN op=create_struct(op,'greythresh',0)
   IF total(where(tag_names(op) eq 'PROBEID')) eq -1 THEN op=create_struct(op,'probeid','A0')
   IF total(where(tag_names(op) eq 'TIMEOFFSET')) eq -1 THEN op=create_struct(op,'timeoffset',0.0)
   IF total(where(tag_names(op) eq 'PTH')) eq -1 THEN op=create_struct(op,'pth','')
   IF total(where(tag_names(op) eq 'WATER')) eq -1 THEN op=create_struct(op,'water',0)
   IF total(where(tag_names(op) eq 'SUBFORMAT')) eq -1 THEN op=create_struct(op,'subformat',0)
   IF total(where(tag_names(op) eq 'ARMWIDTH')) eq -1 THEN op=create_struct(op,'armwidth',0.0)
   IF total(where(tag_names(op) eq 'NUMDIODES')) eq -1 THEN op=create_struct(op,'numdiodes',64)
   IF total(where(tag_names(op) eq 'PARTICLEFILE')) eq -1 THEN op=create_struct(op,'particlefile',0)
   IF total(where(tag_names(op) eq 'NCDFPARTICLEFILE')) eq -1 THEN op=create_struct(op,'ncdfparticlefile',0)
   IF total(where(tag_names(op) eq 'CLUSTERTHRESH')) eq -1 THEN op=create_struct(op,'clusterthresh',0.0)   
   IF total(where(tag_names(op) eq 'SAVFILE')) eq -1 THEN op=create_struct(op,'savfile',1)  
   IF total(where(tag_names(op) eq 'SHORTNAME')) eq -1 THEN op=create_struct(op,'shortname',op.probetype)  
   IF total(where(tag_names(op) eq 'WAVELENGTH')) eq -1 THEN op=create_struct(op,'wavelength',0.658e-6)  
   IF total(where(tag_names(op) eq 'KEEPLARGEST')) eq -1 THEN op=create_struct(op,'keeplargest',0)
   IF total(where(tag_names(op) eq 'JUELICHFILTER')) eq -1 THEN op=create_struct(op,'juelichfilter',0)
   IF total(where(tag_names(op) eq 'CENTERIN')) eq -1 THEN op=create_struct(op,'centerin',1)
   IF total(where(tag_names(op) eq 'CENTERINREJECTION')) eq -1 THEN op=create_struct(op,'centerinrejection',1)
   IF total(where(tag_names(op) eq 'SEATAG')) eq -1 THEN op=create_struct(op,'seatag',[33000,0,0])
   IF total(where(tag_names(op) eq 'YRES')) eq -1 THEN op=create_struct(op,'yres',op.res)  ;A constant y-res, may change to variable
   IF total(where(tag_names(op) eq 'IGNOREDEADTIME')) eq -1 THEN op=create_struct(op,'ignoredeadtime',0)  ;for SPEC probes, sometimes overload is suspicious
END
  