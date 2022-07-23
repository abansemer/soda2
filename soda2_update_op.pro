PRO soda2_update_op, op
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   ;Updates the 'op' structure if any new tags are added
   IF total(where(tag_names(op) eq 'GREYTHRESH')) eq -1 THEN op=create_struct(op,'greythresh',2)
   IF total(where(tag_names(op) eq 'PROBEID')) eq -1 THEN op=create_struct(op,'probeid','A0')
   IF total(where(tag_names(op) eq 'TIMEOFFSET')) eq -1 THEN op=create_struct(op,'timeoffset',0.0)
   IF total(where(tag_names(op) eq 'PTH')) eq -1 THEN op=create_struct(op,'pth','')
   IF total(where(tag_names(op) eq 'WATER')) eq -1 THEN op=create_struct(op,'water',0)
   IF total(where(tag_names(op) eq 'SUBFORMAT')) eq -1 THEN op=create_struct(op,'subformat',0)
   IF total(where(tag_names(op) eq 'ARMWIDTH')) eq -1 THEN op=create_struct(op,'armwidth',0.0)
   IF total(where(tag_names(op) eq 'NUMDIODES')) eq -1 THEN op=create_struct(op,'numdiodes',64)
   IF total(where(tag_names(op) eq 'PARTICLEFILE')) eq -1 THEN op=create_struct(op,'particlefile',0)
   IF total(where(tag_names(op) eq 'NCDFPARTICLEFILE')) eq -1 THEN op=create_struct(op,'ncdfparticlefile',0)
   IF total(where(tag_names(op) eq 'ASCIIPSDFILE')) eq -1 THEN op=create_struct(op,'asciipsdfile',0)
   IF total(where(tag_names(op) eq 'CLUSTERTHRESH')) eq -1 THEN op=create_struct(op,'clusterthresh',0.0)
   IF total(where(tag_names(op) eq 'STUCKBITS')) eq -1 THEN op=create_struct(op,'stuckbits',0)
   IF total(where(tag_names(op) eq 'SAVFILE')) eq -1 THEN op=create_struct(op,'savfile',1)
   IF total(where(tag_names(op) eq 'SHORTNAME')) eq -1 THEN op=create_struct(op,'shortname',op.probetype)
   IF total(where(tag_names(op) eq 'RAKEFIX')) eq -1 THEN op=create_struct(op,'rakefix',0)  ;1 fixes odd diodes, 2 fixes even ones
   IF total(where(tag_names(op) eq 'WAVELENGTH')) eq -1 THEN op=create_struct(op,'wavelength',0.658e-6)
   IF total(where(tag_names(op) eq 'KEEPLARGEST')) eq -1 THEN op=create_struct(op,'keeplargest',0)
   IF total(where(tag_names(op) eq 'JUELICHFILTER')) eq -1 THEN op=create_struct(op,'juelichfilter',0)
   IF total(where(tag_names(op) eq 'EAWMETHOD')) eq -1 THEN op=create_struct(op,'eawmethod','centerin')
   IF total(where(tag_names(op) eq 'SEATAG')) eq -1 THEN op=create_struct(op,'seatag',[33000,0,0])  ;[Image, 1D_data, empty] for CIP
   IF total(where(tag_names(op) eq 'YRES')) eq -1 THEN op=create_struct(op,'yres',op.res)  ;Default to x, can change with stretchcorrect
   IF total(where(tag_names(op) eq 'IGNOREDEADTIME')) eq -1 THEN op=create_struct(op,'ignoredeadtime',0)  ;for SPEC probes, sometimes overload is suspicious
   IF total(where(tag_names(op) eq 'APPLY_PSC')) eq -1 THEN op=create_struct(op,'apply_psc',0)  ;for applying the Poisson spot correction during reprocessing from PBP files
   IF total(where(tag_names(op) eq 'DOFCONST')) eq -1 THEN op=create_struct(op,'dofconst',0)  ;Depth of field constant, setting to 0 will trigger decision in soda2_samplearea
   IF total(where(tag_names(op) eq 'DOFREJECT')) eq -1 THEN op=create_struct(op,'dofreject',0)  ;Reject based on DoF flag (DMT or NCAR), or Level 3 grey pixel (CIP-G)
   IF total(where(tag_names(op) eq 'STRETCHCORRECT')) eq -1 THEN op=create_struct(op,'stretchcorrect',0)  ;Adjust yres when aircraft TAS and probe TAS mismatch
   IF total(where(tag_names(op) eq 'STRICTCOUNTER')) eq -1 THEN op=create_struct(op,'strictcounter',0)  ;Apply particle-counter rejection for DMT probes (which are often noisy)
   IF total(where(tag_names(op) eq 'ACTIVETIMEFROMMISSED')) eq -1 THEN op=create_struct(op,'activetimefrommissed',0)  ;Compute active/dead time based on missed particle counts (DMT)

   ;Check for incompatible options
   IF (op.format eq 'SPEC') and (op.stuckbits eq 1) THEN BEGIN
      print, 'op.stuckbits will not work for SPEC probe data.'
      print, 'Setting op.stuckbits to 0.'
      op.stuckbits = 0
   ENDIF
   IF (op.stretchcorrect eq 1) and (op.format ne 'SPEC') THEN print, 'Stretch correction not available for this format, will not be applied.'
   IF (op.probetype eq '2DS') and (op.yres ne 10.0) THEN print, 'Y-resolution on 2DS is not 10um, timing may be inaccurate if y-res not applied during data acquisition.'

   ;For model TXT data, read in parameters from header and override
   IF strmid(op.fn[0],3,4,/reverse) eq '.txt' THEN BEGIN
      s=''
      openr,lun,op.fn[0],/get_lun
      readf,lun,s  ;Read header
      v=str_sep(strcompress(s),' ')
      op.res=float(v[1])
      op.yres=op.res
      op.numdiodes=float(v[3])
      op.armwidth=float(v[5])
      op.format='TXT'
      op.probetype='TXT'
      tas=float(v[7])
      close,lun
      print,'Overriding probe geometry with header info.'
   ENDIF
END
