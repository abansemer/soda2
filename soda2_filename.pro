FUNCTION soda2_filename, op, suffix, extension=extension, outdir=outdir
   ;Generates filenames with the given suffix
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(outdir) eq 0 THEN outdir = op.outdir
   IF n_elements(extension) eq 0 THEN extension = '.dat'
   fn_out = outdir+strtrim(string(op.date),2)+'_'+strtrim(string(long(sfm2hms(op.starttime)),form='(i06)'),2)+'_'+suffix+extension
   IF total(tag_names(op) eq 'PROBETYPE') gt 0 THEN $
      IF (op.probetype eq '2DS') or (op.probetype eq '3VCPI') or (op.probetype eq 'HVPS4') THEN $
         fn_out = outdir+strtrim(string(op.date),2)+'_'+strtrim(string(long(sfm2hms(op.starttime)),form='(i06)'),2)+'_'+suffix+'_'+op.probeid+extension
   return,fn_out
END
