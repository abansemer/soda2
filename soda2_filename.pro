FUNCTION soda2_filename, op, suffix, extension=extension, outdir=outdir
   ;Generates filenames with the given suffix (usually based on probe name)
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(outdir) eq 0 THEN outdir = op.outdir        ;Output directory
   IF n_elements(extension) eq 0 THEN extension = '.dat'     ;Filetype extension

   ;User defined filetag (e.g. 'xsize'), add underscore if one exists
   IF total(tag_names(op) eq 'FILETAG') gt 0 THEN BEGIN
      IF strlen(op.filetag) eq 0 THEN filetag = '' ELSE filetag = '_'+op.filetag
   ENDIF ELSE filetag = ''

   ;Check for probes with an extra ID, such as 'H' or 'V' and add to the suffix
   IF total(tag_names(op) eq 'PROBETYPE') gt 0 THEN BEGIN
      IF (op.probetype eq '2DS') or (op.probetype eq '3VCPI') or (op.probetype eq 'HVPS4') THEN $
         suffix = suffix + '_' + op.probeid
   ENDIF

   ;Generate filename
   fn_out = outdir + strtrim(string(op.date),2) + '_' + strtrim(string(long(sfm2hms(op.starttime)), form='(i06)'),2) $
      + '_' + suffix + filetag + extension

   return,fn_out
END
