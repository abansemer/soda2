FUNCTION soda2_restore_pbp, fn, offset=offset, count=count, op=op
   ;Read in a pbp.nc file and put elements in a structure
   ;compatible for SODA-2 reprocessing.
   ;offset and count are passed into restorenc.pro for
   ;only reading certain parts of the file.
   
   restorenc, fn, 'x', offset=offset, count=count
   op = x.global  ;For returning to user

   ;Convert from structure of arrays to array of structures
   n =  n_elements(x.time)
   tags = tag_names(x)
   c = 0
   itag = intarr(n_elements(tags))
   
   ;Construct the base structure
   FOR i = 0L, n_elements(tags)-1 DO BEGIN
      IF n_elements(x.(i)) eq n THEN BEGIN  ;This skips 'global' and 'attributes' tags
         IF c eq 0 THEN base = create_struct(tags[i], (x.(i))[0]) ELSE $
             base = create_struct(base, tags[i], (x.(i))[0])
         itag[c] = i
         c++
      ENDIF
   ENDFOR
   
   ;Replicate and stuff with data
   data = replicate(base, n)
   FOR i = 0L, c-1 DO data.(i) = x.(itag[i])

   return, data
END