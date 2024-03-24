FUNCTION sea_tagpositions, lun, tag
   ;Returns the position of all the tags of a certain type
   ;in an SEA file.
   ;Aaron Bansemer, NCAR, 8-2011

   IF n_elements(tag) eq 0 THEN tag=0   ;Default is the time tag

   ;Variable initialization
   startpoint = 0L
   point_lun, lun, startpoint  ;Reset file to beginning
   datapointer = lonarr(10000000)
   timepointer = lonarr(10000000)
   c = 0L

   ;Read through file
   REPEAT BEGIN
      d = readdatadir(lun)

      ;Find time tags
      IF d.tagnumber eq 0 THEN tpoint = startpoint+d.dataoffset

      ;Found the desired tag, save it and its associated time tag
      IF d.tagnumber eq tag THEN BEGIN
         datapointer[c] = startpoint+d.dataoffset
         timepointer[c] = tpoint
         c++
      ENDIF

      ;Found a 'next' tag
      IF d.tagnumber eq 999 THEN BEGIN
         startpoint = startpoint+d.dataoffset+d.numberbytes
         point_lun, lun, startpoint
      ENDIF
   ENDREP until eof(lun)
   
   return, {datapointer:datapointer[0:c-1], timepointer:timepointer[0:c-1]}
END
