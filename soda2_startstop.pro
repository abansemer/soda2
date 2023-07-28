FUNCTION soda2_startstop, fn
   ;FUNCTION to quickly return the date, start, and stop time of a raw SPEC or DMT file.
   ;Output in Julian date format.  Use caldat procedure to convert.
   ;AB 2019

   out = {starttime:0d, stoptime:0d, format:'', size:0d, err:0}

   ;Define bad file structure and test for existence
   badfile = out
   badfile.err = 1
   IF file_test(fn) eq 0 THEN return, badfile

   ;Get file stats
   openr, lun, fn, /get_lun
   f=fstat(lun)
   out.size=f.size
   IF f.size lt 5000 THEN return, badfile


   ;First check if this is an SEA file, need to use a different method
   ;==================================================================
   buf = readdatadir(lun)  ;SEA file should start with a time dir
   IF (buf.tagnumber eq 0) THEN BEGIN
      header = {year:0us, month:0us, day:0us, hour:0us, minute:0us, second:0us}
      point_lun, lun, buf.dataoffset
      readu, lun, header
      IF (header.year ge 1995) and (header.year lt 2050) and (header.month le 12 )THEN BEGIN
         out.format = 'SEA'
         out.starttime = julday(header.month, header.day, header.year, header.hour, header.minute, header.second)

         ;Read in the last 100kb of the file and search for a reasonable date
         blocksize = f.size < 100000   ;100kb or overall file size
         block = intarr(blocksize/2)
         point_lun, lun, f.size-blocksize
         readu, lun, block
         w = where(block eq header.year and block[1:*] ge header.month)  ;Find same year and month
         lastpoint = f.size - blocksize + w[-1]*2
         point_lun, lun, lastpoint
         readu, lun, header
         out.stoptime = julday(header.month, header.day, header.year, header.hour, header.minute, header.second+1)
         free_lun, lun
         return, out
      ENDIF
   ENDIF

   ;Next check for TXT format (simulations)
   ;==================================================================
   point_lun, lun, 0
   x = bytarr(200)
   readu, lun, x
   IF strpos(string(x), 'Resolution') ne -1 THEN BEGIN
      out.format = 'TXT'
      out.starttime = julday(1, 1, 2000, 0, 0, 0)  ;Always Jan 1 2000

      s=''
      point_lun, lun, 0
      readf, lun, s  ;Read header line
      v = str_sep(strcompress(s),' ')
      w = where(strpos(v,'Duration') ne -1, nw)
      IF nw THEN out.stoptime = julday(1, 1, 2000, 0, 0, float(v[w+1])+1)
      free_lun, lun
      return, out
   ENDIF

   ;Next check for HVPS1995 format (T28)
   ;==================================================================
   point_lun, lun, 0
   header = {type:bytarr(2), sec1900:0UL, millisec:0s, timezone:0s, dstflag:0s}
   readu, lun, header
   IF (header.sec1900 gt 3000000000) and (header.sec1900 lt 3160000000) THEN BEGIN  ;roughly 1995-2000
      out.format = 'HVPS1995'
      ;starttime = header.sec1900 / 86400d + JulDay(1,1,1900,0,0,0)
      ;out.starttime = starttime

      ;Index the entire file to get the times, since need to take care of time zones, etc.
      op = {format:'HVPS1995', timeoffset:0}
      pop = ptr_new(op)
      index = soda2_buildindex(fn, pop)
      ptr_free, pop
      caldat, index.date[0] + index.bufftime[0]/86400d + 0.5, month, day, year, hour, minute, second
      out.starttime =  index.date[0] + index.bufftime[0]/86400d - 0.5
      out.stoptime = index.date[-1] + index.bufftime[-1]/86400d - 0.5

      elaptime = index.bufftime[-1] - index.bufftime[0]
      IF elaptime lt 0 THEN stop ;need to test:  elaptime += 86400 ;Midnight crossing

      free_lun, lun
      return, out
   ENDIF

   ;Next check for DMT, SPEC, or NCAR
   ;==================================================================
   ;Find the buffer size from the position of 'year' data, should typically be 4114 bytes
   point_lun, lun, 0
   x = intarr(10000<(f.size/2))
   readu, lun, x
   free_lun, lun  ;Close here to avoid returning with an open file handle
   IF (x[0] lt 2000) or (x[0] gt 2050) THEN return, badfile  ;Year is first int, probably not a SPEC file
   w = where((x eq x[0]) and (x[1:*] eq x[1]) and (x[2:*] eq x[2]), nw)   ;Year/month/day equals first year/month/day
   IF nw lt 2 THEN return, badfile                           ;No repeating buffers in first 10k ints
   buffsize = w[1]*2ULL
   numbuffs = f.size/buffsize

   ;Buffsize indicates format
   CASE buffsize OF
      4114: BEGIN
         out.format = 'SPEC'
         header={year:0s, month:0s, weekday:0s, day:0s, hour:0s, minute:0s, second:0s, millisecond:0s}
      END
      4112: BEGIN   ;PADS - Subformat=1
         out.format = 'DMT'
         header={year:0S, month:0S, day:0S, hour:0S, minute:0S, second:0S, millisecond:0S, weekday:0S}
      END
      4124: BEGIN   ;PACS - Subformat=0
         out.format = 'DMT'
         header={year:0S, month:0S, day:0S, hour:0S, minute:0S, second:0S, millisecond:0S, weekday:0S}
      END
      4116: BEGIN
        ;Placeholder for RAF, not ready yet, need offset for XML header and also an endian swap
        header={probetype:0b, probenumber:0b, hours:0s, minutes:0s, seconds:0s, year:0s, month:0s, day:0s}
      END
      ELSE: return, badfile
   ENDCASE

   ;Read first and last headers in the file
   openr, lun, fn, /get_lun
   point_lun, lun, 0
   readu, lun, header
   out.starttime = julday(header.month, header.day, header.year, header.hour, header.minute, header.second)
   point_lun, lun, (numbuffs-1)*buffsize
   readu, lun, header
   out.stoptime = julday(header.month, header.day, header.year, header.hour, header.minute, header.second+1)
   free_lun, lun

   return, out
END
