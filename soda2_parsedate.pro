FUNCTION soda2_parsedate, date
   ;Parse a date string and return the individual components
   ;Expected formats are mmddyyyy or yyyymmdd

   ;Check format
   IF (strmid(date, 0, 2) eq '19') or (strmid(date, 0, 2) eq '20') THEN BEGIN
      ;Year first format
      year = strmid(date, 0, 4)
      month = strmid(date, 4, 2)
      day = strmid(date, 6, 2)
      order = 'ymd'
   ENDIF ELSE BEGIN
      ;Year last format
      year = strmid(date, 4, 4)
      month = strmid(date, 0, 2)
      day = strmid(date, 2, 2)
      order = 'mdy'
   ENDELSE

   return, {year:year, month:month, day:day, yearnum:fix(year), monthnum:fix(month), daynum:fix(day), $
      order:order, julday:julday(month, day, year)}
END
