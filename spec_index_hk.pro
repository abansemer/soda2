FUNCTION spec_index_hk, fn
   ;FUNCTION to return the file pointers to housekeeping buffers in 2DSCPIHK (Hawkeye and 3VCPI) data.
   ;The base*2DSCPI files do not contain housekeeping data, and are saved separately in 2DSCPIHK files.
   ;Buffer sizes can be variable from one buffer to the next.  Both HK and Mask buffers may be present.
   ;AB 2022
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   lun=1
   close,lun
   openr,lun,fn
   f=fstat(lun)
   header={year:0s, month:0s, dummy:0s, day:0s, hour:0s, minute:0s, second:0s, millisecond:0s, buffertype:0us}
   headersize=n_tags(header,/data_length)
   ;Get initial start time
   point_lun,lun,0
   readu,lun,header
   firstheader=header

   ;Read through all data and record position of HK and Mask buffers
   i=0L
   hkc=0L
   maskc=0L
   numbuffs = 100000
   bufftime=dblarr(numbuffs)
   buffpoint=lindgen(numbuffs)
   date=lonarr(numbuffs)
   maskp=lon64arr(numbuffs)
   hkp=lon64arr(numbuffs)

   ;Search through file every short int (2 bytes)
   FOR ip=0,(f.size-headersize)/2 - 1 DO BEGIN
      point_lun,lun,ip*2
      readu,lun,header
      ;Allow increment of one day/month.  Out of luck if year increments.
      IF (header.year eq firstheader.year) and $
         ((header.month eq firstheader.month) or (header.month eq firstheader.month+1)) and $
         ((header.day eq firstheader.day) or (header.day eq firstheader.day+1)) THEN BEGIN

         ;Time and time pointers
         buffpoint[i]=ip*2
         bufftime[i]=header.hour*3600D + header.minute*60D + header.second + header.millisecond/1000D
         date[i]=julday(header.month,header.day,header.year)
         i++

         ;Housekeeping
         IF header.buffertype eq 18507 THEN BEGIN
            hkp[hkc]=ip*2 + headersize - 2
            hkc++
         END

         ;Mask
         IF header.buffertype eq 19787 THEN BEGIN
            maskp[maskc]=ip*2 + headersize - 2
            maskc++
         END
      ENDIF
   ENDFOR
   date=date[0:(i-1)>0]
   buffpoint=buffpoint[0:(i-1)>0]
   bufftime=bufftime[0:(i-1)>0]
   hkp=hkp[0:(hkc-1)>0]
   maskp=maskp[0:(maskc-1)>0]

   close,lun
   buffsize = 0 ;Set to zero to disable header skipping with spec_readint.pro.  Not appropriate for these files.
   return,{date:date, bufftime:bufftime, pointer:buffpoint, hkp:hkp, maskp:maskp, buffsize:buffsize, error:0}
END
