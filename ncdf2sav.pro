PRO ncdf2sav, ncfile, data, varlist=varlist, lite=lite, nosave=nosave, compress=compress
   ;PRO to convert an ncdf file to IDL sav format.
   ;If only a few variables are desired, enter the tags into 'varlist'
   ;   Varlist is case sensitive.
   ;AB 11/2009
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   IF n_elements(lite) eq 0 THEN lite=0 
   IF n_elements(compress) eq 0 THEN compress=0 
   IF n_elements(nosave) eq 0 THEN nosave=0 
   IF n_elements(varlist) eq 0 THEN BEGIN
      extractall=1 
      varlist=[' ']
   ENDIF ELSE BEGIN
      extractall=0
      varlist=strupcase(varlist)
   ENDELSE
   IF file_test(ncfile) THEN id=ncdf_open(ncfile) ELSE return
   
   global={ncdf_filename:ncfile}
   datainfo=ncdf_inquire(id)
   
   ;Netcdf allows more characters in variable names.  List of characters
   ;to be replaced.  Duplicate names (due to up/low case) will be skipped.
   badchar='[. #,:-]'
   ;Reserved names can't be used as structure tags, add new ones here as needed.
   reservednames=['and','begin','break','case','common','continue','do','else','end',$
      'endcase','endelse','endfor','endif','endrep','endswitch','endwhile','eq,','for',$
      'foreach','function','ge','goto','gt','if','inherits','le','lt','mod','ne','not',$
      'of','or','pro','repeat','switch','then','until','while','xor']
   
   ;Create a structure for global attributes
   FOR i=0,datainfo.ngatts-1 DO BEGIN
      attname=ncdf_attname(id,i,/global)
      attinfo=ncdf_attinq(id,attname,/global)
      ncdf_attget,id,attname,att,/global  
      IF attinfo.datatype eq 'CHAR' then att=string(att)
      
      ;Replace bad characters with underscore
      newattname=strjoin(strsplit(attname,badchar,/ext,/regex), '_')  
      ;Can't use IDL reserved names as structure tags, append underscore
      IF total(newattname eq reservednames) gt 0 THEN newattname=newattname+'_'
      firstletter_ascii=byte(strmid(newattname,0,1))  ;Make sure it doesn't start with a number
      IF (firstletter_ascii ge 48) and (firstletter_ascii le 57) THEN newattname='x'+newattname

      IF i eq 0 THEN global=create_struct(newattname, att) $
         ELSE IF (total(tag_names(global) eq strupcase(newattname)) eq 0) THEN $
           global=create_struct(global, newattname, att)
   ENDFOR
           
   ;Add in all the data vectors
   data={global:global}
   attributes={long_name:'Attribute listing for all variables'}
   FOR i=0,datainfo.nvars-1 DO BEGIN
      varinfo=ncdf_varinq(id,i)
      IF ((total(varlist eq strupcase(varinfo.name)) gt 0) or (extractall eq 1)) THEN BEGIN
         ncdf_varget,id,i,vardata
         varname=strjoin(strsplit(varinfo.name,badchar,/ext,/regex), '_')  
         IF total(varname eq reservednames) gt 0 THEN varname=varname+'_'
         firstletter_ascii=byte(strmid(varname,0,1))  ;Make sure it doesn't start with a number
         IF (firstletter_ascii ge 48) and (firstletter_ascii le 57) THEN varname='x'+varname
         
         ;Check for duplicates (usually due to upper/lowercase), get data
         IF total(tag_names(data) eq strupcase(varname)) eq 0 THEN BEGIN
            ;Write attributes
            FOR j=0,varinfo.natts-1 DO BEGIN
               attname=ncdf_attname(id,i,j)
               attinfo=ncdf_attinq(id,varinfo.name,attname)
               ncdf_attget,id,varinfo.name,attname,att  
               IF attinfo.datatype eq 'CHAR' then att=string(att)
            
               newattname=strjoin(strsplit(attname,badchar,/ext,/regex), '_')
               IF total(newattname eq reservednames) gt 0 THEN newattname=newattname+'_'
               firstletter_ascii=byte(strmid(newattname,0,1))  ;Make sure it doesn't start with a number
               IF (firstletter_ascii ge 48) and (firstletter_ascii le 57) THEN newattname='x'+newattname
               IF j eq 0 THEN attstruct=create_struct(newattname,att) $
                  ELSE attstruct=create_struct(attstruct,newattname,att)
            ENDFOR
                        
            ;Write data to main structure, only do 1-dimensional variables if flagged
            IF (lite eq 0) or ((lite eq 1) and ((size(vardata))[0] eq 1)) THEN BEGIN
               attributes=create_struct(attributes, varname, attstruct)
               data=create_struct(data, varname, vardata) 
            ENDIF 
         ENDIF ELSE print, 'Skipped duplicate variable: ',varinfo.name
      ENDIF
   ENDFOR
  
   ;Merge attributes into main structure
   data=create_struct('attributes', attributes, data)

   ;Save data, make sure file is writable first
   IF nosave eq 0 THEN BEGIN
      filename=ncfile+'.sav'
      openw,1,filename,error=errorcheck
      close,1
      WHILE (errorcheck ne 0) DO BEGIN
         newpath=dialog_pickfile(/read,/directory,get_path=a2,title='Cannot write to current directory, select a new one')
         IF newpath eq '' THEN return
         filename=newpath+file_basename(ncfile)+'.sav'
         openw,1,filename,error=errorcheck
         close,1
      ENDWHILE 
      
      save,data,file=filename,compress=compress
      print,'Saved file '+filename
   ENDIF
  
END
           
