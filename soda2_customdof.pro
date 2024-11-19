PRO soda2_customdof_event, ev
   ;GUI for custom DoF routine
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   uname = widget_info(ev.id, /uname)
   widget_control, widget_info(ev.top, find='dofbase'), get_uvalue=pinfo

   CASE uname OF

      'saveloadfile': BEGIN ;===========================================================================
         IF ev.value eq 0 THEN BEGIN ;Load file
            a = dialog_pickfile(/read,path=defaultpath,filter='*.txt',title='File to Load',dialog_parent=ev.id)
            IF file_test(a[0]) THEN BEGIN
               openr, lun, a[0], /get_lun
               (*pinfo).newendbins[*] = 0; = dblarr(10000)
               (*pinfo).newdof[*] = 0; dblarr(10000)
               i = 0
               v = ''
               readf, lun, v
               ;Make sure file is a copy of
               IF strmid(v, 0, 14) ne 'BinLowEdge(um)' THEN BEGIN
                  dummy = dialog_message('First line header incorrect, check format.', dialog_parent=ev.id, /info)
                  return
               ENDIF
               REPEAT BEGIN
                  readf, lun, v
                  fields = strsplit(v, '[ ,' + STRING(9B) + ']+', /regex, /extract)
                  (*pinfo).newendbins[i] = fields[0]
                  (*pinfo).newendbins[i+1] = fields[1]
                  (*pinfo).newdof[i] = fields[2]*1e4  ;cm to microns
                  i++
               ENDREP UNTIL eof(lun)
               (*pinfo).newnumbins = i
               free_lun, lun
               plot_customdof, pinfo
            ENDIF
         ENDIF

         IF ev.value eq 1 THEN BEGIN  ;Save file
            a = dialog_pickfile(/read,file='soda2_custom_dof.txt',path=defaultpath,title='File to Save',dialog_parent=ev.id)
            IF strlen(a[0]) gt 0 THEN BEGIN
               openw, lun, a[0], /get_lun
               printf, lun, 'BinLowEdge(um)', 'BinHiEdge(um)', 'DoF(cm)', format='(3a-15)'
               numbins = n_elements((*pinfo).dof)
               FOR i = 0, numbins-1 DO printf, lun, (*pinfo).endbins(i), (*pinfo).endbins(i+1), (*pinfo).dof(i)/1e4, $
                  format='(3f-15.2)'
               free_lun, lun
               dummy = dialog_message('Saved: '+a[0], dialog_parent=ev.id, /info)
            ENDIF
         ENDIF
      END

      'process': BEGIN
         (*pinfo).proceedprocessing_flag = 1
         WIDGET_CONTROL, ev.TOP, /DESTROY
      END
      'cancel': WIDGET_CONTROL, ev.TOP, /DESTROY
      'quit': WIDGET_CONTROL, ev.TOP, /DESTROY

      ELSE: dummy=0
   ENDCASE

END

PRO plot_customdof, pinfo
   color1=70  ;Blue
   color2=220 ;Red
   wset,(*pinfo).plotid
   !p.background=255
   !p.color=0

   ;Make stairstep plot
   numbins = (*pinfo).newnumbins
   stairdof = stairsteps((*pinfo).newdof[0:numbins-1], (*pinfo).newendbins[0:numbins])
   plot, stairdof.x, stairdof.y/1e4, xtit='Diameter (um)', ytit='Depth of Field (cm)', title='Current DoF', psym=-6, $
      symsize=0.5, thick=1, yr=[0, (*pinfo).armwidth*1.1], /ysty
   oplot, stairdof.x, fltarr(numbins)+(*pinfo).armwidth, color=color2, line=2, thick=2
   oplot, stairdof.x, stairdof.y/1e4, color=color1, thick=1
   legend_old,['Depth of Field', 'Arm Width'], line=[0,2], box=0, color=[color1, color2], /bottom, /right, charsize=1.0, thick=2
END


PRO soda2_customdof, op, pinfo=pinfo, groupleaderid=groupleaderid
   IF !version.os_family eq 'windows' THEN widget_control,default_font='Helvetica*fixed*12'
   IF !version.os_family eq 'unix' THEN widget_control,default_font='-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1' ;use xlsfonts to see more
   device,decompose=0,get_screen_size=screen_size   ;Set to 8-bit color
   IF n_elements(op) eq 0 THEN op={endbins:findgen(25)*40, armwidth:7.0, res:25.0, numdiodes:64, eawmethod:'centerin',$
      wavelength:658e-9, dofconst:3.0, smethod:'centerin'}

   numbins = n_elements(op.endbins)-1
   midbins = (float(op.endbins[0:numbins-1])+op.endbins[1:numbins])/2.0
   dof = fltarr(numbins)
   dummy=soda2_samplearea(midbins, op.res, op.armwidth, op.numdiodes, op.eawmethod, $
      op.smethod, op.wavelength, op.dofconst, dof=dof)

   ;Make sure GUI will fit on the screen
   screen_x = (screen_size[0]-50)  <900
   screen_y = (screen_size[1]-300) <400
   IF n_elements(groupleaderid) gt 0 THEN BEGIN
      ;This 'blocks' the main SODA GUI so it waits on this routine to complete before continuing
      base = WIDGET_BASE(COLUMN=1, title='Custom Depth of Field', uname='dofbase', /modal, group_leader=groupleaderid)
   ENDIF ELSE BEGIN
      ;For running without SODA GUI, mainly testing
      base = WIDGET_BASE(COLUMN=1, title='Custom Depth of Field', uname='dofbase')
   END

   ;fileID = widget_button(menubarID, value='File', /menu)
   ;quitID = widget_button(fileID, value='Quit',uname='quit')

   subbase = widget_base(base,column=1,/frame)
   addfile = cw_bgroup(subbase,['Load...', 'Save Current...'], uname='saveloadfile',/row,label_left='DoF files:')
   plotid = widget_draw(subbase, xsize=screen_x, ysize=screen_y, uname='wplot')

   process = WIDGET_BUTTON(base, value='Continue Processing', uname='process')
   process = WIDGET_BUTTON(base, value='Cancel Processing', uname='cancel')

   ;Start widget
   WIDGET_CONTROL, base, /REALIZE
   ;XMANAGER, 'soda2_customdof', base, /no_block
   widget_control, widget_info(base, find='wplot'), get_value=plotid

   ;Create output structure, will contain separate variables with new bins/dof for use by soda2.pro
   ;Have to initialize here with large array since can't modify size after pointer created.
   newendbins = fltarr(10000)
   newdof = fltarr(10000)
   newendbins[0:numbins] = op.endbins
   newdof[0:numbins-1] = dof
   newnumbins = numbins
   info = {endbins:op.endbins, dof:dof, armwidth:op.armwidth, plotid:plotid, $
      newendbins:newendbins, newdof:newdof, newnumbins:newnumbins, proceedprocessing_flag:0}
   pinfo = ptr_new(info)

   ;Set up pointer pass-through and make initial DoF plot
   pinfo = ptr_new(info)
   widget_control, base, set_uvalue=pinfo
   plot_customdof, pinfo
   XMANAGER, 'soda2_customdof', base, /no_block
END
