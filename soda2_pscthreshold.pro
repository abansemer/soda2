PRO soda2_pscthreshold_event, ev
   ;Simple GUI for PSC threshold
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

   uname = widget_info(ev.id, /uname)
   widget_control, widget_info(ev.top, find='pscbase'), get_uvalue=pinfo

   CASE uname OF
      'process': BEGIN
         (*pinfo).proceedprocessing_flag = 1
         widget_control,widget_info(ev.top,find='threshold'),get_value=threshold
         (*pinfo).threshold = threshold
         WIDGET_CONTROL, ev.TOP, /DESTROY
      END
      'cancel': WIDGET_CONTROL, ev.TOP, /DESTROY
      'quit': WIDGET_CONTROL, ev.TOP, /DESTROY

      ELSE: dummy=0
   ENDCASE
END


PRO soda2_pscthreshold, defaultthresh, pinfo=pinfo, groupleaderid=groupleaderid
   IF !version.os_family eq 'windows' THEN widget_control,default_font='Helvetica*fixed*12'
   IF !version.os_family eq 'unix' THEN widget_control,default_font='-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1' ;use xlsfonts to see more
   device,decompose=0,get_screen_size=screen_size   ;Set to 8-bit color

   IF n_elements(groupleaderid) gt 0 THEN BEGIN
      ;This 'blocks' the main SODA GUI so it waits on this routine to complete before continuing
      base = WIDGET_BASE(COLUMN=1, title='Upper Size Limit', uname='pscbase', /modal, group_leader=groupleaderid)
   ENDIF ELSE BEGIN
      ;For running without SODA GUI, mainly testing
      base = WIDGET_BASE(COLUMN=1, title='Upper Size Limit', uname='pscbase')
   END

   subbase = widget_base(base,column=1,/frame)

   mainwindow=cw_field(base, /int, title='Optional PSC Size Limit (microns):',uname='threshold' , xsize=5, value=defaultthresh)
   process = WIDGET_BUTTON(base, value='Continue Processing', uname='process')
   process = WIDGET_BUTTON(base, value='Cancel Processing', uname='cancel')

   ;Start widget
   WIDGET_CONTROL, base, /REALIZE

   info = {threshold:0L, proceedprocessing_flag:0}
   pinfo = ptr_new(info)

   ;Set up pointer pass-through
   pinfo = ptr_new(info)
   widget_control, base, set_uvalue=pinfo
   XMANAGER, 'soda2_pscthreshold', base, /no_block
END
