PRO soda2_properties,p1,pop,top
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

    
    base = WIDGET_BASE(COLUMN=1,title='File Properties',MBar=menubarID,group_leader=top)
    subbase1=widget_base(base,column=2,/frame)
    dummy=widget_label(subbase1,value='Flight date: '+(*pop).date,/align_left)
    dummy=widget_label(subbase1,value='Data start (s): '+strtrim(string((*pop).starttime),2),/align_left)
    dummy=widget_label(subbase1,value='Data stop (s): '+strtrim(string((*pop).stoptime),2),/align_left)
    dummy=widget_label(subbase1,value='Date processed: '+(*p1).date_processed,/align_left)
    dummy=widget_label(subbase1,value='Probe Type: '+strtrim(string((*pop).probetype),2),/align_left)
    dummy=widget_label(subbase1,value='Resolution: '+strtrim(string((*pop).res),2),/align_left)
    dummy=widget_label(subbase1,value='Sizing Method: '+strtrim(string((*pop).smethod),2),/align_left)
    dummy=widget_label(subbase1,value='Interarrival Rejection: '+strtrim(string((*pop).inttime_reject,form='(i3)'),2),/align_left)
    dummy=widget_label(subbase1,value='Reconstruction: '+strtrim(string((*pop).reconstruct,form='(i3)'),2),/align_left)
    dummy=widget_label(subbase1,value='Stuck Bit Detection: '+strtrim(string((*pop).stuckbits,form='(i3)'),2),/align_left)
    dummy=widget_label(subbase1,value='Water Processing: '+strtrim(string((*pop).water,form='(i3)'),2),/align_left)
    dummy=widget_label(subbase1,value='Time Offset: '+strtrim(string((*pop).timeoffset),2),/align_left)
 
    subbase2=widget_base(base,column=1,/frame)
    dummy=widget_label(subbase2,value='Raw data files: ',/align_left)
    filelist= widget_text(subbase2,/scroll,uname='filelist',value=(*pop).fn,ysize=6)
     
    WIDGET_CONTROL, base, /REALIZE
END
