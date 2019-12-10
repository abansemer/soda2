PRO soda2_merge_event, ev
   ;GUI for merging data from two probes
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   uname=widget_info(ev.id,/uname)

   CASE uname OF
      'browse1':BEGIN
         a=dialog_pickfile(/read,filter=['*.dat'],dialog_parent=widget_info(ev.top,find='process'))
         IF file_test(a) THEN BEGIN
            widget_control,widget_info(ev.top,find='fn1'),set_value=a
            
            ;Fill in the 2DP and PTH if they can be found
            ;Clear them first...
            widget_control,widget_info(ev.top,find='fn_pth'),set_value=''
            widget_control,widget_info(ev.top,find='fn2'),set_value=''
            pos=max([strpos(a,'2D'),strpos(a,'CI'),strpos(a,'CIP10'),strpos(a,'CIP7'),strpos(a,'2DC7'),strpos(a,'2DG')])
            IF pos ne -1 THEN BEGIN
               trythesefiles=[strmid(a,0,pos)+'2DP.dat',strmid(a,0,pos)+'HVPS3.dat',strmid(a,0,pos)+'PIP.dat']
               i=where(file_test(trythesefiles) eq 1)
               IF i[0] ne -1 THEN BEGIN
                  widget_control,widget_info(ev.top,find='fn2'),set_value=trythesefiles[i[0]]
                  widget_control,widget_info(ev.top,find='crossover'),set_value=1000
               ENDIF 
            ENDIF
            pos=max([strpos(a,'2D'),strpos(a,'CI'),strpos(a,'PI'),strpos(a,'HV')])
            IF pos ne -1 THEN BEGIN
                trythisfile=strmid(a,0,pos)+'PTH.dat'
                IF file_test(trythisfile) THEN widget_control,widget_info(ev.top,find='fn_pth'),set_value=trythisfile
                trythisfile=strmid(a,0,pos)+'MMS.dat'
                IF file_test(trythisfile) THEN widget_control,widget_info(ev.top,find='fn_pth'),set_value=trythisfile
            ENDIF
         ENDIF
      END
      'browse2':BEGIN
         a=dialog_pickfile(/read,filter=['*.dat'],dialog_parent=widget_info(ev.top,find='process'))
         IF file_test(a) THEN BEGIN
            widget_control,widget_info(ev.top,find='fn2'),set_value=a
            widget_control,widget_info(ev.top,find='crossover'),set_value=1000
         ENDIF   
      END
      'browse3':BEGIN
         a=dialog_pickfile(/read,filter=['*.dat'],dialog_parent=widget_info(ev.top,find='process'))
         IF file_test(a) THEN widget_control,widget_info(ev.top,find='fn_pth'),set_value=a
      END
      'browse4': BEGIN 
         a=dialog_pickfile(/read,/directory,get_path=a2,dialog_parent=widget_info(ev.top,find='process'))
         IF file_test(a) THEN widget_control,widget_info(ev.top,find='outdir'),set_value=a2
      END
      'paramtype': BEGIN
         id=widget_info(ev.top,find='paramtype')
         widget_control,id,get_uvalue=values
         widget_control,id,get_value=iparam
         paramtype=values[iparam]
         IF paramtype eq 'Brown/Francis' THEN BEGIN
            widget_control,widget_info(ev.top,find='acoeff'),set_value=0.00294
            widget_control,widget_info(ev.top,find='bcoeff'),set_value=1.9
         ENDIF
         IF paramtype eq 'CRYSTAL' THEN BEGIN
            widget_control,widget_info(ev.top,find='acoeff'),set_value=0.0061
            widget_control,widget_info(ev.top,find='bcoeff'),set_value=2.05
         ENDIF
         IF paramtype eq 'Water' THEN BEGIN
            widget_control,widget_info(ev.top,find='acoeff'),set_value=!pi/6
            widget_control,widget_info(ev.top,find='bcoeff'),set_value=3.0
         ENDIF
         IF paramtype eq 'Other' THEN BEGIN
            widget_control,widget_info(ev.top,find='acoeff'),set_value=0
            widget_control,widget_info(ev.top,find='bcoeff'),set_value=0
         ENDIF
      END
      'process': BEGIN 
         widget_control,widget_info(ev.top,find='fn1'),get_value=fn1
         widget_control,widget_info(ev.top,find='fn2'),get_value=fn2
         widget_control,widget_info(ev.top,find='fn_pth'),get_value=pthfile
         widget_control,widget_info(ev.top,find='outdir'),get_value=outdir

         widget_control,widget_info(ev.top,find='acoeff'),get_value=acoeff
         widget_control,widget_info(ev.top,find='bcoeff'),get_value=bcoeff
         ;widget_control,widget_info(ev.top,find='alpha'),get_value=alpha
         widget_control,widget_info(ev.top,find='crossover'),get_value=crossover
         widget_control,widget_info(ev.top,find='binstart'),get_value=binstart
         widget_control,widget_info(ev.top,find='suffix'),get_value=suffix


         IF file_test(fn1) THEN BEGIN
           widget_control,widget_info(ev.top,find='process'),set_value='Please wait...'
           merge_master, fn1, fn2=fn2, pth=pthfile, outdir=outdir, kcoeff=acoeff*6/!pi, ncoeff=0, $
               alpha=bcoeff-3.0, crossover=crossover, binstart=binstart, suffix=suffix, computeradar=0, allspec=0
           widget_control,widget_info(ev.top,find='process'),set_value='BEGIN PROCESSING'
           dummy=dialog_message('Operation Complete',dialog_parent=widget_info(ev.top,find='process'),/info)
         ENDIF ELSE BEGIN
            widget_control,widget_info(ev.top,find='process'),set_value='INVALID FILENAME, TRY ANOTHER'
            wait,3
            widget_control,widget_info(ev.top,find='process'),set_value='BEGIN PROCESSING'
         ENDELSE
      END

      'load':BEGIN
         a=dialog_pickfile(/read,filter=['*BULK.dat'],path=defaultpath,dialog_parent=widget_info(ev.top,find='process'))
         IF file_test(a) THEN BEGIN
            restore,a
            widget_control,widget_info(ev.top,find='fn1'),set_value=data.op.fn1
            widget_control,widget_info(ev.top,find='fn2'),set_value=data.op.fn2
            widget_control,widget_info(ev.top,find='fn_pth'),set_value=data.op.pth
            widget_control,widget_info(ev.top,find='outdir'),set_value=data.op.outdir
            widget_control,widget_info(ev.top,find='crossover'),set_value=data.op.crossover

            widget_control,widget_info(ev.top,find='acoeff'),set_value=data.op.kcoeff*!pi/6
            widget_control,widget_info(ev.top,find='bcoeff'),set_value=data.op.alpha+3.0
            widget_control,widget_info(ev.top,find='binstart'),set_value=data.op.binstart
            widget_control,widget_info(ev.top,find='suffix'),set_value=data.op.suffix
         ENDIF
      END
      
      'compare':BEGIN
         widget_control,widget_info(ev.top,find='fn1'),get_value=fn1
         widget_control,widget_info(ev.top,find='fn2'),get_value=fn2
         IF fn2 eq '' THEN fn2=fn1  ;Just compare with itself if no extra file listed
         widget_control,widget_info(ev.top,find='crossover'),get_value=crossover
         IF file_test(fn1) eq 1 and (file_test(fn2) eq 1) THEN soda2_compare, fn1, fn2, crossover=crossover ELSE BEGIN
            widget_control,widget_info(ev.top,find='compare'),set_value='INVALID FILENAME, TRY ANOTHER'
            wait,3
            widget_control,widget_info(ev.top,find='compare'),set_value='Compare Data'
         ENDELSE
      END
      
      'quit': WIDGET_CONTROL, ev.TOP, /DESTROY 

      ELSE: dummy=0
   ENDCASE
END



PRO soda2_merge
   base = WIDGET_BASE(COLUMN=1,title='Compare and Merge Spectra',MBar=menubarID)

   fileID=widget_button(menubarID, value='File', /menu)
   loadID=widget_button(fileID, value='Load Settings...',uname='load')
   quitID=widget_button(fileID, value='Quit',uname='quit')

   filebase1=widget_base(base,row=1)
   filebase2=widget_base(base,row=1)
   xbase=widget_base(base,row=1)
   filebase3=widget_base(base,row=1)
   filebase4=widget_base(base,row=1)

   file1ID=cw_field(filebase1,/string,   title='Filename:             ',uname='fn1',xsize=60)
   browse1=widget_button(filebase1,value='Select...',uname='browse1')

   file2ID=cw_field(filebase2,/string,   title='Filename 2 (optional):',uname='fn2',xsize=60)
   browse2=widget_button(filebase2,value='Select...',uname='browse2')

   crossoverID=cw_field(xbase,/int, title='Crossover[um]:',uname='crossover',xsize=6,value=0)
   binstartID=cw_field(xbase,/int, title='Start bin index:',uname='binstart',xsize=2,value=1)
   suffixID=cw_field(xbase,/string, title='Suffix:',uname='suffix',xsize=6)

   pthfileID=cw_field(filebase3,/string, title='PTH file (optional):  ',uname='fn_pth',xsize=60)
   browse2=widget_button(filebase3,value='Select...',uname='browse3')
   
   cd,current=currentdir
   outdirID=cw_field(filebase4,/string,  title='Output dir (optional):',value=currentdir+path_sep(),uname='outdir',xsize=60)
   browse2=widget_button(filebase4,value='Select...',uname='browse4')

   coeffbase=widget_base(base,column=1,/frame)
   coeffbase1=widget_base(coeffbase,row=1)
   coeffbase2=widget_base(coeffbase,row=1)

   vals=['Brown/Francis', 'CRYSTAL', 'Water', 'Other']
   paramtype=cw_bgroup(coeffbase1,vals, uname='paramtype',/row,/exclusive,label_left='Mass Parameterization (cgs units): ',uval=vals,set_value=0)

   acoeffID=cw_field(coeffbase2,/float, title='a=  ',uname='acoeff',value=0.00294)
   bcoeffID=cw_field(coeffbase2,/float, title='b=  ',uname='bcoeff',value=1.9)

   compare = WIDGET_BUTTON(base, value='Compare Data', uname='compare')
   process = WIDGET_BUTTON(base, value='BEGIN PROCESSING', uname='process')

   WIDGET_CONTROL, base, /REALIZE
   XMANAGER, 'soda2_merge', base, /no_block
END
