FUNCTION soda2_probespecs, name=name, id=id
   ;PRO to return settings for all supported probes in a structure
   ;Units:  res[um], armwidth[cm], tau[s], wavelength[m]
   ;AB, NCAR, 2010
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.


   base={probename:'', shortname:'', format:'', subformat:0s, probetype:'', probeid:'', $
         greythresh:0, res:0.0, yres:0.0, armwidth:0.0, numdiodes:0.0, tau:0.0, firstsliceskip:0, $
         dofthreshold:0.0, dofconst:3.0, wavelength:0.0, seatag:lonarr(3)}
   ;Tau is the response time in seconds as in Baumgardner 1997 JTECH
   ;Legacy probes use mean value from Strapp et al JTECH 2001, newer probes various communications.
   ;Yres is the nominal resolution in airflow direction, as controlled by slicing rate.  May be
   ;   adjusted to correct airflow speed distortions.  Can be further modified by stretch correction option.


   ;First addition
   x=base
   x.probename='NASA Langley CAPS'
   x.shortname='CIP10'
   x.format='DMT'
   x.subformat=0
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=10
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=x

   ;Add more probes to the list starting here:
   x=base
   x.probename='CAPS-Grey'
   x.shortname='CIPG'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIPG'
   x.probeid=''
   x.greythresh=2
   x.res=15
   x.yres=15
   x.armwidth=10
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='CAPS-Grey (University of Wyoming)'
   x.shortname='CIPG'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIPG'
   x.probeid=''
   x.greythresh=2
   x.res=25
   x.yres=25
   x.armwidth=6.1
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NOAA HRD CIP Grey (PADS)'
   x.shortname='CIPG'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIPG'
   x.probeid=''
   x.greythresh=2
   x.res=25
   x.yres=25
   x.armwidth=10.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='DLR CAPS-Grey (PADS)'
   x.shortname='CIP_DLR'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIPG'
   x.probeid=''
   x.greythresh=2
   x.res=25
   x.yres=25
   x.armwidth=4.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='DLR PIP (PADS)'
   x.shortname='PIP'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIPG'  ;This probe uses the grey array board.  All shadows are at grey level 1.
   x.probeid=''
   x.greythresh=1
   x.res=100
   x.yres=100
   x.armwidth=26.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='CIRPAS CIP (PACS)'
   x.shortname='CIP'
   x.format='DMT'
   x.subformat=0
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=6.1
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='CIRPAS PIP (PACS)'
   x.shortname='PIP'
   x.format='DMT'
   x.subformat=0
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=100
   x.yres=100
   x.armwidth=10
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='CIRPAS CIP (PADS)'
   x.shortname='CIP'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=6.1
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='CIRPAS PIP (PADS)'
   x.shortname='PIP'
   x.format='DMT'
   x.subformat=1
   x.probetype='PIP'
   x.probeid=''
   x.greythresh=0
   x.res=100
   x.yres=100
   x.armwidth=10
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NCAR-RAL CIP (PADS)'
   x.shortname='CIP'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=10.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NCAR-RAL PIP (PADS)'
   x.shortname='PIP'
   x.format='DMT'
   x.subformat=1
   x.probetype='PIP'
   x.probeid=''
   x.greythresh=0
   x.res=100
   x.yres=100
   x.armwidth=26.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NOAA HRD CIP (PADS)'
   x.shortname='CIP'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=10.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NOAA HRD PIP (PADS)'
   x.shortname='PIP'
   x.format='DMT'
   x.subformat=1
   x.probetype='PIP'
   x.probeid=''
   x.greythresh=0
   x.res=100
   x.yres=100
   x.armwidth=40.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='FAAM CIP100 (PADS)'
   x.shortname='CIP'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=100
   x.yres=100
   x.armwidth=7.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='FAAM CIP-Grey (PADS)'
   x.shortname='CIPG'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIPG'
   x.probeid=''
   x.greythresh=2
   x.res=15
   x.yres=15
   x.armwidth=4.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='SEA CIP-Grey (SEA M300)'
   x.shortname='CIPG'
   x.format='SEA'
   x.subformat=1
   x.probetype='CIPG'
   x.probeid=''
   x.greythresh=2
   x.res=25
   x.yres=25
   x.armwidth=7.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[3000, 2000]
   all=[all,x]

   x=base
   x.probename='NASA Glenn CIP-Grey (SEA M300)'
   x.shortname='CIPG'
   x.format='SEA'
   x.subformat=1
   x.probetype='CIPG'
   x.probeid=''
   x.greythresh=2
   x.res=15
   x.yres=15
   x.armwidth=10.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[23000, 22000]
   all=[all,x]

   x=base
   x.probename='Shanxi CIP-Mono (PADS)'
   x.shortname='CIP'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=10.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='Shanxi PIP-Mono (PADS)'
   x.shortname='PIP'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=100
   x.yres=100
   x.armwidth=26.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='Shanxi PIP-Grey (PADS)'
   x.shortname='PIPG'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIPG'
   x.probeid=''
   x.greythresh=2
   x.res=100
   x.yres=100
   x.armwidth=26.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='Monash CIP (SEA M300)'
   x.shortname='CIP'
   x.format='SEA'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=7.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[33000]
   all=[all,x]

   x=base
   x.probename='NRC CIP (SEA M300)'
   x.shortname='CIP'
   x.format='SEA'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=15
   x.yres=15
   x.armwidth=7.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[33000]
   all=[all,x]

   x=base
   x.probename='NRC PIP (SEA M300)'
   x.shortname='PIP'
   x.format='SEA'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=100
   x.yres=100
   x.armwidth=22.7
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[31000]
   all=[all,x]

   x=base
   x.probename='NRC 2DC (SEA M300)'
   x.shortname='2DC'
   x.format='SEA'
   x.subformat=1
   x.probetype='2DC'
   x.probeid=''
   x.greythresh=0
   x.res=50
   x.yres=50
   x.armwidth=7.7
   x.numdiodes=32
   x.tau=0.58e-6
   x.firstsliceskip=1
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.6328e-6
   x.seatag=[7000, 7006, 7007]
   all=[all,x]

   x=base
   x.probename='NRC 2DP (SEA M300)'
   x.shortname='2DP'
   x.format='SEA'
   x.subformat=1
   x.probetype='2DC'
   x.probeid=''
   x.greythresh=0
   x.res=200
   x.yres=200
   x.armwidth=26.0
   x.numdiodes=32
   x.tau=0.58e-6
   x.firstsliceskip=1
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.6328e-6
   x.seatag=[8000, 8006, 8007]
   all=[all,x]

   x=base
   x.probename='HIWC PIP (SEA M300)'
   x.shortname='PIP'
   x.format='SEA'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=100
   x.yres=100
   x.armwidth=22.2  ;Has Korolev tips
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[35000, 34000, 0]  ;Second item is 1D data
   all=[all,x]

   x=base
   x.probename='HIWC PIP (PADS)'
   x.shortname='PIP'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=100
   x.yres=100
   x.armwidth=22.2  ;Has Korolev tips
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[35000, 34000, 0]  ;Second item is 1D data
   all=[all,x]

   x=base
   x.probename='CCP from SPEC (SEA M300)'
   x.shortname='CIP'
   x.format='SEA'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=15
   x.yres=15
   x.armwidth=7.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[36000]
   all=[all,x]

   x=base
   x.probename='NIMS CCP (15um)'
   x.shortname='CIP'
   x.format='DMT'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=15         ;Guess for now
   x.yres=15
   x.armwidth=4.0   ;Just a guess, looks like they have Korolev tips/shoes
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NCAR Fast-2DC [C4] (25um)'
   x.shortname='F2DC'
   x.format='RAF'
   x.subformat=3
   x.probetype='F2DC'
   x.probeid='C4'
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=6.1
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NCAR Fast-2DC_v2 [C4] (25um)'
   x.shortname='F2DC'
   x.format='RAF'
   x.subformat=3
   x.probetype='F2DC_v2'
   x.probeid='C4'
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=6.1
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NCAR Fast-2DC [C4] (25um) Pre-2011'
   x.shortname='F2DC'
   x.format='RAF'
   x.subformat=0
   x.probetype='F2DC'
   x.probeid='C4'
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=6.1
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NCAR Fast-2DC (25um) 66% DoF threshold'
   x.shortname='F2DC'
   x.format='RAF'
   x.subformat=3
   x.probetype='F2DC66'
   x.probeid='C4'
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=6.1
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=66.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NCAR Fast-2DC [C5] (25um)'
   x.shortname='F2DC'
   x.format='RAF'
   x.subformat=3
   x.probetype='F2DC'
   x.probeid='C5'
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=6.1
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NCAR Fast-2DC [C6] (10um)'
   x.shortname='F2DC10'
   x.format='RAF'
   x.subformat=3
   x.probetype='F2DC'
   x.probeid='C6'
   x.greythresh=0
   x.res=10
   x.yres=10
   x.armwidth=6.1
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NCAR Fast-2DC_v2 [C6] (10um)' ;30MHz clock
   x.shortname='F2DC10'
   x.format='RAF'
   x.subformat=3
   x.probetype='F2DC_v2'
   x.probeid='C6'
   x.greythresh=0
   x.res=10
   x.yres=10
   x.armwidth=6.1
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NCAR 2DP'
   x.shortname='2DP'
   x.format='RAF'
   x.subformat=3
   x.probetype='2DC'
   x.probeid='P1'
   x.greythresh=0
   x.res=200
   x.yres=200
   x.armwidth=26.1
   x.numdiodes=32
   x.tau=0.6e-6
   x.firstsliceskip=1
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.6328e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='NCAR Fast-2DP [P4] (150um)'
   x.shortname='F2DP'
   x.format='RAF'
   x.subformat=3
   x.probetype='F2DP'
   x.probeid='P4'
   x.greythresh=0
   x.res=150
   x.yres=150
   x.armwidth=26.1
   x.numdiodes=64
   x.tau=0.6e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.6328e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='Wyoming Fast-2DC (NCAR-RAF format)'
   x.shortname='F2DC'
   x.format='RAF'
   x.subformat=3
   x.probetype='F2DC'
   x.probeid='C5'
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=6.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='Wyoming 2DP (NCAR-RAF format)'
   x.shortname='2DP'
   x.format='RAF'
   x.subformat=4        ;4 flags use of fix2dimage for bad timelines.
   x.probetype='2DP'
   x.probeid='P1'
   x.greythresh=0
   x.res=200
   x.yres=200
   x.armwidth=26.0
   x.numdiodes=32
   x.tau=0.6e-6
   x.firstsliceskip=1
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.6328e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='HVPS-3'
   x.shortname='HVPS3'
   x.format='SPEC'
   x.subformat=0
   x.probetype='HVPS3'
   x.probeid='V'
   x.greythresh=0
   x.res=150
   x.yres=150
   x.armwidth=16.25
   x.numdiodes=128
   x.tau=41.0e-9
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=8.12
   x.wavelength=0.785e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='2D-S Horizontal Array'
   x.shortname='2DS'
   x.format='SPEC'
   x.subformat=0
   x.probetype='2DS'
   x.probeid='H'
   x.greythresh=0
   x.res=10
   x.yres=10
   x.armwidth=6.3
   x.numdiodes=128
   x.tau=41.0e-9
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=8.12
   x.wavelength=0.785e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='2D-S Vertical Array'
   x.shortname='2DS'
   x.format='SPEC'
   x.subformat=0
   x.probetype='2DS'
   x.probeid='V'
   x.greythresh=0
   x.res=10
   x.yres=10
   x.armwidth=6.3
   x.numdiodes=128
   x.tau=41.0e-9
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=8.12
   x.wavelength=0.785e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='2D-S Horizontal Array (11um)'
   x.shortname='2DS'
   x.format='SPEC'
   x.subformat=0
   x.probetype='2DS'
   x.probeid='H'
   x.greythresh=0
   x.res=11
   x.yres=10    ;For most probes the slice rate is still set to 10um
   x.armwidth=6.3
   x.numdiodes=128
   x.tau=41.0e-9
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=8.12
   x.wavelength=0.785e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='2D-S Vertical Array (11um)'
   x.shortname='2DS'
   x.format='SPEC'
   x.subformat=0
   x.probetype='2DS'
   x.probeid='V'
   x.greythresh=0
   x.res=11
   x.yres=10    ;For most probes the slice rate is still set to 10um
   x.armwidth=6.3
   x.numdiodes=128
   x.tau=41.0e-9
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=8.12
   x.wavelength=0.785e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='3VCPI/Hawkeye Horizontal Array'
   x.shortname='3VCPI'
   x.format='SPEC'
   x.subformat=0
   x.probetype='3VCPI'
   x.probeid='H'
   x.greythresh=0
   x.res=10
   x.yres=10
   x.armwidth=5.08
   x.numdiodes=128
   x.tau=41.0e-9
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=8.12
   x.wavelength=0.785e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='3VCPI/Hawkeye Horizontal Array (50um)'
   x.shortname='3VCPI'
   x.format='SPEC'
   x.subformat=0
   x.probetype='3VCPI'
   x.probeid='H'
   x.greythresh=0
   x.res=50
   x.yres=50
   x.armwidth=5.08
   x.numdiodes=128
   x.tau=41.0e-9
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=8.12
   x.wavelength=0.785e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='3VCPI/Hawkeye Vertical Array'
   x.shortname='3VCPI'
   x.format='SPEC'
   x.subformat=0
   x.probetype='3VCPI'
   x.probeid='V'
   x.greythresh=0
   x.res=10
   x.yres=10
   x.armwidth=5.08
   x.numdiodes=128
   x.tau=41.0e-9
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=8.12
   x.wavelength=0.785e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='HVPS-4 Vertical Array (50um)'
   x.shortname='HVPS4'
   x.format='SPEC'
   x.subformat=0
   x.probetype='HVPS4'
   x.probeid='V50'
   x.greythresh=0
   x.res=50
   x.yres=50
   x.armwidth=12.0  ;A guess for now, need to verify
   x.numdiodes=128
   x.tau=41.0e-9
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=8.12
   x.wavelength=0.785e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='HVPS-4 Vertical Array (150um)'
   x.shortname='HVPS4'
   x.format='SPEC'
   x.subformat=0
   x.probetype='HVPS4'
   x.probeid='V150'
   x.greythresh=0
   x.res=150
   x.yres=150
   x.armwidth=12.0  ;A guess for now, need to verify
   x.numdiodes=128
   x.tau=41.0e-9
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=8.12
   x.wavelength=0.785e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='HVPS-4 Horizontal Array (50um)'
   x.shortname='HVPS4'
   x.format='SPEC'
   x.subformat=0
   x.probetype='HVPS4'
   x.probeid='H50'
   x.greythresh=0
   x.res=50
   x.yres=50
   x.armwidth=12.0  ;A guess for now, need to verify
   x.numdiodes=128
   x.tau=41.0e-9
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=8.12
   x.wavelength=0.785e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='HVPS-4 Horizontal Array (150um)'
   x.shortname='HVPS4'
   x.format='SPEC'
   x.subformat=0
   x.probetype='HVPS4'
   x.probeid='H150'
   x.greythresh=0
   x.res=150
   x.yres=150
   x.armwidth=12.0  ;A guess for now, need to verify
   x.numdiodes=128
   x.tau=41.0e-9
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=8.12
   x.wavelength=0.785e-6
   x.seatag=[0]
   all=[all,x]

   x=base
   x.probename='UND 2DC'
   x.shortname='2DC'
   x.format='SEA'
   x.subformat=0
   x.probetype='2DC'
   x.probeid=''
   x.greythresh=0
   x.res=30
   x.yres=30
   x.armwidth=6.1
   x.numdiodes=32
   x.tau=0.6e-6
   x.firstsliceskip=1
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.6328e-6
   x.seatag=[5000, 5001, 5002]
   all=[all,x]

   x=base
   x.probename='UND 2DC with Korolev Tips'
   x.shortname='2DC7'
   x.format='SEA'
   x.subformat=0
   x.probetype='2DC'
   x.probeid=''
   x.greythresh=0
   x.res=30
   x.yres=30
   x.armwidth=7.0
   x.numdiodes=32
   x.tau=0.6e-6
   x.firstsliceskip=1
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.6328e-6
   x.seatag=[5000, 5001, 5002]
   all=[all,x]

   x=base
   x.probename='UND CIP (SEA M300)'
   x.shortname='CIP'
   x.format='SEA'
   x.subformat=1
   x.probetype='CIP'
   x.probeid=''
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=6.0
   x.numdiodes=64
   x.tau=0.051e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.658e-6
   x.seatag=[33000, 32000, 0]  ;Second item is 1D data
   all=[all,x]

   x=base
   x.probename='MSC 2DC'
   x.shortname='2DC'
   x.format='SEA'
   x.subformat=0
   x.probetype='2DC'
   x.probeid=''
   x.greythresh=0
   x.res=25
   x.yres=25
   x.armwidth=6.1
   x.numdiodes=32
   x.tau=0.58e-6
   x.firstsliceskip=1
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.6328e-6
   x.seatag=[33000]
   all=[all,x]

   x=base
   x.probename='SEA 1D2D (15um)'
   x.shortname='1D2D'
   x.format='SEA'
   x.subformat=0
   x.probetype='1D2D'
   x.probeid=''
   x.greythresh=0
   x.res=15
   x.yres=15
   x.armwidth=5.4
   x.numdiodes=64
   x.tau=0.020e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.660e-6
   x.seatag=[10000, 10001]  ;[ImageTag, TASfactorTag]
   all=[all,x]

   x=base
   x.probename='SEA 1D2D (50um)'
   x.shortname='1D2D'
   x.format='SEA'
   x.subformat=0
   x.probetype='1D2D'
   x.probeid=''
   x.greythresh=0
   x.res=50
   x.yres=50
   x.armwidth=5.4
   x.numdiodes=64
   x.tau=0.020e-6
   x.firstsliceskip=0
   x.dofthreshold=0.0
   x.dofconst=3.0
   x.wavelength=0.660e-6
   x.seatag=[11000, 11001]  ;[ImageTag, TASfactorTag]
   all=[all,x]


   IF n_elements(name) ne 0 THEN BEGIN
      w=where(name eq all.probename,nw)
      IF nw gt 0 THEN return, all[w] ELSE return, base
   ENDIF
   IF n_elements(id) ne 0 THEN BEGIN
      IF max(id) lt n_elements(all) THEN return, all[id] ELSE return, base
   ENDIF

   ;Return everything if no probe specified
   return, all
END
