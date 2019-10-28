;Example script for running a job without the user interface.
;This example uses a fixed TAS of 20 m/s on a 2D-S file.
;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.

;Flight details
date='12042015'            ;mmddyyyy
starttime=65000            ;seconds
stoptime=66000             ;seconds
project='MYPROJECT'
fn='base151204165330.2DS'  ;Raw data file
pthfile=''                 ;Name of file with TAS data
outdir=''                  ;Output directory

;Probe parameters, most are stored in soda2_probespecs
probe=soda2_probespecs(name='2D-S Horizontal Array')

;Processing details and options
endbins=[5,15,25,35,45,55,65,75,85,95,105,125,145,175,225,275,$
         325,400,475,550,625,700,800,900,1000,1200,1400,1600,1800,2000]
arendbins=[0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
rate=1               ;Averaging time
smethod='fastcircle' ;Sizing method, default is 'fastcircle'
particlefile=1       ;Flag to output particle-by-particle data
savfile=1            ;Flag to output sav file
inttime_reject=1     ;Flag to turn shattering correction on/off
reconstruct=1        ;Flag to turn particle reconstruction on/off
stuckbits=0          ;Flag to turn stuck bit detection on/off
water=0              ;Flag to turn water processing on/off
fixedtas=20.0        ;Use a fixed TAS (m/s)
timeoffset=0.0       ;Time correction
greythresh=0         ;Grey threshold (for CIPG)

;Build structure and process data
;Fields not specified here will be updated with defaults in soda2_update_op.pro
op={fn:fn, date:date, starttime:starttime, stoptime:stoptime, project:project,$
    outdir:outdir, timeoffset:timeoffset, format:probe.format, $
    subformat:probe.subformat, probetype:probe.probetype, res:probe.res, $
    armwidth:probe.armwidth, numdiodes:probe.numdiodes, probeid:probe.probeid,$
    shortname:probe.shortname,  wavelength:probe.wavelength, $
    seatag:probe.seatag, endbins:endbins, arendbins:arendbins, rate:rate, $
    smethod:smethod, pth:pthfile, particlefile:particlefile, savfile:savfile, $
    inttime_reject:inttime_reject, reconstruct:reconstruct, stuckbits:stuckbits,$
    water:water, fixedtas:fixedtas, greythresh:greythresh}

soda2_process_2d, op   

END