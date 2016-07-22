function fit_poisson,h,dt,minidt,plot=plot       
 ;h is histogram with dt midbins.  Minidt is first guess separator... probably 1e-3 for 2DC
 ;plot displays a comparison

 ;output
 ;a array with solutions for eqn below
 ;dp/dlnt=A0*sigma*dt*exp(-dt*sigma)
 ;a(0)=A0
 ;a(1)=sigma  (=1/tau)  

 ;dt dt to overplot best fit poisson
 ;poissonfit   best fit poisson
 ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
 
 ;Deconstruct the histogram to get individual particles
 int_t=0.0
 FOR i=0,n_elements(dt)-1 DO IF h[i] gt 0 THEN int_t=[int_t,fltarr(h[i])+dt[i]]
 int_t=int_t[1:*]  ;get rid of initial zero needed for initialization

 dlogdt=0.2
 th=total(h)
 h2=h/th/dlogdt; normalised dy/dlog10dt
 
 ;ix=where(int_t lt minidt and int_t gt 1e-9)
 iy=where(int_t gt minidt and int_t lt 1.0)
 if iy[0] eq -1 then return,{a:[0.0,0.0,0.0]}

 a=[0.99,1./(mean(int_t(iy),/nan)),1./(mean(int_t(iy),/nan)/1000.)]
 a1=[1.0,1./(mean(int_t(iy),/nan))]

 res=lmfit(dt,h2,a,func='fn_poisson',/double,iter=150,converg=dummy)
 ;res=lmfit(dt,h2,a1,func='fn_poisson1',/double,iter=150,converg=dummy)

 IF plot THEN BEGIN
  poissonfit=fltarr(n_elements(dt))
  ;poissonfit1=fltarr(n_elements(dt))
  for i=0,n_elements(dt)-1 do begin 
   aa=fn_poisson(dt(i),a)
   poissonfit(i)=aa(0) 
   ;aa1=fn_poisson1(dt(i),a1)
   ;poissonfit1(i)=aa1(0) 
  endfor
  plot,dt,h,/xl,thick=2,xtit='Interarrival time (s)'
  oplot,dt,poissonfit*th*dlogdt,color=80,thick=2
  ;oplot,dt,poissonfit1*th*dlogdt,color=150,thick=2
 ENDIF
 
 return,{a:a}         ;,a1:a1,fit:poissonfit,fit1:poissonfit1,int_t:int_t}
end 
