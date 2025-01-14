function mie, wavel, dspec, arspec, k=k,n=n,alpha=alpha
   ;wavelength is in mm
   ;dspec is an array of size midbins in cm  **!!! DIAMETER !!!**
   ;arspec is an array of ar midbins 
   
   ;outputs:
   ;   qback:  normalized backscattering cross section, sigma/(pi*radius^2)
   ;   qback1: same, but for Rayleigh scattering?
   ;Units have been checked and verified, 10-25-02 ARB
   ;As of 10-2002, extinction units are untested
   
   IF n_elements(k) eq 0 THEN k=0.07
   IF n_elements(n) eq 0 THEN n=1.5
   IF n_elements(alpha) eq 0 THEN alpha=-0.5
   
   s=size(dspec)
   ntot=s[1]
   s=size(arspec)
   dbin=s[1]

   IF (k eq 1.0) and (n eq 0) and (alpha eq 0) THEN BEGIN
      ;Water options
      densitylimit=1.0 
    
      ;For water.  From Simone 2/2007:
      IF wavel ge 22.0 THEN mi=complex(7.7271,2.174);  at 13GHz and longer wavelengths
      IF wavel eq 8.4 THEN mi=complex(5.5943,2.812);  at 35
      IF wavel eq 3.19 THEN mi=complex(3.5704,2.1109); at 94
   ENDIF ELSE BEGIN
      ;Ice
      densitylimit=0.91
      ;k2=0.2 ;Not sure if this is the right thing to do since we're using melted D...

      ; calculated normalized backscatter cross section for solid ice
      ; density of solid ice essen independent of temperature
      ; from Warren applied optics 1224 vol 13 april 1984 at all lambda
      ; 10 cm 1.738 3 mm 1.786 so use average

      mi=complex(1.78,0.00)  
   ENDELSE
   
   k2=0.93                        ;Long wavelengths
   IF wavel eq 8.4 THEN k2=0.88   ;Ka, according to Sergey Jan 2015 email
   IF wavel eq 3.19 THEN k2=0.69  ;W, according to Sergey Jan 2015 email
   
   rhoi=((dspec^alpha) # (k*arspec^n))<densitylimit

   kice_sq=fltarr(ntot,dbin)
   qext=fltarr(ntot,dbin) & qext1=fltarr(ntot,dbin) & qback=fltarr(ntot, dbin) 
   qback1=fltarr(ntot, dbin) & ze=fltarr(ntot,dbin)

   for j = 0, ntot-1 do begin
      for i = 0, dbin-1 do begin
         ei=mi^2
         ri=0.90
         
         ; density of the ice (mixture)
         r=rhoi[j, i]
         
         ; from Matrosov tables normalized backscatter cross sections
         ; at 3 mm 0C (90 ghz) m = 2.81 + 1.31 imaginary part
         ; at 8.6 mm exact m = 4.08 + 2.45 imaginary part
         ; at 2.0 cm 6.04 + 3.02 imaginary part at 0C
         e=(ri*ei+2.*ri+2.*r*ei-2*r)/(ri*ei+2.*ri-r*ei+r)
         m=complex(sqrt(e))
         kice_sq[j, i]=abs((m^2-1)/(m^2+2))^2
         refrel=m

         ; MAKE SURE YOU KNOW---in Sergey's program units are mm (wavelength) multipy d by 10
         rad=10.*dspec[j]/2.
         ; wavel is the wavelength in mm
         ; refrel is the complex refrective index of ice
         ; d is particle diameter in mm
         x=2.*3.14159*rad/wavel
         a=abs(complex((refrel^2-1.)/(refrel^2+2.)))^2
         qext1[j, i]=8./3.*x^4*a
         qback1[j, i]=4*x^4*a
         br, refrel,wavel,rad,qe,qsca,qabs,x,qb,g,forf,back
         qback[j, i]=qb
         qext[j, i]=qe
         ze[j,i]=wavel^4*qback[j,i]*!pi*rad^2/(!pi^5*k2)  ; in mm^6
      endfor
   endfor
   return,{qback1:qback1, qback:qback, qext1:qext1, qext:qext, kice_sq:kice_sq, ze:ze}
end
