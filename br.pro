pro br, refrel,wavel,rad,qext,qsca,qabs,x,qback,g,forf,back
   
   
   amu=fltarr(100) & theta=fltarr(100) & pi = fltarr(100)
   tau=fltarr(100) & pi0=fltarr(100) & pi0=fltarr(100)
   pi1=fltarr(100) & su=fltarr(200) & d=dcomplexarr(3000)
   ; dcomplex d(3000),y,refrel,xi,xi0,xi1,an,bn,
   s1=dcomplexarr(200) & s2=dcomplexarr(200)
   ; double precision psi0,psi1,psi,dn,dx
   
   nang=11
   dang=1.570796327/float(nang-1)
   dx=double(x)
   y=x*refrel
   ; print, 'y = , x=', y, x
   xstop=x+4.*x^(.3333)+2.0
   nstop=xstop
   ymod=abs(complex(y))
   ; nmx=amax1(xstop,ymod)+15
   ; HELP USES AMAX1 function
   nmx=max(xstop, ymod)+15
   nmx=fix(nmx)
   dang=1.570796327/float(nang-1)
   for j = 1, nang do begin
      theta[j]=(float(j)-1.)*dang
      amu[j]=cos(theta[j])
   endfor
   d[nmx]=dcomplex(0.0,0.0)
   nn=nmx-1
   for n=1,nn do begin
      rn=nmx-n+1
      d[nmx-n]=(rn/y)-(1./(d[nmx-n+1]+rn/y))
      ; print, n, rn, nmx, y
   endfor
   for j=1,nang do begin
      pi0[j]=0.0
      pi1[j]=1.0
   endfor
   nn=2*nang-1
   for j=1,nn do begin
      s1[j]=dcomplex(0.0,0.0)
      s2[j]=dcomplex(0.0,0.0)
   endfor
   psi0=double(cos(dx))
   psi1=double(sin(dx))
   chi0=-sin(x)
   chi1=cos(x)
   apsi0=psi0
   apsi1=psi1
   xi0=dcomplex(apsi0,-chi0)
   xi1=dcomplex(apsi1,-chi1)
   qsca=0.0
   n=1
   
   mistake:
   ; 200 dn=n
   dn=n
   rn=n
   fn=(2.*rn+1.)/(rn*(rn+1.))
   psi=double((2.*dn-1.)*psi1/dx-psi0)
   apsi=psi
   chi=(2.*rn-1.)*chi1/x - chi0
   xi=dcomplex(apsi,-chi)
   an=double((d[n]/refrel+rn/x)*apsi - apsi1)
   an=an/((d[n]/refrel+rn/x)*xi-xi1)
   bn=double((refrel*d[n]+rn/x)*apsi - apsi1)
   bn=bn/((refrel*d[n]+rn/x)*xi - xi1)
   qsca=qsca+(2.*rn+1.)*(abs(complex(an))*abs(complex(an))$
   +abs(complex(bn))*abs(complex(bn)))
   ; print, 'x1=', xi
   ; print, 'x=, dn=', x, d(n)
   ; print, rn, apsi, apsi1, refrel, chi
   ; print, qsca
   ; print, 'an=', an
   ; print, 'bn=', bn
   ; fn and psi are fine but qsca isn't
   ; print, 'qsca=', qsca, fn, psi
   
   for j=1,nang do begin
      jj=2*nang-j
      pi[j]=pi1[j]
      tau[j]=rn*amu[j]*pi[j] - (rn+1.)*pi0[j]
      p=(-1.)^(n-1)
      s1[j]=s1[j]+fn*(an*pi[j]+bn*tau[j])
      t=(-1.)^n
      s2[j]=s2[j]+fn*(an*tau[j]+bn*pi[j])
      ;?
      if(j ne jj) then begin
         s1[jj]=s1[jj] + fn*(an*pi[j]*p+bn*tau[j]*t)
         s2[jj]=s2[jj] + fn*(an*tau[j]*t+bn*pi[j]*p)
      endif
   endfor
   psi0=psi1
   psi1=psi
   apsi1=psi1
   chi0=chi1
   chi1=chi
   xi1=dcomplex(apsi1,-chi1)
   n=n+1
   rn=n
   
   for j=1,nang do begin
      pi1[j]=((2.*rn-1.)/(rn-1.))*amu[j]*pi[j]
      pi1[j]=pi1[j]-rn*pi0[j]/(rn-1.)
      pi0[j]=pi[j]
   endfor
   
   xxx=fix(n-1-nstop)
   ; print, 'xxx=', xxx
   if (xxx lt 0)then begin
      ; if (n gt 1) then $
      ; print, 'go to', n, nstop
      goto, mistake
   endif
   
   qsca=(2./(x*x))*qsca
   qext=(4./(x*x))*float(s1[1])
   qback=(4./(x*x))*abs(s1[2*nang-1])*abs(s1[2*nang-1])
   ; print, 'scat', qsca, qext, qback, s1(2*nang-1), nang
   alb=qsca/qext
   qabs=qext-qsca
   otn=rad/wavel
   s11nor=0.5*(abs(s2[1])^2+abs(s1[1])^2)
   nan=2*nang-1
   as=0.0
   ar=0.0
   for j=1,nan do begin
      aj=j
      s11=0.5*abs(s2[j])*abs(s2[j])
      s11=s11+0.5*abs(s1[j])*abs(s1[j])
      s12=0.5*abs(s2[j])*abs(s2[j])
      s12=s12-0.5*abs(s1[j])*abs(s1[j])
      pol=-s12/s11
      s33=float(s2[j]*conj(s1[j]))
      s33=s33/s11
      s34=abs(imaginary(s2[j]*conj(s1[j])))
      s34=s34/s11
      s11=s11/s11nor
      su[j]=s11
      ang=dang*(aj-1.)*57.2958
      ug=dang*(aj-1.)
      if(ug lt 3.11) then ug=ug+.1
      as=as+0.5*su[j]*sin(ug)*cos(ug)*dang
      ar=ar+0.5*su[j]*sin(ug)*dang
   endfor
   
   g=as/ar
   nang1=nang-1
   forf=0.
   back=0.
   for j=1,nang1 do begin
      aj=j
      ang=dang*(aj-1.)*57.2958
      if(ang gt 3.12) then ang=3.12
      if(ang lt 0.02) then ang=0.02
      forf=forf+(su[j]+su[j+1])/2.*sin(ang)
      back=back+(su[nang1+j]+su[nang1+1+j])/2.*sin(ang)
   endfor
   
   coef=forf+back
   forf=forf/coef
   back=back/coef
end
