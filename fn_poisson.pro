function fn_poisson,dt,ans

;y=a*dt*sigma*exp(-sigma*dt) + (1-a)*dt*rho*exp(-rho*dt)   sigma=1/tau1, rho=1/tau2

;ans=[a,sigma,rho]
k=alog10(exp(1))

out=fltarr(4)

out(0)=ans(0)*ans(1)*dt*exp(-ans(1)*dt)/k+(1.0-ans(0))*ans(2)*dt*exp(-ans(2)*dt)/k
out(1)=ans(1)*dt*exp(-ans(1)*dt)/k-ans(2)*dt*exp(-ans(2)*dt)/k
out(2)=ans(0)*dt*exp(-ans(1)*dt)/k-ans(0)*ans(1)*dt^2*exp(-ans(1)*dt)/k
out(3)=(1.0-ans(0))*dt*exp(-ans(2)*dt)/k-(1.0-ans(0))*ans(2)*dt^2*exp(-ans(2)*dt)/k


return,out

end
