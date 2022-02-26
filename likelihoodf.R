library(maxLik)
#2
da1=read.csv("data1.csv")
n1 <- length(da1$price)
sim_ret1 <- diff(da1$price)/da1$price[1:n1-1]
log_ret1 <- diff(log(da1$price))
plot(da1$price,type='l')
plot(log_ret1,type='l')
#log_ret1 <- as.data.frame(log_ret1)

rt = log_ret1[-1]   #刪除log_ret第一筆資料
rt_1 = log_ret1[-length(log_ret1)]
likelihoodf = function(param){
  phi0 <- param[1]
  phi1 <- param[2]
  sigma <- param[3]
  f = (1/(2 * pi * sigma)^(0.5)) * exp((-1 / (2 * sigma)) * (rt - phi0 - phi1 * rt_1)^2)
  lnf = sum(log(f))
  return(lnf + log(((1 - phi1^2) / (2 * pi * sigma ))^(0.5)*exp((-(1 - phi1^2) / (2 * sigma ))* (rt[1] - phi0 / (1 - phi1))^2)))
}

likeli = function(param){return(-likelihoodf(param))} #optim 是找最小值,所以likeli取負,變成找最大值

p <- optim(c(phi0=0.5,phi1=0.5,sigma=0.5),likeli)
p
result <- maxLik(likelihoodf, start = c(0.5,0.5,0.5))
result

grad = function(param){
           phi0 <- param[1]
           phi1 <- param[2]
           sigma <- param[3]
         f1=(-1/(2*sigma))*(sum((-2)*(rt-phi0-phi1*rt_1 ))+2*(1-phi1^2)*(rt[1]-phi0/(1-phi1))*(-1/(1-phi1)))
         f2=(phi1/(1-phi^1))-(-1/(2*sigma))*(sum((-2)*(rt_1)*(rt-phi0-phi1*rt_1 ))-2*phi1*(rt[1]-phi0/(1-phi1))^2*-2*phi0*(1-phi1^2)*(rt[1]-phi0/(1-phi1))/(1-phi1)^2)
         f3=-(n1/2*sigma)-(1/(2*sigma)^2)*((sum(rt-phi0-phi1*rt_1)^2+(1-phi1^2)*(rt[1]-phi0/(1-phi1))^2))
         return(c(f1,f2,f3))
}

optim(c(phi0=0.5,phi1=0.5,sigma=0.5),likeli,grad)

model=ar(log_ret1,order.max = 1,method = 'mle') #可寫成 ar.mle(log_ret1)
#model=arima(log_ret1,c(1,0,0))
m=c(model$ar,model$var.pred)
print(m)
