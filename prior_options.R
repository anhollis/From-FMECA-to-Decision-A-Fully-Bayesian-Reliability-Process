parametersolver = function(qu,p,init) {
  # qu is the value of the quantile: median = qu[1]
  # p is the quantile: 0.5, 0.25 (median, Q1)
  # init is an initial value
  qu <- qu
  p <- p
  betaoptim = function(param) { 
    q1 <- qu[1]
    q2 <- qu[2]
    p1 <- p[1]
    p2 <- p[2]
    (pbeta(q1,param[1],param[2])-p1)^2 + (pbeta(q2,param[1],param[2])-p2)^2
  }
  
  r = optim(init,betaoptim)
  v = unlist(r)
  t = c(v[1],v[2])
  print(t)
}

IQR_optim<-function(qu1,p1,init1,qu2,p2,init2){
  betaoptim = function(param) { 
    quant1<-(pbeta(qu1[1],param[1],param[2])-p1[1])^2 + (pbeta(qu1[2],param[1],param[2])-p1[2])^2
    quant2<-(pbeta(qu2[1],param[3],param[4])-p2[1])^2 + (pbeta(qu2[2],param[3],param[4])-p2[2])^2
    IQR1<-qbeta(0.75,param[1],param[2])-qbeta(0.25,param[1],param[2])
    IQR2<-qbeta(0.75,param[3],param[4])-qbeta(0.25,param[3],param[4])
    IQR_diff<-(IQR1-IQR2)^2
    return(quant1+quant2+IQR_diff)
  }
  r = optim(c(init1,init2),betaoptim)
  v = unlist(r)
  t = c(v[1],v[2],v[3],v[4])
  print(t)
}

#Prior 1

bp1 <- parametersolver(c(.01,.1),c(.7,.99),c(1,1))

x=seq(0,1,by=0.001)
plot(x,dbeta(x,bp1[1],bp1[2]),type="l",col="darkgreen",main="Prior 1",xlab="Failure Probability",
     ylab="",ylim=c(0,10),lwd=2)

bp2<-parametersolver(c(.1,.5),c(.15,.85),c(1,1))
lines(x,dbeta(x,bp2[1],bp2[2]),col="gold",lwd=2)

legend(0.6, 8, legend=c("OCC 1", "OCC 3"),
       col=c("darkgreen", "gold"), lty=1, cex=0.8)

bp1[1]/(bp1[1]+bp1[2])
bp2[1]/(bp2[1]+bp2[2])
(bp1[1]*bp1[2])/((bp1[1]+bp1[2])^2*(bp1[1]+bp1[2]+1))
(bp2[1]*bp2[2])/((bp2[1]+bp2[2])^2*(bp2[1]+bp2[2]+1))

#Prior2
bp1 <- parametersolver(c(.01,.45),c(.6,0.9),c(1,1))

x=seq(0,1,by=0.001)
plot(x,dbeta(x,bp1[1],bp1[2]),type="l",col="darkgreen",main="Prior 2",xlab="Failure Probability",
     ylab="",ylim=c(0,10),lwd=2)

bp2<-parametersolver(c(.2,.45),c(.05,.95),c(1,1))
lines(x,dbeta(x,bp2[1],bp2[2]),col="gold",lwd=2)

legend(0.6, 8, legend=c("OCC 1", "OCC 3"),
       col=c("darkgreen", "gold"), lty=1, cex=0.8)

bp1[1]/(bp1[1]+bp1[2])
bp2[1]/(bp2[1]+bp2[2])
(bp1[1]*bp1[2])/((bp1[1]+bp1[2])^2*(bp1[1]+bp1[2]+1))
(bp2[1]*bp2[2])/((bp2[1]+bp2[2])^2*(bp2[1]+bp2[2]+1))
