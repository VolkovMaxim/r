cars
plot(cars)
x=cars$speed
y=cars$dist
spanvec=c(0.05, 0.2, 0.5, 1)
for (j in (1:4)){
  lines(supsmu(x,y, span=spanvec[j]),col=j)
}
legend(18,40, legend=spanvec, col=seq(spanvec),
       lty=1,cex=.5)
plot(cars)
lines(supsmu(x,y))
lines(supsmu(x,y,span=0.2), col="red")

plot(cars)
lines(supsmu(x,y,span=1), col="red")
abline(lm(y~x))

plot(cars)
lines(supsmu(x,y, bass=10)))
lines(supsmu(x,y, bass=0), col="red")

l1=loess(dist~speed, cars, 
         control=loess.control(surface="direct"))
plot(cars)
lines(l1)
s=seq(from=5, to=30, by=1)
s
p=predict(l1, data.frame(speed=s),se=TRUE)
plot(x,y, xlim=c(4,31))
lines(s,p$fit, col="red")

install.packages("fANCOVA")
library(fANCOVA)
loess.as(x,y,plot=TRUE)
l2=loess.as(x,y,criterion="gcv",plot=TRUE)
loess.as(x,y,criterion="aicc",plot=TRUE)
l2$fitted

install.packages("np")
library(np)
bvec=c("cv.aic", "cv.ls")
kervec=c("epanechnikov","gaussian")
polvec=c("lc","ll")
res=1000
for (i in (1:2)){
  for (j in (1:2)){
    for (k in (1:2)){
      model=npreg(txdat=x, tydat=y,
                  ckertype=kervec[i],
                  regtype=polvec[j],
                  bwmethod=bvec[k])
      m=mean((fitted(model)-y)^2)
      if (m<res){
        res=m
        vec=c(i,j,k)
      }
    }
  }
}
model=npreg(txdat=x, tydat=y,
            ckertype=kervec[vec[1]],
            regtype=polvec[vec[2]],
            bwmethod=bvec[vec[3]])
plot(cars)
points(x,fitted(model), type="l", col="red")

r=function(v, cc){
  print(mean((y-v)^2))
  points(x,v, type="l", col=cc)
}
plot(x,y)
r(fitted(model), "red")
model2=loess(y~x)
r(predict(model2),"blue")
model3=supsmu(x,y)
yy=rep(0,length(x))
for (j in (1: length(x))){
  yy[j]=model3$y[which(model3$x==x[j])]
}
r(yy,"green”)