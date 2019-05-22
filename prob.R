hist(rnorm(1000, mean=0, sd=1))
N=1000
N
xi=sample(0:5,size=N,replace=TRUE, prob = c(rep(0.1,5),0.5))
table(xi)
m=c(-1,-1/2,0,1/2,1,0)
s=c(rep(0.1,5),1)
X=rep(0,N)
for (j in (1:N)){
  X[j]=rnorm(1, mean=m[xi[j]+1], sd=s[xi[j]+1])
}
hist(X, breaks=100)

p=function(x){
  y=dnorm(x)/2
  for (j in (0:4)){
    y=y+dnorm(x,mean=m[j+1],sd=0.1)/10
  }
  return(y)
}
plot(p,xlim=c(-3,3),col="red",ylab="",lwd=3)

hist(X, breaks=100, freq=FALSE)
u=seq(length=N, from=-3, to=3)
points(u,sapply(u,p),type="l", col="red",lwd=3)

H=hist(X, breaks="Scott")
(max(X)-min(X))/(0.1*3.5*sd(X))
pretty(X,22)
hist(X, breaks="FD")
