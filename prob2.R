faithful 
x=faithful$eruptions 
x 
plot(density(x)) 
density(x)$bw 
band=c("nrd0", "nrd", "ucv", "bcv", "SJ") 
for (j in (2:5)){ 
  lines(density(x, bw=band[j]),col=j) 
  print(density(x, bw=band[j])$bw) 
} 

N=1000 
xi=sample(0:5,size=N,replace=TRUE, 
          prob = c(rep(0.1,5),0.5)) 
table(xi) 
m=c(-1,-1/2,0,1/2,1,0) 
s=c(rep(0.1,5),1) 
X=rep(0,N) 
for (j in (1:N)){ 
  X[j]=rnorm(1, mean=m[xi[j]+1], 
             sd=s[xi[j]+1]) 
} 
K=eval(formals(density.default)$kernel) 
plot(density(X)) 
for (j in (2:5)){ 
  #lines(density(X, kernel=K[j]), col=j) 
  lines(density(X, bw=band[j]),col=j) 
} 

p=function(x){ 
  y=dnorm(x)/2 
  for (j in (0:4)){ 
    y=y+dnorm(x,mean=m[j+1],sd=0.1)/10 
  } 
  return(y) 
} 

J=function(d){ 
  R=0 
  for (i in (1:length(d$x))){ 
    R=R+(p(d$x[i])-d$y[i])^2 
  } 
  return(R) 
} 

M=matrix(data=NA, nrow=7, ncol=5) 
for (j in (1:7)){ 
  for (k in (1:5)){ 
    M[j,k]=J(density(X, kernel=K[j], bw=band[k])) 
  } 
} 
which.min(M) 
M 
j=6 
k=3 
plot(density(X, kernel=K[j], bw=band[k])) 
density(X, kernel=K[j], bw=band[k])$bw 

vec=seq(from=0.0001, to=0.2, length=100) 
M2=rep(0,100) 
for (j in (1:100)){ 
  M2[j]=J(density(X, bw=vec[j])) 
} 
which.min(M2) 
plot(density(X, bw=vec[26]))
