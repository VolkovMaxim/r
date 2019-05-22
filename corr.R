?mtcars 
x=mtcars[,1] 
ks.test(unique(x),"pnorm") 
q=rnorm(100000,mean(x),sd(x)) 
ks.test(unique(x),q) 

plot(density(x)) 

qqnorm(x) 
x 
qqline(x, col="red") 

install.packages("SuppDists") 
library(SuppDists) 
pKendall( -16/36, N=9, lower.tail = T) 

x=seq(-1,1, length=50) 
y=pKendall(x,N=3, lower.tail=TRUE) 
plot(x,y, type="l") 

x=mtcars[,1] 
?mtcars 
y=mtcars[,6] 
plot(x,y) 
rho=cor.test(x,y, method="pearson")$estimate 
rho*sd(y)/sd(x) 
lm(y~x) 

cor.test(x,y, method="kendall") 
M=cbind(x,y) 
duplicated(x) 
M=M[-which(duplicated(x)),] 
M=M[-which(duplicated(M[,2])),] 
cor.test(M[,1],M[,2], method="kendall") 

res=matrix(data=NA, nrow=6, ncol=6) 
M=mtcars[,c(1,3:7)] 
for (i in (1:6)){ 
  for (j in (1:6)){ 
    w=cor.test(M[,i],M[,j],method="kendall") 
    res[i,j]=round(w$estimate,2) 
    if (w$p.value<0.05){ 
      res[i,j]=paste(toString(res[i,j]),'*') 
    } 
    if (w$p.value<0.001){ 
      res[i,j]=paste(toString(res[i,j]),'*') 
    } 
  } 
} 
res 

install.packages("NSM3") 
library(NSM3) 
kendall.ci(x,y,alpha=0.1, type="t", 
           bootstrap=T, B=100)