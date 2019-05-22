?mtcars 
x=mtcars[,1] 
y=mtcars[,6] 
cor.test(x,y, method="spearman",exact=NULL) 
cor.test(rank(x),rank(y), method="pearson") 
M=cbind(x,y) 
M=M[-which(duplicated(x)),] 
M=M[-which(duplicated(M[,2])),] 
cor.test(M[,1],M[,2], 
         method="spearman",exact=NULL) 

install.packages("pspearman") 
library(pspearman) 
spearman.test(M[,1],M[,2], 
              approximation = "exact") 
spearman.test(M[,1],M[,2], 
              approximation = "t-distribution") 
spearman.test(M[,1],M[,2], 
              approximation = "AS89") 

N=10 
f=function(u){ 
  pspearman(u,N,approximation="exact") 
} 
maxval=(N^3-N)/3 
int=seq(0, maxval, length=100) 
plot(int, sapply(int, f), type="l") 

ldeaths 
mdeaths 
fdeaths 

ldeaths-mdeaths-fdeaths 

matrm=matrix(data=mdeaths, ncol=6, nrow=12) 
matrf=matrix(data=fdeaths, ncol=6, nrow=12) 
m=colSums(matrm) 
f=colSums(matrf) 
plot(m,f, type="b", xlim=c(15500, max(m)+500)) 
text(m,f,labels=1974:1979, pos=2) 

cor.test(m,f, method="pearson") 
cor.test(m,f, method="kendall") 
cor.test(m,f, method="spearman") 

airquality 
?airquality 
head(airquality) 
x=airquality[,1] 
names=c("Pearson", "Kendall", "Spearman") 
res=rep(0,3) 
for (i in (2:4)){ 
  y=airquality[,i] 
  c1=cor.test(x,y, method="pearson") 
  c2=cor.test(x,y, method="kendall") 
  c3=cor.test(x,y, method="spearman") 
  vec1=c(c1$p.value, c2$p.value, c3$p.value) 
  vec2=c(c1$estimate, c2$estimate, c3$estimate) 
  ind=which.max(vec1) 
  res[i-1]=paste(names[ind], 
                 toString(round(vec2[ind],2)), 
                 toString(round(vec1[ind],2))) 
} 
res 
