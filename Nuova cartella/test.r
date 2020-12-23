lamba0=1/5
alfa=0.01
qnorm(1-alfa/2,mean=0,sd=1)
n=50
meancap=5.330421
sqrt(n)*(lamba0*meancap-1)

lamba0=1/3.5
alfa=0.01
qnorm(1-alfa,mean=0,sd=1)
n=50
meancap=5.330421
sqrt(n)*(lamba0*meancap-1)

lamba0=1/3.5
alfa=0.01
qnorm(alfa,mean=0,sd=1)
n=50
meancap=5.330421
sqrt(n)*(lamba0*meancap-1)


install.packages("readxl")
library(readxl)



test = read_xlsx("campioneEsponenziale (1).xlsx",sheet = "sheet1")
test=as.matrix(test[,-1])
media=mean(test)
media
a=numeric(4)
for (i in 1:4) {
  a[i]=qexp(0.2*i, rate=1/media)
}
a
r=5
nint=numeric(r)
nint[1]=length(which(test<a[1]))
nint[2]=length(which((test>=a[1])&(test<a[2])))
nint[3]=length(which((test>=a[2])&(test<a[3])))
nint[4]=length(which((test>=a[3])&(test<a[4])))
nint[5]=length(which(test>=a[4]))
nint
sum(nint)
chiquadro=sum(((nint-50*0.2)/sqrt(50*0.2))^2)
chiquadro
#distribuzione esponenziale 1 non noto
k=1
#grado di libertà=3
alfa=0.05
qchisq(alfa/2,df=r-k-1)
#0.2157953
qchisq(1-alfa/2,df=r-k-1)
#9.348404





