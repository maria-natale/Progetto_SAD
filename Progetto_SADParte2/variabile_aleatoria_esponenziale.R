png("grafici/densitàdistribuzioneEsponenziale.png")
par ( mfrow =c(1 ,2) )
curve ( dexp(x,rate=3) ,from =0, to=10, xlab="x", ylab="f(x)",main=" lambda=3")
curve ( pexp(x,rate=3) ,from =-2, to =10, xlab="x",ylab= expression (P(X<=x)),main=" lambda=3")
dev.off()

png("grafici/rappresentazioneProbEsponenziale.png")
curve ( dexp(x,rate =3) ,from =0, to =3.5 , xlab="x",ylab="f(x)")
x<-seq (0.5 ,1.5 ,0.01)
lines (x, dexp(x,rate =3) ,type="h",col =" grey")
text (1.1 ,0.5 , "P(0.5<X<1.5) ")
dev.off()

prob<-pexp (1.5 ,3) -pexp (0.5 ,3)

z <-c (0 ,0.25 ,0.5 ,0.75 ,1)
qexp(z, rate =3)

png("grafici/densitàEsponenzialeESimulata.png")
par ( mfrow =c(1 ,2))
curve ( dexp(x,rate=3) ,from =0, to=10, xlab="x", ylab="f(x)",main="Densità di probabilità geometrica")
sim<-rexp(5000, rate =3)
hist(sim,freq=F,xlim =c(0 ,8) ,ylim =c(0 ,2) ,breaks =100 , xlab ="x", ylab=" Istogramma ",main=" Densita ’  simulata ,N =5000 ")
dev.off()


