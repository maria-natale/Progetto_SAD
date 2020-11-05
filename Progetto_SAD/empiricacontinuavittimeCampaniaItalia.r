minContinua=min((Vittime_per_regione_e_anno[,2:dim(Vittime_per_regione_e_anno)[2]]))
maxContinua=max(as.matrix(Vittime_per_regione_e_anno[,2:dim(Vittime_per_regione_e_anno)[2]]))

classe=round((maxContinua-minContinua)/3,digits = 0)
classi=c(minContinua,minContinua+classe,minContinua+classe*2,maxContinua)
frelclassi= table(cut(utenti,breaks=classi,right = FALSE))/length(utenti)
fcum=cumsum(frelclassi)
num<-0;
for (i in 1:length(utenti)){
  if(utenti[i]==maxContinua)
    num<-num+1
}
fcum [3]= fcum[3]+ num/length(utenti)
x=c(0,classi,maxContinua+classe)
y=c(0,0, fcum,1)
plot(x,y, type = "b", axes = FALSE , main = "
Funzione di distribuzione empirica continua delle vittime in Campania", col =" blue ",ylim=c(0 ,1) ,xlab="x",ylab="F(x)")
axis (1, format(x, digits=2))
axis (2, format(c(0.625,0.75,1), digits=2))
box()
#italia

frelclassiITA=table(cut(mediavittimeitalia, breaks = classi,right = FALSE))/length(mediavittimeitalia)
FcumI=cumsum(frelclassiITA)
for (i in 1:length(mediavittimeitalia)){
  if(mediavittimeitalia[i]==maxContinua)
    num<-num+1
}
if (num>0){
  FcumI [3] <-FcumI [3]+ num/length(utenti_nazione)
}
png("grafici/funzionedidistribuzioneItalia.png")
ascisse<-c(0, classi, maxContinua+classe)
ordinate <-c(0, 0, FcumI [1:3] ,1)
plot(ascisse , ordinate , type = "b", axes = FALSE , main = "
Funzione di distribuzione empirica continua Italia", col =" red ",ylim=c(0 ,1) ,xlab="x",ylab="F(x)")
axis (1, format(ascisse, digits=2))
axis (2, format(FcumI, digits=2))
box()


