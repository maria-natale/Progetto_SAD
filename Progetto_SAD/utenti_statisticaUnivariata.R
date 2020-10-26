#prendo i dati relativi alla Campania e la media nazionale degli utenti negli anni
matrixUtenti<-as.matrix(utenti_per_regione_e_anno)
for (i in 1:dim(matrixUtenti)[1]){
  if (matrixUtenti[i,1]=="Campania"){
    utenti_campania<-as.numeric(matrixUtenti[i, 2:dim(matrixUtenti)[2]])
  }
  if (matrixUtenti[i,1]=="Media nazionale"){
    utenti_nazione<-as.numeric(matrixUtenti[i, 2:dim(matrixUtenti)[2]])
  }
}
utenti_nazione<-round(utenti_nazione,0)

#creo barplot per Campania e media nazionale
png("grafici/chiamateEffettuateUtentiCampaniaFrequenza.png")
x<-barplot(utenti_campania, xlab="Anni", ylab="Numero di chiamate effettuate", ylim=c(0,1800), col=1:9,
        names.arg = colnames, main ="Numero di utenti al numero verde in Campania")
text(x, y=utenti_campania, pos = 3, labels = utenti_campania, col="red")
dev.off()

png("grafici/chiamateEffettuateUtentiItaliaFrequenza.png")
x<-barplot(utenti_nazione, xlab="Anni", ylab="Numero di chiamate effettuate", ylim=c(0,max(utenti_nazione)+100), col=1:9,
           names.arg = colnames, main ="Numero di utenti al numero verde in Italia")
text(x, y=utenti_nazione, pos = 3, labels = utenti_nazione, col="red")
dev.off()

#unisco i due boxplot su un unico grafico per confrontarli
png("grafici/barplotUtentiConfrontoItaliaCampania.png")
par(mfrow=c(2,1))
plot(utenti_nazione, col="red", xaxt='n')
lines(utenti_nazione, col="red")
axis(1, at=1:8, labels=colnames)
plot(utenti_campania, col="blue", xaxt='n')
lines(utenti_campania, col="red")
axis(1, at=1:8, labels=colnames)
dev.off()

#calcolo i quantili e la media dei due vettori di dati
summary(utenti_campania)
summary(utenti_nazione)

#confronto i due boxplot
png("grafici/boxplotUtentiConfrontoItaliaCampania.png")
par(mfrow=c(1,2))
boxplot(utenti_nazione, col="red", main="Boxplot utenti in Italia")
boxplot(utenti_campania, col="blue", main="Boxplot utenti in Campania")
dev.off()




