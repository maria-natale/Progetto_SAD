matrixUtenti<-as.matrix(utenti_per_regione_e_anno)
for (i in 1:dim(matrixUtenti)[1]){
  if (matrixUtenti[i,1]=="Campania"){
    utenti_campania<-as.numeric(matrixUtenti[i, 2:dim(matrixUtenti)[2]])
  }
  if (matrixUtenti[i,1]=="Lombardia"){
    utenti_lombardia<-as.numeric(matrixUtenti[i, 2:dim(matrixUtenti)[2]])
  }
}
png("grafici/chiamateEffettuateUtentiCampaniaFrequenza.png")
x<-barplot(utenti_campania, xlab="Anni", ylab="Numero di chiamate effettuate", ylim=c(0,1800), col=1:9,
        names.arg = colnames, main ="Numero di utenti al numero verde nel periodo marzo-giugno in Campania")
text(x, y=utenti_campania, pos = 3, labels = utenti_campania, col="red")
dev.off()

png("grafici/chiamateEffettuateUtentiLombardiaFrequenza.png")
x<-barplot(utenti_lombardia, xlab="Anni", ylab="Numero di chiamate effettuate", ylim=c(0,2400), col=1:9,
           names.arg = colnames, main ="Numero di utenti al numero verde nel periodo marzo-giugno in Lombardia")
text(x, y=utenti_lombardia, pos = 3, labels = utenti_lombardia, col="red")
dev.off()


cumsum(table(utenti_campania)/length(utenti_campania))
png("grafici/funzioneDiDistrbuzioneEmpiricaDiscretaCampania.png")
plot(ecdf(utenti_campania), main="Funzione di distribuzione empirica discreta", 
     verticals=FALSE, col="blue")
dev.off()
summary(utenti_campania)