#prendo i dati relativi alla Campania e la media nazionale degli utenti negli anni
install.packages("xlsx")
library(xlsx)
utenti_per_regione_e_anno <-read.xlsx("utenti_per_regione_e_anno.xlsx")

round(utenti_per_regione_e_anno[,-1],0)
for (i in 1:dim(utenti_per_regione_e_anno)[1]){
  if (utenti_per_regione_e_anno[i,1]=="Campania"){
    utenti_campania<-as.numeric(utenti_per_regione_e_anno[i, 2:dim(matrixUtenti)[2]])
  }
  if (utenti_per_regione_e_anno[i,1]=="Media nazionale"){
    utenti_nazione<-as.numeric(utenti_per_regione_e_anno[i, 2:dim(matrixUtenti)[2]])
  }
}

matrixNumeric<-as.matrix(round(utenti_per_regione_e_anno[,2:dim(utenti_per_regione_e_anno)[2]]),0)
colnames(matrixNumeric)<-c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
rownames(matrixNumeric)<-utenti_per_regione_e_anno[1:nrow(utenti_per_regione_e_anno),1]
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

#diagramma di Pareto
png("grafici/paretoUtentiCampania.png")
tableCamp<-table(c(rep("2013", utenti_campania[1]), rep("2014",utenti_campania[2]), rep("2015",utenti_campania[3]),
             rep("2016",utenti_campania[4]), rep("2017",utenti_campania[5]), rep("2018",utenti_campania[6]),
             rep("2019",utenti_campania[7]), rep("2020",utenti_campania[8])))
#ord<-sort(t(matrixNumeric[17, ]), decreasing=TRUE)
ord<-sort(tableCamp, decreasing = TRUE)
propOrd <- prop.table (ord)
x <- barplot (propOrd , ylim = c(0, 1.05) , main = "Diagramma di Pareto Campania", col =1:8 , las =2)
lines (x, cumsum ( propOrd ), type = "b", pch = 16)
text(x - 0.2, cumsum (propOrd) + 0.03 , paste (format ( cumsum ( propOrd
) * 100, digits = 2) , "%"))
dev.off()

png("grafici/paretoUtentiItalia.png")
tableNaz<-table(c(rep("2013", utenti_nazione[1]), rep("2014",utenti_nazione[2]), rep("2015",utenti_nazione[3]),
                   rep("2016",utenti_nazione[4]), rep("2017",utenti_nazione[5]), rep("2018",utenti_nazione[6]),
                   rep("2019",utenti_nazione[7]), rep("2020",utenti_nazione[8])))
#ord<-sort(t(matrixNumeric[17, ]), decreasing=TRUE)
ord<-sort(tableNaz, decreasing = TRUE)
propOrd <- prop.table (ord)
x <- barplot (propOrd , ylim = c(0, 1.05) , main = "Diagramma di Pareto Italia", col =1:8 , las =2)
lines (x, cumsum ( propOrd ), type = "b", pch = 16)
text(x - 0.2, cumsum (propOrd) + 0.03 , paste (format ( cumsum ( propOrd
) * 100, digits = 2) , "%"))
dev.off()

#funzione di distribuzione continua 
minOsservazione = min(utenti_campania)
maxOsservazione = max(utenti_campania)
frequenza<-table(utenti_campania)/length(utenti_campania)
lung<-length(frequenza)
classe<-round((maxOsservazione-minOsservazione)/3, digits=0)
classi<-c(minOsservazione, minOsservazione+classe, minOsservazione+2*classe, maxOsservazione)
frelclassi <-table (cut (utenti_campania, breaks = classi,right = FALSE ))/ length (utenti_campania)
Fcum <-cumsum (frelclassi)
Fcum[3]<-Fcum[3]+frequenza[lung]

png("grafici/funzionedidistribuzioneCampania.png")
ascisse<-c(0, classi, maxOsservazione+100)
ordinate <-c(0, 0, Fcum [1:3] ,1)
plot(ascisse , ordinate , type = "b", axes = FALSE , main = "
Funzione di distribuzione empirica continua Campania", col =" blue ",ylim=c(0 ,1) ,xlab="x",ylab="F(x)")
axis (1, format(ascisse, digits=2))
axis (2, format(Fcum, digits=2))
box()
dev.off()


minOsservazione = min(utenti_nazione)
maxOsservazione = max(utenti_nazione)
frequenza<-table(utenti_nazione)/length(utenti_nazione)
lung<-length(frequenza)
classe<-round((maxOsservazione-minOsservazione)/3, digits=0)
classi<-c(minOsservazione, minOsservazione+classe, minOsservazione+2*classe, maxOsservazione)
frelclassiItalia <-table (cut (utenti_nazione, breaks = classi,right = FALSE ))/ length (utenti_nazione)
FcumI <-cumsum (frelclassiItalia)
FcumI[3]<-FcumI[3]+frequenza[lung]
num<-0

png("grafici/funzionedidistribuzioneItalia.png")
ascisse<-c(0, classi, maxOsservazione+100)
ordinate <-c(0, 0, FcumI [1:3] ,1)
plot(ascisse , ordinate , type = "b", axes = FALSE , main = "
Funzione di distribuzione empirica continua Italia", col =" red ",ylim=c(0 ,1) ,xlab="x",ylab="F(x)")
axis (1, format(ascisse, digits=2))
axis (2, format(FcumI, digits=2))
box()
dev.off()




#unisco i due barplot su un unico grafico per confrontarli
png("grafici/plotUtentiConfrontoItaliaCampania.png")
par(mfrow=c(2,1))
plot(utenti_nazione, col="red", xaxt='n')
lines(utenti_nazione, col="red")
axis(1, at=1:8, labels=colnames)
plot(utenti_campania, col="blue", xaxt='n')
lines(utenti_campania, col="blue")
axis(1, at=1:8, labels=colnames)
dev.off()

#calcolo i quantili e la media dei due vettori di dati
summary(utenti_campania)
summary(utenti_nazione)

#confronto istogrammi e ricavo la moda
classi<-c(0, 500, 1000, 1500, 2000, 2500)
fclassiCampania <-table (cut (utenti_campania, breaks = classi,right = FALSE, dig.lab = 10))
for (i in 1:length(utenti_campania)){
  if(utenti_campania[i]==2500)
    fclassiCampania[3]<-fclassiCampania[3]+1
}
fclassiItalia <-table (cut (utenti_nazione, breaks = classi,right = FALSE, dig.lab=10))
for (i in 1:length(utenti_nazione)){
  if(utenti_nazione[i]==2500)
    fclassiItalia[3]<-fclassiItalia[3]+1
}

png("grafici/istogrammaClassiCampania.png")
hist(utenti_campania, breaks=classi, col=rainbow(3), main="Istogramma delle frequenze delle classi in Campania")
dev.off()
png("grafici/istogrammaClassiItalia.png")
hist(utenti_nazione, breaks=classi, col=rainbow(3), main="Istogramma delle frequenze delle classi in Italia")
dev.off()



#confronto i due boxplot
png("grafici/boxplotUtentiConfrontoItaliaCampania.png")
par(mfrow=c(1,2))
boxplot(utenti_nazione, col="red", main="Boxplot utenti in Italia")
boxplot(utenti_campania, col="blue", main="Boxplot utenti in Campania")
dev.off()


#quantili con i differenti algoritmi di R
tipiquartili=function (x){
  y = numeric (0)
  for (i in 1:9) {
    y <- rbind (y ,c( quantile (x ,0 , type = i) , quantile (x ,0.25 , type =i),
                      quantile (x ,0.5 , type =i) , quantile (x ,0.75 , type =i ) 
                      ,quantile (x ,1 , type =i ))) }
  rownames (y)=paste ( " type " ,1:9)
  y}
quartiliCampania<-tipiquartili(utenti_campania)
quartiliItalia<-tipiquartili(utenti_nazione)

#indici di dispersione
var(utenti_campania)
sd(utenti_campania)
coefficienteVariazioneCampania<-sd(utenti_campania)/abs(mean(utenti_campania))
coefficienteVariazioneCampania
var(utenti_nazione)
sd(utenti_nazione)
coefficienteVariazioneItalia<-sd(utenti_nazione)/abs(mean(utenti_nazione))
coefficienteVariazioneItalia



skw <-function (x){
  n<-length (x)
  m2 <-(n -1) *var (x)/n
  m3 <- (sum ( (x- mean(x))^3) )/n
  m3/(m2 ^1.5)
}

curt <-function (x){
  n <-length (x)
  m2 <-(n -1) *var (x)/n
  m4 <- (sum ((x-mean(x))^4) )/n
  m4/(m2 ^2) -3
}

skw(utenti_campania)
skw(utenti_nazione)

curt(utenti_campania)
curt(utenti_nazione)

matrixNumeric2020<-round(as.matrix(utenti_per_regione_e_anno[,dim(utenti_per_regione_e_anno)[2]]),0)
matrixNumeric2019<-round(as.matrix(utenti_per_regione_e_anno[,dim(utenti_per_regione_e_anno)[2]-1]),0)
matrix<-cbind(matrixNumeric2019, matrixNumeric2020)
colnames(matrix)<-c("2019", "2020")
rownames(matrix)<-utenti_per_regione_e_anno[1:nrow(utenti_per_regione_e_anno),1]
matrix1<-matrix[1:round(nrow(matrix)/3,0),]
matrix2<-matrix[(round(nrow(matrix)/3,0)+1):round(nrow(matrix)/3*2,0),]
matrix3<-matrix[(round(nrow(matrix)/3*2,0)+1):nrow(matrix),]
png("grafici/confrontoRegioni20192020.png", width=1000)
x<-barplot(t(matrix1), beside=TRUE, main="Frequenze assolute congiunte 2019 e 2020", col=c("green", "blue"))
dev.off()
png("grafici/confrontoRegioni20192020_2.png", width=1000)
x<-barplot(t(matrix2), beside=TRUE, main="Frequenze assolute congiunte 2019 e 2020", col=c("green", "blue"))
dev.off()
png("grafici/confrontoRegioni20192020_3.png", width=1000)
x<-barplot(t(matrix3), beside=TRUE, main="Frequenze assolute congiunte 2019 e 2020", col=c("green", "blue"))
dev.off()

