v=c(2013,2014,2015,2016,2017,2018,2019,2020)
ff=barplot(utenti, names.arg=v, col = c("green","red","yellow","black","blue","brown","azure","gray"),main="Vittime in Campania", xlab = "vittime per anni",ylim = c(0,900))
text(ff, y=utenti, pos = 3, labels = utenti, col="red")

v=c(2013,2014,2015,2016,2017,2018,2019,2020)
ff=barplot(mediavittimeitalia, names.arg=v, col = c("green","red","yellow","black","blue","brown","azure","gray"),main="Vittime in Italia", xlab = "vittime per anni",ylim = c(0,300))
text(ff, y=round(mediavittimeitalia), pos = 3, labels = round(mediavittimeitalia), col="red")
round


tipiquartili=function (x){
  y = numeric (0)
  for (i in 1:9) {
    y <- rbind (y ,c( quantile (x ,0 , type = i) , quantile (x ,0.25 , type =i),
                      quantile (x ,0.5 , type =i) , quantile (x ,0.75 , type =i ) 
                      ,quantile (x ,1 , type =i ))) }
  rownames (y )=paste ( " type " ,1:9)
  y}
media=tipiquartili(mediavittimeitalia)
campania=tipiquartili(utenti)
print(media)
print(campania)

var(utenti)
sd(utenti)
coefficienteVariazioneCampania
var(mediavittimeitalia)
sd(mediavittimeitalia)
coefficienteVariazioneItalia




matrixNumeric2020<-round(as.matrix(Vittime_per_regione_e_anno[,dim(Vittime_per_regione_e_anno)[2]]),0)
matrixNumeric2019<-round(as.matrix(Vittime_per_regione_e_anno[,dim(Vittime_per_regione_e_anno)[2]-1]),0)
matrix<-cbind(matrixNumeric2019, matrixNumeric2020)
colnames(matrix)<-c("2019", "2020")
rownames(matrix)<-Vittime_per_regione_e_anno[1:nrow(Vittime_per_regione_e_anno),1]
matrix1<-matrix[1:round(nrow(matrix)/3,0),]
matrix2<-matrix[(round(nrow(matrix)/3,0)+1):round(nrow(matrix)/3*2,0),]
matrix3<-matrix[(round(nrow(matrix)/3*2,0)+1):nrow(matrix),]

x<-barplot(t(matrix1), beside=TRUE, main="Frequenze assolute congiunte 2019 e 2020", col=c("green", "blue"))
x<-barplot(t(matrix2), beside=TRUE, main="Frequenze assolute congiunte 2019 e 2020", col=c("green", "blue"))
x<-barplot(t(matrix3), beside=TRUE, main="Frequenze assolute congiunte 2019 e 2020", col=c("green", "blue"))





tableCamp<-table(c(rep("2013", utenti[1]), rep("2014",utenti[2]), rep("2015",utenti[3]),
                   rep("2016",utenti[4]), rep("2017",utenti[5]), rep("2018",utenti[6]),
                   rep("2019",utenti[7]), rep("2020",utenti[8])))
ord<-sort(tableCamp, decreasing = TRUE)
propOrd <- prop.table (ord)
x <- barplot (propOrd , ylim = c(0, 1.05) , main = "Diagramma di Pareto Campania", col =1:8 , las =2)
lines (x, cumsum ( propOrd ), type = "b", pch = 16)
text(x - 0.2, cumsum (propOrd) + 0.03 , paste (format ( cumsum ( propOrd
) * 100, digits = 2) , "%"))




tableNaz<-table(c(rep("2013", mediavittimeitalia[1]), rep("2014",mediavittimeitalia[2]), rep("2015",mediavittimeitalia[3]),
                  rep("2016",mediavittimeitalia[4]), rep("2017",mediavittimeitalia[5]), rep("2018",mediavittimeitalia[6]),
                  rep("2019",mediavittimeitalia[7]), rep("2020",mediavittimeitalia[8])))
ord<-sort(tableNaz, decreasing = TRUE)
propOrd <- prop.table (ord)
x <- barplot (propOrd , ylim = c(0, 1.05) , main = "Diagramma di Pareto Italia", col =1:8 , las =2)
lines (x, cumsum ( propOrd ), type = "b", pch = 16)
text(x - 0.2, cumsum (propOrd) + 0.03 , paste (format ( cumsum ( propOrd
) * 100, digits = 2) , "%"))


mediavittimeitalia

mean(utenti)
mean(mediavittimeitalia)









summary(utenti)
311.8-1.5* (489-311.8 )
311.8+1.5* (489-311.8 )
summary(mediavittimeitalia)
121.23-1.5*(190.48-121.23)
121.23+1.5*(190.48-121.23)








