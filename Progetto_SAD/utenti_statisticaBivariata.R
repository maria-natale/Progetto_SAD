library(openxlsx)
utenti_per_regione_e_anno <-read.xlsx("utenti_per_regione_e_anno.xlsx")

df<-round(utenti_per_regione_e_anno[,-1],0)
row.names(df)<-utenti_per_regione_e_anno[,1]

options("scipen"=100, "digits"=4)

#regressione lineare semplice
png("grafici/bivariata/scatterPlotUtenti2020_2019.png")
plot(df$"2019", df$"2020", main="2020 in funzione di 2019", col="blue",
      xlab="2019", ylab="2020")
abline (v=median (df$"2019" ),lty =1, col =" magenta ")
abline (v=mean(df$"2019" ),lty =2, col =" green")
abline (h=median (df$"2020"),lty =1, col =" magenta ")
abline (h=mean(df$"2020"),lty =2, col ="green")
legend (5, 2000,c(" Mediana "," Media"),pch =0, col =c("magenta","green"), cex =0.8)
dev.off()

cov(df$"2019", df$"2020")
cor(df$"2019", df$"2020")

png("grafici/bivariata/scatterPlotUtenti2020_2019RettaRegressione.png")
plot(df$"2019", df$"2020", main="Retta di regressione 2020 in funzione di 2019", col="blue",
     xlab="2019", ylab="2020")
abline(lm(df$"2020"~df$"2019"), col="magenta")
dev.off()

linearmodel<-lm(df$"2020"~df$"2019")
residui<-linearmodel$residuals
residuistandard<-residui/sd(residui)

png("grafici/bivariata/scatterPlotUtenti2020_2019RettaRegressioneResidui.png")
plot(df$"2019", df$"2020", main="Retta di regressione 2020 in funzione di 2019 con residui", col="blue",
     xlab="2019", ylab="2020")
abline(linearmodel, col="magenta")
segments (df$"2019", linearmodel$fitted.values, df$"2019", df$"2020" ,col="green")
dev.off()

png("grafici/bivariata/scatterPlotUtenti2020_2019DiagrammaResidui.png")
plot(df$"2019", residui, main="Diagramma dei residui", xlab="2019", ylab="Residui", col="blue", pch =9)
abline (h=0, col ="magenta",lty=2)
dev.off()


#regressione lineare multipla

apply(df, 2, median)
round(apply(df, 2, mean),2)
round(apply(df, 2, sd),2)

cov(df)
cor(df)

png("grafici/bivariata/scatterPlotUtentiCoppieVariabili.png")
pairs(df, main="Scatterplot per le coppie di variabili", col="blue")
dev.off()

multiplelinearmodel<-lm(df$"2020"~df$"2013" +df$"2014" + df$"2015" + df$"2016" + df$"2017" + df$"2018" + df$"2019")
residui<-multiplelinearmodel$residuals
residuistandard<-residui/sd(residui)
stime<-multiplelinearmodel$fitted.values
png("grafici/bivariata/residuiStandarValoriStimatiMultipleUtenti.png")
plot(stime, residuistandard, main="Residui standardizzati rispetto ai valori stimati", xlab="valori stimati"
     , ylab="Residui standard", pch=5, col="blue")
abline (h=0, col ="magenta",lty =2)
dev.off()


