##2020 in funzione 2019 
dataframe=round(Vittime_per_regione_e_anno[,-1],0)
row.names(dataframe)=Vittime_per_regione_e_anno[,1]
options("scipen"=100, "digits"=4)
plot(dataframe$"2019",dataframe$"2020",main = "vittime del 2020 in funzione del 2019",col="blue"
     ,xlab = "2019",ylab = "2020")
abline(v=median(dataframe$"2019"),lty=1,col= "magenta")
abline(v=mean(dataframe$"2019"),lty=2,col="green")
abline(h=median(dataframe$"2020"),lty=1,col= "magenta")
abline(h=mean(dataframe$"2020"),lty=2,col="green")
legend (5, 1000,c(" Mediana "," Media"),pch =0, col =c("magenta","green"), cex =0.8)
##median mean sd
median(dataframe$"2019")
mean(dataframe$"2019")
sd(dataframe$"2019")
print("cambio anno")
median(dataframe$"2020")
mean(dataframe$"2020")
sd(dataframe$"2020")

#covarianza , correlazione campionaria
cov(dataframe$"2019", dataframe$"2020")
cor(dataframe$"2019",dataframe$"2020")

#retta di regressione
plot(dataframe$"2019",dataframe$"2020",main = "retta di regrssione delle vittime del 2020 in funzione del 2019",col="blue"
     ,xlab = "2019",ylab = "2020")
abline(lm(dataframe$"2020"~dataframe$"2019"),col="magenta")

##residui
linearmodel=lm(dataframe$"2020"~dataframe$"2019")
linearmodel
linearmodel$fitted.values
round(linearmodel$residuals,3)
residui=linearmodel$residuals
residuistandard=residui/sd(residui)
residuistandard
plot(dataframe$"2019",dataframe$"2020",main = "retta di regrssione delle vittime del 2020 in funzione del 2019 con residui",col="blue"
     ,xlab = "2019",ylab = "2020")
abline(lm(dataframe$"2020"~dataframe$"2019"),col="magenta")
segments(dataframe$"2019",linearmodel$fitted.values, dataframe$"2019",dataframe$"2020",col = "green")


plot(dataframe$"2019",residui,main="diagramma residui", xlab="2019",ylab = "residui", col="blue", pch=9)
abline(h=0,col="magenta",lty=2)

##regressione lineare
print(lm(dataframe$"2020"~dataframe$"2019"))
print(linearmodel$fitted.values)
print(round(linearmodel$residuals,3))
print(round(residuistandard,3))
print(median(linearmodel$residuals))
print(var(linearmodel$residuals))
print(sd(linearmodel$residuals))
print(summary(linearmodel)$r.square)


#regressione lineare multipla
print(apply(dataframe, 2, median))
print(round(apply(dataframe, 2, mean),2))
print(round(apply(dataframe, 2, sd),2))

round(cov(dataframe),2)
round(cor(dataframe),2)


#scatterplot per le coppie variabili
pairs(dataframe,main="scatter plot per le coppie variabili",col="blue")


modellomultiplo=lm(dataframe$"2020"~dataframe$"2013" +dataframe$"2014" +dataframe$"2015" +dataframe$"2016" +dataframe$"2017"
                   +dataframe$"2018" +dataframe$"2019")
modellomultiplo

residuiM=modellomultiplo$residuals
residuistandardM=residuiM/sd(residuiM)
residuistandardM
stime=modellomultiplo$fitted.values
stime
round(residuiM,3)
plot(stime,residuistandardM,main="Residui standardizzati rispetto ai valori stimati per le vittime",xlab = "valori stimati",ylab="residui stimati",pch=5,col="blue")
abline(h=0,col="magenta",lty=2)
summary(modellomultiplo)$r.square

