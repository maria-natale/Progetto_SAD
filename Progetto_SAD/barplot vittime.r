utenti_campani=(Vittime_per_regione_e_anno[17,2:9])
utenti=as.vector(t(utenti_campani))
hist(utenti, breaks = 4, col = "red", main = "vittime della campania",xlim=c(219,738),ylab= "valori")
v=c(2013,2014,2015,2016,2017,2018,2019,2020)
barplot(utenti,width = 3,space = 2, names.arg=v, col = c("green","red","yellow","black","blue","brown","azure","gray"),xlab = "vittime per anni")

barplot(mediavittimeitalia,width = 3,space = 2, names.arg=v, col = c("green","red","yellow","black","blue","brown","azure","gray"),xlab = "vittime per anni"))