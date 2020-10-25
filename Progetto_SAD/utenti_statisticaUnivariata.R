utenti_campania<-(utenti_per_regione_e_anno[17,2:9])
utenti<-as.vector(t(utenti_campania))
barplot(utenti, xlab="Frequenza delle chiamate al numero verde per anno", col=1:9,
        names.arg = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"))               