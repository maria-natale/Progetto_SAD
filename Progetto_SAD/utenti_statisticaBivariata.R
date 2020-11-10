df<-utenti_per_regione_e_anno[,-1]
apply(df, 2, median)
round(apply(df, 2, mean),2)
round(apply(df, 2, sd),2)

cov(df)
cor(df)

png("grafici/bivariata/chiamateEffettuateUtentiCampaniaFrequenza.png")
pairs(df, main="Scatterplot per le coppie di variabili", col="blue")
dev.off()

multiplelinearmodel<-lm(df$"2020"~df$"2013" +df$"2014" + df$"2015" + df$"2016" + df$"2017" + df$"2018" + df$"2019")