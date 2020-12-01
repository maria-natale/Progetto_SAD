install.packages("xlsx")
library(xlsx)
campione_esponenziale<-read.xlsx("campioneEsponenziale.xlsx")

#metodo dei momenti
stimatheta <-1.0 /mean (campioneEsponenziale)

