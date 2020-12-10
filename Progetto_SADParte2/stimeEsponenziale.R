#install.packages("xlsx")
#library(xlsx)
#campione_esponenziale<-read.xlsx("campioneEsponenziale.xslx", sheetName="sheet1")
install.packages("readxl")
library(readxl)
campione_esponenziale <- read_xlsx("campioneEsponenziale.xlsx",sheet = "sheet1")
camp<-as.matrix(campione_esponenziale[,-1])
campione_esponenziale2<- read_excel("campioneEsponenziale.xlsx",sheet = "sheet2")
camp2<-as.matrix(campione_esponenziale2[,-1])

#metodo dei momenti
stimatheta <-1.0 /mean (camp)

#intervalli di confidenza
alpha <-1 -0.99
n<-length(camp)
cb<-stimatheta/(1+ qnorm (1- alpha /2,mean =0, sd =1) / sqrt(n))
ca<-stimatheta/(1-qnorm (1- alpha /2,mean =0, sd =1) / sqrt(n))

alpha <-1 -0.95
cb<-stimatheta/(1+ qnorm (1- alpha /2,mean =0, sd =1) / sqrt(n))
ca<-stimatheta/(1-qnorm (1- alpha /2,mean =0, sd =1) / sqrt(n))

alpha <-1 -0.99
n2<-length(camp2)
n<-lenght(camp)
media1<-mean(camp)
media2<-mean(camp2)
rad<-sqrt(1/(media1^2*n)+1/(media2^2*n2))
cb<-media1-media2-qnorm (1-alpha /2, mean =0, sd =1)*rad
ca<-media1-media2+qnorm (1-alpha /2, mean =0, sd =1)*rad

