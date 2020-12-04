install.packages("xlsx")
library("xlsx")

campioneEsponenziale<-rexp(50, rate =0.2)

write.xlsx(campioneEsponenziale, file = "campioneEsponenziale.xlsx",
           sheetName = "sheet1", append = FALSE)

camp2<-rexp(80, rate=0.1)
write.xlsx(camp2, file = "campioneEsponenziale.xlsx",
           sheetName = "sheet2", append = TRUE)
