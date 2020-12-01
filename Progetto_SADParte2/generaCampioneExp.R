install.packages("xlsx")
library("xlsx")

campioneEsponenziale<-rexp(50, rate =3)

write.xlsx(campioneEsponenziale, file = "campioneEsponenziale.xlsx",
           sheetName = "sheet1", append = FALSE)


