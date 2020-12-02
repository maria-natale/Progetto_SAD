par ( mfrow =c (2 ,2) )
y=0:5
plot (y , dgeom (y , prob =0.95),xlab = "y" , ylab = "P( Y=y )" , type ="h" ,main = "p =0.95")

y <- 0:10
plot (y , dgeom (y , prob =0.05) ,
xlab = "y" , ylab = "P( Y=y )" , type ="h" ,
main = "p =0.05 ")

y <- 0:20
plot (y , dgeom (y , prob =0.5) ,
xlab = "y" , ylab = "P( Y=y )" , type ="h" ,main = "p =0.5 ")

y <- 0:20
plot (y , dgeom (y , prob =0.2) ,
xlab = "y" , ylab = "P( Y=y )" , type ="h" ,
main = "p =0.2 ")

par ( mfrow =c (2 ,2) )
y <- 0:5
plot (y , pgeom (y , prob =0.95) ,
         xlab = "y" , ylab = expression (P (Y <= y) ) , ylim =c (0 ,1) , type="s" ,
         main = "p =0.95 ")
y <- 0:50
plot (y , pgeom (y , prob =0.05) ,
        xlab = "y" , ylab = expression (P (Y <= y) ) , ylim =c (0 ,1) , type="s" ,
        main = "p =0.05 ")
y <- 0:20
plot (y , pgeom (y , prob =0.5) ,
        xlab = "y" , ylab = expression (P (Y <= y) ) , ylim =c (0 ,1) , type="s" ,
        main = "p =0.5 ")
y <- 0:20
plot (y , pgeom (y , prob =0.2) ,
        xlab = "y" , ylab = expression (P (Y <= y) ) , ylim =c (0 ,1) , type="s" ,
        main = "p =0.2 ")



z=c(0 ,0.25 ,0.5 ,0.75 ,1)
qgeom (z , prob =0.2)


######################
install.packages("xlsx")
library("xlsx")

campionegeometrico=rgeom(50, prob = 0.3)
write.xlsx(campionegeometrico, file = "campionegeometrico.xlsx",
           sheetName = "sheet1", append = FALSE)


#popolazionegeometricametodomomenti
stimap=1/(1+mean(campionegeometrico))
stimap


