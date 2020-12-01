tipiquartili=function (x){
y = numeric (0)
for (i in 1:9) {
y <- rbind (y ,c( quantile (x ,0 , type = i) , quantile (x ,0.25 , type =i),
                  quantile (x ,0.5 , type =i) , quantile (x ,0.75 , type =i ) 
                  ,quantile (x ,1 , type =i ))) }
rownames (y )=paste ( " type " ,1:9)
y}
media=tipiquartili(mediavittimeitalia)
campania=tipiquartili(utenti)
print(media)
print(campania)