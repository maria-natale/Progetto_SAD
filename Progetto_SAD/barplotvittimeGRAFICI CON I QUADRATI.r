par(mfrow=c(1,2))
boxplot (mediavittimeitalia, col="red", main="Boxplot utenti in Italia")
boxplot(utenti, col="blue", main="Boxplot utenti in Campania")

summary(utenti)
summary(mediavittimeitalia)

"Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
219.0   311.8   354.5   418.1   489.0   738.0 
> summary(mediavittimeitalia)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
98.77  121.23  133.80  161.27  190.48  262.41 
>" 