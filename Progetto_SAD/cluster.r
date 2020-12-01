library(openxlsx)
Vittime_per_regione_e_anno=read.xlsx("vittime_per_regione_e_anno.xlsx")




z=round(Vittime_per_regione_e_anno[1:nrow(Vittime_per_regione_e_anno)-1,-1],0)
row.names(z)=Vittime_per_regione_e_anno[1:nrow(Vittime_per_regione_e_anno)-1,1]
n=nrow(z)

d=dist(z,method = "euclidean",diag=TRUE,upper=TRUE)


#singolo
hls=hclust(d,method = "single")
plot(hls,hang=-1,xlab = "Agglomerativo singolo",sub=" ")
rect.hclust(hls,k=2,border = "green")
axis(side=4,at=round(c(0,hls$height),2))

#completo
hlc=hclust(d,method = "complete")
plot(hlc,hang=-1,xlab="Agglomerativo completo",sub = " ")
rect.hclust(hlc,k = 2,border = "green")
axis(side=4,at=round(c(0,hls$height),0))

#average
hlm=hclust(d, method="average")
plot(hlm, hang=-1, xlab="Agglomerativo medio legame medio", sub="")
rect.hclust(hlm, k=2, border="green")
axis(side=4, at=round(c(0, hls$height),2))

#centroide
d2=d^2
hc=hclust(d2,method = "centroid")
plot(hc,hang=-1,xlab = "agglomerativo centroide",sub=" ")
rect.hclust(hc,k=2,border = "green")
axis(side=4,at=round(c(0,hls$height),2))

# mediana
hmed=hclust(d2,method = "median")
plot(hmed,hang=-1,xlab = "agglomerativo mediana",sub=" ")
rect.hclust(hmed,k=2,border = "green")
axis(side=4,at=round(c(0,hls$height),2))

# non omogeneità
n=nrow(z)
trh=(n-1)*sum(apply(z,2,var))



legamesingolo=list()
kval=1:n
for (i in kval){
  taglio=cutree(hlc,i)
  num=table(taglio)
  tagliolist=list(taglio)
  agvar=aggregate(z,tagliolist,var)[,-1]
  trlist=list()
  for(cluster in 1:i){
    if(num[[cluster]]>1)
      trlist[cluster]=(num[[cluster]]-1)*sum(agvar[cluster, ])
    else
      trlist[cluster]=0
  }
  trb=trh-Reduce("+",trlist)
  legamesingolo[i]=trb/trh
}


#legame singolo
taglio=cutree(hls,k=2)
num=table(taglio)
tagliolist=list(taglio)
agvar=aggregate(z,tagliolist,var)[,-1]
trh1=(num[[1]]-1)*sum(agvar[1, ])
trh2=(num[[2]]-1)*sum(agvar[2, ])
trb=trh-trh1-trh2 #misura cluster non omogeeni
rapportolsingolo=trb/trh


#legame completo
taglio=cutree(hlc, k=2)
num =table (taglio)
tagliolist=list(taglio)
agvar = aggregate (z, tagliolist , var)[, -1]
trh1 =(num [[1]]-1) * sum (agvar [1, ])
trh2 =(num [[2]]-1) * sum (agvar [2, ])
trb=trh-trh1-trh2 #misura non omogeneità cluster
rapportoLegameCompleto=trb/trh

#legame medio
taglio=cutree(hlm, k=2)
num =table (taglio)
tagliolist=list(taglio)
agvar = aggregate (z, tagliolist , var)[, -1]
trh1 =(num [[1]]-1) * sum (agvar [1, ])
trh2 =(num [[2]]-1) * sum (agvar [2, ])
trb=trh-trh1-trh2#misura non omogeneità cluster
rapportoLegameMedio=trb/trh

#metodo centroide
taglio=cutree(hc, k=2)
num =table (taglio)
tagliolist=list(taglio)
agvar = aggregate (z, tagliolist , var)[, -1]
trh1 =(num [[1]]-1) * sum (agvar [1, ])
trh2 =(num [[2]]-1) * sum (agvar [2, ])
trb=trh-trh1-trh2 #misura di non omogeneità tra i cluster
rapportoCentroide=trb/trh
centroidiIniziali =aggregate (z, tagliolist , mean )[,-1]

#metodo mediana
taglio=cutree(hmed, k=2)
num =table (taglio)
tagliolist=list(taglio)
agvar = aggregate (z, tagliolist , var)[, -1]
trh1 =(num [[1]]-1) * sum (agvar [1, ])
trh2 =(num [[2]]-1) * sum (agvar [2, ])
trb=trh-trh1-trh2 #misura di non omogeneità tra i cluster
rapportoMediana=trb/trh

#metodo kmeans
km =kmeans (z, centers=2, iter.max =10, nstart =1)
rapportoKMeans=km$betweenss/km$totss









