z=round(Vittime_per_regione_e_anno[1:nrow(Vittime_per_regione_e_anno),-1],0)
row.names(z)=Vittime_per_regione_e_anno[1:nrow(Vittime_per_regione_e_anno),1]
n=nrow(z)

z=scale(z)
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
trh


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
trh1
trh2=(num[[2]]-1)*sum(agvar[2, ])
trh2
trb=trh-trh1-trh2 #misura cluster non omogeeni
trb
rapportolsingolo=trb/trh
rapportolsingolo


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
trh1
trh2 =(num [[2]]-1) * sum (agvar [2, ])
trh2
trb=trh-trh1-trh2 #misura di non omogeneità tra i cluster
trb
rapportoCentroide=trb/trh
rapportoCentroide
centroidiIniziali =aggregate (z, tagliolist , mean )[,-1]

#metodo mediana
taglio=cutree(hmed, k=2)
num =table (taglio)
tagliolist=list(taglio)
agvar = aggregate (z, tagliolist , var)[, -1]
trh1 =(num [[1]]-1) * sum (agvar [1, ])
trH1
trh2 =(num [[2]]-1) * sum (agvar [2, ])
trb=trh-trh1-trh2 #misura di non omogeneità tra i cluster
trb
rapportoMediana=trb/trh

#metodo kmeans
km =kmeans (z, centers=2, iter.max =10, nstart =1)
rapportoKMeans=km$betweenss/km$totss
km
km$centers
rapportoKMeans
km$betweenss
km$withinss




stirling2 <-function (n,m){
  s<-0
  if ((m >=1)&(m <=n)){
    for (k in seq (0,m)){
      s<-s+( choose (m,k)*(-1)^k*(m-k)^n/ factorial (m))}
    return (c(s))
  }
}

stirling2(nrow(z),2)
stirling2(nrow(z),3)




trH<-(n -1)*sum (apply(z,2,var))

taglio<-cutree(hlc, k=3)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trH3<-(num [[3]]-1) * sum (agvar [3, ])
trB<-trH-trH1-trH2-trH3 #misura di non omogeneità tra i cluster
rapportoLegameCompleto<-trB/trH

#legame medio 3 cluster 
taglio<-cutree(hlm, k=3)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trH3<-(num [[3]]-1) * sum (agvar [3, ])
trB<-trH-trH1-trH2-trH3 #misura di non omogeneità tra i cluster
rapportoLegameMedio<-trB/trH

#legame singolo 3 cluster
taglio<-cutree(hls, k=3)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trH3<-(num [[3]]-1) * sum (agvar [3, ])
trB<-trH-trH1-trH2-trH3 #misura di non omogeneità tra i cluster
rapportoLegameSingolo<-trB/trH


#centroide 3 cluster
taglio<-cutree(hc, k=3)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trH3<-(num [[3]]-1) * sum (agvar [3, ])
trB<-trH-trH1-trH2-trH3 #misura di non omogeneità tra i cluster
rapportoCentroide<-trB/trH
centroidiIniziali <-aggregate (z, tagliolist , mean )[,-1]

#mediana 3 cluster
taglio<-cutree(hmed, k=3)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trH3<-(num [[3]]-1) * sum (agvar [3, ])
trB<-trH-trH1-trH2-trH3 #misura di non omogeneità tra i cluster
rapportoMediana<-trB/trH

#k-means 3 cluster
km <-kmeans (z, centers=3, iter.max =10, nstart =1)
rapportoKMeans<-km$betweenss/km$totss



png("grafici/cluster/3_cluster/dendrogrammaUtenti_LegameSingolo3.png")
plot(hls, hang=-1, xlab="Metodo gerarchico agglomerativo", sub="del legame singolo")
rect.hclust(hls, k=3, border="green")
axis(side=4, at=round(c(0, hls$height),2))
dev.off()

png("grafici/cluster/3_cluster/dendrogrammaUtenti_LegameCompleto3.png")
plot(hlc, hang=-1, xlab="Metodo gerarchico agglomerativo", sub="del legame completo")
rect.hclust(hlc, k=3, border="green")
axis(side=4, at=round(c(0, hls$height),2))
dev.off()

png("grafici/cluster/3_cluster/dendrogrammaUtenti_LegameMedio3.png")
plot(hlm, hang=-1, xlab="Metodo gerarchico agglomerativo", sub="del legame medio")
rect.hclust(hlm, k=3, border="green")
axis(side=4, at=round(c(0, hls$height),2))
dev.off()

png("grafici/cluster/3_cluster/dendrogrammaUtenti_MetodoCentroide3.png")
plot(hc, hang=-1, xlab="Metodo gerarchico agglomerativo", sub="del centroide")
rect.hclust(hc, k=3, border="green")
axis(side=4, at=round(c(0, hls$height),2))
dev.off()

png("grafici/cluster/3_cluster/dendrogrammaUtenti_MetodoMediana3.png")
plot(hmed, hang=-1, xlab="Metodo gerarchico agglomerativo", sub="della mediana")
rect.hclust(hmed, k=3, border="green")
axis(side=4, at=round(c(0, hls$height),2))
dev.off()





