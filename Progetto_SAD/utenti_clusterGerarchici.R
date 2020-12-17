install.packages("xlsx")
#library(xlsx)
#utenti_per_regione_e_anno <-read.xlsx("utenti_per_regione_e_anno.xlsx")

Z<-round(utenti_per_regione_e_anno[1:nrow(utenti_per_regione_e_anno)-1,-1],0)
row.names(Z)<-utenti_per_regione_e_anno[1:nrow(utenti_per_regione_e_anno)-1,1]
n<-nrow(Z)

Z<-scale(Z)
d<-dist(Z, method="euclidean", diag=TRUE, upper=TRUE)

stirling2 <-function (n,m){
  s<-0
  if ((m >=1)&(m <=n)){
    for (k in seq (0,m)){
      s<-s+( choose (m,k)*(-1)^k*(m-k)^n/ factorial (m))}
    return (c(s))
  }
}

stirling2(nrow(Z),2)

#metodo del legame singolo
hls<-hclust(d, method="single")
png("grafici/cluster/dendrogrammaUtenti_LegameSingolo.png")
plot(hls, hang=-1, xlab="Metodo gerarchico agglomerativo", sub="del legame singolo")
rect.hclust(hls, k=2, border="green")
axis(side=4, at=round(c(0, hls$height),2))
dev.off()

#metodo del legame completo
hlc<-hclust(d, method="complete")
png("grafici/cluster/dendrogrammaUtenti_LegameCompleto.png")
plot(hlc, hang=-1, xlab="Metodo gerarchico agglomerativo", sub="del legame completo")
rect.hclust(hlc, k=2, border="green")
axis(side=4, at=round(c(0, hls$height),2))
dev.off()

#metodo del legame medio
hlm<-hclust(d, method="average")
png("grafici/cluster/dendrogrammaUtenti_LegameMedio.png")
plot(hlm, hang=-1, xlab="Metodo gerarchico agglomerativo", sub="del legame medio")
rect.hclust(hlm, k=2, border="green")
axis(side=4, at=round(c(0, hls$height),2))
dev.off()

#metodo del centroide
d2<-d^2
hc<-hclust(d2, method="centroid")
png("grafici/cluster/dendrogrammaUtenti_MetodoCentroide.png")
plot(hc, hang=-1, xlab="Metodo gerarchico agglomerativo", sub="del centroide")
rect.hclust(hc, k=2, border="green")
axis(side=4, at=round(c(0, hls$height),2))
dev.off()

#metodo della mediana
hmed<-hclust(d2, method="median")
png("grafici/cluster/dendrogrammaUtenti_MetodoMediana.png")
plot(hmed, hang=-1, xlab="Metodo gerarchico agglomerativo", sub="della mediana")
rect.hclust(hmed, k=2, border="green")
axis(side=4, at=round(c(0, hls$height),2))
dev.off()

#calcolo misura di non omogeneità totale in Z
n<-nrow(Z)
trH<-(n -1)*sum (apply(Z,2,var))

#calcolo misure di non omogeneità per metodo del legame singolo al variare di k
legamesingolo<-list()
kval<-1:n

for (i in kval){
  print(i)
  taglio<-cutree(hlc, k=i)
  num <-table (taglio)
  tagliolist<-list(taglio)
  agvar <- aggregate (Z, tagliolist , var)[, -1]
  trList<-list()
  for (cluster in 1:i){
    if (num[[cluster]]>1)
      trList[cluster]<-(num[[cluster]]-1) * sum (agvar [cluster, ])
    else
      trList[cluster]<-0
  }
  trB<-trH-Reduce("+", trList)
  legamesingolo[i]<-trB/trH
}




#legame singolo
taglio<-cutree(hls, k=2)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (Z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trB<-trH-trH1-trH2 #misura di non omogeneità tra i cluster
rapportoLegameSingolo<-trB/trH

#legame completo
taglio<-cutree(hlc, k=2)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (Z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trB<-trH-trH1-trH2 #misura di non omogeneità tra i cluster
rapportoLegameCompleto<-trB/trH

#legame medio
taglio<-cutree(hlm, k=2)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (Z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trB<-trH-trH1-trH2#misura di non omogeneità tra i cluster
rapportoLegameMedio<-trB/trH

#metodo centroide
taglio<-cutree(hc, k=2)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (Z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trB<-trH-trH1-trH2 #misura di non omogeneità tra i cluster
rapportoCentroide<-trB/trH
centroidiIniziali <-aggregate (Z, tagliolist , mean )[,-1]

#metodo mediana
taglio<-cutree(hmed, k=2)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (Z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trB<-trH-trH1-trH2 #misura di non omogeneità tra i cluster
rapportoMediana<-trB/trH

#metodo kmeans
km <-kmeans (Z, centers=2, iter.max =10, nstart =1)
rapportoKMeans<-km$betweenss/km$totss



#legame completo 3 cluster
taglio<-cutree(hlc, k=3)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (Z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trH3<-(num [[3]]-1) * sum (agvar [3, ])
trB<-trH-trH1-trH2-trH3 #misura di non omogeneità tra i cluster
rapportoLegameCompleto<-trB/trH

#legame medio 3 cluster 
taglio<-cutree(hlm, k=3)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (Z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trH3<-(num [[3]]-1) * sum (agvar [3, ])
trB<-trH-trH1-trH2-trH3 #misura di non omogeneità tra i cluster
rapportoLegameMedio<-trB/trH

#legame singolo 3 cluster
taglio<-cutree(hls, k=3)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (Z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trH3<-(num [[3]]-1) * sum (agvar [3, ])
trB<-trH-trH1-trH2-trH3 #misura di non omogeneità tra i cluster
rapportoLegameSingolo<-trB/trH


#centroide 3 cluster
taglio<-cutree(hc, k=3)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (Z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trH3<-(num [[3]]-1) * sum (agvar [3, ])
trB<-trH-trH1-trH2-trH3 #misura di non omogeneità tra i cluster
rapportoCentroide<-trB/trH
centroidiIniziali <-aggregate (Z, tagliolist , mean )[,-1]

#mediana 3 cluster
taglio<-cutree(hmed, k=3)
num <-table (taglio)
tagliolist<-list(taglio)
agvar <- aggregate (Z, tagliolist , var)[, -1]
trH1 <-(num [[1]]-1) * sum (agvar [1, ])
trH2 <-(num [[2]]-1) * sum (agvar [2, ])
trH3<-(num [[3]]-1) * sum (agvar [3, ])
trB<-trH-trH1-trH2-trH3 #misura di non omogeneità tra i cluster
rapportoMediana<-trB/trH

#k-means 3 cluster
km <-kmeans (Z, centers=3, iter.max =10, nstart =1)
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

