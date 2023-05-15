#lettura dei dati flows.csv
data<- read.csv('flows/flows1.csv')

################## CLUSTERING GERARCHICO ################## 

#Calcolo delle distanze euclidee:
# restituisce la matrice di distanze euclidee, cioè tutte le possibili distanze tra coppie di punti. 
distances<- dist(data, method="euclidean")

# algoritmo standard di clustering gerarchico:
#"ward.D" "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
fit <- hclust(distances, method="ward.D")

#plot del dendrogramma
plot(fit, labels = FALSE, hang=(-1))
abline(h=1.3e11, col="red",  lty=2)
#zoom del plot
#plot(as.dendrogram(fit), ylim = c(0,10000000000), leaflab="none")
#plot(dendrogramma, ylim = c(0,200000000000), labels = FALSE, hang=(-1))
# utilizziamo la funzione rect.hclust per mettere i rettangoli
#k è il numero di cluster che individuiamo guardando il dendrogramma
rect.hclust(fit, k=4, border = "blue")
legend( x="topright", 
        legend=c("Taglio delle partizioni", "k cluster"),
        col=c("red","blue"), lwd=1, cex=0.7)
#Taglio dell'albero: 
#l’uscita è fornita come identificativo del cluster di appartenenza per ogni punto.
groups<- cutree(fit, k=3)

g <- hcut(data, k = 3,
            hc_func = "hclust", hc_method = "ward")

#g
#fviz_cluster(g) plotta solo  due dimensioni

#creazione dataframe
clusters<-groups
df<-clusters
df
plot(data, col=groups, pch=groups, cex=1) 

#distribuzione dei punti nei cluster
#hist(g, breaks=5, xlab = "k", main=paste("Distribuzione dei punti nei cluster" ), labels = TRUE, ylim=c(0,2500))
h<-hist(df, col="lightblue", labels = TRUE, 
        breaks=seq(min(df)-1,max(df)),
        axes=F, 
        ylim=c(0,2500),
        xlab="K",
        main="Distribuzione dei punti nei cluster")
axis(2)
#stampa il cluster di ogni bin)
axis(1,at=h$mids,seq(min(df),max(df)))



################## CLUSTERING K-MEANS ################## 
data<- read.csv('flows/flows1.csv')

#definiamo un vettore di 10 elementi con un id crescente (tss=total squares sum): 
#ipotizziamo di testare fino a 10 cluster (k=10)
#tss è la metrica che vogliamo misurare in base al numero di cluster.
tss<-seq(1,10,1)  

# Test dei valori di k da 1 a 10.
for (i in 1:10) tss[i] <- kmeans(data,i)$tot.withinss   
tss   #-> è una metrica del totale della somma dei quadrati intra-cluster. Varia molto velocemente quando varia il numero di cluster.
plot(tss, type='o', xlab="K")


#l'idea è scegliere qualcosa che si trova in corripondenza del ginocchio o subito dopo il ginocchio.
points( c(4), tss[which.min(tss>2e20)], pch=20, col="green", cex=2)

# k=4 
res<-kmeans(data,4) #non va bene perché restituisce un cluster costituito da un unico punto.

#restituisce l'id del cluster per i vari punti
res$cluster 

#restituisce i centroidi
res$centers 

#restituisce restituisce la somma dei quadrati delle distanze dei punti rispetto al centroide (intra-cluster). Se faccio tot.withinss ottengo la somma di questi valori.
res$withinss

#creazione dataframe
clusters_kmeans<-res$cluster

#distribuzione dei punti nei cluster
hist(res$cluster, xlab = "k", main="Distribuzione dei punti nei cluster")

h2<-hist(clusters_kmeans, col="lightblue", labels = TRUE, 
        breaks=seq(min(clusters_kmeans)-1,max(clusters_kmeans)),
        axes=F, main="Distribuzione dei punti nei cluster", xlab="K", ylim=c(0,2500))
axis(2)
axis(1,at=h2$mids,seq(min(clusters_kmeans),max(clusters_kmeans)))
plot(data, col=res$cluster, pch=res$cluster)


## RIMOZIONE OUTLIER 
clusters<-table(res$cluster)<10

# Get the indices where the logical vector is TRUE
cluster_index <- which(clusters)

#rimozione del cluster con indice cluster_index dal dataset trasformato e dai risultati del kmeans
rows_to_remove <- res$cluster == cluster_index
data2<-subset(data, (res$cluster) != cluster_index)
#data.translated2 <- data.translated[-rows_to_remove,]
dim(data.translated2)
res2<-res
res2$centers <- res$centers[-cluster_index, ]
res2$cluster <- res$cluster[!rows_to_remove]
res2$size <- res$size[-cluster_index]
res2$totss <- res$totss - sum(res$withinss[cluster_index])
res2$withinss <- res$withinss[-cluster_index]
res2$betweenss <- res$totss - sum(res$withinss)
res2$iter <- res$iter + 1

table(res2$cluster)


#new distribution
clusters_pca1<-res2$cluster
clusters_pca1

#distribuzione dei punti nei cluster
h3<-hist(clusters_pca1, col="lightblue", labels = TRUE, 
         breaks=seq(min(clusters_pca1)-1,max(clusters_pca1)),
         axes=F, main="Distribuzione dei punti nei cluster: scale = FALSE", xlab="K", ylim=c(0,2500))
axis(2)
axis(1,at=h3$mids,seq(min(clusters_pca1),max(clusters_pca1)))



sequence<-seq(min(clusters_pca1)-1,max(clusters_pca1))
my_seq_no_zero <- sequence[sequence != 0]
my_seq_no_zero
