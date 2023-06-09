#librerie
library("FactoMineR")
library("factoextra")

#lettura dei dati flows.csv
data<- read.csv('flows/flows1.csv')

data.pca <- prcomp(data, scale=TRUE)

# sono i valori degli attributi le cui coordinate sono in un sistema di riferimento ruotato che ha 8 componenti.
data.pca$x 

#dataset traslato
data.translated<- data.pca$x[,1:2]
plot(data.translated, main = "Dataset traslato: scale = TRUE")

############ k means
tss<-seq(1,10,1)  

# Test dei valori di k da 1 a 10.
for (i in 1:10) tss[i] <- kmeans(data.translated,i)$tot.withinss   
tss   #-> è una metrica del totale della somma dei quadrati intra-cluster. Varia molto velocemente quando varia il numero di cluster.
plot(tss, type='o', xlab="K", main="Numero ottimale di cluster: scale = TRUE")


#l'idea è scegliere qualcosa che si trova in corripondenza del ginocchio o subito dopo il ginocchio.
points( c(4), tss[which.max(tss<4000)], pch=20, col="green", cex=2) #scale=TRUE

res<-kmeans(data.translated,4) #non va bene perché restituisce un cluster costituito da un unico punto.

clusters_kmeans<-res$cluster

#distribuzione dei punti nei cluster
h2<-hist(clusters_kmeans, col="lightblue", labels = TRUE, 
         breaks=seq(min(clusters_kmeans)-1,max(clusters_kmeans)),
         axes=F, main="Distribuzione dei punti nei cluster: scale = TRUE", xlab="K", ylim=c(0,2500))
axis(2)
axis(1,at=h2$mids,seq(min(clusters_kmeans),max(clusters_kmeans)))

# Visualize clusters
fviz_cluster(res, data.translated,  geom = c("point"), 
              repel=TRUE,  stand=FALSE,
              #xlim=c(-5,5), ylim=c(-12,3),
              title="Cluster plot: scale = TRUE") +
              theme(plot.title = element_text(hjust = 0.5)) + 
              geom_point(data = as.data.frame(res$centers), 
              aes(x = res$centers[,1], y =res$centers[,2] ), 
              size = 5, shape = 8)


clusters<-table(res$cluster)<10

# Get the indices where the logical vector is TRUE
cluster_index <- which(clusters)

#rimozione del cluster con indice cluster_index dal dataset trasformato e dai risultati del kmeans
data.translated2<-subset(data.translated, (res$cluster) != cluster_index)
dim(data.translated2)

#################################################

plot(data.translated2, main = "Dataset traslato: scale = TRUE")

############ k means
tss<-seq(1,10,1)  

# Test dei valori di k da 1 a 10.
for (i in 1:10) tss[i] <- kmeans(data.translated2,i)$tot.withinss   
tss   #-> è una metrica del totale della somma dei quadrati intra-cluster. Varia molto velocemente quando varia il numero di cluster.
plot(tss, type='o', xlab="K", main="Numero ottimale di cluster: scale = TRUE")


#l'idea è scegliere qualcosa che si trova in corripondenza del ginocchio o subito dopo il ginocchio.
points( c(4), tss[which.max(tss<2000)], pch=20, col="green", cex=2) #scale=TRUE

res<-kmeans(data.translated2,4) #non va bene perché restituisce un cluster costituito da un unico punto.

clusters_kmeans<-res$cluster

#distribuzione dei punti nei cluster
h2<-hist(clusters_kmeans, col="lightblue", labels = TRUE, 
         breaks=seq(min(clusters_kmeans)-1,max(clusters_kmeans)),
         axes=F, main="Distribuzione dei punti nei cluster: scale = TRUE", xlab="K", ylim=c(0,2500))
axis(2)
axis(1,at=h2$mids,seq(min(clusters_kmeans),max(clusters_kmeans)))

# Visualize clusters
fviz_cluster(res, data.translated2,  geom = c("point"), 
             repel=TRUE,  stand=FALSE,
             #xlim=c(-5,5), ylim=c(-12,3),
             title="Cluster plot: scale = TRUE") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_point(data = as.data.frame(res$centers), 
             aes(x = res$centers[,1], y =res$centers[,2] ), 
             size = 5, shape = 8)



