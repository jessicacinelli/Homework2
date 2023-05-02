#First task: 
#run both hclust and kmeans; 
#produce all related plots (e.g., dendrogram, sensitivity of tot.withinss); 
#estimate the number of clusters based on the results of hclust and kmeans; 
# discuss obtained results.

#lettura dei dati flows.csv
data<- read.csv('flows/flows1.csv')
data


################## CLUSTERING GERARCHICO ################## 

#Calcolo delle distanze euclidee:
# restituisce la matrice di distanze euclidee, cioè tutte le possibili distanze tra coppie di punti. 
d<- dist(data, method="euclidean")

# algoritmo standard di clustering gerarchico:
#"ward.D" "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
fit <- hclust(d, method="ward.D")

#plot del dendrogramma
plot(fit)

#zoom del plot
#plot(as.dendrogram(fit), ylim = c(0,200000000000))

# utilizziamo la funzione rect.hclust per mettere i rettangoli
#k è il numero di cluster che individuiamo guardando il dendrogramma
rect.hclust(fit, k=3, border = "blue")

#Taglio dell'albero: 
#l’uscita è fornita come identificativo del cluster di appartenenza per ogni punto.
g<- cutree(fit, k=3)
g

#creazione dataframe
clusters<-g
df<-clusters
df
#plot(data, col=g, pch=g, cex=2) 

#distribuzione dei punti nei cluster
#hist(g, breaks=5, xlab = "k", main=paste("Distribuzione dei punti nei cluster" ), labels = TRUE, ylim=c(0,2500))
h<-hist(df, col="lightblue", labels = TRUE, 
          breaks=seq(min(df)-1,max(df)),
          axes=F, main="Distribuzione dei punti nei cluster", xlab="K")
axis(2)
axis(1,at=h$mids,seq(min(df),max(df)))

################## CLUSTERING K-MEANS ################## 
#definiamo un vettore di 10 elementi con un id crescente (tss=total squares sum): 
#ipotizziamo di testare fino a 10 cluster (k=10)
#tss è la metrica che vogliamo misurare in base al numero di cluster.
tss<-seq(1,10,1)  

# Test dei valori di k da 1 a 10.
for (i in 1:10) tss[i] <- kmeans(data,i)$tot.withinss   
tss   #-> è una metrica del totale della somma dei quadrati intra-cluster. Varia molto velocemente quando varia il numero di cluster.
plot(tss, type='o')
plot(tss, type="o")

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
df_km<-clusters_kmeans
df_km

#distribuzione dei punti nei cluster
hist(res$cluster, xlab = "k", main="Distribuzione dei punti nei cluster")

h2<-hist(df_km, col="lightblue", labels = TRUE, 
        breaks=seq(min(df_km)-1,max(df_km)),
        axes=F, main="Distribuzione dei punti nei cluster", xlab="K")
axis(2)
axis(1,at=h$mids,seq(min(df),max(df)))
#plot(data, col=res$cluster, pch=res$cluster)

# Aggiunta dei centroidi sul grafico
#points(res$centers, pch=5) 

