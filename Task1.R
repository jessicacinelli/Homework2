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

# algoritmo standard di clustering gerarchico
fit <- hclust(d, method="ward.D")

#plot del dendrogramma
plot(fit)

# utilizziamo la funzione rect.hclust per mettere i rettangoli
#k è il numero di cluster che individuiamo guardando il dendrogramma
rect.hclust(fit, k=4, border = "blue")

#Taglio dell'albero: 
#l’uscita è fornita come identificativo del cluster di appartenenza per ogni punto.
g<- cutree(fit, k=4)
g

#plot(data, col=g, pch=g, cex=2) 

#distribuzione dei punti nei cluster
hist(g, xlab = "" )


################## CLUSTERING K-MEANS ################## 