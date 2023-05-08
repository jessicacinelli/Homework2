#librerie
library("FactoMineR")
library("factoextra")

#lettura dei dati flows.csv
data<- read.csv('flows/flows1.csv')

data.pca <- prcomp(data, scale=TRUE)

eig.val <- get_eigenvalue(data.pca)
eig.val
#screeplot: i seguenti metodi sono identici
fviz_screeplot(data.pca, addlabels=TRUE,
               title="Scree plot - scale = TRUE") +
  theme(plot.title = element_text(hjust = 0.5))
fviz_eig(data.pca, addlabels = TRUE, 
         title="Scree plot - scale = TRUE") + 
  theme(plot.title = element_text(hjust = 0.5))

#calcolo della varianza
sum(data.pca$sdev[1:5]^2) / sum (data.pca$sdev^2)

#biplot
fviz_pca_var(data.pca, col.var = "black",
             title="Biplot variables - scale = TRUE")+ 
  theme(plot.title = element_text(hjust = 0.5))


#var$contrib: contains the contributions (in percentage) of the variables to the principal components.
fviz_pca_var(data.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title="Biplot variables - scale = TRUE") + 
  theme(plot.title = element_text(hjust = 0.5))

#Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(data.pca, choice = "var", axes = 1:2,
          title="Quality of representation (cos2) - scale = TRUE")+ 
  theme(plot.title = element_text(hjust = 0.5))

