#librerie
library("FactoMineR")
library("factoextra")

#lettura dei dati flows.csv
data<- read.csv('flows/flows1.csv')

data.pca <- prcomp(data, scale=FALSE)

eig.val <- get_eigenvalue(data.pca)
eig.val
#screeplot: i seguenti metodi sono identici
fviz_screeplot(data.pca, addlabels=TRUE,
               title="Scree plot - scale = FALSE") +
                theme(plot.title = element_text(hjust = 0.5))
fviz_eig(data.pca, addlabels = TRUE, 
         title="Scree plot - scale = FALSE") + 
          theme(plot.title = element_text(hjust = 0.5))

#biplot
fviz_pca_var(data.pca, col.var = "black",
             xlim=c(-6e8,75000), ylim=c(0,5e7),
             title="Biplot variables - scale = FALSE")+ 
               theme(plot.title = element_text(hjust = 0.5))
fviz_pca_var(data.pca, col.var = "black",
             xlim=c(-6e8,6e8), ylim=c(-6e8,6e8),
             title="Biplot variables - scale = FALSE")+ 
  theme(plot.title = element_text(hjust = 0.5))

#var$contrib: contains the contributions (in percentage) of the variables to the principal components.
fviz_pca_var(data.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title="Biplot variables - scale = FALSE") + 
                theme(plot.title = element_text(hjust = 0.5))

#Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(data.pca, choice = "var", axes = 1:2, 
          title="Quality of representation (cos2) - scale = FALSE", addlabels=TRUE)+ 
            theme(plot.title = element_text(hjust = 0.5))
data.pca
var <- get_pca_var(data.pca)
var$cos2
corrplot(var$cos2, is.corr = FALSE)