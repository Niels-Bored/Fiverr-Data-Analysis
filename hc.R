#Clustering con K-means

#Importar los datos

dataset = read.csv("Fiverr_Orders.csv")

dataset = dataset[, 3:6]

#Codificar las variables categóricas

dataset$Status = factor(dataset$Status,
                        levels = c("Completed", "Cancelled"),
                        labels = c(1, 0))

#Tratamiento de los valores NAs

dataset$Stars = ifelse(is.na(dataset$Stars), 
                       ave(dataset$Stars, FUN=function(x) mean(x, na.rm = TRUE)),
                       dataset$Stars)

#Utilizar el dendrograma para encontrar el número óptimo de clusters

dendrogram = hclust(dist(dataset, method = "euclidean"),
                    method = "ward.D")

plot(dendrogram,
     main = "Dendrograma")

#Ajustar el clustering jerárquico a nuestro dataset

hc = hclust(dist(dataset, method = "euclidean"),
            method = "ward.D")

Y_hc = cutree(hc, k=3)

library(cluster)

dataset = dataset[1:2]

clusplot(dataset,
         Y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = "Clustering de ordenes",
         xlab = "Ganancia",
         ylab = "Tiempo"
)