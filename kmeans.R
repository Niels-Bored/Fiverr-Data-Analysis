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

#Método del codo

set.seed(6)

wcss = vector()

for (i in 1:10){
  wcss[i] <- sum(kmeans(dataset, i)$withinss)
}

plot(1:10, 
     wcss, 
     type = 'b', 
     main = "Método del codo", 
     xlab = "Número de clusters (k)", 
     ylab = "WCSS")

#Aplicar el algoritmo de k-means con k óptimo

set.seed(29)

kmeans <- kmeans(dataset, 3, iter.max = 300, nstart = 10)

#Visualización de los clusters

dataset = dataset[,1:2]
#dataset = dataset[, c("Total", "Elapsed.Time")]

library(cluster)

clusplot(dataset,
         kmeans$cluster,
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
