#############################################################################
#############################################################################
#
#       MODELO DE PROPENSION BASADO EN EL SISTEMA DE RECOMENDACION
#
#############################################################################
# Version: V1
# Fecha: 02/10/2020
# Autor: Victor Blanco
#############################################################################
#############################################################################

rm(list=ls(all=TRUE))

#install.packages("recommenderlab")
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(tidyverse)
library(skimr)

#cargar la data
setwd("C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020/SISTEMA_RECOMENDACION")
rating_data <- read.csv("SISTEMA_RECOMENDACION.rpt", sep = "~") # clientes con productos de mercado APC
str(rating_data)
summary(rating_data)
head(rating_data)

# PREPROCESAMIENTO DE LA DATA
colnames(rating_data) <- c("identificacion","producto1", "rating")
rating_data1 <- rating_data%>% 
  filter(!is.na(rating))
# %>%
#   mutate(producto1 = 
#            case_when (producto == 'TARJ. CREDITO' ~ 1,
#                       producto == 'PREST. PERSONAL' ~ 2,
#                       producto == 'OTROS' ~ 3))%>%
#     dplyr::select("identificacion", "producto1", "rating" )
# #head(rating_data1)

rating_data1%>% group_by(producto1)%>% summarise(n())
  


# REALIZAR UNA MATRIZ DE CLIENTES VS PRODUCTOS
ratingMatrix <- dcast(rating_data1, identificacion~producto1, value.var = "rating", na.rm=FALSE)
head(ratingMatrix)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove la columna de usuarios
head(ratingMatrix)
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
head(ratingMatrix@data)

# Modelando
# #toma la matriz real para modelar
# recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
# names(recommendation_model)
# #lapply(recommendation_model, "[[", "description")
# 
# #elige el modelo basado en la matriz real
# recommendation_model$IBCF_realRatingMatrix$parameters
# 
# #basado en las recomendaciones de un usuario a otro
# # se contruye una matriz de disimilaridad
# 
# #entre usuarios pero solo con 4 uusarios
# similarity_mat <- similarity(ratingMatrix[1:4, ],
#                              method = "cosine",
#                              which = "users")
# as.matrix(similarity_mat)
# image(as.matrix(similarity_mat), main = "User's Similarities")
# 
# 
# #entre peliculas
# movie_similarity <- similarity(ratingMatrix[, 1:3], method =
#                                  "cosine", which = "items")
# as.matrix(movie_similarity)
# image(as.matrix(movie_similarity), main = "Movies similarity")

#valores de rating
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values) # extracting unique ratings
Table_of_Ratings <- table(rating_values) # creating a count of movie ratings
Table_of_Ratings

# PRODUCTO MAS UTILIZADO
producto_views <- colCounts(ratingMatrix) # cuenta las columnas (movies)
# se hace una tabla con las peliculas y cuantas veces fueron vistas
table_views <- data.frame(producto = names(producto_views),
                          views = producto_views) # create dataframe of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ] # sort by number of views
# NORMALIZACION
normalized_ratings <- normalize(ratingMatrix)
#sum(rowMeans(normalized_ratings) > 0.00001)
# image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
#                          colCounts(normalized_ratings) > minimum_users],
#       main = "Normalized Ratings of the Top Users")

# BINARIZACION
binary_minimum_productos <- quantile(rowCounts(ratingMatrix), 0.95)
binary_minimum_clientes <- quantile(colCounts(ratingMatrix), 0.95)
#movies_watched <- binarize(movie_ratings, minRating = 1)

good_rated_productos <- binarize(ratingMatrix, minRating = 3) # asigna 1 si el rating es por encima de 3 y 0 en TOS
# image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
#                        colCounts(movie_ratings) > binary_minimum_users],
#       main = "Heatmap of the top users and movies")

# Collaborative Filtering System

#muestreo
sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(ratingMatrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- ratingMatrix[sampled_data, ]
testing_data <- ratingMatrix[!sampled_data, ]

dim(training_data)
dim(testing_data)

# Modelo
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

# Entrenamiento
recommen_model <- Recommender(data = training_data,
                              method = "IBCF"
                              #,parameter = list(k = 30)
                              )
recommen_model
class(recommen_model)

save(recommen_model, file="recommen_model.rda")


# Evaluacion del modelo
model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 3
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")

model_info$sim

sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)

#sum_cols <- colSums(model_info$sim > 0)
#qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution of the column count")

# Prediccion
top_recommendations <- 3 # the number of items to recommend to each user
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations
                                     , type = "ratingMatrix")

predicted_recommendations@data

predicciones_por_producto <- as.data.frame(as.matrix(predicted_recommendations@data))%>%
  tibble()%>%
  select("PREST. PERSONAL", "TARJ. CREDITO")

names(predicciones_por_producto)
skim(predicciones_por_producto)

##################################################################################################
# ARCHIVO DE CALCULO
##################################################################################################
#install.packages("recommenderlab")
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(tidyverse)
library(skimr)

#cargar la data
setwd("C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020/SISTEMA_RECOMENDACION")

rating_data <- read.csv("SISTEMA_RECOMENDACION.rpt", sep = "~") # clientes con productos de mercado APC
str(rating_data)
summary(rating_data)
head(rating_data)

# PREPROCESAMIENTO DE LA DATA
colnames(rating_data) <- c("identificacion","producto1", "rating")
rating_data1 <- rating_data%>% 
  filter(!is.na(rating))
rating_data1%>% group_by(producto1)%>% summarise(n())


# Guardar la identificacion de clientes
clientes <- rating_data1%>%
  select(identificacion)%>%
  group_by(identificacion)%>%
  slice(1)

# Matriz de clientes productos
ratingMatrix <- dcast(rating_data1, identificacion~producto1, value.var = "rating", na.rm=FALSE)
#head(ratingMatrix)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove la columna de usuarios
#head(ratingMatrix)
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
#head(ratingMatrix@data)

# Modelo
sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(ratingMatrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- ratingMatrix[sampled_data, ]
#testing_data <- ratingMatrix[!sampled_data, ]

# Entrenamiento
recommen_model <- Recommender(data = training_data,
                              method = "IBCF"
                              ,parameter = list(k = 30))

# Predicciones
predicted_recommendations1 <- predict(object = recommen_model,
                                     newdata = ratingMatrix
                                     , type = "ratingMatrix")

#predicted_recommendations1@data
#predicted_recommendations1@data@Dimnames

predicciones_por_producto1 <- as.data.frame(as.matrix(predicted_recommendations1@data))%>%
  tibble()%>%
  select("PREST. PERSONAL", "TARJ. CREDITO")


predicciones3 <- cbind(clientes, predicciones_por_producto1)
#head(predicciones3)
