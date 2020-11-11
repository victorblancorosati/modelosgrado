#############################################################################
#############################################################################
#
#                      MODELO PROPENSION PRODUCTOS
#                        PROYECCIONES DIARIAS
#
#############################################################################
# Version: V1
# Fecha: 12/06/2020
# Autor: Victor Blanco
#############################################################################
#############################################################################
# V1. 12/06/2020

rm(list=ls(all=TRUE))
#ap <- available.packages()
#install.packages("broom.mixed")
#update.packages ()
#devtools::install_github("tidymodels/broom")


#
#library('reshape')
#library('reshape2')
#library('keras')
#library ('rsample')
#library ('recipes')
#library('ggpubr')
#library('ggplot2')
#library('purr')
#library('corrr')
#library('ggcorrplot')
#library('caret')

#devtools::install_github("tidymodels/broom")
library('lubridate')
library('broom')
library('tidymodels')
library('broom.mixed')
library('modelgrid')
library('readr')
library('tidyverse')
library ('rsample')
library ('recipes')
library('dplyr')
library('caret')


setwd( "C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020")

# CARGA DE ARCHIVOS MOVIMIENTOS DIARIOS

#carga de archivo clientes
x <- read_delim("PROPENSION_PRODUCTOS_TXT.txt", 
                                                 "~", escape_double = FALSE, col_names = FALSE, 
                                                 col_types = cols(X2 = col_character()), 
                                                 locale = locale(decimal_mark = ","), 
                                                 trim_ws = TRUE)

colnames(x) <- c('fecha','cuenta','saldo','saldo_promedio','cod_subproducto', 'cod_cliente',
 
                              'creditos_monto','creditos_tx','debitos_monto','debitos_tx','id_cliente')

glimpse(x)
x1 <- x[1:10,]

# Convirtiendo variables
variable_factor = function(x) {as.factor(x)}
variable_character = function(x) {as.character(x)}
variable_numerica = function(x) {as.numeric(as.character(x))}
variable_tenencia <- function(x) {as.factor(if_else(x == 0, "No", "Si"))}

#cliente ejemplo
z = x %>% filter(id_cliente == '8-782-2109')

# Agrupa por cliente las cuentas
x1 <- x %>%
  group_by(id_cliente, fecha)%>%
  summarise(saldo = sum(saldo),
            saldo_promedio = sum(saldo_promedio),
            creditos_monto = sum(creditos_monto),
            debitos_monto = sum(debitos_monto),
            creditos_tx = sum(creditos_tx),
            debitos_tx = sum(debitos_tx)) 

x2 <- x1 %>%
  arrange(id_cliente,fecha)

# carga de fecha de apertura de productos
a1 <- read_delim("APERTURA_CREDITOS.rpt",  "~", escape_double = FALSE, trim_ws = TRUE)

a1 <- a1 %>%
  mutate(producto = case_when( 
    str_detect(NOM_PRODUCTO,"PERSONAL") ~ "PP",
    str_detect(NOM_PRODUCTO, "TARJETA") ~ "TDC",
    str_detect(NOM_PRODUCTO, "DPF") ~ "DPF",
    TRUE                                ~ "OTRO"))

z1=a1 %>%
  group_by(producto, NOM_PRODUCTO)%>%
  summarise(n())

# Union de ambas bases
datos1 <-x2 %>%
  left_join(a1, by = c('id_cliente'='ID_CLIENTE_N5'))

glimpse(datos1)
z = datos1 %>% filter(id_cliente == '4-167-478')
Sys.Date( ) 


# Identificar desde cuando comienza la tenencia del productos
# Calculo de la tenencia
datos2= datos1 %>%
  # filter(id_cliente == '8-397-782')%>%
  mutate(fecha_apertura = as_date(fecha_apertura),
         YEAR =year(fecha_apertura),
         anomes_producto = paste(YEAR = year(fecha_apertura), month(fecha_apertura), sep = "_"),
         anomes_pasivo = paste(YEAR = year(fecha), month(fecha), sep = "_"),
         tenencia = ifelse( fecha_apertura <= fecha, 0, 1),
         tenencia_tdc = case_when(
           producto == 'TDC' & tenencia == 1 ~ '1',
           TRUE ~ '2'),
         tenencia_pp = case_when(
           producto == 'PP' & tenencia == 1 ~ '1',
           TRUE ~ '2'),
         tenencia_dpf = case_when(
           producto == 'DPF' & tenencia == 1 ~ '1',
           TRUE ~ '2')
         )

z = datos2 %>% filter(id_cliente == '9-149-406')

# Contador para utilizar los modelos longitudinales
datos3 <- datos2%>%
  group_by(id_cliente)%>%
  mutate(meses = seq(1:length(id_cliente)))

table(datos3$anomes_producto, datos3$tenencia)

z1= datos3 %>% filter(id_cliente == '4-804-1637')
z2= datos3 %>% filter(anomes_producto == '2019_11')
z3= datos3 %>% filter(id_cliente == '4-769-703')

datos4 <- datos3%>%
#          filter(tenencia== '1')%>%
          arrange(id_cliente, fecha)

# no todos los clientes tienen la misma cantidad de dias así que voy a agrupar por días

# percentiles de dias
probs <- c(0.25, 0.5, 0.75)
percentiles <- datos4 %>% 
  group_by(producto) %>% 
  summarise(p = list(probs), q = list(quantile(meses, probs)),
            n()) %>% 
  unnest()

#Puede seer por fechas, NO VALE LA PENA
# fechas = datos4 %>%
#   group_by( anomes_producto, meses) %>%
#   summarise(n())
# 

# percentiles de dias
datos4_nest<- datos4 %>%
         group_by(producto) %>%
          nest

datos4_nest <- datos4_nest %>%
               mutate(percentil_25 = map(.x = data, .f = ~quantile(.x$meses, .25)),
                      percentil_50 = map(.x = data, .f = ~quantile(.x$meses, .50)),
                      percentil_75 = map(.x = data, .f = ~quantile(.x$meses, .75)))%>%
 unnest()

#calculo de max me por credito

datos4_nest1<- datos4_nest %>%
  group_by(NO_CREDITO) %>%
  nest


datos4_nest1 <- datos4_nest1 %>%
  mutate(max_mes = map(.x = data, .f = ~max(.x$meses)))%>%
  unnest()

# Agrupando por el mayor numero de meses
datos4_nest1 <- datos4_nest1%>%
  mutate(grupo = case_when(
    max_mes < percentil_25 | max_mes == percentil_25 ~ "g1",
    max_mes < percentil_50 | max_mes == percentil_50  ~ 'g2',
    max_mes < percentil_75 | max_mes == percentil_75  ~ 'g3',
    TRUE ~ 'g4'))
       
# Revision

# z = datos4_nest%>%
# unnest()
# 
# z = datos4_nest1 %>%
#   filter(producto== 'TDC')%>%
#   group_by(anomes_producto,grupo,max_mes)%>%
#   summarise(n())
# 
# z1 = datos4_nest1 %>%
#   filter(producto== 'TDC', max_mes == 12)
#   
# 
# datos4_nest1 %>% 
#   filter(producto == 'TDC', grupo == 'g3', saldo > 100000 ) %>% 
#   ggplot(mapping = aes(x = fecha, y = log(saldo_promedio))) +
#   geom_point() + 
#   geom_smooth(se = FALSE)
# 
#glimpse(datos4_nest1)
# 
# names(datos4_nest1)

z = datos4_nest1 %>%
     filter(NO_CREDITO== '6010030832')

# Eliminacion de vacios por ceros
f1 = function(x) {ifelse(is.na(x),0,x)}


datos4_nest2 =
   datos4_nest1 %>%
      dplyr::select(NO_CREDITO, id_cliente, saldo, saldo_promedio, creditos_monto, creditos_tx, 
             debitos_monto, debitos_tx, tenencia, tenencia_tdc, tenencia_pp, tenencia_dpf,max_mes,grupo, anomes_producto) %>%
  mutate(saldo = f1(saldo),
         saldo_promedio = f1(saldo_promedio),
         creditos_tx = f1(creditos_tx),
         creditos_monto = f1(creditos_monto),
         debitos_tx = f1(debitos_tx),
         debitos_monto = f1(debitos_monto),
         tenencia_tdc = as.factor(tenencia_tdc),
         tenencia_dpf = as.factor(tenencia_dpf))

#glimpse(datos4_nest2)

summary(datos4_nest2)

z = datos4_nest2 %>% filter(id_cliente == '9-149-406')

# Guardar los datos
setwd("C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020")
write.csv(datos4_nest2, "datos4_nes_2.csv")

resumen <- datos4_nest2 %>%
  group_by(grupo, tenencia_tdc)%>%
  summarise(n_distinct(id_cliente))

#######################################
#PREPROCESANDO LOS DATOS

# cambiar el nombre de la primera columna
#separar los datos en dos grupos reconociendo los clienets como individuos y separa cada
# grupo y no parte las observaciones de los clientes

# Tomo la información por clientes para tomar la muestra

clientes <- datos4_nest1 %>%
  dplyr::select(id_cliente, tenencia)%>%
  group_by(id_cliente)%>%
  summarise(tenencia_min= case_when(
    min(as.numeric(as.character(tenencia))) == 1 ~ "1",
    TRUE ~ "2"))


clientes_tdc <- datos4_nest1 %>%
  dplyr::select(id_cliente, tenencia_tdc)%>%
  group_by(id_cliente)%>%
  summarise(tenencia_tdc_min= case_when(
  min(as.numeric(as.character(tenencia_tdc))) == 1 ~ "1",
  min(as.numeric(as.character(tenencia_tdc))) == 2 ~ "2"))


clientes_pp <- datos4_nest1 %>%
  dplyr::select(id_cliente, tenencia_pp)%>%
  group_by(id_cliente)%>%
  summarise(tenencia_pp_min= case_when(
    min(as.numeric(as.character(tenencia_pp))) == 1 ~ "1",
    min(as.numeric(as.character(tenencia_pp))) == 2 ~ "2"))


clientes_dpf <- datos4_nest1 %>%
  dplyr::select(id_cliente, tenencia_dpf)%>%
  group_by(id_cliente)%>%
  summarise(tenencia_dpf_min= case_when(
    min(as.numeric(as.character(tenencia_dpf))) == 1 ~ "1",
    min(as.numeric(as.character(tenencia_dpf))) == 2 ~ "2"))


#z = clientes %>% filter(id_cliente == '9-149-406')
clientes %>% group_by(tenencia_min) %>% summarise(n())
clientes_tdc %>% group_by(tenencia_tdc_min) %>% summarise(n())
clientes_pp %>% group_by(tenencia_pp_min) %>% summarise(n())
clientes_dpf %>% group_by(tenencia_dpf_min) %>% summarise(n())


# Guardar los datos
setwd("C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020")
write.csv(datos4_nest2, "datos4_nes_2.csv")
write.csv(clientes, "clientes.csv")
write.csv(clientes_tdc, "clientes_tdc.csv")
write.csv(clientes_pp, "clientes_pp.csv")
write.csv(clientes_dpf, "clientes_dpf.csv")

#########################################################
# MANIPULACION Y MODELOS
#########################################################
x3 <- datos4_nest2
glimpse(x3)

# Distancia de mahalanobis para verificar atípicos
#m_dist <- function(x) { mahalanobis(x,colMeans(x),cov(x))} 

#Limpiando los datos
receta <- x3 %>%
  recipe(tenencia_tdc ~ .) %>%         # variable a dependiente
  add_role(NO_CREDITO, new_role = "no_borrar")%>%
  add_role(id_cliente, new_role = "no_borrar")%>%
  add_role(grupo, new_role = "no_borrar")%>%
  step_rm(tenencia, tenencia_pp, tenencia_dpf,anomes_producto) %>%   # eliminar columnas
  step_naomit(all_outcomes(), all_predictors())%>%   #ominir NA's de todos las variables
  #step_log(SALDOS_VISTA_1SEM, SALDOS_VISTA_6M, SALDO, INGRESO_ESTIMADO) %>%            #tomar el logaritmo
  #step_mutate(N_TC = ifelse(N_TC == "1",1,0)) %>%   #convesi?n logica
  #step_dummy(all_nominal(), -all_outcomes()) %>% # conversi?n de variables como dummys
  #step_center(all_numeric(), -all_outcomes()) %>%  # centrar las variables predictoras
  #step_scale(all_numeric(), -all_outcomes()) %>%  # escalar las variables predictoras
  #  step_mutate_at( all_numeric(), fn = ~  m_dist(.)) %>%
  #               outlier1=quantile(outlier,.01),
  #              outlier2=quantile(outlier,.99)) %>%
  # step_filter( outlier > outlier1, outlier < outlier2) %>%
  
  #step_nzv(all_numeric(), -all_outcomes())%>%
  #step_pca(all_numeric(), threshold = .75) %>%
  prep()

summary(receta)

datos_tdc <- bake(receta, new_data = x3) 


receta1 <- x3 %>%
  recipe(tenencia_pp ~ .) %>%         # variable a dependiente
  add_role(NO_CREDITO, new_role = "no_borrar")%>%
  add_role(id_cliente, new_role = "no_borrar")%>%
  add_role(grupo, new_role = "no_borrar")%>%
  step_rm(tenencia, tenencia_tdc, tenencia_dpf,anomes_producto) %>%   # eliminar columnas
  step_naomit(all_outcomes(), all_predictors())%>%   #ominir NA's de todos las variables
  #step_log(SALDOS_VISTA_1SEM, SALDOS_VISTA_6M, SALDO, INGRESO_ESTIMADO) %>%            #tomar el logaritmo
  #step_mutate(N_TC = ifelse(N_TC == "1",1,0)) %>%   #convesi?n logica
  #step_dumm☻y(all_nominal(), -all_outcomes()) %>% # conversi?n de variables como dummys
  #step_center(all_numeric(), -all_outcomes()) %>%  # centrar las variables predictoras
  #step_scale(all_numeric(), -all_outcomes()) %>%  # escalar las variables predictoras
  #  step_mutate_at( all_numeric(), fn = ~  m_dist(.)) %>%
  #               outlier1=quantile(outlier,.01),
  #              outlier2=quantile(outlier,.99)) %>%
  # step_filter( outlier > outlier1, outlier < outlier2) %>%
  
  #step_nzv(all_numeric(), -all_outcomes())%>%
#step_pca(all_numeric(), threshold = .75) %>%
prep()

datos_pp <- bake(receta1, new_data = x3) 


receta2 <- x3 %>%
  recipe(tenencia_dpf ~ .) %>%         # variable a dependiente
  add_role(NO_CREDITO, new_role = "no_borrar")%>%
  add_role(id_cliente, new_role = "no_borrar")%>%
  add_role(grupo, new_role = "no_borrar")%>%
  step_rm(tenencia, tenencia_tdc, tenencia_pp,anomes_producto) %>%   # eliminar columnas
  step_naomit(all_outcomes(), all_predictors())%>%   #ominir NA's de todos las variables
  #step_log(SALDOS_VISTA_1SEM, SALDOS_VISTA_6M, SALDO, INGRESO_ESTIMADO) %>%            #tomar el logaritmo
  #step_mutate(N_TC = ifelse(N_TC == "1",1,0)) %>%   #convesi?n logica
  #step_dumm☻y(all_nominal(), -all_outcomes()) %>% # conversi?n de variables como dummys
  #step_center(all_numeric(), -all_outcomes()) %>%  # centrar las variables predictoras
  #step_scale(all_numeric(), -all_outcomes()) %>%  # escalar las variables predictoras
  #  step_mutate_at( all_numeric(), fn = ~  m_dist(.)) %>%
  #               outlier1=quantile(outlier,.01),
  #              outlier2=quantile(outlier,.99)) %>%
  # step_filter( outlier > outlier1, outlier < outlier2) %>%
  
  #step_nzv(all_numeric(), -all_outcomes())%>%
#step_pca(all_numeric(), threshold = .75) %>%
prep()

datos_dpf <- bake(receta2, new_data = x3) 



############################################################
# Utilizando H20
############################################################
library(h2o)

h2o.init(ip = "localhost",
         # -1 indica que se empleen todos los cores disponibles.
         nthreads = -1,
         # Máxima memoria disponible para el cluster.
         max_mem_size = "6g")

###################
# Modelo TDC
###################

datos <- as.h2o(datos_tdc)

splits <- h2o.splitFrame(datos,c(0.7,0.1),seed=1234)
train <- h2o.assign(splits[[1]], "train.hex")  #70% 
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

h2o.describe(train)
h2o.describe(test)


# Modelos para TDC
y <- "tenencia_tdc"
x <- setdiff(names(train), c(y,"id_cliente", "NO_CREDITO", "grupo"))

# convirtiendo el valor factor
train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])
valid[, y] <- as.factor(valid[, y])

############################################
# 1 sin dividir por grupo
############################################
h2o.table(datos[, "tenencia_tdc"])

aml_tdc <- h2o.automl(y = y,
                      x = x,
                      training_frame = train,
                      max_models = 10,
                      seed = 1,
                      validation_frame = valid
                                      #,balance_classes = TRUE
)

lb <- aml_tdc@leaderboard
print(lb)
print(lb, n = nrow(lb))
model_ids <- as.data.frame(aml_tdc@leaderboard$model_id)[,1]
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
metalearner_tdc <- h2o.getModel(se@model$metalearner$name)
h2o.varimp(metalearner_tdc)
h2o.varimp_plot(metalearner_tdc)

pred_class_metaleraner_tdc = h2o.predict(metalearner_tdc, test) %>% as.data.frame() %>% pull(predict)
metricas_metaleraner_tdc = h2o.performance(model = metalearner_tdc, newdata = test)
h2o.confusionMatrix(metricas_metaleraner_tdc)
h2o.performance(model = metalearner_tdc, newdata = test)@metrics$AUC


se1 <- h2o.getModel(grep("GBM_5", model_ids, value = TRUE)[1])
gbm_tdc <- h2o.getModel(se1@model_id)
h2o.varimp(gbm_tdc)
h2o.varimp_plot(gbm_tdc)

pred_class_gbm_tdc = h2o.predict(gbm_tdc, test) %>% as.data.frame() %>% pull(predict)
metricas_gbm_tdc = h2o.performance(model = gbm_tdc, newdata = test)
h2o.confusionMatrix(metricas_gbm_tdc)
h2o.performance(model = gbm_tdc, newdata = test)@metrics$AUC

#Guardar el modelo
metalearner_diario_tdc= h2o.download_model(metalearner_tdc, path = "C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020")
gbm_diario_tdc= h2o.download_model(gbm_tdc, path = "C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020")

# ############################################
# # 2 dividiendo por grupo
# ############################################
# table.H2OFrame(datos[, "grupo"])
# tdc_g1 <- datos[datos[, "grupo"] == "g1" ,]
# 
# splits <- h2o.splitFrame(tdc_g1,c(0.7,0.1),seed=1234)
# train <- h2o.assign(splits[[1]], "train.hex")  #70% 
# valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
# test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex
# 
# h2o.describe(train)
# h2o.describe(test)
# 
# 
# # Modelos para TDC
# y <- "tenencia_tdc"
# x <- setdiff(names(train), c(y,"id_cliente", "NO_CREDITO", "grupo"))
# 
# # convirtiendo el valor factor
# train[, y] <- as.factor(train[, y])
# test[, y] <- as.factor(test[, y])
# valid[, y] <- as.factor(valid[, y])
# 
# ############################################
# # Modelo g1
# ############################################
# 
# aml_tdc_g1 <- h2o.automl(y = y,
#                       x = x,
#                       training_frame = train,
#                       max_models = 10,
#                       seed = 1,
#                       validation_frame = valid
#                       #,balance_classes = TRUE
# )
# 
# lb_g1 <- aml_tdc_g1@leaderboard
# print(lb_g1)
# print(lb_g1, n = nrow(lb_g1))
# model_ids <- as.data.frame(aml_tdc_g1@leaderboard$model_id)[,1]
# se_g1 <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
# metalearner_tdc_g1 <- h2o.getModel(se_g1@model$metalearner$name)
# h2o.varimp(metalearner_tdc_g1)
# h2o.varimp_plot(metalearner_tdc_g1)

###################
# Modelo PP
###################

datos <- as.h2o(datos_pp)

splits <- h2o.splitFrame(datos,c(0.7,0.1),seed=1234)
train <- h2o.assign(splits[[1]], "train.hex")  #70% 
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

#h2o.describe(train)
#h2o.describe(test)


# Modelos para TDC
y <- "tenencia_pp"
x <- setdiff(names(train), c(y,"id_cliente", "NO_CREDITO", "grupo", "tenencia_tdc"))

# convirtiendo el valor factor
train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])
valid[, y] <- as.factor(valid[, y])

############################################
# 1 sin dividir por grupo
############################################
h2o.table(datos[, "tenencia_pp"])

aml_pp <- h2o.automl(y = y,
                      x = x,
                      training_frame = train,
                      max_models = 10,
                      seed = 1,
                      validation_frame = valid
                      #,balance_classes = TRUE
)

lb_pp <- aml_pp@leaderboard
print(lb_pp)
print(lb_pp, n = nrow(lb_pp))
model_ids <- as.data.frame(aml_pp@leaderboard$model_id)[,1]
se <- h2o.getModel(grep("StackedEnsemble_BestOfFamily", model_ids, value = TRUE)[1])
metalearner_pp <- h2o.getModel(se@model$metalearner$name)
h2o.varimp(metalearner_pp)
h2o.varimp_plot(metalearner_pp)

pred_class_metaleraner_pp = h2o.predict(metalearner_pp, test) %>% as.data.frame() %>% pull(predict)
metricas_metaleraner_pp = h2o.performance(model = metalearner_pp, newdata = test)
h2o.confusionMatrix(metricas_metaleraner_pp)
h2o.performance(model = metalearner_pp, newdata = test)@metrics$AUC


se1 <- h2o.getModel(grep("GBM_5", model_ids, value = TRUE)[1])
gbm_pp <- h2o.getModel(se1@model_id)
h2o.varimp(gbm_pp)
h2o.varimp_plot(gbm_pp)

pred_class_gbm_pp = h2o.predict(gbm_pp, test) %>% as.data.frame() %>% pull(predict)
metricas_gbm_pp = h2o.performance(model = gbm_pp, newdata = test)
h2o.confusionMatrix(metricas_gbm_pp)
h2o.performance(model = gbm_pp, newdata = test)@metrics$AUC

#Guardar el modelo
metalearner_diario_pp= h2o.download_model(metalearner_pp, path = "C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020")
gbm_diario_pp= h2o.download_model(gbm_pp, path = "C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020")

###################
# Modelo DPF
###################

datos <- as.h2o(datos_dpf)

splits <- h2o.splitFrame(datos,c(0.7,0.1),seed=1234)
train <- h2o.assign(splits[[1]], "train.hex")  #70% 
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

#h2o.describe(train)
#h2o.describe(test)


# Modelos para TDC
y <- "tenencia_dpf"
x <- setdiff(names(train), c(y,"id_cliente", "NO_CREDITO", "grupo", "tenencia_tdc", "saldo"))

# convirtiendo el valor factor
train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])
valid[, y] <- as.factor(valid[, y])

############################################
# 1 sin dividir por grupo
############################################
h2o.table(datos[, "tenencia_dpf"])

aml_dpf <- h2o.automl(y = y,
                     x = x,
                     training_frame = train,
                     max_models = 10,
                     seed = 1,
                     validation_frame = valid
                     #,balance_classes = TRUE
)

lb_dpf <- aml_dpf@leaderboard
print(lb_dpf)
print(lb_dpf, n = nrow(lb_dpf))
model_ids <- as.data.frame(aml_dpf@leaderboard$model_id)[,1]
se <- h2o.getModel(grep("StackedEnsemble_BestOfFamily", model_ids, value = TRUE)[1])
metalearner_dpf <- h2o.getModel(se@model$metalearner$name)
h2o.varimp(metalearner_dpf)
h2o.varimp_plot(metalearner_dpf)

pred_class_metaleraner_dpf = h2o.predict(metalearner_dpf, test) %>% as.data.frame() %>% pull(predict)
metricas_metaleraner_dpf = h2o.performance(model = metalearner_dpf, newdata = test)
h2o.confusionMatrix(metricas_metaleraner_dpf)
h2o.performance(model = metalearner_dpf, newdata = test)@metrics$AUC


se1 <- h2o.getModel(grep("DRF_1", model_ids, value = TRUE)[1])
drf_dpf <- h2o.getModel(se1@model_id)
h2o.varimp(drf_dpf)
h2o.varimp_plot(drf_dpf)

pred_class_drf_dpf = h2o.predict(drf_dpf, test) %>% as.data.frame() %>% pull(predict)
metricas_drf_dpf = h2o.performance(model = drf_dpf, newdata = test)
h2o.confusionMatrix(metricas_drf_dpf)
h2o.performance(model = drf_dpf, newdata = test)@metrics$AUC

#Guardar el modelo
metalearner_diario_dpf= h2o.download_model(metalearner_dpf, path = "C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020")
drf_diario_drf_dpf= h2o.download_model(drf_dpf, path = "C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020")


#
h2o.shutdown()


####################################
# ARCHIVO DE CALCULO 
####################################
rm(list=ls(all=TRUE))

library(tidymodels)
library(tidyverse)
library(skimr) # AED
library(readxl)
library(h2o)


#setwd("C:/1 N5/Modelos/MODELO2020/MODELO_PROPENSION_2020")

setwd( "C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020")

# CARGA DE ARCHIVOS MOVIMIENTOS DIARIOS
x <- read_delim("MOVIMIENTO_DIARIO.rpt", 
                "~", escape_double = FALSE, col_names = TRUE, 
                col_types = cols(SALDO = col_double(),
                                 SALDO_PROMEDIO= col_double(),
                                 CREDITOS_MONTO= col_double(),
                                 CREDITOS_TXN= col_double(),
                                 DEBITOS_MONTO= col_double(),
                                 DEBITOS_TXN= col_double()), 
                locale = locale(decimal_mark = "."), 
                trim_ws = TRUE)

colnames(x) <- c('id_cliente','fecha','max_meses','saldo','saldo_promedio','creditos_monto','creditos_tx','debitos_monto','debitos_tx')

#glimpse(x)
#z1 <- x[1:10,]

# Convirtiendo variables
variable_factor = function(x) {as.factor(x)}
variable_character = function(x) {as.character(x)}
variable_numerica = function(x) {as.numeric(as.character(x))}
variable_tenencia <- function(x) {as.factor(if_else(x == 0, "No", "Si"))}
f1 = function(x) {ifelse(is.na(x),0,x)}


#cliente ejemplo
#z = x %>% filter(id_cliente == '155670130-2-2018')

x1 <- x%>%
  dplyr::select(-fecha)%>%
  mutate(creditos_monto = f1(creditos_monto),
         creditos_tx = f1(creditos_tx),
         debitos_monto = f1(debitos_monto),
         debitos_tx = f1(debitos_tx))

skim(x1)
#z = x1 %>% filter(id_cliente == '8-782-2109')


h2o.init(ip = "localhost",
         # -1 indica que se empleen todos los cores disponibles.
         nthreads = -1,
         # Máxima memoria disponible para el cluster.
         max_mem_size = "6g")


datos <- as.h2o(x1)
#setwd("C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020")

modelopp = h2o.upload_model("GBM_5_AutoML_20200928_160105")
modelotdc = h2o.upload_model("GBM_5_AutoML_20200928_143609")
modelodpf = h2o.upload_model("DRF_1_AutoML_20200928_163219")

pred_dpf = as.data.frame(h2o.cbind(datos[,1],h2o.predict(modelodpf, datos)[,3]))
pred_tdc = as.data.frame(h2o.cbind(datos[,1],h2o.predict(modelotdc, datos)[,3]))
pred_pp = as.data.frame(h2o.cbind(datos[,1],h2o.predict(modelopp, datos)[,3]))

predicciones <- pred_dpf%>%
  left_join(pred_pp, by = "id_cliente")%>%
  left_join(pred_tdc, by = "id_cliente")%>%
  group_by(id_cliente)%>%
  summarise(dpf = max(p2.x),
            tdc = max(p2.y),
            pp = max(p2))

head(predicciones)
skim(predicciones)

write_csv(predicciones, "predicciones1.csv")

h2o.shutdown() # se utiliza para terminar el cluster

#############################################################################
#############################################################################
#
#                                           MODELO CARET
#
#############################################################################
# Version: V1
# Fecha: 18/12/2019
# Autor: Victor Blanco
#############################################################################
#############################################################################




# rm(list = c("a1", "datos1","datos2","datos3" ,"datos4","datos4_nest","datos4_nest1","fechas","percentiles","probs","separa_datos" ,     
#             "variable_character", "variable_factor","variable_numerica"," variable_tenencia"))


rm(list=ls(all=TRUE))

library(readr)
library(tidyverse)
library(rsample)
library(caret)
library(recipes)

clientes <- read_csv("clientes.csv")
datos4_nest2 <- read_csv("datos4_nes_2.csv")

# Atipicos
m_dist <- function(x) { mahalanobis(x,colMeans(x),cov(x))} 

glimpse(datos4_nest2)
datos4_nest2$outlier <- m_dist(datos4_nest2[,c(4,9)])
summary(datos4_nest2)

set.seed(123)
separa_datos <- initial_time_split(clientes,
                                   prop = 0.3)

#Saber cual variable es un factor
#x= data.frame(map_dbl(datos4_nest2, .f = function(x){is.character(x)}))

tbl_entrenar1 <- training(separa_datos)      #tabla de entrenamiento

tbl_entrenar <- datos4_nest2%>% 
                    dplyr::semi_join(tbl_entrenar1, by= "id_cliente")


tbl_prueba1 <- testing(separa_datos)         #tabla de prueba

tbl_prueba <- datos4_nest2%>% 
                dplyr::semi_join(tbl_prueba1, by= "id_cliente")


#nrow(tbl_entrenar) + nrow(tbl_prueba) 


# set.seed(234)
# val_set <- validation_split(tbl_entrenar,    #aplicar set validation
#                             strata = id_cliente, 
#                             prop = 0.80)

# Estadisticas
tbl_entrenar %>% group_by(tenencia_tdc) %>% summarise(n())
tbl_prueba %>% group_by(tenencia_tdc) %>% summarise(n())

# # Antes de separa datos balanceo 
set.seed(9560)
# # Balancear a la clase mas baja

down_train <- downSample(x = tbl_entrenar,
                          y = as.factor(tbl_entrenar$tenencia_tdc))

table(down_train$tenencia_tdc)
glimpse(down_train)

# 
# # Balancear a la clase mas alta
# set.seed(9560)
# up_train <- upSample(x = datos4_nest2[, -ncol(datos4_nest2)],
#                      y = datos4_nest2$tenencia_tdc)
# table(up_train$tenencia_tdc)  

xz <- tbl_entrenar %>%
  mutate( tenencia_tdc= case_when( tenencia_tdc == 1 ~"yes",
                                        TRUE ~"no"),
               outlier1=quantile(outlier,.01),
               outlier2=quantile(outlier,.99))
  
glimpse(xz)


#Limpiando los datos
receta <- tbl_entrenar %>%
  recipe(tenencia_tdc ~ .) %>%         # variable a dependiente
  step_mutate( tenencia_tdc= case_when( tenencia_tdc == 1 ~"yes",
                                        TRUE ~"no"),
               outlier1=quantile(outlier,.01),
               outlier2=quantile(outlier,.99)) %>%
  step_filter( outlier > outlier1, outlier < outlier2) %>%
  step_rm(NO_CREDITO, tenencia, tenencia_pp, anomes_producto, grupo, id_cliente, X1, outlier) %>%   # eliminar columnas
  step_naomit(all_outcomes(), all_predictors())%>%   #ominir NA's de todos las variables
  #step_log(SALDOS_VISTA_1SEM, SALDOS_VISTA_6M, SALDO, INGRESO_ESTIMADO) %>%            #tomar el logaritmo
  #step_mutate(N_TC = ifelse(N_TC == "1",1,0)) %>%   #convesi?n logica
  #step_dummy(all_nominal(), -all_outcomes()) %>% # conversi?n de variables como dummys
  step_center(all_numeric(), -all_outcomes()) %>%  # centrar las variables predictoras
  step_scale(all_numeric(), -all_outcomes()) %>%  # escalar las variables predictoras
  step_nzv(all_numeric(), -all_outcomes())%>%
  #step_pca(all_predictors(), threshold = .75) %>%
  prep()

# Se aplican las transformaciones al conjunto de entrenamiento y de test
#datos_train_prep <- bake(receta, new_data = tbl_entrenar)
datos_test_prep  <- bake(receta, new_data = tbl_prueba)
datos_train_prep <- bake(receta, new_data = down_train)

summary(datos_train_prep)
summary(datos_test_prep)

#levels(datos_train_prep$tenencia_tdc)
#levels(datos_test_prep$tenencia_tdc)


# Modelos
library(tidymodels)
library(modelgrid)
library(caret)


grid_modelos <- model_grid()
grid_modelos

seeds <- vector(mode = "list", length = nrow(tbl_entrenar) + 1)
seeds <- lapply(seeds, function(x) 1:10)

head(seeds)


# Definir tunning

grid_modelos <- grid_modelos %>%
  share_settings(
    y = datos_train_prep$tenencia_tdc,                     # Variable dependiente
    x = datos_train_prep %>% select(-tenencia_tdc),        # Variables predictoras 
    metric = "Accuracy",                           # metrica para el modelo
    trControl = trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 5,
                             returnResamp = "final",
                             verboseIter = FALSE,
                             allowParallel = TRUE,
                             classProbs = TRUE,
                             savePredictions = TRUE
                             # ,summaryFunction = twoClassSummary # si se utiliza toma por defecto la metrica de ROC y se pierden los resultados de Kappa y Accuracy
    )
  )


# Modelos a entrenar

# install.packages("adabag")
# library(adabag)

grid_modelos <- grid_modelos %>%
   add_model(
     model_name = "Reg_logistica",            # nombre
     method     = "glm",                      # metodo
     family     = binomial(link = "logit")    
   )  %>%
   add_model(
     model_name = "RandomForest",
     method     = "ranger",
     num.trees  = 5,
     tuneGrid   = expand.grid(
       mtry = c(3, 4),
       min.node.size = c(200, 1000, 5000),
       splitrule = "gini"
     )
   ) %>%
   add_model(
     model_name = "ada_boost1",
     method     = "ada_boost",
     tuneGrid   = expand.grid( 
       mfinal= 50,
         maxdepth= 4)
  ) #%>%
  # add_model(
  #   model_name = "SVM",
  #   method = "svmRadial",
  #   tuneGrid   = expand.grid(
  #     sigma = c(0.001, 0.01, 0.1, 0.5, 1),
  #     C = c(1 , 20, 50, 100, 200, 500, 700)
  #   )
  # )
grid_modelos$models



# 4) Entrenar los modelos

# Detectar el total de cores a disposición
cores <- parallel::detectCores()
cores


grid_modelos <- caret::train(grid_modelos, train_all = FALSE, resample_seed = 123)
grid_modelos$model_fits

# Revisar modelos finales
grid_modelos$model_fits$RandomForest$finalModel
grid_modelos$model_fits$ada_boost$result


# Extracci?n de las m?tricas de validaci?n cruzada.
metricas_cv <- caret::resamples(x = grid_modelos$model_fits) # Visualizacion de resultados
metricas_cv$values %>% head()

# Predicci?n
predicciones <- extractPrediction(
  models = grid_modelos$model_fits,
  testX = datos_test_prep, # Solo se pueden ingresar en los datos las variables 
  # con las que se trabajo el modelo
  testY = datos_test_prep$tenencia_tdc
)
predicciones %>% head()


# Comparacion de modelos
names(grid_modelos$model_fits)

grid_modelos$model_fits %>%
  caret::resamples(.) %>%
  summary(.)

grid_modelos$model_fits %>%
  caret::resamples(.) %>%
  bwplot(.)



# MODELO 1
grid_modelos$model_fits$Reg_logistica

modelo_logistico <- train(tenencia_tdc ~ .,
                          data = datos_train_prep,
                          method = "glm",
                          family = "binomial",
                          tuneGrid = data.frame(parameter = "none"),
                          metric = "Accuracy",
                          trControl = trainControl(method = "repeatedcv",
                                                   number = 10,
                                                   repeats = 5,
                                                   returnResamp = "final",
                                                   verboseIter = FALSE,
                                                   allowParallel = TRUE,
                                                   classProbs = TRUE
                          )
) 

summary(modelo_logistico$finalModel)                                                    

# Matriz de confusion
confusionMatrix(data = predict(modelo_logistico,
                               newdata = datos_test_prep,
                               type = "raw"),
                reference = datos_test_prep$tenencia_tdc,
                positive = "yes")

predicciones_log <- predict(object = modelo_logistico,
                            newdata = datos_test_prep,
                            type = "prob")


# MODELO 2
grid_modelos$model_fits$RandomForest

modelo_rf <- train(tenencia_tdc ~ .,
                   data = datos_train_prep,
                   method = "ranger",
                   tuneGrid = expand.grid(mtry = 3,
                                          min.node.size = 1000,
                                          splitrule = "gini"),
                   metric = "Accuracy",
                   num.trees = 200,
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10,
                                            repeats = 5,
                                            returnResamp = "final",
                                            verboseIter = FALSE,
                                            allowParallel = TRUE,
                                            classProbs = TRUE
                                            
                   )
) 

modelo_rf$finalModel                                               

# Matriz de confusion
confusionMatrix(data = predict(modelo_rf,
                               newdata = datos_test_prep,
                               type = "raw"),
                reference = datos_test_prep$tenencia_tdc,
                positive = "yes")

predicciones_rf <- predict(object = modelo_rf,
                           newdata = datos_test_prep,
                           type = "prob")


# Calculos de la curva ROC

# Funcion de ROC
# En esta funcion se debe cambiar la variable respuestas y los niveles

test_roc <- function(model, data) {
  library(pROC)
  roc_obj <- roc(data$tenencia_tdc, 
                 predict(model, data, type = "prob")[, "yes"],
                 levels = c("no", "yes")
                  ,plot = TRUE
  )
  ci(roc_obj)
  plot.roc(roc_obj, add = TRUE)
}


# Modelos a evaluar
modelos = list(rf = modelo_rf,
               rl = modelo_logistico )

# Aplica el calculo de ROC a todos los modelos
outside_test <- lapply(modelos, test_roc, data = datos_test_prep)
# Convierte en un vector
outside_test <- lapply(outside_test, as.vector)
# Une las filas
outside_test <- do.call("rbind", outside_test)
# Coloca nombres
colnames(outside_test) <- c("lower", "ROC", "upper")
# Lo convierte en tabla
outside_test <- as.data.frame(outside_test)
outside_test

# Uniendo los resultados de los modelos
#Stacking Algorithms

result <- resamples(grid_modelos)

caretLis

# Elegir entre otros modelos basados en disimilaridad de jaccard
# 
# tag <- read.csv("tag_data.csv", row.names = 1)
# tag <- as.matrix(tag)
# 
# ## Select only models for regression
# regModels <- tag[tag[,"Two.Class.Only"] == 1,]
# 
# all <- 1:nrow(regModels)
# ## Seed the analysis with the SVM model
# start <- grep("(glm)", rownames(regModels), fixed = TRUE)
# pool <- all[all != start]
# 
# ## Select 4 model models by maximizing the Jaccard
# ## dissimilarity between sets of models
# nextMods <- maxDissim(regModels[start,,drop = FALSE], 
#                       regModels[pool, ], 
#                       method = "Jaccard",
#                       n = 4)
# 
# rownames(regModels)[c(start, nextMods)]

