#############################################################################
#############################################################################
#
#                      MODELO PROPENSION PRODUCTOS
#                        PROYECCIONES MENSUALES
#
#############################################################################
# Version: V1
# Fecha: 18/09/2020
# Autor: Victor Blanco
#############################################################################
#############################################################################


rm(list=ls(all=TRUE))
#install.packages("DataExplorer")


library(tidymodels)
library(tidyverse)
library(skimr) # AED
library(raster) # calculo del coef. variacion
library(DataExplorer)
library(readxl)

setwd("C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020")

x <- read_delim("CLIENTES.rpt", 
                "~", escape_double = FALSE,
                col_names = TRUE, 
                col_types = cols(DIAS_SIN_USO_TC_NUEVA = col_double(),
                                 SALDO_CA= col_double(),
                                 SALDOS_VISTA= col_double(),
                                 SALDO_PASIVOS= col_double(),
                                 SALDOS_VISTA_6M= col_double(),
                                 SALDOS_VISTA_1SEM= col_double(),
                                 MAX_SALDOPROM= col_double(),
                                 SDO_CPLANI= col_double(),
                                 VIDA= col_double(),
                                 IMPORTE= col_double(),
                                 Negativa= col_double(),
                                 Positiva= col_double(),
                                 Neutral= col_double(),
                                 USO_TOTAL_TC= col_double(),
                                 INGRESOS= col_double(),
                                 SALARIO= col_double(),
                                 ANTIGUEDAD= col_double(),
                                 CANTIDAD= col_double(),
                                 MONTO_ORIGINAL= col_double(),
                                 V12_1= col_double(),
                                 V12_2= col_double(),
                                 V12_3= col_double(),
                                 V12_MAYOR_3= col_double(),
                                 NUMDEPEND= col_double(),
                                 DIA_ULTMOV_TC= col_double(),
                                 DIA_ULTMOV_CA= col_double(),
                                 INGRESO_ESTIMADO= col_double(),
                                 TRABAJO_MESES_CONTINUIDAD= col_double()), 
                locale = locale(decimal_mark = ","), 
                trim_ws = TRUE)

head(x)
#skim(x)

c1=x %>%
  head()

###################################################################################
# Tratamiento de valores perdidos
###################################################################################

x %>%
  select( starts_with('SALDO'))%>%
  map(summary)


x %>%
  dplyr::select(starts_with('SALDO'))  %>%
  DataExplorer::plot_missing(ggtheme = theme_gray())


x %>%
  dplyr::select(starts_with('SALDO'))  %>%
  DataExplorer::plot_missing(ggtheme = theme_gray())

x %>% 
  dplyr::select(starts_with('G_'))  %>%
  plot_bar(ggtheme = theme_gray(), ncol = 2, nrow = 4)


x %>% 
  dplyr::select(starts_with('SALDO'))  %>%
  plot_histogram(ggtheme = theme_gray(), ncol = 2, nrow = 4, scale_x = "log10")


# # Mediana por productos
# z1=x %>%
#   filter(COD_TIPOCLIENTE =='1')%>%
#   group_by(PRODUCTO)%>%
#   summarise(n(),
#             med_salario =median(SALARIO, na.rm = TRUE),
#             variacion= cv(SALARIO, na.rm = TRUE))
# 
# 
# # Mediana de ingresos por ocupacion
# z3=x %>%
#   filter(COD_TIPOCLIENTE =='1')%>%
#   group_by(CARGO_ACTUAL)%>%
#   summarise(n(),
#             med_salario =median(SALARIO, na.rm = TRUE),
#             variacion= cv(SALARIO, na.rm = TRUE))


# Anos de estudio
x%>%
  group_by(DESC_EDUCACION)%>%
  summarise(n())

# Mediana de ingresos por ocupacion

# TABLA DE OCUPACION HOMOLOGADA CON OIT

OCUPACION <- read_excel("C:/1 Riesgo/Cubo de informacion/OCUPACION.xlsx")

OCUPACION1<- OCUPACION%>%
  dplyr::select(PROFESION, OCUPACIONES)%>%    
  group_by(PROFESION, OCUPACIONES)%>%
  summarise(n())

x1 <- x%>%
  dplyr::select(ID_CLIENTE, DESC_PROFESION,SALARIO)%>%
  group_by(ID_CLIENTE, DESC_PROFESION)%>%
  summarise(salario = mean(SALARIO, na.rm = TRUE))%>%
  left_join(OCUPACION1, by= c("DESC_PROFESION"="PROFESION"))

SALARIO_OCUPACION <- x1%>%
                    dplyr::select(-ID_CLIENTE)%>%
                    group_by(OCUPACIONES)%>%
                    nest()%>%
                    mutate(mediana=map(.x= data,  .f = ~median(.x$salario ,na.rm = TRUE)))%>%
                  unnest()


# Vida util por relacion
VIDA_MERCADO=x %>%
  filter(ESTATUS== 'CANCELADA')%>%
  group_by(RELACION1)%>%
  summarise(vida_util= median(VIDA, na.rm = TRUE) )  


x1 <- x %>%
    # dplyr::select(ID_CLIENTE, OCUPACION, SALARIO)%>%
     left_join(SALARIO_OCUPACION, by=c('ID_CLIENTE'))%>%
     left_join(VIDA_MERCADO, by=c('RELACION1'))%>%
     mutate(SALARIO1 = if_else(is.na(SALARIO), mediana, SALARIO),
            VIDA1= case_when( 
              is.na(VIDA) & RELACION1 == 'OTROS' ~  vida_util,
              is.na(VIDA) & RELACION1 == 'PREST. PERSONAL' ~  vida_util,
              is.na(VIDA) & RELACION1 == 'TARJ. CREDITO' ~  vida_util,
              TRUE ~ VIDA),
            N_TC1 = if_else(N_TC==0, 0, 1),
            N_PP1 = if_else(N_PP==0, 0, 1),
            N_DPF1 = if_else(N_DPF==0, 0, 1),
            ANO_ESTUDIO = case_when(
              DESC_EDUCACION == 'BÁSICO INCOMPLETOS' ~ 5,
              DESC_EDUCACION == 'BÁSICO COMPLETOS' ~ 8,
              DESC_EDUCACION == 'MEDIOS INCOMPLETOS' ~ 10,
              DESC_EDUCACION == 'MEDIOS COMPLETOS' ~ 12,
              DESC_EDUCACION == 'TÉCNICOS' ~ 15,
              DESC_EDUCACION == 'UNIVERSITARIOS INCOMPLETO' | DESC_EDUCACION == 'MEDIOS INCOMPLETOS' ~ 15,
              DESC_EDUCACION == 'UNIVERSITARIOS COMPLETOS' ~ 17,
              TRUE ~ 99))#%>%head(100)


# x1%>%
#   group_by(DESC_EDUCACION)%>%
#   summarise(n(), 
#             estudio = mean(ANO_ESTUDIO, na.rm = TRUE))



DISTANCIAS_CIUDADES <- read_excel("DISTANCIAS CIUDADES.xlsx", 
                                  col_types = c("blank", "text", "blank", 
                                                "blank", "blank", "numeric"))

x1<- x1%>%
  left_join(DISTANCIAS_CIUDADES, by=c('RESIDENCIA_CORREGIMIENTO'))


# x1%>%
#   group_by(RESIDENCIA_CORREGIMIENTO)%>%
#   summarise(n(), Distancia_km = mean(Distancia_km))
# 
# c1=x1 %>%
#   head(100)


tipo_variables <- sapply(x1, class)

#Datos numericos
datos_n_numeric <- x1 %>%
  dplyr::select(ID_CLIENTE,which(tipo_variables=="numeric"))%>% 
  map_df(.,function(x1) ifelse(is.na(x1),0,x1))

#Datos no_numericos
datos_n_caracter <- x1 %>%
  dplyr::select(ID_CLIENTE,which(tipo_variables!="numeric"))

names(datos_n_caracter)
# datos_n_caracter%>%
#   group_by(RESIDENCIA_PROVINCIA)%>%
#   summarise(n())


datos_n_caracter <- datos_n_caracter %>%
    mutate(GENERO1 = if_else(GENERO %in% c('F', 'M'), GENERO, 'ND' ),
           ESTADO_CIVIL1 = if_else(ESTADO_CIVIL %in% c('CASADO/A', 'DIVORCIADO/A', 'SOLTERO/A', 'UNIDO/A', 'VIUDO/A'), ESTADO_CIVIL, 'ND' ))    

# xx<-x%>%
#   group_by(RESIDENCIA_CORREGIMIENTO)%>%
#   summarise(n()
#             ,SALDO_V= mean(SALDOS_VISTA, na.rm = TRUE)/n()
#             ,SALDO_PP= mean(SALDO_TOTAL_PP, na.rm = TRUE)/n())
# 
# write.csv(xx, "corregimientos.csv")


x2 <- datos_n_caracter%>%
  dplyr::select(-c('GENERO', 'ESTADO_CIVIL', 'DESC_PROFESION.y', 'DESC_PROFESION.x','n()', 'OCUPACION'))%>%
  bind_cols(datos_n_numeric)


# c2=x2 %>%
#   #filter(ID_CLIENTE...1 =='E-8-104083')
#   head(10)
# 
# names(x2)
# 
# x2 %>% 
#   dplyr::select(starts_with('SALDO'))  %>%
#   plot_histogram(ggtheme = theme_gray(), ncol = 2, nrow = 4, scale_x = "log10")
# 
# x2 %>% 
#   dplyr::select(starts_with('SALARIO'))  %>%
#   plot_histogram(ggtheme = theme_gray(), ncol = 2, nrow = 4, scale_x = "log10")
# 
# 
# skim(x2)
# 
# x2 %>% 
#   dplyr::select("RESIDENCIA_PAIS")%>%
#   plot_bar(ggtheme = theme_gray())
# 
# #names(x2)
# 
# x21 <- x2 %>% 
#   dplyr::select(c('DESC_VIVALQ','RESIDENCIA_PAIS','RESIDENCIA_PROVINCIA','TRABAJO_PAIS', 
#                     'TRABAJO_PROVINCIA', 'TRABAJO_DISTRITO','TRABAJO_CIUDAD','ACTIVIDAD_ECONOMICA','CARGO_ACTUAL',
#                     'OCUPACIONES','GENERO1','ESTADO_CIVIL1', 'ID_CLIENTE...1'))
# head(x21)
#


x3 <- x2 %>% 
  filter(!is.na(ID_CLIENTE...1), JUBILADO == 0)%>%
  dplyr::select(- c("salario","vida_util","mediana","ID_CLIENTE...22","SALARIO","FECHA_ULTMOV_CC","VIDA", 
                    "INGRESOS", "INGRESO_ESTIMADO", "N_TC", "N_PP", "N_DPF", 
                    'RESIDENCIA_CORREGIMIENTO', 'DESC_EDUCACION', 'DESC_VIVALQ','RESIDENCIA_PAIS',              
                    'RESIDENCIA_PROVINCIA', 'RESIDENCIA_DISTRITO','RESIDENCIA_CIUDAD','TRABAJO_PAIS', 
                    'TRABAJO_PROVINCIA', 'TRABAJO_DISTRITO','TRABAJO_CIUDAD','ACTIVIDAD_ECONOMICA','CARGO_ACTUAL',
                    'OCUPACIONES','GENERO1','ESTADO_CIVIL1'))


# GUARDADO DEL ARCHIVO QUE USO PARA MODELAR
write.csv(x3, "x3")


names(x3)
sapply(x3, class)
skim(x3)

receta <- x3%>%
  recipe(N_TC1 ~.)%>%
  step_nzv(all_numeric())%>%
  prep()


x4_tdc  <- bake(receta, new_data = x3)
skim(x4_tdc)

x4_tdc%>%
     group_by(N_TC1)%>%
     summarise(n())
   


############################################################
# Utilizando H20
############################################################
library(h2o)

h2o.init(ip = "localhost",
         # -1 indica que se empleen todos los cores disponibles.
         nthreads = -1,
         # Máxima memoria disponible para el cluster.
         max_mem_size = "6g")

datos <- as.h2o(x4_tdc)

splits <- h2o.splitFrame(datos,c(0.7,0.1),seed=1234)
train <- h2o.assign(splits[[1]], "train.hex")  #70% 
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

h2o.describe(train)
h2o.describe(test)


# Modelos para TDC
y <- "N_TC1"
x <- setdiff(names(train), c(y,"ID_CLIENTE...1", "ESTATUS", "RELACION1", "PRODUCTO", "JUBILADO", "CLIENTE_VIGENTE", "TDC"))

# convirtiendo el valor factor
train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])
valid[, y] <- as.factor(valid[, y])

############################################
# 1 Clientes con TDC mercado ACTIVA
############################################
tdc_mercado_activas <- train[train[, "TDC"] == 1 & train[, "ESTATUS"] == "ACTIVA",]
h2o.describe(tdc_mercado_activas)
h2o.describe(test)

h2o.table(tdc_mercado_activas[, "N_TC1"])

aml_tdc_mercado_activas <- h2o.automl(y = y,
                                      x = x,
                                      training_frame = train,
                                      max_models = 10,
                                      seed = 1,
                                      validation_frame = valid
                                      #,balance_classes = TRUE
                                      )

lb <- aml_tdc_mercado_activas@leaderboard
print(lb)
print(lb, n = nrow(lb))
model_ids <- as.data.frame(aml_tdc_mercado_activas@leaderboard$model_id)[,1]
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
metalearner_tdc_mercado_activas <- h2o.getModel(se@model$metalearner$name)
h2o.varimp(metalearner_tdc_mercado_activas)
h2o.varimp_plot(metalearner_tdc_mercado_activas)

pred_class_metaleraner = h2o.predict(metalearner_tdc_mercado_activas, test) %>% as.data.frame() %>% pull(predict)
metricas_metaleraner = h2o.performance(model = metalearner_tdc_mercado_activas, newdata = test)
h2o.confusionMatrix(metricas_metaleraner)
h2o.performance(model = metalearner, newdata = test)@metrics$AUC


# el modelo de StackedEnsemble_AllModels_AutoML sobrestima verificando el resto de los modelos decidí elegir el de
# mayor importancia en la conformación del ensemble

se1 <- h2o.getModel(grep("GBM_4_AutoML_20200923_110437", model_ids, value = TRUE)[1])
gbm_tdc_mercado_activas <- h2o.getModel(se1@model_id)
h2o.varimp(gbm_tdc_mercado_activas)
h2o.varimp_plot(gbm_tdc_mercado_activas)

pred_class_gbm_mercado_activas = h2o.predict(gbm_tdc_mercado_activas, test) %>% as.data.frame() %>% pull(predict)
metricas_gbm_mercado_activas = h2o.performance(model = gbm_tdc_mercado_activas, newdata = test)
h2o.confusionMatrix(metricas_gbm_mercado_activas)
h2o.performance(model = gbm_tdc_mercado_activas, newdata = test)@metrics$AUC

#Guardar el modelo
d_metalearner= h2o.download_model(metalearner, path = "C:/1 N5/Modelos/MODELO2020/MODELO_PROPENSION_2020")
d_gbm= h2o.download_model(gbm_tdc_mercado_activas, path = "C:/1 N5/Modelos/MODELO2020/MODELO_PROPENSION_2020")

#h2o.download_mojo(aml@leader, path = "./")

#Cargar el modelo
z = h2o.upload_model(d_gbm)
#z@model


######################################
# 2 PRESTAMOS PERSONALES
######################################
receta_pp <- x3%>%
  recipe(N_PP1 ~.)%>%
  step_nzv(all_numeric())%>%
  prep()


x4_pp  <- bake(receta_pp, new_data = x3)
skim(x4_pp)


h2o.init(ip = "localhost",
         # -1 indica que se empleen todos los cores disponibles.
         nthreads = -1,
         # Máxima memoria disponible para el cluster.
         max_mem_size = "6g")

datos <- as.h2o(x4_pp)

splits <- h2o.splitFrame(datos,c(0.7,0.1),seed=1234)
train <- h2o.assign(splits[[1]], "train.hex")  #70% 
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

h2o.describe(train)
h2o.describe(test)


# Modelos para PP
y <- "N_PP1"
x <- setdiff(names(train), c(y,"ID_CLIENTE...1", "ESTATUS", "RELACION1", "PRODUCTO", "JUBILADO", "CLIENTE_VIGENTE", "PP"))

# convirtiendo el valor factor
train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])
valid[, y] <- as.factor(valid[, y])

# h2o.table(train[, "ESTATUS"])
# tdc_mercado_cancelada <- train[train[, "TDC"] == 1 & train[, "ESTATUS"] == "CANCELADA",]
# h2o.describe(tdc_mercado_cancelada)
#h2o.describe(test)

h2o.table(train[, "N_PP1"])

aml_PP <- h2o.automl(y = y,
                     x = x,
                     training_frame = train,
                     max_models = 10,
                     seed = 1,
                     validation_frame = valid
                     #,balance_classes = TRUE
                     )

lb <- aml_PP@leaderboard
print(lb)
print(lb, n = nrow(lb))
model_ids <- as.data.frame(aml_PP@leaderboard$model_id)[,1]
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
metalearner_pp <- h2o.getModel(se@model$metalearner$name)
h2o.varimp(metalearner_pp)
h2o.varimp_plot(metalearner_pp)

pred_class_metaleraner = h2o.predict(metalearner_pp, test) %>% as.data.frame() %>% pull(predict)
metricas_metaleraner = h2o.performance(model = metalearner_pp, newdata = test)
h2o.confusionMatrix(metricas_metaleraner)
h2o.performance(model = metalearner_pp, newdata = test)@metrics$AUC


# el modelo de StackedEnsemble_AllModels_AutoML sobrestima verificando el resto de los modelos decidí elegir el de
# mayor importancia en la conformación del ensemble

se1 <- h2o.getModel(grep("DRF_1_AutoML", model_ids, value = TRUE)[1])
drf_pp <- h2o.getModel(se1@model_id)
h2o.varimp(drf_pp)
h2o.varimp_plot(drf_pp)

pred_class_dfr_pp = h2o.predict(drf_pp, test) %>% as.data.frame() %>% pull(predict)
metricas_drf_pp = h2o.performance(model = drf_pp, newdata = test)
h2o.confusionMatrix(metricas_drf_pp)
h2o.performance(model = drf_pp, newdata = test)@metrics$AUC

#Guardar el modelo
e_metalearner= h2o.download_model(metalearner_pp, path = "C:/1 N5/Modelos/MODELO2020/MODELO_PROPENSION_2020")
e_gbm= h2o.download_model(drf_pp, path = "C:/1 N5/Modelos/MODELO2020/MODELO_PROPENSION_2020")

#h2o.download_mojo(aml@leader, path = "./")

#Cargar el modelo
z = h2o.upload_model(d_gbm)
#z@model

######################################
# 2 DEPOSITOS A PLAZO FIJO
######################################
#table(x3$N_DPF1)
receta_dpf <- x3%>%
  recipe(N_DPF1 ~.)%>%
  step_nzv(all_predictors())%>%
  prep()


x4_dpf  <- bake(receta_dpf, new_data = x3)
skim(x4_dpf)


h2o.init(ip = "localhost",
         # -1 indica que se empleen todos los cores disponibles.
         nthreads = -1,
         # Máxima memoria disponible para el cluster.
         max_mem_size = "6g")

datos <- as.h2o(x4_dpf)

splits <- h2o.splitFrame(datos,c(0.7,0.1),seed=1234)
train <- h2o.assign(splits[[1]], "train.hex")  #70% 
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

h2o.describe(train)
h2o.describe(test)


# Modelos para DPF
y <- "N_DPF1"
x <- setdiff(names(train), c(y,"ID_CLIENTE...1", "ESTATUS", "RELACION1", "PRODUCTO", "CLIENTE_VIGENTE"))

# convirtiendo el valor factor
train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])
valid[, y] <- as.factor(valid[, y])

# h2o.table(train[, "ESTATUS"])
# tdc_mercado_cancelada <- train[train[, "TDC"] == 1 & train[, "ESTATUS"] == "CANCELADA",]
# h2o.describe(tdc_mercado_cancelada)
#h2o.describe(test)

h2o.table(train[, "N_DPF1"])

aml_DPF <- h2o.automl(y = y,
                     x = x,
                     training_frame = train,
                     max_models = 10,
                     seed = 1,
                     validation_frame = valid
                     #,balance_classes = TRUE
)


lb <- aml_DPF@leaderboard
print(lb)
print(lb, n = nrow(lb))
model_ids <- as.data.frame(aml_DPF@leaderboard$model_id)[,1]
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
metalearner_dpf <- h2o.getModel(se@model$metalearner$name)
h2o.varimp(metalearner_dpf)
h2o.varimp_plot(metalearner_dpf)

pred_class_metaleraner = h2o.predict(metalearner_dpf, test) %>% as.data.frame() %>% pull(predict)
metricas_metaleraner = h2o.performance(model = metalearner_dpf, newdata = test)
h2o.confusionMatrix(metricas_metaleraner)
h2o.performance(model = metalearner_dpf, newdata = test)@metrics$AUC


# el modelo de StackedEnsemble_AllModels_AutoML sobrestima verificando el resto de los modelos decidí elegir el de
# mayor importancia en la conformación del ensemble

se1 <- h2o.getModel(grep("DRF_1_AutoML_20200924_115055", model_ids, value = TRUE)[1])
drf_dpf <- h2o.getModel(se1@model_id)
h2o.varimp(drf_dpf)
h2o.varimp_plot(drf_dpf)

pred_class_dfr_dpf = h2o.predict(drf_dpf, test) %>% as.data.frame() %>% pull(predict)
metricas_drf_dpf = h2o.performance(model = drf_dpf, newdata = test)
h2o.confusionMatrix(metricas_drf_dpf)
h2o.performance(model = drf_dpf, newdata = test)@metrics$AUC


#Guardar el modelo
f_metalearner= h2o.download_model(metalearner_dpf, path = "C:/1 N5/Modelos/MODELO2020/MODELO_PROPENSION_2020")
f_drf= h2o.download_model(drf_dpf, path = "C:/1 N5/Modelos/MODELO2020/MODELO_PROPENSION_2020")

#h2o.download_mojo(aml@leader, path = "./")

#Cargar el modelo
z = h2o.upload_model(d_gbm)
#z@model

####################################
# Modelos seleccionados
####################################
# d # TDC emsemble
# e_metalearner # PP emsemble
# f_metalearner # DPF emsemble

# estos son los modelos
# d_gbm #TDC gbm *
# e_gbm # pp gbm *
# f_drf # DPF drf *


####################################
# ARCHIVO DE CALCULO 
####################################
rm(list=ls(all=TRUE))

#library(raster) # calculo del coef. variacion
#library(DataExplorer)

library(tidymodels)
library(tidyverse)
library(skimr) # AED
library(readxl)
library(h2o)

setwd("C:/1 N5/Modelos/MODELO2020/MODELO_PROPENSION_2020")

#CARGAR ARCHIVOS
x <- read_delim("C:/1 N5/Modelos/MODELO2020/MODELOS_PRUEBAS_JUN_2020/CLIENTES.rpt", 
                "~", escape_double = FALSE,
                col_names = TRUE, 
                col_types = cols(DIAS_SIN_USO_TC_NUEVA = col_double(),
                                 SALDO_CA= col_double(),
                                 SALDOS_VISTA= col_double(),
                                 SALDO_PASIVOS= col_double(),
                                 SALDOS_VISTA_6M= col_double(),
                                 SALDOS_VISTA_1SEM= col_double(),
                                 MAX_SALDOPROM= col_double(),
                                 SDO_CPLANI= col_double(),
                                 VIDA= col_double(),
                                 IMPORTE= col_double(),
                                 Negativa= col_double(),
                                 Positiva= col_double(),
                                 Neutral= col_double(),
                                 USO_TOTAL_TC= col_double(),
                                 INGRESOS= col_double(),
                                 SALARIO= col_double(),
                                 ANTIGUEDAD= col_double(),
                                 CANTIDAD= col_double(),
                                 MONTO_ORIGINAL= col_double(),
                                 V12_1= col_double(),
                                 V12_2= col_double(),
                                 V12_3= col_double(),
                                 V12_MAYOR_3= col_double(),
                                 NUMDEPEND= col_double(),
                                 DIA_ULTMOV_TC= col_double(),
                                 DIA_ULTMOV_CA= col_double(),
                                 INGRESO_ESTIMADO= col_double(),
                                 TRABAJO_MESES_CONTINUIDAD= col_double()), 
                locale = locale(decimal_mark = ","), 
                trim_ws = TRUE)


####################################
#IMPUTACION DE DATOS
####################################

# TABLA DE OCUPACION HOMOLOGADA CON OIT

OCUPACION <- read_excel("C:/1 Riesgo/Cubo de informacion/OCUPACION.xlsx")

OCUPACION1<- OCUPACION%>%
  dplyr::select(PROFESION, OCUPACIONES)%>%    
  group_by(PROFESION, OCUPACIONES)%>%
  summarise(n())

x1 <- x%>%
  dplyr::select(ID_CLIENTE, DESC_PROFESION,SALARIO)%>%
  group_by(ID_CLIENTE, DESC_PROFESION)%>%
  summarise(salario = mean(SALARIO, na.rm = TRUE))%>%
  left_join(OCUPACION1, by= c("DESC_PROFESION"="PROFESION"))

# IMPUTACION DE SALARIO POR OCUPACIONES OIT

SALARIO_OCUPACION <- x1%>%
  dplyr::select(-ID_CLIENTE)%>%
  group_by(OCUPACIONES)%>%
  nest()%>%
  mutate(mediana=map(.x= data,  .f = ~median(.x$salario ,na.rm = TRUE)))%>%
  unnest()


# IMPUTACION DE VIDA UTIL DE LA RELACION CUANDO LA TARJETA EN EL MERCADO ESTA ACTIVA SE DEBE COLOCAR EL TIEMPO DE VIDA UTIL DE CANCELADAS
VIDA_MERCADO=x %>%
  filter(ESTATUS== 'CANCELADA')%>%
  group_by(RELACION1)%>%
  summarise(vida_util= median(VIDA, na.rm = TRUE) )  


x1 <- x %>%
  # dplyr::select(ID_CLIENTE, OCUPACION, SALARIO)%>%
  left_join(SALARIO_OCUPACION, by=c('ID_CLIENTE'))%>%
  left_join(VIDA_MERCADO, by=c('RELACION1'))%>%
  mutate(SALARIO1 = if_else(is.na(SALARIO), mediana, SALARIO),
         VIDA1= case_when( 
           is.na(VIDA) & RELACION1 == 'OTROS' ~  vida_util,
           is.na(VIDA) & RELACION1 == 'PREST. PERSONAL' ~  vida_util,
           is.na(VIDA) & RELACION1 == 'TARJ. CREDITO' ~  vida_util,
           TRUE ~ VIDA),
         N_TC1 = if_else(N_TC==0, 0, 1),
         N_PP1 = if_else(N_PP==0, 0, 1),
         N_DPF1 = if_else(N_DPF==0, 0, 1),
         ANO_ESTUDIO = case_when(
           DESC_EDUCACION == 'BÁSICO INCOMPLETOS' ~ 5,
           DESC_EDUCACION == 'BÁSICO COMPLETOS' ~ 8,
           DESC_EDUCACION == 'MEDIOS INCOMPLETOS' ~ 10,
           DESC_EDUCACION == 'MEDIOS COMPLETOS' ~ 12,
           DESC_EDUCACION == 'TÉCNICOS' ~ 15,
           DESC_EDUCACION == 'UNIVERSITARIOS INCOMPLETO' | DESC_EDUCACION == 'MEDIOS INCOMPLETOS' ~ 15,
           DESC_EDUCACION == 'UNIVERSITARIOS COMPLETOS' ~ 17,
           TRUE ~ 99))#%>%head(100)


# CAMBIO DE CORREGIMIENTOS POR DISTANCIAS

DISTANCIAS_CIUDADES <-  readxl::read_excel("DISTANCIAS CIUDADES.xlsx", 
                                  col_types = c("blank", "text", "blank", 
                                                "blank", "blank", "numeric"))

x1<- x1%>%
  left_join(DISTANCIAS_CIUDADES, by=c('RESIDENCIA_CORREGIMIENTO'))

#names(x1)
#tipo_variables <- sapply(x1, class)


receta <- x1%>%
  recipe(N_TC1 ~.)%>%
  step_filter(!is.na(ID_CLIENTE), JUBILADO == 0)%>%
  add_role(ID_CLIENTE, new_role = "no_borrar" )%>%
  add_role(N_PP1, new_role = "no_borrar" )%>%
  add_role(N_DPF1, new_role = "no_borrar" )%>%
   step_meanimpute(all_numeric())%>%
  step_rm(all_nominal(),-ID_CLIENTE, "salario","vida_util","mediana","SALARIO","FECHA_ULTMOV_CC","VIDA", 
            "INGRESOS", "INGRESO_ESTIMADO", "N_TC", "N_PP", "N_DPF")%>%
  prep()

x2 <- bake(receta, x1)

#skim(x2)
#names(x2)

##############################################
# MODELOS
##############################################

h2o.init(ip = "localhost",
         # -1 indica que se empleen todos los cores disponibles.
         nthreads = -1,
         # Máxima memoria disponible para el cluster.
         max_mem_size = "6g")


datos <- as.h2o(x2)

modelopp = h2o.upload_model("DRF_1_AutoML_20200924_101811")
modelotdc = h2o.upload_model("GBM_4_AutoML_20200923_110437")
modelodpf = h2o.upload_model("DRF_1_AutoML_20200924_115055")

pred_dpf = as.data.frame(h2o.cbind(datos[,1],h2o.predict(modelodpf, datos)[,3]))
pred_tdc = as.data.frame(h2o.cbind(datos[,1],h2o.predict(modelotdc, datos)[,3]))
pred_pp = as.data.frame(h2o.cbind(datos[,1],h2o.predict(modelopp, datos)[,3]))

predicciones <- pred_dpf%>%
  left_join(pred_pp, by = "ID_CLIENTE")%>%
  left_join(pred_tdc, by = "ID_CLIENTE")%>%
  group_by(ID_CLIENTE)%>%
  summarise(dpf = max(p1.x),
            tdc = max(p1.y),
            pp = max(p1))
    
# predicciones%>%
#   filter(dpf > .8)%>%
#   summarise(n())

h2o.shutdown() # se utiliza para terminar el cluster

write_csv(predicciones, "predicciones.csv")


