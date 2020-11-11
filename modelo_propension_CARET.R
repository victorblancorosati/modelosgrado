#############################################################################
#############################################################################
#
#                      MODELO PROPENSION PRODUCTOS
#                        UTILIZANDO EL PAQUETE CARET
#
#############################################################################
# Version: V1
# Fecha: 31/10/2019
# Autor: Victor Blanco
#############################################################################
#############################################################################
#install.packages("tensorflow")
#install.packages("keras")
#install.packages("rsample")
#install.packages("tydr")
#install.packages("rlang")
#install.packages("recipes", dependencies = t)
#install.packages("ggpubr", dependencies = T)
#install.packages("purr", dependencies = T)
#install.packages("corr", dependencies = T)
#install.packages("ggcorrplot")
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggcorrplot")

rm(list=ls(all=TRUE))
library('tidyverse')
library('lubridate')
library('reshape')
library('reshape2')
library('keras')
library ('rsample')
library ('recipes')
library('ggpubr')
#library('ggplot2')
#library('purr')
#library('corrr')
library('ggcorrplot')
library('caret')

#ap <- available.packages()


setwd("C:/1 N5/Modelos/archivos_modelos/INPUT/SQL/Input")

#carga de archivo clientes
cliente <- read_delim("CLIENTES1.rpt", 
                      col_types = 
                        list(
                          N_BANCOS=col_double(),
                          MONTO=col_double(),
                          PAGO_MENSUAL=col_double(),
                          SALDO=col_double(),
                          SCORE_APC=col_double(),
                          DURACION_MAX=col_double(),
                          DURACION_MINIMA=col_double(),
                          MORA30_MAX=col_double(),
                          MORA30_MIN=col_double(),
                          MORA60_MAX=col_double(),
                          MORA60_MIN=col_double(),
                          MORA90_MAX=col_double(),
                          MORA90_MIN=col_double(),
                          PCOMERCIAL_MONTO_ORIGINAL=col_double(),
                          PP_MONTO_ORIGINAL=col_double(),
                          TDC_MONTO_ORIGINAL=col_double(),
                          PCOMERCIAL_IMPORTE=col_double(),
                          PP_IMPORTE=col_double(),
                          TDC_IMPORTE=col_double(),
                          PCOMERCIAL_SALDO=col_double(),
                          PP_SALDO=col_double(),
                          TDC_SALDO=col_double(),
                          PCOMERCIAL_MORA30=col_double(),
                          PP_MORA30=col_double(),
                          TDC_MORA30=col_double(),
                          PCOMERCIAL_MORA60=col_double(),
                          PP_MORA60=col_double(),
                          TDC_MORA60=col_double(),
                          DESCUENTO_DIRECTO_MONTO_ORIGINAL=col_double(),
                          PAGOS_VOLUNTARIOS_MONTO_ORIGINAL=col_double(),
                          DESCUENTO_DIRECTO_IMPORTE=col_double(),
                          PAGOS_VOLUNTARIOS_IMPORTE=col_double(),
                          DESCUENTO_DIRECTO_SALDO=col_double(),
                          PAGOS_VOLUNTARIOS_SALDO=col_double(),
                          DESCUENTO_DIRECTO_MORA30=col_double(),
                          PAGOS_VOLUNTARIOS_MORA30=col_double(),
                          DESCUENTO_DIRECTO_MORA60=col_double(),
                          PAGOS_VOLUNTARIOS_MORA60=col_double()),
                      delim = "~", na =c("NULL","NA", ""))


cliente10 <- cliente[1:10,]
x= cliente %>% filter(ID_CLIENTE == '3-743-1643')
# Resumen del set de datos
glimpse(cliente)


# AED
datos <- cliente
nrow(datos)
x= datos %>% filter(ID_CLIENTE == '3-743-1643')


# Convirtiendo variables
variable_factor = function(x) {as.factor(x)}
variable_character = function(x) {as.character(x)}
variable_numerica = function(x) {as.numeric(as.character(x))}
variable_tenencia <- function(x) {as.factor(if_else(x == 0, "No", "Si"))}

datos = datos %>% 
     mutate(ESTADO_CIVIL = variable_factor(ESTADO_CIVIL),
            GENERO = variable_factor(GENERO),
            COD_TIPOCLIENTE = variable_factor(COD_TIPOCLIENTE),
            DESC_PROFESION = variable_factor(DESC_PROFESION),
            DESC_EDUCACION = variable_factor(DESC_EDUCACION),
            RESIDENCIA_PROVINCIA = variable_factor(RESIDENCIA_PROVINCIA),
            RESIDENCIA_DISTRITO = variable_factor(RESIDENCIA_DISTRITO),
            RESIDENCIA_CORREGIMIENTO = variable_factor(RESIDENCIA_CORREGIMIENTO),
            ACTIVIDAD_ECONOMICA = variable_factor(ACTIVIDAD_ECONOMICA),
            SECTOR_ECONOMICO = variable_factor(SECTOR_ECONOMICO),
            CARGO_ACTUAL = variable_factor(CARGO_ACTUAL),
            N_CTA_INT = variable_tenencia(N_CTA_INT),
            N_CTA_NAVIDAD = variable_tenencia(N_CTA_NAVIDAD),
            N_CTA_RYA = variable_tenencia(N_CTA_RYA),
            N_CUENTA_CA = variable_tenencia(N_CUENTA_CA),
            N_CUENTA_CC = variable_tenencia(N_CUENTA_CC),
            N_DPF = variable_tenencia(N_DPF),
            N_HIPOTECA = variable_tenencia(N_HIPOTECA),
            N_OTROS_PP = variable_tenencia(N_OTROS_PP),
            N_PP = variable_tenencia(N_PP),
            N_PYME = variable_tenencia(N_PYME),
            N_TARJETA_CLAVE = variable_tenencia(N_TARJETA_CLAVE),
            N_TC = variable_tenencia(N_TC),
            N_TDCPREP = variable_tenencia(N_TDCPREP),
            PLANILLA = variable_tenencia(PLANILLA),
            REGISTRO_BLINEA = variable_tenencia(REGISTRO_BLINEA),
            INGRESO_BLINEA = variable_tenencia(INGRESO_BLINEA),
            PRO_CANCELADO = variable_tenencia(PRO_CANCELADO),
            PRO_CERRADO = variable_tenencia(PRO_CERRADO),
            G_FUGA = variable_tenencia(G_FUGA),
            TIENE_MAIL = variable_tenencia(TIENE_MAIL),
            TIENE_TELEFONO = variable_tenencia(TIENE_TELEFONO),
            COD_PROFESION = variable_character(COD_PROFESION),
            CLIENTE_VIGENTE = variable_tenencia(CLIENTE_VIGENTE),
            JUBILADO = variable_character(JUBILADO))

summary(datos)

#Eligiendo Clientes para modelar

# EN vista del resultado de la mediana con clientes no vigentes las variables toman MUCHOS 0
# mediana_datos_1 <- datos_n%>%
#   #  filter(N_TC == 'Si')%>%   
#   group_by(CLIENTE_VIGENTE, N_TC) %>%
#   summarise_if(is.numeric, median ,na.rm = TRUE)



datos_n = datos%>% 
  filter(COD_TIPOCLIENTE == '1',  #Clientes naturales
         COD_PROFESION != "100" , COD_PROFESION != "1419", EDAD < 60, JUBILADO == 0, #JUBILADO
         COD_PROFESION != "102", EDAD > 21) #JMENOR DE EDAD





# x= datos_n %>% filter(ID_CLIENTE == '3-743-1643')
# x = datos_n %>%  
#   group_by(DESC_PROFESION, COD_PROFESION) %>%
#   summarise(n())


#Variable respuesta
# TDC
# respuestas <- datos_n %>% 
#   select(starts_with("N_"))%>%      # Divide en grupos por variables
#   map(table)
# 
# ggplot(data = datos_n) + geom_bar(mapping = aes(x = N_TC, y = ..prop.., group = 1, colour = N_TC))
# ggplot(data = datos_n) +  geom_count(mapping = aes(x = N_TC, y = N_CUENTA_CA))

#print(respuestas)
#respuestas$N_TC

# graficas de las variables
# ggplot(data = (datos_n),
#     aes(x = N_TC, y = ..count.., fill = N_TC)) +
#     geom_bar() +
#     scale_fill_manual(values = c("gray50", "orangered2")) +
#     labs(title = ) +
#     theme_bw() +
#     theme(legend.position = "bottom")


#Variables continuas

# Estad?sticos de variables
# eligiendo variables numericas
# str(datos_n)


# #media 
# media_datos <- datos_n%>%
#             group_by(N_TC) %>%
#             summarise_if(is.numeric, mean ,na.rm = TRUE)
# 
# 
# #mediana
# mediana_datos <- datos_n%>%
#   group_by(N_TC) %>%
#   summarise_if(is.numeric, median ,na.rm = TRUE)
# 
# #mediana
# varianza_datos <- datos_n%>%
#   group_by(N_TC) %>%
#   summarise_if(is.numeric, var ,na.rm = TRUE)
# 
# #frecuencia                   
# datos_n %>% group_by(N_TC)%>% summarise(n())
# 
#correlacion
# datos_n_numeric1 <- datos_n_numeric
# names(datos_n_numeric1) <-  str_sub(names(datos_n_numeric), 1, 3)
# corr <- round(cor(datos_n_numeric1, use = "na.or.complete"), 1)
# head(corr[, 1:6])
# p.mat <- cor_pmat(datos_n_numeric1)
# head(p.mat[, 1:4])
# ggcorrplot(corr)
# ggcorrplot(corr, method = "circle")
# ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
# ggcorrplot(corr, hc.order = TRUE, type = "lower", outline.col = "white")
# ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)
# 
# names = rbind(names1 = names(datos_n_numeric), names2 = names(datos_n_numeric1))
# 
# 
# muestra <- initial_split(datos_n,prop = 0.3)
# tbl_grafico <- training(muestra)
# 
# 
# p1 <- ggplot(data = tbl_grafico, mapping = aes(x = N_TC, y = PAGOS_VOLUNTARIOS_MONTO_ORIGINAL)) + 
#               geom_boxplot(mapping = aes(group = cut_width(USO_TOTAL_TC, 1)))
# 
# ggplot(data = tbl_grafico, aes(x= log(DURACION_MAX), fill=N_TC )) + geom_density() #+ stat_ecdf(geom = "step" ) + ylim(0,0.5)
# ggplot(data = tbl_grafico, aes(x= log(DESCUENTO_DIRECTO_SALDO), fill=N_TC )) + geom_density() #+ stat_ecdf(geom = "step" ) + ylim(0,0.5)
# p21 <- ggplot(data = tbl_grafico, aes(x= log(PAGOS_VOLUNTARIOS_SALDO), fill=N_TC )) + geom_density()  #+ stat_ecdf(geom = "step" ) + ylim(0,0.5)
# p22 <- ggplot(data = tbl_grafico, aes(x= log(PCOMERCIAL_MONTO_ORIGINAL), fill=N_TC )) + geom_density() #+ stat_ecdf(geom = "step") + ylim(0,0.5)
# p23 <- ggplot(data = tbl_grafico, aes(x= log(SALDOS_VISTA_6M), fill=N_TC )) + geom_density() #+ stat_ecdf(geom = "step") + ylim(0,0.5)
# #p24 <- ggplot(data = tbl_grafico, aes(x= log(INGRESOS), fill=N_TC )) + geom_density()
# p25 <- ggplot(data = tbl_grafico, aes(x= log(INGRESO_ESTIMADO), fill=N_TC )) + geom_density() + stat_ecdf(geom = "step")
# #p26 <- ggplot(data = tbl_grafico, aes(x= DEPOSITOS_ULT30DIAS, fill=N_TC )) + geom_density()
# p27 <- ggplot(data = tbl_grafico, aes(x= log(POS), fill=N_TC )) + geom_density()
# p28 <- ggplot(data = tbl_grafico, aes(x= N_BANCOS, fill=N_TC )) + geom_density()
# p29 <- ggplot(data = tbl_grafico, aes(x= log(PP_MONTO_ORIGINAL), fill=N_TC )) + geom_density()
# p30 <- ggplot(data = tbl_grafico, aes(x= log(PAGOS_VOLUNTARIOS_IMPORTE), fill=N_TC )) + geom_density()
# 
#revision de correlaci?n de variables analizadas anteriormente
# datos2 <-  datos_n_numeric %>%
#   select(PAGOS_VOLUNTARIOS_SALDO,PCOMERCIAL_MONTO_ORIGINAL, SALDOS_VISTA_6M,INGRESOS,
#          INGRESO_ESTIMADO, DEPOSITOS_ULT30DIAS, POS, N_BANCOS, PP_MONTO_ORIGINAL, PAGOS_VOLUNTARIOS_IMPORTE )
# 
# corr2 <- round(cor(datos2, use = "na.or.complete"), 1)
# p.mat2 <- cor_pmat(datos2)
# ggcorrplot(corr2)
# ggcorrplot(corr2, method = "circle")
# ggcorrplot(corr2, hc.order = TRUE, outline.col = "white")
# ggcorrplot(corr2, hc.order = TRUE, type = "lower", outline.col = "white")
# ggcorrplot(corr2, hc.order = TRUE, type = "lower", lab = TRUE)



# Tratamiento de NA's
# 1. para las variables continuas las NA's se hacen 0
#glimpse(datos_n)

variables_numericas <- sapply(datos_n, class)
datos_n_numeric <- datos_n %>%
  select(which(variables_numericas=="numeric"))%>% 
  map_df(.,function(datos_n) ifelse(is.na(datos_n),0,datos_n))
 
datos_n_string <- datos_n %>%
  select(-names(datos_n_numeric))

skim(datos_n_string)
skim(datos_n_numeric)

datos_n <- cbind.data.frame(datos_n_string,datos_n_numeric)

#x <- rbind(datos %>% filter(ID_CLIENTE == '3-743-1643'), datos_n_1 %>% filter(ID_CLIENTE == '3-743-1643'))



# convirtiendo en grupos
#AYUDA A IDENTIFICAR LOS PUNTOS DE CORTES POR VARIABLES
datos_n %>% 
# group_by(N_TC) %>%
  summarise_at(vars('PAGOS_VOLUNTARIOS_IMPORTE'),  # CAMBIAR LA VARIABLE PARA EVALUAR
               funs(mean(.,na.rm = TRUE),
                    sd(.,na.rm = TRUE),
                    quantile = list(as.tibble(as.list(quantile(., 
                                                               probs = c(0.1,0.15,0.17,0.20,0.25, 0.30,0.5, 0.75,0.80,0.90), na.rm = TRUE))))))%>%
  unnest


datos_n <- datos_n %>%
            mutate (PAGOS_VOLUNTARIOS_SALDO_DISC = 
                      case_when(PAGOS_VOLUNTARIOS_SALDO <= 773 ~"P_V_S_D =< 733",
                                 PAGOS_VOLUNTARIOS_SALDO > 773 ~ "P_V_S_D > 733"),
                    PCOMERCIAL_MONTO_ORIGINAL_DISC = 
                      case_when(PCOMERCIAL_MONTO_ORIGINAL <= 535 ~"PC_M_O =< 535",
                                PCOMERCIAL_MONTO_ORIGINAL > 535 ~ "PC_M_O > 535"),
                    SALDOS_VISTA_6M_DISC = 
                      case_when(SALDOS_VISTA_6M <= 0.33 ~"S_V_6M =< 0.33",
                                SALDOS_VISTA_6M > 0.33 ~ "S_V_6M > 0.33"),
                    INGRESO_ESTIMADO_DISC = 
                      case_when(INGRESO_ESTIMADO <= 594 ~ "ING_E =< 594",
                                INGRESO_ESTIMADO > 594 ~ "ING_E > 594"),
                    POS_DISC = 
                      case_when(POS <= 529 ~ "POS =< 529",
                                POS > 529 ~ "POS > 529"),
                    N_BANCOS_DISC = 
                      case_when(N_BANCOS <= 2 ~ "N_BANCOS <= 2",
                                N_BANCOS > 2 ~ "N_BANCOS > 2"),
                    PP_MONTO_ORIGINAL_DISC = 
                      case_when(PP_MONTO_ORIGINAL<= 6000 ~ "PP_MONTO_ORIGINAL<= 6000",
                                PP_MONTO_ORIGINAL > 6000 ~ "PP_MONTO_ORIGINAL > 6000"),
                    PAGOS_VOLUNTARIOS_IMPORTE_DISC = 
                      case_when(PAGOS_VOLUNTARIOS_IMPORTE<= 76 ~ "PAGOS_VOLUNTARIOS_IMPORTE<= 76",
                                PAGOS_VOLUNTARIOS_IMPORTE > 76 ~ "PAGOS_VOLUNTARIOS_IMPORTEL > 76"))

prop.table(table(datos_n$N_TC, datos_n$PAGOS_VOLUNTARIOS_SALDO_DISC)) %>% round(digits = 2)
prop.table(table(datos_n$N_TC, datos_n$PCOMERCIAL_MONTO_ORIGINAL_DISC)) %>% round(digits = 2)
prop.table(table(datos_n$N_TC, datos_n$SALDOS_VISTA_6M_DISC)) %>% round(digits = 2)
prop.table(table(datos_n$N_TC, datos_n$INGRESO_ESTIMADO_DISC)) %>% round(digits = 2)
prop.table(table(datos_n$N_TC, datos_n$POS_DISC)) %>% round(digits = 2)
prop.table(table(datos_n$N_TC, datos_n$N_BANCOS_DISC)) %>% round(digits = 2)
prop.table(table(datos_n$N_TC, datos_n$PP_MONTO_ORIGINAL_DISC)) %>% round(digits = 2)
prop.table(table(datos_n$N_TC, datos_n$PAGOS_VOLUNTARIOS_IMPORTE_DISC)) %>% round(digits = 2)

datos_n = datos_n %>% 
  mutate( PAGOS_VOLUNTARIOS_SALDO_DISC = variable_factor(PAGOS_VOLUNTARIOS_SALDO_DISC),
          PCOMERCIAL_MONTO_ORIGINAL_DISC = variable_factor(PCOMERCIAL_MONTO_ORIGINAL_DISC),
          SALDOS_VISTA_6M_DISC= variable_factor(SALDOS_VISTA_6M_DISC),
          INGRESO_ESTIMADO_DISC= variable_factor(INGRESO_ESTIMADO_DISC),
          POS_DISC= variable_factor(POS_DISC),
          N_BANCOS_DISC= variable_factor(N_BANCOS_DISC),
          PP_MONTO_ORIGINAL_DISC= variable_factor(PP_MONTO_ORIGINAL_DISC),
          PAGOS_VOLUNTARIOS_IMPORTE_DISC= variable_factor(PAGOS_VOLUNTARIOS_IMPORTE_DISC))


#GRAFICOS VARIABLES DISCRETAS
# ggplot(data = datos_n) + geom_count(mapping = aes(x = N_TC, y = POS_DISC))


# valores nulos
# observar los datos
#datos_long <- datos_n %>% gather(key = "variable", value = "valor", -ID_CLIENTE)
#head(datos_long)
# 
# # Detecci?n si hay alguna fila incompleta
any(!complete.cases(datos_n))
# 
# # N?mero de datos ausentes por variable
 map_dbl(datos_n, .f = function(x){sum(is.na(x))})
# 
# z <- as.data.frame(table(datos_n$N_TC ,datos_n$RESIDENCIA_PROVINCIA))
# z <- as.data.frame(table(datos_n$N_TC ,datos_n$RESIDENCIA_CORREGIMIENTO))
# 
# x = datos_n %>% 
#   mutate(INGRESOS = ifelse(is.na(INGRESOS),0,INGRESOS))
# 
# x %>%
#   summarise(sum(INGRESOS))
# 
# datos_n %>%
#   summarise(sum(INGRESOS, na.rm = TRUE))

# # Representaci?n gr?fica de los datos ausentes
# no funciona con muchas variables

# datos_long <- datos_long %>%  mutate(ausente = is.na(valor))
# ggplot(data = datos_long, aes(x = variable, y = ID_CLIENTE, fill = ausente)) +
#   geom_raster() +
#   scale_fill_manual(values = c("gray60", "orangered2")) +
#   theme_bw() +
#   labs(title = "Valores ausentes por variable") +
#   theme(legend.position = "bottom")

# datos_long %>%
#   group_by(variable) %>% 
#   summarize(porcentaje_NA = 100 * sum(is.na(valor)) / length(valor)) %>%
#   ggplot(aes(x = reorder(variable, desc(porcentaje_NA)), y = porcentaje_NA)) +
#   geom_col() +
#   labs(title = "Porcentaje valores ausentes por variable",
#        x = "Variable", y = "Porcentaje NAs") +
#   theme_bw()


# muy pesado
# p1 <- ggplot(data = datos_n, aes(x = EDAD, fill = N_TC)) +
#   geom_density(alpha = 0.5) +
#   scale_fill_manual(values = c("gray50", "orangered2")) +
#   geom_rug(aes(color = N_TC), alpha = 0.5) +
#   scale_color_manual(values = c("gray50", "orangered2")) +
#   theme_bw()
# p2 <- ggplot(data = datos_n, aes(x = N_TC, y = EDAD, color = N_TC)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(alpha = 0.3, width = 0.15) +
#   scale_color_manual(values = c("gray50", "orangered2")) +
#   theme_bw()
# final_plot <- ggarrange(p1, p2, legend = "top")
# final_plot <- annotate_figure(final_plot, top = text_grob("Edad", size = 15))
# final_plot

# Otra forma de evaluar valores perdidos
# install.packages('naniar')
# install.packages('skimr')

suppressPackageStartupMessages(library(naniar))
suppressPackageStartupMessages(library(skimr))

# evaluar valores perdidos
z = datos_n %>% miss_var_summary()

# estadisticos basicos
datos_n %>% skim()


#PREPROCESANDO LOS DATOS

# cambiar el nombre de la primera columna
#separar los datos en dos grupos
separa_datos <- initial_split(datos_n,
                              prop = 0.7)

#Saber cual variable es un factor
x = datos_n[1:10,]
x= data.frame(map_dbl(datos_n, .f = function(x){is.character(x)}))

#tabla de entrenamiento
tbl_entrenar <- training(separa_datos)
#tabla de prueba
tbl_prueba <- testing(separa_datos)

tbl_entrenar %>% skim()


#Limpiando los datos
receta <- tbl_entrenar %>%
          recipe(N_TC ~ .) %>%         # variable a dependiente
            step_rm(ID_CLIENTE, JUBILADO,COD_PROFESION, CARGO_ACTUAL, APC_SCORE, LIMITE_TC,
                    FECHA_CIERRE, FECHA_CIERRETDC, FECHA_CONSULTA_APC, IDENTIFICACION ) %>%   # eliminar columnas
            step_unknown(all_nominal(), -all_outcomes())%>% # asignar un nivel desconocido a las predictoras nominales
            step_naomit(all_outcomes(), all_predictors())%>%   #omitir NA's de todos las variables
            #step_discretize(INGRESO_ESTIMADO, options = list(cuts = 4)) %>%  #Discretizar variables
            step_log(SALDOS_VISTA_1SEM, SALDOS_VISTA_6M, SALDO, INGRESO_ESTIMADO) %>%            #tomar el logaritmo
            #step_mutate(N_TC = ifelse(N_TC == "1",1,0)) %>%   #convesi?n logica
            step_dummy(all_nominal(), -all_outcomes()) %>% # conversi?n de variables como dummys
            step_center(all_numeric(), -all_outcomes()) %>%  # centrar las variables predictoras
            step_scale(all_numeric(), -all_outcomes()) %>%  # escalar las variables predictoras
            step_nzv(all_numeric(), -all_outcomes())%>%
            step_pca(all_predictors(), threshold = .95) %>%
  prep()

# glimpse(datos_n)
#x = summary(receta)

# Se aplican las transformaciones al conjunto de entrenamiento y de test
datos_train_prep <- bake(receta, new_data = tbl_entrenar)
datos_test_prep  <- bake(receta, new_data = tbl_prueba)

write.csv(datos_n, "C:/1 N5/Modelos/MODELO2020/MODELO_PROPENSION_2020/datos_n.csv")
write.csv(datos_test_prep, "C:/1 N5/Modelos/MODELO2020/MODELO_PROPENSION_2020/datos_test_prep.csv")
write.csv(datos_train_prep, "C:/1 N5/Modelos/MODELO2020/MODELO_PROPENSION_2020/datos_train_prep.csv")


############################################################
############################################################
################   SELECCION DE VARIABLES   ################
############################################################
############################################################

# Aplico tres metodos para la seleccion: 1) Metodos Wrapper y 2) Filtrado
############################################################

datos_variables = sample_frac(datos_train_prep, size = .1)

# Metodo Wrapper 1.1 : 
# Eliminacion recursiva de variables
# Paralelismo
#install.packages("doMC")
#library(doMC)
# seleccionarse en funci?n del ordenador que se est? empleando.
#registerDoMC(cores = 4)
# Tama?o de los conjuntos de predictores analizados
subsets <- c(3:ncol(datos_variables)) # el maximo es el total de variables 
# N?mero de resamples para el proceso de bootstrapping
repeticiones <- 3

# Se crea una semilla para cada repetici?n de validaci?n. Esto solo es necesario si
# se quiere asegurar la reproducibilidad de los resultados, ya que la validaci?n
# cruzada y el bootstrapping implican selecci?n aleatoria.
set.seed(123)
seeds <- vector(mode = "list", length = repeticiones + 1)
         for (i in 1:repeticiones) {
  seeds[[i]] <- sample.int(1000, length(subsets))
} 
seeds[[repeticiones + 1]] <- sample.int(1000, 1)


# Se crea un control de entrenamiento donde se define el tipo de modelo empleado
# para la selecci?n de variables, en este caso random forest, la estrategia de
# resampling, en este caso bootstrapping con 30 repeticiones, y las semillas para
# cada repetici?n. Con el argumento returnResamp = "all" se especifica que se
# almacene la informaci?n de todos los modelos generados en todas las repeticiones.

ctrl_rfe <- rfeControl(functions = rfFuncs, 
                       method = "boot", 
                       number = repeticiones,
                       returnResamp = "all", 
                       allowParallel = TRUE, 
                       verbose = FALSE,
                       seeds = seeds)


# Se ejecuta la eliminaci?n recursiva de predictores
set.seed(342)
rf_rfe <- rfe(N_TC~., data = datos_variables,
              sizes = subsets,
              metric = "Accuracy",# El accuracy es la proporci?n de clasificaciones correctas
              rfeControl = ctrl_rfe,
              ntree = 500)


# Resumen
rf_rfe # (1)
# Variables seleccionadas en el mejor modelo
rf_rfe$optVariables

#Para cada uno de los tama?os analizados (subsets), el modelo se ha ajustado y evaluado
# 3 veces, cada vez con unos conjuntos de entrenamiento y validaci?n distintos creados
# mediante bootstrapping. Se dispone por lo tanto de 30 valores de accuracy y kappa 
# por tama?o. Por defecto, se emplea el promedio de accuracy para identificar el mejor 
# conjunto de predictores.

# Valores de accuracy y kappa para cada tama?o de modelo en cada resample.
rf_rfe$resample %>%
  select(1, 2, 3, 8) %>%
  head(8)

# M?tricas promedio de cada tama?o
# Esto coincide con # (1)
rf_rfe$resample %>% 
  group_by(Variables) %>%
  summarise(media_accuracy = mean(Accuracy),
            media_kappa = mean(Kappa)) %>%
  arrange(desc(media_accuracy))

# Graficando la seleccion de variables con Acuracidad
ggplot(data = rf_rfe$results, aes(x = Variables, y = Accuracy)) +
  geom_line() +
  scale_x_continuous(breaks  = unique(rf_rfe$results$Variables)) +
  geom_point() +
  geom_errorbar(aes(ymin = Accuracy - AccuracySD, ymax = Accuracy + AccuracySD),
                width = 0.2) +
  geom_point(data = rf_rfe$results %>% slice(which.max(Accuracy)),
             color = "red") +
  theme_bw()

#las n-variables que ingresaron en el modelo
head(rf_rfe$variables, 9)

#Importancia de las variables
rf_rfe$variables %>%
  filter(Variables == 10) %>%
  group_by(var) %>%
  summarise(media_influencia = mean(Overall),
            sd_influencia = sd(Overall)) %>%
  arrange(desc(media_influencia))

# Metodo Wrapper 1.2: 
# Algoritmo Genetico

# Control de entrenamiento
ga_ctrl <- gafsControl(functions = rfGA,
                       method = "cv",
                       number = 5,
                       allowParallel = TRUE,
                       genParallel = TRUE, 
                       verbose = FALSE)

# Seleccion de predictores
set.seed(10)
rf_ga <- gafs(x = datos_variables[, -1],
              y = datos_variables$N_TC,
              iters = 10, 
              popSize = 10,
              gafsControl = ga_ctrl,
              ntree = 100)

# Resumen
rf_ga
# Variable seleccionadas
rf_ga$optVariables

# Accuracy media en cada generaci?n
rf_ga$external %>% 
  group_by(Iter) %>%
  summarize(accuracy_media = mean(Accuracy))


# Muchas veces se necesita regular la cantidad de predictores porque los modelos pueden
# tomar muchas variables para los mismos individuos asi que se puede limitar generando
# una poblacion muestra

crear_poblacion <- function(popSize, n_variables, n_max){
  # Esta funci?n crea una matriz binaria en la que el n?mero de 1s por
  # fila no supera un valor m?ximo.
  # Argumentos:
  #   popSize:     n?mero total de individuos (n?mero de filas de la matriz).
  #   n_variables: longitud de los individuos (n?mero de columnas de la matriz).
  #   n_max:       n?mero m?ximo de 1 que puede contener un individuo.
  
  # Matriz donde almacenar los individuos generados.
  poblacion <- matrix(data = NA, nrow = popSize, ncol = n_variables)
  
  # Bucle para crear cada individuo.   
  for(i in 1:popSize){
    # Se selecciona con (igual probabilidad ) el n?mero de valores = 1 que puede
    # tener el individuo.
    numero_1s <- sample(x = 1:n_max, size = 1)
    
    # Se crea un vector con todo ceros que representa el individuo.
    individuo <- rep(0, times = n_variables)
    
    # Se sustituyen (numero_1s) posiciones aleatorias por unos.
    individuo[sample(x = 1:n_variables, size = numero_1s)] <- 1
    
    # Se a?ade el nuevo individuo a la poblaci?n.
    poblacion[i,] <- individuo
  }
  return(poblacion)
}

# Se crea una poblacion incial de 10 individuos
poblacion_inicial <- crear_poblacion(popSize = 10,
                                     n_variables = ncol(datos_variables) - 1 ,
                                     n_max = 4)
poblacion_inicial

# Se estima de nuevo la seleccion de las variables
set.seed(10)
rf_ga <- gafs(x = datos_variables[, -1],
              y = datos_variables$N_TC,
              iters = 10, 
              popSize = 10,
              suggestions = poblacion_inicial,
              gafsControl = ga_ctrl,
              ntree = 100)
rf_ga

# VAriables obtenidas
rf_ga$optVariables
# Accuracy media en cada generaci?n
rf_ga$external %>% 
  group_by(Iter) %>%
  summarize(accuracy_media = mean(Accuracy))



# Metodo filtrado 1: 
# Utilizando Random Forest
# Considerando que muestra el mejor desempeño de acuracidad
particiones = 10
repeticiones = 5
set.seed(123)
seeds <- sample.int(1000, particiones * repeticiones + 1)

# Control del filtrado
ctrl_filtrado <- sbfControl(functions = rfSBF, 
                            method = "repeatedcv",
                            number = particiones,
                            repeats = repeticiones,
                            seeds = seeds, verbose = FALSE, 
                            saveDetails = TRUE, allowParallel = TRUE)

set.seed(234)
rf_sbf <- sbf(N_TC ~ ., data = datos_variables,
              sbfControl = ctrl_filtrado,
              # argumentos para el modelo de evaluaci?n
              ntree = 500)

# Resumen
rf_sbf
# Variables
rf_sbf$optVariables

# Creacion del Modelo Predictivo

# Utilizando las variables seleccionadas en el metodo wraper 1
variables_seleccion <- rf_rfe$optVariables

datos_train_prep1 <- datos_train_prep %>%
                                   select (N_TC, variables_seleccion)

datos_test_prep1 <- datos_test_prep %>%
                         select (N_TC, variables_seleccion)


# 
# #head(datos_train_prep1)
# ############################################################
# ############################################################
# #############   ENTRENANDO MULTIPLES MODELOS   #############
# ############################################################
# ############################################################

############################################################
# Esto fue utilizando CARET
############################################################


# #install.packages('modelgrid')
# library(modelgrid)
# 
# # 1) Definir una parrilla de modelos
# grid_modelos <- model_grid()
# grid_modelos
# 
# # shared_settings: almacena informaci?n de configuraci?n que se aplica de la misma forma a todos los modelos que forman el grid. Generalmente suele ser el nombre de la variable respuesta y de los predictores, el tipo de estrategia de validaci?n, transformaciones de preprocesado, etc.
# # models: almacena el nombre de los modelos que forman el grid, as? como elementos de configuraci?n individuales. Si un mismo par?metro se encuentra definido tanto en $shared_settings como en el modelo individual, este ?ltimo toma preferencia.
# # model_fits: almacena los modelos ajustados (uno por cada modelo definido en models) despu?s de entrenar el model_grid.
# 
# # 2) Definiendo parametros que seran ultilizados por todos los modelos
# 
# grid_modelos <- grid_modelos %>%
#   share_settings(
#     y = datos_train_prep1$N_TC,                     # Variable dependiente
#     x = datos_train_prep1 %>% select(-N_TC),        # Variables predictoras 
#     metric = "Accuracy",                           # metrica para el modelo
#     trControl = trainControl(method = "repeatedcv",
#                              number = 10,
#                              repeats = 5,
#                              returnResamp = "final",
#                              verboseIter = FALSE,
#                              allowParallel = TRUE,
#                              classProbs = TRUE,
#                              savePredictions = TRUE
#                             # ,summaryFunction = twoClassSummary # si se utiliza toma por defecto la metrica de ROC y se pierden los resultados de Kappa y Accuracy
#     )
#   )
# 
# # 3) Definir modelos a trabajar con caracteristicas diferentes
# 
# grid_modelos <- grid_modelos %>%
#                         add_model(
#                           model_name = "Reg_logistica",            # nombre
#                           method     = "glm",                      # metodo
#                           family     = binomial(link = "logit")    
#                         ) %>%
#                         add_model(
#                           model_name = "RandomForest",
#                           method     = "ranger",
#                           num.trees  = 500,
#                           tuneGrid   = expand.grid(
#                             mtry = c(3, 4, 5, 7),
#                             min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30),
#                             splitrule = "gini"
#                           )
#                         ) %>%
#                         add_model(
#                           model_name = "SVM",
#                           method = "svmRadial",
#                           tuneGrid   = expand.grid(
#                             sigma = c(0.001, 0.01, 0.1, 0.5, 1),
#                             C = c(1 , 20, 50, 100, 200, 500, 700)
#                           )
#                         )
# grid_modelos$models
# 
# 
# # 4) Entrenar los modelos
# library(doMC)
# registerDoMC(cores = 4)
# 
# grid_modelos <- caret::train(grid_modelos, train_all = FALSE, resample_seed = 123)
# grid_modelos$model_fits
# 
# # Revisar modelos finales
# grid_modelos$model_fits$RandomForest$finalModel
# 
# 
# # Extracci?n de las m?tricas de validaci?n cruzada.
# metricas_cv <- caret::resamples(x = grid_modelos$model_fits) # Visualizacion de resultados
# metricas_cv$values %>% head()
# 
# # Predicci?n
# predicciones <- extractPrediction(
#           models = grid_modelos$model_fits,
#           testX = datos_test_prep[variables_seleccion], # Solo se pueden ingresar en los datos las variables 
#                                                        # con las que se trabajo el modelo
#           testY = datos_test_prep$N_TC
#         )
# predicciones %>% head()
# 
# 
# # Comparacion de modelos
# names(grid_modelos$model_fits)
# 
# grid_modelos$model_fits %>%
#   caret::resamples(.) %>%
#   summary(.)
# 
# grid_modelos$model_fits %>%
#   caret::resamples(.) %>%
#   bwplot(.)
# 
# 
# # Evaluar el ROC de los modelos
# 
# # MODELO 1
# grid_modelos$model_fits$Reg_logistica
# 
# modelo_logistico <- train(N_TC ~ .,
#                           data = datos_train_prep1,
#                           method = "glm",
#                           family = "binomial",
#                           tuneGrid = data.frame(parameter = "none"),
#                           metric = "Accuracy",
#                           trControl = trainControl(method = "repeatedcv",
#                                                    number = 10,
#                                                    repeats = 5,
#                                                    returnResamp = "final",
#                                                    verboseIter = FALSE,
#                                                    allowParallel = TRUE,
#                                                    classProbs = TRUE
#   )
# ) 
# 
# summary(modelo_logistico$finalModel)                                                    
# 
# # Matriz de confusion
# confusionMatrix(data = predict(modelo_logistico,
#                                newdata = datos_test_prep,
#                                type = "raw"),
#                 reference = datos_test_prep$N_TC,
#                 positive = "Si")
# 
# predicciones_log <- predict(object = modelo_logistico,
#                         newdata = datos_test_prep,
#                         type = "prob")
# 
# 
# # MODELO 2
# grid_modelos$model_fits$RandomForest
# 
# modelo_rf <- train(N_TC ~ .,
#                           data = datos_train_prep1,
#                           method = "ranger",
#                           tuneGrid = expand.grid(mtry = 7,
#                                       min.node.size = 4,
#                                       splitrule = "gini"),
#                           metric = "Accuracy",
#                           num.trees = 500,
#                           trControl = trainControl(method = "repeatedcv",
#                                                    number = 10,
#                                                    repeats = 5,
#                                                    returnResamp = "final",
#                                                    verboseIter = FALSE,
#                                                    allowParallel = TRUE,
#                                                    classProbs = TRUE
#                                                    
#                           )
# ) 
# 
# modelo_rf$finalModel                                               
# 
# # Matriz de confusion
# confusionMatrix(data = predict(modelo_rf,
#                                newdata = datos_test_prep,
#                                type = "raw"),
#                 reference = datos_test_prep$N_TC,
#                 positive = "Si")
# 
# predicciones_rf <- predict(object = modelo_rf,
#                             newdata = datos_test_prep,
#                            type = "prob")
# 
# 
# # Calculos de la curva ROC
# 
# # Funcion de ROC
# # En esta funcion se debe cambiar la variable respuestas y los niveles
# 
# test_roc <- function(model, data) {
#   library(pROC)
#   roc_obj <- roc(data$N_TC, 
#                  predict(model, data, type = "prob")[, "Si"],
#                  levels = c("No", "Si")
#                 # ,plot = TRUE
#                 )
#   ci(roc_obj)
#   plot.roc(roc_obj, add = TRUE)
# }
# 
# 
# # Modelos a evaluar
# modelos = list(rf = modelo_rf,
#                rl = modelo_logistico )
# 
# # Aplica el calculo de ROC a todos los modelos
# outside_test <- lapply(modelos, test_roc, data = datos_test_prep)
# # Convierte en un vector
# outside_test <- lapply(outside_test, as.vector)
# # Une las filas
# outside_test <- do.call("rbind", outside_test)
# # Coloca nombres
# colnames(outside_test) <- c("lower", "ROC", "upper")
# # Lo convierte en tabla
# outside_test <- as.data.frame(outside_test)
# outside_test
# 
# # Uniendo los resultados de los modelos
# #Stacking Algorithms
# 
# result <- resamples(grid_modelos)
# 
# caretLis
# 
# # Elegir entre otros modelos basados en disimilaridad de jaccard
# # 
# # tag <- read.csv("tag_data.csv", row.names = 1)
# # tag <- as.matrix(tag)
# # 
# # ## Select only models for regression
# # regModels <- tag[tag[,"Two.Class.Only"] == 1,]
# # 
# # all <- 1:nrow(regModels)
# # ## Seed the analysis with the SVM model
# # start <- grep("(glm)", rownames(regModels), fixed = TRUE)
# # pool <- all[all != start]
# # 
# # ## Select 4 model models by maximizing the Jaccard
# # ## dissimilarity between sets of models
# # nextMods <- maxDissim(regModels[start,,drop = FALSE], 
# #                       regModels[pool, ], 
# #                       method = "Jaccard",
# #                       n = 4)
# # 
# # rownames(regModels)[c(start, nextMods)]
# 

############################################################
# Utilizando H20
############################################################
# install.packages('h2o')

skim(datos_test_prep1)
skim(datos_train_prep1)


library(h2o)

h2o.init(ip = "localhost",
         # -1 indica que se empleen todos los cores disponibles.
         nthreads = -1,
         # Máxima memoria disponible para el cluster.
         max_mem_size = "6g")

datos_train_prep11 <- as.h2o(datos_train_prep1)
datos_test_prep11 <- as.h2o(datos_test_prep)

h2o.describe(datos_train_prep11)

y <- "N_TC"
x <- setdiff(names(datos_train_prep11), y)

aml <- h2o.automl(y = y,
                  x = x,
                  training_frame = datos_train_prep11,
                  max_models = 10,
                  seed = 1)

lb <- aml@leaderboard
print(lb)
print(lb, n = nrow(lb))
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
metalearner <- h2o.getModel(se@model$metalearner$name)
h2o.varimp(metalearner)
h2o.varimp_plot(metalearner)

model= h2o.loadModel(path = "./product_backorders_model_bin")


pred_class = h2o.predict(metalearner, datos_test_prep11) %>% as.data.frame() %>% pull(predict)
metricas = h2o.performance(model = metalearner, newdata = datos_test_prep11)
h2o.confusionMatrix(metricas)


## Save Leader Model
h2o.saveModel(aml@leader, path = "./product_backorders_model_bin")


#```

aml_b <- h2o.automl(y = y,
                  x = x,
                  training_frame = datos_train_prep11,
                  max_models = 10,
                  seed = 1,
                  balance_classes = TRUE)

lb_b <- aml_b@leaderboard
print(lb_b)
print(lb_b, n = nrow(lb_b))
model_ids_b <- as.data.frame(aml_b@leaderboard$model_id)[,1]
se_b <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids_b, value = TRUE)[1])
metalearner_b <- h2o.getModel(se_b@model$metalearner$name)
h2o.varimp(metalearner_b)
h2o.varimp_plot(metalearner_b)

pred_class_b = h2o.predict(metalearner_b, datos_test_prep11) %>% as.data.frame() %>% pull(predict)
metricas_b = h2o.performance(model = metalearner_b, newdata = datos_test_prep11)
h2o.confusionMatrix(metricas_b)

h2o.performance(model = metalearner_b, newdata = datos_test_prep11)@metrics$AUC

#Guradar el modelo
d= h2o.download_model(metalearner_b, path = "C:/1 N5/Modelos/MODELO2020/MODELO_PROPENSION_2020")
z = h2o.upload_model(d)

h2o.performance(model = z, newdata = datos_test_prep11)@metrics$AUC

## Save Leader Model
##############h2o.saveModel(aml@leader_b, path = "./modelo_balanceado")
#```



h2o.download_mojo(aml@leader, path = "./")



h2o.shutdown() # se utiliza para terminar el cluster
