#En este archivo replicamos el codigo de Datos.qmd para formar la tabla "datos seleccionados"
#Esto lo hacemos para evitar exportar una tabla y que el archivo zip no exceda los 10 MB

library(readr)  
library(tidyverse)
library(ggplot2)
library(plotly)
library(kableExtra)
library(GGally)
library(FactoClass)
library(dplyr)
library(readxl)
library(scales)
library(DT)
library(gt)
library(openxlsx)

Tabla1<- read_csv("DatosSeleccionados.csv",locale = locale(encoding = "ISO-8859-1"))


varCuantitativas<-c("EDAD","NUM_HIJOS","EDAD_ESCOLARIDAD","EDAD_CONSUMO_TABACO","EDAD_CONSUMO_ALCOHOL","EDAD_CONSUMO_MARIHUANA","EDAD_CONSUMO_INHALABLES","EDAD_CONSUMO_LSD","EDAD_CONSUMO_HONGOS","EDAD_CONSUMO_COCAINA","EDAD_CONSUMO_PASTA_COCAINA","EDAD_CONSUMO_CRACK","EDAD_CONSUMO_HEROINA","EDAD_CONSUMO_CHOCHOS","EDAD_CONSUMO_TRANQUILIZANTES","EDAD_CONSUMO_ANFETAMINAS","N_PERSONAS_DEPENDIENTES","DINERO_SOBORNO","DELITOS_ABIERTOS","TIEMPO_RECLUIDO","TIEMPO_PROCESO","CANTIDAD_PERSONAS_CELDA","CELDA_HORAS","VECES_SENTENCIADO")

varCualitativas<-setdiff(colnames(Tabla1),c("ID_PER","NOM_ENT",varCuantitativas))