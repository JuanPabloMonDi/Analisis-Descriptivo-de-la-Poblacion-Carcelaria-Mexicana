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

Tabla1<- read_csv("Encuesta Carceles INEGI 2021/bd_enpol_2021_csv/ENPOL2021_SOC.csv")
Tabla23<- read_csv("Encuesta Carceles INEGI 2021/bd_enpol_2021_csv/ENPOL2021_2_3.csv")
Tabla4<- read_csv("Encuesta Carceles INEGI 2021/bd_enpol_2021_csv/ENPOL2021_4.csv")
Tabla5<- read_csv("Encuesta Carceles INEGI 2021/bd_enpol_2021_csv/ENPOL2021_5.csv")
Tabla6<- read_csv("Encuesta Carceles INEGI 2021/bd_enpol_2021_csv/ENPOL2021_6.csv")
Tabla7<- read_csv("Encuesta Carceles INEGI 2021/bd_enpol_2021_csv/ENPOL2021_7.csv")
Tabla89<- read_csv("Encuesta Carceles INEGI 2021/bd_enpol_2021_csv/ENPOL2021_8_9_10_11.csv")

VarInicial1<-ncol(Tabla1)
Variables<-c( "ID_PER", #Identificación del recluso
              "NOM_ENT", #Estado donde esta ubicado el centro penitenciario
              "P1_1", #Tipo de centro penitenciario
              "P1_1A", #Cuánto tiempo tiene privado de su libertad. Categórico
              "P1_2", # SEXO
              "P1_3", #Edad
              "P1_4", #Nacionalidad
              "P1_7", #Estado Civil
              "P1_8", #Posee hijos
              "P1_9", #Cuantos hijos menores de 18 años tiene
              "P1_18_N", #Nivel de escolaridad
              "P1_20", #A qué edad obtuvo el grado escolar
              "P1_21", #Por qué no continuó estudiando
              "P1_22", #cómo se identifica
              "P1_23", #Orientación sexual
              #P1_24_25_26 #ENFERMEDADES
              "P1_24_8", #COVID Fue diagnostivado de covid?
              #"P1_25_8", #Cuándo fue detectado el covid?
              #"P1_28_8", #Por qué no hace el tratamiento
              "P1_29", #Ha considerado el suicidio
              "P1_31_1" ,# Posee discapacidad visual
              "P1_31_2", #Discacidad auditiva
              "P1_31_3", #Dispacidad motriz
              #P1_41 Alguna vez ha consumido...
              #P1_42 A que edad probó por primera vez...
              "P1_41_1", #Tabaco
              "P1_42_1", #Tabaco
              "P1_41_2", #Alcohol
              "P1_42_2", #Alcohol
              "P1_41_3", #Marihuana
              "P1_42_3" #Marihuana
)
#Este pedazo NO es definitivo
Tabla1 <- Tabla1[Variables] %>%
  rename(TIPO_CARCEL= "P1_1",
         TIEMPO_CARCEL = P1_1A,
         SEXO= P1_2,
         EDAD= P1_3,
         NACION= P1_4,
         EST_CIVIL=P1_7,
         HIJOS=P1_8,
         NUM_HIJOS=P1_9,
         NIVEL_ESCOLARIDAD=P1_18_N,
         EDAD_ESCOLARIDAD=P1_20,
         PARO_ESTUDIOS=P1_21,
         IDENTI_GENERO=P1_22,
         ORIENT_SEXUAL=P1_23,
         COVID=P1_24_8,
         CONSIDERO_SUICIDIO=P1_29,
         DISC_VISUAL=P1_31_1,
         DISC_AUDITIVA=P1_31_2,
         DISC_MOTRIZ=P1_31_3,
         CONSUMO_TABACO=P1_41_1,
         CONSUMO_ALCOHOL=P1_41_2,
         CONSUMO_MARIHUANA=P1_41_3,
         EDAD_CONSUMO_TABACO=P1_42_1,
         EDAD_CONSUMO_ALCOHOL=P1_42_2,
         EDAD_CONSUMO_MARIHUANA=P1_42_3,
  ) %>% mutate(TIPO_CARCEL= as.factor(TIPO_CARCEL),
               TIEMPO_CARCEL = as.factor(TIEMPO_CARCEL) ,
               SEXO= as.factor(SEXO),
               EDAD= as.numeric(EDAD),
               NACION= as.factor(NACION),
               EST_CIVIL=as.factor(EST_CIVIL),
               HIJOS=as.factor(HIJOS),
               NUM_HIJOS=as.numeric(NUM_HIJOS),
               NIVEL_ESCOLARIDAD=as.factor(NIVEL_ESCOLARIDAD),
               EDAD_ESCOLARIDAD=as.numeric(EDAD_ESCOLARIDAD),
               PARO_ESTUDIOS=as.factor(PARO_ESTUDIOS),
               IDENTI_GENERO=as.factor(IDENTI_GENERO),
               ORIENT_SEXUAL=as.factor(ORIENT_SEXUAL),
               COVID=as.factor(COVID),
               CONSIDERO_SUICIDIO=as.factor(CONSIDERO_SUICIDIO),
               DISC_VISUAL=as.factor(DISC_VISUAL),
               DISC_AUDITIVA=as.factor(DISC_AUDITIVA),
               DISC_MOTRIZ=as.factor(DISC_MOTRIZ),
               CONSUMO_TABACO=as.factor(CONSUMO_TABACO),
               CONSUMO_ALCOHOL=as.factor(CONSUMO_ALCOHOL),
               CONSUMO_MARIHUANA=as.factor(CONSUMO_MARIHUANA),
               EDAD_CONSUMO_TABACO=as.numeric(EDAD_CONSUMO_TABACO),
               EDAD_CONSUMO_ALCOHOL=as.numeric(EDAD_CONSUMO_ALCOHOL),
               EDAD_CONSUMO_MARIHUANA=as.numeric(EDAD_CONSUMO_MARIHUANA))

Tabla1[is.na(Tabla1["NUM_HIJOS"]),]["NUM_HIJOS"]<-0
Tabla1[is.na(Tabla1["EDAD_CONSUMO_TABACO"]),]["EDAD_CONSUMO_TABACO"]<-mean(na.omit(Tabla1$EDAD_CONSUMO_TABACO))
Tabla1[is.na(Tabla1["EDAD_CONSUMO_ALCOHOL"]),]["EDAD_CONSUMO_ALCOHOL"]<-mean(na.omit(Tabla1$EDAD_CONSUMO_ALCOHOL))
Tabla1[is.na(Tabla1["EDAD_CONSUMO_MARIHUANA"]),]["EDAD_CONSUMO_MARIHUANA"]<-mean(na.omit(Tabla1$EDAD_CONSUMO_MARIHUANA))

Tabla23$VIVE_CON <- ifelse(Tabla23$P2_3_01 == 1, "Solo",
                           ifelse(Tabla23$P2_3_02 == 1, "Pareja",
                                  ifelse((Tabla23$P2_3_03 == 1 | Tabla23$P2_3_04==1), "Madre/Padre",ifelse(Tabla23$P2_3_05==1, "Hermanos",ifelse(Tabla23$P2_3_06==1,"Abuelos",
                                                                                                                                                 ifelse(Tabla23$P2_3_07==1,"Hijos", ifelse(Tabla23$P2_3_08==1, "Otros familiares", ifelse(Tabla23$P2_3_09==1, "Amigos",ifelse((Tabla23$P2_3_98==1|Tabla23$P2_3_99==1),"No sabe/No responde", "Otros")))))))))


VarInicial2<-ncol(Tabla23)
Variables2<-c( "ID_PER", #Identificación del recluso
               "VIVE_CON", #Con quien vivia antes de la detención
               "P2_5", #A cuantas personas mantenia economicamente
               "P2_6", #Alguna vez ha trabajado con pago? no importa si es legal o ilegal
               "P2_14_1", # El año anterior tenian lo suficiente para comer
               "P2_14_2", #Tenian alguna deuda?
               "P2_15", #Cuanto ganó el mes antes de su detención
               "P3_1", #Por qué esta detenido?
               "P3_9", #Cuanto tiempo pasó entre el delito y la detención
               "P3_21_1", #La policia intentó sobornar para no detenerlo
               "P3_24" #Cuánto dinero se apropió la policia?
)


Tabla23 <- Tabla23[Variables2] %>%
  rename(
    #VIVE_CON= "P2_3",
    N_PERSONAS_DEPENDIENTES = P2_5,
    TRABAJO= P2_6,
    PODIA_COMER= P2_14_1,
    DEUDA= P2_14_2,
    SALARIO_MES_ANTERIOR=P2_15,
    MOTIVO_DETENCION=P3_1,
    TIEMPO_DEL_DET=P3_9,
    INTENTO_SOBORNO=P3_21_1,
    DINERO_SOBORNO=P3_24
  ) %>% mutate(
    VIVE_CON= as.factor(VIVE_CON),
    N_PERSONAS_DEPENDIENTES = as.numeric(N_PERSONAS_DEPENDIENTES),
    TRABAJO= as.factor(TRABAJO),
    PODIA_COMER= as.factor(PODIA_COMER),
    DEUDA= as.factor(DEUDA),
    SALARIO_MES_ANTERIOR=as.factor(SALARIO_MES_ANTERIOR),
    MOTIVO_DETENCION=as.factor(MOTIVO_DETENCION),
    TIEMPO_DEL_DET=as.factor(TIEMPO_DEL_DET),
    INTENTO_SOBORNO=as.factor(INTENTO_SOBORNO),
    DINERO_SOBORNO=as.numeric(DINERO_SOBORNO))

Tabla23[is.na(Tabla23$N_PERSONAS_DEPENDIENTES),]["N_PERSONAS_DEPENDIENTES"]<-0
Tabla23[is.na(Tabla23$DINERO_SOBORNO),]["DINERO_SOBORNO"]<-0

VarInicial4<-ncol(Tabla4)
Variables4<-c( "ID_PER", #Identificación del recluso
               "P4_7", # Por qué se declaró culpable
               "P4_15_1" #Las autoridades le pidieron dinero a cambio de no hacer daño
)

Tabla4 <- Tabla4[Variables4] %>%
  rename(
    DECLARACION_CULPABLE=P4_7,
    INTENTO_SOBORNO_JUICIO=P4_15_1
  ) %>% mutate(
    DECLARACION_CULPABLE= as.factor(DECLARACION_CULPABLE),
    INTENTO_SOBORNO_JUICIO=as.factor(INTENTO_SOBORNO_JUICIO))


VarInicial5<-ncol(Tabla5)
Variables5<-c( "ID_PER", #Identificación del recluso
               "P5_4_A", #Años a los que fue condenado
               "P5_4_M", #Meses a los que fue condenado
               #"P5_11" #Por cuales delitos fue sentenciado
               "P5_28", #Cuantos delitos tiene abiertos actualmente
               #P5_31 #Por cual delito fue sentenciado inicialmente
               "P5_34_A", #Cuantos años ha durado su proceso
               "P5_34_M" #Cuantos meses ha durado su proceso
)

Tabla5 <- Tabla5[Variables5] %>%
  rename(
    ANOS_CONDENA=P5_4_A,
    MESES_CONDENA=P5_4_M,
    DELITOS_ABIERTOS=P5_28,
    ANOS_PROCESO=P5_34_A,
    MESES_PROCESO=P5_34_M
  ) %>% mutate(
    ANOS_CONDENA= as.numeric(ANOS_CONDENA),
    ANOS_PROCESO=as.numeric(ANOS_PROCESO),
    MESES_CONDENA=as.numeric(MESES_CONDENA),
    MESES_PROCESO=as.numeric(MESES_PROCESO),
    DELITOS_ABIERTOS=as.numeric(DELITOS_ABIERTOS)
  )

Tabla5[is.na(Tabla5$ANOS_CONDENA),]["ANOS_CONDENA"]<-0
Tabla5[is.na(Tabla5$MESES_CONDENA),]["MESES_CONDENA"]<-0
Tabla5[is.na(Tabla5$ANOS_PROCESO),]["ANOS_PROCESO"]<-0
Tabla5[is.na(Tabla5$MESES_PROCESO),]["MESES_PROCESO"]<-0
Tabla5[is.na(Tabla5$DELITOS_ABIERTOS),]["DELITOS_ABIERTOS"]<-0

Tabla5$TIEMPO_RECLUIDO<-(Tabla5$ANOS_CONDENA*12+Tabla5$MESES_CONDENA)/12
Tabla5$TIEMPO_PROCESO<-(Tabla5$ANOS_PROCESO*12+Tabla5$MESES_PROCESO)/12


VarInicial6<-ncol(Tabla6)
Variables6<-c( "ID_PER", #Identificación del recluso
               "P6_1", #Con cuantas personas comparte celda
               "P6_4_1", #la celda cuenta con ventanas
               "P6_4_2", #Cuenta con agua potable en la celda
               "P6_4_3", # La celda cuenta con drenaje
               "P6_4_4", #Luz electrica
               "P6_4_5", #Lugar para bañarse
               "P6_4_6", #Lugar para hacer el baño
               "P6_10_01", #El centro le proporciona servicio médico
               "P6_12" #El centro le proporciona alimentos
)

Tabla6 <- Tabla6[Variables6] %>%
  rename(
    CANTIDAD_PERSONAS_CELDA=P6_1,
    CELDA_VENTANAS=P6_4_1,
    CELDA_AGUA_POTABLE=P6_4_2,
    CELDA_DRENAJE=P6_4_3,
    CELDA_LUZ=P6_4_4,
    CELDA_DUCHA=P6_4_5,
    CELDA_SANITARIO=P6_4_6,
    CELDA_SERVICIO_MEDICO=P6_10_01,
    CELDA_ALIMENTOS=P6_12
  ) %>% mutate(
    CANTIDAD_PERSONAS_CELDA= as.numeric(CANTIDAD_PERSONAS_CELDA),
    CELDA_VENTANAS=as.factor(CELDA_VENTANAS),
    CELDA_AGUA_POTABLE=as.factor(CELDA_AGUA_POTABLE),
    CELDA_DRENAJE=as.factor(CELDA_DRENAJE),
    CELDA_LUZ=as.factor(CELDA_LUZ),
    CELDA_DUCHA=as.factor(CELDA_DUCHA),
    CELDA_SANITARIO=as.factor(CELDA_SANITARIO),
    CELDA_SERVICIO_MEDICO=as.factor(CELDA_SERVICIO_MEDICO),
    CELDA_ALIMENTOS=as.factor(CELDA_SERVICIO_MEDICO)
  )

VarInicial7<-ncol(Tabla7)
Variables7<-c( "ID_PER", #Identificación del recluso
               "P7_2", #Cuantas horas pasa en su celda al día
               "P7_5", #Cuenta con espacio para ejercitarse
               "P7_6", #Cuenta con material de lectura
               "P7_10", #Puede practicar su religión
               "P7_11", #Puede realizar llamadas a sus familiares
               "P7_18", #Estudia dentro de la carcel
               "P7_25", #En el ultimo año ha recibido una visita
               "P7_35", #Se siente seguro en su celda
               "P7_36" #Se siente seguro en la carcel
               #P7_46 Discriminación
)

Tabla7 <- Tabla7[Variables7] %>%
  rename(
    CELDA_HORAS=P7_2,
    CARCEL_EJERCICIO=P7_5,
    CARCEL_LECTURA=P7_6,
    CARCEL_RELIGION=P7_10,
    CARCEL_LLAMADAS=P7_11,
    CARCEL_ESTUDIO=P7_18,
    CARCEL_VISITA=P7_25,
    CELDA_SEGURIDAD=P7_35,
    CARCEL_SEGURIDAD=P7_36
  ) %>% mutate(
    CELDA_HORAS= as.numeric(CELDA_HORAS),
    CARCEL_EJERCICIO=as.factor(CARCEL_EJERCICIO),
    CARCEL_LECTURA=as.factor(CARCEL_LECTURA),
    CARCEL_RELIGION=as.factor(CARCEL_RELIGION),
    CARCEL_LLAMADAS=as.factor(CARCEL_LLAMADAS),
    CARCEL_ESTUDIO=as.factor(CARCEL_ESTUDIO),
    CARCEL_VISITA=as.factor(CARCEL_VISITA),
    CELDA_SEGURIDAD=as.factor(CELDA_SEGURIDAD),
    CARCEL_SEGURIDAD=as.factor(CARCEL_SEGURIDAD)
  )

VarInicial8<-ncol(Tabla89)
Variables8<-c( "ID_PER", #Identificación del recluso
               "P9_1", #Habia sido sentenciado por un delito antes
               "P9_4", #Cuantas veces habia sido sentenciado
               "P9_8_1", #Vivió con su madre en la crianza
               "P9_8_2", #Vivió con su padre en la crianza
               "P10_2", #Tiene donde vivir despues de salir 
               "P10_5_1", #Piensa que no conseguirá empleo
               "P10_5_2", #No conseguirá estudiar 
               "P10_5_3", #No podra encontrarse con sus amigos
               "P10_5_4", #No podrá reintegrarse a la familia
               "P10_6", #El centro proporcionó herramientas para la reincorporación
               "P10_10" #Como se percibe la persona
)

Tabla89 <- Tabla89[Variables8] %>%
  rename(
    SENTENCIADO_ANTES=P9_1,
    VECES_SENTENCIADO=P9_4,
    VIVIO_MADRE_ADOLESCENCIA=P9_8_1,
    VIVIO_PADRE_ADOLESCENCIA=P9_8_2,
    SALIDA_VIVIENDA=P10_2,
    SALIDA_EMPLEO=P10_5_1,
    SALIDA_ESTUDIO=P10_5_2,
    SALIDA_AMIGOS=P10_5_3,
    SALIDA_FAMILIA=P10_5_4,
    SALIDA_REINCORPORACION=P10_6,
    COLOR_PIEL=P10_10
  ) %>% mutate(
    SENTENCIADO_ANTES= as.factor(SENTENCIADO_ANTES),
    VECES_SENTENCIADO=as.numeric(VECES_SENTENCIADO),
    VIVIO_MADRE_ADOLESCENCIA=as.factor(VIVIO_MADRE_ADOLESCENCIA),
    VIVIO_PADRE_ADOLESCENCIA=as.factor(VIVIO_PADRE_ADOLESCENCIA),
    SALIDA_VIVIENDA=as.factor(SALIDA_VIVIENDA),
    SALIDA_EMPLEO=as.factor(SALIDA_EMPLEO),
    SALIDA_ESTUDIO=as.factor(SALIDA_ESTUDIO),
    SALIDA_AMIGOS=as.factor(SALIDA_AMIGOS),
    SALIDA_FAMILIA=as.factor(SALIDA_FAMILIA),
    SALIDA_REINCORPORACION=as.factor(SALIDA_REINCORPORACION),
    COLOR_PIEL=as.factor(COLOR_PIEL)
  )

Tabla89[is.na(Tabla89$VECES_SENTENCIADO),]["VECES_SENTENCIADO"]<-0

Tabla1<-inner_join(Tabla1,Tabla23, by="ID_PER") %>% inner_join(.,Tabla4,by="ID_PER")%>% inner_join(.,Tabla5,by="ID_PER")%>%inner_join(.,Tabla6,by="ID_PER")%>%inner_join(.,Tabla7,by="ID_PER")%>% inner_join(.,Tabla89,by="ID_PER")
