#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library('vcd') 
library(FactoClass)
library(factoextra)
library(knitr) # para función kable (tablas estáticas)
library(DT) # para tablas interactivas
library(bslib)
library(plotly)
library(dplyr)
library(corrplot)
library(ggplot2)
library(shinythemes)
library(DT)
source("DatosSeleccionados2.R")
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),  # Para usar shinyjs para manejar interacciones
  theme = bs_theme(preset = "flatly",
                   #bg = "#C0C0C0",
                   #fg = "#111"
                   ),
  sidebarLayout(position = "left",
                sidebarPanel( width=2,
                              img(src = "LogoUnal.png", height=50,width=110, align="center"),
                              br(),
                              br(),
                              h4("Análisis descriptivo ENPOL 2021"),
                              hr(),
                              br(),
                              accordion(open=F,
                                accordion_panel(
                                  "Resumen",
                                  HTML(
                                    "<p>&#8226; <a href='https://juanpablomondi.github.io/Analisis-Descriptivo-de-la-Poblacion-Carcelaria-Mexicana/ResumenIntro.html' target='_blank'> Resumen</a></p>"
                                  ),
                                )
                              ),
                              accordion(open = F,
                                accordion_panel(
                                  "Marco Conceptual",
                                  HTML(
                                    "<p>&#8226; <a href='https://juanpablomondi.github.io/Analisis-Descriptivo-de-la-Poblacion-Carcelaria-Mexicana/Antecedentes.html' target='_blank'>Marco de Antecedentes</a></p>
     <p>&#8226; <a href='https://juanpablomondi.github.io/Analisis-Descriptivo-de-la-Poblacion-Carcelaria-Mexicana/' target='_blank'>Objetivos y Justificación</a></p>
     <p>&#8226; <a href='https://juanpablomondi.github.io/Analisis-Descriptivo-de-la-Poblacion-Carcelaria-Mexicana/Datos.html' target='_blank'>Descripción de los datos</a></p>"
                                  )
                                )
                              ),
                              accordion(open = F,
                                accordion_panel(
                                  "Análisis Descriptivo",
                                  HTML(
                                    "<p>&#8226; <a href='https://juanpablomondi.github.io/Analisis-Descriptivo-de-la-Poblacion-Carcelaria-Mexicana/dashboard.html' target='_blank'>Análisis Descriptivo Univariado</a></p>
   <p>&#8226; <a href='https://qglhmj-juan0pablo-montano0diaz.shinyapps.io/analisis_descriptivo_bivariado/' target='_blank'>Análisis Descriptivo Bivariado</a></p>
   <p>&#8226; <a href='https://juanpablomondi.github.io/Analisis-Descriptivo-de-la-Poblacion-Carcelaria-Mexicana/ACP.html' target='_blank'>Análisis de Componentes Principales (PCA)</a></p>
   <p>&#8226; <a href='https://qglhmj-juan0pablo-montano0diaz.shinyapps.io/CorrespondenciaSimples/' target='_blank'>Análisis de Correspondencias Simples</a></p>
   <p>&#8226; <a href='https://juanpablomondi.github.io/Analisis-Descriptivo-de-la-Poblacion-Carcelaria-Mexicana/ACS.html' target='_blank'>Análisis de Correspondencias Múltiples (MCA)</a></p>
   "
                                    
                                  )
                                  
                                )
                              ),
                              accordion(open = F,
                                accordion_panel("Sobre nosotros",
                                                HTML(
                                                  "<p>&#8226; <a href='https://juanpablomondi.github.io/Analisis-Descriptivo-de-la-Poblacion-Carcelaria-Mexicana/about.html' target='_blank'> Sobre Nosotros</a></p> "
                                                )
                                                )
                              )
  
  
  ),
  #theme = shinytheme("cosmo"),
  mainPanel(width=10,
            br(),
            h1("Análisis de correspondencia simple (ACS)",  align = "center"),
  tabsetPanel(
  tabPanel("Análisis de los resultados" ,
           br(),
           hr(),
           h2("Resultados del ACS"),
           br(),
          fluidRow(
            
            column(2,
                   # Esta columna está vacía para dejar espacio en blanco
                   p("")
            ),
            
            column(8,
                   p("Dada la gran cantidad de variables cualitativas a analizar, se decidió hacer 
    el análisis de correspondencias simple de un forma más interactiva para el lector. Siendo así, esta sección
    se compone de 2 partes: Análisis y resultados encontrados, y el ACS Interactivo. Resaltaremos los principales
    resultados vistos entre las posibles combinaciones de variables cualitativas que existen en nuestra base de datos."),
                   br(),
                   hr(),
                   br(),
                   
                   h5("Observaciones generales"),
                   
                   tags$ul(
                     tags$li("Las categorías 'No sabe' y 'No responde' presentan un comportamiento distintivo en comparación con las categorías marginales, sugiriendo que están más relacionadas entre sí que con otras categorías del análisis"),
                     tags$li("Se observa una dependencia entre las variables de edad de consumo de sustancias psicoactivas y las categorías de edad de consumo. Las personas que han consumido la sustancia proporcionan información sobre la edad en que lo hicieron, mientras que aquellos que no han consumido no aportan datos relevantes."),
                   ),
                   
                   h5("Respecto a las variables de vida intracarcelaria"),
                   
                   tags$ul(
                     tags$li("La mayoría de los internos en centros varoniles y mixtos han estado encarcelados por más de 2 años, con los varones destacando por tiempos de reclusión más largos. En contraste, en los centros femeniles, una proporción significativa está encarcelada por menos de 6 meses."),
                     tags$li("En los centros mixtos, un porcentaje notable de internos se encuentra en celdas con 11-97 personas, mientras que en los masculino, predominan las celdas con 0-2 personas. En general, los centros varoniles tienen una mayor proporción de internos en todas las categorías de número de personas por celda."),
                     tags$li("La falta de dinero y la necesidad de trabajar son las principales razones por las cuales los internos dejaron de estudiar. En los centros femeniles, la maternidad y el matrimonio son factores adicionales que contribuyen al abandono de los estudios."),
                     tags$li("En los centros varoniles y mixtos, la mayoría de los internos se identifica como heterosexual, mientras que en los centros femeniles hay una mayor proporción de internos bisexuales y homosexuales. Además, los centros femeniles muestran una mayor proporción de internos que han considerado el suicidio en comparación con otros tipos de centros."),
                     tags$li("La mayoría de los internos fueron detenidos por delitos cometidos o acusaciones falsas, siendo los centros femeniles los que presentan una mayor proporción de internos acusados falsamente. También, los centros femeniles tienen un mayor porcentaje de internos que no han sido sentenciados anteriormente."),
                     tags$li("Los internos que han estado en prisión menos de 6 meses tienden a tener períodos de reclusión más cortos. Por otro lado, aquellos con más de 2 años de prisión se encuentran predominantemente en períodos de reclusión más largos."),
                   ),
            ), #Fin primera columnsa
            
            column(2,
                   # Esta columna está vacía para dejar espacio en blanco
                   p("")
            ),
          ),
  
), #Fin de TabPanel
  tabPanel("ACS Interactivo",
           layout_columns(
             card(card_header("Análisis de Correspondencias Simple"),
                  plotOutput("acsi")),
             layout_columns(
               wellPanel("Perfiles a mostrar",
                         selectInput("filacol", "Perfiles:", choices = c("Fila","Columna","Fila y columna")),
                          ),
               wellPanel("Selecciona las variables",
                         selectInput("var1_cuali_cuali", "Variable cualitativa 1:", choices = varCualitativas),
                         selectInput("var2_cuali_cuali", "Variable cualitativa 2:", choices = varCualitativas)
               ),
               card(card_header("Tabla de perfiles"),
                    plotOutput("perfiles")),
               col_widths = c(12, 12)
             )
           ),
           card(card_header("Comentarios"),
                uiOutput("selected_text"))
           ,)
  ),
  
)
)
)






# Define server logic para la aplicación
server <- function(input, output) {
  
   
  #Esta es la funcion de los comentarios, dado que es bastante especifica, es demasiado extensa
  output$selected_text <- renderUI({
    # Mostrar el texto según la selección
    if (input$var1_cuali_cuali == input$var2_cuali_cuali) {
      return("Las variables seleccionadas son las mismas, no es posible realizar un análisis")
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIPO_CARCEL", "TIEMPO_CARCEL"))){
      (tags$ul(
        tags$li("La mayoría de la población en los tres tipos de cárceles ha pasado más de 2 años en prisión"),
        tags$li("Los centros varoniles y mixtos tienen una alta proporción de personas que llevan más de 2 años encarceladas, con los centros varoniles destacando en tiempos más largos."),
        tags$li("En los centros femeniles, hay una mayor proporción de personas que llevan menos de 6 meses en prisión comparado con los otros tipos de cárceles."),
         ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIPO_CARCEL", "PARO_ESTUDIOS"))){
      (tags$ul(
        tags$li("En general, los resultados muestran que la falta de dinero y la necesidad de trabajar son las principales razones por las cuales las personas encarceladas dejaron de estudiar. "),
        tags$li("En los centros femeniles, la maternidad y el matrimonio son factores relevantes.")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIPO_CARCEL", "ORIENT_SEXUAL"))){
      (tags$ul(
        tags$li("La mayoría de los internos en centros varoniles y mixtos son heterosexuales (96.9% y 94.1% respectivamente)."),
        tags$li("En centros femeniles, hay una mayor proporción de internos bisexuales (14.9%) y homosexuales (6.7%) en comparación con los otros tipos de centros.")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIPO_CARCEL", "CONSIDERO_SUICIDIO"))){
      (tags$ul(
        tags$li("La mayoría de los internos en todos los tipos de cárceles no han considerado el suicidio (89.0% en centros varoniles, 82.4% en centros femeniles, y 86.9% en centros mixtos)."),
        tags$li("Los centros femeniles tienen una mayor proporción de internos que han considerado el suicidio (17.5%) en comparación con los otros tipos de centros.")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIPO_CARCEL", "MOTIVO_DETENCION"))){
      (tags$ul(
        tags$li("La mayoría de los internos en todos los tipos de cárceles fueron detenidos porque cometieron un delito o fueron acusados falsamente de cometer un delito."),
        tags$li("Los centros femeniles tienen una mayor proporción de internos que fueron acusados falsamente de cometer un delito (52.9%) en comparación con los otros tipos de centros.")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIPO_CARCEL", "SENTENCIADO_ANTES"))){
      (tags$ul(
        tags$li("En los centros varoniles, el 78.9% de los internos no han sido sentenciados antes, mientras que en los centros femeniles, este porcentaje es aún mayor (92.1%)."),
        tags$li("Perfiles por Columnas: Del total de internos que han sido sentenciados antes, el 56.9% están en centros varoniles, el 4.6% en femeniles y el 38.5% en mixtos")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIPO_CARCEL","CAR_PERSONAS_CELDA"))){
      (tags$ul(
        tags$li("En los centros mixtos, el 30.2% de los internos están en celdas con 11-97 personas."),
        tags$li("Del total de internos en celdas con 0-2 personas, el 59.7% están en centros varoniles, el 18.2% en femeniles y el 22.1% en mixtos."),
        tags$li("La mayoría de las personas están en celdas con 2-4 personas y en centros varoniles."),
        tags$li("Dentro de cada categoría de número de personas por celda, la mayoría de las personas están en centros varoniles."),
        tags$li("La distribución de personas por celda varía según el tipo de cárcel, pero en general, los centros varoniles tienen una mayor proporción de personas en todas las categorías de número de personas por celda."),
        
        ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIPO_CARCEL","CATEGORIAS_TIEMPO_RECLUIDO"))){
      (tags$ul(
        tags$li("La mayor parte de los reclusos, tanto en centros varoniles, femeniles como mixtos, han estado recluidos en la categoría 0-0.083 años, es decir, entre 0 y aproximadamente un mes."),
        tags$li("En general, los centros varoniles y mixtos tienen una distribución más balanceada entre las distintas categorías de tiempo de reclusión, mientras que en los centros femeniles, la mayoría de las reclusas han tenido reclusiones muy cortas o muy largas.")
        ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIPO_CARCEL","CATEGORIAS_TIEMPO_PROCESO"))){
      (tags$ul(
        tags$li("Las reclusas en centros femeniles tienden a tener tiempos de procesamiento más cortos comparados con los centros varoniles y mixtos."),
        tags$li("Los centros varoniles concentran una mayor parte de los reclusos en tiempos de proceso más largos.")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIEMPO_CARCEL","SEXO"))){
      (tags$ul(
        tags$li("Dentro de cada sexo, la mayoría de los hombres y mujeres han estado en la cárcel por más de 2 años."),
        tags$li("Perfiles por columna: La mayoría de las personas en cada categoría de tiempo de cárcel son hombres, especialmente en las categorías de tiempo más largas.")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIEMPO_CARCEL","NIVEL_ESCOLARIDAD"))){
      (tags$ul(
        tags$li("Dentro de cada nivel de escolaridad, la mayoría de las personas han estado en la cárcel por más de 2 años."),
        tags$li("La mayoría de las personas en cada categoría de tiempo de cárcel tienen un nivel de escolaridad de secundaria.")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIEMPO_CARCEL","MOTIVO_DETENCION"))){
      (tags$ul(
        tags$li("Los motivos de falsa acusación y no poder comprobar inocencia parecen estar presentes de manera significativa en las diferentes duraciones de encarcelamiento."),
        tags$li("Para aquellos que han estado menos de 6 meses en prisión, una gran proporción fue falsamente acusada (49.6%).")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIEMPO_CARCEL","TIEMPO_DEL_DET"))){
      (tags$ul(
        tags$li("Los reclusos que fueron detenidos después de un año o más tienen una proporción más alta de reclusos que han estado en prisión más de 2 años (68.0%)."),
        tags$li("La mayoría de las detenciones ocurren inmediatamente o dentro de unos días del delito, y es raro que los reclusos no recuerden el tiempo que pasó antes de su detención.")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIEMPO_CARCEL","CATEGORIAS_TIEMPO_PROCESO"))){
      (tags$ul(
        tags$li("Los reclusos que han estado en prisión menos de 6 meses tienden a haber cumplido entre 0.083-6.5 años de reclusión."),
        tags$li("Las categorías de tiempo de proceso más cortas (menos de 6 meses) tienen una mayor proporción de reclusos en periodos de tiempo de reclusión más cortos, mientras que aquellos con más de 2 años de prisión se encuentran predominantemente en periodos de reclusión más largos.")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("TIEMPO_CARCEL","CATEGORIAS_TIEMPO_RECLUIDO"))){
      (tags$ul(
        tags$li("La mayoría de los individuos han estado recluidos por menos de 0.083 años, y una proporción significativa ha estado recluida por más de 2 años, especialmente en la categoría de 20-33.333 años."),
        tags$li("Es evidente que las dos variables están muy relacionadas")
      ))
    }  else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("SEXO","ESTADO_CIVIL"))){
      (tags$ul(
        tags$li("La distribución sugiere que los hombres privados de libertad tienden a estar más frecuentemente en uniones libres o casados, mientras que las mujeres tienen una representación algo mayor en el grupo de solteras y viudas.")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("SEXO","HIJOS"))){
      (tags$ul(
        tags$li("A pesar de la diferencia en el número total de hombres y mujeres en la población, una proporción considerable de ambos grupos tiene hijos.")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("SEXO","NIVEL_ESCOLARIDAD"))){
      (tags$ul(
        tags$li("Hay más hombres que mujeres en todos los niveles de escolaridad, pero la distribución dentro de los sexos es relativamente similar."),
        tags$li("Las diferencias entre hombres y mujeres en términos de escolaridad no son tan marcadas, aunque las mujeres tienen una mayor proporción en niveles educativos más altos como licenciatura o profesional y maestría o doctorado."),
        tags$li("El porcentaje de individuos sin ningún nivel de escolaridad es bajo en ambos sexos, pero hay más hombres en esta categoría.")
        ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("SEXO","PARO_ESTUDIOS"))){
      (tags$ul(
        tags$li("Los hombres tienen una mayor tendencia a abandonar los estudios por razones laborales (tener que trabajar), mientras que las mujeres tienden más a casarse o embarazarse como motivo principal para dejar de estudiar."),
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("SEXO","IDENTI_GENERO"))){
      (tags$ul(
        tags$li("La gran mayoría de personas privadas de la libertad se identifican con su sexo biológico. "),
        tags$li("Existe un pequeño porcentaje de personas que se identifican como transgénero en los cuales predominan los hombres")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("SEXO","ORIENT_SEXUAL"))){
      (tags$ul(
        tags$li("Hay una mayor proporción de mujeres que se identifican como bisexuales (12.9%) en comparación con los hombres (2%)."),
        tags$li("Mujeres tienen una representación más alta en las categorías de bisexual y homosexual en comparación con los hombres.")
       ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("SEXO","CONSIDERO_SUICIDIO"))){
      (tags$ul(
        tags$li("Un mayor porcentaje de mujeres (18%) ha considerado el suicidio en comparación con los hombres (11.2%)."),
        ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("NACION","NIVEL_ESCOLARIDAD"))){
      (tags$ul(
        tags$li("La mayoría de los individuos de México tienen educación secundaria y primaria, mientras que en Estados Unidos predominan aquellos con educación preparatoria o bachillerato."),
      ))
    }  else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("NACION","PARO_ESTUDIOS"))){
      (tags$ul(
        tags$li("El 98.8% de los individuos que dejaron de estudiar porque “Tenía que trabajar” son de México."),
        tags$li("En otros países dejaron de estudiar porque “No tenía dinero”")
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("NACION","SALARIO_MES_ANTERIOR"))){
      (tags$ul(
        tags$li("La mayoría de los individuos en México ganan menos de $3,000 o entre $3,000 y $5,500, mientras que en Estados Unidos predominan aquellos que ganan más de $11,000."),
      ))
    } else if (all(c(input$var1_cuali_cuali, input$var2_cuali_cuali) %in% c("NACION","CAR_PERSONAS_CELDA"))){
      (tags$ul(
        tags$li("La mayoría de los individuos en México están en celdas con 4-6 personas, mientras que en Estados Unidos y otros países predominan aquellos en celdas con 0-2 personas."),
      ))
    } else {return("No se han visto relaciones claras entre las variables")}
    
    
    
    
  })
  #bs_themer()
  output$scatterplot <-renderPlot({
    ggplot(Tabla1, aes_string(x = input$var1_cuanti_cuanti, y = input$var2_cuanti_cuanti)) +
      geom_point() +
      labs(x = input$var1_cuanti_cuanti, y = input$var2_cuanti_cuanti) +
      theme_bw()
  })
  
  output$perfiles <- renderPlot({
    var1 <- sym(input$var1_cuali_cuali)
    var2 <- sym(input$var2_cuali_cuali)

    tc<-table(Tabla1[[input$var1_cuali_cuali]],Tabla1[[input$var2_cuali_cuali]])
    
    plotct(tc, "row", col=1+(1:ncol(tc)))
  })
  
  

  
  output$acsi <- renderPlot({
    K <- unclass(table(as.factor(Tabla1[[(input$var1_cuali_cuali)]]), as.factor(Tabla1[[(input$var2_cuali_cuali)]])))
    acs <- dudi.coa(K, scannf = FALSE, nf = 3)
    
    plotacs <- plot(acs, ex = 1, ey = 2, asp = 1, cframe = 1, gg = TRUE,
                    Tcol = input$filacol != "Fila",
                    Trow = input$filacol != "Columna")
    
    plotacs
  })
  
  
  
  output$correlacion<-renderPlot({
    corrplot(cor(Tabla1[c(input$var1_cuanti_cuanti,input$var2_cuanti_cuanti)]),method = "color",type="upper",tl.srt = 15)
    })
}

# Corre la aplicación Shiny
shinyApp(ui = ui, server = server)
