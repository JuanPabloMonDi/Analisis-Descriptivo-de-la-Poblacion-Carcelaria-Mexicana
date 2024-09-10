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
            layout_columns(
    card(card_header("Diagrama de dispersión"),
         plotOutput("scatterplot")),
    layout_columns(
      #card(card_header("Selección de variables"),
           wellPanel("Selecciona las variables",
                     selectInput("var1_cuanti_cuanti", "Variable cuantitativa 1:", choices = varCuantitativas),
                     selectInput("var2_cuanti_cuanti", "Variable cuantitativa 2:", choices = varCuantitativas)
           ),
           #),
      card(card_header("Correlación entre las variables"),
           plotOutput("correlacion",height = "70%")),
      col_widths = c(12, 12)
    )
  ),
  card(card_header("Comentarios"))
  ),
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
           card(card_header("Comentarios"))
           ,)
  ),
  
)
)
)





# Define server logic para la aplicación
server <- function(input, output) {
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
