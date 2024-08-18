
# list.of.packages <- c(
#  "shiny", "shinyjs", "shinyBS", "remotes", "shinycssloaders", "shinyFiles"
# )
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install.packages("remotes")
# remotes::install_github("daattali/shinycssloaders")

library(shiny)
library(shinyjs)
library(shinyBS)
library(remotes)
library(shinycssloaders)
library(shinyFiles)

# Define UI
fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
  ),
  
  useShinyjs(),
  
  tags$style(HTML("
    /* ----- títulos de las pestañas(tabsetPanel) ----- */
    .nav-tabs > li > a {
      color: #5F249F;
    }
    
    /*---------- buttons ----------*/
    .btn-default {
      background-color: #5F249F;
      color: white;
    }
    .btn-default:hover {
      background-color: #5F247F;
      color: white;
    }
    
    .download-manual {
      # display: flex;
      # justify-content: end;
      margin: 0;
      margin-bottom: 10px;
      padding: 0;
    }
    
    /* ---------- footer ---------- */
    footer {
      background-color: #f5f5f5;
      padding: 5px;
      text-align: center;
      left: 0; 
      bottom: 0; 
      width: 100%;
      border-radius: 5px;
      border: 1px solid #e3e3e3;
    }
    
    footer p {
      margin: 0;
      padding: 0;
    }
  ")),
  
  tags$nav(
    class = "navbar",
    tags$div(
      class = "container",
      tags$img(src = "img/logo-cenicana.png", alt = "Logo de la aplicación")
    )
  ),
  
  fluidRow(
    column(10, titlePanel("Análisis de varianza")),  # Columna para el título
    column(2, align = "right", downloadButton("download_button", "")),  # Columna para el botón de descarga
    style = "margin-bottom: 10px; display:flex; align-items:center"
  ),
  
  sidebarLayout(
    sidebarPanel(
      
      actionButton("variable_clasificatoria", ""),
      textAreaInput("class_var", "Variable Clasificatoria:",value = "Rep,Block,Name"),
      
      
      ######
      actionButton("variable_by", ""),
      textAreaInput("by", "Analisis por grupo:",value = "Ambiente"),
      #####
      
     
      actionButton("fijos_info", ""),
      textInput("fijos", "Fijos: ", value = "Name"),
      
      actionButton("aleatorios_info", ""),
      textInput("aleatorios", "Aleatorios:", value = "Rep"),
      
      actionButton("file-input", ""),
      fileInput(inputId = "ds", label = "Seleccionar Archivo", buttonLabel = "Archivo",
                accept = c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv'), multiple = F,placeholder = "Sin archivo"),
      
      actionButton("variable_respuesta", ""),
      selectInput(inputId = "var", label = "Variable de Respuesta:", choices=c()),
      
      actionButton("fijos_test", ""),
      selectInput("TestFixed", "Test efectos fijos:", choices = c("SI", "NO")),
      
      actionButton("lsmeans_info", ""),
      selectInput("lsmeans", "Medias Ajustadas (lsmeans):", choices = c()),
      
      actionButton("post_hoc_test", ""),
      selectInput("post", "Post-hoc Tests:", choices = c("fdr","none", "tukey", "bonferroni", "sidak")),
      
      actionButton("heterocedasticidad_info", ""),
      selectInput("heteroce", "Heterocedasticidad:", 
                  choices = c("NO", "SI")),
      
      conditionalPanel(
        condition = "input.heteroce == 'SI'",
        
        actionButton("varcom_info", ""),
        textInput("varcom", "Varcom:", value = ""),
        
        actionButton("het_info", ""),
        textInput("HET", "Het: ", value = "")
      ),
      checkboxInput("VB", "Detalles sobre iteración ", value = FALSE),
      actionButton("run", "Ejecutar función"),
      
      # Tooltips de la aplicación
      bsTooltip("variable_clasificatoria", title = "Variables que se utilizaran en el analisis de varianza para clasificar los factores de los modelos estadisticos. Valores típicos  para esta variable son Tratamiento, Variedad, Repeticiones, Bloques, etc. ", 
                placement = "right"),
      bsTooltip("file-input", title = "Cargue un archivo en formato .csv",
                placement = "right"),
      bsTooltip("variable_respuesta", title = "Variables que se miden en las unidades experimentales y que corresponden a la respuesta cuantificable de cada uno de los tratamientos utilizados en el experimento. Valores típicos para esta variable son peso por parcela, contenido de sacarosa, altura de la planta, etc.",
                placement = "right"),
      bsTooltip("fijos_info", title = "Corresponde a todos los factores que el investigador quiere establecer como fijos. Comunmente, los factores fijos son determinados por el investigador y se establecen acorde con: i) Si fueron seleccionados por el investigador previamente al experimento y no constituyen una muestra aleatoria. ii) Si el investigador desea estimar los efectos de cada factor y no las varianzas de cada uno. iii) Si el investigador desea hacer inferencias solo sobre los niveles seleccionados del factor y no sobre la poblacion de posibles niveles. Valores típicos para esta variable son tratamiento, variedades, etc.",
                placement = "right"),
      bsTooltip("aleatorios_info", title = "Corresponde a todos los factores que el investigador quiere establecer como aleatorios. Comunmente, los factores aleatorios son determinados por el investigador y se establecen acorde con: i) Si fueron seleccionados como una muestra representativa y aleatoria de la poblacion del factor. ii) Si el investigador desea estimar las varianzas de cada uno de los factores. iii) Si el investigador desea hacer inferencias sobre toda la poblacion. Valores típicos para esta variable son interacciones como genotipo por variedad, repeticion por genotipo, etc.", 
                placement = "right"),
      bsTooltip("fijos_test", title = "Una opcion de Si o No, para indicar si estamos interesados en que se imprima la prueba de los efectos fijos.",
                placement = "right"),
      bsTooltip("lsmeans_info", title = "Corresponde a un listado de variables seleccionadas como efectos fijos, sobre las cuales se quiere imprimir los valores de las medias ajustadas o lsmeans.", 
                placement = "right"),
      bsTooltip("post_hoc_test", title = "Tipo de estadistico que se quiere utilizar para probar la significancia en la diferencia entre efectos fijos.",
                placement = "right"),
      bsTooltip("heterocedasticidad_info", title = "Opcion de Si o No, para evaluar la hipotesis de varianzas residuales homogeneas (NO) o heterogeneas (SI) acorde con un factor. Al seleccionar esta opcion en Si, el modelo evaluara las varianzas residuales.", 
                placement = "right"),
      bsTooltip("varcom_info", title = "Opción para decirle al modelo que no calcule las varianzas de los efectos aleatorios de forma iterativa, sino que use las que se les va a proporcionar. Esta opción se utiliza cuando se conocen previo al analisis las varianzas y se quiere reducir el tiempo de procesamiento.",
                placement = "right"),
      bsTooltip("het_info", title = "Nombre del factor que se quiere utilizar para probar la heterocedasticidad en el modelo. Típicamente se utiliza el efecto del ambiente, para probar la heterocedasticidad en los residuales acorde con el ambiente de evaluacion.",
                placement = "right"),
    ),
    mainPanel(
      
      tabsetPanel(
        tabPanel("Prueba Efectos Fijos",
                 tableOutput("Fixed_table"),
                 shinyjs::hidden(downloadButton("download_fixed_table", "Descargar resultado"))),
        tabPanel("Componentes de Varianza",
                 tableOutput("coefs_table"),
                 shinyjs::hidden(downloadButton("download_coefs_table", "Descargar resultado"))),
        tabPanel("BLUEs",
                 tableOutput("Blues_table"),
                 shinyjs::hidden(downloadButton("download_BLUEs_table", "Descargar resultado"))),
        tabPanel("Medias Ajustadas",
                 tableOutput("lsmeans_table"),
                 shinyjs::hidden(downloadButton("downloads_lmeans_table", "Descargar resultado"))),
        tabPanel("Post-Hoc Test",
                 tableOutput("PHT"),
                 shinyjs::hidden(downloadButton("download_posthoctest_table", "Descargar resultado"))),
        tabPanel("BLUPs",
                 tableOutput("ranef_table"),
                 shinyjs::hidden(downloadButton("download_BLUPs_table", "Descargar resultado"))),
        tabPanel("Resumen del modelo",
                 verbatimTextOutput("summary_text"),
                 shinyjs::hidden(downloadButton("download_summary_table", "Descargar resumen"))),
      ),
      textOutput("result")
    ),
    position = "left"
  ),
  
  tags$footer(
    tags$p("Laboratorio Biotecnología - v1.0"),
    tags$p("Cenicaña"),
    tags$p("[Samuel Pulgarin Muñoz]")
  )
)
