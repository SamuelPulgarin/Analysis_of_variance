#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
# d

#remove.packages("dplyr")
#install.packages("dplyr",dependencies=TRUE)
# list.of.packages <- c("shiny","shinyjs", "dplyr", "tidyverse","stringr","data.table",
#                       "sommer","gtools","lme4","openxlsx","car","shinycssloaders",
#                       "remotes","shinyFiles", "readr","lsmeans", "emmeans","grafify", "shinyBS")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) {install.packages(new.packages)}
# 
# invisible(lapply(list.of.packages, library, character.only = TRUE))

# Tamaño maximo de archivo en bytes
options(shiny.maxRequestSize=30*1024^2)
rm(list = ls())
gc()

library(shiny)
library(shinyjs)
library(shinyBS)
library(dplyr)
library(tidyverse)
library(stringr)
library(data.table)
library(sommer)
library(gtools)
library(lme4)
library(openxlsx)
library(car)
library(shinycssloaders)
library(remotes)
library(shinyFiles)
library(readr)
library(lsmeans)
library(emmeans)
library(grafify)


# Define UI for application that draws a histogram
ui <- fluidPage(
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
      textAreaInput("class_var", "Variable Clasificatoria:",value = "Rep,Block,Ambiente,Name"),
      
      actionButton("file-input", ""),
      fileInput(inputId = "ds", label = "Seleccionar Archivo", buttonLabel = "Archivo",
                accept = c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv'), multiple = F,placeholder = "Sin archivo"),
      
      actionButton("variable_respuesta", ""),
      selectInput(inputId = "var", label = "Variable de Respuesta:", choices=c()),
      
      actionButton("fijos_info", ""),
      textInput("fijos", "Fijos: ", value = "Ambiente,Name"),
      
      actionButton("aleatorios_info", ""),
      textInput("aleatorios", "Aleatorios:", value = "Rep:Ambiente,Block:Rep:Ambiente"),
      
      actionButton("fijos_test", ""),
      selectInput("TestFixed", "Test efectos fijos:", choices = c("SI", "NO")),
      
      actionButton("lsmeans_info", ""),
      selectInput("lsmeans", "Medias Ajustadas (lsmeans):", choices = c()),
      
      actionButton("post_hoc_test", ""),
      selectInput("post", "Post-hoc Tests:", choices = c("fdr","none", "tukey", "bonferroni", "sidak")),
      
      actionButton("heterocedasticidad_info", ""),
      selectInput("heteroce", "Heterocedasticidad:", choices = c("NO", "SI")),

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
        tabPanel("Gráficos de diagnóstico",
                 plotOutput("residuals_plot"),
                 plotOutput("qqplot"))
      ),
      textOutput("result")
    ),
    position = "left"
  ),
  
  tags$footer(
    tags$p("Laboratorio Biotecnología - v1.0"),
    tags$p("cenicaña"),
    tags$p("[Samuel Pulgarin Muñoz]")
  )
)



server <- function(input, output, session) {
  
  # Observar cuando se carga un archivo y almacenar el dataframe
  ds <- reactiveVal(NULL)
  
  observeEvent(input$ds, {
    
    req(input$ds)
    
    df <- fread(input$ds$datapath, na.strings = c("", "NA",NA), data.table=F)
    
    # Filtrar el dataframe
    df_filtered <- df #%>% 
       # filter(Ambiente %in% c("Balsora_1", "Balsora_2") & Rep < 3 & Periodo >= 12)
    
    # Seleccionar columnas de manera dinámica
    # selected_columns <- c("Ambiente", "Rep", "Block", "Name", input$var)
    # df_selected <- df_filtered[, selected_columns, drop = FALSE]  # drop = FALSE para mantener el dataframe
    updateSelectInput(session = session,inputId = "var",
                      choices = mixedsort(colnames(df_filtered)[-grep(paste(c(str_split(input$class_var, ",")[[1]]),
                                                                           collapse = "|"), colnames(df_filtered))]))
    updateSelectInput(session = session,inputId = "lsmeans",
                      choices = mixedsort(colnames(df_filtered)[grep(paste(c(str_split(input$fijos, ",")[[1]]),
                                                                           collapse = "|"), colnames(df_filtered))]))
    ds(df_filtered)
  })
  
  # Función varianza
  varianza <- reactive({
    
    req(input$run)
    req(ds()) 
    
    print("Empecé con la función")
    a <- function(var=var,ds=ds, fijos = 1, aleatorios = NULL, heteroce = "No",
                  varcom = NULL, VB = FALSE, class_var = class_var, HET = NULL,
                  TestFixed = "No") {
      
      cv = colnames(ds)[grep(paste(paste0("^",class_var,"$"), collapse = "|"), colnames(ds), ignore.case = T)]
      ds <- ds %>% mutate_at(c(paste(cv)), as.factor)
      if (!is.null(HET)) {
        ds <- ds %>% mutate(HET = get(HET))
      }
      
      vr = colnames(ds)[grep(paste0("^",var,"$"), colnames(ds), ignore.case = T)]
      
      if (TestFixed %in% TestFixed[grep(paste(c("Si","Yes"), collapse = "|"), 
                                        TestFixed,ignore.case = T)]) {
        if (is.null(aleatorios)){
          fj = (paste0(c(vr," ~ ",paste0(c(fijos), collapse =  "+")),
                       collapse = "" ))
          
          md = as.formula(paste0(fj))
          qwe <- lm(formula = md, data = ds)
        } else {
          fj = (paste0(c(vr," ~ ",paste0(c(fijos), collapse =  "+")),
                       collapse = "" ))
          
          al = (paste0("(1|",aleatorios,")", collapse = "+" ))
          md = as.formula(paste0(fj,"+",al))
          qwe <- lmer(formula = md, data = ds, REML = T)
        }
    } else if (TestFixed %in% TestFixed[grep(paste(c("No"), collapse = "|"), 
                                               TestFixed,ignore.case = T)]) {
        if (heteroce %in% heteroce[grep(paste(c("Si","Yes"), collapse = "|"), heteroce,ignore.case = T  )]) {
          if (is.null(aleatorios)){
            qwe = mmer( fixed =  as.formula(paste0(c(vr," ~ ",paste0(c(fijos), collapse =  "+") ), collapse = "" )),
                        init =varcom ,
                        data=ds,
                        rcov = ~vsr(dsr(HET), units),
                        dateWarning = F,
                        verbose = VB)
          } else {
            qwe = mmer( fixed =  as.formula(paste0(c(vr," ~ ",paste0(c(fijos), collapse =  "+") ), collapse = "" )),
                        random = as.formula(paste0(c(" ~ ",paste0(c(aleatorios), collapse =  "+") ), collapse = "" )),
                        init=varcom,
                        data=ds,
                        rcov = ~vsr(dsr(HET), units),
                        dateWarning = F,
                        verbose = VB)
          }
        } else if (heteroce %in% heteroce[grep(paste(c("NO"), collapse = "|"),
                                               heteroce,ignore.case = T  )]) {
          
          if (is.null(aleatorios)){
            qwe = mmer( fixed =  as.formula(paste0(c(vr," ~ ",paste0(c(fijos), collapse =  "+") ), collapse = "" )),
                        init =varcom ,
                        data=ds,
                        rcov = ~units,dateWarning = F, verbose = VB)
          } else {
            qwe = mmer( fixed =  as.formula(paste0(c(vr," ~ ",paste0(c(fijos), collapse =  "+") ), collapse = "" )),
                        random = as.formula(paste0(c(" ~ ",paste0(c(aleatorios), collapse =  "+") ), collapse = "" )),
                        init =varcom ,
                        data=ds,
                        rcov = ~units,dateWarning = F, verbose = VB)
          }
        }
      }
      
      return(qwe)
      
    }
    
    
    # Recoger los datos ingresados en los inputs
    var <- input$var
    ds <- input$ds
    # ruta_completa <- file.path("//192.168.153.238/biodata10/rstudio/fernando/20.Funciones/ShinnyApp",
    #                            "Base_datos_compilada_SAM_12Ambientes_Solo_Quimicas_7Mayo2024_Datos_CURADOS_Ronda1_SIN_OUTLIERS.csv")
    # ds <- fread(ruta_completa, data.table = FALSE, na.strings = c("", NA))
    # d1 <- subset(ds, Ambiente %in% c("Balsora_1","Balsora_2") & Rep < 3 & Periodo >= 12)
    # ds <- d1[,c("Ambiente","Rep","Block","Name","Sacarosa_cana")]
    # d1 = ds
    
    fijos <- input$fijos
    fijos <- if (input$fijos != "") {
      str_split(input$fijos, ",")[[1]]
    } else {
      NULL
    }
    
    aleatorios <- input$aleatorios
    aleatorios <- if (input$aleatorios != "") {
      str_split(input$aleatorios, ",")[[1]]
    } else {
      NULL
    }
    
    heteroce <- input$heteroce
    VB <- input$VB
    TestFixed <- input$TestFixed
    
    varcom <- input$varcom
    varcom <- if (input$varcom != "") {
      str_split(input$varcom, ",")[[1]]
    } else {
      NULL
    }
    
    HET <- input$HET
    HET <- if (input$HET != "") {
      str_split(input$HET, ",")[[1]]
    } else {
      NULL
    }
    
    class_var <- input$class_var
    class_var <- if (input$class_var != "") {
      str_split(input$class_var, ",")[[1]]
    } else {
      NULL
    }
    
    
    result <- a(var = var, ds = ds(), fijos = fijos, aleatorios = aleatorios,
                heteroce = heteroce, varcom = varcom, VB = VB,
                class_var = class_var, HET = HET,TestFixed = TestFixed)
    
    result
  })
  
  observeEvent(input$run, {
    
    req(input$ds)
    
    showPageSpinner() # start loading
    
    result <- varianza()
    
    # Componentes de varianza
    output$coefs_table <- renderTable({
      
      shinyjs::show("download_coefs_table") # Botón de descarga
      
      if(class(result) == "mmer") {
        coefs_df <- as.data.frame(summary(result)$varcomp) %>% 
          rownames_to_column("Efecto") 
        coefs_df 
      } else if(class(result) == "lmerMod"){
        coefs_df <- as.data.frame(summary(result)$varcor) 
        coefs_df <- coefs_df[,!colnames(coefs_df) %in% c("var1","var2")]
        colnames(coefs_df)[1] <- "Efecto"
        coefs_df
      }
    })
    
    # Prueba Efectos Fijos
    output$Fixed_table <- renderTable({
      
      shinyjs::show("download_fixed_table") # Botón de descarga
      
      if(class(result) == "mmer") {
        Fixed_table <- NULL
        Fixed_table
      } else if(class(result) %in% c("lmerMod","lm")){
        Fixed_table <- as.data.frame(anova(result)) %>% 
          rownames_to_column("Efecto")
        # colnames(Fixed_table)[2] <- "DF"
        Fixed_table
      }
    })
    
    # BLUPs
    output$ranef_table <- renderTable({
      
      shinyjs::show("download_BLUPs_table") # Botón de descarga
      
      if(class(result) == "mmer") {
        jkl = names(result$U)
        ranef_df <- NULL
        for (jk in 1:length(jkl)) {
          asd = as.data.frame(result[['U']][[paste0(jkl[jk])]]) %>% rownames_to_column("Efecto") %>% 
            mutate(Effect = jkl[jk])
          ranef_df <- rbind(ranef_df,asd)
          rm(asd)
        } 
        ranef_df 
      } else if(class(result) == "lmerMod"){
        
        ranef_df = as.data.frame(ranef(result)) 
        ranef_df <- ranef_df[,!colnames(ranef_df) %in% c("term")]
        colnames(ranef_df) <- c("Efecct", "Efecto","Estimate","SD")
        ranef_df
        
      }
    })
    
    # Resumen del modelo
    output$summary_text <- renderPrint({
      
      shinyjs::show("download_summary_table") # Botón descarga
      
      summary_stats <- summary(result)
      summary_stats
    })
    
    
    # LSMEANS
    output$lsmeans_table <- renderTable({
      
      shinyjs::show("downloads_lmeans_table") # Botón de descarga
      
      lsmeans_table <- as.data.frame(lsmeans(result, paste0(input$lsmeans))) %>% 
        mutate(lsmean = as.numeric(lsmean))
      lsmeans_table
    })
    
    # Post Hoc Test
    output$PHT <- renderTable({
      
      shinyjs::show("download_posthoctest_table")
      
      PHT <- as.data.frame(posthoc_Pairwise(Model = result, Fixed_Factor = paste0(input$lsmeans),
                              P_Adj = input$post)) %>% 
        mutate(emmean = as.numeric(emmean))
      PHT
    })
    
    # BLUES
    output$Blues_table <- renderTable({
      
      shinyjs::show("download_BLUEs_table") # Botón de descarga
      
      if(class(result) == "mmer") {
        Blues_table <- as.data.frame(coef.mmer(result))
        Blues_table
      } else if(class(result) == "lmerMod"){
        
        Blues_table = as.data.frame(fixef(result)) %>% 
          rownames_to_column("Efecto") %>% 
          mutate(Efecto= gsub(paste(c("\\(","\\)"), collapse = "|"), "", Efecto))
        colnames(Blues_table)[2] <- "Estimate"
        Blues_table
        
      } else if(class(result) == "lm"){
        
        Blues_table = as.data.frame(coef(result)) %>% 
          rownames_to_column("Efecto") %>% 
          mutate(Efecto= gsub(paste(c("\\(","\\)"), collapse = "|"), "", Efecto))
        colnames(Blues_table)[2] <- "Estimate"
        Blues_table
        
      }
    })
    
    # Sección: Gráficos de diagnóstico
    diagnostic_plots <- function(model) {
      p1 <- plot(model)
      p2 <- plot(model, resid = TRUE)
      list(residuals = p1, qqplot = p2)
    }
    
    diagnostic_plots_data <- reactive({
      diagnostic_plots(result)
    })
    
    output$residuals_plot <- renderPlot({
      diagnostic_plots_data()$residuals
    })
    
    output$qqplot <- renderPlot({
      diagnostic_plots_data()$qqplot
    })
    
    
    # Sección: Descargar resultados:
    
    output$download_button <- downloadHandler(
      filename = function() {
        "manual_de_uso.pdf"
      },
      content = function(file) {
        file.copy("documentation/manual_de_uso.pdf", file)
      }
    )
    
    output$download_fixed_table <- downloadHandler(
      filename = function() {
        paste("Prueba de efectos fijos", ".csv", sep = "")
      },
      content = function(file) {
        if(class(result) == "mmer") {
          Fixed_table <- NULL
        } else if(class(result) %in% c("lmerMod","lm")){
          Fixed_table <- as.data.frame(anova(result)) %>% 
            rownames_to_column("Efecto")
        }
        write.csv(Fixed_table, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    
    output$download_coefs_table <- downloadHandler(
      filename = function() {
        paste("Componentes de varianza", ".csv", sep = "")
      },
      content = function(file){
        if(class(result) == "mmer") {
          coefs_df <- as.data.frame(summary(result)$varcomp) %>% 
            rownames_to_column("Efecto") 
        } else if(class(result) == "lmerMod"){
          coefs_df <- as.data.frame(summary(result)$varcor) 
          coefs_df <- coefs_df[,!colnames(coefs_df) %in% c("var1","var2")]
          colnames(coefs_df)[1] <- "Efecto"
        }
        write.csv(coefs_df, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    
    output$download_BLUEs_table <- downloadHandler(
      filename = function(){
        paste("BLUEs", ".csv", sep = "")
      },
      content = function(file){
        if(class(result) == "mmer") {
          Blues_table <- as.data.frame(coef.mmer(result))
          
        } else if(class(result) == "lmerMod"){
          Blues_table = as.data.frame(fixef(result)) %>% 
            rownames_to_column("Efecto") %>% 
            mutate(Efecto= gsub(paste(c("\\(","\\)"), collapse = "|"), "", Efecto))
          colnames(Blues_table)[2] <- "Estimate"
          
        } else if(class(result) == "lm"){
          Blues_table = as.data.frame(coef(result)) %>% 
            rownames_to_column("Efecto") %>% 
            mutate(Efecto= gsub(paste(c("\\(","\\)"), collapse = "|"), "", Efecto))
          colnames(Blues_table)[2] <- "Estimate"
        }
        write.csv(Blues_table, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    
    output$downloads_lmeans_table <- downloadHandler(
      filename = function(){
        paste("Medias Ajustadas", ".csv", sep = "")
      },
      content = function(file){
        lsmeans_table <- as.data.frame(lsmeans(result, paste0(input$lsmeans))) %>% 
          mutate(lsmean = as.numeric(lsmean))
        
        write.csv(lsmeans_table, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    
    output$download_posthoctest_table <- downloadHandler(
      filename = function(){
        paste("Post-hot-test", ".csv", sep = "")
      },
      content = function(file){
        PHT <- as.data.frame(posthoc_Pairwise(Model = result, Fixed_Factor = paste0(input$lsmeans),
                                              P_Adj = input$post)) %>% 
          mutate(emmean = as.numeric(emmean))
        
        write.csv(PHT, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    
    output$download_BLUPs_table <- downloadHandler(
      filename = function(){
        paste("BLUPs", ".csv", sep = "")
      },
      content = function(file){
  
        if(class(result) == "mmer") {
          jkl = names(result$U)
          ranef_df <- NULL
          
          for (jk in 1:length(jkl)) {
            asd = as.data.frame(result[['U']][[paste0(jkl[jk])]]) %>% rownames_to_column("Efecto") %>% 
              mutate(Effect = jkl[jk])
            ranef_df <- rbind(ranef_df,asd)
            rm(asd)
          } 
          
        } else if(class(result) == "lmerMod"){
          ranef_df = as.data.frame(ranef(result)) 
          ranef_df <- ranef_df[,!colnames(ranef_df) %in% c("term")]
          colnames(ranef_df) <- c("Efecct", "Efecto","Estimate","SD")
        }
        write.csv(ranef_df, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    
    output$download_summary_table <- downloadHandler(
      filename = function(){
        paste("Resumen del modelo", ".txt", sep = "")
      },
      content = function(file){
        summary_stats <- capture.output(summary(result))
        
        cat(summary_stats, file = file, sep = "\n")
      },
      contentType = "text/plain"
    )
    
    hidePageSpinner() # stop loading
    
  })
  
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

