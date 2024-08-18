
#list.of.packages <- c("shiny", "shinyjs", "remotes", "shinyFiles", "shinycssloaders", "dplyr",
#                          "tidyverse", "stringr", "data.table", "sommer", "gtools",
#                          "lme4", "openxlsx", "car", "readr", "lsmeans", "emmeans",
#                          "grafify", "R.utils", "promises", "future")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#install.packages("remotes")
#remotes::install_github("daattali/shinycssloaders")
library(shinyBS)
library(shiny)
library(shinyjs)
library(remotes)
library(shinyFiles)
library(shinycssloaders)
library(dplyr)
library(tidyverse)
library(stringr)
library(data.table)
library(sommer)
library(gtools)
library(lme4)
library(openxlsx)
library(car)
library(readr)
library(lsmeans)
library(emmeans)
library(grafify)
library(R.utils)
library(promises)
library(future)
library(lmerTest)

source("global.R")

###Tamaño maximo de archivo en bytes ###
max_file_size <- 100 * 1024^2
options(shiny.maxRequestSize=100*1024^2)

# Sessiones asyncronas
plan(multisession)

# Define server logic
function(input, output, session) {
  
  #' ----------------------------------------------------------------------------------------------
  #### 1. Upload files - Save dataframe ####
  #' ----------------------------------------------------------------------------------------------

  ds <- reactiveVal(NULL)
  observeEvent(input$ds, {

    # Verificar tamaño del archivo
    file_info <- file.info(input$ds$datapath)
    file_size <- file_info$size

    if (file_size > max_file_size){
      showModal(modalDialog(
        title = "Tamaño de archivo excedido",
        "Ha intentado cargar un archivo que supera los ", round(max_file_size / 1024^2, 2), "MB. Por favor, seleccione un archivo más pequeño o comuníquese con los administradores",
        footer = NULL,
        easyClose = TRUE
      ))
      return()
    }
    
    req(input$ds)
    
    df <- fread(input$ds$datapath, na.strings = c("", "NA",NA), data.table=F)
    
    df_filtered <- df
    
    updateSelectInput(session = session,inputId = "var",
                      choices = mixedsort(colnames(df_filtered)[-grep(paste(c(str_split(input$class_var, ",")[[1]]),
                                                                            collapse = "|"), colnames(df_filtered))]))
    updateSelectInput(session = session,inputId = "lsmeans",
                      choices = mixedsort(c(str_split(input$fijos,",")[[1]])))
    ds(df_filtered)
  })
  
  
  #' ----------------------------------------------------------------------------------------------
  ####2. Get data ####
  #' ----------------------------------------------------------------------------------------------

  observeEvent(input$run, {
    
    #req(input$ds)
    #req(ds()) 
    
    var <- isolate(input$var)
    
    fijos <- isolate(input$fijos)
    fijos <- if (input$fijos != "") {
      str_split(input$fijos, ",")[[1]]
    } else {
      NULL
    }
    
    aleatorios <- isolate(input$aleatorios)
    aleatorios <- if (input$aleatorios != "") {
      str_split(input$aleatorios, ",")[[1]]
    } else {
      NULL
    }
    
    by = isolate(input$by)
    by = if (input$by != "") {
      str_split(input$by, ",")[[1]]
    } else {
      NULL
    }
    
    heteroce <- isolate(input$heteroce)
    VB <- isolate(input$VB)
    TestFixed <- isolate(input$TestFixed)
    
    varcom <- isolate(input$varcom)
    varcom <- if (input$varcom != "") {
      str_split(input$varcom, ",")[[1]]
    } else {
      NULL
    }
    
    HET <- isolate(input$HET)
    HET <- if (input$HET != "") {
      str_split(input$HET, ",")[[1]]
    } else {
      NULL
    }
    
    class_var <- isolate(input$class_var)
    class_var <- if (input$class_var != "") {
      str_split(input$class_var, ",")[[1]]
    } else {
      NULL
    }
    

    #' ----------------------------------------------------------------------------------------------
    #### 3. Extract reactive data ####
    #' ----------------------------------------------------------------------------------------------
  
    by <- isolate(by)
    var <- isolate(var)
    ds_value <- isolate(ds())
    fijos <- isolate(fijos)
    aleatorios <- isolate(aleatorios)
    heteroce <- isolate(heteroce)
    varcom <- isolate(varcom)
    VB <- isolate(VB)
    HET <- isolate(HET)
    TestFixed <- isolate(TestFixed)
    class_var <- isolate(class_var)

    #' ----------------------------------------------------------------------------------------------
    #### 4. Asynchronous process ####
    #' ----------------------------------------------------------------------------------------------

    showPageSpinner()
    
    future_promise({
      
      withTimeout({ 
        if (is.null(by)) {
          cat("Paso 1 sin by\n")
          vr = colnames(ds_value)[grep(paste(paste0("^",var,"$"), collapse = "|"),
                                       colnames(ds_value), ignore.case = T)]
          
          cat(paste0("Variable ", vr,"\n"))
          
          ds1 <- ds_value[!is.na(ds_value[,vr]),]
          cat("Inicio lmer\n")
          
          result <-varianza(var = var, ds = ds1, fijos = fijos,
                            aleatorios = aleatorios, heteroce = heteroce,
                            varcom = varcom, VB = VB, class_var = class_var,
                            HET = HET, TestFixed = TestFixed)
          #cat("Finalizo lmer\n")
          
        } else {
          cat("Paso 1 con by\n")
          vr = colnames(ds_value)[grep(paste(paste0("^",var,"$"), collapse = "|"),
                                       colnames(ds_value), ignore.case = T)]
          cat(paste0("Variable ", vr,"\n"))
          
          ds1 <- ds_value[!is.na(ds_value[,vr]),]
          
          cat("Paso 2, se eliminaron ",nrow(ds_value) - nrow(ds1),"\n")
          
          BY = colnames(ds1)[grep(paste(paste0("^",by,"$"), collapse = "|"),
                                  colnames(ds1), ignore.case = T)]
          
          cat("Se inicia la creacion de la columna grupo\n")
          if (length(BY) > 1) {
            cat("Columna Grupo cuando By > 1\n")
            ds1$BYZZZZ <- apply(ds1[,BY],1, paste,collapse = "_")
          } else if (length(BY) == 1) {
            cat("Columna Grupo cuando By = 1\n")
            ds1$BYZZZZ <- ds1[,BY]
          } 
          
          cat("Inicio del analisis de varianza\n")
          result <- lapply(split(ds1, ds1$BYZZZZ), function(xlin) {
            varianza(var = var, ds = xlin, fijos = fijos,
                     aleatorios = aleatorios, heteroce = heteroce,
                     varcom = varcom, VB = VB, class_var = class_var,
                     HET = HET, TestFixed = TestFixed)
          })
          
          
          # result <- lapply(xlin, function(xa) {
          #   varianza(var = "Alt", ds = xa, fijos = fijos, aleatorios = aleatorios, heteroce = "NO",
          #            class_var =  c("Block","Rep","Name"))
          # }
          
          
        }
        
      }, timeout = 1800)
      
    }) %...>% (function(result){

      #' ----------------------------------------------------------------------------------------------
      #### 5. Outputs ####
      #' ----------------------------------------------------------------------------------------------
    
      print("toy aquí hahaha")
      
      if (is.null(by)) {
        ###### 5.1 Prueba Efectos Fijos #####
        
        output$Fixed_table <- renderTable({
          if (class(result) == "mmer") {
            Fixed_table <- NULL
          } else if (class(result) %in% c("lmerMod", "lm", 
                                          "lmerModLmerTest") ) {
            Fixed_table <- as.data.frame(anova(result)) %>%
              rownames_to_column("Efecto")
            # colnames(Fixed_table)[2] <- "DF"
          } else {
            Fixed_table <- NULL
          }
          
          shinyjs::show("download_fixed_table") # Botón de descarga
          
          Fixed_table
        })
        
        
        ##### 5.2 Componentes de varianza #####
        output$coefs_table <- renderTable({
          if(class(result) == "mmer") {
            coefs_df <- as.data.frame(summary(result$varcomp)) %>%
              rownames_to_column("Efecto")
          } else if(class(result) %in% c("lmerMod","lmerModLmerTest")){
            coefs_df <- as.data.frame(VarCorr(result))
            coefs_df <- coefs_df[,!colnames(coefs_df) %in% c("var1","var2")]
            colnames(coefs_df)[1] <- "Efecto"
          }
          
          shinyjs::show("download_coefs_table") # Botón de descarga
          
          coefs_df
        })
        
        
        ##### 5.3 BLUPs #####
        output$ranef_table <- renderTable({
          
          showPageSpinner()
          
          if(class(result) == "mmer") {
            jkl = names(result$U)
            ranef_df <- NULL
            for (jk in 1:length(jkl)) {
              asd = as.data.frame(result[['U']][[paste0(jkl[jk])]]) %>% rownames_to_column("Efecto") %>%
                mutate(Effect = jkl[jk])
              ranef_df <- rbind(ranef_df,asd)
              rm(asd)
            }
            
            hidePageSpinner()
            
          } else if(class(result) %in% c("lmerMod","lmerModLmerTest")){
            
            ranef_df = as.data.frame(ranef(result))
            ranef_df <- ranef_df[,!colnames(ranef_df) %in% c("term")]
            colnames(ranef_df) <- c("Efecct", "Efecto","Estimate","SD")
            
            hidePageSpinner()
          }
          
          shinyjs::show("download_BLUPs_table") # Botón de descarga
          
          ranef_df
        })
        
        
        ##### 5.4 Resumen del modelo #####
        output$summary_text <- renderPrint({
          
          showPageSpinner()
          
          summary_stats <- summary(result)
          
          hidePageSpinner()
          
          shinyjs::show("download_summary_table") # Botón descarga
          
          summary_stats
        })
        
        
        ##### 5.5 LSMEANS #####
        output$lsmeans_table <- renderTable({
          
          showPageSpinner()
          
          lsmeans_table <- as.data.frame(lsmeans(result, paste0(input$lsmeans))) %>%
            mutate(lsmean = as.numeric(lsmean))
          
          hidePageSpinner()
          
          shinyjs::show("downloads_lmeans_table") # Botón de descarga
          
          lsmeans_table
        })
        
        
        ##### 5.6 Post Hoc Test #####
        output$PHT <- renderTable({
          
          showPageSpinner()
          
          
          PHT  = emmeans(result, list(as.formula(paste0("pairwise ~ ",input$lsmeans))),
                         adjust = input$post)
          PHT = as.data.frame(PHT[2])
          colnames(PHT) <- gsub(paste(c("pairwise","\\.","differences","of.",input$lsmeans), 
                                      collapse = "|"),
                                "", colnames(PHT), ignore.case = T)
          colnames(PHT)[1] <- "Diferencia"
          
          hidePageSpinner()
          
          shinyjs::show("download_posthoctest_table") # Botón de descarga
          
          PHT
        })
        
        
        ##### 5.7 BLUES #####
        output$Blues_table <- renderTable({
          
          showPageSpinner()
          
          if(class(result) == "mmer") {
            Blues_table <- as.data.frame(coef.mmer(result))
            
            hidePageSpinner()
            
          } else if(class(result) %in% c("lmerMod","lmerModLmerTest")){
            
            Blues_table = as.data.frame(fixef(result)) %>%
              rownames_to_column("Efecto") %>%
              mutate(Efecto= gsub(paste(c("\\(","\\)"), collapse = "|"), "", Efecto))
            colnames(Blues_table)[2] <- "Estimate"
            
            hidePageSpinner()
            
          } else if(class(result) == "lm"){
            
            Blues_table = as.data.frame(coef(result)) %>%
              rownames_to_column("Efecto") %>%
              mutate(Efecto= gsub(paste(c("\\(","\\)"), collapse = "|"), "", Efecto))
            colnames(Blues_table)[2] <- "Estimate"
            
            hidePageSpinner()
          }
          
          shinyjs::show("download_BLUEs_table") # Botón de descarga
          
          Blues_table
        })
        
      } else {
        NULL
        print("Me fuí por aquí")
      }
      
      
      
      hidePageSpinner()
      
      
      #' ----------------------------------------------------------------------------------------------
      #### 6. Download results ####
      #' ----------------------------------------------------------------------------------------------
      
      ##### 6.1 Manual de uso #####
      output$download_button <- downloadHandler(
        filename = function() {
          "manual_de_uso.pdf"
        },
        content = function(file) {
          file.copy("documentation/manual_de_uso.pdf", file)
        }
      )
# 
#       ##### 6.2 Test Fijos #####
#       output$download_fixed_table <- downloadHandler(
#         filename = function() {
#           paste("Prueba de efectos fijos", ".csv", sep = "")
#         },
#         content = function(file) {
# 
#           showPageSpinner()
# 
#           if(class(result) == "mmer") {
#             Fixed_table <- NULL
#           } else if(class(result) %in% c("lmerMod","lm","lmerModLmerTest")){
#             Fixed_table <- as.data.frame(anova(result)) %>%
#               rownames_to_column("Efecto")
#           }
# 
#           hidePageSpinner()
# 
#           write.csv(Fixed_table, file, row.names = FALSE)
#         },
#         contentType = "text/csv"
#       )
# 
#       ##### 6.3 Componentes de varianza #####
#       output$download_coefs_table <- downloadHandler(
#         filename = function() {
#           paste("Componentes de varianza", ".csv", sep = "")
#         },
#         content = function(file){
# 
#           showPageSpinner()
# 
#           if(class(result) == "mmer") {
#             coefs_df <- as.data.frame(summary(result)$varcomp) %>%
#               rownames_to_column("Efecto")
#           } else if(class(result) == "lmerMod"){
#             coefs_df <- as.data.frame(summary(result)$varcor)
#             coefs_df <- coefs_df[,!colnames(coefs_df) %in% c("var1","var2")]
#             colnames(coefs_df)[1] <- "Efecto"
#           }
# 
#           hidePageSpinner()
# 
#           write.csv(coefs_df, file, row.names = FALSE)
#         },
#         contentType = "text/csv"
#       )
# 
#       ##### 6.4 BLUEs #####
#       output$download_BLUEs_table <- downloadHandler(
#         filename = function(){
#           paste("BLUEs", ".csv", sep = "")
#         },
#         content = function(file){
# 
#           showPageSpinner()
# 
#           if(class(result) == "mmer") {
#             Blues_table <- as.data.frame(coef.mmer(result))
# 
#           } else if(class(result) == "lmerMod"){
#             Blues_table = as.data.frame(fixef(result)) %>%
#               rownames_to_column("Efecto") %>%
#               mutate(Efecto= gsub(paste(c("\\(","\\)"), collapse = "|"), "", Efecto))
#             colnames(Blues_table)[2] <- "Estimate"
# 
#           } else if(class(result) == "lm"){
#             Blues_table = as.data.frame(coef(result)) %>%
#               rownames_to_column("Efecto") %>%
#               mutate(Efecto= gsub(paste(c("\\(","\\)"), collapse = "|"), "", Efecto))
#             colnames(Blues_table)[2] <- "Estimate"
#           }
# 
#           hidePageSpinner()
# 
#           write.csv(Blues_table, file, row.names = FALSE)
#         },
#         contentType = "text/csv"
#       )
# 
# 
#       ##### 6.5 Lsmeans #####
#       
#       output$downloads_lmeans_table <- downloadHandler(
#         filename = function(){
#           paste("Medias Ajustadas", ".csv", sep = "")
#         },
#         content = function(file){
# 
#           showPageSpinner()
# 
#           lsmeans_table <- as.data.frame(lsmeans(result, paste0(input$lsmeans))) %>%
#             mutate(lsmean = as.numeric(lsmean))
# 
#           hidePageSpinner()
# 
#           write.csv(lsmeans_table, file, row.names = FALSE)
#         },
#         contentType = "text/csv"
#       )
# 
#       ##### 6.6 Post-Hoc test #####
# 
#       output$download_posthoctest_table <- downloadHandler(
#         filename = function(){
#           paste("Post-hot-test", ".csv", sep = "")
#         },
#         content = function(file){
# 
#           showPageSpinner()
# 
#           PHT  = emmeans(result, list(as.formula(paste0("pairwise ~ ",input$lsmeans))),
#                          adjust = input$post)
#           PHT = as.data.frame(PHT[2])
#           colnames(PHT) <- gsub(paste(c("pairwise","\\.","differences","of.",input$lsmeans), 
#                                       collapse = "|"),
#                                  "", colnames(PHT), ignore.case = T)
#           colnames(PHT)[1] <- "Diferencia"
#           
#           
#           hidePageSpinner()
# 
#           write.csv(PHT, file, row.names = FALSE)
#         },
#         contentType = "text/csv"
#       )
# 
#       ##### 6.7 BLUPs #####
#       
#       output$download_BLUPs_table <- downloadHandler(
#         filename = function(){
#           paste("BLUPs", ".csv", sep = "")
#         },
#         content = function(file){
# 
#           showPageSpinner()
# 
#           if(class(result) == "mmer") {
#             jkl = names(result$U)
#             ranef_df <- NULL
# 
#             for (jk in 1:length(jkl)) {
#               asd = as.data.frame(result[['U']][[paste0(jkl[jk])]]) %>% rownames_to_column("Efecto") %>%
#                 mutate(Effect = jkl[jk])
#               ranef_df <- rbind(ranef_df,asd)
#               rm(asd)
#             }
# 
#           } else if(class(result) == "lmerMod"){
#             ranef_df = as.data.frame(ranef(result))
#             ranef_df <- ranef_df[,!colnames(ranef_df) %in% c("term")]
#             colnames(ranef_df) <- c("Efecct", "Efecto","Estimate","SD")
#           }
# 
#           hidePageSpinner()
# 
#           write.csv(ranef_df, file, row.names = FALSE)
#         },
#         contentType = "text/csv"
#       )
# 
#       ##### 6.8 Resumen modelo #####
# 
#       output$download_summary_table <- downloadHandler(
#         filename = function(){
#           paste("Resumen del modelo", ".txt", sep = "")
#         },
#         content = function(file){
# 
#           showPageSpinner()
# 
#           summary_stats <- capture.output(summary(result))
# 
#           cat(summary_stats, file = file, sep = "\n")
# 
#           hidePageSpinner()
#         },
#         contentType = "text/plain"
#       )
      
      #### 7. Condicional Error ####
      
    }) %...!% (function(error) {
      
      log_error(error)
      
      hidePageSpinner()
      
      showModal(modalDialog(
        title = "Error en la ejecución",
        "Se ha producido un error en el proceso o el tiempo de espera maximo ha sido excedido.",
        footer = NULL,
        easyClose = TRUE
      ))
    })
})
}
