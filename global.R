
# Función para llevar el registro de logs
log_error <- function( message ) {
  log_file_path <- file.path("logs", "shiny_error_log.log")
  cat(paste(Sys.time(), "-", message, "\n"), file = log_file_path, append = TRUE)
}


# Función varianza
varianza <- function( var,ds, fijos = 1, aleatorios = NULL, heteroce = "No", 
                      varcom = NULL, VB = FALSE, class_var, HET = NULL,
                      TestFixed = "No" ) {
  
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
