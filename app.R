
# No console do R
#renv::init()  # Se ainda não inicializou"
#renv::snapshot()
#renv::install(c("shinyWidgets", "bslib", "readxl", "openxlsx", "dplyr", "lubridate", "rbcb"))  # Instala os pacotes necessários

source("ui.R")
#install.packages("rbcb", "lubridate")4
require(shiny)
require(dplyr)
require(DT)
require(bslib)
require(shinyWidgets)
require(openxlsx)
require(readxl)
#require(rbcb)
require(lubridate)
require(Rcpp)





# -------------------------------------------------------


server <- function(input, output, session) {
  user <- unname(Sys.info()["user"])
  if (user == "shiny") {
    
    # Set library locations
    .libPaths(c(
      "C:/Users/55819/Desktop/guisa/renv/library/windows/R-4.4/x86_64-w64-mingw32"
    )
    )
  }
  # Create reactive value to track validation status
  validation_status <- reactiveVal(FALSE)
  
  # Função reativa para capturar os parâmetros ao clicar no botão
  parametros_reativos <- eventReactive(input$validar, {
    # Extrair o ano e o mês a partir do airMonthpickerInput
    data_inicio <- as.Date(paste(substr(input$data_inicio, 1, 4), 
                                 substr(input$data_inicio, 6, 7), "01", sep = "-"))
    data_fim <- as.Date(paste(substr(input$data_fim, 1, 4), 
                              substr(input$data_fim, 6, 7), "01", sep = "-"))
    
    parametros <- list(
      "Data de Início" = format(data_inicio, "%m/%Y"),
      "Data de Término" = format(data_fim, "%m/%Y"),
      "Indexador Econômico" = input$indexador
    )
    
    # Exportar os parâmetros para um arquivo Excel
    write.xlsx(as.data.frame(parametros), "resultados.xlsx", rowNames = FALSE)
    
    # Set validation status to TRUE to show download button
    validation_status(TRUE)
    
    return(parametros)
  })
  
  # Render download button conditionally
  output$download_button <- renderUI({
    if (validation_status()) {
      downloadButton("downloadData", 
                     label = "Relatório Detalhado",  # Removido o tags$span e icon()
                     class = "btn btn-success"
      )
    }
  })
  
  # Handle file download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("resultados-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      file.copy("resultados.xlsx", file)
    }
  )
  
  
  # Exibir os parâmetros capturados no verbatimTextOutput
  output$parametros <- renderText({
    
    # Define o código do indexador com base no parâmetro selecionado
    código_indexador <- if (parametros_reativos()$`Indexador Econômico` == "INPC") {
      188
    } else if (parametros_reativos()$`Indexador Econômico` == "IPCA") {
      433
    }
    
    # Define a data de início e término como o último dia do mês
    data_inicio <- as.Date(paste0("01-", parametros_reativos()$`Data de Início`), format = "%d-%m/%Y") + months(1) - days(1)
    data_termino <- as.Date(paste0("01-", parametros_reativos()$`Data de Término`), format = "%d-%m/%Y") + months(1) - days(1)
    
    # Obtém a série de índices e calcula o acumulado
    #serie <- rbcb::get_series(código_indexador, start_date = data_inicio, end_date = data_termino)
    
    # Calcula o acumulado da variação do indexador
    #acumulado <- prod(1 + (serie[2] / 100)) - 1
    
    # Exibe os parâmetros capturados e o acumulado
    paste(
      "Parâmetros salvos no arquivo 'resultados.xlsx':",
      "\n",
      "\nIndexador: ", parametros_reativos()$`Indexador Econômico`,
      "\nInício: ", parametros_reativos()$`Data de Início`,
      "\nTérmino: ", parametros_reativos()$`Data de Término`,
      "\nAcumulado: ", round(1 * 100, 2), "%"
    )
  })
  
  # Outras funções do servidor permanecem iguais...
}

shinyApp(ui = ui, server = server)