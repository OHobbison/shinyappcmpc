
#Definição do logo da empresa
logo = 'logo.png'

Sys.setenv(CUDA = "cpu")
Sys.setenv(TORCH_INSTALL = 1)
Sys.setenv(TORCH_HOME = "./torch/")

library(shiny)
library(dplyr)
library(readr)
library(data.table)
library(DT)
library(ggplot2)
library(luz)
library(torch)
library(dplyr)
library(zip)
library(shinyalert)
source('padronizacao.R')
source('utils.R')
source('rna_r.R')

# Filtros
filtroString <- '(CD_PROC_IMPORTA_HUB %in% input$processoHub) & 
(CD_MEDICAO %in% input$codigoMedicao) &
(CD_MEDICAO_PARCELA %in% input$parcela)'

js_select_all <- paste0(c(
  "var selectinput = document.getElementById('processoHub');",
  "var allSelected = true;",
  "$(selectinput).find('option').each(function() {",
  "   if (!$(this).prop('selected')) {",
  "       allSelected = false;",
  "       return false;", 
  "   }",
  "});",
  "$(selectinput).find('option').prop('selected', !allSelected);",
  "$(selectinput).trigger('change');"
))



# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  #Flag que define se o botão de rna foi clicado  
    flag_rna <- reactiveVal("F")
    flag_rna_aplicada <- reactiveVal("F")
    showNotification("This is a notification.")
    
    
    # Leitura do data frame
  
    data <- reactive({
      flag_rna("F")
      flag_rna_aplicada("F")
      fileInput <- input$file
      #!endsWith(fileInput$name, ".csv")
      if(is.null(fileInput)){
          return()
      }else if (any(!endsWith(fileInput$name, ".csv"))){
        shinyalert("O arquivo não é um CSV!", "Você deve fazer upload de um arquivo .csv para aplicação das redes.", type = "error")
        return()
      } else {
        showModal(modalDialog("Carregando dados...", footer=NULL))
        fileInput <- padronizar(fileInput)
        removeModal()
      }

      # Filtros de seleção
      updateSelectInput(session, "processoHub",
                        choices = unique(fileInput[["dados"]]$CD_PROC_IMPORTA_HUB),
                        selected = unique(fileInput[["dados"]]$CD_PROC_IMPORTA_HUB))
      
      updateSelectInput(session, "codigoMedicao",
                        choices = unique(fileInput[["dados"]]$CD_MEDICAO),
                        selected = unique(fileInput[["dados"]]$CD_MEDICAO))
      
      updateSelectInput(session, "parcela",
                        choices = unique(fileInput[["dados"]]$CD_MEDICAO_PARCELA),
                        selected = unique(fileInput[["dados"]]$CD_MEDICAO_PARCELA))
    
      #AVISOS DE ALTURAS MEDIDAS
      if (any(fileInput[["dados"]]$FLG_ERRO_ALTURA == 1)) {
        shinyalert("Atenção!", 'Algumas parcelas possuem menos de 4 alturas. Para visualizar a parcela com problema ver "detalhes".', type = "warning")
      }else if(any(fileInput[["dados"]]$FLG_ERRO_ALTURA == 2)){
        shinyalert("Atenção!", 'Algumas parcelas possuem mais de 4 alturas. Para visualizar a parcela com problema ver "detalhes".', type = "warning")
      }
      #AVISOS DE ERROS GRAVES
      if (any(fileInput[["dados"]]$FLG_ERRO1 == 1)) {
        shinyalert("Erro!", "Os dados possuem alturas sem CAP. Não é possível aplicar a RNA.", type = "error")
        return()
      }
      
      
      
      return(fileInput)
      
    })
    # Status
    output$status <- renderDataTable({
      
      data()[["dados"]] %>%
        
        mutate(Parcela = paste(ID_PROJETO, CD_TALHAO, NRO_PARCELA,sep="-"),
               Id_Processo = CD_PROC_IMPORTA_HUB,
               Qtd_HT = QTD_ARVORE,
               Status = ifelse(FLG_ERRO_ALTURA == 0, "Ok",
                               ifelse(FLG_ERRO_ALTURA == 2,"Mais de 4 alturas medidas","Menos de 4 alturas medidas"))) %>%
        select(Id_Processo,Parcela, Qtd_HT, Status) %>%
        distinct() %>%
        filter(Qtd_HT != 4)%>%
        datatable(options = list(paging = TRUE,
                                 pageLength = 5,
                                 searching = FALSE))
    })
    #Tabela dados de campo
    output$table <- renderDataTable({
      
      datatable(data()[["dados"]] %>%
                  # Aplicação dos filtros
                  filter(eval(parse(text = filtroString))))
    })
    
    # plots dos dados de campo
    output$plot <- renderPlotly({
      
      plotscatter(data()[["dados"]] %>%
                    # Aplicação dos filtros
                    filter(eval(parse(text = filtroString))))
                    
    })
    #DETALHES
    detalhes <- reactive({
      temp=data.frame(data()[["dados"]])
      temp$CHAVE <- paste(temp$ID_PROJETO, temp$CD_TALHAO, sep = '-') %>%
        paste(" |")
      lista <- unique(temp$CHAVE)
      return(lista)
    })
    #observador do botão "aplicar rna"
    data_rna <- reactiveVal(NULL)
    
    
    #Observador do evento de clique no botão aplicar_rna
    observeEvent(input$aplicar_rna, {
      if (is.null(data())){
        shinyalert("Faça upload dos dados!", "A rede neural só pode ser aplicada após a realização do upload dos dados csv.", type = "error")
      }else{
        showModal(modalDialog("Aplicando RNA, isso pode levar alguns minutos...", footer = NULL))
        dados <- run_rna_r(data()[["dados"]])
        data_rna(dados)  # Atribuir o valor de 'dados' à variável reativa 'data_rna'
        flag_rna_aplicada("T")
        removeModal()
      }
      
    })
    #Observador do evento de clique no botão selecionar tudo
    
    # output$teste <- renderDataTable({
    # 
    #   datatable(data()[["dados"]])
    # })
    
    #plots da rna aplicada
    output$plot_rna <- renderPlotly({
      
      plotscatter_rna(data_rna() %>%
                    # Aplicação dos filtros
                    filter(eval(parse(text = filtroString))))
      
    })

    
  
    #CRIAÇÃO DA TABELA DE MÉTRICAS
    metricas <- reactive({
      df=data_rna()
      df <- data.frame(df)
      df = df %>%
        filter(eval(parse(text=filtroString)))
      
      base_metrics <- df %>%
        filter(ORIGEM == "Campo")
      
      
      y_true = base_metrics$HT/10
      y_pred = base_metrics$HTEST_RNA/10
      
      #Mean absolute error
      mae = mean(abs(y_true-y_pred))
      #Mean sequared error
      mse = mean((y_true -y_pred)^2)
      #Mean absolute percentual error
      mape = mean(abs((y_true-y_pred)/y_true))*100
      #Mean percentual error
      mpe = mean((y_true-y_pred)/y_true)*100
      #Correlação
      corr=cor(base_metrics$CAP,base_metrics$HTEST_RNA)
      #R²
      r2 = get_r2(y_true,y_pred)
      
      df_metrics = round(data.frame(mae = mae, mse = mse, mape = mape, mpe = mpe, corr = corr,r2=r2),2)
      col_names = c('MAE (metros)','MSE (metros)','MAPE (%)','MPE (%)','Cor',"R2")
      colnames(df_metrics) = col_names
      df_metrics = df_metrics %>%
        tibble()
      return(df_metrics)
    })
    
    output$table_metrics <- renderDataTable({
      metricas() %>%
      datatable(options = list(paging = F,
                               searching = FALSE))
    })
    
    
    #Fazer download dos csvs
    output$download <- downloadHandler(

      filename = function() {
        paste("Arquivos_estimados_",format(Sys.Date(), "%d-%m"), ".zip", sep="")
      },
      content = function(fname) {
        
        fs <- c()
        ids<- unique(data_rna()$CD_PROC_IMPORTA_HUB)
        ids=subset(ids, ids %in% input$processoHub)
        tmpdir <- tempdir()
        setwd(tempdir())
        data<-data_rna() 
        data$HT <- data$HT_FINAL
        data <- data%>%
          select(col_name)
        
        for (i in ids) {
          arquivo = data%>%
            filter(CD_PROC_IMPORTA_HUB==i )%>%
            data.frame()
          path <- paste0("ID", i, ".csv")
          fs <- c(fs, path)
          write_csv2(arquivo, path,na="")
        }
        zip(zipfile=fname, files=fs)
      },
      contentType = "application/zip"
    )
    #UI CONDICIONAIS
    #Só mostra os inputs quando tiver sido feito upload de dados
    output$selectInputsWrapper <- renderUI({
      if (!is.null(data())) {
        tagList(
          selectInput(
            inputId = 'processoHub',
            label = 'Processo',
            choices = c(),
            multiple = T,
            selectize=F,
          ),
          selectInput(
            inputId = 'codigoMedicao',
            label = 'Medição',
            choices = c(),
            multiple = T,
          ),
          selectInput(
            inputId = 'parcela',
            label = 'Parcela',
            choices = c(),
            multiple = T
          )
        )
      }
    })
    #Botões
    #Só mostra o botão de download quando tiver sido feito upload de dados
    output$aplicarRNAButtonWrapper <- renderUI({
      if (!is.null(data())) {
        div(
          actionButton("aplicar_rna", "Aplicar RNA",icon=icon("gear")),
          style = "margin-bottom: 10px;"
        )
      }
    })
    #Só mostra o botão de download quando a rna tiver sido aplicada
    output$downloadButtonWrapper <- renderUI({
      if (!is.null(data_rna())& flag_rna_aplicada()=='T') {
        downloadButton("download", "Download...",icon=icon("download"))
      }
    })
    output$msg_download <- renderUI({
      if (!is.null(data_rna())& flag_rna_aplicada()=='T') {
        tags$h5(renderText("Atenção: Selecione os processos que deseja fazer download!"))
      }
    })
    #Selecionar tudo botão
  
    
    output$select_all <- renderUI({
      if (!is.null(data())) {
        actionButton('select', 'Selecionar', style='padding:4px; font-size:70%',onclick=js_select_all,icon=icon("square-check"))
      }
    })




    # Criação das abas no mainPanel
    output$tb <- renderUI({
        if (is.null(data())){
          div(
          h4("Essa aplicação foi desenvolvida com o objetivo de estimar as alturas dos inventários realizado a campo,"),
          h4(" utilzando redes neurais artificais."),
          br(),
          h5("Powered by", tags$img(src=logo, heigth=500, width=500, style = "display: block; margin: 0 auto;"))
          )
          #Se o upload dos dados for feito
        }else if (!(is.null(data())) && flag_rna_aplicada()=="F"){
            tabsetPanel(selected="Visualizar dados",
                        tabPanel("Detalhes",
                                 tags$h2("Talhões | Hortos", style = "text-align: center;"),
                                 tags$h4(renderText(detalhes()), style = "text-align: center;"),
                                 dataTableOutput("status")
                                 ),
                        tabPanel("Visualizar dados",
                                 tags$h2("Dados de campo", style = "text-align: center;"),
                                 plotlyOutput("plot")))
          #Se rna estiver sido aplicada
        }else if (!(is.null(data())) && flag_rna_aplicada()=="T"){
          tabsetPanel(selected="Resultado RNA",
                      tabPanel("Detalhes",
                               tags$h2("Talhões | Hortos", style = "text-align: center;"),
                               tags$h4(renderText(detalhes()), style = "text-align: center;"),
                               dataTableOutput("status"),
                      ),
                      tabPanel("Visualizar dados",
                               tags$h5("Dados de campo", style = "text-align: center;"),
                               plotlyOutput("plot")),
                      tabPanel("Resultado RNA",
                               tags$h4("Resultados RNA", style = "text-align: center;"),
                               plotlyOutput("plot_rna"),
                               br(),
                               tags$h2("Métricas", style = "text-align: center;"),
                               dataTableOutput("table_metrics")
                      ))
          }
          
        
            
    })

})
