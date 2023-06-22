
library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Estimativas"),
  theme = shinythemes::shinytheme("sandstone"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "file",
        label = "Faça upload dos arquivos CSV que deseja estimar",
        accept = ".csv",
        multiple = T
      ),
      uiOutput(outputId = "msg_download"),
      uiOutput(outputId = "aplicarRNAButtonWrapper", style = 'display: inline-block;'),  # Placeholder do botão "Aplicar RNA"
      uiOutput(outputId = "downloadButtonWrapper", style = 'display: inline-block;'), # Placeholder do botão "dowbload"
      uiOutput(outputId = "select_all"), # Placeholder do botão de seleção
      uiOutput(outputId = "selectInputsWrapper"),  # Placeholder dos elementos do selectInput 
      
    ),
    mainPanel(
      uiOutput(outputId = "tb")
    )
  )
))


